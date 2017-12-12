# Takes the input string and outputs a nice table
processSearch<-function(query=NULL){
  
  #Running Query on Pubmed - kinda just gets me PMIDS
  withProgress(message = 'Running Pubmed Query', value = 0, {
    resQ1 <- EUtilsSummary(query, type='esearch', db='pubmed')
  
    #check the total number of articles, return everything if its less than 20,000
    resQ1 <- EUtilsSummary(query, type='esearch', db='pubmed',retmax=resQ1@count)
    pmidUnique<-unique(resQ1@PMID)
  })
  
  #Gather data in batches and convert to data frame
  withProgress(message = 'Downloading and formatting query results', value = 0, {
    #format the data
    #need to put everything together in batches, 2000 articles was the magic number before time outs occur
    numVals<-seq(from=0,to=length(pmidUnique),by=2000)
    numVals<-c(numVals,tail(numVals, n=1) + (length(pmidUnique) %% 2000))
    
    corpus<-c()
    for(i in 1:(length(numVals)-1)){
      incProgress(amount = 1/length(numVals), message = NULL, detail = NULL,
                  session = getDefaultReactiveDomain())
      #to make this faster, form new query on ID run in parallel
      start = numVals[i]+1
      end = numVals[i + 1]
      corpus <- rbind(corpus,formatData(pmidUnique[start:end]))
    }
  })
  
  return(corpus)
}


#formatting the pubmed data. Helper script to processSearch function
#retrieve and format data
formatData<-function(ids = NULL){
  #limitation : extract resuts as sets of 200. There's a limit to how many
  #iI passed 2000 objects in, but 1/10th of that seems to be the magic # before timeoouts
  
  if(length(ids) < 200){
    #process all at once
    numVals<-c(1,length(ids))
  }else{
    #break it up into chunks b.c it makes it easier to query
    numVals<-seq(from=0,to=length(ids),by=200)
    
    if(tail(numVals, n=1)< length(ids)){
      numVals<-c(numVals,tail(numVals, n=1) + (length(ids) %% 200))
    }
  }
  allData<-data.frame(PMID=NULL,
                      YearPub=NULL,
                      Journal=NULL,
                      Authors=NULL,
                      Title=NULL,
                      Abstract=NULL,
                      meshTerms=NULL)
  
  for(i in 1:(length(numVals)-1)){
    #to make this faster, form new query on ID run in parallel
    start = numVals[i]+1
    end = numVals[i + 1]
    pubResults<-EUtilsGet(paste0(ids[start:end],collapse = ","),type="efetch",db="pubmed")
    
    #covert author list from list object to string obeject with author names seperated by ;
    authors<-sapply(pubResults@Author,function(x){
      apply(x,1,function(y){sprintf("%s, %s",y[1],y[2])}) %>% paste0(.,collapse=";")
    })
    
    #putting all the data together into a data frame
    temp<-data.frame(PMID=pubResults@PMID,
                     YearPub=pubResults@YearPubmed,
                     Journal=pubResults@Title,
                     Authors=authors,
                     Title=pubResults@ArticleTitle,
                     Abstract=pubResults@AbstractText)
    
    
    temp$meshTerms<-pubResults@Mesh
    
    allData<-rbind(allData,temp)
  }
  
  return(allData)
}

#Apply glorious tidy text methods. Just use one term for an overview
#commonly used words in the english language.
#note that I am indebted to Julia and David: http://tidytextmining.com/

tidyCorpus<- function(corpus=NULL){
  data(stop_words)
  #remove some common terms that will occur in abstract
  customStopTerms<-data.frame(word=c("abstract", "text", "abstracttext","introduction","background","method","methods","methodology","conclusion","conclusions","objectives","results","result","we","materials","purpose","significance","significant","mg"))
  
  # Make text tidy!
  withProgress(message = 'Formatting data for text analysis', value = 0.1, {
    tidyCorpus_df <- corpus[,c("PMID","Title","Abstract")] %>%
      mutate(text = paste0(Title,Abstract)) %>%
      unnest_tokens(word, text) %>%
      mutate(word = strsplit(as.character(word), "\\.")) %>% #some text is stuck together for example person.METHODS so, I am fixing that
      tidyr::unnest(word) %>% 
      anti_join(stop_words) %>%
      anti_join(customStopTerms) %>%
      filter(str_length(word)>2) %>% #only keeps words with length of 2 or greater (AMR, a useful abbreviation, is three characters long) %>%
      filter(!str_detect(word,"\\d")) %>% #get rid of any numbers
      mutate(wordStemmed = wordStem(word)) %>% #finally, get the word stems (Porter's algorithm)
      select(PMID,word,wordStemmed)
  })
  
  #Calculate TF IDF
  withProgress(message = 'Calculating TD_IDF metric', value = 0.1, { 
    #Cleaning up terms
    #removing terms that are really really common & those that are very infrequent
    # I will now also add the term frequency document frequency values.
    tidyCorpus_df<-tidyCorpus_df %>%
      count(PMID, wordStemmed, sort = TRUE) %>%
      ungroup() %>%
      bind_tf_idf(wordStemmed, PMID, n)
  })
 
  #Remove super common and super rare terms
  withProgress(message = 'Removing very rare & very common terms', value = 0.1, {   
    totalArticles<-nrow(corpus)
    
    wordToRemove<-tidyCorpus_df %>%
      group_by(wordStemmed) %>%
      count() %>%
      filter(nn < totalArticles*0.01 | nn > totalArticles*0.7) #hard criteria, could be done more empirically
    
    tidyCorpus_df<-anti_join(tidyCorpus_df,wordToRemove,by="wordStemmed")
  })
  
  return(tidyCorpus_df)
}

#runs tSNE, paramters can be modified
runTSNE<-function(tidyCorpus_df=NULL, ...){
  # Create DTM for analysis
  withProgress(message = 'Creating document term matrix', value = 0.1, {
    dtm<-tidyCorpus_df %>%
    select(PMID,wordStemmed,tf_idf) %>%
    spread(PMID,tf_idf)
  
    colVals<-dtm$wordStemmed
    rowVals<-colnames(dtm)[2:ncol(dtm)]
    
    dtm<-as.matrix(dtm[,2:ncol(dtm)]) %>% t()
    rownames(dtm)<-rowVals
    colnames(dtm)<-colVals
    
    dtm[is.na(dtm)]<-0
  })
  
  #Run tsne Analysis
  withProgress(message = 'Running tSNE', value = 0.1, {
    tsneObj<-Rtsne(dtm,...)
    tsneObj$Y<-data.frame(cbind(rownames(dtm),tsneObj$Y),stringsAsFactors = F)
    colnames(tsneObj$Y)<-c("PMID",paste("tsneComp",1:(ncol(tsneObj$Y)-1),sep=""))
    
    #finally, just a tiny clean up to make sure the numeric values are numeric
    tsneObj$Y[, 2:ncol(tsneObj$Y)] <- sapply(tsneObj$Y[, 2:ncol(tsneObj$Y)], as.numeric) %>% unname()
  })
  return(tsneObj)
}


#run HDBSCAN, this requires that tsne step already be run
runHDBSCAN<-function(corpus=NULL,minPts = 50){
  #TO DO: Add an error detecting mechanism if not tsneCOMP passed
  
  #get the clusters for the tsne perplexity 100 plot using HDBSCAN
  tmp <- corpus %>%
    select(contains("tsneComp")) %>%
    as.matrix()
  
  cl <- hdbscan(tmp, minPts = minPts) #minimum cluster size of 150 documents
  
  return(cl)
}

# A function that tries to find a sweet spot in tSNE HBSCAN
# The point here is "good enough" parameters and not "globally optimal parameters"
# My defintion of good parameters is set that puts the most documents into non-noise clusters,
# but there is also going to be a penalization on having too many clusters
# Also keeping track (and overtime discounting) articles that just never cluster into anything
# (yes those exist, it's a level of noise I am trying to remove). 
optimalParam<-function(){
  set.seed(416) #repping the 6ix!
  
  #cl<-hdbscan(tmp, minPts) #minimum cluster size of 150 documents
  
  nDocs<-nrow(corpus)
  
  #minPts are course - I don't care for an exhaustive search, just a gist
  #the minPts values will depend on how many documents the search return
  minPtsVals<-c(10,30,50,75,100,125,200)
  if(nDocs<100 & nDocs>50){
    minPtsVals<-c(5,10,20,30,50)
  }else if(nDocs>100 & nDocs<500){
    minPtsVals<-minPtsVals[1:(length(minPtsVals)-2)]
  }else if(nDocs>500 & nDocs<1000){
    minPtsVals<-c(minPtsVals,500) #trying out a bunch of different values
  }else if(nDocs > 1000){
    inPtsVals<-c(minPtsVals,500,1000)
  }
  
  #run HBDSCAN and get cluster stats
  dfPts<-c()
  for(minPts in minPtsVals){
    cl<-runHDBSCAN(corpus,minPts)
    dfPts<-rbind(dfPts,cbind(as.character(corpus$PMID),cl$cluster,rep(minPts,nrow(corpus))))
  }
  
  #summarize all of the runs
  dfPts<-data.frame(dfPts,stringsAsFactor=FALSE)
  colnames(dfPts)<-c("PMID","cluster","groupingLevel")
  
  dfPts<-corpus %>%
        select(PMID,contains("tsneComp")) %>%
        inner_join(dfPts,tsneCord)
   
  #analyze all of the data an pick the optimal value
  #TO DO: This could be done more efficently if it were an on the fly calculation

}
