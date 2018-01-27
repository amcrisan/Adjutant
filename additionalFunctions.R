# Takes the input string and outputs a nice table
processSearch<-function(query=NULL,demoversion=TRUE){
  
  #Running Query on Pubmed - kinda just gets me PMIDS
  withProgress(message = 'Running Pubmed Query', value = 0, {
    resQ1 <- EUtilsSummary(query, type='esearch', db='pubmed')
  
    # return all queries, or if it's the demo version, only return the first 1000
    if(!demoversion){
      resQ1 <- EUtilsSummary(query, type='esearch', db='pubmed',retmax=resQ1@count)
    }
    
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
      #progress indicatory that currently doesn't seem to work
      #incProgress(amount = 1/length(numVals), message = NULL, detail = NULL,
      #            session = getDefaultReactiveDomain())
      #to make this faster, form new query on ID run in parallel
      start = numVals[i]+1
      end = numVals[i + 1]
      corpus <- rbind(corpus,formatData(pmidUnique[start:end]))
    }
  })
  
  corpus<- dplyr::distinct(corpus)
  return(corpus)
}


#formatting the pubmed data. Helper script to processSearch function
#retrieve and format data
formatData<-function(ids = NULL){
  #limitation : extract resuts as sets of 200. There's a limit to how many
  #iI passed 2000 objects in, but 1/10th of that seems to be the magic # before timeoouts
  
  if(length(ids) <100){
    #process all at once
    numVals<-c(1,length(ids))
  }else{
    #break it up into chunks b.c it makes it easier to query
    numVals<-seq(from=0,to=length(ids),by=100)
    
    if(tail(numVals, n=1)< length(ids)){
      numVals<-c(numVals,tail(numVals, n=1) + (length(ids) %% 100))
    }
  }
  
  #data frame to be filled with glorious data
  allData<-data.frame(PMID=NULL,
                      YearPub=NULL,
                      Journal=NULL,
                      Authors=NULL,
                      Title=NULL,
                      Abstract=NULL,
                      journalType = NULL,
                      language = NULL,
                      pmcCitationCount = NULL,
                      pmcID = NULL,
                      doi = NULL,
                      meshTerms=NULL)
  
  
  #This here makes the queries into small manageable chucks so it doesn't time out.
  
  # TO DO: maybe now since Pubmed allows for JSON files to be downloaded, I could just use that
  # instead of risemed? It was the XML parsing that was stupid... think about the conversion.
  # right now risemed works really well.
  for(i in 1:(length(numVals)-1)){
    #to make this faster, form new query on ID run in parallel
    start = numVals[i]
    end = numVals[i + 1]
    
    tmpids<-ids[start:end]
    pubResults<-EUtilsGet(paste0(tmpids,collapse = ","),type="efetch",db="pubmed")
    
    #make sure that results out = result in. EUtils 
    #its an odd fringe case, but this does actually happen
    #and it needs to be delt with more gracefully
    notThere<-which(!(tmpids %in% pubResults@PMID))
    if(length(notThere)>0){
      missingPMID<-tmpids[notThere]
      tmpids<-setdiff(tmpids,missingPMID)
    }
    
    #convert author list from list object to string obeject with author names seperated by ;
    authors<-sapply(pubResults@Author,function(x){
      apply(x,1,function(y){sprintf("%s, %s",y[1],y[2])}) %>% paste0(.,collapse=";")
    })
    
    #also convert mesh Terms from a list
    meshTerms<-sapply(pubResults@Mesh, function(x){
      if(length(x) == 1){
        return(NA)
      }

      x<-x %>%
        filter(Type == "Descriptor") %>% 
        summarise(meshTerms=paste0(Heading,collapse=";"))
      
      return(x$meshTerms)
    })
    #get some article metadata that doesn't ship with risemed EUTilsGet
    # an important note: article citations from PUBmed are basically summarizing
    # what *other pubmed articles* have referenced this work. This number *does not*
    # match what a google search provides. The number provided here relies on 
    # Pubmed Central (open access). SO it's a decent heuristic, but its not perfect
    
    metadata<-c()
    url<-sprintf("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esummary.fcgi?db=pubmed&id=%s&retmode=json",paste0(tmpids,collapse="+"))
    tmp<-fromJSON(url)
    
    #TO DO: move to lapply once I've got a better sanity check to make sure result order matches up
    for(pmid in tmpids){
      metadata<-rbind(metadata,processMetaJSON(pmid,tmp$result[[as.character(pmid)]]))
    }
    
    #putting all the data together into a data frame
    temp<-data.frame(PMID=pubResults@PMID,
                     YearPub=pubResults@YearPubmed,
                     Journal=pubResults@Title,
                     Authors=authors,
                     Title=pubResults@ArticleTitle,
                     Abstract=pubResults@AbstractText,
                     journalType = metadata[,1],
                     language = metadata[,2],
                     pmcCitationCount =metadata[,3],
                     pmcID = metadata[,4],
                     doi = metadata[,5])
    
    #temp$meshTerms<-pubResults@Mesh
    temp$meshTerms<-meshTerms
    
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
  #withProgress(message = 'Formatting data for text analysis', value = 0.1, {
    tidyCorpus_df <- corpus[,c("PMID","Title","Abstract")] %>%
      mutate(text = paste0(Title,Abstract)) %>%
      unnest_tokens(word, text) %>%
      mutate(word = strsplit(as.character(word), "\\.")) %>% #some text is stuck together for example person.METHODS so, I am fixing that
      tidyr::unnest(word) %>% 
      anti_join(stop_words) %>%
      anti_join(customStopTerms) %>%
      filter(str_length(word)>2) %>% #only keeps words with length of 2 or greater (AMR, a useful abbreviation, is three characters long)
      filter(!str_detect(word,"\\d")) %>% #get rid of any numbers
      mutate(wordStemmed = wordStem(word)) %>% #finally, get the word stems (Porter's algorithm)
      select(PMID,word,wordStemmed)
  #})
  
  #Calculate TF IDF
  #withProgress(message = 'Calculating TD_IDF metric', value = 0.1, { 
    # I will now also add the term frequency document frequency values.
    tidyCorpus_df<-tidyCorpus_df %>%
      dplyr::count(PMID, wordStemmed,sort = TRUE) %>%
      ungroup() %>%
      bind_tf_idf(wordStemmed, PMID, n)
  #})
 
  ##Cleaning up terms
  # removing terms that are really really common & those that are very infrequent
  #withProgress(message = 'Removing very rare & very common terms', value = 0.1, {   
    totalArticles<-nrow(corpus)
    
    #TO DO: Hard criteria, could be done more empirically
    wordToRemove<-tidyCorpus_df %>%
      group_by(wordStemmed) %>%
      dplyr::count() %>%
      filter(nn < totalArticles*0.01 | nn > totalArticles*0.7) 
    
    tidyCorpus_df<-anti_join(tidyCorpus_df,wordToRemove,by="wordStemmed")
  #})
  
  return(tidyCorpus_df)
}

#runs tSNE, paramters can be modified
# TO DO : doesn't work super well on very small corpuses because of tsne params
# either die gracefully or also improve param selection
runTSNE<-function(tidyCorpus_df=NULL, ...){
  # Create DTM for analysis
  #withProgress(message = 'Creating document term matrix', value = 0.1, {
    dtm<-tidyCorpus_df %>%
    select(PMID,wordStemmed,tf_idf) %>%
    spread(PMID,tf_idf)
  
    colVals<-dtm$wordStemmed
    rowVals<-colnames(dtm)[2:ncol(dtm)]
    
    dtm<-as.matrix(dtm[,2:ncol(dtm)]) %>% t()
    rownames(dtm)<-rowVals
    colnames(dtm)<-colVals
    
    dtm[is.na(dtm)]<-0
  #})
  
  #Run tsne Analysis
  #withProgress(message = 'Running tSNE', value = 0.1, {
    tsneObj<-Rtsne(dtm,...)
    tsneObj$Y<-data.frame(cbind(rownames(dtm),tsneObj$Y),stringsAsFactors = F)
    colnames(tsneObj$Y)<-c("PMID",paste("tsneComp",1:(ncol(tsneObj$Y)-1),sep=""))
    
    #finally, just a tiny clean up to make sure the numeric values are numeric
    tsneObj$Y[, 2:ncol(tsneObj$Y)] <- sapply(tsneObj$Y[, 2:ncol(tsneObj$Y)], as.numeric) %>% unname()
  #})
  return(tsneObj)
}


#run HDBSCAN, this requires that tsne step already be run
runHDBSCAN<-function(corpus=NULL,...){
  #TO DO: Add an error detecting mechanism if not tsneCOMP passed
  
  #get the clusters for the tsne perplexity 100 plot using HDBSCAN
  tmp <- corpus %>%
    select(contains("tsneComp")) %>%
    as.matrix()
  
  cl <- hdbscan(tmp, ...) #minimum cluster size of 150 documents
  
  return(cl)
}

# A function that tries to find a sweet spot in tSNE HBSCAN
# The point here is "good enough" parameters and not "globally optimal parameters"
# My defintion of good parameters is set that puts the most documents into non-noise clusters,
# but there is also going to be a penalization on having too many clusters
# Also keeping track (and overtime discounting) articles that just never cluster into anything
# (yes those exist, it's a level of noise I am trying to remove). 
optimalParam<-function(corpus = NULL){
  #cl<-hdbscan(tmp, minPts) #minimum cluster size of 150 documents
  
  nDocs<-nrow(corpus)
  
  #minPts are course - I don't care for an exhaustive search, just a gist
  #the minPts values will depend on how many documents the search return
  minPtsVals<-c(10,20,30,50,75,100,125,200)
  if(nDocs>50 & nDocs<100){
    minPtsVals<-c(5,10,20,30,50)
  }else if(nDocs>100 & nDocs<500){
    minPtsVals<-minPtsVals[1:(length(minPtsVals)-2)]
  }else if(nDocs>500 & nDocs<1000){
    minPtsVals<-c(minPtsVals,500) #trying out a bunch of different values
  }else if(nDocs > 1000){
    minPtsVals<-c(minPtsVals,500,1000)
  }
  
  #run HBDSCAN and get cluster stats
  dfPts<-c() #this is most useful for debugging & exploring this method
  clustFitScore<-c()
  for(minPts in minPtsVals){
    cl<-runHDBSCAN(corpus,minPts)
    nClust<-table(cl$cluster) %>% length()
    if(nClust > 1){
      tmp<-cbind(as.character(corpus$PMID),cl$cluster,cl$membership,cl$outlier_scores,rep(minPts,nrow(corpus)))
    
      dfPts<-rbind(dfPts,tmp) #don't store unless debugging or exploring alternatives
    
      #now, for some goodness of fit computations
      tmp<-data.frame(tmp, stringsAsFactors = FALSE)
      colnames(tmp)<-c("PMID","cluster","membershipProb","outlierScore","groupingLevel")
    
      tmp<-tmp %>%
        mutate(membershipProb = as.numeric(membershipProb)) %>% #add one to mitigate effect of zeros
        mutate(outlierScore = as.numeric(outlierScore)) %>% #add one to mitigate the effect of zeros
        mutate(jointProb = (membershipProb * outlierScore)+1) %>%
        mutate(cluster = factor(cluster,levels=sort(unique(cl$cluster))))
    
    #user a linear model to calculate how well the membership & outlier scores explain the data
    #right now, doing this punishes methods that classify a lot of information as noise
        fit<-lm(log(jointProb)~cluster,data=tmp)
        fitSum<-summary(fit)
        clustFitScore<-rbind(clustFitScore,c(minPts,BIC(fit),fitSum$adj.r.squared))
    }else{
      #if there's only one cluster, then everything is so very very bad
      #clustFitScore<-rbind(clustFitScore,c(minPts,-Inf,0))
      break; #usually once it all becomes noise, everything parameters afterwards will too.
    }
  }
  
  # Summarizing Membership across all runs
  dfPts<-data.frame(dfPts) 
  colnames(dfPts)<-c("PMID","tsneCluster","membershipProb","outlierScore","groupingLevel")
  
  dfPts<-dfPts %>%
    mutate(membershipProb= as.numeric(as.character(dfPts$membershipProb))) %>%
    mutate(outlierScore= as.numeric(as.character(dfPts$outlierScore))) %>%
    mutate(groupingLevel = factor(as.character(dfPts$groupingLevel),levels=minPtsVals))
    
  dfPts<-corpus %>%
        select(PMID,contains("tsneComp")) %>%
        inner_join(dfPts,corpus,by="PMID")
  
  # Summarizing the performance
  # Note: Subjectively (i.e. the method agreeing with what I would pick) seems to suggest that adjust r squared is the method I want to use
  clustFitScore<-data.frame(clustFitScore,stringsAsFactors = F)
  colnames(clustFitScore)<-c("groupingLevel","BIC","adj.r.squared")
  
  #get the maximum adj.r.squared
  bestFit<-max(clustFitScore$adj.r.squared)
  clustFitScore<-mutate(clustFitScore,nextBest = bestFit - adj.r.squared)
  
  clustFitSeveralBest<-clustFitScore %>%
    filter(nextBest <0.05)%>%
    arrange(-BIC)
  
  #return the best clustering (most explaining, and lowest BIC)
  #also, flag those data points that are never clustered
  isVeryNoisey<-dfPts %>%
    filter(tsneCluster == 0) %>%
    group_by(PMID)%>%
    dplyr::count()%>% #count how often an article is in the noise bin
    filter(n >= (length(minPtsVals)-2)) #zero in all but two instances
  
  tmp<-dfPts %>% 
    filter(groupingLevel == clustFitSeveralBest$groupingLevel[1]) %>%
    mutate(veryNoisey = ifelse(PMID %in% isVeryNoisey$PMID,"Noise","Signal")) %>%
    select(PMID,tsneCluster,veryNoisey)
  
  return(tmp)
  
  
  ## #DEBUGGING: VISUALIZE DIFFERENT CLUSTER POSSIBILITIES
  # #Some key insights:- it is *GOOD* when noise cluster surrounds all of the other clusters (speard out) (so normally distributed noise might be good...)
  # #                  - it is *bad* when any one cluster has excessive membership
  # #                  - stability of clusters is good
  # p1<-dfPts %>%
  #   #filter(groupingLevel %in% 20:23)%>%
  #   mutate(isNoise = ifelse(cluster==0,"Noise","Signal")) %>%
  #   ggplot(aes(x=tsneComp1,y=tsneComp2)) +
  #   geom_point(aes(alpha=membershipProb))+
  #   theme_bw()+
  #   facet_wrap(~groupingLevel) +
  #   stat_ellipse(aes(group=cluster,col=isNoise))+
  #   scale_colour_manual(values=c("blue","red"))+
  #   theme(legend.position="None")

  # #distribution of group sizes
  # p2<-dfPts %>%
  #   filter(groupingLevel %in% 20:23)%>%
  #   #filter(cluster!= 0) %>% #don't show
  #   ggplot(aes(x=membershipProb))+
  #   geom_histogram()+
  #   facet_wrap(~groupingLevel) +
  #   theme_bw()
  # 
  # p3<-dfPts %>%
  #   filter(groupingLevel %in% 20:23)%>%
  #   #filter(cluster!= 0) %>% #don't show
  #   ggplot(aes(x=outlierScore))+
  #   geom_histogram()+
  #   facet_wrap(~groupingLevel) +
  #   theme_bw()
  # 
  # p4<-dfPts %>%
  #   filter(groupingLevel %in% 20:23)%>%
  #   #filter(cluster!= 0) %>% #don't show
  #   ggplot(aes(x=outlierScore*membershipProb))+
  #   geom_histogram()+
  #   facet_wrap(~groupingLevel) +
  #   theme_bw()
  # 
  # cowplot::plot_grid(p1 + ggtitle("A) tsne + hdbscan overlay"),
  #                    p2+ggtitle("B) hdbscan membership hist"),
  #                    p3+ggtitle("C) outlier scores")
  #                    ,align="h")
  
  
}

#a function that names clusters
getTopTerms<-function(clustPMID=NULL,topNVal=1,clustValue = NA,tidyCorpus = NULL){
  clustValue<-clustValue[1]
  
  if(clustValue == 0)
    return("Noise")
  
  topWord<-tidyCorpus %>%
    filter(PMID %in% clustPMID) %>%
    ungroup() %>%
    group_by(wordStemmed) %>%
    dplyr::count() %>%
    ungroup()%>%
    arrange(-nn) %>%
    top_n(topNVal)
  
  # return character and collapse top terms (useful in even of a tie)
  topWord<-paste0(topWord$wordStemmed,collapse = "-")
  return(topWord)
}


#find the largest cluster that is near where a participant is mousing
hoveredClusterSum<-function(corpus =NULL, e = NULL){
  if(!is.null(e)){
    hovPts<-brushedPoints(corpus, e, xvar = "tsneComp1", yvar = "tsneComp2")
    
    if(nrow(hovPts)>0){
      #get the most common cluster in hovPts
      tmp<-hovPts %>%
        group_by(tsneClusterNames)%>%
        dplyr::count()%>%
        arrange(-n)
      
      tmp<-corpus %>% 
        filter(tsneClusterNames == tmp$tsneClusterNames[1])%>%
        group_by(tsneClusterNames)%>% #this is literally *just* to keep the stupid name
        dplyr::count()
      
      return(tmp)
    }else{
      return(NULL)
    }
  }else{
    return(NULL)
  }
}


#Print a summary of cluster text
clusterSummaryText<-function(tmp = NULL){
  hovMsg<-HTML("<em>Brush (drag mouse to form box)</em> over points in the <em>cluster graph (left)</em> to get summary information about the cluster")
  
  if(!is.null(tmp)){
    hovMsg<-HTML(sprintf("<h4><em> Cluster Name</em></h4>Cluster <strong>%s</strong> has <strong>%d</strong> members<br>",tmp$tsneClusterNames[1],tmp$n[1]))
  }
  
  return(hovMsg)
}

#Processing pubmed JSON metadata


#processing the JSON files from the pubmed document additional metadata
processMetaJSON<-function(pmid = NULL,tmp=NULL){
  if("error" %in% names(tmp)){
    #There is no document summary information available, just result NA
    return(c(NA,NA,NA,NA,NA))
  }else{
    #get DOI & PMC
    altID<-tmp$articleids %>% 
      filter(idtype %in% c("pmc","doi","pubmed")) %>% 
      select(idtype,value)%>%
      tidyr::spread(idtype,value,NA)
    
    # if there are missing values, fill them in
    # since stuff might not be consistently returned
    if(is.null(altID$doi)){altID$doi<-NA}
    if(is.null(altID$pmc)){altID$pmc<-NA}
    if(is.null(altID$pubmed)){altID$pubmed<-NA}
    
    #pmc ref count
    pmcrefCount<-tmp$pmcrefcount
    if(pmcrefCount=="" | is.null(pmcrefCount)){pmcrefCount<-NA}
    
    # clearly add the pubtype (i.e. journal article)
    pubtype<-tmp$pubtype
    if(is.null(pubtype)){
      pubtype<-NA
    }else if(class(pubtype)=="list"){
      pubtype <- "Journal Article" #i chose this default
    }
    
    #also add subtype if it's there (how i've interpretted this)
    #seems like sometimes the second option is a specific subtype (like a review)
    if(length(pubtype)>1){ pubtype<-pubtype[2]}
    
    # finally, the language 
    lang<-tmp$lan
    if(is.null(lang) | class(lang) == "list"){lang<-NA}
    
    #if statement is a sanity check to make sure the records are the same
    if(altID$pubmed == pmid){
      return(c(pubtype,lang,pmcrefCount,altID$pmc,altID$doi))
    }else{
      return(c(NA,NA,NA,NA,NA))
    } 
  }
}


#get some highly cited (by PMC internal count) papers from a cluster
getTopPapers<-function(corpus=NULL,hoveredCluster=NULL){
  topPapers<-""
  if(!is.null(hoveredCluster)){
    topRef<-corpus %>%
      filter(tsneClusterNames %in% hoveredCluster$tsneClusterNames[1]) %>%
      mutate(pmcCitationCount = as.numeric(as.character(pmcCitationCount)))%>%
      filter(!is.na(pmcCitationCount))
   
    
      if(nrow(topRef)>0){
        topRef<-topRef%>%
          arrange(-pmcCitationCount)%>%
          ungroup()%>%
          top_n(5,pmcCitationCount)
      
        #really only return 5 if there are alot of ties
        if(nrow(topRef)>5){
          topRef<-topRef[1:5,]
        }
        
        #summarize this all into a word thing to output
        tmp2<-select(topRef,"PMID","YearPub","Journal","Authors","Title")
        topPaperText<-apply(tmp2,1,function(x){
          pmid = x[1]
          year = x[2]
          journal = x[3]
          authors =  paste(strsplit(as.character(x[4]),";")[[1]][1],"<em>et.al</em>")
          title = x[5]
          
          sprintf("%s (<strong>%s</strong>) <em>%s</em> <strong>%s</strong> PMID:<a target='_blank' href='https://www.ncbi.nlm.nih.gov/pubmed/%s'>%s</a>",authors,year,title,journal,pmid,pmid)
        })

        topPapers<-paste0(topPaperText,collapse="<br><br>")
        topPapers<-HTML(sprintf("<h4><em> Top Papers: </em></h4> %s",topPapers))
      }
  }

  return(topPapers)
}


#top terms in each cluster
topClustTerms<-function(corpus = NULL,corpusTidy=NULL,hoveredCluster = NULL){
  if(is.null(hoveredCluster)){
    NULL
  }else{
    
    #Find docs in cluster
    clustVal<-hoveredCluster$tsneClusterNames[1]
    pmids<-filter(corpus,tsneClusterNames %in% clustVal)%>%
      select(PMID)
    
    df<-corpusTidy %>%
      filter(PMID %in% pmids$PMID) %>% 
      group_by(wordStemmed) %>%
      dplyr::count()%>%
      ungroup()%>%
      mutate(freq = nn/nrow(pmids)) %>%
      arrange(-freq) %>% 
      top_n(10)
    
    topTerms<-paste(as.character(df$wordStemmed),collapse =",")
    return(HTML(sprintf("<h4><em> Top Terms </em><br></h4> %s </br>",topTerms)))
  }
}



#library(jsonlite)
#url<-"https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esummary.fcgi?db=pubmed&id=22368089&retmode=json"
#test<-fromJSON(url)
