
#Run the initial query string

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
  #format the data
  #need to put everything together in batches, 200 articles was the magic number before time outs occur
  #in both risemed and also limits of what esummary will return
  numVals<-seq(from=0,to=length(pmidUnique),by=100)
  numVals<-c(numVals,tail(numVals, n=1) + (length(pmidUnique) %% 100))
    
  corpus<-c()
  for(i in 1:(length(numVals)-1)){
    start = numVals[i]+1
    end = numVals[i + 1]
    corpus <- rbind(corpus,formatData(pmidUnique[start:end]))
  }
  
  corpus<- dplyr::distinct(corpus)
  
  #finally check for any items in the corpus that DO NOT have either a title or abstract
  corpus<- corpus %>%
    filter()
  return(corpus)
}


#formatting the pubmed data. Helper script to processSearch function
#retrieve and format data
formatData<-function(ids = NULL){
  #limitation : extract resuts as sets of 200. There's a limit to how many
  #iI passed 2000 objects in, but 1/10th of that seems to be the magic # before timeoouts
  
  # if(length(ids) <100){
  #   #process all at once
  #   numVals<-c(1,length(ids))
  # }else{
  #   #break it up into chunks b.c it makes it easier to query
  #   numVals<-seq(from=0,to=length(ids),by=100)
  #   
  #   if(tail(numVals, n=1)< length(ids)){
  #     numVals<-c(numVals,tail(numVals, n=1) + (length(ids) %% 100))
  #   }
  # }
  
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
                      meshTerms=NULL,
                      stringsAsFactors = FALSE)
  
  
  #This here makes the queries into small manageable chucks so it doesn't time out.
  
  # TO DO: Switch from RISEmed to just parsing JSON files. Right now, esummary produces valid JSON
  # files that are nice and easy to parse. But efetch does not produce something nice and it seems
  # the best way to query through R right now is to use risemed for eftech. Apparently efetch is the only
  # part of the eUtils suite that DOESN'T yet properly return JSON. eSearch does and eSummary does.
  #for(i in 1:(length(numVals)-1)){
    #to make this faster, form new query on ID run in parallel
    #start = numVals[i]
    #end = numVals[i + 1]
    
    tmpids<-ids
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
    
    #allData<-rbind(allData,temp)
    
    allData<-temp
  
  return(allData)
}


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
