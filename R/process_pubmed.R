#' Processing PubMed Search
#'
#' @description Wrapper interface for RISmed and adding additional article metadata.
#' @param query query for PubMed search
#' @param demoversion hard limit on PubMED retrieved records to 1000. Mainly used for easy transition to demo app version of Adjutant  [Default: FALSE]
#' @param ... arguments passed to RISmed::EUtilsSummary method
#'
#' @return corpus 
#'
#' @examples See online useage demonstration:https://github.com/amcrisan/Adjutant#demo
#' @import RISmed
#' @import dplyr
#' @import jsonlite
#' @import stringr
#' @export
processSearch<-function(query=NULL,demoversion=FALSE, ...){
  
  addedParam<- list(...)
  
  #Running Query on Pubmed - kinda just gets me PMIDS
  resQ1 <- EUtilsSummary(query=query, type='esearch', db='pubmed', ...)
  
  # return all queries, or if it's the demo version, only return the first 1000
  if(!demoversion){
    #if no retmax is specified, than, run this query to get more than 1000 PMIDS
    if(is.null(addedParam[['retmax']])){
      resQ1 <- EUtilsSummary(query, type='esearch', db='pubmed',retmax=resQ1@count, ...)
    }
  }
  
  pmidUnique<-unique(resQ1@PMID)
  
  #Gather data in batches and convert to data frame
  #format the data
  #need to put everything together in batches, 100 articles was the magic number before time outs occur
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
  
  return(corpus)
}


#formatting the pubmed data. Helper script to processSearch function
#retrieve and format data
formatData<-function(ids = NULL){
  #This here makes the queries into small manageable chucks so it doesn't time out.
  
  # TO DO: Switch from RISEmed to just parsing JSON files. Right now, esummary produces valid JSON
  # files that are nice and easy to parse. But efetch does not produce something nice and it seems
  # the best way to query through R right now is to use risemed for eftech. Apparently efetch is the only
  # part of the eUtils suite that DOESN'T yet properly return JSON. eSearch does and eSummary does.
    
    tmpids<-ids #sanity check for when results suddenly seem to get dropped
    pubResults<-EUtilsGet(paste0(tmpids,collapse = ","),type="efetch",db="pubmed")
    
    #make sure that results out = result in. EUtils 
    #its an odd fringe case, but this does actually happen
    #and it needs to be delt with more gracefully
    notThere<-which(!(tmpids %in% pubResults@PMID))
    if(length(notThere)>0){
      missingPMID<-tmpids[notThere]
      print("These are missing")
      print(missingPMID)
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
    url<-sprintf("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/esummary.fcgi?db=pubmed&id=%s&retmode=json",paste0(tmpids,collapse="+"))
    tmp<-fromJSON(url)
    
    metadata<-sapply(tmp$result[2:length(tmp$result)],function(x){
      processMetaJSON(x)
    }) %>% t() %>%
      data.frame(stringsAsFactors=FALSE) %>%
      mutate(X1=as.factor(X1))

    colnames(metadata)<-c("PMID","articleType","language","pmcCitationCount","pmcID","doi","Title")

    #putting all the data together into a data frame
    risResults<-data.frame(PMID=pubResults@PMID,
                     YearPub=pubResults@YearPubmed,
                     Journal=pubResults@Title,
                     Authors=authors,
                     Title=pubResults@ArticleTitle,
                     Abstract=pubResults@AbstractText,
                     stringsAsFactors = FALSE)
  
    
    allData<-dplyr::inner_join(risResults,dplyr::select(metadata,-contains("Title")),by="PMID")
    
    
    ### FIND AND CLEAN UP MISSING DATA
    #find missing titles and replace them with metadata version
    #really need to eventually replace rismed in pipeline
    
    missingInfo<-filter(allData,is.na(Title))
    
    if(nrow(missingInfo)>0){
      idxMatch<-match(missingInfo$PMID,metadata$PMID)
      idxdf<-match(missingInfo$PMID,allData$PMID)
      
      allData[idxdf,]$Title<-metadata[idxMatch,]$Title
    }
    
    #find missing abstracts and add them properly do the data
    missingInfo<-filter(allData,Abstract == "")
    
    if(nrow(missingInfo)>0){
      idxdf<-match(missingInfo$PMID,allData$PMID)
      missingAbs<-sapply(missingInfo$PMID,function(x){
        getMissingAbstract(x)
      })
      
      allData[idxdf,]$Abstract<-missingAbs
    }
    
    
    #adding those meshTerms
    allData$meshTerms<-meshTerms
    
    #finally, clean up the pmcCitationCOunt
    allData<-allData %>%
      mutate(pmcCitationCount = ifelse(is.na(pmcCitationCount),0,pmcCitationCount))
    
  return(allData)
}


#processing the JSON files from the pubmed document additional metadata
processMetaJSON<-function(tmp=NULL){
  if("error" %in% names(tmp)){
    #There is no document summary information available, just result NA
    return(c(NA,NA,NA,NA,NA,NA,NA))
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
    
    #storing title as backup, beacuse sometimes rise med doesn't properly parse the XML
    # due to HTML elements existing in the title (sigh...)
    
    
    articleTitle<-tmp$title
    articleTitle<-gsub("&lt;[//]?i&gt;","",articleTitle) #getting rid of italics <i> the most common HTML tag in title
    
    #pmc ref count
    pmcrefCount<-tmp$pmcrefcount
    if(pmcrefCount=="" | is.null(pmcrefCount)){pmcrefCount<-NA}
    
    # clearly add the pubtype (i.e. journal article)
    pubtype<-tmp$pubtype
    if(is.null(pubtype) | class(pubtype)=="list"){
      pubtype<-NA
    }
    
    #also add subtype if it's there (how i've interpretted this)
    #seems like sometimes the second option is a specific subtype (like a review)
    if(length(pubtype)>1){ pubtype<-pubtype[2]}
    
    # finally, the language 
    lang<-tmp$lan
    if(is.null(lang)){
      lang<-NA
    }else if (length(lang) > 1){
      lang<-paste0(lang,collapse=";")
    }
    
    
    #if statement is a sanity check to make sure the records are the same
    if(!is.na(altID$pubmed)){
      return(c(altID$pubmed,pubtype,lang,pmcrefCount,altID$pmc,altID$doi,articleTitle))
    }else{
      return(c(NA,NA,NA,NA,NA,NA,NA))
    } 
  }
}


getMissingAbstract<-function(PMID=NULL){
  test<-paste(readLines(sprintf("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=pubmed&id=%s",PMID)),collapse="\n")
  test<-str_extract(test,'abstract\\s+("([^"]|"")*")') %>% gsub("abstract","",.) %>% gsub('\\"',"",.) %>% gsub("\n","",.)
  return(test)
}