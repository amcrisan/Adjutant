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