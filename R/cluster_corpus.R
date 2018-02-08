#runs tSNE, paramters can be modified
# TO DO : doesn't work super well on very small corpuses because of tsne params
# either die gracefully or also improve param selection
runTSNE<-function(tidyCorpus_df=NULL, ...){

  dtm<-cast_dtm(tidyCorpus_df,PMID,wordStemmed,tf_idf)
  
  #Run tsne Analysis
  #Oddly, RTsne does not accept sparse matricies
  tsneObj<-Rtsne(as.matrix(dtm),...)
  
  #creating a data frame
  tsneObj$Y<-data.frame(cbind(rownames(dtm),tsneObj$Y),stringsAsFactors = F)
  colnames(tsneObj$Y)<-c("PMID",paste("tsneComp",1:(ncol(tsneObj$Y)-1),sep=""))
  
  #finally, just a tiny clean up to make sure the numeric values are numeric
  tsneObj$Y[, 2:ncol(tsneObj$Y)] <- sapply(tsneObj$Y[, 2:ncol(tsneObj$Y)], as.numeric) %>% unname()
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
  
  #identify several models with roughlyg similar R^2 performance
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