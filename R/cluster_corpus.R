#' Wrapped for rTSNE
#'
#' @details A wrapper for the t-SNE method that creates a document term matrix from a tidy corpus, run t-SNE, and provides a cleaned up data frame output for further analysis.
#' @param tidyCorpus_df A tidy corpus. Run the "tidyCorpus" method to generate the expected input
#' @param ... parameters that can be passed to rTSNE
#'
#' @import dplyr
#' @import Rtsne
#' 
#' @return a data frame with with x,y t-SNE co-ordinates for
#' @export
#'
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
  
  cl <- hdbscan(tmp, ...)
  
  return(cl)
}


#' Find Optimal HDBSCAN parameters
#'
#' @details A function that tries to find the best possible minimum cluster size parameter (minPts) for HBSCAN. This method relies on the inputs from the runTSNE method. The appraoch of this method aims for a "good enough" classifcation rather than a globally optimized solution. The defintion of "good enough" for this function is the minimum number of clusters that best explain the data. To do this, the function first derives clusters  using the hdbscan algorithm (from the dbscan package) for minPts values to be tested.  The method then identifies the optimal minPts parameters by leveraging goodness-of-fit measurements derived from linear models, specifically the adjusted R^2 and the Bayesian Information Criteria (BIC); thus each minPts parameter value tested will have an associated R^2 and BIC measure. Adjutant makes this calculation by fitting separate linear models to each of the two t-SNE dimensions, where for each linear model the t-SNE component co-ordinates are used as the dependent variable and the clusters are used as the independent variables. Each cluster is a vector of membership probabilities, from 0 (not in the cluster) to 1 (definitely a cluster member). The adjusted R^2 between the two component models are multiplied, and the BICs are averaged. To choose the optimal minPts parameters, the method than identifies all minPts values with an adjusted R^$ within 0.05 of the best performing minPts value, and among those different options selects the minPts value with the lowest BIC.
#' 
#' @param corpus a document corpus
#' @param minPtsVal single term or vector of minPts parameter values to test
#'
#' @import dbscan
#' @import dplyr
#' @importFrom stats BIC as.formula dist lm median
#' @importFrom graphics text
#' @return data frame of clusters for each PMID
#' @export
#'
optimalParam<-function(corpus=NULL,minPtsVal = NULL){

  if(is.null(minPtsVal)){
    nDocs<-nrow(corpus)
    minPtsVals<-c(10,20,25,30,50,75,100,125,200)
    if(nDocs>50 & nDocs<100){
      minPtsVals<-c(5,10,20,30,50)
    }else if(nDocs>100 & nDocs<500){
      minPtsVals<-c(5,minPtsVals[1:(length(minPtsVals)-2)])
    }else if(nDocs>500 & nDocs<1000){
      minPtsVals<-c(minPtsVals,500) #trying out a bunch of different values
    }else if(nDocs >= 1000){
      minPtsVals<-c(minPtsVals,500,1000)
    }
  }
  #storing all the runs
  optValues<-vector("list",length(minPtsVals))
  
  
  #Run HDBSCAN A FEW TIMES AND SETTLE ON A MIN PT VALUE THAT SEEMS TO WORK WELL
  i=1
  for(minPt in minPtsVals){
    optValues[[i]]<-runOptPotential(corpus,minPt)
    i=i+1
  }
  
  
  #remove any list slots that were empty
  idx<-sapply(optValues,function(x){
    !is.null(x)
  })
  optValues<-optValues[idx]

  tmp<-getOptBest(optValues)
  clustFitScore<-tmp$clustFitScore
  clustFitSeveralBest<-tmp$clustFitSeveralBest

  
  #now really pick the best model
  if(nrow(clustFitSeveralBest)>1){
    clustFitSeveralBest<-clustFitSeveralBest %>% filter(BIC >0) %>% top_n(1,-BIC)
  }
  #The final most optimal choice
  optChoice<-optValues[[which(clustFitScore$minPtsVal== clustFitSeveralBest$minPtsVal)]]

  
  retItems<-data.frame(PMID = as.character(corpus$PMID),
                       tsneCluster = optChoice$clRes$cluster)
  
  return(list(retItems=retItems,altChoices = optValues, altChoicesTable=clustFitScore))
  
}

getOptBest<-function(optValues){
  #Now that several different models have been run, pick the best one
  #priority = r^2, then, least complex model (by BIC) preferred
  
  clustFitScore<-lapply(optValues,function(x){
    #c(x$minPt,x$regSum$r.sq,x$BIC)
    c(x$minPt,x$regSum,x$BIC)
  }) %>% do.call(rbind,.)
  
  clustFitScore<-data.frame(clustFitScore,stringsAsFactors = F)
  colnames(clustFitScore)<-c("minPtsVal","r.squared","BIC")
  
  bestCor<-max(clustFitScore$r.squared)
  
  clustFitSeveralBest<-clustFitScore %>%
    mutate(nextBest = bestCor - r.squared ) %>%
    filter(nextBest <0.05)%>%
    arrange(BIC)
  
  return(list(clustFitScore=clustFitScore,clustFitSeveralBest=clustFitSeveralBest))
}


#the task of searching for the optimal parameters
runOptPotential<-function(corpus=NULL,minPt=NULL){
    cl<-runHDBSCAN(corpus,minPt) 
    # usually once everything gets classified as noise there isn't 
    # much point in continuing. 
    if(length(unique(cl$cluster))==1){
      #stopping because there is not point!
      return(NULL)   
    }
    
    #combine info across runs
    tmp<-data.frame(PMID = as.character(corpus$PMID),
                    cluster = cl$cluster,
                    clusterName = paste0("clust",cl$cluster,sep=""),
                    clustMembership= cl$membership,
                    clustOutlier = cl$outlier_scores,
                    tsneComp1 = corpus$tsneComp1,
                    tsneComp2 = corpus$tsneComp2)
    
    
    #visualize the clusters for this run, store this because it's useful for debugging
    clusterNames <- tmp %>%
      dplyr::group_by(cluster) %>%
      dplyr::summarise(medX = median(tsneComp1),
                       medY = median(tsneComp2)) %>%
      dplyr::filter(cluster != 0)
    
    p<-ggplot(tmp,aes(x=tsneComp1,y=tsneComp2,group=cluster))+
      geom_point(alpha=0.2)+
      geom_label(data=clusterNames,aes(x=medX,y=medY,label=cluster),size=2,colour="red")+
      stat_ellipse(aes(alpha=cluster==0))+
      scale_alpha_manual(values=c(1,0))+
      theme_bw()
    
    #Essentially fit two linear regression model for each tsne component co-ordinates
    #and pick the model with the best adjusted R^2 and lowest BIC
    
    analysisMat<-tmp%>%
      ungroup()%>%
      filter(clusterName !="clust0")%>% 
      select(PMID,tsneComp1,tsneComp2,clusterName,clustMembership) %>% 
      tidyr::spread(clusterName,clustMembership,fill=0) #note the implicit sort, which is bad...
    
    colInfo<-colnames(analysisMat)
    f1 <- as.formula(paste('tsneComp1 ~', paste(colInfo[4:length(colInfo)], collapse='+')))
    f2 <- as.formula(paste('tsneComp2 ~', paste(colInfo[4:length(colInfo)], collapse='+')))
    
    tst1<-lm(f1,analysisMat)
    tst2<-lm(f2,analysisMat)
    
    BIC<-mean(c(BIC(tst1),BIC(tst2)))
    
    R2<-summary(tst1)$adj.r.squared *   summary(tst2)$adj.r.squared
    
    #return the results
    return(list(regSum=R2,fitPlot=p,BIC = BIC,minPt=minPt,clRes=cl))
    
}


#a function that names clusters
getTopTerms<-function(clustPMID=NULL,topNVal=1,clustValue = NA,tidyCorpus = NULL){
  clustValue<-clustValue[1]
  
  if(clustValue == 0)
    return("Not-Clustered")
  
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




#the task of searching for the optimal parameters
#altnerative and experimental version
runOptPotentialAlt<-function(corpus=NULL,minPt=NULL){
  
  cl<-runHDBSCAN(corpus,minPt) 
  # usually once everything gets classified as noise there isn't 
  # much point in continuing. 
  if(length(unique(cl$cluster))==1){
    #stopping because there is not point!
    return(NULL)   
  }
  
  
  #combine info across runs
  tmp<-data.frame(PMID = as.character(corpus$PMID),
                  cluster = cl$cluster,
                  clusterName = paste0("clust",cl$cluster,sep=""),
                  clustMembership= cl$membership,
                  clustOuterlier = cl$outlier_scores,
                  tsneComp1 = corpus$tsneComp1,
                  tsneComp2 = corpus$tsneComp2)
  
  
  #visualize the clusters for this run
  clusterNames <- tmp %>%
    dplyr::group_by(cluster) %>%
    dplyr::summarise(medX = median(tsneComp1),
                     medY = median(tsneComp2)) %>%
    dplyr::filter(cluster != 0)
  
  p<-ggplot(tmp,aes(x=tsneComp1,y=tsneComp2,group=cluster))+
    geom_point(alpha=0.2)+
    geom_label(data=clusterNames,aes(x=medX,y=medY,label=cluster),size=2,colour="red")+
    stat_ellipse()+
    theme_bw()
  

    #speed this up by sampling only the minimum # of points per cluster
  #now create a subset
  tmpSub<-tmp %>%
    filter(cluster!=0)%>% #will limit the amount of noise included
    group_by(clusterName)%>%
    sample_frac(0.2)

  #noise can often be very, and slow down the computation, so I will limit the amount of noise included
  sampNoiseMax<-ifelse(sum(tmp$cluster==0)>100,100,sum(tmp$cluster==0))
  tmpSubNoise<- tmp %>%
    filter(cluster==0)%>%
    sample_n(sampNoiseMax)

  tmpSub<-full_join(tmpSub,tmpSubNoise)
  remove(tmpSubNoise)
  gc()

  ptTsne<-cbind(tmpSub$tsneComp1,tmpSub$tsneComp2)
  ptDist<-dist(ptTsne)

  #use silhouette width as the univariate measure to the regression?
  sil<-cluster::silhouette(x=tmpSub$cluster,ptDist)
  silSum<-summary(sil)

  tmpSub$sil_width <- sil[,3] #sil width for inividuals points

  #it's hard for this data to be normal and get a good lm response,
  #so I am going to make it logistic regresion
  # 1= happily in a cluster 0=not happily in a cluster
  # I am choose an arbitraty threshold of 0.4 score or higher
  # It's not a probability but a score..


  analysisMat<-tmpSub%>%
    ungroup()%>%
    select(PMID,sil_width_box,clusterName,clustMembership) %>%
    tidyr::spread(clusterName,clustMembership,fill=0) #note the implicit sort, which is bad...

  f <- as.formula(paste('sil_width ~', paste(colInfo[3:length(colInfo)], collapse='+')))
  #run a multivariate regression
  colInfo<-colnames(analysisMat)
  #usually noise resolveds to nothing, so don't include that, clust the clustered stuff
  f <- as.formula(paste('sil_width ~', paste(colInfo[3:length(colInfo)], collapse='+')))

  regAttempt<-lrm(f,data = analysisMat)
  regAttemptSum<-summary(regAttempt)
  regAttemptBIC<-BIC(regAttempt)

  return(list(regSum=regAttemptSum,reg=regAttempt,fitPlot=p,silInfo = silSum ,BIC = regAttemptBIC,minPt=minPt,clRes=cl))
}


#------------ MIGHT GET BUMPED OUT --------

optimalParamOLD<-function(corpus = NULL){
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
    mutate(veryNoisey = ifelse(PMID %in% isVeryNoisey$PMID,"Not-Clustered","Signal")) %>%
    select(PMID,tsneCluster,veryNoisey)
  
  return(tmp)
}