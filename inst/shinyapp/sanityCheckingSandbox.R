#testing out some visualizations to summarize data
library(shiny)
library(RISmed)
library(dplyr)
library(DT)
library(dplyr)
library(tidytext)
library(Rtsne)
library(SnowballC)
library(dbscan)
library(ggplot2)
library(stringr)
library(tidyr)
library(scales)
library(jsonlite)
library(ggthemes)
library(wordcloud)

#additional analytic functions
source("R/process_pubmed.R")
source("R/tidy_corpus.R")
source("R/cluster_corpus.R")
source("R/explore_clusters.R")


df<-readRDS(file="./inst/shinyapp/storedRuns/special2/2018-02-07_15-03_temporaryStorage.RDS")


#getting a tidy corpus
tidy_df<-tidyCorpus(corpus = df)

#running tsne on df
tsneObj<-runTSNE(tidy_df,check_duplicates=FALSE)
corpus<-inner_join(df,tsneObj$Y,by="PMID")


#tsnePlot
ggplot(corpus,aes(x=tsneComp1,y=tsneComp2))+
  geom_point(alpha=0.2)+
  theme_bw()

#run hdbscan
optClusters <- optimalParam(corpus)
corpus<-inner_join(corpus,optClusters,by="PMID")

clusterNames <- corpus %>%
  dplyr::group_by(tsneCluster) %>%
  dplyr::summarise(medX = median(tsneComp1),
                   medY = median(tsneComp2)) %>%
  dplyr::filter(tsneCluster != 0)

p<-ggplot(corpus,aes(x=tsneComp1,y=tsneComp2,group=tsneCluster))+
  geom_point(alpha=0.2)+
  geom_label(data=clusterNames,aes(x=medX,y=medY,label=tsneCluster),size=2,colour="red")+
  stat_ellipse()+
  theme_bw()


############ PLAY AREA BELOW 

#altnernative to HBDSCAN clustering
minPts<-c(10,20,30,50,75,100,125,200,500)
optValues<-vector("list",length(minPts))


i=1
for(minPt in minPts){
  tmp <- corpus %>%
    select(contains("tsneComp")) %>%
    as.matrix()
  
  cl <- hdbscan(tmp, minPts = minPt)
  
  # usually once everything gets classified as noise there isn't 
  # much point in continuing. 
  if(length(unique(cl$cluster))==1){
    #stopping because there is not point!
    break
  }
  
  
  
  #combine info across runs
  tmp<-data.frame(PMID = as.character(corpus$PMID),
                  cluster = cl$cluster,
                  clusterName = paste0("clust",cl$cluster,sep=""),
                  clustMembership= cl$membership,
                  clustOuterlier = cl$outlier_scores,
                  tsneComp1 = tmp[,1],
                  tsneComp2 = tmp[,2])
  
  
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
  
  
  
  #fit gam
  library(mgcv)
  
  Y<-cbind(tmp$tsneComp1,tmp$tsneComp2)
  
  analysisMat<-tmp%>%
    ungroup()%>%
    select(tsneComp1,tsneComp2,clusterName,clustMembership) %>%
    tidyr::spread(clusterName,clustMembership,fill=0) #note the implicit sort, which is bad...
  
  colInfo<-colnames(analysisMat)
  f1 <- as.formula(paste('tsneComp1 ~', paste(colInfo[3:length(colInfo)], collapse='+')))
  f2 <- as.formula(paste('tsneComp2 ~', paste(colInfo[3:length(colInfo)], collapse='+')))
  
  tst1<-lm(f1,analysisMat)
  tst2<-lm(f2,analysisMat)
  
  BIC<-mean(c(BIC(tst1),BIC(tst2)))
  
  R2<-summary(tst1)$adj.r.squared *   summary(tst2)$adj.r.squared
  
  optValues[[i]]<-list(regSum=R2,fitPlot=p,BIC = BIC,minPt=minPt)
  i=i+1
  
  # #Get Shillouette
  # #assumes p1 and p2 are always order x,y
  # pointDist<-function(p1,p2){return(sqrt((p2[1]-p1[1])^2 + (p2[2] - p1[2])^2))}
  # 
  # #speed this up by sampling only the minimum # of points per cluster
  # #now create a subset
  # tmpSub<-tmp %>%
  #   filter(cluster!=0)%>% #will limit the amount of noise included
  #   group_by(clusterName)%>%
  #   sample_frac(0.2)
  # 
  # #noise can often be very, and slow down the computation, so I will limit the amount of noise included
  # sampNoiseMax<-ifelse(sum(tmp$cluster==0)>100,100,sum(tmp$cluster==0))
  # tmpSubNoise<- tmp %>%
  #   filter(cluster==0)%>%
  #   sample_n(sampNoiseMax)
  # 
  # tmpSub<-full_join(tmpSub,tmpSubNoise)
  # remove(tmpSubNoise)
  # gc()
  #   
  # ptTsne<-cbind(1:nrow(tmpSub),tmpSub$tsneComp1,tmpSub$tsneComp2)
  # 
  # 
  # #--- FAST DISTANCE MATRIX CALCULATION
  # ptDist<-apply(ptTsne,1,function(x){
  #   rowDown<-x[1]
  #   if(rowDown == nrow(ptTsne)) return(0)
  #   apply(ptTsne[rowDown:nrow(ptTsne),],1,function(y){
  #     pointDist(x[c(2:3)],y[c(2:3)])
  #   })
  # })
  # 
  # #get the upper triangular matrix
  # ptDist<-sapply(ptDist,function(x){
  #   addZeros = nrow(ptTsne) - length(x)
  #   if(addZeros == 0){
  #     return(x)
  #   }else{
  #     return(c(rep(0,addZeros),x))
  #   }
  #   
  # }) %>% as.dist()
  # 
  # #use silhouette width as the univariate measure to the regression?
  # sil<-cluster::silhouette(x=tmpSub$cluster,ptDist)
  # 
  # tmpSub$sil_width <- sil[,3] #sil width for inividuals points
  # 
  # analysisMat<-tmpSub%>%
  #   ungroup()%>%
  #   select(PMID,sil_width,clusterName,clustMembership) %>%
  #   tidyr::spread(clusterName,clustMembership,fill=0) #note the implicit sort, which is bad...
  # 
  # 
  # #run a multivariate regression
  # colInfo<-colnames(analysisMat)
  # #usually noise resolveds to nothing, so don't include that, clust the clustered stuff
  # f <- as.formula(paste('sil_width ~', paste(colInfo[3:length(colInfo)], collapse='+')))
  # 
  # regAttempt<-gam(f,data = analysisMat,family="scat")
  # 
  # regAttemptSum<-summary(regAttempt)
  # regAttemptBIC<-BIC(regAttempt)
  # 
  # 
  #optValues[[i]]<-list(regSum=regAttemptSum,reg=regAttempt,fitPlot=p,BIC = regAttemptBIC,minPt=minPt)
  i=i+1
}

#find the model with the highest adjusted R2 for Y2 and Y1

lapply(optValues,function(x){
  c(x$regSum,x$BIC)
}) %>% do.call(rbind,.)
  






#give the clusters a name
clustNames<-corpus %>%
  group_by(tsneCluster)%>%
  mutate(tsneClusterNames = getTopTerms(clustPMID = PMID,clustValue=tsneCluster,topNVal = 2,tidyCorpus=tidy_df)) %>%
  select(PMID,tsneClusterNames) %>%
  ungroup()

#update document corpus with cluster names
corpus<-inner_join(corpus,clustNames,by=c("PMID","tsneCluster"))


corpus %>%
  group_by(tsneClusterNames) %>%
  count()%>%
  arrange(-n) %>%
  mutate(modName = sprintf("%s (%d)",tsneClusterNames,n)) %>% View()





# plot with clusters
df<-corpus

#seperate data object for cluster names
clusterNames <- corpus %>%
  dplyr::group_by(tsneClusterNames) %>%
  dplyr::summarise(medX = median(tsneComp1),
                   medY = median(tsneComp2)) %>%
  dplyr::filter(tsneClusterNames != "Noise")

#draw hulls around the clusters
chulls <- plyr::ddply(df, "tsneClusterNames", function(dat) dat[chull(dat$tsneComp1, dat$tsneComp2), ]) %>%
  select(tsneClusterNames,tsneComp1,tsneComp2) %>%
  filter(tsneClusterNames != "Noise") %>%
  na.omit()

#draw the tsne plot itself
p<-df %>%
  mutate(isNoise = ifelse(tsneCluster==0,"Noise","Signal"))%>% 
  ggplot(aes(x=tsneComp1,y=tsneComp2,group=tsneClusterNames))+
  geom_point(aes(colour=isNoise,alpha=isNoise))+
  geom_polygon(data = chulls,aes(x=tsneComp1,y=tsneComp2,group=tsneClusterNames),size=2,colour="red",fill=NA)+
  ylab("tsne comp. 2")+
  xlab("tsne comp. 1")+
  scale_size_manual(values=c(0,1))+
  scale_alpha_manual(values=c(0.2,0.5))+
  scale_colour_manual(values=c("#ade6e6","black"))+
  theme_bw()+
  theme(legend.position = "bottom")

p


### GET ARTICLE TITLE AND TEXT WHEN IT IS MISSING FROM PUB MED

test<-paste(readLines("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/efetch.fcgi?db=pubmed&id=29492318"),collapse="\n")
str_extract(test,'abstract\\s+("([^"]|"")*")') %>% gsub("abstract","",.) %>% gsub('\\"',"",.)


