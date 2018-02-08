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
source("../../R/process_pubmed.R")
source("R/tidy_corpus.R")
source("R/cluster_corpus.R")
source("R/explore_clusters.R")


df<-readRDS(file="./inst/shinyapp/storedRuns/special2/2018-02-07_15-03_temporaryStorage.RDS")


#getting a tidy corpus
tidy_df<-tidyCorpus(corpus = df)

#running tsne on df
tsneObj<-runTSNE(tidy_df,check_duplicates=FALSE)
tsne_df<-inner_join(df,tsneObj$Y,by="PMID")


#tsnePlot
ggplot(tsne_df,aes(x=tsneComp1,y=tsneComp2))+
  geom_point(alpha=0.2)+
  theme_bw()

#run hdbscan
optClusters <- optimalParam(corpus)
corpus<-inner_join(corpus,optClusters,by="PMID")


#give the clusters a name
clustNames<-corpus %>%
  group_by(tsneCluster)%>%
  mutate(tsneClusterNames = getTopTerms(clustPMID = PMID,clustValue=tsneCluster,topNVal = 2,tidyCorpus=tidy_df)) %>%
  select(PMID,tsneClusterNames) %>%
  ungroup()

#update document corpus with cluster names
corpus<-inner_join(corpus,clustNames,by=c("PMID","tsneCluster"))

# plot with clusters
df<-corpus

#seperate data object for cluster names
clusterNames <- df %>%
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




