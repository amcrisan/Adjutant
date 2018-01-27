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

#additional analytic functions
source("R/process_pubmed.R")
source("R/tidy_corpus.R")
source("R/cluster_corpus.R")
source("R/explore_clusters.R")


df<-readRDS(file="storedRuns/2018-01-08_13-33_temporaryStorage.RDS")

#getting a tidy corpus
tidy_df<-tidyCorpus(corpus = df)

#running tsne on df
tsneObj<-runTSNE(tidy_df,check_duplicates=FALSE)
tsne_df<-inner_join(df,tsneObj$Y,by="PMID")

#cluster the tsneData

