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
source("../../R/process_pubmed.R")
source("../../R/tidy_corpus.R")
source("../../R/cluster_corpus.R")
source("../../R/explore_clusters.R")

set.seed(416) #repping the 6ix! 

#a variable that changes up some parameters depending on whether this is the demo version of the app or not
demoVersion<-FALSE
  
#Query Strings used for testing testing - I use a couple of example here
#queryString<-'"mycobacterium tuberculosis "[All Fields] AND "genome"[All Fields] AND (("2016/01/01"[PDAT] : "2017/10/01"[PDAT]) AND English[lang]) AND (("2016/01/01"[PDAT] : "2017/10/01"[PDAT]) AND English[lang])'
#queryString<-'"mycobacterium tuberculosis "[All Fields] AND (("2016/10/01"[PDAT] : "2017/10/01"[PDAT]) AND English[lang]) AND (("2016/01/01"[PDAT] : "2017/10/01"[PDAT]) AND English[lang])'
#queryString<-'(("prostatic neoplasms"[MeSH Terms] OR ("prostatic"[All Fields] AND "neoplasms"[All Fields]) OR "prostatic neoplasms"[All Fields] OR ("prostate"[All Fields] AND "cancer"[All Fields]) OR "prostate cancer"[All Fields]) AND ("genomics"[MeSH Terms] OR "genomics"[All Fields])) AND ("2014/10/01"[PDAT] : "2014/12/01"[PDAT])'
queryString<-'"IEEE transactions on visualization and computer graphics"[Journal]'


# ---- SHINY SPECIFIC FUNCTIONS ---
#From: https://github.com/rstudio/shiny-examples/blob/master/035-custom-input-bindings/url-input.R
dropNulls <- function(x) {
  x[!vapply(x, is.null, FUN.VALUE=logical(1))]
}

#function to update searchInput widget. Adapted from update awesomeCheckbox
updateSearchInput <- function (session, inputId, value = NULL) {
  message <- dropNulls(list(value = value))
  session$sendInputMessage(inputId, message)
}


## START THE SHOW
shinyServer(function(input, output,session) {

  #Kill browser deployed app on exit for non-demo version
  if(!demoVersion){ session$onSessionEnded(stopApp)}
  
  #Storage of reactive dataset values
  values<-reactiveValues(
    totalDocs = 0,
    corpus = NULL, #original document corpus 
    corpusTidy = NULL, # tidyText version of corpus
    subset = NULL,
    tsneObj = NULL,
    hoveredCluster= NULL,
    analysisProgress = FALSE
  )

  ##########################################
  # Events to support loading of dataset
  #
  # Two loading options: Search pubmed or load prior run
  
  
  # OPTION 1: USER SEARCHES PUBMED
  # once people click to search pubmed, do and store the data in 'values' variable
  
  # User can load example query string to observe functionality.
  # Example text loaded into search textbox
  observeEvent(input$loadExample,{
    updateSearchInput(session,"searchQuery",value = queryString)
  })
  
  # Observe is 'search' button is pushed to initate pubmed query
  observeEvent(input$searchQuery_search,{
    if(!values$analysisProgress){
      
       withProgress(message = 'Querying Pubmed', value = 0, {
        df<-processSearch(input$searchQuery,demoVersion) #search pubmed
       })
      
      values$corpus<-df #save corpus
      values$totalDocs<-nrow(df) #save total # of documents
      values$analysisProgress<-TRUE #indicate an analysis is now in progress
      
      #save file in storedRuns, unless demo (shinyapps.io) version is running
      if(!demoVersion){ 
        savedFileName<-paste("./storedRuns/",format(Sys.time(), "%Y-%m-%d_%H-%M"), "_temporaryStorage.RDS", sep = "")
        saveRDS(df,file=savedFileName)
      }
    }else{
      print("Please clear previous analysis before starting a new one")
    }
  })
  
  # OPTION 2: Users load PREVIOUS data
  # populate the dataset based upon uploading previous file
  observeEvent(input$prevAnalysis,{
    if(!values$analysisProgress){
      
      if(!is.null(input$prevAnalysis)){
        
        df<-readRDS(paste("./storedRuns/",input$prevAnalysis$name,sep=""))
        values$corpus<-df
        values$totalDocs<-nrow(df)
        values$analysisProgress <-TRUE
        
      }
      
    }else{
      print("Please clear previous analysis before starting a new one")
    }
  })
  
  
  ##########################################
  # Summaries of loaded dataset
  #
  # Populate the Search results tab with a summary message
  # and table of the search results. Enable the user to initate 
  # the clustering analysis
  
  #output some summary text of how many articles there are in a document
  output$summaryText<-renderUI({
    
    if(!is.null(values$corpus)){
     HTML(sprintf('<p>There are currently <strong>%d</strong> articles in the document corpus</p>',values$totalDocs))
    }else{
      NULL
    }
    
  })
  
  # text that user clicks on to begin downstream clustering analysis
  output$analysisButton <-
    renderUI(expr = if (!is.null(values$corpus)) {
      actionLink("analyzeCorpus","Click here to generate an automatic overview of the document corpus")
    } else {
      HTML(sprintf('<br><p><Strong>Nothing to report!</strong> Enter a search query first.</p>'))
    })
  
  #output some summary text of how many articles there are in a document
  output$summaryTextAnalysis<-renderUI({
    
    if(!is.null(values$tsneObj)){
      HTML("<br> <strong>Here are your results!</strong>. Have a lot of fun exploring")
    }else{
      HTML("<br> <strong>Nothing here yet!</strong> Enter a query to generate a document corpus and then select the option to start the analysis in the <em>'Search Results'</em> tab")
    }
    
  })
  
  # A data table containing the document corpus
  output$documentTable <- DT::renderDataTable({
    
    if(!is.null(values$corpus)){
      values$corpus %>%
        select(-contains("meshTerms"), -contains("Abstract"))
    }
    
  },server=TRUE,extensions = c('Responsive','Buttons','FixedHeader'), options = list(
    dom = 'Bfrtip',
    buttons = c('copy', 'csv', 'excel'),
    fixedHeader=TRUE
  ))
  
  
  ##########################################
  # Cluster Analysis of document corpus
  
  # Once the analysis button is clicked, autmatically go to that tab
  observeEvent(input$analyzeCorpus, {
    
    if(is.null(values$tsneObj)){
      
      # convert to document corpus to tidy text format
      withProgress(message = 'Making corpus tidy', 
                   detail = "\n Cleaning up the text data, removing stop words, calcuting td_idf metric, removing very common words", value = 0, {
        
        incProgress(amount = 0.2) #doesn't do much but visually pacifying
                     
        tidyCorpus_df<-tidyCorpus(values$corpus)
        values$corpusTidy<-tidyCorpus_df
      })
      #run tSNE the tidy corpus (note the runTSNE fuction 
      # will turn the tidy corpus into a document term matrix)
      withProgress(message = 'Dimensionality Reduction',
                   detail = "Running tsne on tidy corpus document term matrix",value = 0, {
                     
        incProgress(amount = 0.2) #doesn't do much but visually pacifying
                     
        tsneObj<-runTSNE(tidyCorpus_df,check_duplicates=FALSE)
        values$tsneObj<-tsneObj
      })
  
      #update document corpus with tsne info
      values$corpus<-inner_join(values$corpus,tsneObj$Y,by="PMID")
      
      #Using HDBSCAN, identify that optimal parameters for generating clusters
      withProgress(message = 'Clustering Corpus ', 
                   detail="Running HDBSCAN and finding optimal number of cluster", value = 0, {
        optClusters <- optimalParam(values$corpus)
        values$corpus<-inner_join(values$corpus,optClusters,by="PMID")
      })
      
      #name clusters according to top-two terms appear in cluster
      withProgress(message = "Namiing Clusters",
                   detail= "Data are now clustered and are being assigned names to make it easier to navigate", value = 0, {
        clustNames<-values$corpus %>%
          group_by(tsneCluster)%>%
          mutate(tsneClusterNames = getTopTerms(clustPMID = PMID,clustValue=tsneCluster,topNVal = 2,tidyCorpus=tidyCorpus_df)) %>%
          select(PMID,tsneClusterNames) %>%
          ungroup()
        
        #update document corpus with cluster names
        values$corpus<-inner_join(values$corpus,clustNames,by=c("PMID","tsneCluster"))
      })
      
      
      #if not the demo version, save the tsne, cluster object for further analysis
      if(!(demoVersion)){
        savedFileName<-paste("./storedRuns/", "TSNE_temporaryStorage.RDS", sep = "")
        saveRDS(values$corpus,file=savedFileName)
      }
      
      #Update the tabset panel to corpus view
      updateTabsetPanel(session, "overviewPanel",
                        selected = "Corpus Structure"
      )
      
    }
  })
  
  #As people hover over points in the tsnebox populate information about the cluster
  observe({
    if(!is.null(input$plot_brush)){
      values$hoveredCluster<-hoveredClusterSum(values$corpus,input$plot_brush)
    }
  })
  

  # In the cluster tab, and when the user brushes
  # over points in the tsnePlot, populate a 
  # box with some information about the cluster.
  # That informaiton is: the name of the cluster,
  # the top-ten most frequently occuring words
  # and the 5 most cited papers (according to pubmed PMC)
  output$exploreBox<-renderUI({
    if(!is.null(values$tsneObj)){
      shinydashboard::box(title="Explore Clusters",
        id="exploreClust",
        width=NULL,#column layout
        clusterSummaryText(values$hoveredCluster),
        topClustTerms(values$corpus,values$corpusTidy,values$hoveredCluster),
        getTopPapers(values$corpus,values$hoveredCluster)
      )
    }else{
      NULL
    }
  })
  
  ##########################################
  # Visualizations
  
  #Plot the tSNE result
  output$tsnePlot<-renderPlot({
    if(!is.null(values$corpus$tsneClusterNames)){
      
      df<-values$corpus
      
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
      
      if(input$showName){
        p<-p+geom_label(data=clusterNames,aes(x=medX,y=medY,label=tsneClusterNames),col="blue",size=3,check_overlap=TRUE,fontface="bold")
      }
      p
    }else{
      NULL
    }
  })
  
  #testing a box
  output$topTermsBox<-renderPlot({
    if(is.null(values$hoveredCluster)){
      NULL
    }else{
      
      #Find docs in cluster
      clustVal<-values$hoveredCluster$tsneClusterNames[1]
      pmids<-filter(values$corpus,tsneClusterNames %in% clustVal)%>%
        select(PMID)
      
      df<-values$corpusTidy %>%
        filter(PMID %in% pmids$PMID) %>% 
        group_by(wordStemmed) %>%
        dplyr::count()%>%
        ungroup()%>%
        mutate(freq = nn/nrow(pmids)) %>%
        arrange(-freq) %>% 
        top_n(10)
          
      
      p<-ggplot(df,aes(x=reorder(wordStemmed,freq),y=freq))+
        xlab("Keyword (stemmed)")+
        ylab("% of documents containing term")+
        scale_y_continuous(labels=scales::percent,limits=c(0,1))+
        geom_bar(stat="identity")+
        coord_flip()+
        theme_bw()
      
      p
    }
  })
  
})


  
  # columnDefs = list(list(
  #   targets = c(3,4,5),
  #   render = JS(
  #     "function(data, type, row, meta) {",
  #     "return type === 'display' && data.length > 100 ?",
  #     "'<span title=\"' + data + '\">' + data.substr(0, 100) + '...</span>' : data;",
  #     "}")
  # ))), callback = JS('table.page(3).draw(false);')
