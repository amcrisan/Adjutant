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
library(jsonlite)

source("additionalFunctions.R")


#a variable that changes up some parameters depending on whether this is the demo version of the app or not
demoVersion<-FALSE
  
#Query String used to testing
queryString<-'"mycobacterium tuberculosis "[All Fields] AND "genome"[All Fields] AND (("2016/01/01"[PDAT] : "2017/10/01"[PDAT]) AND English[lang]) AND (("2016/01/01"[PDAT] : "2017/10/01"[PDAT]) AND English[lang])'
#queryString<-'"mycobacterium tuberculosis "[All Fields] AND (("2016/10/01"[PDAT] : "2017/10/01"[PDAT]) AND English[lang]) AND (("2016/01/01"[PDAT] : "2017/10/01"[PDAT]) AND English[lang])'
#queryString<-'(("prostatic neoplasms"[MeSH Terms] OR ("prostatic"[All Fields] AND "neoplasms"[All Fields]) OR "prostatic neoplasms"[All Fields] OR ("prostate"[All Fields] AND "cancer"[All Fields]) OR "prostate cancer"[All Fields]) AND ("genomics"[MeSH Terms] OR "genomics"[All Fields])) AND ("2014/10/01"[PDAT] : "2014/12/01"[PDAT])'

#function to update searchInput. Adapted from update awesomeCheckbox
updateSearchInput <- function (session, inputId, value = NULL) {
  message <- dropNulls(list(value = value))
  session$sendInputMessage(inputId, message)
}

#From: https://github.com/rstudio/shiny-examples/blob/master/035-custom-input-bindings/url-input.R
dropNulls <- function(x) {
  x[!vapply(x, is.null, FUN.VALUE=logical(1))]
}

## START THE SHOW
shinyServer(function(input, output,session) {

  #Kill browser deployed app on exit for non-demo version
  if(!demoVersion){ session$onSessionEnded(stopApp)}
  
  #Storage of reactive dataset values
  values<-reactiveValues(
    totalDocs = 0,
    corpus = NULL,
    corpusTidy = NULL,
    subset = NULL,
    tsneObj = NULL,
    hoveredCluster=NULL
  )

  #populate the dataset based upon searching
  observeEvent(input$searchQuery_search,{
    df<-processSearch(input$searchQuery)
    values$corpus<-df
    values$totalDocs<-nrow(df)
    
    if(!demoVersion){ 
      savedFileName<-paste("./storedRuns/",format(Sys.time(), "%Y-%m-%d_%H-%M"), "_temporaryStorage.RDS", sep = "")
      saveRDS(df,file=savedFileName)
    }
  })
  
  #populate the dataset based upon uploading previous file
  observeEvent(input$prevAnalysis,{
    if(!is.null(input$prevAnalysis)){
      df<-readRDS(paste("./storedRuns/",input$prevAnalysis$name,sep=""))
      values$corpus<-df
      values$totalDocs<-nrow(df)
    }
  })
  
  #update search string with example
  observeEvent(input$loadExample,{
    updateSearchInput(session,"searchQuery",value = queryString)
  })
  
  #output some summary text of how many articles there are in a document
  output$summaryText<-renderUI({
    if(!is.null(values$corpus)){
     HTML(sprintf('<p>There are currently <strong>%d</strong> articles in the document corpus</p>',values$totalDocs))
    }else{
      NULL
    }
  })
  
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
  
  
  # Once the analysis button is clicked, autmatically go to that tab
  observeEvent(input$analyzeCorpus, {
    
    # Only run the analysis *ONCE* per search
    # TO DO: Clear the values if a new analysis has begun
    if(is.null(values$tsneObj)){
      #run the actual analysis, getting the tSNE object!
      tidyCorpus_df<-tidyCorpus(values$corpus)
      tsneObj<-runTSNE(tidyCorpus_df,check_duplicates=FALSE)
      
      #add these data objects to the running register
      values$corpusTidy<-tidyCorpus_df
      values$tsneObj<-tsneObj
      
      #add the co-ordinate information to the main corpus document
      values$corpus<-inner_join(values$corpus,tsneObj$Y,by="PMID")
      
      #save the tsne object too, it helps a tad with the debugged
      savedFileName<-paste("./storedRuns/", "TSNE_temporaryStorage.RDS", sep = "")
      saveRDS(values$corpus,file=savedFileName)
      
      #get the clusters for the tsne perplexity 100 plot using HDBSCAN
      optClusters <- optimalParam(values$corpus) #still working on this
      values$corpus<-inner_join(values$corpus,optClusters,by="PMID")
      
      #give names to the cluster points
      clustNames<-values$corpus %>%
        group_by(tsneCluster)%>%
        mutate(tsneClusterNames = getTopTerms(clustPMID = PMID,clustValue=tsneCluster,topNVal = 2,tidyCorpus=tidyCorpus_df)) %>%
        select(PMID,tsneClusterNames)
      
      values$corpus<-inner_join(values$corpus,clustNames,by="PMID")
      #corpus<-inner_join(corpus,tsneObj$Y,by="PMID")
      
      #Update the tabset panel
      updateTabsetPanel(session, "overviewPanel",
                        selected = "Corpus Structure"
      )
      
    }
  })
  
  # Documents within a selected cluster
  output$documentTable <- renderDataTable({
    if(!is.null(values$corpus)){
      values$corpus %>%
        select(-contains("meshTerms"), -contains("Abstract"))
    }
  },server=TRUE,extensions = c('Responsive','Buttons','FixedHeader'), options = list(
    dom = 'Bfrtip',
    buttons = c('copy', 'csv', 'excel'),
    fixedHeader=TRUE
  ))
  
  #Explore box output once analysis has been run
  output$exploreBox<-renderUI({
    if(!is.null(values$tsneObj)){
      shinydashboard::box(title="Explore Clusters",
        solidHeader = TRUE,
        collapsible = FALSE,
        width=NULL,#column layout
        computeClustSummary(values$corpus,input$plot_hover)
      )
    }else{
      NULL
    }
  })
  
  # VISUALIZATIONS
  
  #Plot the tSNE result
  output$tsnePlot<-renderPlot({
    if(!is.null(values$tsneObj)){
      
      df<-values$corpus
      
      #seperate data object for cluster names
      clusterNames <- df %>%
        group_by(tsneClusterNames) %>%
        summarise(medX = median(tsneComp1),
                  medY = median(tsneComp2)) %>%
        filter(tsneClusterNames != "Noise")
      
      #draw the tsne graph
      p<-df %>%
      mutate(isNoise = ifelse(tsneCluster.x==0,"Noise","Signal"))%>% 
      ggplot(aes(x=tsneComp1,y=tsneComp2,group=tsneClusterNames))+
        geom_point(aes(colour=isNoise,alpha=isNoise))+
        stat_ellipse(col="red",aes(size=isNoise))+
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
  
})


  
  # columnDefs = list(list(
  #   targets = c(3,4,5),
  #   render = JS(
  #     "function(data, type, row, meta) {",
  #     "return type === 'display' && data.length > 100 ?",
  #     "'<span title=\"' + data + '\">' + data.substr(0, 100) + '...</span>' : data;",
  #     "}")
  # ))), callback = JS('table.page(3).draw(false);')
