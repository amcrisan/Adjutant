library(shiny)
library(RISmed)
library(rms)
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
source("../../R/tidy_corpus.R")
source("../../R/cluster_corpus.R")
source("../../R/explore_clusters.R")

set.seed(416) #repping the 6ix! 

#a variable that changes up some parameters depending on whether this is the demo version of the app or not
demoVersion<-FALSE
  
#Query Strings used for testing testing - I use a couple of example here
queryString<-'"mycobacterium tuberculosis "[All Fields] AND "genome"[All Fields] AND (("2016/01/01"[PDAT] : "2017/10/01"[PDAT]) AND English[lang]) AND (("2016/01/01"[PDAT] : "2017/10/01"[PDAT]) AND English[lang])'
#queryString<-'"mycobacterium tuberculosis "[All Fields] AND (("2016/10/01"[PDAT] : "2017/10/01"[PDAT]) AND English[lang]) AND (("2016/01/01"[PDAT] : "2017/10/01"[PDAT]) AND English[lang])'
#queryString<-'(("prostatic neoplasms"[MeSH Terms] OR ("prostatic"[All Fields] AND "neoplasms"[All Fields]) OR "prostatic neoplasms"[All Fields] OR ("prostate"[All Fields] AND "cancer"[All Fields]) OR "prostate cancer"[All Fields]) AND ("genomics"[MeSH Terms] OR "genomics"[All Fields])) AND ("2014/10/01"[PDAT] : "2014/12/01"[PDAT])'
#queryString<-'"IEEE transactions on visualization and computer graphics"[Journal]'


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
  
  #------------------------------------------------------------------------------------
  # REACTIVE VALUES AND DATA SETS
  #------------------------------------------------------------------------------------
  
  #Storage of reactive dataset values
  values<-reactiveValues(
    totalDocs = 0,
    corpus = NULL, #original document corpus 
    corpusTidy = NULL, # tidyText version of corpus
    subset = NULL,
    tsneObj = NULL,
    hoveredCluster= NULL,
    analysisProgress = FALSE,
    fileName = format(Sys.time(), "%Y-%m-%d_%H-%M")
  )

  #------------------------------------------------------------------------------------
  # REACTIVE EVENTS FOR LOADING DATA
  #------------------------------------------------------------------------------------
  
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
  
  # Observe if 'search' button is pushed to initate pubmed query
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
    
    #automatically go to the searchOverview
    updateTabItems(session, "sidebarTabs","searchOverview")
    
  })
  
  # OPTION 2: Users load PREVIOUS data
  # populate the dataset based upon uploading previous file
  observeEvent(input$prevAnalysis,{
    if(!values$analysisProgress){
      
      if(!is.null(input$prevAnalysis)){
        
        #df<-readRDS(paste("./storedRuns/",input$prevAnalysis$name,sep=""))
        df<-readRDS(input$prevAnalysis$datapath)
        values$corpus<-df
        values$totalDocs<-nrow(df)
        values$analysisProgress <-TRUE
        
      }
      
    }else{
      print("Please clear previous analysis before starting a new one")
    }
    
    #automatically go to the searchOverview
    updateTabItems(session, "sidebarTabs","searchOverview")
  })
  
  
  
  ##########################################
  # Summaries of loaded dataset
  #
  # Populate the Search results tab with a summary message
  # and table of the search results. Enable the user to initate 
  # the clustering analysis
  
  # A data table containing the document corpus
  output$documentTable <- DT::renderDataTable({
    
    if(!is.null(values$corpus)){
      values$corpus %>%
        select(-contains("meshTerms"), -contains("Abstract"))
    }
    
  },server=TRUE,extensions = c('Responsive','Buttons','FixedHeader'), options = list(
    dom = 'Bfrtip',
    buttons = c('csv', 'excel'),
    fixedHeader=TRUE
  ))
  
  
  #------------------------------------------------------------------------------------
  # REACTIVE EVENTS FOR DIMENSIONALITY REDUCTION & TOPIC CLUSTERING
  #------------------------------------------------------------------------------------
  
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
  

  
  #------------------------------------------------------------------------------------
  # REACTIVE EVENTS TO CLEAR THE PRESENT ANALYSIS 
  #------------------------------------------------------------------------------------
  observeEvent(input$sidebarTabs,{
    if(input$sidebarTabs == "clearAnalysis"){
      
      values$totalDocs <- 0
      values$corpus <- NULL #original document corpus
      values$corpusTidy <- NULL
      values$subset <- NULL
      values$tsneObj <- NULL
      values$hoveredCluster <- NULL
      values$analysisProgress <- FALSE
      
      updateTabsetPanel(session, "sidebarTabs", selected = "searchIn")
      
      updateSearchInput(session,"searchQuery",value = "")
    }
    
  })
  
  #------------------------------------------------------------------------------------
  # UI ELEMENTS (WIDGETS, MENUS) THAT ARE DATA DEPENDENT 
  #------------------------------------------------------------------------------------
  
  #menu item that displays the number of documents in the corpus
  output$searchMenu<-renderMenu({
    badgeLabel<-ifelse(is.null(values$corpus),"0",toString(nrow(values$corpus)))
    menuItem("Search Results", tabName = "searchOverview", icon = icon("book"),badgeLabel = badgeLabel)
  })
  
  #output some summary text of how many articles there are in a document
  output$summaryText<-renderUI({
    
    if(!is.null(values$corpus)){
      tmp<-values$corpus$Journal %>% unique() %>% length()
       
      HTML(sprintf("There are <b>%d</b> documents sourced from <b>%d</b> journals published from <b>%s</b> to <b>%s</b>", nrow(values$corpus),tmp,min(values$corpus$YearPub),max(values$corpus$YearPub)))
    }else{
      HTML("You need to start a search or load some data before you can have any results!")
    }
    
  })
  
  
  # a button that initiates the analysis of the document corpus!
  
  output$topicClustInitiateButton<-renderUI({
    if(!is.null(values$corpus) & is.null(values$tsneObj)){
      actionButton("analyzeCorpus",label="Inititate Topic Clustering")
    }else if(!is.null(values$tsneObj)){
      return(NULL)
    }else{
      return(NULL)
    }
  })
  
  
  
  #Some instruction text that tells user to run a search before they can do topic anlaysis
  output$corpusOverviewStatement<-renderUI({
    if(!is.null(values$corpus)){
      return(NULL)
    }else{
      HTML("<br> <strong>Nothing here yet!</strong> Search PubMed or load a prior analysis from the 'Search' menu item")
    }
    
  })
  
  #Allow user to enter alternative file name for analysis files
  output$analysisFileName<-renderUI({
    if(input$saveAnalysis){
      searchInput(
        inputId = "analysisName", 
        label = "File Names for Save Analysis:", 
        placeholder = sprintf("Enter alternative file"),
        value = values$fileName,
        btnSearch = icon("check"), 
        btnReset = icon("remove"), 
        width = "300px"
      )
    }else{
      return(NULL)
    }
  })
  
  
  #make summary buttons of topics 
  
  output$selectCluster<-renderUI({
    if(!is.null(values$corpus$tsneClusterNames)){
      #get clustername and size
      clustName<-values$corpus %>%
        group_by(tsneClusterNames) %>%
        count()%>%
        arrange(-n) %>%
        mutate(modName = sprintf("%s (%d)",tsneClusterNames,n)) 
      
      #from shiny widgets
      checkboxGroupButtons(
        inputId = "clustButtonSelect", label = "Click on a cluster to get more details! :", 
        choiceNames = clustName$modName, 
        choiceValues = clustName$tsneClusterNames,
        justified = FALSE, 
        status = "primary",
        individual=TRUE,
        checkIcon = list(yes = icon("ok", lib = "glyphicon"))
    )
    }else{
      NULL
    }
    
  })
  
  
  # In the topic cluster tab, and when the user brushes
  # over points in the tsnePlot, populate a 
  # box with some information about the cluster.
  # That informaiton is: the name of the cluster,
  # the top-ten most frequently occuring words
  # and the 5 most cited papers (according to pubmed PMC)
  output$clusterBox<-renderUI({
    if(!is.null(values$tsneObj)){
      shinydashboard::box(title="Cluster Details",
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
  
  
  #------------------------------------------------------------------------------------
  # VISUALIZATIONS
  #------------------------------------------------------------------------------------
 
  ################################
  # Document summary plots
  
  #years spanning the publication records
  output$yearPubPlot<-renderPlot({
    p<-NULL
    if(!is.null(values$corpus)){
      p<- values$corpus %>%
        group_by(YearPub) %>%
        count()%>% 
        ggplot(aes(x=YearPub,y=n))+
        geom_bar(stat="identity")+
        ylab("total # of papers")+
        xlab("Publication Year")+
        scale_x_continuous(limits=c(min(values$corpus$YearPub),max(values$corpus$YearPub)))+
        ggtitle("Publications per Year")+
        theme_bw()
    }
    p
    
  })
  
  #fiv most common journals
  output$journalPubTime<-renderPlot({
    p<-NULL
    
    if(!is.null(values$corpus)){
      #get the most published in journals for the document corpus
      journalCount<-values$corpus %>%
        group_by(Journal)%>%
        count() %>%
        arrange(-n)
      
      #make the plot
      p<-values$corpus %>%
        filter(Journal %in% journalCount$Journal[1:5]) %>%
        select(YearPub,Journal)%>%
        group_by(YearPub,Journal)%>%
        count()%>%
        ungroup() %>%
        ggplot(aes(x=YearPub,y=n))+
        geom_line(aes(colour=Journal),size=2)+
        geom_point(aes(colour=Journal),pch=21,fill="white",size=2)+
        labs(title="Top five journals where articles were published",
             x="Publication Year",
             y="% of documents published in that journal") +
        scale_x_continuous(limits=c(min(values$corpus$YearPub),max(values$corpus$YearPub)))+
        scale_colour_brewer(palette = "Set2")+
        theme_bw()
    }
    
    p
  })
  #top ten most common meshTerms over time
  output$meshTimePlot<-renderPlot({
    p<-NULL
    
    if(!is.null(values$corpus)){
      #meshTerms over time
      topTerms<-values$corpus%>% 
        select(YearPub,meshTerms) %>%
        transform(meshTerms = strsplit(meshTerms,";"))%>%
        tidyr::unnest(meshTerms) %>% 
        na.omit(meshTerms) %>%
        group_by(YearPub,meshTerms) %>%
        count()
      
      #overall top mesh terms
      overallCount <- topTerms %>%
        ungroup()%>%
        group_by(meshTerms)%>%
        tally(n)%>%
        top_n(10)
      
      #make the plot!
      p<-topTerms %>%
        ungroup()%>%
        filter(meshTerms %in% overallCount$meshTerms) %>%
        ggplot(aes(x=YearPub,y=n,group=meshTerms))+
        geom_line(aes(colour=meshTerms),size=2)+
        geom_point(aes(colour=meshTerms),pch=21,fill="white",size=2)+
        labs(title="Instances of ten most common mesh terms over time",
             subtitle="Note that not all articles have been assigned mesh terms ",
             x="Publication Year",
             y="# of documents containing the Mesh Term") +
        scale_colour_tableau("tableau10medium")+
        scale_x_continuous(limits=c(min(values$corpus$YearPub),max(values$corpus$YearPub)))+
        theme_bw()
      
    }
    
    p
  })
  
  #top 10 common mesh terms and their use over time
  
  output$meshWordCloud<-renderPlot({
    p<-NULL
    
    if(!is.null(values$corpus)){
      meshTermsCount<-values$corpus %>% 
        select(YearPub,meshTerms) %>%
        transform(meshTerms = strsplit(meshTerms,";"))%>%
        tidyr::unnest(meshTerms) %>% 
        transform(meshTerms = strsplit(meshTerms,",")) %>% #split out some of those concepts for worldcloud
        tidyr::unnest(meshTerms) %>% 
        na.omit(meshTerms) %>%
        group_by(meshTerms)%>%
        count()
      
      meshTermsCount<-meshTermsCount %>%
        mutate(freq = n/nrow(meshTermsCount))
      
      p<-wordcloud(meshTermsCount$meshTerms,meshTermsCount$freq,max.words=50,rot.per=0,main="mesh term wordcloud")
    }
    
    p
  })
  
  
  #now, make a list of the 10 most cited articles (according to PMC)
  
  output$topCorpusArticles<-renderUI({
    topPapers<-""
    
    if(!is.null(values$corpus)){
      topRef<-values$corpus%>%
        mutate(pmcCitationCount = as.numeric(as.character(pmcCitationCount))) %>% #for some reason this is a factor
        ungroup()%>%
        arrange(-pmcCitationCount)%>%
        top_n(10,pmcCitationCount)
      
      #summarize this all into a word thing to output
      tmp2<-select(topRef,"PMID","YearPub","Journal","Authors","Title")
      topPaperText<-apply(tmp2,1,function(x){
        pmid = x[1]
        year = x[2]
        journal = x[3]
        authors =  paste(strsplit(as.character(x[4]),";")[[1]][1],"<em>et.al</em>")
        title = x[5]
        
        sprintf("%s (<strong>%s</strong>) <strong>%s</strong> %s PMID:<a target='_blank' href='https://www.ncbi.nlm.nih.gov/pubmed/%s'>%s</a>",authors,year,title,journal,pmid,pmid)
      })
      
      topPapers<-paste0(topPaperText,collapse="<br><br>")
      topPapers<-HTML(sprintf("%s",topPapers))
    }
    
    topPapers
    
  })
  
  ################################
  # Clustering and t-SNE scatter plot
  
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
      
      if(!is.null(input$showName)){
        if(input$showName){
          p<-p+geom_label(data=clusterNames,aes(x=medX,y=medY,label=tsneClusterNames),col="blue",size=3,check_overlap=TRUE,fontface="bold")
        }
      }
      p
    }else{
      NULL
    }
  })
  
  #checkbox indidicating whether names should be overlain on the t-SNE plot
  output$overlayNamesCheckbox<-renderUI({
    if(!is.null(values$tsneObj)){
      checkboxInput("showName","Overlay cluster names",value=FALSE)
    }else{
      NULL
    }
  })
  
  # Define UI for the little cog wheel by the t-SNE plot
  # It will depend on the optimal parameters
  output$plotOptions<-renderUI({
    
    if(!is.null(values$corpus$tsneClusterNames)){
      dropdownButton(
        tags$h3("Change plot parameters"),
        p("Change Graph Appearance"),
        checkboxInput("showName","Overlay cluster names",value=FALSE),
        p("tsne parameters"),
        selectInput(inputId = 'xcol', label = 'X Variable', choices = names(iris)),
        br(),
        br(),
        p("hbscan parameters"),
        selectInput(inputId = 'ycol', label = 'Y Variable', choices = names(iris), selected = names(iris)[[2]]),
        sliderInput(inputId = 'clusters', label = 'Cluster count', value = 3, min = 1, max = 9),
        circle = TRUE, status = "danger", icon = icon("gear"), width = "300px",
        tooltip = tooltipOptions(title = "Click to see inputs !")
      )
      
     }else{
      NULL
    }
  })
  
  
  #------------------------------------------------------------------------------------
  # INFO SUMMARY BOX UI
  #------------------------------------------------------------------------------------
  
  
  #For search statement
  output$searchInfoStatement<-renderUI({
    HTML("<b><big>Search PubMed</big></b> <a href='#searchInfo'data-toggle='collapse'><small><em>(show search details)</small></em></a>
           <div id='searchInfo' class= 'collapse'>
         Begin your search here by entering a valid <a href='https://www.ncbi.nlm.nih.gov/pubmed/' target='_blank'><strong>PubMed</strong></a> search string. To form more specific search strings, please consult Pubmed directly and copy and paste the Pubmed generated search string here. You can also <strong> load prior analyses </strong>, which are automatically stored in the 'prior runs' folder everytime you run a search with Adjutant
         </div>")
  })
  
  
  #For search results tab
  output$searchResInfoStatement<-renderUI({
    HTML("<b><big>PubMed Search Results</big></b> <a href='#searchResultInfo'data-toggle='collapse'><small><em>(show search result details)</small></em></a>
           <div id='searchResultInfo' class= 'collapse'>
         Results from your Pubmed search will be displayed here. You can browse and download a table of your search results (referred to as the Document Corpus), or you are view a graphic summary of your results. Once you are satisfied with your search results you can initite an analysis to discover topic clusters within the document corpus.
         </div>")
  })
  
  
  #For topic clustering tab
  output$topicClustInfoStatement<-renderUI({
    HTML("<b><big>Topic Clustering</big></b> <a href='#topicClustInfo'data-toggle='collapse'><small><em>(show topic clustering details)</small></em></a>
               <div id='topicClustInfo' class= 'collapse'>
                <br>
                <p><em><b>Unsupervised cluster analysis of the document corpus</em></b><br> Adjutant uses article titles and abstracts to identify document clusters pertainaing to some topic. Given some set of documents, Adjutant creates a tidytext corpus using single terms <a href='https://www.tidytextmining.com/' target='_blank'>(see the excellent tidytext mining online book)</a>. Following some data wrangling and cleaning, which is detailed in our manuscript, Adjuntant calculates the <a href='https://www.tidytextmining.com/tfidf.html' target ='_blank'> td-idf metric </a> and generates a document term matrix (DTM) to prepare for cluster analysis. Our approach unsupervised clustering was to use t-SNE to dimensionally reduce the data, via the <a href='https://cran.r-project.org/web/packages/Rtsne/index.html' target ='_blank'>RTsne package</a>, followed by hdbscan, from the <a href='hhttps://cran.r-project.org/web/packages/dbscan/README.html' target ='_blank'>dbscan package</a>, to cluster documents. Adjutant tries to optimize the initial hbscan parameters, again detailed in our paper, but you can also choose your own parameters. Documents that are not part of any cluster are considered to be noise. Finally clustered are assigned topics based upon the two most frequently occuring terms in each cluster. </p>
<p><em><b>Exploring the topic clusters</em></b>
  <ul>
  <li>t-SNE scatter plot: the scatter plot below shows the 2D co-ordinates based upon dimensionality reduction of the DTM following t-SNE. The red shapes deliniate the cluster boundries, and you can double-click on a cluster to get more details in the <em>'Cluster Details'</em>. To change some t-SNE or hdbscan parameters click on the red cog.</li>
  <li>Cluster Details box: an overview of the cluster including the total number of members, top-ten frequently occuring terms, and the five most cited papers in that cluster (according to PubMed central citation counts).</li>
  </ul>
</p>
           </div>")
  })

})
