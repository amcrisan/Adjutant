library(adjutant) #!! We can do this now!!
library(RISmed)
library(purrr)
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
library(wordcloud)
library(ggthemes)
library(tcltk)

#additional analytic functions
set.seed(416) #repping the 6ix! 

#Query Strings used for testing testing - I use a couple of example here
queryString<-"(outbreak OR epidemic OR pandemic) AND genom*"
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

# custom update to checkboxGroupButtons so that I can get the deselect button to work
updatecheckboxGroupButtons_Custom <- function (session, inputId, value = NULL) {
 message <- list(selected = value)
 session$sendInputMessage(inputId, message)
}

#checking if the storedAnalysis folder exists, and create one if it does not
#this allows the analysis to be saved along the way

if(!dir.exists(paste0(workDir,"/storedAnalysis/"))){
  dir.create(file.path(workDir, "/storedAnalysis/"))
}

## START THE SHOW
shinyServer(function(input, output,session) {
  
  session$onSessionEnded(stopApp) #kill the app when the browser is closed
  
  #------------------------------------------------------------------------------------
  # REACTIVE VALUES AND DATA SETS
  #------------------------------------------------------------------------------------
  
  #Storage of reactive dataset values
  values<-reactiveValues(
    workDir = workDir,
    totalDocs = 0,
    corpus = NULL, #original document corpus 
    corpusTidy = NULL, # tidyText version of corpus
    corpusSubset = NULL, #subset of the data
    tsneObj = NULL,
    analysisProgress = FALSE,
    tnsePlot = NULL,
    tsnePer = 30,
    tsneTheta = 0.5,
    hdbscanMinClustSize = NULL,
    fileName = paste(format(Sys.time(), "%Y-%m-%d_%H-%M"),"documentCorpus",sep="_"),
    #------------------------
    # Note to future self 
    #----------------------
    # Making a 'deselect all' button (in topic discovery tab) work properly 
    # required some hacks, and the variables below all beginning with
    # clustInfo (+ the one tsne plot) are the side effects of those hacks. 
    # So - dear future self, when wondering WTF I made certain
    # choices, know it was present self's attempts and
    # ignorance and I'd like to see you do better.
    clustInfoDetailsUI = NULL,
    clustInfoClustOverview = NULL,
    clustInfoDetailsPlot = NULL,
    pTsneFinal=NULL,
    #---------------------------
    # For the demo version only
    #---------------------------
    reanalyzeNum = 0,
    demoVersion=FALSE
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
    updateTextInput(session,"retmax",value = 1000)
  })
  
  # Observe if 'search' button is pushed to initate pubmed query
  observeEvent(input$searchQuery_search,{
    #search query is not null
    if(!is.null(input$searchQuery_search) ){
      
      #make sure that there's actually text in the search query, and not empty spaces
      if(grepl("[a-zA-Z]+",input$searchQuery)){
           withProgress(message = 'Querying Pubmed', value = 0,
                        detail="This can take a few minutes depending upon how many articles there are. This step also looks up multiple sources of information too. For about one thousand articles it can take 2 - 3 minutes to grab all the data and format it for R.",{
             incProgress(amount = runif(1,min=0.1,max=0.4)) #doesn't do much but visually pacifying because it looks like things are happening
                          
            #creating a vector list of arguements
            searchArgs<-list(query = ifelse(is.null(input$searchQuery),NA,input$searchQuery),
                        demoversion = values$demoVersion,
                        retmax = ifelse(input$retmax == "",NA,as.numeric(input$retmax)), #as numeric failure defaults to NA
                        mindate = ifelse(!input$dateRange,NA,as.character(format(input$dateRangeVal[1],"%Y/%m/%d"))),
                        maxdate = ifelse(!input$dateRange,NA,as.character(format(input$dateRangeVal[2],"%Y/%m/%d")))
                        )
            #remove empty elements
            searchArgs<-searchArgs[sapply(searchArgs,function(x){!is.na(x)})]
            

            #search pubmed
            df<-do.call(processSearch,searchArgs)
           })
          
          if(!is.null(df)){
            values$corpus<-df #save corpus
            values$totalDocs<-nrow(df) #save total # of documents
            values$analysisProgress<-TRUE #indicate an analysis is now in progress
            
            #if an alternative analysis name is provided
            if(input$analysisName != values$fileName){
              analysisName<-gsub("\\s+","_",input$analysisName)
              values$fileName<-paste(analysisName,"documentCorpus",sep="_")
            }
            
            # save analysis
            if(input$saveAnalysis){ 
              savedFileName<-paste(values$workDir,"/storedAnalysis/", values$fileName,".RDS", sep = "")
              saveRDS(df,file=savedFileName)
            }
            
            #automatically go to the searchOverview
            updateTabItems(session, "sidebarTabs","searchOverview")
          }else{
            sendSweetAlert(
              session = session,
              title = "Could not get PubMed results",
              text = "Adjutant could not retrieve PubMed results. It's likely that there was  a problem with your Internet connection (including your firewall settings). Please check your internet connection and try your search again.",
              type = "error"
            )
          }
      }else{
        sendSweetAlert(
          session = session,
          title = "Invalid search string",
          text = "Check that you've entered a valid search string and retry your search",
          type = "error"
        )   
      }
    }
    
  })
  
  # OPTION 2: Users load PREVIOUS accepts only RDA files
  # populate the dataset based upon uploading previous file
  # throw an error if the input data is not RDA
  # TO DO: BE A BIT STRICTER OF WHAT PEOPLE CAN INCLUDE
  
  #Update the working directory if necessary
  observeEvent(input$chooseDir,{
    values$workDir<-tk_choose.dir()
  })
  
  
  output$loadAnalysisUI<-renderUI({
    if(dir.exists(paste0(values$workDir,"/storedAnalysis/"))){
      if(!grepl("storedAnalysis",values$workDir)){
        tmp<-paste0(values$workDir,"/storedAnalysis/")
      }else{
        tmp<-values$workDir
      }
      
      fileChoices<-unique(sapply(list.files(tmp),function(x){strsplit(x,"_documentCorpus")[[1]][1]}))
      
      selectInput("prevAnalysis",
                  label = "Choose and analysis to load",
                  choices = c("",fileChoices),
                  selected="",
                  multiple = FALSE)
    }else{
      HTML("<strong>Please choose a directory that contains a storedAnalysis folder with Adjutant compatible files</strong>")
    }
  })
  
  observeEvent(input$prevAnalysis,{
    if(!values$analysisProgress){
       if(input$prevAnalysis !=""){
         values$fileName <-input$prevAnalysis #general file names taken from existing project name
         
         #loading prior analysis files
         if(file.exists(paste0(values$workDir,"/storedAnalysis/", values$fileName,"_documentCorpus_topicClusters.RDS"))){
           df<-readRDS(paste0(values$workDir,"/storedAnalysis/", values$fileName,"_documentCorpus_topicClusters.RDS"))
           
           values$clusterNames <- df %>%
             dplyr::group_by(tsneClusterNames) %>%
             dplyr::summarise(medX = median(tsneComp1),
                              medY = median(tsneComp2)) %>%
             dplyr::filter(tsneClusterNames != "Not-Clustered")
           
         }else{
           #now load the data object
           df<-readRDS(paste0(values$workDir,"/storedAnalysis/", values$fileName,"_documentCorpus.RDS"))
         }
         
         #updating reactive values
         values$corpus<-df
         values$totalDocs<-nrow(df)
         values$analysisProgress <-TRUE
         
         
         #autodetect if there are other files that can also automatically be laoded
         if(file.exists(paste0(values$workDir,"/storedAnalysis/",values$fileName,"_documentCorpus_tidyText.RDS"))){
           values$corpusTidy<-readRDS(paste0(values$workDir,"/storedAnalysis/",values$fileName,"_documentCorpus_tidyText.RDS"))
         }
         
         updateTabItems(session, "sidebarTabs","searchOverview")
       }
    }
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
      #values$corpus %>%
      #  select(-contains("tsneClusterNames"), -contains("Abstract"))
      values$corpus[,c("PMID","YearPub","Journal","Authors" ,"Title","doi","articleType","pmcCitationCount","pmcID", "language","Abstract","meshTerms" )]
    }
    
  },server=TRUE,
  extensions = c('Responsive','Buttons','FixedHeader'), 
  options = list(
    autoWidth = TRUE,
    columnDefs = list(list(width = '450px', targets = c(4,5))),
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
      
      if(is.null(values$corpusTidy)){
          # convert to document corpus to tidy text format
          withProgress(message = 'Making corpus tidy', 
                      detail = "\n Cleaning up the text data, removing stop words, calcuting td_idf metric, removing very common words", value = 0, {
            
          incProgress(amount = runif(1,min=0.1,max=0.4)) #doesn't do much but visually pacifying because it looks like things are happening
                         
            tidyCorpus_df<-tidyCorpus(values$corpus)
            values$corpusTidy<-tidyCorpus_df
            
            if(input$saveAnalysis){
              savedFileName<-paste(values$workDir,"/storedAnalysis/", values$fileName,"_tidyText.RDS", sep = "")
              saveRDS(values$corpusTidy,file=savedFileName)
            }
          
        })
      }
        
      #run tSNE the tidy corpus (note the runTSNE fuction 
      # will turn the tidy corpus into a document term matrix)
      withProgress(message = 'Dimensionality Reduction',
                   detail = "\n Running tsne on tidy corpus document term matrix",value = 0, {
                     
        incProgress(amount = runif(1,min=0.1,max=0.4)) #doesn't do much but visually pacifying because it looks like things are happening
                     
        if(nrow(values$corpus)>=1000){
          values$tsnePer<-50
        }else if(nrow(values$corpus)<=100){
          values$tsnePer<-5
        }
        
        #run t-SNE 
        tsneObj<-runTSNE(tidyCorpus_df,check_duplicates=FALSE,perplexity=values$tsnePer)
        values$tsneObj<-tsneObj
      })
  
      #update document corpus with tsne info
      values$corpus<-inner_join(values$corpus,tsneObj$Y,by="PMID")
      
      #Using HDBSCAN, identify that optimal parameters for generating clusters
      withProgress(message = 'Clustering Corpus ', 
                   detail="\n Running HDBSCAN and finding optimal number of cluster", value = 0, {
        optClusters <- optimalParam(values$corpus)$retItems
        values$corpus<-inner_join(values$corpus,optClusters,by="PMID")
      })
      
      #name clusters according to top-two terms appear in cluster
      withProgress(message = "Naming Clusters",
                   detail= "Data are now clustered and are being assigned names to make it easier to navigate", value = 0, {
        incProgress(amount = runif(1,min=0.1,max=0.4)) #doesn't do much but visually pacifying because it looks like things are happening
        clustNames<-values$corpus %>%
          group_by(tsneCluster)%>%
          mutate(tsneClusterNames = getTopTerms(clustPMID = PMID,clustValue=tsneCluster,topNVal = 2,tidyCorpus=tidyCorpus_df)) %>%
          select(PMID,tsneClusterNames) %>%
          ungroup()
        
        #update document corpus with cluster names
        values$corpus<-inner_join(values$corpus,clustNames,by=c("PMID","tsneCluster"))
      })
      
      
      #save the cluster names data frame so I don't constantly recompute it
      withProgress(message = "Naming Clusters",
                   detail= "Data are now clustered and are being assigned names to make it easier to navigate", value = 0, {
        incProgress(amount = runif(1,min=0.1,max=0.4)) #doesn't do much but visually pacifying because it looks like things are happening
        values$clusterNames <- values$corpus %>%
          dplyr::group_by(tsneClusterNames) %>%
          dplyr::summarise(medX = median(tsneComp1),
                           medY = median(tsneComp2)) %>%
          dplyr::filter(tsneClusterNames != "Not-Clustered")
        })
      
      if(input$saveAnalysis){
        savedFileName<-paste(values$workDir,"/storedAnalysis/", values$fileName,"_topicClusters.RDS", sep = "")
        saveRDS(values$corpus,file=savedFileName)
      }
    }
    
  })
  

  #Allowing the user to enter custom tsne parameters, and re-running the results
  observeEvent(input$reanalyze,{

    if(values$demoVersion & values$reanalyzeNum>1){
      sendSweetAlert(
        session = session,
        title = "No more re-analysis for you!",
        text = "The demo version of Adjutant limits the number of times a corpus can be reanalyzed ",
        type = "error"
      )
    }else if(input$tsnePerplexity!=values$tsnePer | input$tsneTheta!=values$tsneTheta){
      paramOK<-TRUE
      
      #resetting the global parameters
      values$tsnePer<-input$tsnePerplexity
      values$tsneTheta <- input$tsneTheta
      
      #re-run tsne
      withProgress(message = 'Dimensionality Reduction',
                   detail = "\n Running tsne on tidy corpus document term matrix",value = 0, {
                     
          incProgress(amount = runif(1,min=0.1,max=0.4)) #doesn't do much but visually pacifying because it looks like things are happening

          tryCatch({
            tsneObj<-runTSNE(values$corpusTidy,perplexity = input$tsnePerplexity,check_duplicates=FALSE)
          },
          error = function(err){
            paramOK<<-FALSE #dbl arrow needed
            sendSweetAlert(
              session = session,
              title = "t-SNE perplexity parameter too large!",
              text = "Please choose a smaller value for the t-SNE perplexity parameter ",
              type = "error"
            )
          })
         })
      
          if(paramOK){
            tmp<-inner_join(x=values$corpus,y=tsneObj$Y,suffix=c("OLD","NEW"),by="PMID")
            values$tsneObj<-tsneObj
            
        
            values$corpus<- tmp %>%
              dplyr::select(-contains("OLD"))%>%
              transform(tsneComp1 = tsneComp1NEW,
                        tsneComp2 = tsneComp2NEW) %>%
              dplyr::select(-contains("NEW"))
           
        
          #re-run hbscan optimal params
          #Using HDBSCAN, identify that optimal parameters for generating clusters
          withProgress(message = 'Clustering Corpus ', 
                       detail="\n Running HDBSCAN and finding optimal number of cluster", value = 0, {
            optClusters <- optimalParam(values$corpus)$retItems
            tmp<-inner_join(x=values$corpus,y=optClusters,suffix=c("OLD","NEW"),by="PMID")
            
            incProgress(amount = 0.5)
            
            values$corpus<- tmp %>%
              dplyr::select(-contains("OLD"))%>%
              transform(tsneCluster = tsneClusterNEW) %>%
              dplyr::select(-contains("NEW"))
           })
          
          #re-name clusters
          clustNames<-values$corpus %>%
            group_by(tsneCluster)%>%
            mutate(tsneClusterNames = getTopTerms(clustPMID = PMID,clustValue=tsneCluster,topNVal = 2,tidyCorpus=values$corpusTidy)) %>%
            select(PMID,tsneClusterNames) %>%
            ungroup()
          
          #update document corpus with cluster names
          tmp<-inner_join(values$corpus,clustNames,by=c("PMID","tsneCluster"),suffix=c("OLD","NEW"))
          
          values$corpus<- tmp %>%
            dplyr::select(-contains("OLD"))%>%
            transform(tsneClusterNames = tsneClusterNamesNEW) %>%
            dplyr::select(-contains("NEW"))
          
          #saving the clusterNames
          values$clusterNames <- values$corpus %>%
            dplyr::group_by(tsneClusterNames) %>%
            dplyr::summarise(medX = median(tsneComp1),
                             medY = median(tsneComp2)) %>%
            dplyr::filter(tsneClusterNames != "Not-Clustered")
        
          #save the analysis
          #initize redrawing the plot
          
          if(input$saveAnalysis){
            saveRDS(values$corpus,paste0(values$workDir,"/storedAnalysis/", values$fileName,"_documentCorpus_topicClusters.RDS"))
          }
          values$tsnePlot<-NULL
          remove(tmp)
          gc()
          }
    }
    
  })
  
  # In the t-SNE plot, allow the user to double click on a cluster in order
  # to bring up information about that cluster in the detailed view
  observeEvent(input$plot_dbclick,{
    if(!is.null(input$plot_dbclick)){
      clickedClust<-clickedClusterSum(values$corpus,input$plot_dbclick)
      
      if(!is.null(clickedClust$tsneClusterNames)){
        if(clickedClust$tsneClusterNames != "Not-Clustered" & clickedClust$tsneClusterNames %in% input$clustButtonSelect){
          
          #update only if actual cluster is clicked on
          if(!is.null(input$clustDetails)){
            #make sure that whatever you clicked in is available as a choice..
            updateSelectInput(session,"clustDetails",selected = clickedClust$tsneClusterNames)
          }
        }
      }
    }
  })

  #------------------------------------------------------------------------------------
  # REACTIVE EVENTS FOR SUBSETTING DATA
  #------------------------------------------------------------------------------------
  observeEvent(input$filterGoButton,{
    #modify filJournalChoices to actually match
    filtJournalChoices<-gsub("\\s+\\(n=[0-9]+\\)","",input$filtJournalChoices)
    filtArticleChoices<-gsub("\\s+\\(n=[0-9]+\\)","",input$filtArticleChoices)
    
    datSub<-values$corpus %>%
      filter(Journal %in% filtJournalChoices)%>%
      filter(articleType %in% filtArticleChoices)%>%
      filter(YearPub >= input$filtYearChoices[1] & YearPub <= input$filtYearChoices[2])
      
    if(!is.null(input$filtTopicChoices)){
      filtTopicsChoices<-gsub("\\s+\\(n=[0-9]+\\)","",input$filtTopicChoices)
      
      datSub<-datSub %>%
        filter(tsneClusterNames %in% filtTopicChoices)
    }
    
    #only include known open articles (i.e. has a PMC ID)
    if(input$filtIsOpenChoices){
      datSub <- datSub %>%
        filter(!is.na(pmcID))
    }
      
    
    #Generating sample weights if weighted sampling is selected
    if(input$sampleWeight== "citation"){
      #calculate weights by citation #
      maxCite<-max(as.numeric(datSub$pmcCitationCount)+1,na.rm=TRUE) #add 1 so that none f the sampling proabilities are zero
        datSub<-datSub %>%
            mutate(sampWeight = (as.numeric(pmcCitationCount)+1)/maxCite) 
      }else if(input$sampleWeight == "recency"){
        maxYear<-max(datSub$YearPub)
        minYear<-min(datSub$YearPub)
        
        datSub<-datSub %>%
          mutate(sampWeight = (YearPub - minYear)+1)
      }
    
    #now sampling
    
    if(input$sampChoices == "random"){
      if(input$sampleWeight !="none"){
        datSub<-datSub %>%
          sample_n(size=input$filtSampleSizeChoice,weight=sampWeight)
      }else{
        datSub<-datSub %>%
          sample_n(size=input$filtSampleSizeChoice)
      }
      
    }else if(input$sampChoices == "randomStratified"){
      groupVal<-"Journal"
      
      if(input$stratifiedChoice == "Publication Year"){
        groupVal<-"YearPub"
      }else if(input$stratifiedChoice == "Topic"){
        groupVal<-"tsneClusterNames"
      }
   
      #check the input size and and modify as need
      sizePerGroup<-datSub %>%
        group_by_(groupVal) %>%
        count() %>%
        mutate(groupSampSize = ifelse(n < input$filtSampleSizeChoice,n,input$filtSampleSizeChoice)) %>%
        dplyr::select_(groupVal,"groupSampSize")
      
      
      if(input$sampleWeight !="none"){
        datSub<-datSub %>%
          ungroup()%>% #in case any are left over
          group_by_(groupVal)%>%
          nest() %>%
          inner_join(sizePerGroup) %>%
          mutate(samp = purrr::map2(data, groupSampSize,function(x,y){dplyr::sample_n(x,y,FALSE,weight=sampWeight)} ))%>%
          select_(groupVal, "samp") %>%
          unnest()
      }else{
        datSub<-datSub %>%
          ungroup()%>% #in case any are left over
          group_by_(groupVal)%>%
          nest() %>%
          inner_join(sizePerGroup) %>% 
          mutate(samp = purrr::map2(data, groupSampSize,sample_n))%>%
          select_(groupVal, "samp") %>%
          unnest()
      }    
    }else if(input$sampChoices == "ranked"){
      datSub <- datSub %>%
        filter(pmcCitationCount>0)%>%
        top_n(input$filtSampleSizeChoice,pmcCitationCount)
      
    }else if(input$sampChoices == "rankedStratified"){
      groupVal<-"Journal"
      
      if(input$stratifiedChoice == "Publication Year"){
        groupVal<-"YearPub"
      }else if(input$stratifiedChoice == "Topic"){
        groupVal<-"tsneClusterNames"
      }
    
      datSub <- datSub %>%
        ungroup()%>%
        filter(pmcCitationCount>0)%>% 
        group_by_(groupVal)%>%
        top_n(input$filtSampleSizeChoice,pmcCitationCount)
    }
    
    values$corpusSubset<-datSub
    remove(datSub)
    gc()
    
    if(input$saveAnalysis){
      savedFileName<-paste(values$workDir,"/storedAnalysis/", values$fileName,"_subset.RDS", sep = "")
      saveRDS(values$corpusSubset,file=savedFileName)
    }
      
  })
  
  #------------------------------------------------------------------------------------
  # REACTIVE EVENTS FOR CLEARING THE PRESENT ANALYSIS  (WARNING & ACTION)
  #------------------------------------------------------------------------------------
  observeEvent(input$sidebarTabs,{
    if(input$sidebarTabs == "searchIn" & values$analysisProgress){
      sendSweetAlert(
        session = session, 
        title = "Please Clear Analysis First!", 
        text = "Please clear the current analysis (see option in sidebar panel)  before entering a new search term or loading a previous analysis (don't worry, all data from you current analysis is automatically saved in the 'storedAnalysis' folder)", 
            type = "error"
          )
          
        #automatically go to the searchOverview
        updateTabItems(session, "sidebarTabs","searchOverview")
      }else if(input$sidebarTabs == "clearAnalysis"){
      
      values$totalDocs <- 0
      values$corpus <- NULL #original document corpus
      values$corpusTidy <- NULL
      values$subset <- NULL
      values$tsneObj <- NULL
      values$analysisProgress <- FALSE
      values$clusterNames<-NULL
      values$tsnePlot<-NULL
      values$pTsneFinal<-NULL
      
      #update some straggling widgets
      updateTabsetPanel(session, "sidebarTabs", selected = "searchIn")
      
      updateSearchInput(session,"searchQuery",value = "")
      
      updatecheckboxGroupButtons_Custom(session,"searchQuery",NULL)
      
      sendSweetAlert(
        session = session,
        title = "Previous Analysis Cleared",
        text = "",
        type = "success"
      )
    }
    
  })
  
  #------------------------------------------------------------------------------------
  # UI ELEMENTS (WIDGETS, MENUS, SUMMARY TEXT STATEMENTS, BOXES) THAT ARE DATA DEPENDENT 
  #------------------------------------------------------------------------------------
  
  #######################
  # SIDE BAR UI ELEMENTS
  #######################
  # menu item that displays the number of documents in the corpus
  # commented out, it's over eager and updates all the time causing problems
  
 # output$searchMenu<-renderMenu({
#    badgeLabel<-ifelse(is.null(values$corpus),"0",toString(nrow(values$corpus)))
#    menuItem("Search Results", tabName = "searchOverview", icon = icon("book"),badgeLabel = badgeLabel)
#  })
  
  #######################
  # SEARCH INPUT UI ELEMENTS
  #######################
  
  # If user choose to save their analysis (default option)
  # Allow user to enter their own file name. Default file name
  # Is based upon date & time
  # Note to self : I add the loadData condition so that
  # a user can't enter a new file name, switch to the load data tab
  # a save a bunch of new files
  output$analysisFileName<-renderUI({
    if(input$saveAnalysis & input$loadData !="Load Data"){
      textInput("analysisName",
                label = "File name prefix for analysis (saved in storedAnalysis folder):",
                value = values$fileName)
    # }else if(!input$saveAnalysis & input$loadData == "Load Data"){
    #   textInput("analysisName",
    #             label = "File name prefix for revised cluster analysis (saved in priorAnalysis folder):",
    #             value = values$fileName)
    }else{
      NULL
    }
  })
  
  
  ###################################
  # SEARCH RESULTS OVERVIEW ELEMENTS
  ###################################
  
  #output some summary text of how many articles there are in a document
  output$summaryText<-renderUI({
    if(!is.null(values$corpus)){
      tmp<-values$corpus$Journal %>% unique() %>% length()
       
      HTML(sprintf("There are <b>%d</b> documents sourced from <b>%d</b> journals published from <b>%s</b> to <b>%s</b>", nrow(values$corpus),tmp,min(values$corpus$YearPub),max(values$corpus$YearPub)))
    }else{
      HTML("You need to start a search or load some data before you can have any results!")
    }
    
  })
  
  
  ###################################
  # TOPIC DISCOVERY ELEMENTS
  ###################################
  
  # a button that initiates the analysis of the document corpus!
  output$topicClustInitiateButton<-renderUI({
    if(!is.null(values$corpus) & is.null(values$corpus$tsneClusterNames)){
      if(nrow(values$corpus)>=150){
        actionButton("analyzeCorpus",label="Initiate Topic Clustering")
      }else{
        #too few articles to properly run t-SNE
        NULL
      }
    }else if(!is.null(values$corpus$tsneClusterNames)){
      NULL
    }else{
      NULL
    }
  })
  
  
  #Text  that summmarised the cluster results, or document statement if cluster analysis not run yet
  output$clusterOverviewStatement<-renderUI({
    if(!is.null(values$tsneObj)){
      totalDoc<-nrow(values$corpus)
      totalClust<-values$corpus %>% filter(tsneClusterNames !="Not-Clustered") %>% count()
      perClust<-paste0(round((totalClust/totalDoc)*100,1),"%")
      
      nClust<-length(unique(values$corpus$tsneClusterNames))-1
      
      HTML(sprintf("Out of a total <b>%d</b> documents, <b>%d (%s)</b> were clustered within <b>%d</b> topics",totalDoc,totalClust$n,perClust,nClust))
      
    }else if(is.null(values$corpus)){
      HTML("<br> <strong>Nothing here yet!</strong> Search PubMed or load a prior analysis from the 'Search' menu item")
    }else if(nrow(values$corpus)<=150){
      HTML("Sorry, there are too few documents for topic clustering!")
    }else{
      NULL
    }
    
  })
  
  # Define UI for the little cog wheel by the t-SNE plot
  # It will depend on the optimal parameters
  output$plotOptions<-renderUI({
    if(!is.null(values$corpus$tsneClusterNames)){
      dropdownButton(
        tags$h3("Change t-SNE parameters"),
        br(),
        p("tsne parameters"),
        HTML("<small>The perplexity parameter indicates how many other documents t-SNE should consult in the dimensionality reductio process, and the theta parameter trades off speed for accuracy (0 slower, 1 faster)</small>"),
        numericInput("tsnePerplexity",label="Set tsne perplexity parameter",value=values$tsnePer,min=5,max = nrow(values$corpus)/5),
        numericInput("tsneTheta",label="Set tsne theta parameter",value=0.5,min=0,max = 1.0),
        actionButton("reanalyze","Reanalyze"),
        circle = TRUE,status = "danger", icon = icon("gear"), width = "350px",
        tooltip = tooltipOptions(title = "Change the plot parameters!")
      )
      
    }else{
      NULL
    }
  })
  
  
  #make summary buttons of topics 
  output$selectCluster<-renderUI({
    if(!is.null(values$corpus$tsneClusterNames)){
      #get clustername and size
      clustName<-values$corpus %>%
        filter(tsneClusterNames !="Not-Clustered") %>%
        group_by(tsneClusterNames) %>%
        count()%>%
        arrange(-n) %>%
        mutate(modName = sprintf("<b>%s</b> (%d)",tsneClusterNames,n)) 
      
      #from shiny widgets
      checkboxGroupButtons(
        inputId = "clustButtonSelect", 
        label = "", 
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
  
  #checkbox to select all clusters
  output$showAllClustNames<-renderUI({
    if(!is.null(values$corpus$tsneClusterNames)){
      fluidRow(
        p("   "), #lazy spacing fix
        actionButton("showAllClust",label="Select All Topic Clusters"),
        actionButton("deselectAllClust",label="Deselect All Topic Clusters")
      )
    }else{
      NULL
    }
  })

  
  ### GENERATION ALL OF THE CLUSTER DETAILS AND STORING THEM AS NEED  ### 
  observe({
    #The dropbdown box
    if(!is.null(values$corpus$tsneClusterNames)){
      if(!is.null(input$clustButtonSelect)){
        values$clustInfoDetailsUI<-selectInput("clustDetails", "Choose a cluster", choices= input$clustButtonSelect, multiple=FALSE)
      }else{
        values$clustInfoDetailsUI<- HTML("To get cluster details, select some cluster(s) by <em>clicking on the topic buttons</em>.")
      }
    }else{
      values$clustInfoDetailsUI<-HTML("Before you can see cluster details, you need clusters! Click on the 'initiate topic clustering' button to get started")
    }

  })
  
  #This actually works, and I am aghast!
  output$clusterSelect<-renderUI({
    values$clustInfoDetailsUI
  })
  
  #show detailed cluster information on selections
  observe({
    clustInfo<-NULL
    if(!is.null(values$corpus$tsneClusterNames)){
      if(!is.null(input$clustDetails) & !is.null(input$clustButtonSelect)){
        #when there is a reset event
        if(!is.null(values$corpus$tsneClusterNames)){
          #generate the summaries that will fill up the lovely topic box
          clustSum<-clusterSummaryText(values$corpus,input$clustDetails)
          clustTerms<-topClustTerms(values$corpus,values$corpusTidy,input$clustDetails)
          clustPapers<- getTopPapers(values$corpus,input$clustDetails)
          
          clustInfo<-HTML(paste(c(clustSum,clustTerms,clustPapers)))
          updateTabsetPanel(session=session,inputId ="exploreClust",selected="Explore Clusters")
        }
      }
    }
    
    values$clustInfoClustOverview<-clustInfo
  })
  
  #and now, output the cluster details
  output$clusterDetails<-renderUI({
    values$clustInfoClustOverview
  })
  
  
  #This is a silly way to make the deselect / select buttons work
  #for showing cluster labels
  #show base plot if user selects to deselect all cluster names
  observeEvent(input$deselectAllClust,{
    values$pTsneFinal<-values$tsnePlot
    updatecheckboxGroupButtons_Custom(session,"clustButtonSelect",NULL)
    
    #This is a bit hacky, but it does work
    values$clustInfoDetailsUI<-HTML("To get cluster details, select some cluster(s) by <em>clicking on the topic buttons</em>.")
    values$clustInfoClustOverview<-NULL
    values$clustInfoDetailsPlot<-NULL
    
    
  })
  
  # Summary text
  output$clusterDetailsNote<-renderUI({
    
    preamble<-NULL
    if(!is.null(values$clusterNames) & !is.null(input$clustDetails)){
    preamble<-HTML("If you've selected more than one cluster (topic) use the dropdown box below, or you can double click on the plot above, to get more details about a specific cluster <hr>")
    }
    preamble
  })
  
  #brushed papers
  output$brushedPapers<-renderUI({
    if(!is.null(input$plot_brush)){
      res<-brushedPoints(values$corpus,input$plot_brush)
      
      #summarize this all into a word thing to output
      tmp2<-select(res ,"PMID","YearPub","Journal","Authors","Title","tsneClusterNames")
      topPaperText<-apply(tmp2,1,function(x){
        pmid = x[1]
        year = x[2]
        journal = x[3]
        authors =  paste(strsplit(as.character(x[4]),";")[[1]][1],"<em>et.al</em>")
        title = x[5]
        clust = x[6]
        
        sprintf("[%s] %s (<strong>%s</strong>) <em>%s</em> <strong>%s</strong> PMID:<a target='_blank' href='https://www.ncbi.nlm.nih.gov/pubmed/%s'>%s</a><br><br>",clust,authors,year,title,journal,pmid,pmid)
      })
      
      return(HTML(topPaperText))
        
      if(nrow(res) == 0)
        return(HTML("There are no papers selected"))
    }else{
      return(HTML("Explore articles by brushing over data points. To initiate a brush action click on the plot, hold, and drag to create a box. You can move that box around to view articles. Single click anywhere on the plot to make the brush box ago away."))
    }
  })
  
  ###################################
  # SAMPLE DOCUMENTS ELEMENTS
  ###################################
  
  # Choice widget: Sampling weighting choices
  output$weightedSampleOptions<-renderUI({
    if(input$sampChoices %in% c("random","randomStratified")){
      radioButtons("sampleWeight",
                   label = "Selected option for  sampling weighting",
                   choiceNames = c("None - all articles are equally likely to be sampled",
                                   "PMC Citation - articles with higher citations are more likely to be sampled",
                                   "Recency - articles that have been recently published are more likely to be sampled") ,
                   choiceValues = c("none","citation","recency"))
    }else{
      NULL
    }
  })
  
  # Choice widget: Sampling weighting choices
  output$stratifiedSampleOptions<-renderUI({
    
    
    if(input$sampChoices=="randomStratified" | input$sampChoices=="rankedStratified"){
      choices<-c("Journal","Publication Year")
      
      if(!is.null(values$corpus$tsneClusterNames)){
        choices<-c(choices,"Topic")
      }
      
      selectInput("stratifiedChoice",
                  width="100%",
                  label="Choose one strata",
                  choices = choices,
                  multiple=FALSE)
      
    }else{
      NULL
    }
  })
  

  #Choice widget: Sampling weighting choices
  output$sampleSize<-renderUI({
    if(input$sampChoices %in% c("ranked","rankedStratified","random","randomStratified") & !is.null(values$corpus)){
    
      label <- "Choose total sample size"
      value<-nrow(values$corpus)
      
      if(input$sampChoices == "randomStratified" || input$sampChoices == "rankedStratified"){
        label <- "Choose # of articles to sample per strata"
        value <- 1
      }
      numericInput("filtSampleSizeChoice",
                   label = label,
                   value = value,
                   min = 1,
                   max = nrow(values$corpus))
    }else{
      NULL
    }
  })
  
  #Filter widget: filter by journal
  output$filtJournal<-renderUI({
    if(!is.null(values$corpus)){
      #articles coming from differen journals
      journ<-values$corpus %>%
        ungroup()%>% #just in case
        group_by(Journal) %>%
        count() %>%
        arrange(-n) %>%
        mutate(journalName = sprintf("%s (n=%d)",Journal,n ))
        
      pickerInput(
        inputId = "filtJournalChoices", 
        label = "Select journals to include (default: select all)", 
        width="100%",
        choices =  journ$journalName,
        selected = journ$journalName,
        options = list(
          `actions-box` = TRUE, 
          size = 10,
          `selected-text-format` = "count > 3"
        ), 
        multiple = TRUE
      )
    }else{
      NULL
    }
  })
  
  
  #Filted widget: filter articles by year
  output$filtYear<-renderUI({
    if(!is.null(values$corpus)){
      yearMin<-min(values$corpus$YearPub)
      yearMax<-max(values$corpus$YearPub)
      
      sliderInput("filtYearChoices",
                  label="Select years to include (default: select all)",
                  min=yearMin,
                  max=yearMax,
                  value = c(yearMin,yearMax),
                  sep="")
    }else{
      NULL
    }
  })
  
  #Filter widget: filter by article type
  output$filtArticleType<-renderUI({
    if(!is.null(values$corpus)){
      #articles coming from differen journals
      journ<-values$corpus %>%
        ungroup()%>% #just in case
        group_by(articleType) %>%
        count() %>%
        arrange(-n) %>%
        mutate(articleTypeName = sprintf("%s (n=%d)",articleType,n ))
      
      pickerInput(
        inputId = "filtArticleChoices", 
        label = "Select article types (defined by PubMed) to include (default: select all)", 
        width="100%",
        choices =  journ$articleTypeName,
        selected = journ$articleTypeName,
        options = list(
          `actions-box` = TRUE, 
          size = 10,
          `selected-text-format` = "count > 3"
        ), 
        multiple = TRUE
      )
    }else{
      NULL
    }
  })
  
  #Filter widget: filter by topic
  output$filtTopic<-renderUI({
    if(!is.null(values$corpus$tsneClusterNames)){
      #articles coming from differen journals
      journ<-values$corpus %>%
        ungroup()%>% #just in case
        group_by(tsneClusterNames) %>%
        count() %>%
        arrange(-n) %>%
        mutate(topicName = sprintf("%s (n=%d)",as.character(tsneClusterNames),n ))
      
      pickerInput(
        inputId = "filtTopicChoices", 
        label = "Select topics to include (default: select all)", 
        width="100%",
        choices =  journ$topicName,
        selected = journ$topicName,
        options = list(
          `actions-box` = TRUE, 
          size = 10,
          `selected-text-format` = "count > 3"
        ), 
        multiple = TRUE
      )
    }else{
      NULL
    }
  })
  
  #Filter widget: filter by whether article is open source
  output$filtIsOpen<-renderUI({
    if(!is.null(values$corpus)){
      checkboxInput("filtIsOpenChoices",
                    label="Sample *only* articles with full text available (default: no)",
                    value=FALSE,
                    width="100%")
    }else{
      NULL
    }
  })
  
  #Filter widget: filter by minimum number of citations
  output$filtMinCitation<-renderUI({
    
    if(!is.null(values$corpus)){
      #maxCite<-max(as.numeric(as.character(values$corpus$pmcCitationCount)))
      maxCite<-max(values$corpus$pmcCitationCount)
      numericInput("filtMinCitationChoice",
                   label="Select the minimum number of citations an article must have (warning! biases against more recent articles)",
                   min=0,
                   max=maxCite,
                   value=0)
    }else{
      NULL
    }
  })
  
  #Filter widget: filter by minimum number of citations
  output$filtTopic<-renderUI({
    if(!is.null(values$clusterNames)){
      clusterN<-values$corpus %>%
        ungroup()%>% #just in case
        group_by(tsneClusterNames) %>%
        count() %>%
        arrange(-n) %>%
        mutate(tsneClusterNamesFormatted = sprintf("%s (n=%d)",tsneClusterNames,n ))
      
      choicesNoNoise<-filter(clusterN,tsneClusterNames !="Not-Clustered")
      
      pickerInput(
        inputId = "filtTopicsChoices", 
        label = "Select topics (clusters) to include (default: select all except unclustered)", 
        width="100%",
        choices =  clusterN$tsneClusterNames,
        selected = choicesNoNoise$tsneClusterNames,
        options = list(
          `actions-box` = TRUE, 
          size = 10,
          `selected-text-format` = "count > 3"
        ), 
        multiple = TRUE
      )
    }else{
      NULL
    }
  })
  
  #UI element : Summarize the current sample subset
  output$subsetSummary<-renderUI({
    
    msg<-NULL
    
    if(!is.null(values$corpusSubset)){
      subSize  <- nrow(values$corpusSubset)
      totalSize <- nrow(values$corpus)
      
      if(nrow(values$corpusSubset) == 0 & grepl("ranked",input$sampChoices)){
        return("No articles meet your criteria. If this is a small search with few articles that have citations you may not want to use the ranked subsetting options")
      }
      
      
      msg<-sprintf("<em>Sample Summary</em><br>A total of <b>%d documents were sampled </b> out of a total possible %d documents.",subSize,totalSize)
      
      if(input$sampChoices=="randomStratified" || input$sampChoices=="rankedStratified" ){
        msg<-paste(msg,sprintf("Documents were sampled by <em>strata</em> according to <b>%s</b>, with <b>%d</b> documents  sampled per strata.", input$stratifiedChoice,input$filtSampleSizeChoice))
      }
      
      if(!is.null(input$sampleWeight)){
        if(input$sampleWeight =="citation"){
          msg<-paste(msg,"Sampling weights were applied, giving greater weight to articles with higher PMC citation")
        }else if(input$sampleWeight =="recency"){
          msg<-paste(msg,"Sampling weights were applied, giving greater weight more recently published articles.")
        }
      }
      
      return(HTML(msg))
    }else{
      return(msg)
    }
    
  })
  
  #Button widget: Apply filters
  output$filterButton<-renderUI({
    if(!is.null(values$corpus)){
      actionButton("filterGoButton",icon=icon("filter"),label="Subset Document Corpus")
    }else{
      NULL
    }
  })
  #Button widget: Downloading data
  output$downloadSubsetData<-renderUI({
    if(!is.null(values$corpusSubset)){
      downloadButton("downloadSub", label = "Export Subset")
    }else{
      NULL
    }
  })
  
  # Button widget handler: Downloading data
  output$downloadSub<- downloadHandler(
    filename = function() {
      paste(values$fileName,'_subset','.csv', sep='')
    },
    content = function(con) {
      write.csv(values$corpusSubset, con,quote=TRUE,row.names=FALSE)
    }
  )
  
  
  #------------------------------------------------------------------------------------
  # VISUALIZATIONS
  #------------------------------------------------------------------------------------
 
  ###################################
  # SEARCH RESULTS OVERVIEW VIZ
  ###################################
  
  #years spanning the publication records
  output$yearPubPlot<-renderPlot({
    p<-NULL
    if(!is.null(values$corpus)){
      p<-values$corpus %>%
        group_by(YearPub) %>%
        count()%>% 
        ggplot(aes(x=YearPub,y=n))+
        geom_bar(stat="identity")+
        ylab("total # of papers")+
        xlab("Publication Year")+
       # scale_x_continuous(limits=c(min(values$corpus$YearPub),max(values$corpus$YearPub)))+
        ggtitle("Publications per Year")+
        theme_bw()
    }
    p
    
  })
  
  #five most common journals
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
        #scale_x_continuous(limits=c(min(values$corpus$YearPub),max(values$corpus$YearPub)))+
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
        #scale_x_continuous(limits=c(min(values$corpus$YearPub),max(values$corpus$YearPub)))+
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
      
      p<-wordcloud(meshTermsCount$meshTerms,meshTermsCount$freq,
                   max.words=50,
                   scale=c(2,0.5),
                   min.freq=2,
                   rot.per=0,
                   main="mesh term wordcloud")
    }
    
    p
  })
  
  
  #now, make a list of the 10 most cited articles (according to PMC)
  
  output$topCorpusArticles<-renderUI({

    topPapers<-""
    
    if(!is.null(values$corpus)){
      topRef<-values$corpus%>%
        mutate(pmcCitationCount = as.numeric(pmcCitationCount)) %>% #for some reason this is a factor
        ungroup()%>%
        filter(pmcCitationCount>0)%>%
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
  
  ###################################
  # CLUSTERING and t-SNE
  ###################################
  
  # Plot the tSNE result, make this seperate from the cluster labels
  # so I don't recompute the plot every time I update the labels!
  observe({
    if(!is.null(values$corpus$tsneClusterNames) & is.null(values$tsnePlot)){
      
      df<-values$corpus
      
      #draw hulls around the clusters
      chulls <- plyr::ddply(df, "tsneClusterNames", function(dat) dat[chull(dat$tsneComp1, dat$tsneComp2), ]) %>%
        select(tsneClusterNames,tsneComp1,tsneComp2) %>%
        filter(tsneClusterNames != "Not-Clustered") %>%
        na.omit()
 
      
      #draw the tsne plot itself
      p<-df %>%
      mutate(isNoise = ifelse(tsneCluster==0,"Not-Clustered","Clustered"))%>% 
      ggplot(aes(x=tsneComp1,y=tsneComp2,group=tsneClusterNames))+
        geom_point(aes(colour=isNoise,alpha=isNoise))+
        geom_polygon(data = chulls,aes(x=tsneComp1,y=tsneComp2,group=tsneClusterNames),size=2,colour="red",fill=NA)+
        labs(title ="t-SNE Plot of Document Corpus",
             x ="t-SNE co-ordinate 1",
             y ="t-SNE co-ordinate 2")+
        scale_size_manual(values=c(0,1),name="")+
        scale_alpha_manual(values=c(0.3,0.7),name="")+
        scale_colour_manual(values=c("black","#ade6e6"),name="")+
        theme_bw()+
        theme(legend.position = "bottom",legend.text = element_text(size=14))
      
      values$tsnePlot<-p
      values$pTsneFinal<-p #for the selectall /deselect all button to work properly
    }else{
      NULL
    }
  })
  
  #if user selects all show all labels
  observeEvent(input$showAllClust,{
    #updatecheckboxGroupButtons(session,"clustButtonSelect",selected=values$clusterNames$tsneClusterNames)
    
    # I have to do this step too because updatecheckboxGroupButton has some quirks
    # that shiny doesn't seem to respond to (i.e. the dependent widgets don't seem to update)
    p<-values$tsnePlot
    
     if(!is.null(values$tsnePlot) & !is.null(values$clusterNames)){
       clusterNames<-values$clusterNames
         p<-p+geom_label(data=clusterNames,aes(x=medX,y=medY,label=tsneClusterNames),col="blue",size=3,check_overlap=TRUE,fontface="bold")
         
         #also update the topic buttons
         updateCheckboxGroupButtons(session,"clustButtonSelect",selected=clusterNames$tsneClusterNames)
         
         #also update the dropdown list
         values$clustInfoDetailsUI<-values$clustInfoDetailsUI<-selectInput("clustDetails", "Choose a cluster", choices= input$clustButtonSelect, multiple=FALSE)
     }
     
     values$pTsneFinal<-p
    
})
  
  
  #final show just the ones that the user has selected.
  observeEvent(input$clustButtonSelect,{
    p<-values$tsnePlot
    
    if(!is.null(input$clustButtonSelect)){
      clusterNames<-values$clusterNames %>%
        filter(tsneClusterNames %in% input$clustButtonSelect)
      
      #add labels to the plot
      p<-p+geom_label(data=clusterNames,aes(x=medX,y=medY,label=tsneClusterNames),col="blue",size=3,check_overlap=TRUE,fontface="bold")
    }
    values$pTsneFinal<-p
  })
  
  
  #finally send that plot to the UI layer!
  output$tsnePlot<-renderPlot({
    p<-values$pTsneFinal #plot will automatically be NULL if there's nothing there
    
    p
  })
  

  # in cluster details box, plot that shows topic membership over time
  #finally make a plot to show cluster growth over time.
  #if it seperate observe to deal with the UI resetting properly on deselect
  observe({
    detPlot<-NULL
    
    if(!is.null(values$corpus$tsneClusterNames) & !is.null(input$clustDetails) & !is.null(input$clustButtonSelect)){
      detPlot<-values$corpus %>%
        filter(tsneClusterNames == input$clustDetails) %>%
        ungroup()%>%
        group_by(YearPub)%>%
        count()%>%
        ggplot(aes(x=YearPub,y=n))+
        geom_bar(stat="identity")+
        labs(title="Cluster Membership Over Time",
             xlab = "#of articles published",
             ylab = "Year of Publication")+
        #scale_x_continuous(limits=c(min(values$corpus$YearPub),max(values$corpus$YearPub)))+
        theme_bw()
    }
    
    values$clustInfoDetailsPlot<-detPlot
  })
  
  #now render it
  output$clusterDetailsGrowth<-renderPlot({
    values$clustInfoDetailsPlot
  })
  
  
  #------------------------------------------------------------------------------------
  # INFO SUMMARY BOX UI - STATEMENTS AT THE TOP OF EACH PAGE
  #------------------------------------------------------------------------------------
  
  
  #For search statement
  output$searchInfoStatement<-renderUI({
    HTML("<b><big>Search PubMed</big></b> <a href='#searchInfo'data-toggle='collapse'><small><em>(show search details)</small></em></a>
           <div id='searchInfo' class= 'collapse'>
         Begin your search here by entering a valid <a href='https://www.ncbi.nlm.nih.gov/pubmed/' target='_blank'><strong>PubMed</strong></a> search string. To form more specific search strings, please consult PubMed directly and copy and paste the PubMed generated search string here. You can also <strong> load prior analyses </strong>, which are automatically stored in the 'storedAnalysis' folder every time you use Adjutant</div>")
  })
  
  
  #For search results tab
  output$searchResInfoStatement<-renderUI({
    HTML("<b><big>PubMed Search Results</big></b> <a href='#searchResultInfo'data-toggle='collapse'><small><em>(show search result details)</small></em></a>
           <div id='searchResultInfo' class= 'collapse'>
         Results from your Pubmed search will be displayed here. You can browse and export a table of your search results (referred to as the Document Corpus), or you are view a graphic summary of your results. You can also initite an analysis to in the 'Topic Discovery' tab to automatically find groups (clusters) of articles that about the same topic.
         </div>")
  })
  
  
  #For topic clustering tab
  output$topicClustInfoStatement<-renderUI({
    HTML("<b><big>Topic Clustering</big></b> <a href='#topicClustInfo'data-toggle='collapse'><small><em>(show topic clustering details)</small></em></a>
               <div id='topicClustInfo' class= 'collapse'>
                <br>
                <p><em><b>Unsupervised cluster analysis of the document corpus</em></b><br> Adjutant uses article titles and abstracts to identify document clusters pertaining to some topic. Given some set of documents, Adjutant creates a tidytext corpus using single terms <a href='https://www.tidytextmining.com/' target='_blank'>(see the excellent tidytext mining online book)</a>. Following some data wrangling and cleaning, which is detailed in our manuscript, Adjuntant calculates the <a href='https://www.tidytextmining.com/tfidf.html' target ='_blank'> td-idf metric </a> and generates a document term matrix (DTM) to prepare for cluster analysis. Our approach to unsupervised clustering was to use t-SNE to dimensionally reduce the data, via the <a href='https://cran.r-project.org/web/packages/Rtsne/index.html' target ='_blank'>RTsne package</a>, followed by hdbscan, from the <a href='https://cran.r-project.org/web/packages/dbscan/README.html' target ='_blank'>dbscan package</a>, to cluster documents. Adjutant will automatically optimize the hbscan parameters, again detailed in our paper, based upon the t-SNE results. You can modify some of the t-SNE parameters to see how your results can change. Documents that are not part of any cluster are considered to be 'noise' or 'not clustered'. Finally clusters are assigned topics based upon the two most frequently occuring terms in each cluster. </p>
<p><em><b>Exploring the topic clusters</em></b>
  <ul>
<li> Topics: The topics of all the clusters that were found by the text mining analysis. You can click on the topic buttons to reveal more information about the topic clusters</li>
  <li>t-SNE scatter plot: the scatter plot below shows the 2D co-ordinates based upon dimensionality reduction of the DTM following t-SNE. The red shapes deliniate the cluster boundries, and you can double-click on a cluster to get more details in the <em>'Cluster Details'</em>. To change some t-SNE parameters click on the red cog. The two parameters you can change are <b> perplexity</b> (roughly, the total number of neighbouring points to consult when running t-SNE) and <b>theta</b> (accuracy speed trade off; closer to 0 = slower, closer to 1 = faster). To learn more about t-SNE check out this <a href='https://distill.pub/2016/misread-tsne/' target='_blank'>article on Distill Pub</a></li>
  <li>Cluster Details: an overview of the cluster including the total number of members, top-ten frequently occuring terms,the five most cited papers in that cluster (according to PubMed central citation counts), and a histogram to show cluster growth over time.</li>
  </ul>
</p>
           </div>")
  })
  
  
  #For search results tab
  output$sampleInfoStatement<-renderUI({
    HTML("<b><big>Sample Documents</big></b> <a href='#sampleDocsInfo'data-toggle='collapse'><small><em>(show document sampling details)</small></em></a>
         <div id='sampleDocsInfo' class= 'collapse'>
         Document sampling allows you to define a subset of the document corpus for additional analysis that Adjutant does not support. For example, maybe you only want articles from 2015 and beyond, but your document corpus contains articles ranging from 1998 - 2017, in this scenario you can use this document sampling area to get those 2015 and beyond articles. You might wonder, 'why not just modify my PubMed search so that I only have articles from 2015 and beyond?'. The answer is that text mining works best when there's more data - so if you have many years of data you'll get better defined topic clusters, and afterwards you can define the subset of articles that you are most interested in. You can export the sampled documents to your computer in a CSV format. <strong>Be sure to also look at the 'storedAnalysis' folder for automatically saved datasets.</strong><br>
         </div>")
  })
  
  
  
  
  #cluster info box
  output$clustTopicBoxInfo<-renderUI({
    info<-NULL
    
    if(!is.null(values$corpus$tsneClusterNames)){
     info<- HTML("<em>Topic clusters (and their total number of members) are listed below. You can click on a cluster topic button to see where the cluster is on the t-SNE plot and to get more details about that cluster. You can also double click on the t-SNE plot to bring up specific information pertaining to a cluster.<em>")
    } 
    
    info
  })

})


