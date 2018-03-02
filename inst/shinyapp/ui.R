library(shiny)
library(shinyWidgets)
library(shinydashboard)

#-------------------------------------------------------------------------
# MAIN UI ELEMENTS IN APP BODY
#-------------------------------------------------------------------------

body<-dashboardBody(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css"),
    tags$style(".ban2 {color:#a14c4c}")
  ),
  useSweetAlert(),
  tabItems(
    #-------------------
    # About 
    #-------------------
    tabItem("about",
      h2("Adjutant: support for systematic reviews"),
      p(HTML("<strong>Search and analyze PubMed results from R</strong><br>")),
      hr(),
      p("Adjutant is an open-source, interactive, and R-based application to support literature mining of PubMed for the purposes of conducting systematic reviews. Given a PubMed compatible search query, Adjutant will assemble a document corpus and, using unsupervised techniques, identify clusters of documents pertaining to a specific topic. To support rapid analysis of the document corpus, we have made explicit trade-offs between speed and accuracy, which are modifiable by the user, and aim to provide a “good-enough” result to support, rather than supplemental, a researcher’s decision making. Thus, an analysis of several thousand documents takes only a few minutes to complete, from initial query to cluster assignment, and the user can explore the document corpus via a Shiny application. Adjutant saves analytic datasets as they are derived, and these datasets are structured and compatible with each other such that users can conduct further downstream analyses that Adjutant does not explicitly support. ")
    ),
    #-------------------
    # Search Input 
    #-------------------
    tabItem("searchIn",
      uiOutput("searchInfoStatement"),
      hr(class="style-four"), 
      tabsetPanel(id = "loadData", type="pills",
        tabPanel("Enter Query",
          br(),
          actionLink("loadExample",HTML("<small>Load an example query</small>")),
          searchInput(
              inputId = "searchQuery", 
              label = NULL, 
              placeholder = "Enter a PubMed search string here...", 
              btnSearch = icon("search"), 
              btnReset = icon("remove"), 
              width = "100%"
          )
        ),
        tabPanel("Load Data",
          fileInput("prevAnalysis", "Load a RDS file from previous run (see 'storedRuns' folder or load example)",width = "100%")
        )
      ),
      hr(),
      HTML("<em>Search Options</em>"),
      switchInput(inputId = "saveAnalysis",label="Save Analysis?", value = TRUE),
      uiOutput("analysisFileName")
    ),
    #------------------------------
    # Search Summary & Results
    #------------------------------
    tabItem("searchOverview",
      uiOutput("searchResInfoStatement"),
      hr(class="style-four"), 
      tabsetPanel(
        id="searchResSum",
        tabPanel("Document Corpus Table",
          br(),
          uiOutput("summaryText"),
          br(),
          DT::dataTableOutput("documentTable")
        ),
        tabPanel("Overview Summary",
          br(),
          em("This overview summary is intended to give you a sense of where these articles are published, what they cover (based soley on frequency of MeSH terms), and some of the most cited articles. These visualizations are best when there are multiple years of data from multiple journals."),
          hr(),
          h4("Publications over time"),
          fluidRow(
            column(width=8,
                  plotOutput("journalPubTime",height="250px")
            ),
            column(width=4,
                   plotOutput("yearPubPlot",height="250px")
            )
          ),
          hr(),
          h4("MeSH Terms over time"),
          em("Medical Subject Heading (MeSH) terms are a controlled vocabulary used by the National Library of Medicine and assigned to articles within PubMed. MeSH terms are intended to give the reader a sense of what a PubMed article is about, but they can sometimes be much to general. For more specific topic suggestions consider initating a topic clustering, from the 'Topic Discovery' menu item to get some more specific and data-driven sense of topics within your documents."),
          br(),
          br(),
          fluidRow(
            column(width=8,
                   plotOutput("meshTimePlot",width="100%",height="350px")
            ),
            column(width=4,
                  plotOutput("meshWordCloud",height="350px")
            )
          ),
          hr(),
          h4("Top 10 Most Referenced Papers"),
          em("Most reference articles according to PubMed Central internal counts, which don't match Google Scholar but are a reasonable heuristic"),
          br(),
          br(),
          fluidRow( #largely to keep consistent formatting
            column(width=8,
                   uiOutput("topCorpusArticles")
            ),
            column(width=4,
                   p("")
            )
          )
        )
      )
    ),
    #-------------------
    # Topic Clustering
    #-------------------
    tabItem("clusterAnalysis",
      uiOutput("topicClustInfoStatement"),
      hr(class="style-four"), 
      uiOutput("topicClustInitiateButton"),
      uiOutput("clusterOverviewStatement"),
      br(),
      shinydashboard::box(title="Topic Clusters",
                          id = "clusterButtons",
                          width="100%",
                          collapsible = TRUE,
                          collapsed = FALSE,
                          uiOutput("showAllClustNames"),
                          uiOutput("selectCluster")
      ),
      fluidRow(
        column(7,
               shinydashboard::box(title="Topic Cluster Plot",
                                   id = "clusterPlot",
                                   width="100%",
                                   uiOutput("plotOptions"),
                                   plotOutput("tsnePlot", dblclick = "plot_dbclick")
                                   #uiOutput("clustTopicBoxInfo")
                                 )
        ),
        column(5, 
               shinydashboard::box(title="Topic Cluster Details",
                                   id="exploreClust",
                                   width="100%",
                                   #uiOutput("clusterDetailsNote"),
                                   uiOutput("clusterSelect"),
                                   uiOutput("clusterDetails"),
                                   plotOutput("clusterDetailsGrowth",height="250px"))
               )   
        )
              
    ),
    #-------------------
    # Document Sampling
    #-------------------
    tabItem("docSample",
      uiOutput("sampleInfoStatement"),
      hr(class="style-four"), 
      em("Important! Document sampling is only meant to create a smaller subset of the data that you can export to your computer. Topic discovery will use the full document corpus irrespective of this sampling step. Click on 'show document sampling details' for more information.  "),
      br(),
      hr(),
      h4("Sampling Approach"),
      fluidRow(
        column(width=4,
          radioButtons("sampChoices",
                       label="Choose a sampling method", 
                       selected = "random",
                       width ="100%",
                       choiceNames=c("All - select all documents meeting the filter criteria",
                                     "Random - randomly select articles that match filter criteria",
                                     "Random Stratified - randomly select articles that match the filter criteria AND according to some strata (group)"),
                       inline=FALSE,
                       choiceValues = c("all","random","randomStratified"))
        ),
        column(width = 4,
          uiOutput("weightedSampleOptions"),
          uiOutput("stratifiedSampleOptions")
        ),
        column(width = 4,
          uiOutput("sampleSize"),
          uiOutput("filterButton"),
          br(),
          uiOutput("downloadSubsetData")
        )
      ),
      hr(),
      h4("Filter Criteria"),
      fillRow(height="2000px", #this is so all drop down menu items fit
              width="100%",
              column(width=12,
                       uiOutput("filtJournal"),
                       uiOutput("filtIsOpen"),
                       uiOutput("filtYear"),
                       uiOutput("filtArticleType"),
                       uiOutput("filtMinCitation"),
                       uiOutput("filtTopic"))
      )

    )
  )
)

#-------------------------------------------------------------------------
# SIDEBAR MENU
#-------------------------------------------------------------------------

sideDash<-dashboardSidebar(
  sidebarMenu(
    id = "sidebarTabs",
    menuItem("Search", tabName = "searchIn", icon = icon("search")),
    menuItemOutput("searchMenu"),
    menuItem("Topic Discovery", tabName = "clusterAnalysis",icon = icon("spinner")),
    menuItem("Sample Articles", tabName = "docSample",icon=icon("clone")),
    br(),
    menuItem("Clear Analysis", tabName = "clearAnalysis",icon=icon("ban", class="ban2")),
    br(),
    menuItem("About", tabName = "about", icon = icon("info-circle"))
  )
)

#-------------------------------------------------------------------------
# ALL TOGETHER
#-------------------------------------------------------------------------

dashboardPage(
  dashboardHeader(title="Adjutant"),
  sideDash,
  body,
  skin="black"
)