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
  tabItems(
    #-------------------
    # About 
    #-------------------
    tabItem("about",
      h2("Adjutant: support for systematic reviews"),
      p(HTML("<strong>Search and analyze pubmed results from R</strong><br>"))
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
              placeholder = "Enter a pubmed search string here...", 
              btnSearch = icon("search"), 
              btnReset = icon("remove"), 
              width = "100%"
          ),
          hr(),
          #br(),
          HTML("<em>Search Options</em>"),
          switchInput(inputId = "saveAnalysis",label="Save Analysis?", value = TRUE),
          uiOutput("analysisFileName")
        ),
        tabPanel("Load Data",
          #p(HTML("Load an RDS file from a previous run.")),
          fileInput("prevAnalysis", "Load RDS file from previous run (see /priorRuns folder or load example)",width = "100%")
        )
      )
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
          em("This overview summary is intended to give you a sense of where these articles a from, what they cover (based soley on frequency of mesh terms), and some of the most cited articles. These visualizations are best when there are multiple years of data from multiple journals."),
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
          em("Medical Subject Heading (MeSH) terms are a controlled vocabulary used by the National Library of Medicine and assigned to articles within PubMed. MeSH terms are intended to give the reader a sense of what a PubMed article is about, but they can sometimes be much to general. For more specific topic suggestions in the article consider initating a topic clustering, from the 'Topic Clustering' menu item to get some more specific and data-driven sense of topics within your documents."),
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
      fluidRow(
        column(7,
          plotOutput("tsnePlot", dblclick = "plot_dbclick"),
          uiOutput("plotOptions")
        ),
        column(5, 
               uiOutput("clustTopicBoxInfo"),
               br(),
               uiOutput("showAllClustNames"),
               uiOutput("selectCluster"))
        ), #end of fluid row
      shinydashboard::box(title="Cluster Details",
                          id="exploreClust",
                          width=NULL,
                          uiOutput("clusterDetailsNote"),
                          uiOutput("clusterSelect"),
                          fluidRow(
                            column(width=8,
                                   uiOutput("clusterDetails")),
                            column(width=4,
                                   h4(""), #literally just to align it somehwhat with the other box
                                   plotOutput("clusterDetailsGrowth"))
                          )
      )
              
    ),
    #-------------------
    # Document Sampling
    #-------------------
    tabItem("sampleDocs",
      h1("Document Sampling"),
      p("Comming soon!")
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
    menuItem("Document Sampling", tabName = "docSample",icon=icon("clone")),
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