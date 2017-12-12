library(shiny)
library(shinyWidgets)
library(shinydashboard)

#Main visual interface. Very simple
body<-dashboardBody(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
  ),
  h2("Adjutant: support for systematic reviews"),
  p(HTML("<strong>Search and analyze pubmed results from R</strong><br><small><em>Usage Notes: Currently, there is a limit of 20,000 articles (for hosting resource reasons). Use pubmed syntax to search; for more advance queries, use the Pubmed interface to generate a search string that you copy into here.</em></small>")),
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
  br(),
  tabsetPanel(id="overviewPanel",
    tabPanel("Search Results", 
             uiOutput("summaryText"),
             uiOutput("analysisButton"),
             br(),
             dataTableOutput("documentTable")
    ),
    tabPanel("Corpus Structure", 
            uiOutput("summaryTextAnalysis"),
            plotOutput("tsnePlot")
    )
  )
)

#Putting it all together
dashboardPage(
  dashboardHeader(disable=TRUE),
  dashboardSidebar(disable=TRUE),
  body,
  skin="black"
)