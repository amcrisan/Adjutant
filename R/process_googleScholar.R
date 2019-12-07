
#' Process Scholar Search
#' Method that will also explore Semantic Scholar Articles
#' @param query 
#' @import RSelenium
#' @import magrittr
#' @return
#' @export
processScholarSearch<-function(query =NULL,query_max=20000){
  
  article_total = 0
  
  #useful " https://open.semanticscholar.org/
  query<-"(outbreak OR epidemic OR pandemic) AND genom*"

  site<-"https://www.semanticscholar.org/"
  session<- html_session(site)
  
  #searching for information
  search_form<-session %>% 
    html_nodes("form") %>% 
    extract2(1) %>%
    html_form()%>%
    set_values('q'  = query)
  
  #get the results
  result<-submit_form(session,search_form)
  
  
  #SINGLE PAGE RESULT

  ### get the results on each page
  ## Below is the POST query that semantic scholar accepts
  # '{"queryString":"data science","page":1,"pageSize":10,"sort":"relevance","authors":[],"coAuthors":[],"venues":[],"yearFilter":null,"requireViewablePdf":false,"publicationTypes":[],"externalContentTypes":[]}'

  post_request<-list(queryString="data science",
       page=1,
       pageSize=10,
       sort="relevance",
       authors=list(),
       coAuthors=list(),
       venues=list(),
       yearFilter=list(min=2014,max=2015),
       requireViewablePdf=TRUE,
       publicationTypes=list(),
       externalContentTypes=list())
  
  
  paper_data<-POST("https://www.semanticscholar.org/api/1/search",
            content_type('application/json'),
            #body='{"queryString":"data science","page":1,"pageSize":10,"sort":"relevance","authors":[],"coAuthors":[],"venues":[],"yearFilter":{"min":2014,"max":2019},"requireViewablePdf":true,"publicationTypes":[],"externalContentTypes":[]}',
            body = jsonlite::toJSON(post_request,auto_unbox = TRUE),
            verbose())
  
  if(paper_data$status_code != 200){
    stop("Seomthing went wrong getting data from semnatic scholar")
  }
  
 
  paper_data_results<-content(paper_data)$results
  
  #api paper request
  paper<-paste0("http://api.semanticscholar.org/v1/paper/",paper_data_results[[1]]$id)
  
  GET(paper) #YASSSS
  
  

  i= 1
  res<-c()
  
  for(i in 1:10){
    #making it fit into the rest of the adjutant load
    risResults<-data.frame(PMID=paper_data_results[[i]]$id,
                           YearPub=paper_data_results[[i]]$year$text,
                           Journal=paper_data_results[[i]]$venue$text,
                           Authors=paste(sapply(paper_data_results[[i]]$authors,function(x){x[[1]]$name}),collapse=", "),
                           Title=paper_data_results[[i]]$title$text,
                           Abstract=paper_data_results[[i]]$paperAbstract$text,
                           articleType = "type",
                           language = "eng",
                           pmcCitationCount = paper_data_results[[i]]$citationStats$numCitations,
                           pmcID = paper_data_results[[i]]$id,
                           doi = paper_data_results[[1]]$primaryPaperLink$url,
                           stringsAsFactors = FALSE)
    
    res<-rbind(res,risResults)
  }
  
  
  

}
