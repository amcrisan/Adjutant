
#' Process Scholar Search
#' Method that will also explore Semantic Scholar Articles
#' @param query 
#' @import rvest
#' @import magrittr
#' @import jsonlite
#' @import httr
#' @return
#' @export
processScholarSearch<-function(query =NULL,query_max=20000){
  
  article_total <- 0

  
  #useful " https://open.semanticscholar.org/
  #query<-"(outbreak OR epidemic OR pandemic) AND genom*"
  query<-"data science"
  
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

  page_count <-1
  stop_count<-0
  res<-c()
  
  while(article_total < query_max || stop_count > 100){
    
    #body of post request
    post_request<-list(queryString="data science",
         page=page_count,
         pageSize=1000,
         sort="relevance",
         authors=list(),
         coAuthors=list(),
         venues=list(),
         yearFilter=list(min=2014,max=2015),
         requireViewablePdf=TRUE,
         publicationTypes=list(),
         externalContentTypes=list())
  
  

    #getting paper data
    paper_data<-POST("https://www.semanticscholar.org/api/1/search",
              content_type('application/json'),
              #body='{"queryString":"data science","page":1,"pageSize":100,"sort":"relevance","authors":[],"coAuthors":[],"venues":[],"yearFilter":{"min":2014,"max":2019},"requireViewablePdf":true,"publicationTypes":[],"externalContentTypes":[]}',
              body = jsonlite::toJSON(post_request,auto_unbox = TRUE),
              verbose())
    
    if(paper_data$status_code != 200){
      stop("Seomthing went wrong getting data from semnatic scholar")
    }
    
   
    #just do this once
    if(page_count == 1){
      total_papers<-content(paper_data)$totalResults
      if(total_papers < query_max){
        query_max<-total_papers
      }
    }
    
    paper_data_results<-content(paper_data)$results
    
    #api paper request - no need, don't get extra info
    #paper<-paste0("http://api.semanticscholar.org/v1/paper/",paper_data_results[[1]]$id)
    #GET(paper) #YASSSS
    
    
    for(i in 1:length(paper_data_results)){
      paperType <- ifelse(paper_data_results[[i]]$venue$text=="","Book","Journal Article")
      #making it fit into the rest of the adjutant load
      risResults<-data.frame(PMID=paper_data_results[[i]]$id,
                             YearPub=paper_data_results[[i]]$year$text,
                             Journal=paper_data_results[[i]]$venue$text,
                             Authors=paste(sapply(paper_data_results[[i]]$authors,function(x){x[[1]]$name}),collapse=", "),
                             Title=paper_data_results[[i]]$title$text,
                             Abstract=paper_data_results[[i]]$paperAbstract$text,
                             articleType = paperType,
                             language = "eng",
                             pmcCitationCount = paper_data_results[[i]]$citationStats$numCitations,
                             pmcID = paper_data_results[[i]]$id,
                             doi = paper_data_results[[1]]$primaryPaperLink$url,
                             stringsAsFactors = FALSE)
      
      article_total<-article_total + length(paper_data_results)
      page_count<-page_count + 1
      stop_count<-stop_count+1
      
      res<-rbind(res,risResults)
    }
  }
}
