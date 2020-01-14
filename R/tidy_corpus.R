#' Tidying Up the PubMed Corpus
#'
#' @description Creates a tidytext corpus from the PubMed articles titles and abstracts. Adjutant generally follows the approach presented in  'Text Mining with R' (https://www.tidytextmining.com/tidytext.html). 
#' @param corpus a document corpus
#' @param stopTerms stop terms provided by the user
#'
#' @return Tidy Text Corpus
#' @import tidytext
#' @import tm
#' @import dplyr
#' @import SnowballC
#' @import stringr
#' @importFrom utils data tail
#' 
#' @export
#'
tidyCorpus<- function(corpus=NULL, stopTerms = NULL){
  #data(stop_words,envir = environment())
  if (requireNamespace("tidytext", quietly = TRUE)) {
    stop_words<-tidytext::stop_words
  }
  
  #remove some common terms that will occur in abstract
  customStopTerms<-data.frame(word=c("abstract", "text", "abstracttext","introduction","background","method","methods","methodology","conclusion","conclusions","objectives","results","result","we","materials","purpose","significance","significant","mg","http","com"))
  
  surveyWords <- c("abstract", "abstracttext","introduction","background","method","methods","methodology","conclusion","conclusions","objectives","results","result","we","materials","purpose","significance","significant","mg","http","com")
  
  if(!is.null(stopTerms)){
    customStopTerms = data.frame(word = c(stopTerms,surveyWords))
  }else{
    customStopTerms<-data.frame(word= surveyWords)
    
  }

  
  # Make text tidy!
  # Note: latest version of tidyr::unnest is now very slow.
  # This is a known issue, and it is significantly slowing down
  # the performacne of cleaning up the corpus. There is a 
  # fix in the dev version of vtrs, but even updating to latest
  # version of tidyr doesn't work.
  tidyCorpus_df <- corpus[,c("PMID","Title","Abstract")] %>%
    mutate(text = paste0(Title,Abstract)) %>%
    unnest_tokens(word, text) %>% 
    #mutate(word = strsplit(as.character(word), "\\.")) %>% #some text is stuck together for example person.METHODS so, I am fixing that
    #tidyr::unnest(word) %>% 
    anti_join(stop_words) %>%
    anti_join(customStopTerms) %>%
    filter(str_length(word)>2) %>% #only keeps words with length of 2 or greater (AMR, a useful abbreviation, is three characters long)
    filter(!str_detect(word,"\\d")) %>% #get rid of any numbers
    mutate(wordStemmed = wordStem(word)) %>% #finally, get the word stems (Porter's algorithm)
    select(PMID,word,wordStemmed)
    
  #Calculate TF IDF
  # I will now also add the term frequency document frequency values.
  tidyCorpus_df<-tidyCorpus_df %>%
    dplyr::count(PMID, wordStemmed,sort = TRUE) %>%
    ungroup() %>%
    bind_tf_idf(wordStemmed, PMID, n)

  
  ##Cleaning up terms
  # removing terms that are really really common & those that are very infrequent
  #withProgress(message = 'Removing very rare & very common terms', value = 0.1, {   
  totalArticles<-nrow(corpus)
  
  #TO DO: Hard criteria, could be done more empirically
  #Instances of words *per article*
  wordToRemove<-tidyCorpus_df %>%
    dplyr::group_by(wordStemmed) %>%
    dplyr::count(name="total")%>%
    dplyr::filter(total < totalArticles*0.01 | total > totalArticles*0.7) 
  
  tidyCorpus_df<-anti_join(tidyCorpus_df,wordToRemove,by="wordStemmed")
  
  return(tidyCorpus_df)
}