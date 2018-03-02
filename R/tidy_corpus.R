#Apply glorious tidy text methods. Just use one term for an overview
#commonly used words in the english language.
#note that I am indebted to Julia and David: http://tidytextmining.com/
tidyCorpus<- function(corpus=NULL){
  
  data(stop_words)
  #remove some common terms that will occur in abstract
  customStopTerms<-data.frame(word=c("abstract", "text", "abstracttext","introduction","background","method","methods","methodology","conclusion","conclusions","objectives","results","result","we","materials","purpose","significance","significant","mg","http","com"))
  
  # Make text tidy!
  #withProgress(message = 'Formatting data for text analysis', value = 0.1, {
  tidyCorpus_df <- corpus[,c("PMID","Title","Abstract")] %>%
    mutate(text = paste0(Title,Abstract)) %>%
    unnest_tokens(word, text) %>%
    mutate(word = strsplit(as.character(word), "\\.")) %>% #some text is stuck together for example person.METHODS so, I am fixing that
    tidyr::unnest(word) %>% 
    anti_join(stop_words) %>%
    anti_join(customStopTerms) %>%
    filter(str_length(word)>2) %>% #only keeps words with length of 2 or greater (AMR, a useful abbreviation, is three characters long)
    filter(!str_detect(word,"\\d")) %>% #get rid of any numbers
    mutate(wordStemmed = wordStem(word)) %>% #finally, get the word stems (Porter's algorithm)
    select(PMID,word,wordStemmed)
  #})
  
  #Calculate TF IDF
  #withProgress(message = 'Calculating TD_IDF metric', value = 0.1, { 
  # I will now also add the term frequency document frequency values.
  tidyCorpus_df<-tidyCorpus_df %>%
    dplyr::count(PMID, wordStemmed,sort = TRUE) %>%
    ungroup() %>%
    bind_tf_idf(wordStemmed, PMID, n)
  #})
  
  ##Cleaning up terms
  # removing terms that are really really common & those that are very infrequent
  #withProgress(message = 'Removing very rare & very common terms', value = 0.1, {   
  totalArticles<-nrow(corpus)
  
  #TO DO: Hard criteria, could be done more empirically
  wordToRemove<-tidyCorpus_df %>%
    group_by(wordStemmed) %>%
    dplyr::count() %>%
    filter(nn < totalArticles*0.01 | nn > totalArticles*0.7) 
  
  tidyCorpus_df<-anti_join(tidyCorpus_df,wordToRemove,by="wordStemmed")
  #})
  
  return(tidyCorpus_df)
}