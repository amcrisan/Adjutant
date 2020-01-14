#' genID
#' generic id generating function
#'
#' @param n 
#' @param idLen 
#'
#' @return
genID<-function(n=NULL,idLen = 10){
  
    ids<-sapply(1:n,function(x,idLen){
      paste(sample(c(letters,0:9),idLen),collapse = "")
      },idLen = idLen)
    
    return(ids)
}


#' process_inputFile
#' a generic method for reading in differnt text flat files. Helper method for process_singleFile
#'
#' @param fIN 
#'
#' @return dat
process_inputFile<-function(fIN  = NULL){
  
  #assuming that the person is passing in a csv, tsv, or xlsx files
  #get file extension
  ext<-gsub("\\.","",stringr::str_extract(fIN,"\\.[a-zA-Z]+$"))
  
  dat<-switch(ext,
         "csv" = read.csv(file=fIN,header=TRUE),
         "tsv" = read.csv(file=fIN,header=TRUE,sep="\t"),
        " xls" = readxl::read_excel(fIN),
         "xlsx" = readxl::read_excel(fIN),
         NULL)
  
  return(dat)
}


#' process_singleFile
#' A function that allows a user to run adjutant on a single file that has nothing to do with a lit review.
#'
#' @param dat 
#' @param colCollapse 
#'
#' @return dat
#' @export
processSingleFile<-function(dat = NULL,colCollapse = NULL){
  
  objClass<-class(dat)
  
  #check it is the right type of object
  if(!(objClass %in% c("data.frame","character","string"))){
    stop("Cannot work with this object type")
  }
  
  
  #check that the file actually exists
  if(objClass %in% c("character","string")){
   if(!file.exists(dat)) {
     stop("That file does not exist.")
   }
    dat<-process_inputFile(dat)
  }
  
  #user has specificed the columns that shuold be collapsed
  #if they don't, all the rows in the data frame collapse into one
  if(!is.null(colCollapse)){
    if(!(class(colCollapse) == "numeric")){
      stop("colCollapse values must be numeric")
    }
    
    tmp<-apply(dat[,colCollapse],1, function(x){paste(x,  collapse =" ")})
    
  }else{
    warning("All the columns in the data frame will be collpased together. 
            If you didn't intend for this, then either provide a subset of 
            the data or use the colCollapse to identify which columns to collapse")
     
    tmp<-apply(dat,1, function(x){paste(x,  collapse =" ")})
  }
  
  #now make a data.frame with all of the data adjutant's lit review
  #archiecture expects. 
  ids<-genID(n=length(tmp))
  
  dat<-data.frame(PMID=ids,
                  YearPub=rep(NA,length(tmp)),
                  Journal=rep(NA,length(tmp)),
                  Authors=rep(NA,length(tmp)),
                  Title=rep("",length(tmp)),
                  Abstract=tmp,
                  articleType = rep("singleFile",length(tmp)),
                  language = "eng",
                  pmcCitationCount = rep(1,length(tmp)),
                  pmcID = ids,
                  doi = rep(NA,length(tmp)),
                  stringsAsFactors = FALSE)
  
  return(dat)
}