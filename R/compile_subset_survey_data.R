#' A function to compile individual CSVs in the SubsetContributions folder into a flat database.
#' Used to create a database after each individual record is entered.
#'
#' @param path Path to RICH folder.
#' @param format Is the data saved as csv or json?
#' @param what What to compile. "SubsetContributions" by default.
#' @return A CSV of compiled contribution records will be added to the Results folder.
#' @export
#' @examples
#' \dontrun{
#' compile_subset_survey_data(path)
#' }
#'

compile_subset_survey_data = function(path=path, format="json", what = "SubsetContributions"){
#####################
  if(format=="csv"){
    files = list.files(path=paste0(path,"/","SubsetContributions/"), pattern="*.csv")
    Basic = vector("list",length(files))
                              
 for(i in 1:length(files)){         
  Basic[[i]] = matrix(read.csv(paste0(path,"/","SubsetContributions","/",files[i]),stringsAsFactors = FALSE, header = FALSE))
   }

   Nc = c()
 for(i in 1:length(files)){
   Nc[i] = length(Basic[[i]][[1]])
   }

 Nk = c()
 for(i in 1:length(files)){
   Nk[i] = length(matrix(read.csv(paste0(path,"/","SubsetContributions","/",files[i]),stringsAsFactors = FALSE)))
   }

 
 Data = matrix(NA, ncol=length(Basic[[1]][[1]]), nrow=length(files))
 colnames(Data) = Basic[[1]][[1]]
 for(i in 1:length(files)){
   Data[i,] = Basic[[i]][[2]]
   }     

   }

#####################
  if(format=="json"){
    files = list.files(path=paste0(path,"/", what, "/"), pattern="*.json")
    Basic = vector("list",length(files))
                              
 for(i in 1:length(files)){ 
  bob = readLines(paste0(path,"/", what, "/",files[i]))       
  Basic[[i]] = matrix(jsonlite::fromJSON(bob))
   }

   }
 
 Data = matrix(NA, ncol=length(Basic[[1]]), nrow=length(files))
 colnames(Data) = names(jsonlite::fromJSON(bob))
 for(i in 1:length(files)){
   Data[i,] = unlist(Basic[[i]])
   }                         
 
 write.csv(Data, file=paste0(path,"/","Results/", what,"-SummaryTable.csv"),row.names = FALSE)
 }



