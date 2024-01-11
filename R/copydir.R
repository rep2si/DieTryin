#' A helper function
#'
#' This is a helper function to clone directory structures.
#'
#' @param 
#' from An example directory, to be cloned.
#' @param 
#' to Where to saved the cloned layout. 
#' @return 
#' This function dose not return any objects.
#' @export

copydir = function(from, to){
     oridir = list.dirs(from, full.names = FALSE)
     for(i in 2:length(oridir)){
      dir.create(paste0(to,'/',oridir[i]))
       }
      }
