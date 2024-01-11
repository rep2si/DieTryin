#' A helper function
#'
#' This is a helper function to grab file extensions.
#'
#' @param 
#' x A string.
#' @param 
#' n Number of characters to return, from right. 
#' @return 
#' A substring. For example, substrRight("patiently waiting.mp3", 3) returns "mp3".
#' @export

substrRight = function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
   }
