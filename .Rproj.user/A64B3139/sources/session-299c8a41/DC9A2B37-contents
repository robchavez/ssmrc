#' Read reverse correlation .Rda file
#'
#' Reads .Rda file and converts it into a list for analysis.
#' @param path path string to file 
#' @author Robert S. Chavez
#' @export
#' @examples
#' 
#' read_rc_rda('~/my_rc_data.Rda')

read_rc_rda <- function(path){
  require(rio)
  
  data <- import(path)
  return(data)
  
  }