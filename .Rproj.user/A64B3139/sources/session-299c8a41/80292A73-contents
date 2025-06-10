#' Matrix masking function
#'
#' Converts 0 values of a binary matrix into NAs for analysis and plotting
#' @param matrix binary matrix of the mask to be used
#' @param as_vector return a vector instead of a matrix
#' @author Robert S. Chavez
#' @export
#' @examples
#' mat <- matrix(sample(0:1, 100, replace = TRUE),nrow = 10, ncol = 10 )
#' masker(mat)

masker <- function(matrix, as_vector = FALSE){
  
  if(as_vector == FALSE){
    maskmat <- ifelse(matrix == 0, NA, 1)
    return(maskmat)
    
  } else {
    mat2vec <- as.vector(matrix)
    maskvec <- ifelse(mat2vec == 0, NA, 1)
    return(maskvec)
  }
  
}