#' Runs correlation between a variable and a reverse correlation 3D array
#'
#' Calculates 2D summary statistics from all points of a reverse correlation list object 
#' @param array a reverse correlation 3D array
#' @param cor_var a vector of values to correlate with pixels
#' @param mask 2D matrix used for selecting pixels to analyze (i.e., "masking")
#' @param cor_method correlation method to use, either 'pearson', 'spearman', or 'kendall'
#' @param corr_p_method multiple comparison correction method from p.adjust() to be used
#' @author Robert S. Chavez
#' @export
#' @examples
#' run_pixel_correlation(facearray, bias_variable, mask = NULL, cor_method = 'pearson', corr_p_method = 'fdr' ) 


run_pixel_correlation <- function(array, cor_var, mask = NULL, cor_method = 'pearson', corr_p_method = 'fdr' ) {
  # Get the dimensions of the arrays
  dims <- dim(array)
  
  x_dim <- dims[1]
  y_dim <- dims[2]
  
  # Initialize matrices to store p-values, t-statistics, and coordinates
  p_values <- matrix(NA, nrow = x_dim, ncol = y_dim)
  r_statistics <- matrix(NA, nrow = x_dim, ncol = y_dim)
  xcoord <- matrix(NA, nrow = x_dim, ncol = y_dim)
  ycoord <- matrix(NA, nrow = x_dim, ncol = y_dim) 
  
  # Loop through each X-Y coordinate
  for (i in 1:x_dim) {
    for (j in 1:y_dim) {
      
      # Extract the vectors of samples for the current X-Y coordinate
      samples1 <- array[i, j, ]
      
      
      # Perform the t-test
      # The tryCatch will handle cases where a t-test cannot be performed (e.g., insufficient variance)
      test_result <- tryCatch({
        
        if(cor_method =='pearson'){
          
          cor.test(samples1, cor_var, method = 'pearson')
          
        } else {
          
          cor.test(samples1, cor_var, method = cor_method)
          
        }
        
        
      }, error = function(e) {
        # Return NA if t-test fails
        list(p.value = NA, estimate = NA)
      })
      
      # Store the p-value and t-statistic
      p_values[i, j] <- test_result$p.value
      r_statistics[i, j] <- test_result$estimate
      xcoord[i, j] <- i
      ycoord[i, j] <- j
    }
  }
  
  # Apply mask and do multiple comparisons correction
  if(is.null(mask)){
    if(corr_p_method == 'fdr'){
      p_values_vec <- as.vector(p_values)
      corr_p_values <- p.adjust(p_values_vec, method = 'fdr')
    } else {
      p_values_vec <- as.vector(p_values)
      corr_p_values <- p.adjust(p_values_vec, method = corr_p_method)
    }
  } else {
    
    if(corr_p_method == 'fdr'){
      maskvec <- masker(mask, as_vector = TRUE)
      p_values_vec_nomask <- as.vector(p_values)
      p_values_vec <- ifelse(is.na(maskvec) == TRUE, NA, p_values_vec_nomask)
      corr_p_values <- p.adjust(p_values_vec, method = 'fdr')
      
    } else {
      maskvec <- masker(mask, as_vector = TRUE)
      p_values_vec_nomask <- as.vector(p_values)
      p_values_vec <- ifelse(is.na(maskvec) == TRUE, NA, p_values_vec_nomask)
      corr_p_values <- p.adjust(p_values_vec, method = corr_p_method)
    }
    
  }
  
  # Return a data frame containing all of the stats and coordinates
  return(data.frame(Xcoord = as.vector(xcoord), 
                    Ycoord = as.vector(ycoord),
                    p_values = as.vector(p_values), 
                    corr_p_values = corr_p_values,
                    r_statistics = as.vector(r_statistics)))
  
}