#' Runs t-test between two reverse correlation 3D arrays
#'
#' Calculates 2D summary statistics from all points of a reverse correlation list object 
#' @param array1 a reverse correlation 3D array
#' @param array2 a reverse correlation 3D array
#' @param mask 2D matrix used for selecting pixels to analyze (i.e., "masking")
#' @param corr_p_method multiple comparison correction method from p.adjust() to be used
#' @param paired logical if the t-test is a paired samples t-test
#' @param var_equal logical if equal variance is assumed 
#' @author Robert S. Chavez
#' @export
#' @examples
#' run_pixel_ttests(array1, array2, mask = maskface, paired = T)


run_pixel_ttests <- function(array1, array2, mask = NULL, corr_p_method = 'fdr', paired = FALSE, var_equal = FALSE) {
  # Get the dimensions of the arrays
  dims <- dim(array1)
  if (!identical(dims, dim(array2)) & paired == TRUE) {
    stop("Input arrays must have the same dimensions.")
  }
  
  x_dim <- dims[1]
  y_dim <- dims[2]
  # z_dim is the sample dimension
  
  # Initialize matrices to store p-values, t-statistics, and coordinates
  p_values <- matrix(NA, nrow = x_dim, ncol = y_dim)
  t_statistics <- matrix(NA, nrow = x_dim, ncol = y_dim)
  xcoord <- matrix(NA, nrow = x_dim, ncol = y_dim)
  ycoord <- matrix(NA, nrow = x_dim, ncol = y_dim) 
  
  # Loop through each X-Y coordinate
  for (i in 1:x_dim) {
    for (j in 1:y_dim) {
      # Extract the vectors of samples for the current X-Y coordinate
      samples1 <- array1[i, j, ]
      samples2 <- array2[i, j, ]
      
      # Perform the t-test
      # The tryCatch will handle cases where a t-test cannot be performed (e.g., insufficient variance)
      test_result <- tryCatch({
        
        if(paired==FALSE){
          
          val <- c(samples1, samples2)
          group <- c(rep('group1', length(samples1)), rep('group2', length(samples2)))
          ind_t_df <- data.frame(val, group) 
          
          if(var_equal == FALSE){
            t.test(val ~ group, var.equal = FALSE, data = ind_t_df)
          } else{
            t.test(val ~ group,  var.equal = TRUE, data = ind_t_df)
          }
          
        } else {
          
          if(var_equal == FALSE){
            t.test(samples1, samples2, paired=TRUE, var.equal = FALSE)
          } else{
            t.test(samples1, samples2, paired=TRUE, var.equal = TRUE)
          }
        }
        
        
      }, error = function(e) {
        # Return NA if t-test fails
        list(p.value = NA, statistic = NA)
      })
      
      # Store the p-value and t-statistic
      p_values[i, j] <- test_result$p.value
      t_statistics[i, j] <- test_result$statistic
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
                    t_statistics = as.vector(t_statistics)))
  
}
