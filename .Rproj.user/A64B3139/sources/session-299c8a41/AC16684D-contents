#' Plot corrected p-value reverse correlation maps
#'
#' Returns a ggplot2 plot object corrected p-values spatial statistical map
#' @param stat_df a data frame object with t-value, p-value, and coordinate information (e.g., output from run_pixel_ttests())
#' @param backgroud_image a matrix in the same dimensions as the stat_df
#' @param corrp_thresh corrected p-value threshold
#' @author Robert S. Chavez
#' @export
#' @examples
#' rc_corrp_plot(my_pixel_ttest, background_image = my_mean_rc_image)

rc_corrp_plot <- function(stat_df, backgroud_image = NULL, corrp_thresh = .05){
  
  require(ggplot2)
  require(ggnewscale)
  
  # apply threshold
  pvaldf <- stat_df
  pvaldf$corrp_thresh <- ifelse(pvaldf$corr_p_values < corrp_thresh, pvaldf$corr_p_values, NA)
  
  if(all(is.na(pvaldf$corrp_thresh))){
    stop("There are no significant pixels.")
  }
  
  if(is.null(backgroud_image) == FALSE){
    pvaldf$base <- as.vector(backgroud_image)
    
    # plot
    outplot <-ggplot(pvaldf, aes(Ycoord, -Xcoord, fill=base)) + 
      geom_raster() + 
      scale_fill_distiller(palette = 'Greys', 
                           na.value = NA, 
                           guide = guide_colourbar(alpha = 0, title = NULL, 
                                                   theme = theme(legend.text = element_text(color='white')))) +
      new_scale_fill() +
      geom_raster(aes(fill = corrp_thresh)) +
      scale_fill_distiller(palette = 'YlOrRd', 
                           na.value = NA, 
                           name = 'corrected \np-value')  + 
      theme_void() + 
      theme( plot.margin = unit(c(0,.5,0,0), "cm"))
    
    return(outplot)
    
  } else {
    
    outplot <- ggplot(pvaldf, aes(Ycoord, -Xcoord, fill=corrp_thresh)) + 
      geom_raster() + 
      scale_fill_distiller(palette = 'YlOrRd', 
                           na.value = NA, 
                           name = 'corrected \np-value')  + 
      theme_void() + 
      theme( plot.margin = unit(c(0,.5,0,0), "cm"))
    return(outplot)
  } 
  
}