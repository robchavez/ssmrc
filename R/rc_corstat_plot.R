#' Plot correlation coefficients within significant corrected p-value reverse correlation maps
#'
#' Returns a ggplot2 plot object with correlation coefficient within corrected p-value areas spatial statistical map
#' @param stat_df a data frame object with correlation coefficients, p-value, and coordinate information (e.g., output from run_pixel_correlation())
#' @param background_image a matrix in the same dimensions as the stat_df
#' @param corrp_thresh corrected p-value threshold
#' @author Robert S. Chavez
#' @export
#' @examples
#' rc_corstat_plot(my_pixel_cortest, background_image = my_mean_rc_image)

 
rc_corstat_plot <- function(stat_df, background_image = NULL, corrp_thresh = .05){
  
  # apply threshold
  rvaldf <- stat_df
  rvaldf$corrp_thresh <- ifelse(rvaldf$corr_p_values < corrp_thresh, rvaldf$corr_p_values, NA)
  rvaldf$corrpmasked_rval <- ifelse(is.na(rvaldf$corrp_thresh) == TRUE, NA, rvaldf$r_statistics)
  
  if(all(is.na(rvaldf$corrpmasked_rval))){
    stop("There are no significant pixels.")
  }
  
  if(is.null(background_image) == FALSE){
    rvaldf$base <- as.vector(background_image)
    
    # plot
    outplot <-ggplot(rvaldf, aes(Ycoord, -Xcoord, fill=base)) + 
      geom_raster() + 
      scale_fill_distiller(palette = 'Greys', 
                           na.value = NA, 
                           guide = guide_colourbar(alpha = 0, title = NULL, 
                                                   theme = theme(legend.text = element_text(color='white')))) +
      new_scale_fill() +
      geom_raster(aes(fill = corrpmasked_rval)) +
      scale_fill_gradient2(na.value = NA, 
                           high = c("#FFFFB2", "#FECC5C", "#FD8D3C", "#F03B20", "#BD0026"),
                           low = c( "#08519C", "#3182BD","#6BAED6", "#9ECAE1", "#C6DBEF" ),
                           name = 'corrected \np-value')  + 
      theme_void() + 
      theme( plot.margin = unit(c(0,.5,0,0), "cm"))
    
    return(outplot)
    
  } else {
    
    outplot <- ggplot(rvaldf, aes(Ycoord, -Xcoord, fill=corrpmasked_rval)) + 
      geom_raster() + 
      scale_fill_gradient2(na.value = NA, 
                           high = c("#FFFFB2", "#FECC5C", "#FD8D3C", "#F03B20", "#BD0026"),
                           low = c( "#08519C", "#3182BD","#6BAED6", "#9ECAE1", "#C6DBEF" ),
                           name = 'corrected \np-value')  + 
      theme_void() + 
      theme( plot.margin = unit(c(0,.5,0,0), "cm"))
    return(outplot)
  } 
  
}