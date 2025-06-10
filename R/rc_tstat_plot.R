#' Plot t-values within significant corrected p-value reverse correlation maps
#'
#' Returns a ggplot2 plot object showing t-values of significant spatial statistical maps after correcting for multiple comparisons.
#' @param stat_df a data frame object with t-value, p-value, and coordinate information (e.g., output from run_pixel_ttests())
#' @param backgroud_image a matrix in the same dimensions as the stat_df
#' @param corrp_thresh corrected p-value threshold
#' @author Robert S. Chavez
#' @export
#' @examples
#' 
#' rc_tstat_plot(my_pixel_ttest, background_image = my_mean_rc_image)

rc_tstat_plot <- function(stat_df, backgroud_image = NULL, corrp_thresh = .05){
  
  require(ggplot2)
  require(ggnewscale)
  
  # apply threshold
  tvaldf <- stat_df
  tvaldf$corrp_thresh <- ifelse(tvaldf$corr_p_values < corrp_thresh, tvaldf$corr_p_values, NA)
  tvaldf$corrpmasked_tval <- ifelse(is.na(tvaldf$corrp_thresh) == TRUE, NA, tvaldf$t_statistics)
  
  if(all(is.na(tvaldf$corrpmasked_tval))){
    stop("There are no significant pixels.")
  }
  
  if(is.null(backgroud_image) == FALSE){
    tvaldf$base <- as.vector(backgroud_image)
    
    # plot
    outplot <-ggplot(tvaldf, aes(Ycoord, -Xcoord, fill=base)) + 
      geom_raster() + 
      scale_fill_distiller(palette = 'Greys', 
                           na.value = NA, 
                           guide = guide_colourbar(alpha = 0, title = NULL, 
                                                   theme = theme(legend.text = element_text(color='white')))) +
      new_scale_fill() +
      geom_raster(aes(fill = corrpmasked_tval)) +
      scale_fill_gradient2(na.value = NA, 
                           high = c("#FFFFB2", "#FECC5C", "#FD8D3C", "#F03B20", "#BD0026"),
                           low = c( "#08519C", "#3182BD","#6BAED6", "#9ECAE1", "#C6DBEF" ),
                           name = 'corrected \np-value')  + 
      theme_void() + 
      theme( plot.margin = unit(c(0,.5,0,0), "cm"))
    
    return(outplot)
    
  } else {
    
    outplot <- ggplot(tvaldf, aes(Ycoord, -Xcoord, fill=corrpmasked_tval)) + 
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