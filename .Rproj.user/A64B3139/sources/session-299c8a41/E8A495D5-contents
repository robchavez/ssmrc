#' Plot 2D image from array
#'
#' Returns a ggplot2 plot object showing an image of a 2D input. Can be useful for quality control.
#' @param stat_array a 2D matrix (e.g., from stat_array())
#' @param palette character for color palette from RColorBrewer
#' @param legend logical to include legend
#' @author Robert S. Chavez
#' @export
#' @examples
#' 
#' rc_array_plot(my_mean_rc_image)

rc_array_plot <- function(stat_array, palette = 'Greys', legend = FALSE){
  
  require(ggplot2)
  require(ggnewscale)
  
  if(length(dim(stat_array)) >2){
    
    stop("stat_array is not a 2D matrix")
  }
  
  # Get the dimensions of the arrays
  dims <- dim(stat_array)
  
  x_dim <- dims[1]
  y_dim <- dims[2]
  xcoord <- rep(1:x_dim, y_dim)
  ycoord <- rep(1:y_dim, each = x_dim)
  
  
  plot_df <- data.frame(Xcoord = xcoord, 
                        Ycoord = ycoord,
                        image = as.vector(stat_array))
  
  if(legend == FALSE){
    # plot
    outplot <-ggplot(plot_df, aes(Ycoord, -Xcoord, fill=image)) + 
      geom_raster() + 
      scale_fill_distiller(palette = palette, 
                           na.value = NA, 
                           guide = guide_colourbar(alpha = 0, title = NULL, 
                                                   theme = theme(legend.text = element_text(color='white')))) +
      
      theme_void() + 
      theme( plot.margin = unit(c(0,.5,0,0), "cm"))
    
    return(outplot)
    
  } else {
    
    outplot <-ggplot(plot_df, aes(Ycoord, -Xcoord, fill=image)) + 
      geom_raster() + 
      scale_fill_distiller(palette = palette, na.value = NA, name=NULL) +
      theme_void() + 
      theme( plot.margin = unit(c(0,.5,0,0), "cm"))
    
    return(outplot)
    
  }
  
}