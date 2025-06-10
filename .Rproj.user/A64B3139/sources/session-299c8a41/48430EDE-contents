#' Calculate summary statistics from reverse correlation images
#'
#' Calculates 2D summary statistics from all points of a reverse correlation list object 
#' @param rc_list a reverse correlation list imported from .Rda format
#' @param stat_func a character of the name of the function to be used. Default is 'mean'.
#' @param image the image file to be used. Has to be one in c('combined', 'ci', 'base', 'scaled'). Default is 'combined'.
#' @param smoothinkK the full-width at half max Gaussian smoothing kernel to be applied to data. Default is no smoothing.
#' @author Robert S. Chavez
#' @export
#' @examples
#' stat_array(rda_import) # will output the average image from 3D array

stat_array <- function(rc_list, stat_func = 'mean', image = 'combined', smoothingK = 0){
  
  require(abind)
  require(soundgen)
  
  x <- rc_list[[1]][[1]]
  xdim <- dim(x)[1]
  ydim <- dim(x)[1]
  
  blank_array <- matrix(nrow = xdim, ncol = ydim)
  
  rc_list_name <- deparse(substitute(rc_list))
  
  for(i in ls(rc_list)){
    
    cmd <- paste0(rc_list_name,'$`', i,"`$",image)
    tmp <- eval(parse(text = cmd))
    tmp_smooth <- gaussianSmooth2D(tmp,kernelSize = smoothingK)
    blank_array <- abind(blank_array, tmp_smooth, along = 3)
    
  }
  
  out_array <- blank_array[,,-1]
  
  cmdout <- paste0("stat_array <- apply(out_array, 1:2, ", stat_func,")")
  eval(parse(text = cmdout ))
  return(stat_array)
  
}