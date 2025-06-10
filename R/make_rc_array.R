#' Make reverse correlation array
#'
#' Converts list to 3D array object
#' @param rc_list a reverse correlation list imported from .Rda format
#' @param image the image file to be used. Has to be one in c('combined', 'ci', 'base', 'scaled'). Default is 'combined'.
#' @param smoothinkK the full-width at half max Gaussian smoothing kernel to be applied to data. Default is no smoothing.
#' @author Robert S. Chavez
#' @export
#' @examples
#' make_rc_array(rda_import_list, smoothingK = 10)

make_rc_array <- function(rc_list, image = 'combined', smoothingK = 0){
  
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
  return(out_array)
  
}