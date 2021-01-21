
initalise.libraries = function(){

  #'
  #'  Quietly calls all libraries that I commonly use.
  #'
  #' @export

  #libraries needed
  library( rgl,  quietly =T)
  library(rhdf5, quietly =T)
  library(magicaxis, quietly = T)
  library(cooltools, quietly =T)
  library(devtools, quietly = T)
  library(data.table, quietly =T)
  library(roxygen2, quietly = T)
  library(docstring, quietly = T)
  library(dplyr, quietly = T)
  library(pracma, quietly = T)
  library(EBImage, quietly = T)
  library(simstar, quietly = T)
  library(png, quietly = T)
  library(plotrix, quietly = T)
  library(magick, quietly = T)
  library(viridis, quietly = T)
  library(Thermimage, quietly = T)
  require(grDevices, quietly = T)
  library(imager, quietly = T)

}
