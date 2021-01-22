
plot_enst = function(max.layer=5,smoothing=1/3,gamma=0.01, scale=NULL, final=F, col.palette = NULL, bg.col='black') {

  #'
  #' Plot the enstrophy from the refined grid algorithm
  #'
  #' @importFrom graphics rasterImage
  #' @importFrom EBImage gblur
  #' @importFrom cooltools nplot rasterflip
  #'
  #'@description Return an image which combines each of the layers from
  #'the refined grid used to calculate enstrophy.
  #' Each layer of the enstrophy is collapsed from a 3D array into a 2D array
  #'  using the pop.weighted.enst function, before then being expanded to the
  #'  same size 2D array as that of the max.layer selected.
  #'
  #' As each layer is a factor of 3 greater than the previous
  #' ie) 3x3x3 -> 9x9x9 -> 27x27x27 .... each layer is expanded by this logic.
  #'
  #' The 2D layers are then summed together and smoothed in accordance with the
  #' smoothing values selected using a Gaussian filter/brush of the gblur function.
  #'
  #'
  #' The final 'layered' image from superimposing each smoothed layer is then
  #' smoothed again by the gamma value before plottinh the matrix in a cube-helix c
  #' olour mapping as a raster image.
  #'
  #'
  #'@param max.layer
  #'The highest layer within the list structure to include.
  #'
  #'
  #'@param smoothing The level of smoothing within the gblur function, the sigma
  #' values. Naturally a value of 1/3 is assigned.
  #'
  #'The smoothing values scale with the size of the grid selected to account for
  #' the expansion of each grid.
  #'
  #'@param gamma alters the prominence of the fine structure in the image, where
  #' gamma is a constant to scale the values of the matrix 'X' in accordance with
  #' log(X/max(X) + gamma)
  #'
  #'
  #'@param scale
  #'A boolean value. Determining if the frame of the image should be scaled.
  #'If TRUE the image plotted or shown is scaled in from the matrix of enstrophy,
  #' ie) less of the image is shown.
  #'
  #'@param final
  #'A boolean value, True if plot is of the final frame of a halo, ie) at z=0
  #'
  #'
  #'@param col.palette
  #' An optional value.
  #' The colour palette that will be used to plot the enstrophy.
  #'
  #'If NULL then cubehelix(1e3) is used.
  #'
  #'@param bg.col
  #' A string, the background colour for the plot of the enstrophy.
  #' Naturally set to black.
  #'
  #'
  #' @export
  #'


  img = array(NA,c(3^(max.layer-1),3^(max.layer-1),max.layer-1))
  max.smooth = 3^(max.layer-2)

  for (i in 2:max.layer) {
    array2d = pop.weighted.enst(Storage, i)
    array2d[is.na(array2d)] = 0
    array2d = expand(array2d,n=3^(max.layer-i))
    img[,,i-1] = lim(gblur(array2d,min(smoothing*3^(max.layer-i)),max.smooth),min=0,max=1e99)
  }

  print(dim(array2d))

  if(is.null(col.palette)){col = cubehelix(1e3)}else{col = col.palette} # define the colour palette to use

  par(bg=bg.col)

  if(is.null(scale)){s = 1}else{s=scale}

  #nplot(xlim=c(-dim(array2d)[1] * s, dim(array2d)[1] * s), ylim=c(-dim(array2d)[1] * s,dim(array2d)[1] * s),cex=0.25, pty='s') # empty plot as template for the raster image
  nplot(xlim=c(-dim(array2d)[1] / s, dim(array2d)[1] / s), ylim=c(-dim(array2d)[1] / s,dim(array2d)[1] / s),cex=0.25, pty='s') # empty plot as template for the raster image

  img = apply(img,c(1,2),sum) # collapse the array of image matrices as required.
  if(final){img = img/10}

  hold.img <<- img

  floor = 1e-7
  roof = 3e4


  img = lim(img, min=floor, max=roof)


  img = log10(1e2*img + 1.5) * 0.175



  colvals = pmin(1e3,pmax(1,ceiling(img*1e3)))

  img = array(col[pmin(1e3,pmax(1,ceiling(img*1e3)))],dim(img)) # convert values to colour id from colour pallete

  return(rasterImage(rasterflip(img),-dim(array2d)[1],-dim(array2d)[1],dim(array2d)[1],dim(array2d)[1])) # plot as raster image.


}
