
density_weighted_enst = function(data, layer){

  #'
  #'Collapse a 3D array into a 2D array and weight by particle density.
  #'
  #'@description Return a 'n by n'  2D array from the parsed 'n by n by n' 3D structure.
  #' The data is collapsed along the Z axis and weighted by the population along each column of cells on the Z axis in the 3D array
  #'
  #'@param data
  #'The list containing the enstrophy or noise values where each entry correlates to a 3D array of values
  #'
  #'@param  layer
  #'The entry to collapse from the data provided. ie) n=2 is the second entry in the data list provided.
  #'
  #'@export
  #'

  array2d = apply(data[[layer]]*Population[[layer]],c(1,2),sum)/apply(Population[[layer]],c(1,2),sum)
  array2d[is.na(array2d)] = 0

  return(array2d)

}
