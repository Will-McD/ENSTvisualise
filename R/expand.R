
expand = function(x, n=3){

  #'
  #'Expand a 2D matrix or matrix array by a factor of 'n'
  #'
  #'@description Scale up 'expand' a matrix "x" by a scale factor of "n".
  #' Then return the expanded matrix.
  #' i.e.) if x is a 3x3 matrix and n = 3 return a 9x9 matrix where each entry
  #' in x is now equivalent to 9 values within the new 9x9 matrix.
  #'
  #'
  #'@param x
  #' A matrix or matrix array type object to be expanded
  #'
  #'@param n
  #' The scale factor,
  #' which the side lengths of the matrix will be increased by a factor of.
  #'
  #'@export
  #'


  return (t(array(rep(t(array(rep(x,each=n),dim(x)*c(n,1))),each=n),rev(dim(x)*n))))
}
