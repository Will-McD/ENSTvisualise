
generate.empty.data = function(max.cells = 3^Global.nmax){

  #' Make Empty List of n many 3^(n-1) Arrays
  #'
  #'@description Creates a list of length 'n', where each entry 'n' is an empty
  #' array of dimension = (3^(n-1), 3^(n-1), 3^(n-1)).
  #'Each entry 'n' corresponds to a layer of the adaptive grid, where the level
  #' of refinement increases with n by a factor of 3.
  #'
  #' The initial layer is a buffer layer as its just a box with sidelength 3*viral radius
  #'
  #'@param max.cells
  #'The maximum number of cells for one grid length, the largest array in
  #'the list is defined as max.cells^3
  #'
  #'
  #'@export
  #'

  L.grid = list()
  #L.grid[[1]] = NA
  n = log(max.cells)/log(3)
  for(i in 1:n){
    L = 3^(i-1)
    if(L == 1){
      L.grid[[i]] = 0
    }else{
      L.grid[[i]] = array(0, dim = c(L, L, L))
    }
  }
  return(L.grid)
}
