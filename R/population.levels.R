
population.levels=function(n.max = Global.nmax, Grid.L = Global.L){
  #'
  #'Arrays of particle populations in cluster
  #'
  #'@description
  #'Creates a list of arrays with distributions of particle populations based on
  #' the particles positions in a cluster.
  #'This is done at varying levels of grid refinement, progressively more cells
  #'per gird as a function of 3^n.
  #'
  #'n is the grid layer or level in the list.
  #'
  #'@param n.max
  #'an optional value, the maximum level of the grid and number of entries in the list.
  #'It is naturlly set to the Global.nmax (A global constant value), which is
  #'characteristically set to 6 or 7.
  #'i.e) if n.max = 5, the most refined grid will be (3^5-1 = 81) an 81x81x81 array.
  #'
  #'@param Grid.L
  #'The width or length of the grid
  #'
  #'
  #'
  #'@export
  #'


  for(n in 2:n.max){
    l=3^(n-1)
    n.ds = Grid.L/l

    t = length(ID.table$RX)

    for(i in 1:t){
      x.i = ceiling((ID.table$RX[[i]] + Grid.L/2 )/ (n.ds) )
      y.i = ceiling((ID.table$RY[[i]] + Grid.L/2 )/ (n.ds) )
      z.i = ceiling((ID.table$RZ[[i]] + Grid.L/2 )/ (n.ds) )

      if (x.i>=1 & x.i<=l & y.i>=1 & y.i<=l & z.i>=1 & z.i<=l) {

        Population[[n]][x.i, y.i, z.i] <<- Population[[n]][x.i, y.i, z.i]  + 1
      }
    }
  }
}



