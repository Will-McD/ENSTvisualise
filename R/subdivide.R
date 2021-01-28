
subdivide = function(n=1, center=c(0,0,0), id=seq_along(ID.table$RX), ntot = 0, noise=T, flag=F, Grid.L = Global.L){

  #'Recursively Subdivide particles into a 3 x 3 x 3 grid and calculate the enstrophy.
  #'
  #' @importFrom data.table data.table
  #'
  #'
  #'@description
  #'From the input particles ID's place the particles into a 3 x 3 x 3 grid with
  #'a side length of Grid.L/(3^n) around the center given.
  #'With the given grid then calculate the curl around the defined center.
  #'
  #'This processes is continued recursively on a progressively more
  #'refined (smaller) grid with the center moving across the box containing all
  #' particles within in the halo (the center moves as a fraction of Grid.L)
  #'
  #'@param n
  #'The layer of grid refinement and corresponds to the index in the Storage list
  #'
  #'@param center
  #'The center which the grid is defined around given in physical coordinates
  #'
  #'@param id
  #'A vector containing all the particle IDs which the grid is placed over.
  #'
  #'@param ntot
  #'A running total value for subdivision number taking place.
  #'Naturally set to 0
  #'
  #'@param noise
  #' A boolean value determining if numerical noise should be calculated in
  #'determining the curl of the velocity field.
  #'It is naturally set to True.
  #'Set to False to reduce computational time when generating movies
  #'
  #' @param flag
  #' A boolean value determining if the flagged cells should be tracked, if True
  #'  then a data.table is maintained showing all cells that where subdivided.
  #'Naturally set to False as it slows down computational time.
  #'
  #'@param Grid.L
  #' A value to define the length of one side of the total grid or mesh.
  #'
  #'
  #'@export
  #'

  ds = Grid.L/3^n #cell box side length

  # sort each particle from the IDs given into a cell box within the 3x3x3 grid based on the particles x, y and z coordinates
  #stored in data.table for convienence, data.table is very easy to manipulate

  index2.0 = data.table::data.table(
    x.i=ceiling((ID.table$RX[id]-(center[1] - 1.5*ds ) ) / ds),
    y.i=ceiling((ID.table$RY[id]-(center[2] - 1.5*ds ) ) / ds),
    z.i=ceiling((ID.table$RZ[id]-(center[3] - 1.5*ds ) ) / ds),
    id = id # keep track of their ids
  )

  index2.0 = index2.0[!(x.i < 1 | x.i > 3 | y.i < 1 | y.i > 3 | z.i < 1 | z.i > 3),] # remove particles which do not belong inside any of the boxes

  # convert 3x3x3 location into the box number, an integer between 1 and 27, i.e) (3, 2, 1) -> 6
  # improves the speed of calculations by having this notation
  index2.0[,vel.index :={9*(z.i-1) + y.i*(3/y.i * (y.i-1)) + (x.i) *((x.i-3)/(x.i)) + 3}]


  box.n = box.x = box.y = box.z = rep(0, 27) # create empty vectors to store information for each box

  box.n[c(index2.0[, unique(vel.index)])] = c(index2.0[,.(count = .N), by = vel.index]$count) # store number of particles per box

  # calculate the total velocity of each the particles in each box along a given direction
  box.x[c(index2.0[, unique(vel.index)])] = c(index2.0[, sum(ID.table$VX[id]), by=vel.index]$V1)
  box.y[c(index2.0[, unique(vel.index)])] = c(index2.0[, sum(ID.table$VY[id]), by=vel.index]$V1)
  box.z[c(index2.0[, unique(vel.index)])] = c(index2.0[, sum(ID.table$VZ[id]), by=vel.index]$V1)

  # if there is a cross configuration with at least one particle per box calculate the enstrophy of this grid
  if(all(box.n[c(5,11,13,14,15,17,23)] >= 1)){ # is the cross configuration populated

    hold = compute_enst(box.x/box.n, box.y/box.n ,box.z/box.n, ds, noise=noise) # compute the enstrophy of the grid using the mean velocities of the cell boxes.
    s.ds = Grid.L/3^(n-1)
    s.i = ceiling((center[1]+Grid.L/2)/s.ds) # identify where to save the enstrophy value in the data storage list
    s.j = ceiling((center[2]+Grid.L/2)/s.ds) # identify where to save the enstrophy value in the data storage list
    s.k = ceiling((center[3]+Grid.L/2)/s.ds) # identify where to save the enstrophy value in the data storage list
    if(n==1){
      Storage[[1]] <<- hold$enst
      if(noise){Errors[[1]] <<- hold$error}
    }else{
      Storage[[n]][s.i,s.j,s.k] <<- hold$enst
      if(noise){Errors[[n]][s.i, s.j, s.k] <<- hold$error}
    }
    ntot = ntot + 1 # keep count of number of entries into the storage list.
  }

  # continuously subdivide where possible and move around the total grid of n = 1
  if(n < Global.nmax){
    for(l in 1:27){

      i = (l-1)%%3+1        # x, y, z box coordinates to move the center of the new grid around
      j = ((l-i)/3)%%3+1    # x, y, z box coordinates to move the center of the new grid around
      k = (l-(j-1)*3-i)/9+1 # x, y, z box coordinates to move the center of the new grid around

      if(box.n[l] >= 7){

        if(flag){
          Flagged <<- rbind(Flagged, list(n, l, box.n[l])) # Flag locations of where a further subdivision occurred
        }

        # move center of grid, subdivide one layer further
        # recursively call the subdivide function.
        ntot = subdivide(n=n+1, center=center+c(i-2,j-2,k-2)*ds, id=c(index2.0[, id, by=vel.index][which(vel.index==l)]$id), ntot, noise, Grid.L = Grid.L)

      }
    }
  }
  invisible(ntot) #don't want to continuously print the number of curls calculated

}
