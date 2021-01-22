
compute_enst = function(X, Y, Z, ds, noise=T, normalise = NULL){
  #'
  #'Compute the Norm of the Curl from the given mesh
  #'
  #' @importFrom cooltools vectornorm
  #'
  #'
  #'
  #'@description Calculates the normal curl from the given grid about the center of the grid,
  #'the curl is calculated through numerical approximations of partial derivatives
  #'across the grid cells.
  #'
  #'Using the cross configuration, ie) ignoring the corner boxes of the array,
  #'the central 3D cross (boxes with face to face contact with the central box),
  #'can be used to calculate the curl around the central box within the 3x3x3 array of velocities.
  #'Therefore 9 partial derivatives will be numerically approximated from the 3x3x3 array.
  #'
  #'The curl formula is: curl of f(x,y,z) = ( (dz/dy - dy/dz)i , (dx/dz - dz/dx)j , (dy/dx - dx/dy)k )
  #'
  #'Calculated using simulaiton length units [Mpc /h] and velocity units of [(simulaiton length units)/Gyrs],
  #' as the interpolated velocity positions are returned in Gyrs, therefore enstrophy is given in Gyrs^-2.
  #'This allows for less conversion of units, a more natural normalisation of enstrophy & conforms with convention
  #' of natural units as galaxy clusters use Gyrs as a natural unit of time.
  #'
  #'@param X,Y,Z
  #'A 3 x 3 x 3 array with each cell containing the mean velocity of the
  #'particles within each cell over-layed on the halo w.r.t with
  #'the x,y and z axis
  #'
  #'@param ds
  #'The side length of the cells within the grid
  #'
  #'@param noise
  #'A boolean value determining if numerical noise should be calculated in
  #'determining the curl of the velocity field. It is naturally set to True
  #'
  #'@param normalise
  #'An optional value, normalizes the enstrophy and error returned.
  #'The normalize value in units of Gyrs^2
  #'
  #'@export
  #'

  d.sim = c(2 *ds, ds, ds) # [Mpc/h]
  d = d.sim
  #d = d.sim * 3.086e19 # [km], convert [Mpc/h] to [km/h] as velocity is calculated in [km/s]

  #

  if(is.nan(X[14])){X[14]=0}
  if(is.nan(Y[14])){Y[14]=0}
  if(is.nan(Z[14])){Z[14]=0}

  #create indexing vectors
  dx.1 = c(15, 15, 14)
  dx.2 = c(13, 14, 13)
  dy.1 = c(17, 17, 14)
  dy.2 = c(11, 14, 11)
  dz.1 = c(23, 23, 14)
  dz.2 = c(5, 14, 5)

  # Calculate partial derivatives of the velocities of each particle w.r.t each axis of the given box.
  # Using the cross configuration only the boxes in the corners of the 3x3x3 can be ignored, therefore 9 different partial derivatives are needed.


  dvx.dx = (X[dx.1]-X[dx.2])/d
  dvx.dy = (X[dy.1]-X[dy.2])/d
  dvx.dz = (X[dz.1]-X[dz.2])/d

  dvy.dx = (Y[dx.1]-Y[dx.2])/d
  dvy.dy = (Y[dy.1]-Y[dy.2])/d
  dvy.dz = (Y[dz.1]-Y[dz.2])/d

  dvz.dx = (Z[dx.1]-Z[dx.2])/d
  dvz.dy = (Z[dy.1]-Z[dy.2])/d
  dvz.dz = (Z[dz.1]-Z[dz.2])/d


  # Compute the curl w.r.t each axis in the box
  # Curl of f(x,y,z) =  (dz/dy - dy/dz)i + (dx/dz - dz/dx)j + (dy/dx - dx/dy)k
  # Curl = c(dvz.dy-dvy.dz, dvx.dz-dvz.dx, dvy.dx-dvx.dy)
  # Curl.x = c(a, bb, bc, cb, cc)

  curl.x = c(
    dvz.dy[1]-dvy.dz[1],
    dvz.dy[2]-dvy.dz[2],
    dvz.dy[2]-dvy.dz[3],
    dvz.dy[3]-dvy.dz[2],
    dvz.dy[3]-dvy.dz[3]
  )

  curl.y = c(
    dvx.dz[1]-dvz.dx[1],
    dvx.dz[2]-dvz.dx[2],
    dvx.dz[2]-dvz.dx[3],
    dvx.dz[3]-dvz.dx[2],
    dvx.dz[3]-dvz.dx[3]
  )

  curl.z = c(
    dvy.dx[1]-dvx.dy[1],
    dvy.dx[2]-dvx.dy[2],
    dvy.dx[2]-dvx.dy[3],
    dvy.dx[3]-dvx.dy[2],
    dvy.dx[3]-dvx.dy[3]
  )

  # Compute stable enst for center of cross
  enst = 0.5 * cooltools::vectornorm(c(curl.x[1], curl.y[1], curl.z[1]))^2

  if(noise){error = 0.5*sd(cooltools::vectornorm(expand.grid(curl.x[-1], curl.y[-1], curl.z[-1])))^2}else{error=NA}

  # Normalise enst
  if(!is.null(normalise)){
    enst = enst * normalise
    error = error * normalise
  }

  return(list('enst' = enst, 'error' = error))

}
