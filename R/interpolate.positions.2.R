
interpolate.positions.2 = function(track, snapshots, t) {

  #'
  #'Interpolate and extrapolate the position of particles
  #'
  #'@description
  #'Determines from the given look-back time and time-steps if a set of particles
  #' positions needs to be extrapolated or interpolated.
  #'If a particles position needs to be interpolated, the particles positions
  #'will be estimated using a cubic fitted between the two given snapshots to
  #'fit a position within the given time step.
  #'
  #'Essentially makes the movies smoother in appearance by increasing the
  #'number of possible frames and therefore the resultant fps without
  #'sacrificing movie length as it adds extra frames between frames.
  #'
  #'@param track
  #' A list containing a read in halo from Surfsuite.
  #'
  #'@param snapshots
  #' A data.frame object containing particle id's, velocities and positions
  #' at given redshifts and look-back times based on the track list.
  #'
  #'@param t
  #' A value for the look-back time (Gyrs) for the step to be
  #' interpolated or extrapolated too
  #'
  #'@param dt
  #' A value of the time step interval between look-back times
  #'
  #' @export
  #'

  # t = lookback time

  # determine interval
  n.snapshots = dim(snapshots)[1]
  if (t>=snapshots$t[1]) {
    extrapolate = T
    i0 = 1
  } else if (t<=snapshots$t[n.snapshots]) {
    extrapolate = T
    i0 = n.snapshots
  } else {
    extrapolate = F
    i1 = which(snapshots$t<=t)[1]
    i0 = i1-1
  }

  # interpolate/extrapolate
  if (extrapolate) {

    t0 = -snapshots$t[i0]
    dt = (-t)-t0

    str = snstr(snapshots$index[i0])
    x0 = cbind(track$particles[[str]]$rx,track$particles[[str]]$ry,track$particles[[str]]$rz)
    v0 = cbind(track$particles[[str]]$vx,track$particles[[str]]$vy,track$particles[[str]]$vz)*snapshots$vfactor[i0]
    x = x0+v0*dt
    v = v0
  } else {

    t0 = -snapshots$t[i0]
    t1 = -snapshots$t[i1]
    ht = t1-t0
    dt = ((-t)-t0)/ht

    str = snstr(snapshots$index[i0])
    x0 = cbind(track$particles[[str]]$rx,track$particles[[str]]$ry,track$particles[[str]]$rz)
    v0 = cbind(track$particles[[str]]$vx,track$particles[[str]]$vy,track$particles[[str]]$vz)*snapshots$vfactor[i0]*ht

    str = snstr(snapshots$index[i1])
    x1 = cbind(track$particles[[str]]$rx,track$particles[[str]]$ry,track$particles[[str]]$rz)
    v1 = cbind(track$particles[[str]]$vx,track$particles[[str]]$vy,track$particles[[str]]$vz)*snapshots$vfactor[i1]*ht

    q = 2*(x0-x1)+v0+v1
    p = 3*(x1-x0)-2*v0-v1
    x = x0+v0*dt+p*dt^2+q*dt^3
    v = v0 + 2*p*dt + 3*q*dt^2

  }

  return(list(x, v))

}
