
generate.particle.id.table.4 = function(x, v, species=1){

  #' Make Table of Particle IDs, Location and Velocities
  #'
  #' @description  Returns a data.table of each of the particles of the selected
  #'species in the cluster. The data table contains each particles location and
  #'velocity in SI units relative to the center of Mass
  #'(each particles velocity and location in converted to SI units from
  #'L210N1024NR simulation units).
  #'
  #'The particles as assigned an ID by their index in the table.
  #'The table has dimensions = (6, number of particles of selected species)
  #'
  #'
  #' @param Halo
  #'A hdf5 file that has been revrived using SURFsuite already read into
  #'Rstudio using h5read(). Contains all the
  #'particles locations, velocities, species, ID, mass and scale factor.
  #'
  #'@param Snapshot
  #'A Boolean value.
  #'
  #'TRUE when a halo file has multiple snapshots within it.
  #'If TRUE it will retrieve the halos snapshot_199 (z = 0)
  #'(this is designed for the SURFS L210N1024NR simulation
  #'complied through Surfsuite).
  #'
  #'FALSE when halo file does not contain multiple snapshots and this
  #'function will assume that the snapshot is at z=0
  #'
  #'@param Species
  #'An intiger value of either 1 or 2.
  #'
  #'    1 = baryonic particles (gas)
  #'
  #'    2 = dark matter particles
  #'
  #' Naturally set to 1. This is used to calculate the enstrophy, 
  #' which is dependant on gas particles, therefore species 1.
  #'
  #'@export
  #'

  # identify which frame or snapshot to use

  #center the halo based on mass and velocity
  p.mass = c(1.17e8, 6.29e8) # mass of particles in simulation, (Baryon, DM) [Solar Mass / h]
  #p.mass = (f/sum(f)) # mass fraction of particles in the simulation
  m = p.mass[halo$particle$species]
  cm = c(sum(x[,1]*m),sum(x[,2]*m),sum(x[,3]*m))/sum(m)
  vcm = c(sum(v[,1]*m),sum(v[,2]*m),sum(v[,3]*m))/sum(m) # essentially centering by momentum

  x = sweep(x, 2, cm)
  v = sweep(v, 2, vcm)

  rx = x[, 1][which(halo$particles$species == species)]
  ry = x[, 2][which(halo$particles$species == species)]
  rz = x[, 3][which(halo$particles$species == species)]

  vx = v[, 1][which(halo$particles$species == species)]
  vy = v[, 2][which(halo$particles$species == species)]
  vz = v[, 3][which(halo$particles$species == species)]


  #create the data.table of particle ID, location and velocities
  cluster.data =  data.table(
    RX = rx,
    RY = ry,
    RZ = rz,
    VX = vx,
    VY = vy,
    VZ = vz
  )

  return(cluster.data)
}
