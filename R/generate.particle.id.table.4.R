
generate.particle.id.table.4 = function(x, v, select.species = 1, species=track$particle$species, p.mass = c(1.17e8, 6.29e8)){

  #' Make Table of Particle IDs, Location and Velocities
  #'
  #' @importFrom data.table data.table
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
  #'@param x
  #'A matrix, array type object containing particle positions [Mpc/h]
  #'
  #'
  #'@param v
  #'A matrix, array type object containing particle velocities [km/s]
  #'
  #'
  #'@param species
  #'An  array type object containing the species number for every particle
  #' within the halo being observed.
  #'
  #'
  #'@param select.species
  #'An intiger value of either 1 or 2.
  #'
  #'    1 = baryonic particles (gas)
  #'
  #'    2 = dark matter particles
  #'
  #' Naturally set to 1. This is used to calculate the enstrophy,
  #' which is dependant on gas particles, therefore species 1.
  #'
  #' @param p.mass
  #' A vector object containing the mass values for each of the species within the
  #' simulation in simulaiton units [Solar Mass / h], in the order of the
  #' species indexes.
  #'
  #' For L210N1024NR where there are only baryon (1) and dark matter (2)
  #' therefore p.mass is naturally set to (1.17e8, 6.29e8).
  #'
  #' This is used for calculating the center of mass of each snapshot or frame,
  #' so that each particle can be centered on the c.o.m.
  #'
  #'
  #'@examples
  #'To make a data.table of only the baryons within a halo,
  #'
  #'x = interpolate.positions.2(track, snapshots, t.plot[frame])[[1]]
  #'v = interpolate.positions.2(track, snapshots, t.plot[frame])[[2]]
  #'generate.particle.id.table.4(x, v, select.species = 1, species=track$particle$species, p.mass = c(1.17e8, 6.29e8))
  #'
  #'@export
  #'

  # identify which frame or snapshot to use

  #center the halo based on mass and velocity
  #p.mass = c(1.17e8, 6.29e8) # mass of particles in simulation, (Baryon, DM) [Solar Mass / h]
  #p.mass = (f/sum(f)) # mass fraction of particles in the simulation
  m = p.mass[species]
  cm = c(sum(x[,1]*m),sum(x[,2]*m),sum(x[,3]*m))/sum(m)
  vcm = c(sum(v[,1]*m),sum(v[,2]*m),sum(v[,3]*m))/sum(m) # essentially centering by momentum

  x = sweep(x, 2, cm)
  v = sweep(v, 2, vcm)

  rx = x[, 1][which(species == select.species)]
  ry = x[, 2][which(species == select.species)]
  rz = x[, 3][which(species == select.species)]
  vx = v[, 1][which(species == select.species)]
  vy = v[, 2][which(species == select.species)]
  vz = v[, 3][which(species == select.species)]


  #create the data.table of particle ID, location and velocities
  cluster.data =  data.table::data.table(
    RX = rx,
    RY = ry,
    RZ = rz,
    VX = vx,
    VY = vy,
    VZ = vz
  )

  return(cluster.data)
}
