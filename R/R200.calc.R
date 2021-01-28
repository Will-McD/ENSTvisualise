
R200.calc = function(species = halo$particles$species, mass.frac = c(1.17e8, 6.29e8)){

  #'
  #'Calculates The Virial Radius
  #'
  #'@description
  #' Calculates the viral radius (R200) of a halo
  #' in SURFS simulation units of Mpc/h
  #'
  #'@param species
  #'A list or vector type object containing numeric values for each particle
  #'in the halo's species
  #'
  #'
  #'@param mass.frac
  #' A vector containing the mass fraction of the individual species of
  #' particle within the halo in simulation units (solar_mass / h).
  #'Naturally set to the mass fraction from SURFS L210N1024NR.
  #'
  #'              (Baryons, Dark Matter)
  #'
  #'@export
  #'

  G = (6.67430e-11) * (3.403679e-59) / (5.02785e-31) # [Mpc^3 / (Solar Mass) / s^2]
  Ht = 70 * 3.2408e-20 # hubble const assumed as H_0 = 70 km/s Mpc converted to [Hz]
  p.mass = mass.frac # mass of particles in simulation, (Baryon, DM) [Solar Mass / h]
  Mt = sum(p.mass[species]) # total mass of particles within the halo [Solar Mass/ h]

  R200 = (( Mt * G) / (100 * Ht^2)) ^ (1/3) * 1e-3 # the virial radius defined as R_200 in simulation units [Mpc/h]
  return(R200)
}
