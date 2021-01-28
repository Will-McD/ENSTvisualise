
time_ff = function(species = halo$particles$species, mass.frac = c(1.17e8, 6.29e8)){

  #'
  #' Free Fall Time in Gyrs
  #'
  #'@param species
  #'A list or vector type object containing numeric values for each particle in the halo's species
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



  R = R200.calc(species = species, mass.frac = mass.frac) #[Mpc/h]
  G = (6.67430e-11) * (3.403679e-59) / (5.02785e-31) # [Mpc^3 / (Solar Mass) / s^2]
  Ht = 70 * 3.2408e-20 # hubble const assumed as H_0 = 70 km/s Mpc, convert to [Hz]
  M.t = sum(mass.frac[species]) # total mass of particles within the halo [Solar Mass/ h]
  t.ff = 0.5 * (pi * R^(3/2)) / sqrt(2*G*(M.t))
  return(t.ff/cst$yr/1e9) #[Gyrs]
}
