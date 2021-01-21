
time_ff = function(species = halo$particles$species){

  #'
  #' Free Fall Time in Gyrs
  #'
  #' NOT FINISHED YET !
  #'
  #'@export
  #'



  R = R200.calc() #[Mpc/h]
  G = (6.67430e-11) * (3.403679e-59) / (5.02785e-31) # [Mpc^3 / (Solar Mass) / s^2]
  Ht = 70 * 3.2408e-20 # hubble const assumed as H_0 = 70 km/s Mpc, convert to [Hz]
  p.mass = c(6.29e8, 1.17e8) # mass of particles in simulation, (DM, Baryon) [Solar Mass / h]
  M.t = sum(p.mass[species]) # total mass of particles within the halo [Solar Mass/ h]
  t.ff = 0.5 * (pi * R^(3/2)) / sqrt(2*G*(M.t)) # free fall time [s]
  return(t.ff/cst$yr/1e9) #[Gyrs]
}
