
unwrap = function(frame, snap=1, half.box.size = 105){

  #'
  #'Unwarp a halo
  #'
  #' @importFrom pracma mod
  #'
  #'@description
  #'Halos can sometimes get wrapped around the edges of their box and
  #'as a result not be fully connected.
  #'This function takes a snapshot and unwraps the halo.
  #'
  #'It is assumed that the box size is 210 Mpc/h
  #'
  #'@param frame
  #'A list or vector type object containing the partice positions at a given
  #'time
  #'
  #'@param snap
  #'A numeric value for the snapshot number
  #'
  #'@param half.box.size
  #'An optional value for the box width / 2 in simulaiton units (Mpc/h).
  #'It is naturally set as 105 as the simulation boxes are 210 Mpc/h
  #'
  #'@export
  #'

  for(i in 1:3){
    if((max(frame[[i]]) - min(frame[[i]])) >  half.box.size){

      half.range = rep( half.box.size, n.p)
      frame[[i]] = pracma::mod(frame[[i]] + half.range, 2 * half.box.size)
      print(paste(c('x', 'y','z')[i],snap ,'unwrapped'))
    }
  }

  return(frame)
}
