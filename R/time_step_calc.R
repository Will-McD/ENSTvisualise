

time_step_calc = function(movie.length, fps, max.lbt = 12.72){

  #'
  #'time step needed in the movie
  #'
  #'@description
  #'Calculate the time step needed in a movie to get a movie of the desired length and fps input
  #'
  #'@param movie.length
  #'The length of the desired movie
  #'
  #'@param fps
  #'The frames per second of the desired movie
  #'
  #'@param max.lbt
  #'The max look back time of the movie in Gyrs, if from surfsuite it is most likely to be 12.72 Gyrs
  #'
  #'@export

  time.step = max.lbt / (movie.length * fps)
  cat(sprintf('required look time-step: %f  Gyrs', time.step))

}
