

clean.up = function(files){

  #'
  #'Remove the files from the working directory
  #'
  #'@description remove the files from the working directory given as an input
  #'
  #'@param files
  #'List of files to remove from the working directory
  #'
  #'@examples clean.up(c('test_movie.mp4', 'frame_00006.png', 'how_to.pdf'))
  #'
  #'@export
  #'

  for(i in 1: length(files)){
    x = files[i]
    call = sprintf('rm -rf %s', x)
    system(call)
  }
}
