

clean.up = function(files){

  #'
  #'Remove files from the working directory.
  #'
  #'Ideally used to remove many files at once or following creating a
  #'side.by.side movie and then deleting the two parent movies.
  #'
  #'@description remove the files from the working directory given as an input
  #'
  #'@param files
  #A vector of file names to remove from the working directory
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
