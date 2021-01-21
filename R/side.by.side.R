
side.by.side = function(left, right, output){

  #'
  #'Combine two .mp4 files into a single .mp4 file
  #'
  #'@description Combine two .mp4 files of the same length and resolution into
  #'a single .mp4 file. The two movies play side by side to one another. The
  #'new .mp4 file is saved in the working directory.
  #'
  #'@param left
  #'The .mp4 file which will be played on the left hand side of the new .mp4 file
  #'
  #'@param right
  #'The .mp4 file which will be played on the right hand side of the new .mp4 file
  #'
  #'@param output
  #'The name of the new .mp4 file which will be created
  #'
  #'@examples
  #'combining two .mp4 files in the working directory,
  #'
  #'
  #'side.by.side('test_left.mp4', 'test_right.mp4', 'a_new_movie.mp4')
  #'
  #'@export
  #'


  call = sprintf('ffmpeg -i %s -i %s -pix_fmt yuv420p -filter_complex hstack %s', left, right, output)
  system(call)

  print(sprintf('completed %s', output))
}
