
change.mp4.length = function(time.mult , input, output, overwrite=F){


  #'
  #'Change the length of a .mp4 file
  #'
  #'@description
  #'Creates a and saves a copy of a .mp4 file with a different length to that of the original.
  #'The length of the movie is changed by the percent.diff given.
  #'
  #'@param time.mult
  #'The value by which the time of the movie will be scaled by
  #'.mp4 file. Calculated as current length / desired length
  #'
  #'i.e) if the current length is 10 seconds and the desired length is 8 seconds
  #'the percent.diff would be 10/8.
  #'
  #'@param input
  #'The .mp4 file to change length of
  #'
  #'@param output
  #'The name of the .mp4 file that the copy with the altered length will be saved as.
  #'Cannot use the same name as the origonl version, unless choosing to overwrite original.
  #'
  #'@param overwrite
  #'An optional boolean value, if True will delete the original version and replace with the new, altered length version.
  #'
  #'@examples
  #'Changing an mp4 file from 10s to 5s
  #'change.mp4.length(0.5, test.mp4, shorter_test.mp4)
  #'
  #'@export
  #'

  if(overwrite){
    call = sprintf('ffmpeg -i %s -filter:v "setpts=%f*PTS" %s', input, time.mult, 'temp_file.mp4')
    system(call)

    call = sprintf('rm -rf %s',input)
    system(call)

    call = sprintf('mv %s %s','temp_file.mp4', output)
    system(call)

  }else{

    call = sprintf('ffmpeg -i %s -filter:v "setpts=%f*PTS" %s', input, time.mult, output)
    system(call)

  }



}
