
read.halo = function(path='/Users/will_mcd/Documents/project/the halos/', halo.file='halo1track.hdf', hdf5.file = NULL){

  #'Read in the halo hdf5 file
  #'
  #'@importFrom hdf5 h5read
  #'
  #'@description  Read in the data from the halo's hdf5 file (halo.file) from
  #'the given location (path) and return all the data contained in the file as
  #'a list type object.
  #'This function has two methods of reading in the file, either through entering
  #'the hdf5 files location or by entering in the path for where the file is
  #'stored and the name of the file separately.
  #'
  #'The list should contain each particles location in cartesian coordinates,
  #'velocities, mass and the species ID within the halo at a given snapshot.
  #'Each of these values will be in the simulations units of measurement.
  #'
  #'If there are multiple snapshots within the read in file, there will be
  #'entries for each snapshot containing this information
  #'
  #'The list should contain other information such as simulation box size,
  #'particle count, ect...
  #'
  #'@param path
  #'The path for the hdf5 file within the computer
  #'
  #'@param halo.file
  #'The hdf5 files name,
  #'ie) halo1.h5
  #'
  #'
  #'@param hdf5.file
  #'An optional value, the path to the halo's hdf5 file
  #'
  #'@examples calling a file by its location within the device:
  #'read.halo(hdf5.file = '/Users/..../test_halo.h5')
  #'
  #'If wanting to call in multiple halo all from the same directory or file:
  #'halos = c('halo_1', 'halo_2', ..., 'halo_x')
  #'for(i in 1:length(halos)){
  #'read.halo(path='the/file/containing/halos/', halo.file=halos[i])
  #'}
  #'
  #'@export
  #'


  #path = '/Users/will_mcd/Documents/project/the halos/'
  if(is.null(hdf5.file)){filename = paste0(path,halo.file)}
  if(!is.null(hdf5.file)){filename = paste0(hdf5.file)}
  halo = h5read(filename,'/')
  return(halo)
}
