
do_it_all = function(halo.hdf5.file, movie.name, time.step=0.025, n.max=7, FPS=60, enst.col = NULL, keep.frames=T, keep.movies=T, draw.R200 = F, specify.frame.numb = NULL){

  #'Create a movie showing a halo and its turbulence
  #'
  #'@description Using a hdf5 file created by surfsuite of a halo from the SURFS
  #'simulation an mp4 file is created. This mp4 file is a movie of a side by
  #'side comparison of the halos evolution and enstrophy within the halo.
  #'Enstrophy is a proxy value for the halos turbulence (numerical approximation
  #' of solenoidal turbulence).
  #'
  #'The video on the left is the halo showing Dark Matter
  #'(Blue) and Baryons (Red).
  #'The video on the right is the enstrophy
  #'
  #'
  #'@param halo.hdf5.file
  #'The path to the hdf5 file containing the halo
  #'
  #'@param movie.name
  #'The name of the mp4 file to be created
  #'
  #'@param time.step
  #'The interval of time in Gyrs between frames,
  #'by adjusting this value the length of the mp4 file is altered. A lower
  #'time.step value results in more interpolated and extrapolated steps of the halo.
  #'
  #'@param n.max
  #'the maximum layer of the adaptive mesh used to calculate the enstrophy,
  #'naturally it is set to 7
  #'
  #'@param FPS
  #'The frames per second of the the mp4 file
  #'
  #'@param enst.col
  #'The colour palette used in the enstrophy video produced,
  #'if NULL the cubehelix colour palette is used.
  #'
  #'@param keep.frames
  #'If True keep each individual frame used to create the mp4 file movies,
  #'saved in temporary files.
  #'If False the temporary files are removed after creating the mp4 files.
  #'
  #'@param keep.movies
  #'If True each of the individual mp4 files created will be kept,
  #'the movie of the halo file will be saved as <movie.name>.SURFS.mp4 and the
  #'enstrophy movie will be saved as <movie.name>.ENST.mp4
  #'
  #'@param draw.R200
  #'If True a circle showing the virial radius of the halo will be overlayed on
  #'each of the frames used on the mp4 files produced.
  #'The R200 shown is scaled by the scale factor.
  #'
  #'@param specify.frame.numb
  #'An optional vector, giving the frame numbers to be used to generate images
  #'for the movie.
  #'
  #'
  #'@examples
  #'Creating a side by side movie with R200 being shown and a timestep of 0.02 Gyrs.
  #'do.it.all(halo.hdf5.file= "/Users/..../halo4.hdf", movie.name = "halo4_test", time.step=0.02, draw.R200 = T)
  #'
  #'A movie where if in full it would be 10s at 60 fps, therefore 600 frames.
  #'However to have the movie only be on the final 5 seconds of this whole movie:
  #'do.it.all(halo.hdf5.file= "/Users/..../halo4.hdf", movie.name = "halo4_test", specify.frame.numb = seq(499, 600))
  #'
  #' @export
  #'


  halo <<- read.halo(hdf5.file = halo.hdf5.file)


  cat(sprintf(' making movies for %s \n', halo.hdf5.file))

  ID.table = NA
  Storage = NA
  Population = NA

  Global.L <<- 3 * R200.calc()
  Global.nmax <<- n.max
  n.p = halo$halo$n_particles
  r = 1.5 * R200.calc()

  v1 = paste0(movie.name, '.ENST.mp4', sep="")
  v2 = paste0(movie.name, '.SURFS.mp4', sep="")

  modified.surfsmovie(halo, mp4file = v2, radius = r, dt=time.step, fps=FPS, keep.frames = keep.frames, show.R200=draw.R200, specify.frame = specify.frame.numb)
  modified.surfsmovie.enst(halo, mp4file = v1, mesh.width = 3 * R200.calc(), dt=time.step, fps=FPS,  col = enst.col, keep.frames = keep.frames, show.R200=draw.R200, specify.frame = specify.frame.numb)

  side.by.side(left = v2, right = v1, output = paste0(movie.name,'.mp4'))

  if(!keep.movies){

    cat(sprintf( 'deleting %s & %s', v1, v2))
    clean.up(c(v1, v2))

  }

}
