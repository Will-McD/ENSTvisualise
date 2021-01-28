
do_it_all = function(halo.hdf5.file, movie.name,
                     time.step=0.025,
                     n.max=6,
                     FPS=60,
                     enst.col = NULL,
                     keep.frames=T,
                     keep.movies=T,
                     draw.R200 = F,
                     specify.frame.numb = NULL,
                     dynam.plot =F
                     ){

  #'Create a movie visualising a halo's morphology and enstrophy over time.
  #'
  #'@description Using a hdf5 file created by surfsuite of a halo from the SURFS
  #'simulation an mp4 file is created. This mp4 file is a movie of a side by
  #'side comparison of the halos evolution and enstrophy within the halo.
  #'Enstrophy is a proxy value for the halos turbulence (numerical approximation
  #' of solenoidal turbulence).
  #'
  #'The video on the left is the halo showing Dark Matter
  #'(Blue) and Baryons (Red).
  #'The video on the right is the density weighted enstrophy
  #'
  #'
  #'@param halo.hdf5.file
  #'The path to the hdf5 file containing the halo
  #'
  #'@param movie.name
  #'The name of the mp4 file to be created, include the .mp4
  #'
  #'@param time.step
  #'The interval of time in Gyrs between frames,
  #'by adjusting this value the length of the mp4 file is altered. A lower
  #'time.step value results in more interpolated and extrapolated steps of the halo.
  #'
  #'@param n.max
  #'the maximum layer of the adaptive mesh used to calculate the enstrophy.
  #'Naturally it is set to 6
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
  #'@param dynam.plot
  #'A boolean value to determine if the plots size and smoothing should be
  #'adjusted for the levels of the adaptive mesh occupied below the maximum
  #'level defined.
  #'Naturally set to False
  #'
  #'If True it is enabled;
  #'Reduces computational time but reduces the quality of the image.
  #'The resolution and smoothness of the images produced are of lowered as the
  #'layers of the mesh when compiled will only expand
  #'to fit the highest occupied layer and smooth the img at that level.
  #'
  #'If False it is disabled; When compiling the layers of the mesh they will all
  #' be expanded to the Global.nmax and smoothed at that level.
  #'
  #'    !!!!NOT FULLY TESTED YET !!!!!
  #'It is in a stage of working, however has not be tested as extensively.
  #'
  #'
  #'@examples
  #'Creating a side by side movie with R200 being shown and a timestep of 0.02 Gyrs.
  #'do.it.all(halo.hdf5.file= "/Users/..../halo4", movie.name = "halo4_test.mp4", time.step=0.02, draw.R200 = T)
  #'
  #'A movie where if in full it would be 10s at 60 fps, therefore 600 frames.
  #'However to have the movie only be on the final second of this whole movie:
  #'do.it.all(halo.hdf5.file= "/Users/..../halo4", movie.name = "halo4_test.mp4", specify.frame.numb = seq(499, 600))
  #'
  #' @export
  #'


  halo <<- read.halo(hdf5.file = halo.hdf5.file)


  cat(sprintf('Making  %s \n', movie.name))

  ID.table = NA
  Storage = NA
  Population = NA

  Global.L <<- 3 * R200.calc(species = halo$particles$species)
  Global.nmax <<- n.max
  n.p = halo$halo$n_particles
  r = 1.5 * R200.calc()

  v1 = paste0('ENST_', movie.name, sep="")
  v2 = paste0('SURFS_',movie.name,  sep="")

  cat(sprintf('CREATING SURFS MOVIE \n'))
  modified.surfsmovie(
    halo, mp4file = v2, radius = r, dt=time.step, fps=FPS,
    keep.frames = keep.frames, show.R200=draw.R200, specify.frame = specify.frame.numb
    )

  cat(sprintf('CREATING ENST MOVIE \n'))

  modified.surfsmovie.enst(halo, mp4file = v1, mesh.width = Global.L,
                           dt=time.step, fps=FPS,  col = enst.col,
                           keep.frames = keep.frames, show.R200=draw.R200,
                           specify.frame = specify.frame.numb, dynam.plot = dynam.plot
                           )

  side.by.side(left = v2, right = v1, output = paste0(movie.name))

  if(!keep.movies){

    cat(sprintf( 'deleting %s & %s', v1, v2))
    clean.up(c(v1, v2))

  }

}
