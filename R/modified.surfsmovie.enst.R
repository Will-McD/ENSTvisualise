
modified.surfsmovie.enst = function(halo, radius = NULL, aspect = 1,
                                    mp4file, fps = 60,
                                    Global.nmax = 7,
                                    calc.noise = F,
                                    H0 = 70, OmegaM = 0.3, OmegaL = 0.7,
                                    velocity.conversion = 0.00102269032*(H0/100), # [-] (velocity unit)/(length unit/Gyr) at z=0
                                    keep.frames = TRUE,
                                    rotation = 1,
                                    scale = T,
                                    dt = 0.05, # [Gyrs]
                                    f = c(1.17e8, 6.29e8), # (Baryon, Dark Matter) [Solar mass /h]
                                    png.size = c(300,300),
                                    specify.frame = NULL,
                                    col = NULL,
                                    show.R200 = F,
                                    ...) {


  #' Generates a movie from a given halo hdf5 file from surfsuite.
  #'
  #' @importFrom celestial cosdistTravelTime
  #' @importFrom png writePNG
  #' @importFrom magick image_read image_annotate
  #' @importFrom simstar sphview
  #' @importFrom grDevices dev
  #' @importFrom cooltools nplot rasterflip lim griddata2 kde2 quadrupole rotation3
  #' @importFrom grDevices pdf dev.off col2rgb
  #' @importFrom graphics axis lines par rasterImage rect text
  #' @importFrom ggplot qplot
  #'
  #'@description
  #'This is a modified version of the surfsmovie function available in the Simstar package.
  #'
  #'creates an .mp4 file from the provided halo list given showing the evolution
  #'of particles identified to be in the halo at z=0
  #'
  #'
  #'@param halo
  #'A list containing the halo information from a given hdf5 file from surfsuite
  #'
  #'@param radius
  #'An optional value of the radius given in the simulation units of movie to be
  #'shown, if NULL then 1.5 times the virial radius will be used.
  #'
  #'@param aspect
  #'An optional value for the aspect ratio of the .mp4 file produced, naturally aspect ratio is 1 (a square).
  #'
  #'@param mp4file
  #'The file name of the .mp4 file which will be produced.
  #'
  #'@param fps
  #'An optional value for the frames per second of the movie produced, naturally
  #'the fps is 60
  #'
  #'@param Global.nmax
  #'The maximum level of the grid and number of entries in the list.
  #'It is naturally set to 7 as lower values reduce the resolution and values
  #'above 8 are not worth the increased computational time for the improved resolution.
  #'i.e) if n.max = 4, the most refined grid will be (3^4 = 81) an 81x81x81 array.
  #'
  #'@param calc.noise
  #' A boolean value determining if numerical noise should be calculated in
  #'determining the curl of the velocity field.
  #'It is naturally set to FALSE to reduce computational time when generating
  #'movies as the noise level is not used when generating movies.
  #'
  #'@param H0
  #'H0 Hubble constant of the simulation in units of km/s/Mpc
  #'
  #'@param OmegaM
  #'OmegaM matter density of the universe at z=0
  #'
  #'@param OmegaL
  #'dark energy density of the universe at z=0
  #'
  #'@param velocity.conversion
  #'conversion factor from velocity units in the simulation to length units/Gyr
  #'(at z=0). The default corresponds to the standard of Gadget-2.
  #'
  #'@param keep.frames
  #'Boolean value determining whether the frames used to save the movie are to
  #'be stored within the temporary foler used to create the movie.
  #'
  #'@param rotation
  #'either an integer (1-6) or a 3-vector, specifying a rotation of the 3D
  #'particle positions. In case of an integer: 1=(x,y)-plane, 2=(y,z)-plane,
  #'3=(x,y)-plane, 4=(qmax,qmin)-plane, 5=(qmax,qmid)-plane,
  #'6=(qmid,qmin)-plane, where qmax/qmid/qmin are the eigenvectors of the
  #' particle-quadrupole, associated with the maximum/middle/minimum
  #' eigenvalues, respectively. If a vector,
  #' its direction specifies the rotation axis and its norm the rotation angle
  #' in radians in the positive geometric sense.
  #'
  #'@param scale
  #'A boolean value, which determines if the frames shown in the movie
  #'are co-moving or physical. If scale is True then the frames shows an image of
  #'radius / scalefactor else if False then the frames show an image of radius.
  #'Naturally it is True.
  #'
  #'@param dt
  #'A value in Gyrs determining the size of the timestep between each frame, how much
  #'the look-back time advances per frame. Naturally set to 0.02Gyrs
  #'
  #'@param f
  #'A vector containing the mass fraction of particles within
  #'the simulation in simulation units (solar_Mass / h). Naturally set to the
  #'mass fraction in the SURFS simulation L210N1024NR (1.17e8, 6.29e8), {Baryon, Dark Matter}
  #'
  #'@param png.size
  #'An optional value that determines the size of the png files produced and
  #'therefore the size of the mp4 files resolution.
  #'
  #'@param specify.frame
  #' An optional value, a vector specifiying which frames are to be used in creating the movie
  #'
  #'@param col
  #'An optional value.
  #'The colour palette that will be used to plot the enstrophy.
  #'
  #'@param show.R200
  #'A Boolean value which if true will overlay a ring showing R200 at Z=0
  #'
  #'!!! A WORK IN PROGESS !!!
  #'This parameter is not 100% finished and not working in at a suitable level yet!
  #'
  #'@examples
  #'
  #'halo = read.halo(hdf5.file = '/Users/..../test_halo.h5')
  #'modified.surfsmovie.enst(halo, mp4file = 'test_surfs_movie.mp4', radius = R200.calc(), scale = T)
  #'
  #'@export
  #'



  # load track
  track = halo

  # make snapshot dataframe
  snapshot_min = track$tracking$snapshot_min
  snapshot_max = track$tracking$snapshot_max
  snapshots = data.frame(index = seq(snapshot_min, snapshot_max))

  snstr = function(sn) sprintf('snapshot_%d',sn)

  for (i in seq_along(snapshots$index)) {
    snapshots$scalefactor[i] = track$particles[[snstr(snapshots$index[i])]]$scalefactor
  }


  snapshots$z = 1/snapshots$scalefactor-1
  snapshots$t = celestial::cosdistTravelTime(z=snapshots$z, OmegaM = OmegaM, OmegaL = OmegaL, H0 = H0) # [Gyr] look-back time

  #for now ignore and give simple value
  # vfactor = 1/sqrt(scalefactor)
  snapshots$vfactor = velocity.conversion/sqrt(snapshots$scalefactor) # sqrt needed because of Gadget's internal velocity convention in cosmological runs

  `%+=%` = function(x,y) eval.parent(substitute(x <- x + y))

  wrapped = 0
  for(j in seq_along(snapshots$index)){
    frame = track$particles[[snstr(snapshots$index[j])]]
    for(i in 1:3){
      if((max(frame[[i]]) - min(frame[[i]])) > 105){
        
        half.range = rep(105, length(frame[[i]]))
        frame[[i]] = mod(frame[[i]] + half.range, 210)

        track$particles[[snstr(snapshots$index[j])]][[i]] = frame[[i]]

        wrapped %+=% 1
      }
    }
  }

  cat(sprintf('%s snapshots unwrapped \n', wrapped))

  # make temporary directory for frames
  dir = sprintf('%sframes_enst_halo_%d/',paths()$temporary,track$halo$id)
  call = sprintf('rm -rf %s; mkdir %s',dir,dir)
  system(call)

  # determine scale
  if (is.null(radius)) {
    str = snstr(snapshot_max)
    x = cbind(track$particles[[str]]$rx,track$particles[[str]]$ry,track$particles[[str]]$rz)
    x0 = apply(x,2,mean) # geometric centre
    radius = sqrt(max(apply(t(t(x)-x0)^2,1,sum)))
  }
  width = 2*radius*sqrt(aspect)


  # determine scale & rotation
  if (length(rotation)==3) {
    rot = t(cooltools::rotation3(rotation))
  } else {
    if (length(rotation)!=1) stop('rotation must be an integer 1,...,6 or a real 3-vector.')
    if (rotation>3) {
      e = eigen(cooltools::quadrupole(x))$vectors
    }
    if (rotation==1) {
      rot = diag(3)
    } else if (rotation==2) {
      rot = rbind(c(0,0,1),c(1,0,0),c(0,1,0))
    } else if (rotation==3) {
      rot = rbind(c(0,1,0),c(0,0,1),c(1,0,0))
    } else if (rotation==4) {
      rot = e[,c(1,3,2)]
    } else if (rotation==5) {
      rot = e[,c(1,2,3)]
    } else if (rotation==6) {
      rot = e[,c(2,3,1)]
    } else {
      stop('rotation must be an integer 1,...,6 or a real 3-vector.')
    }
  }

  #calculate scalefactors
  t.plot = rev(seq(min(snapshots$t),max(snapshots$t),dt))

  cosmo = cosmofct(0, 10, H0 = 70, OmegaM = 0.3, OmegaL = 0.7)
  sf = 1 / ( cosmo$t2z(t.plot) + 1 )
  lbt.range = range(t.plot)
  cat(sprintf('interpolated look-back time range: (%f, %f) [Gyrs]\n', lbt.range[2], lbt.range[1]))

  # produce frames
  if(!is.null(specify.frame)){loop = specify.frame}else{loop = seq_along(t.plot)}
  for (frame in loop){

    # progress update
    cat(sprintf('Making frame: %d/%d\n',frame,length(t.plot)))

    Global.L <<- 3 * R200.calc() / sf[frame]

    str = snstr(snapshots$index[frame])

    #interpolate/extrapolate particle positions where needed.
    hold = interpolate.positions.2(track, snapshots, t.plot[frame])
    x = hold[[1]]
    v = hold[[2]]

    np = track$halo$n_particles
    ID.table <<- generate.particle.id.table.4(x,v, species=1)

    #divide populations for images and  set up data structures for calculating enstrophy
    Population <<- generate.empty.data()
    Storage <<- generate.empty.data()
    population.levels()
    Population[[1]] = length(ID.table$RX)

    #calculate enstrophy
    ntot = subdivide(noise=calc.noise)


    # remove all NA values saved if they exist.
    for(i in 1:Global.nmax){
      Storage[[i]][which(is.na(Storage[[i]]))] = 0
      if(calc.noise){Errors[[i]][which(is.na(Errors[[i]]))] = 0}
    }

    l=0
    for(n in  1:Global.nmax){
      if(sum(Storage[[n]] > 0) > 0){l=n}
    }
    cat(sprintf('frame_%d subdivision routine complete; numb points of enstrophy saved: %d \n', frame, ntot))
    if(l == Global.nmax){print('n >= max');l=Global.nmax}else{l=l+1}



    # save frame
    cat(sprintf('Generating enstrophy image for frame_%0.6d\n',frame))
    place.png = sprintf('%sframe_%0.6d.png',dir,frame)

    png(file = place.png, width = png.size[1], height = png.size[2]) # name and location combined, size 300 matches the surfsmovie
    par(mar = c(0, 0, 0, 0))

    if(t.plot[frame] == t.plot[length(t.plot)]){f.255 = T}else{f.255 = F}

    plot_enst(max.layer=7,smoothing=1/3,gamma=0.01, scale = sf[frame], final=f.255,  col.palette = col) # create raster image of the layered enstrophy


    dev.off() # Close the pdf file

    if(show.R200){

      #add ring showing R200 on the frame for the given lookback time,
      # W.I.P!. Not how I would like it atm, needs further work to be useful

      fn = sprintf('%sframe_%0.6d.png',dir,frame)


      rgb2 = image_read(fn)
      fig = image_graph(res = 96)

      call = sprintf('rm -rf %s',fn)
      system(call)

      ggplot2::qplot(mpg, wt, data = mtcars, colour = cyl)
      dev.off()

      # Combine
      out = image_composite(fig, rgb2, offset = "+70+30")
      #dev.off()

      place.png = sprintf('%sframe_%0.6d.png',dir,frame)
      png(file = place.png, width = png.size[1], height = png.size[2]) # name and location combined, size 300 matches the surfsmovie
      par(mar = c(0, 0, 0, 0))

      img = image_draw(rgb2)

      draw.circle(x=150, y=150, radius = 100/sf[frame] , border = 'orange') # diameter of the frame = 3*R200

      magick::image_write(img, path = place.png)

      dev.off()

      clear.dev.list()

    }



  }

  # convert frames into movie
  cat(sprintf('Write mp4 movie file %s\n',mp4file))
  linuxspaces = function(txt) gsub(' ','\\\\ ',txt)
  call = sprintf('rm -rf %s',mp4file)
  system(call)


  call = sprintf('ffmpeg -r %d -f image2 -s %dx%d -i %sframe_%%06d.png -vcodec libx264 -crf 18 -pix_fmt yuv420p %s -loglevel quiet',
                 fps,png.size[1],png.size[2],linuxspaces(dir),linuxspaces(mp4file))
  system(call)

  # delete frames
  if (!keep.frames) {
    cat(sprintf('deleting frames from $s', dir))
    call = sprintf('rm -rf %s',dir)
    system(call)
  }




}
