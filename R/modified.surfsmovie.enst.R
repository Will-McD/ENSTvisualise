
modified.surfsmovie.enst = function(track, mesh.width = GLobal.L,
                                    mp4file, fps = 60,
                                    calc.noise = F,
                                    H0 = 70, OmegaM = 0.3, OmegaL = 0.7,
                                    velocity.conversion = 0.00102269032*(H0/100), # [-] (velocity unit)/(length unit/Gyr) at z=0
                                    keep.frames = TRUE,
                                    rotation = 1,
                                    scale = F,
                                    dt = 0.05, # [Gyrs]
                                    f = c(1.17e8, 6.29e8), # (Baryon, Dark Matter) [Solar mass /h]
                                    png.size = c(300,300),
                                    specify.frame = NULL,
                                    col = NULL,
                                    show.R200 = F,
                                    dynam.plot =F,
                                    bright.scale = c(5e2, 1.5, 0.175),
                                    ...) {


  #' Generates a movie of density weighted enstrophy.
  #'
  #' @importFrom celestial cosdistTravelTime
  #' @importFrom png writePNG
  #' @importFrom magick image_read image_annotate
  #' @importFrom simstar sphview paths
  #' @importFrom cooltools nplot rasterflip lim griddata2 kde2 quadrupole rotation3 cosmofct
  #' @importFrom grDevices pdf dev.off col2rgb dev.list
  #' @importFrom graphics axis lines par rasterImage rect text
  #' @importFrom ggplot2 qplot
  #'
  #'@description
  #'This is a modified version of the surfsmovie function available in the Simstar package.
  #'https://github.com/obreschkow/simstar
  #'
  #'creates an .mp4 file from the provided halo list given showing the evolution
  #' of enstrophy within a halo, the enstrophy is dervived from the curl within
  #' particles identified to be within the halo at redshift z=0.
  #'
  #'The aspect ratio of the mp4 file movie is based on the adaptive mesh which
  #'is square, therefore the mp4 file has an aspect ratio of 1
  #'
  #'
  #'
  #'
  #'@param track
  #'A list containing the halo information from a given hdf5 file from surfsuite,
  #'essentially the halo's hdf5 file read in with read.halo
  #'
  #'@param mesh.width
  #'A value of the width of the adaptive mesh used to calculate
  #'enstrophy.Naturally set to the Global.L value.
  #'
  #'
  #'@param mp4file
  #'The file name of the .mp4 file which will be produced.
  #'
  #'include the .mp4 suffix.
  #'
  #'@param fps
  #'An optional value for the frames per second of the movie produced, naturally
  #'the fps is 60
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
  #'are co-moving or physical. If scale is FALSE then the frames shows an image of
  #'radius / scalefactor else if TRUE then the frames show an image of radius R200
  #' at redshift = 0
  #'Naturally it is False.
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
  #'    !!!! A WORK IN PROGESS !!!!
  #'This parameter is not 100% finished and not working in at a suitable level yet!
  #'
  #'@param dynam.plot
  #'A boolean value to determine if the plots size and smoothing should be
  #'adjusted for the levels of the adaptive mesh occupied.
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
  #'@param bright.scale
  #'
  #'a vector containing 3 numeric values, that alter the brightness scale used in creating the png images
  #'bright.scale = c(alpha, beta, gamma)
  #'
  #'where the brightness scale is given as
  #'img = log10(alpha x img + beta) x gamma
  #'
  #'
  #'@examples
  #'
  #'halo = read.halo(hdf5.file = '/Users/..../test_halo')
  #'modified.surfsmovie.enst(halo, mp4file = 'test_surfs_movie.mp4', mesh.width = 3*R200.calc())
  #'
  #'
  #'A movie where if in full it would be 10s at 60 fps, therefore 600 frames.
  #'However to have the movie only be on the final second of this whole movie:
  #'modified.surfsmovie.enst(halo, mp4file = 'test_surfs_movie.mp4', specify.frame = seq(499, 600))
  #'
  #'@export
  #'



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
  dir = sprintf('%sframes_enst_halo_%d/',simstar::paths()$temporary, track$halo$id)
  call = sprintf('rm -rf %s; mkdir %s',dir,dir)
  system(call)


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

  cosmo = cooltools::cosmofct(0, 10, H0 = 70, OmegaM = 0.3, OmegaL = 0.7)
  sf = 1 / ( cosmo$t2z(t.plot) + 1 )
  lbt.range = range(t.plot)
  cat(sprintf('interpolated look-back time range: (%f, %f) [Gyrs]\n', lbt.range[2], lbt.range[1]))

  # produce frames
  if(!is.null(specify.frame)){loop = specify.frame}else{loop = seq_along(t.plot)}
  for (frame in loop){

    # progress update
    cat(sprintf('Making frame: %d/%d\n',frame,length(t.plot)))

    Grid.L = mesh.width / sf[frame]

    #print(c(Global.L, sf[frame]))

    str = snstr(snapshots$index[frame])

    #interpolate/extrapolate particle positions where needed.
    hold = interpolate.positions.2(track, snapshots, t.plot[frame])
    x = hold[[1]]
    v = hold[[2]]

    #np = track$halo$n_particles
    ID.table <<- generate.particle.id.table.4(x,v, select.species=1, species = track$particle$species, p.mass=f)

    #divide populations for images and  set up data structures for calculating enstrophy
    if(!calc.noise){Population <<- Storage <<- generate.empty.data()}else{Population <<- Storage <<-Errors <<- generate.empty.data()}

    #population.levels()  #DOUBLE CHECK OVER THIS POINT AND SEE IF THERE IS A DIFFERENCE
    population.levels(Grid.L=Grid.L)
    Population[[1]] = length(ID.table$RX)

    #sanity check
    if(sum(Population[[2]]) != sum(Population[[3]] )){stop("LOST PARTICLES BETWEEN MESH LAYERS WHEN CALCULATING DENSITY")}


    #calculate enstrophy
    ntot = subdivide(noise=calc.noise, Grid.L = Grid.L)


    # remove all NA values saved if they exist, A SAFTEY NET
    for(i in 1:Global.nmax){
      Storage[[i]][which(is.na(Storage[[i]]))] = 0
      if(calc.noise){Errors[[i]][which(is.na(Errors[[i]]))] = 0}
    }


    cat(sprintf('frame_%d subdivision routine complete; numb points of enstrophy saved: %d \n', frame, ntot))
    #if(l == Global.nmax){print('n >= max');l=Global.nmax}else{l=l+1}

    max.occupied= function(){
      for(n in Global.nmax:1){
        if(any(Storage[[n]] >0)){return(n)}
      }
    }

    l = Global.nmax
    if(dynam.plot){
      l = max.occupied()
      if(l < Global.nmax){l %+=% 1}
      }

    # save frame
    cat(sprintf('Generating enstrophy image for frame_%0.6d\n',frame))
    place.png = sprintf('%sframe_%0.6d.png',dir,frame)

    png(file = place.png, width = png.size[1], height = png.size[2]) # name and location combined, size 300 matches the surfsmovie
    par(mar = c(0, 0, 0, 0))

    if(t.plot[frame] == t.plot[length(t.plot)]){f.255 = T}else{f.255 = F}
    if(scale){s = sf[frame]}else{s = NULL}


    #plot_enst(max.layer=7,smoothing=1/3,gamma=0.01, scale = s, final=f.255,  col.palette = col) # create raster image of the layered enstrophy
    plot_enst(max.layer=l,smoothing=1/3, scale = s, final=f.255,  col.palette = col, alpha =  bright.scale[1], beta = bright.scale[2], gamma=bright.scale[3]) # create raster image of the layered enstrophy


    grDevices::dev.off() # Close the pdf file

    if(show.R200){

      #add ring showing R200 on the frame for the given lookback time,
      # W.I.P!. Not how I would like it atm, needs further work to be useful

      fn = sprintf('%sframe_%0.6d.png',dir,frame)


      rgb2 = image_read(fn)
      fig = image_graph(res = 96)

      call = sprintf('rm -rf %s',fn)
      system(call)

      ggplot2::qplot(mpg, wt, data = mtcars, colour = cyl)
      grDevices::dev.off()

      # Combine
      out = image_composite(fig, rgb2, offset = "+70+30")
      #dev.off()

      place.png = sprintf('%sframe_%0.6d.png',dir,frame)
      png(file = place.png, width = png.size[1], height = png.size[2]) # name and location combined, size 300 matches the surfsmovie
      par(mar = c(0, 0, 0, 0))

      img = image_draw(rgb2)

      draw.circle(x=150, y=150, radius = 100/sf[frame] , border = 'orange') # diameter of the frame = 3*R200

      magick::image_write(img, path = place.png)

      grDevices::dev.off()

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


