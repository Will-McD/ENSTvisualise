
modified.surfsmovie = function(halo, select.species = NULL,radius = NULL, aspect = 1,
                               mp4file, fps = 60,
                               H0 = 70, OmegaM = 0.3, OmegaL = 0.7,
                               velocity.conversion = 0.00102269032*(H0/100), # [-] (velocity unit)/(length unit/Gyr) at z=0
                               keep.frames = TRUE,
                               rotation = 1,
                               show.time = TRUE,
                               text.size = 1,
                               scale = T,
                               dt = 0.05, # [Gyr]
                               f = c(1.17e8, 6.29e8), # (Baryon, Dark Matter)
                               png.size = NULL,
                               show.R200 = F,
                               specify.frame = NULL,
                               ...) {

  #'
  #' Generates a movie from a given halo hdf5 file from surfsuite.
  #'
  #' @importFrom celestial cosdistTravelTime
  #' @importFrom png writePNG
  #' @importFrom magick image_read image_annotate
  #' @import simstar
  #'
  #'
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
  #'@param select.species
  #'An optional value to identify if a species within the halo should be isolated for the movie.
  #'If NULL then all species will be included, otherwise a numerical value to represent which
  #' species to be isolated is used.
  #'
  #'    1 = Baryon
  #'    2 = Dark Matter
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
  #'@param show.time
  #'A boolean specifying whether the look-back time is displayed
  #'
  #'@param text.size
  #'A scaling factor for the text size used for the look-back time
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
  #'mass fraction in the SURFS simulation L210N1024NR (6.29e8, 1.17e8), {DM, Bar}
  #'
  #'@param png.size
  #'An optional value that determines the size of the png files produced and
  #'therefore the size of the mp4 files resolution.
  #'
  #'@param show.R200
  #'A boolean value which if true will overlay a ring showing R200 at Z=0
  #'
  #'@param specify.frame
  #' An optional value, a vector specifiying which frames are to be used in creating the movie
  #'
  #'@examples
  #'halo = read.halo(hdf5.file = '/Users/..../test_halo.h5')
  #'rad = R200.calc()
  #'modified.surfsmovie(halo, mp4file = 'test_surfs_movie.mp4', radius = rad, scale = T)
  #'
  #'@export
  #'


  # load track
  track = halo

  if(!is.null(select.species)){

    allow = which(halo$particles$species == select.species)

    track$particles$id = halo$particles$id[allow]
    track$particles$species = halo$particles$species[allow]

    for(snap in 2:131){

      track$particles[[snap]]$rx = halo$particles[[snap]]$rx[allow]
      track$particles[[snap]]$ry = halo$particles[[snap]]$ry[allow]
      track$particles[[snap]]$rz = halo$particles[[snap]]$rz[allow]

      track$particles[[snap]]$vx = halo$particles[[snap]]$vx[allow]
      track$particles[[snap]]$vy = halo$particles[[snap]]$vy[allow]
      track$particles[[snap]]$vz = halo$particles[[snap]]$vz[allow]

    }

  }

  # make snapshot dataframe
  snapshot_min = track$tracking$snapshot_min
  snapshot_max = track$tracking$snapshot_max
  snapshots = data.frame(index = seq(snapshot_min, snapshot_max))
  snstr = function(sn) sprintf('snapshot_%d',sn)
  for (i in seq_along(snapshots$index)) {
    snapshots$scalefactor[i] = track$particles[[snstr(snapshots$index[i])]]$scalefactor
  }
  #print(snapshots$scalefactor)
  #stop()
  snapshots$z = 1/snapshots$scalefactor-1
  snapshots$t = celestial::cosdistTravelTime(z=snapshots$z, OmegaM = OmegaM, OmegaL = OmegaL, H0 = H0) # [Gyr] look-back time

  snapshots$vfactor = velocity.conversion/sqrt(snapshots$scalefactor) # sqrt needed because of Gadget's internal velocity convention in cosmological runs

  # make temporary directory for frames
  dir = sprintf('%sframes_halo_%d/',paths()$temporary,track$halo$id)
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
  center = c(0,0,0)

  # determine scale
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




  t.plot = rev(seq(min(snapshots$t),max(snapshots$t),dt))
  #print(t.plot)
  cosmo = cosmofct(0, 10, H0 = 70, OmegaM = 0.3, OmegaL = 0.7)
  sf = 1 / ( cosmo$t2z(t.plot) + 1 )
  #print(sf)

  # produce frames
  if(!is.null(specify.frame)){loop = specify.frame}else{loop = seq_along(t.plot)}
  for (frame in loop) {

    # progress update
    cat(sprintf('Make frame %d/%d\n',frame,length(t.plot)))

    str = snstr(snapshots$index[frame])

    # make Center of Mass the center of the image
    m = (f/sum(f))[halo$particle$species] #* 7.47e8 * 1.9889200011446e30 / 0.7, #leaving in simulation units, using halo as all particle species are needed for this


    # interpolate positions
    if (scale) {
      str = snstr(snapshots$index[frame])

      # make Center of Mass the center of the image
      m = (f/sum(f))[halo$particle$species] #leaving in simulation units, using halo as all particle species are needed for this

      x = interpolate.positions(track, snapshots, t.plot[frame], dt)
      cm = c(sum(x[,1]*m),sum(x[,2]*m),sum(x[,3]*m))/sum(m) #using halo as all particle species are needed for this

      if(is.null(png.size)){
        rgb = sphview(x, track$particles$species, screen=FALSE, rotation=rot, weight = NULL, center=cm, width=width/sf[frame], ...)$rgb

      }else{
        rgb = sphview(x, track$particles$species, screen=FALSE, rotation=rot, weight = NULL, center=cm, width=width/sf[frame], ngrid=png.size, ...)$rgb
      }

    } else {

      str = snstr(snapshots$index[frame])
      x = interpolate.positions(track, snapshots, t.plot[frame])
      cm = c(sum(x[,1]*m),sum(x[,2]*m),sum(x[,3]*m))/sum(m)

      rgb = sphview(x, track$particles$species, screen=FALSE, rotation=rot, center=cm, width=width, ...)$rgb

    }

    # make frame


    # add text to frame
    if (show.time) {
      diagonal = sqrt(prod(dim(rgb)[1:2]))
      s = 0.03*diagonal*text.size
      rgb = magick::image_read(rgb)
      rgb = magick::image_annotate(rgb, sprintf('Lookback time = %.2f Gyr',t.plot[frame]),
                                   size = s, location = sprintf('%+d%+d',round(1.8*s),round(s)), color = 'white', font='sans', degrees=90)
      rgb = as.numeric(rgb[[1]])[,,1:3]
    }


    # save frame
    fn = sprintf('%sframe_%0.6d.png',dir,frame)
    writePNG(rasterflip(rgb),fn)


    if(show.R200){

      clear.dev.list = function(){
        cat(sprintf('closing %d device[s] \n', length(dev.list())))
        for (i in dev.list()[1]:dev.list()[length(dev.list())]) {
          dev.off()
        }
      }

      fn = sprintf('%sframe_%0.6d.png',dir,frame)

      rgb2 = image_read(fn)
      fig = image_graph(res = 96)

      call = sprintf('rm -rf %s',fn)
      system(call)

      ggplot2::qplot(mpg, wt, data = mtcars, colour = cyl)
      dev.off()

      # Combine
      out = image_composite(fig, rgb2, offset = "+70+30")

      place.png = sprintf('%sframe_%0.6d.png',dir,frame)
      png(file = place.png, width = 300, height = 300) # name and location combined, size 300 matches the surfsmovie
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
                 fps,dim(rgb)[1],dim(rgb)[2],linuxspaces(dir),linuxspaces(mp4file))
  system(call)

  # delete frames
  if (!keep.frames) {
    cat(sprintf('deleting frames from $s', dir))
    call = sprintf('rm -rf %s',dir)
    system(call)
  }

}
