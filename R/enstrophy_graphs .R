




enstrophy_graphs = function(halo.vec, 
                            halo.directory, 
                            graph.directory, 
                            g.ind = 3,
                            n.max = 6,
                            snapshot.nums = seq(70,199)
                            ){
  #'
  #'Produces graphs from a hdf5 file containing the a halo from SURFS
  #'
  #' @importFrom magicaxis magplot
  #' @importFrom data.table data.table setnames
  #' @importFrom grDevices pdf dev.off
  #' @importFrom graphics points legend
  #'
  #' @description Generates graphs of a halo(s) for their mean density weighted 
  #' enstrophy against look back time of the simulation and or the signal to 
  #' noise ratio of the enstrophy.
  #' 
  #' Multiple halos can be queued and all saved to the same directory. 
  #'
  #'
  #'@param halo.vec
  #'A vector contianing the names of the halo hdf5 files for which graphs will 
  #'be produced
  #'
  #'@param halo.directory 
  #'A string with the location of the directory containing the halos to be retrieved
  #'
  #'@param graph.directory 
  #'A string containing the path to the directory where the graphs will be saved
  #'
  #'@param g.ind
  #'The graph index, a numerical value of 1, 2 or 3 indicating which graph(s) to
  #' be produced. 
  #'
  #'       1: only make graphs of enstrophy against time 
  #'       
  #'       2: only makes graphs of signal to noise ratio
  #'       
  #'       3: make both graphs 
  #'       
  #'@param n.max
  #'The maxium layer of the adaptive mesh to be used, sets the Gobal.nmax
  #'
  #'
  #'@param snapshot.nums
  #'A vector containing all the snapshots from the hdf5 file which are to be 
  #'used in making the graphs.
  #'
  #'
  #'
  #'@examples
  #'
  #'make both enstrophy against time and signal to noise for halos 1,2 and 3
  #'halo.vec = c("halo_1", "halo_2", "halo_3")
  #'halo.directory = "/where/halos/are/saved"
  #'graphs.directory = "/where/graphs/will/be/saved"
  #'
  #'enstrophy_graphs(halo.vec, halo.directory, graphs.directory, g.ind = 3)
  #'
  #'
  #'@export
  #'
  
  generate.data = function(Grid.L, g.ind, halo.name = NULL, n.max = 7, snapshot=199){
    
    frame = halo$particles[[snstr(snapshot)]]
    
    #unwrap the halo
    for(i in 1:3){
      if((max(frame[[i]]) - min(frame[[i]])) > 105){
        
        half.range = rep(105, length(frame[[i]]))
        frame[[i]] = mod(frame[[i]] + half.range, 210)
        
      }
    }
    
    
    ID.table <<- generate.particle.id.table.4(x=cbind(frame$rx,frame$ry,frame$rz),
                                              v=cbind(frame$vx,frame$vy,frame$vz),
                                              select.species=1, 
                                              species = halo$particles$species
                                              )
    
    #divide populations for images and  set up data structures for calculating enstrophy
    cat(sprintf('Generating empty data structures and calculating density.\n'))
    Population <<- Storage <<- Errors <<- generate.empty.data()
    
    population.levels(Grid.L = Grid.L)
    Population[[1]] = sum(halo$particles$species==1)
    
    
    cat(sprintf('Applying adaptive mesh for enstrophy and error. \n'))
    n = TRUE
    if(g.ind !=1){n = FALSE}
    ntot = subdivide(Grid.L = Grid.L, noise = n)
    
    
  }
  
  add.data= function(x, y, g.ind){
    noise = c(NA)
    enst = c(NA)
    cat(sprintf("weighting by density \n"))
    #plot.data = c('id','bi' ,'np', 1, 2, 3, 4, 5, 6, 7)
    for(j in seq(2, Global.nmax)){
      
      enst[j] = mean(density_weighted_enst(Storage, j)[density_weighted_enst(Storage, j)>0])
      noise[j] = mean(density_weighted_enst(Errors, j)[density_weighted_enst(Storage, j)>0])
      
    }
    
    enst[is.nan(enst)] = NA
    enst[enst = 0] = NA
    
    noise[is.nan(noise)] = NA
    noise[noise = 0] = NA
    
    x = rbind(x,rbind(c(halo$halo$id, NA, halo$halo$n_particles, enst)))
    
    if(g.ind!=1){ y = rbind(y,rbind(c(halo$halo$id, NA, halo$halo$n_particles, noise)))}
   
    return(list('enst' = x, 'noise'=y))
    
  }
  
  for(name in halo.vec){
    cat(sprintf("Beginning %s \n", name))
    
    #read in halo
    
    halo <<- read.halo(path =sprintf("%s/",halo.directory), halo.file = name )
    
    #set up all global parameters and make sure they are clear for the next run through
    ID.table = NA
    Storage = NA
    Population = NA
    
    Global.L <<- 3 * R200.calc()
    Global.nmax <<- n.max
    
    n.p = sum(halo$particles$species==1)
    
    if(n.p >= 250000){Global.nmax <<-7;cat(sprintf("moving to a Global.nmax of 6 due to particle count \n"))}
    

    #create empty data.tables to fill
    x2 = y2 = data.table::data.table()
    
    #iterate over all snapshots 
    for(i in snapshot.nums){
      cat(sprintf("SNAPSHOT %d / $d \n", i-snapshot.nums[1]-1), diff(range(snapshot.nums)))
      
      Grid.L = Global.L / halo$particles[[snstr(i)]]$scalefactor

      generate.data(Grid.L, g.ind, halo.name = name, snapshot = i, n.max = Global.nmaxs)
      
      hold = add.data(x2, y2, g.ind)
      x2 = hold[[1]]
      y2 = hold[[2]]

    }
    
    ## name data.table columns  
    old = c('V1', 'V2', "V3")
    new = c('id','bi' ,'np')
    for (i in 1:Global.nmax) {
      vcol = sprintf('V%01d', i)
      nlayer = sprintf('n%01d', i)
      old[i + 3] = vcol
      new[i + 3] = nlayer
    }
    data.table::setnames(x2, old, new)
    data.table::setnames(y2, old, new)
    
    #get Look-Back Time
    scalefactor = c()
    for(i in snapshot.nums){
      scalefactor[(i-snapshot.nums[1]-1)] = halo$particles[[snstr(i)]]$scalefactor
    }
    lbt = celestial::cosdistTravelTime(z=(1/scalefactor - 1),  H0 = 70, OmegaM = 0.3, OmegaL = 0.7)
    

    colour = grDevices::rainbow(Global.nmax)
    
    if(g.ind == 1 | g.ind ==3){
      
      #open pdf
      path = sprintf("%s/Enst_lbt_Halo_%d.pdf", graph.directory, halo$halo$id)
      grDevices::pdf(path)
      
      #plot template
      magicaxis::magplot(NA, xlim = c(13, 0), ylim = c(1e2, 1e10), 
              xlab = 'Look-Back Time [Gyrs]', 
              ylab = expression("<" ~ hat(epsilon) ~ ">" %.% tau[0]^2),
              log='y')
      
      
      for(i in 1:Global.nmax){
        graphics::points(lbt, x2[[sprintf('n%d',i)]],  pch=20, type = 'p', col=colour[i])
      }
      
      graphics::legend('top', col=colour, legend = new[-c(1,2,3)], horiz = T, lty=1, cex=0.5, bg='transparent')
      
      # commit plot and close pdf file
      grDevices::dev.off()
      
    }
    
    if(g.ind ==2 | g.ind ==3){
      
      #open pdf
      path = sprintf("%s/Noise_2_Signal_Halo_%d.pdf", graph.directory , halo$halo$id)
      grDevices::pdf(path)
      
      magicaxis::magplot(NA, xlim = c(1, 1e10), ylim = c(1, 1e10),
              xlab = expression("<" ~ hat(sigma) ~ ">" %.% tau[0]^2), 
              ylab = expression("<" ~ hat(epsilon) ~ ">" %.% tau[0]^2),
              log='xy')
      
      for(i in 1:Global.nmax){
        graphics::points(y2[[sprintf('n%d',i)]]/1e6, x2[[sprintf('n%d',i)]],  pch=20, type = 'p', col=colour[i])
      }
      
      graphics::legend('top', col=colour, legend = new[-c(1,2,3)], horiz = T, lty=1, cex=0.5, bg='transparent')
      
      # commit plot and close pdf file
      grDevices::dev.off()
      
      
    }
    

  }
  
}











