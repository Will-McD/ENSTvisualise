# ENSTvisualise
Tools for the Visualisation of Turbulence within an SPH simulation

Enstrophy can be numerically estimated from the SPH simulations and be used as a viable substitute solenoidal turbulence wihtin a galaxy cluster. 
This code uses numerical enstimations of enstrophy to visualise the turbulence within a Halo from the SURFS simulations.

This repository is largley dependant on code from the Simstar and Surfssuite repositories and packages found at: 
https://github.com/obreschkow/simstar & https://github.com/obreschkow/surfsuite.

Installation of ffmpeg is required for the compiling of the mp4 files to fully visualise the data. 

The functions within this repository used to create the mp4 files rely on defining two global variables and the updating of two global lists. Therefore if not using the do_it_all function define the global intigers Global.L and Global.nmax prior to using other functions. 
Usually defined as:  Global.L = 3*R_200 & Global.nmax = 7

A lower Global.nmax value will reduce the computational time, but also reduce the resolution of the images as it outlines the maximum layer of the adaptive mesh allowed. 

Computational time will vary from halo to halo depending on the number of particles within the halo and the density of the halo, a more dense halo will increase the computational time as there will be a greater number of points of enstrophy calculated. Therefore there will be a larger vector used to store the enstrophy, density and noise. This also implies that there will be more points to smooth and compile when plotting the density weighted enstrophy. 

