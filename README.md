# ENSTvisualise
Tools for the Visualisation of Turbulence within an SPH simulation

Enstrophy can be numerically estimated from the SPH simulations and be used as a viable substitute solenoidal turbulence within a galaxy cluster. This code uses numerical estimations of enstrophy to visualise the turbulence within a Halo from the SURFS simulations.

This repository is largely dependant on code from the simstar and Surfsuite repositories and packages found at: https://github.com/obreschkow/simstar & https://github.com/obreschkow/surfsuite.

NON-R REQUIREMENTS:

Access to the SURFS simulation data for L210N1024NR or a similar hydrodynamic sph simulation with identified and seperated halos that can be compiled via surfsuite.
ffmpeg is required for the compiling of the mp4 files to fully visualise the data.
THE BASICS OF HOW TO CREATE MOVIES:

Have the halo in a hdf5 file format from surfsuite
Use either the do_it_all or create own script using the other functions within the repository.
WITH THE DO_IT_ALL FUNCTION:

This will create an individual movie for the particles and turbulence then create a new movie with both playing side by side.

Run the function, giving it an input halo file location and outputname.mp4. The other parameters can be adjusted as desired to alter the movie length, fps, frame resolution and smoothing.

WITHOUT THE DO_IT_ALL FUNCTION:

The functions within this repository used to create the mp4 files rely on defining two global variables and the updating of two-three global lists. Therefore if not using the do_it_all function define the global intigers Global.L and Global.nmax prior to using other functions. Usually defined as: Global.L = 3*R_200 & Global.nmax = 6 or 7

Global.L is the total side length of the adaptive mesh used, it is defined as three times the virial radius of the halo at redshift zero. This allows for the irregular shape or lack of ellipticity within some halos by capturing just outside of the virial radius. The virial radius is calculated in Mpc/h, which is the simulation unit used in SURFS.

Global.nmax is the maximum level or highest layer of the adaptive mesh used, therefore a higher value will result in a more refined mesh and an increased maximum resolution. However there is rarely a need to use a Global.nmax above 6, as a higher value is only required if it is going to be used, otherwise it increases computational time with no benefits. Higher levels of the mesh are used when the layer below it is fully occupied in a way that allows for the calculation of enstrophy, therefore more dense halos will occupy higher layers of the adaptive mesh than less dense halos. As an example a Global.nmax value of 5 will produce an adaptive mesh with the most refined grid being (3^(5-1) = 81) 81x81x81 cells. Therefore each cell has a side length of Global.L/81 at redshift = 0. Computational time can be reduced through a reduced Global.nmax, however this will lead to a reduced maximum resolution given the layers above the lower Global.nmax being used would be occupied.

(3^(5-1) is used as the Global lists which store each layer of data are indexed from 1, with 1 being the layer which has an empty grid or the layer that is the halo itself with no grid applied i.e) just the box of side length R200 )

(Global.nmax = 6 works for majority of cases, however with some large halos a Global.nmax of 7 or greater is needed. !!! There is a relatively large slowdown between 6 and 7 on [my machine atleast}] !!! )


COMPUTATIONAL TIME WARNING:
Computational time will vary from halo to halo depending on the number of particles within the halo and the density of the halo, a more dense and or massive  halo will increase the computational time.  This is because more memory will be required to store the larger number of enstrophy points calculated and the more points of enstrophy will need to be compiled to generate an image.

