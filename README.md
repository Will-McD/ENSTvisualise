# ENSTvisualise
Tools for the Visualisation of Turbulence within an SPH simulation
CURRENTLY ONLY EQUIPED TO RUN FOR SURFS L210N1024NR, MORE GENERALISED FORM IS CURRENTLY IN PROGRESS. 



######## BROKEN PACKAGE AND DEPENDS ON DEPRECATED PACKAGES #######
Currently the visualisation and interpolation scripts are not working due to being dependant on out dated packages. 
DO NOT USE THIS REPOSITORY OR CODE AS IT IS INCOMPLETE


This repository is largely dependant on code from the Surfsuite repository: https://github.com/obreschkow/surfsuite.


NON-R REQUIREMENTS:

1. Input simulation data must be in a HDF5 file that is formatted as given by https://github.com/obreschkow/surfsuite
2. ffmpeg is required for the compiling of the mp4 files to fully visualise the data. 


Computing Enstrophy:
The functions within this repository rely on defining two global variables and the updating of two<->three global lists. 

GLOBAL VARIBALES: 
To start define the global variables for the largest grid used (Globa.L) and the maximum number of recursive subdivisions (Global.nmax)
--> Global.L must be specified as either being a co-moving or physical length. 
--> Global.nmax defined the minimum sidelenght of a cell used, L_cell = Global.L /3^n . It should be defined with both the softening length and computation cost of further refining the grid in mind. 



Changes to the Global.nmax can also alter the brightness of the images produced as it will affect the level to which the patricle density is also calculated and the level of smoothing. If after adjusting the Global.nmax the brightness seems either too high or low, alter the brightness scale within the plot_enst function.



GLOBAL LISTS: 
Theses are R type lists, initally must be created as an empty object (use the function: generate.empty.data()) that is filled with the estimated enstrophy. 
example: Population <<- Storage <<- Errors <<- generate.empty.data()




