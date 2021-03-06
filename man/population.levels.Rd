% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/population.levels.R
\name{population.levels}
\alias{population.levels}
\title{Arrays of particle populations in cluster}
\usage{
population.levels(n.max = Global.nmax, Grid.L = Global.L)
}
\arguments{
\item{n.max}{an optional value, the maximum level of the grid and number of entries in the list.
It is naturlly set to the Global.nmax (A global constant value), which is
characteristically set to 6 or 7.
i.e) if n.max = 5, the most refined grid will be (3^5-1 = 81) an 81x81x81 array.}

\item{Grid.L}{The width or length of the grid}
}
\description{
Creates a list of arrays with distributions of particle populations based on
the particles positions in a cluster.
This is done at varying levels of grid refinement, progressively more cells
per gird as a function of 3^n.

n is the grid layer or level in the list.
}
