% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/recruitment.R
\name{recruitment.cal}
\alias{recruitment.cal}
\title{Estimation of Recruitment and primary producer growth}
\usage{
recruitment.cal(ini.nc.file, out.nc.file, yoy.file, grp.file, prm.file,
  quiet = TRUE)
}
\arguments{
\item{ini.nc.file}{Character string with the path to the netcdf file to read
in. This netcdf file contains the initial conditions for the Atlantis model and
usually ends in \emph{.nc}.}

\item{out.nc.file}{Character string with the path to the netcdf file to read
in. This netcdf file contains a generic output from an Atlantis run usually
starting with \emph{output} and ending in \emph{.nc}.}

\item{yoy.file}{Character string with the path to Young of the Year (YOY) standard
output file. Usually the name for this file is : \emph{output[YOUR_MODEL]YOY.txt}
where [YOUR_MODEL] is the name of your Atlantis model.}

\item{grp.file}{Character string with the path to the Groups \emph{*.csv} file
(Atlantis input file).}

\item{prm.file}{Character string with the path to the biology parameter file
\emph{*.prm} (Atlantis input file).}

\item{quiet}{(Default = TRUE) this parameter helps during the process of
debugging.}
}
\value{
The outputs of this function are divided into 3 tabs:
\itemize{
        \item \bold{Recruits and YOY}: Shows the recruitment and YOY curves
from the Atlantis output for each functional group and provides the option to
test different parameter values to obtain new recruitment and YOY curves.
        \item \bold{Growth Zoo and PP's}: Allows the user to check the light
levels and biomass values for primary producers (PP's) and zooplankton (Zoo) by box and
layer. The user has the option to set those values as a proportion by box,
proportion by layer, or their logarithmic value.
    \item \bold{Help}: Shows information about the inputs, parameter values and
output. It also, provides an overview of the different options for the user.}
}
\description{
This function helps to estimate the recruitment for age structured functional
groups and the primary and secondary production.
\itemize{
    \item \bold{Recruits and young of the year (YOY)}: This option shows the parameter values and the
recruitment equation for the chosen functional groups (i.e. Ricker or
Beverton-holt). Based on the larval survival and the biomass at each reproductive
time step the function allows the user to re-calculate
    the recruitment based on a new set of parameters provided by the user.
     \item \bold{Primary production and light limitation}:
    This option provides a view of the
    primary production (phytoplankton, seagrass and macroalgae),  light and
    secondary production (zooplankton) by box and layer. This helps to  calibrate
    the growth of primary producers and the consumption of light.}
}
\author{
Demiurgo
}
