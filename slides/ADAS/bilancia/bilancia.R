# Utility functions for the weighing scale experiment


#' Calculate the tilt angle resulting from a given load
#' 
#' @param deltaF load difference
#' @param Ftot total load
#' @param l distance between plates (mm)
#' @param h vertical offset between the pivot point and the CoM of the unloaded movable part (mm)
#' @return the tilt angle in degrees
scale_angle <- function(F, deltaF, Fm=50, l=100, h=50) atan(l*deltaF/(2*h*(2*F+Fm+deltaF)))/pi*180

#' Measurement noise (sinusoidal fluctuation + random rerm)
#' 
#' @param t time in seconds (possibly a vector of times)
#' @param w amplitude of fluctuation
#' @param p period of fluctuation in seconds
#' @param sd standard deviation of the normal noise
#' @return an offset value in the range [-w, w]
noise <- function(t, w=1, p=3600, sd=0.4) rnorm(length(t), 0, sd) + w*sin(t/p*2*pi)

#' Elapsed time in seconds from midnight
#' 
#' @return number of seconds elapsed since midnight
elapsed <- function() as.numeric(difftime(Sys.time(),Sys.Date(), units = "secs"))