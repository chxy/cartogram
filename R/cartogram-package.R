##' Make a cartogram
##'
##' This package was designed mainly for creating cartograms with
##' different methods, given the geographic and density information.
##'
##' @name cartogram-package
##' @docType package
##'
NULL


##' Coordinates of the borders of 49 US states.
##'
##' The data is collected from package \pkg{map}.
##'
##' \describe{
##' \item{\code{x}}{A vector of the longitude.} 
##' \item{\code{y}}{A vector of the latitude.} 
##' \item{\code{state}}{A vector of the state names.}
##' \item{\code{abbr}}{Abbreviation of the states.}  
##' \item{\code{polygon}}{A vector of the polygon names.}
##' }
##' @name state
##' @docType data
##' @usage data(state)
##' @keywords datasets
##' @examples
##' head(state)
##'
NULL


##' Count of shared border of the 49 US states.
##'
##' A matrix of the number of shared border points.
##'
##' @name stateborder
##' @docType data
##' @usage data(stateborder)
##' @keywords datasets
##' @examples
##' stateborder[1:5,1:5]
##'
NULL


##' Information of 51 US states.
##'
##' The data includes the basic information of 50 US states and Washington D.C.
##'
##' \describe{
##' \item{\code{State}}{A vector of the state names.} 
##' \item{\code{Abbr}}{Abbrevation of the states.} 
##' \item{\code{Capital}}{Names of the state capitals.}
##' \item{\code{Latitude}}{Latitude of the capitals.}  
##' \item{\code{Longitude}}{Longitude of the capitals.}
##' \item{\code{TotalSqMi}}{The total area of the states in Square Miles.}
##' \item{\code{LandSqMi}}{Total land area of the states in Square Miles.}
##' \item{\code{WaterSqMi}}{Total water area of the states in Square Miles.}
##' \item{\code{centroidx}}{Longitude of the state centroids.}
##' \item{\code{centroidy}}{Latitude of the state centroids.}
##' }
##' @name usCapitals
##' @docType data
##' @usage data(usCapitals)
##' @keywords datasets
##' @examples
##' head(usCapitals)
##'
NULL


##' Neighbor states of the 49 US states.
##'
##' A list of the neighbors of each state.
##'
##' @name statenbrs
##' @docType data
##' @usage data(statenbrs)
##' @keywords datasets
##' @examples
##' statenbrs[1:5]
##'
NULL


##' Presidential election 2012.
##'
##' The data includes the result of presidential election in 2012.
##'
##' \describe{
##' \item{\code{state}}{A vector of the abbrevated state names.} 
##' \item{\code{electors}}{Number of electors in each state.} 
##' \item{\code{result}}{A character vector of red(republican) or blue(democratic).}
##' }
##' @name election2012
##' @docType data
##' @usage data(election2012)
##' @keywords datasets
##' @examples
##' head(election2012)
##'
NULL
