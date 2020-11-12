#' Kaya Identity
#'
#' This function calculates the total CO2 emmissions from human sources according
#' to the kaya identity.
#' See \url{https://en.wikipedia.org/wiki/Kaya_identity} for more details.
#'
#' @param pop Population size (in millions).
#' @param gdp GDP per capita (in 1000$/person).
#' @param enInt Energy Intensity (in Gigajoule/$1000GDP).
#' @param carbInt Carbon Intensity (in tonnes CO2/Gigajoule).
#' @return Total CO2 emmissions from human sources according to the kaya identity.
#' @examples
#' # Kaya Identity for Germany
#' kaya(82.4, 44, 5, 0.05)
kaya <- function(pop, gdp, enInt, carbInt) {
  pop * gdp * enInt * carbInt
}
