#' Kaya Identity
#'
#' This function calculates the total CO2 emissions from human sources according
#' to the kaya identity. \cr
#' The Kaya Identity can be computed as: co2 = pop * gdp * enInt * carbInt \cr
#' See \url{https://en.wikipedia.org/wiki/Kaya_identity} for more details.
#'
#' @param pop Population size (in millions).
#' @param gdp GDP per capita (in 1000$/person).
#' @param enInt Energy Intensity (in Gigajoule/$1000GDP).
#' @param carbInt Carbon Intensity (in tonnes CO2/Gigajoule).
#' @param output_type Output type can be either "CO2" or "C".
#' @return Total yearly emissions according to the kaya identity in CO2 or C.
#' @examples
#' # Kaya Identity for Germany
#' kaya(82.4, 44, 5, 0.05)
#' # Kaya Identity for Germany in C (Carbon)
#' kaya(82.4, 44, 5, 0.05, output_type = "C")
#' @export
kaya <- function(pop, gdp, enInt, carbInt, output_type = "CO2") {
  checkmate::assert_numeric(pop, lower = 0, len = 1, any.missing = FALSE)
  checkmate::assert_numeric(gdp, lower = 0, len = 1, any.missing = FALSE)
  checkmate::assert_numeric(enInt, lower = 0, len = 1, any.missing = FALSE)
  checkmate::assert_numeric(carbInt, lower = 0, len = 1, any.missing = FALSE)
  checkmate::assert_character(output_type, any.missing = FALSE)

  #stopifnot(output_type %in% c("CO2", "C"))
  if (!output_type %in% c("CO2", "C")) {
    stop("`output_type` must be 'CO2' or 'C'")
  }

  emissions <- pop * gdp * enInt * carbInt
  if (output_type == "C") {
    emissions <- emissions / 3.67
  }
  emissions
}
