#' Social Vulnerability Index per County
#'
#' Contains social vulnerability measures in 4 themes 15 factors: \cr
#' 1. Socioeconomic: below poverty, unemployed, income, no high school diploma \cr
#' 2. Household Composition/Disability: aged 65 or older, aged 17 or younger, older than age 5 with a disability, single-parent households \cr
#' 3. Minority Status/Language: minority, speaks English "less than well" \cr
#' 4. Housing Type/Transportation: multi-unit structures, mobile homes, crowding, no vehicle, group quarters
#'
#' @details
#' See
#' \href{https://svi.cdc.gov/Documents/Data/2018_SVI_Data/SVI2018Documentation.pdf}{CDC SVI 2018 Documentation}
#' for more information
#' \describe{
#'     \item{E_}{estimates}
#'     \item{EP_}{percentage of estimates}
#'     \item{EPL_}{percentile percentage of estimates}
#'     \item{M_}{margins of error for estimates}
#'     \item{SPL_}{sum of percentile percentage in the theme}
#'     \item{RPL_}{percentile ranking of the theme}
#' }
#'
#' @name vulnerability
#' @docType data
#' @keywords data
NULL




#' Diabetes Deaths per County from CDC
#'
#' `County` is name of county and abbreviation of state name; \cr
#' `County.Code` is the FIPS code for the county; \cr
#' `Deaths` is number of people died because of diabetes in this county; \cr
#' `Population` is the population of the county; \cr
#' `Crude.Rate` is number of people died because of diabetes per 100,000 people in the county;
#'
#' @name diabetes
#' @docType data
#' @keywords data
NULL
