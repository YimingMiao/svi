
#' Change NA to Target Value
#'
#' This function changes NA in the selected data columns to the target value.
#' @param data original data.
#' @param column column needs to be substituted.
#' @param target target value.
#' @return a data frame with NA's of selected columns replaced with the target value.
#' @examples
#' data("diabetes")
#' diabetes |> mnnn_to_na("Crude.Rate", "Unreliable")
#' @export
mnnn_to_na <- function(data, column, target) {
  for (i in column) {
    data[data[[i]] == target, i] <- NA
  }
  data
}


#' Get the County-level Mortality
#'
#' The mortality is calculated by the number of deaths per 100,000 total
#' population. If reliable is TRUE, the mortality for counties with <= 20
#' deaths is marked as unavailable.
#' @param data original data. It is assumed to have
#' a `Deaths`, `Population`, and `Crude.Rate` column.
#' @param reliable if taking into account deaths <= 20. (Default is `TRUE`)
#' @return a data frame with county-level mortality.
#' @examples
#' data("diabetes")
#' diabetes |> cr_interpolate(reliable = TRUE)
#' @export
cr_interpolate <- function(data, reliable = TRUE) {
  should_have <- c("Deaths", "Population", "Crude.Rate")
  if (!(all(should_have %in% colnames(data)))) {
    stop(paste("`data` must contain",
               colnames(data)[!(colnames(data) %in% should_have)][1],
               "column."))
  }
  if (reliable == TRUE) {
    data <- mnnn_to_na(data, "Crude.Rate", "Unreliable")
  } else {
    data$Crude.Rate <- round(data$Deaths / data$Population * 100000, 1)
  }
  data$Crude.Rate <- as.numeric(data$Crude.Rate)
  return(data)
}



#' Prepare Data for Plot and Regression
#'
#' This function joins SVI data and mortality data from CDC.
#' @param vulnerability original SVI data from CDC.
#' @param mortality original mortality data from CDC.
#' @param reliable if taking into account deaths <= 20. (Default is `TRUE`)
#' @return a data frame joining SVI data and mortality data.
#' @importFrom dplyr rename select left_join
#' @importFrom purrr map_lgl
#' @importFrom utils head
#' @examples
#' data("vulnerability")
#' data("diabetes")
#' prepare(vulnerability, diabetes)
#' @export
prepare <- function(vulnerability, mortality, reliable = TRUE) {
  mortality <- mortality |>
               cr_interpolate(reliable = reliable) |>
               head(-1) |>
               rename(MORTALITY = Crude.Rate) |>
               select(County.Code, MORTALITY)
  vulnerability <- vulnerability |>
                   mnnn_to_na(names(which(map_lgl(vulnerability, is.double))), -999) |>
                   select(STATE, COUNTY, FIPS, RPL_THEMES, RPL_THEME1, RPL_THEME2, RPL_THEME3, RPL_THEME4)
  out <- left_join(vulnerability, mortality, by = c("FIPS" = "County.Code"))
  return(out)
}


#' Get the mortality by state from county-level data
#'
#' This function groups mortality by state based on county-level data.
#' @param data county-level data on mortality. It is assumed to have
#' a `County`, `County.Code`, `Deaths`, and `Population` column.
#' @return a data frame with state-level deaths, population, and mortality.
#' @importFrom dplyr mutate group_by summarise
#' @importFrom utils head
#' @examples
#' data("diabetes")
#' mortality_by_state(diabetes)
#' @export
mortality_by_state <- function(data) {
  should_have <- c("County", "County.Code", "Deaths", "Population")
  if (!(all(should_have %in% colnames(data)))) {
    stop(paste("`data` must contain",
                colnames(data)[!(colnames(data) %in% should_have)][1],
                "column."))
  }
  out <- data |>
         head(-1) |>
         mutate(state = toupper(substr(County, nchar(County) - 1, nchar(County))),
                fips = substr(County.Code, 1, 2)) |>
         group_by(state, fips) |>
         summarise(total_deaths = sum(Deaths),
                   total_population = sum(Population),
                   total_mortality = round(total_deaths / total_population * 100000, 1))
  return(out)
}
