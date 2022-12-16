
#' Get the Spectral Signature of Accelerometry Data
#'
#' The spectral signature is calculated by taking the modulus of the
#' Fourier coefficients of the signal.
#' @param data data
#' @param column column needs to be substituted
#' @param target target
mnnn_to_na <- function(data, column, target) {
  for (i in column) {
    data[data[[i]] == target, i] <- NA
  }
  data
}


#' Get the Spectral Signature of Accelerometry Data
#'
#' The spectral signature is calculated by taking the modulus of the
#' Fourier coefficients of the signal.
#' @param data data
#' @param reliable default is TRUE
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



#' Get the Spectral Signature of Accelerometry Data
#'
#' The spectral signature is calculated by taking the modulus of the
#' Fourier coefficients of the signal.
#' @param vulnerability original SVI data from CDC
#' @param mortality original mortality data from CDC
#' @param reliable if taking into account case<= 20 (Default is `TRUE`)
#' @return a data frame
#' @importFrom dplyr rename select left_join
#' @importFrom purrr map_lgl
#' @importFrom utils head
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


#' Get the Spectral Signature of Accelerometry Data
#'
#' The spectral signature is calculated by taking the modulus of the
#' Fourier coefficients of the signal.
#' @param data data
#' @importFrom dplyr mutate group_by summarise
#' @importFrom utils head
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
