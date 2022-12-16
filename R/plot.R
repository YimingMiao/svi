#' Get the Spectral Signature of Accelerometry Data
#'
#' The spectral signature is calculated by taking the modulus of the
#' Fourier coefficients of the signal.
#' @param data data
#' @param index string
#' @return a plot
#' @importFrom dplyr rename
#' @importFrom usmap plot_usmap us_map
#' @importFrom ggplot2 scale_fill_continuous geom_polygon aes ggtitle theme element_text
#' @export
svi_map <- function(data, index) {
  if (!("fips" %in% colnames(data))) {
    stop("`data` must be a data.frame containing `fips` column.")
  }
  if (min(data[[index]], na.rm = TRUE) < 0 ||
             max(data[[index]], na.rm = TRUE) > 1) {
    stop("This function can only be used to plot SVI percentile ranging from 0 to 1.")
  }
  plot_usmap(data = data, values = index, linewidth = 0.08, color = "black") +
    scale_fill_continuous(limits = c(0, 1),
                          low = "#ffffdc", high = "#678ec2",
                          name = index) +
    geom_polygon(data = us_map(regions = "states"),
                 aes(x, y, group = group),
                 fill = NA, linewidth = 0.6, color = "black") +
    ggtitle(paste("Map of SVI -", index, "by County")) +
    theme(plot.title = element_text(color = "black",
                                    size = 16, face = "bold",
                                    hjust = 0.5, vjust = -1),
          legend.position = c(0.95, 0.3))
}


#' Get the Spectral Signature of Accelerometry Data
#'
#' The spectral signature is calculated by taking the modulus of the
#' Fourier coefficients of the signal.
#' @param data data
#' @param colname string of column name which represents the mortality in the data
#' @param level string, "County" or "State"
#' @param disease string of the disease name showing
#' @return a plot
#' @importFrom usmap plot_usmap us_map
#' @importFrom ggplot2 scale_fill_gradientn geom_polygon aes ggtitle theme element_text
#' @export
mortality_map <- function(data, colname, level="County", disease="Diabetes") {
  if (!("fips" %in% colnames(data))) {
    stop("`data` must be a data.frame containing `fips` column.")
  } else {
    plot_usmap(data = data, values = colname,
               linewidth = 0.08, color = "black") +
      # scale_fill_continuous(low = "#fff7ec",
      #                       high = "#b30000",
      #                       mid = "#fdd49e",
      #                       name = "deaths per 100,000 population") +
      scale_fill_gradientn(colours = c("#fff7ec", "#fdd49e", "#fcbd59", "#d72f1f", "#b30000")) +
      geom_polygon(data = us_map(regions = "states"),
                   aes(x, y, group = group),
                   fill = NA, linewidth = 0.6, color = "black") +
      ggtitle(paste(disease, "Mortality by", level)) +
      theme(plot.title = element_text(color = "black",
                                      size = 16, face = "bold",
                                      hjust = 0.5, vjust = -1),
            legend.position = c(0.95, 0.3))
  }
}



#' Get the Spectral Signature of Accelerometry Data
#'
#' The spectral signature is calculated by taking the modulus of the
#' Fourier coefficients of the signal.
#' @param data prepared data
#' @param index the svi want to plot in x-axis
#' @param disease string of disease name
#' @return a plot
#' @importFrom ggplot2  ggplot geom_point aes theme labs
#' @importFrom ggthemes theme_clean
#' @export
mortality_vs_svi_scatter <- function(data, index, disease="Diabetes") {
  if (!("MORTALITY" %in% colnames(data))) {
    stop("`data` must be a data.frame containing `MORTALITY` column.")
  }
  svi_cols = names(which(map_lgl(vulnerability, is.double)))
  svi_estimates = svi_cols[2: (length(svi_cols)-2)]
  if (!(index %in% svi_estimates)) {
    stop("`x` axis here should be of SVI estimates.")
  }
  col <- as.symbol(index)
  ggplot(data, aes(x = {{col}}, y = log(MORTALITY))) +
    geom_point(color = "#0d5194", na.rm = TRUE) +
    theme_clean() +
    labs(y = paste("Log(", disease, " Mortality)", sep = "")) +
    theme(axis.title = element_text(face = "bold"))
}
