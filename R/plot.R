#' Plot US SVI Map
#'
#' This function plots US SVI map for selected estimates.
#' @param data data. It is assumed to have a `fips` column.
#' @param index string of column name which represents the SVI estimates.
#' @return a US SVI Map.
#' @importFrom dplyr rename
#' @importFrom usmap plot_usmap us_map
#' @importFrom ggplot2 scale_fill_continuous geom_polygon aes ggtitle theme element_text
#' @examples
#' library(dplyr)
#' library(purrr)
#' data("vulnerability")
#' vulnerability |>
#'   mnnn_to_na(names(which(map_lgl(vulnerability, is.double))), -999) |>
#'   rename(fips = FIPS) |>
#'   svi_map("EPL_POV")
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


#' Plot US Mortality Map
#'
#' This function plots US mortality map for selected disease.
#' @param data data. It is assumed to have a `fips` column.
#' @param colname string of column name which represents the mortality in the data.
#' @param level string, "County" or "State". (Default is `County`)
#' @param disease string of the disease name showing. (Default is `Diabetes`)
#' @return a US mortality map.
#' @importFrom usmap plot_usmap us_map
#' @importFrom ggplot2 scale_fill_gradientn geom_polygon aes ggtitle theme element_text
#' @examples
#' library(dplyr)
#' data("diabetes")
#' diabetes |>
#'   cr_interpolate(reliable = FALSE) |>
#'   rename(fips = County.Code) |>
#'   mortality_map("Crude.Rate", "County", "Diabetes")
#' @export
mortality_map <- function(data, colname, level = "County", disease = "Diabetes") {
  if (!("fips" %in% colnames(data))) {
    stop("`data` must be a data.frame containing `fips` column.")
  } else {
    plot_usmap(data = data, values = colname,
               linewidth = 0.08, color = "black") +
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



#' Scatter Plot of Mortality vs. SVI Estimates
#'
#' This function shows the scatter plot of mortality versus the selected SVI estimates.
#' @param data prepared data. It is assumed to have a `MORTALITY` column.
#' @param index selected SVI estimates.
#' @param disease string of disease name. (Default is `Diabetes`)
#' @return a scatter plot.
#' @importFrom ggplot2  ggplot geom_point aes theme labs
#' @importFrom ggthemes theme_clean
#' @examples
#' data("vulnerability")
#' data("diabetes")
#' df <- prepare(vulnerability, diabetes, reliable = FALSE)
#' mortality_vs_svi_scatter(df, "RPL_THEME1", "Diabetes")
#' @export
mortality_vs_svi_scatter <- function(data, index, disease = "Diabetes") {
  if (!("MORTALITY" %in% colnames(data))) {
    stop("`data` must be a data.frame containing `MORTALITY` column.")
  }
  svi_cols <-  names(which(map_lgl(vulnerability, is.double)))
  svi_estimates <-  svi_cols[2 : (length(svi_cols) - 2)]
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
