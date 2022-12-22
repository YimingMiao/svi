## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----setup, message=FALSE-----------------------------------------------------
library(svi)
library(dplyr)
library(purrr)
library(ggplot2)

## ----fig.width=9,fig.height=4-------------------------------------------------
vulnerability |> mnnn_to_na(names(which(map_lgl(vulnerability, is.double))), -999) |>
                 rename(fips = FIPS) |>
                 svi_map("RPL_THEMES")

## ----fig.show="hold", out.width="50%"-----------------------------------------
temp <- vulnerability |>
        mnnn_to_na(names(which(map_lgl(vulnerability, is.double))), -999) |>
        rename(fips = FIPS)
svi_map(temp, "RPL_THEME1")
svi_map(temp, "RPL_THEME2")
svi_map(temp, "RPL_THEME3")
svi_map(temp, "RPL_THEME4")

## ----fig.show="hold", out.width="50%"-----------------------------------------
diabetes |> cr_interpolate(reliable = FALSE) |>
                                rename(fips = County.Code) |>
                                mortality_map("Crude.Rate")
diabetes |> mortality_by_state() |>
                               mortality_map("total_mortality", "State", "Diabetes")

## ----fig.show="hold", out.width="50%"-----------------------------------------
df <- prepare(vulnerability, diabetes, reliable = FALSE)
hist(df$MORTALITY, breaks = 9,
       main = "Histogram of Diabetes Mortality Rate",
       xlab = "Mortality Rate")

hist(log(df$MORTALITY), breaks = 9,
       main = "Histogram of Log-transformed Mortality Rate",
       xlab = "Log of Mortality Rate")

## ----fig.width=5,fig.height=3, fig.align = 'center'---------------------------
df |> mortality_vs_svi_scatter("RPL_THEMES", "Diabetes")

## ----fig.show="hold", out.width="25%"-----------------------------------------
df |> mortality_vs_svi_scatter("RPL_THEME1", "Diabetes")
df |> mortality_vs_svi_scatter("RPL_THEME2", "Diabetes")
df |> mortality_vs_svi_scatter("RPL_THEME3", "Diabetes")
df |> mortality_vs_svi_scatter("RPL_THEME4", "Diabetes")

## -----------------------------------------------------------------------------
model0 <- lm(log(MORTALITY) ~ RPL_THEMES, data = df)
summary(model0)

## -----------------------------------------------------------------------------
model1 <- lm(log(MORTALITY) ~ RPL_THEME1 + RPL_THEME2 + RPL_THEME3 + RPL_THEME4, data = df)
summary(model1)

