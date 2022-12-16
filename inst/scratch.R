library(purrr)
library(dplyr)

df <- prepare(vulnerability, diabetes, reliable = FALSE)

df |> rename(fips = FIPS) |>
      svi_map("RPL_THEME1")

vulnerability |> mnnn_to_na(names(which(map_lgl(vulnerability, is.double))), -999) |>
                 rename(fips = FIPS) |>
                 svi_map("EPL_POV")

df |> rename(fips = FIPS) |>
      mortality_map("Diabetes", "County", "MORTALITY")

diabetes |> interpolate_CR(reliable = FALSE) |>
            rename(fips = County.Code) |>
            mortality_map("Crude.Rate")

diabetes |> mortality_by_state() |>
            mortality_map("total_mortality", "State", "Diabetes")


df |> mortality_vs_svi_scatter("RPL_THEME1", "Diabetes")

diabetes |> mortality_vs_svi_scatter



