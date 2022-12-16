# svi_map() --------------------------------------------------------------------------

test_that(
  "The svi_map() returns a ggplot object.",
  {
    p <- vulnerability |>
         mnnn_to_na(names(which(map_lgl(vulnerability, is.double))), -999) |>
         rename(fips = FIPS) |>
         svi_map("EPL_POV")
    expect_true(inherits(p, "gg"))
  }
)

test_that(
  "The svi_map() errors when no `fips` column.",
  {
    data(iris)
    expect_error(svi_map(iris, "Sepal.Length"))
  }
)

test_that(
  "The svi_map() errors when index not in (0,1)",
  {
    temp <- vulnerability |>
            rename(fips = FIPS)
    expect_error(svi_map(temp, "EPL_POV"))
  }
)

test_that(
  "The svi_map() is correct for SVI data.",
  {
    p <- vulnerability |>
         mnnn_to_na(names(which(map_lgl(vulnerability, is.double))), -999) |>
         rename(fips = FIPS) |>
         svi_map("EPL_POV")
    vdiffr::expect_doppelganger("map-for-EPL_POL", p)
  }
)





# mortality_map() --------------------------------------------------------------------------

test_that(
  "The mortality_map() returns a ggplot object.",
  {
    p <- diabetes |>
         CR_interpolate(reliable = FALSE) |>
         rename(fips = County.Code) |>
         mortality_map("Crude.Rate", "County", "Diabetes")
    expect_true(inherits(p, "gg"))
  }
)

test_that(
  "The mortality_map() errors when no `fips` column.",
  {
    data(iris)
    expect_error(mortality_map(iris, "Sepal.Length"))
  }
)

test_that(
  "The mortality_map() is correct for diabetes data at county level.",
  {
    p <- diabetes |>
         CR_interpolate(reliable = FALSE) |>
         rename(fips = County.Code) |>
         mortality_map("Crude.Rate")
    vdiffr::expect_doppelganger("diabetes-county-map", p)
  }
)

test_that(
  "The mortality_map() is correct for diabetes data at state level.",
  {
    p <- diabetes |>
         mortality_by_state() |>
         mortality_map("total_mortality", "State", "Diabetes")
    vdiffr::expect_doppelganger("diabetes-state-map", p)
  }
)



# mortality_vs_svi_scatter() --------------------------------------------------------------------------

test_that(
  "The mortality_vs_svi_scatter() returns a ggplot object.",
  {
    df <- prepare(vulnerability, diabetes, reliable = FALSE)
    p <- mortality_vs_svi_scatter(df, "RPL_THEME1", "Diabetes")
    expect_true(inherits(p, "gg"))
  }
)

test_that(
  "The mortality_vs_svi_scatter() errors when no `MORTALITY` column.",
  {
    data(iris)
    expect_error(mortality_vs_svi_scatter(iris, "Sepal.Length"))
  }
)

test_that(
  "The mortality_vs_svi_scatter() errors when index not SVI estimates",
  {
    temp <- diabetes |>
            rename(fips = County.Code, MORTALITY = Crude.Rate)
    expect_error(mortality_vs_svi_scatter(temp, "Deaths"))
  }
)

test_that(
  "The mortality_vs_svi_scatter() is correct for SVI data.",
  {
    df <- prepare(vulnerability, diabetes, reliable = FALSE)
    p <- mortality_vs_svi_scatter(df, "RPL_THEME1", "Diabetes")
    vdiffr::expect_doppelganger("diabetes-RPL_THEME1", p)
  }
)

