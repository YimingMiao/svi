# mnnn_to_na() --------------------------------------------------------------------------

test_that(
  "The mnnn_to_na() returns a dataframe.",
  {
    d <- diabetes |> mnnn_to_na("Crude.Rate", "Unreliable")
    expect_true(inherits(d, "data.frame"))
  }
)


# cr_interpolate() --------------------------------------------------------------------------

test_that(
  "The cr_interpolate() returns a dataframe for `reliable = TRUE`.",
  {
    d <- diabetes |> cr_interpolate(reliable = TRUE)
    expect_true(inherits(d, "data.frame"))
  }
)

test_that(
  "The cr_interpolate() returns a dataframe for `reliable = FALSE`.",
  {
    d <- diabetes |> cr_interpolate(reliable = FALSE)
    expect_true(inherits(d, "data.frame"))
  }
)

test_that(
  "The cr_interpolate() errors when no `Deaths`/`Population`/`Crude.Rate` columns.",
  {
    data(iris)
    expect_error(cr_interpolate(iris))
  }
)


# prepare() --------------------------------------------------------------------------

test_that(
  "The prepare() returns a dataframe.",
  {
    d <- prepare(vulnerability, diabetes)
    expect_true(inherits(d, "data.frame"))
  }
)


# mortality_by_state() --------------------------------------------------------------------------

test_that(
  "The mortality_by_state() returns a dataframe.",
  {
    d <- mortality_by_state(diabetes)
    expect_true(inherits(d, "data.frame"))
  }
)

test_that(
  "The mortality_by_state() errors when no `County`/`County.Code`/`Deaths`/`Population` columns.",
  {
    data(iris)
    expect_error(mortality_by_state(iris))
  }
)
