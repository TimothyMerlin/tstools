test_that("tsggplot", {
  tsl <- list(AirPassengers = AirPassengers, JohnsonJohnson = JohnsonJohnson)
  tsg <- list(
    AirPassengers = diff(log(AirPassengers)) * 100,
    JohnsonJohnson = diff(log(JohnsonJohnson)) * 100
  )

  tsggplot(tsl)

  tstools::tsplot(list(tsl$AirPassengers), tsr = list(tsl$JohnsonJohnson))
  tsggplot(list(tsl$AirPassengers), tsr = list(tsl$JohnsonJohnson))

  # bar
  t <- tstools::init_tsplot_theme(x_tick_dt = 2)
  tstools::tsplot(list(tsl$AirPassengers),
    tsr = list(tsl$JohnsonJohnson),
    theme = t
  )
  t <- init_tsggplot_theme(axis_y_show = FALSE, axis_x_show = FALSE)
  tsggplot(list(tsl$AirPassengers),
    tsr = list(tsl$JohnsonJohnson),
    left_as_bar = TRUE,
    theme = t
  )

  # grouped bar
  tstools::tsplot(list(tsg$JohnsonJohnson, lag(tsg$JohnsonJohnson, 2)),
    left_as_bar = TRUE,
    relative_bar_chart = TRUE
  )
  tsggplot(list(tsg$JohnsonJohnson, lag(tsg$JohnsonJohnson, 2)),
    left_as_bar = TRUE,
    group_bar_chart = FALSE
  )

  # band
  tstools::tsplot(list(tsl$AirPassengers),
    tsr = list(tsg$JohnsonJohnson),
    left_as_band = TRUE
  )
  tsggplot(list(tsl$AirPassengers),
    tsr = list(tsg$JohnsonJohnson),
    left_as_band = TRUE
  )

  tstools::tsplot(
    list(tsl$AirPassengers),
    tsr = list(tsg$AirPassengers),
    left_as_bar = TRUE
  )
  t <- init_tsggplot_theme(grids_x_show = TRUE, axis_x_label_dt = 1)
  tsggplot(list(tsl$AirPassengers),
    tsr = list(tsg$AirPassengers),
    left_as_bar = TRUE,
    theme = t
  )

  tstools::tsplot(
    list(tsl$JohnsonJohnson),
    tsr = list(tsl$JohnsonJohnson),
    left_as_bar = TRUE
  )
  tsggplot(
    list(tsl$JohnsonJohnson),
    tsr = list(tsg$JohnsonJohnson),
    left_as_bar = TRUE
  )

  tstools::tsplot(
    list(tsl$AirPassengers),
    tsr = list(tsg$JohnsonJohnson),
    left_as_bar = TRUE
  )
  tsggplot(list(tsl$AirPassengers),
    tsr = list(tsl$JohnsonJohnson),
    left_as_bar = TRUE
  )
})
