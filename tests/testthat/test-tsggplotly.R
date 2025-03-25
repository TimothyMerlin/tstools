test_that("tsggplotly", {
  tsl <- list(AirPassengers = AirPassengers, JohnsonJohnson = JohnsonJohnson)
  labs <- list(
    x = "Engine displacement (litres)",
    y = "Highway miles per gallon",
    y_right = "Medication per kilogram",
    title = "Air Passengers",
    subtitle = "In thousands"
  )
  t <- init_tsggplot_theme(
    plot.title = element_text(size = 30, face = "bold", color = "red"),
    plot.subtitle = element_text(size = 20),
    plot.caption = element_text(size = 25),
    plot.tag = element_text(size = 15)
  )
  p <- tsggplot(tsl, labs = labs, theme = t)

  tsggplotly(p)
})

test_that("tsggplotly", {
  t <- init_tsggplot_theme(
    axis.line.y = element_line(colour = NA),
    axis.line.x = element_line(colour = NA),
    axis.text.x = element_text(hjust = 0, size = 10),
    axis.text.y.left = element_text(size = 10),
    axis.ticks.x.bottom = element_blank(),
    axis.minor.ticks.x.bottom = element_blank(),
    legend.justification = "left",
    plot.title = element_text(size = 20, face = "bold"),
    plot.subtitle = element_text(size = 15),
    grids_x_show = TRUE,
    axis_x_label_dt = 1,
    text = element_text(family = "sans")
  )
  labs <- list(
    title = "Air Passengers",
    subtitle = "In thousands"
  )
  p <- tsggplot(list("Prognose Frühjahr" = diff(log(AirPassengers)) * 100),
    tsr = list("Niveau, rechte Skala" = AirPassengers),
    left_as_bar = TRUE,
    theme = t,
    labs = labs
  )

  tsggplotly(p)


  # Check if the theme attributes are applied correctly
})
