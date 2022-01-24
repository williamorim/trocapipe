library(dplyr)

mtcarms |>
  select(mpg, cyl, wt) |>
  group_by(cyl) |>
  summarise(
    mpg = mean(mpg),
    wt = mean(wt)
  )
