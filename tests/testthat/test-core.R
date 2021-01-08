library(scorebars)

test_that("scores are lazily converted from data.frame", {
  expect_equal(maybe_convert_dataframe(
    data.frame(
      home_team_score = c(1, 2, 3),
      away_team_score = c(1, 2, 3),
      is_away_game = c(T, F, T)
    )),
    list(c(1, 1, 1), c(2, 2, 0), c(3, 3, 1)))
})
