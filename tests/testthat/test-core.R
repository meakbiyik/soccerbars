library(testthat)
library(scorebars)

pdf(NULL)

test_dataframes <- list(
    list(
        data.frame(
            home_team_score = c(8, 4, 4, 1, 5, 0, 1, 2, NA, NA, NA),
            away_team_score = c(0, 1, 4, 4, 0, 0, 1, 3, NA, NA, NA),
            is_away_game = c(F, T, F, T, F, T, F, T, F, T, F)
        ),
        list(
            list(home_team_score = 8, away_team_score = 0, is_away_game = F),
            list(home_team_score = 4, away_team_score = 1, is_away_game = T),
            list(home_team_score = 4, away_team_score = 4, is_away_game = F),
            list(home_team_score = 1, away_team_score = 4, is_away_game = T),
            list(home_team_score = 5, away_team_score = 0, is_away_game = F),
            list(home_team_score = 0, away_team_score = 0, is_away_game = T),
            list(home_team_score = 1, away_team_score = 1, is_away_game = F),
            list(home_team_score = 2, away_team_score = 3, is_away_game = T),
            list(
                home_team_score = NA_integer_,
                away_team_score = NA_integer_,
                is_away_game = F
            ),
            list(
                home_team_score = NA_integer_,
                away_team_score = NA_integer_,
                is_away_game = T
            ),
            list(
                home_team_score = NA_integer_,
                away_team_score = NA_integer_,
                is_away_game = F
            )
        )
    ),
    list(
        data.frame(
            home_team_score = c(),
            away_team_score = c(),
            is_away_game = c()
        ),
        list()
    ),
    list(
        list(
            data.frame(
                home_team_score = c(8, 4, 4),
                away_team_score = c(0, 1, 4),
                is_away_game = c(F, T, F)
            ),
            data.frame(
                home_team_score = c(NA, NA, NA),
                away_team_score = c(NA, NA, NA),
                is_away_game = c(F, T, F)
            )
        ),
        list(
            list(
                list(
                    home_team_score = 8,
                    away_team_score = 0,
                    is_away_game = F
                ),
                list(
                    home_team_score = 4,
                    away_team_score = 1,
                    is_away_game = T
                ),
                list(
                    home_team_score = 4,
                    away_team_score = 4,
                    is_away_game = F
                )
            ),
            list(
                list(
                    home_team_score = NA,
                    away_team_score = NA,
                    is_away_game = F
                ),
                list(
                    home_team_score = NA,
                    away_team_score = NA,
                    is_away_game = T
                ),
                list(
                    home_team_score = NA,
                    away_team_score = NA,
                    is_away_game = F
                )
            )
        )
    )
)

bad_scores <- list(
    list(3, strwrap("Assertion on 'scores' failed: Must
        be of type 'list', not 'double'.")),
    list(list(), strwrap("Assertion on 'scores' failed:
        Must have length >= 1, but has length 0.")),
    list(list(list(1, "A", F)), strwrap("Assertion on 'Scores
    of the away team' failed: Must be of type 'single integerish
    value', not 'character'.", width = 1200)),
    list(list(list(list(1, 2, 3), list(1, 2, 3))), strwrap("Assertion
    on 'Away flag' failed: Must be of type 'logical', not
    'double'.", width = 1200))
)

test_inputs <- list(
    list(
        list(home_team_score = 8, away_team_score = 0, is_away_game = F),
        list(home_team_score = 4, away_team_score = 1, is_away_game = T),
        list(home_team_score = 4, away_team_score = 4, is_away_game = F),
        list(home_team_score = 1, away_team_score = 4, is_away_game = T),
        list(home_team_score = 5, away_team_score = 0, is_away_game = F),
        list(home_team_score = 0, away_team_score = 0, is_away_game = T),
        list(home_team_score = 1, away_team_score = 1, is_away_game = F),
        list(home_team_score = 2, away_team_score = 3, is_away_game = T),
        list(
            home_team_score = NA,
            away_team_score = NA,
            is_away_game = F
        ),
        list(
            home_team_score = NA,
            away_team_score = NA,
            is_away_game = T
        ),
        list(
            home_team_score = NA,
            away_team_score = NA,
            is_away_game = F
        )
    ),
    list(
        list(
            list(
                home_team_score = 8,
                away_team_score = 0,
                is_away_game = F
            ),
            list(
                home_team_score = 4,
                away_team_score = 1,
                is_away_game = T
            ),
            list(
                home_team_score = 4,
                away_team_score = 4,
                is_away_game = F
            )
        ),
        list(
            list(
                home_team_score = NA,
                away_team_score = NA,
                is_away_game = F
            ),
            list(
                home_team_score = NA,
                away_team_score = NA,
                is_away_game = T
            ),
            list(
                home_team_score = NA,
                away_team_score = NA,
                is_away_game = F
            )
        )
    )
)

dummy_file_name <- ".dummy.png"

test_parameters <- list(
    list(outlined = F, twogoalline = F,
        nozerodots = F, show = F, output_path = NULL),
    list(outlined = F, twogoalline = F,
        nozerodots = F, show = T, output_path = NULL),
    list(outlined = F, twogoalline = F,
        nozerodots = T, show = F, output_path = NULL),
    list(outlined = F, twogoalline = F,
        nozerodots = T, show = T, output_path = NULL),
    list(outlined = F, twogoalline = T,
        nozerodots = F, show = F, output_path = NULL),
    list(outlined = F, twogoalline = T,
        nozerodots = F, show = T, output_path = NULL),
    list(outlined = F, twogoalline = T,
        nozerodots = T, show = F, output_path = NULL),
    list(outlined = F, twogoalline = T,
        nozerodots = T, show = T, output_path = NULL),
    list(outlined = T, twogoalline = F,
        nozerodots = F, show = F, output_path = dummy_file_name),
    list(outlined = T, twogoalline = F,
        nozerodots = F, show = T, output_path = dummy_file_name),
    list(outlined = T, twogoalline = F,
        nozerodots = T, show = F, output_path = dummy_file_name),
    list(outlined = T, twogoalline = F,
        nozerodots = T, show = T, output_path = dummy_file_name),
    list(outlined = T, twogoalline = T,
        nozerodots = F, show = F, output_path = dummy_file_name),
    list(outlined = T, twogoalline = T,
        nozerodots = F, show = T, output_path = dummy_file_name),
    list(outlined = T, twogoalline = T,
        nozerodots = T, show = F, output_path = dummy_file_name),
    list(outlined = T, twogoalline = T,
        nozerodots = T, show = T, output_path = dummy_file_name)
)

test_that("scores are lazily converted from data.frame", {
        for (example in test_dataframes) {
            expect_equal(maybe_convert_dataframe(example[[1]]), example[[2]])
        }
    }
)

test_that("bad inputs are correctly identified", {
        for (example in bad_scores) {
            expect_error(check_scores(example[[1]]), example[[2]])
        }
    }
)

test_that("config factory produces correct configuration lists", {
        expect_equal(config_factory(T), default_config)
        expect_equal(config_factory(F)[["edge_thickness"]], 0)
        expect_equal(config_factory(T, slant = 20)[["slant"]],
            sin(20 * pi / 180))
        expect_equal(config_factory(T, zerodot = 10)[["zerodot"]],
            10 * default_config[["thickness"]])
        expect_equal(config_factory(T, zerodot = 10,
            thickness = 5)[["zerodot"]], 10 * 5)
        expect_equal(config_factory(T, fill_color = "red")[["fill_color"]],
            col2rgb("red", alpha = TRUE))
        expect_equal(config_factory(T, fill_color = "#FF0000")[["fill_color"]],
            col2rgb("red", alpha = TRUE))
        expect_equal(config_factory(T,
            fill_color = "#FF0000FF")[["fill_color"]],
            col2rgb("red", alpha = TRUE))
        expect_equal(config_factory(T,
            clip_slanted_lines = F)[["clip_slanted_lines"]], F)
        expect_error(config_factory(T, fake_argument = F),
            ".+?is not a valid configuration parameter.+")
    }
)

test_that("colors are consistent with the match and config", {
        home_color <- do.call(
            rgb, append(default_config[["home_color"]],
            list(maxColorValue = 255))
        )
        away_color <- do.call(
            rgb, append(default_config[["away_color"]],
            list(maxColorValue = 255))
        )
        bright_away_color <- do.call(
            rgb, append(list(
                red = default_config[["away_color"]][[1]] +
                    (255 - default_config[["away_color"]][[1]]) *
                    default_config[["brighten"]] / 100,
                green = default_config[["away_color"]][[2]] +
                    (255 - default_config[["away_color"]][[2]]) *
                    default_config[["brighten"]] / 100,
                blue = default_config[["away_color"]][[3]] +
                    (255 - default_config[["away_color"]][[3]]) *
                    default_config[["brighten"]] / 100,
                alpha = default_config[["away_color"]][[4]]
            ), list(maxColorValue = 255))
        )
        fill_color <- do.call(
            rgb, append(default_config[["fill_color"]],
            list(maxColorValue = 255))
        )
        expect_equal(colors(F, F, default_config), list(home_color, home_color))
        expect_equal(colors(T, F, default_config),
            list(bright_away_color, bright_away_color))
        expect_equal(colors(F, T, default_config), list(home_color, home_color))
        expect_equal(colors(T, T, default_config), list(fill_color, away_color))
    }
)

test_that("plot_scores run without any errors, warnings or prints", {
        for (example in test_inputs) {
            for (parameters in test_parameters) {
                expect_silent(
                    do.call(plot_scores, append(list(example), parameters))
                )
            }
        }
        if (file.exists(dummy_file_name)) {
            file.remove(dummy_file_name)
        }
    }
)