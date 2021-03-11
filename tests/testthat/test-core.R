library(testthat)
library(soccerbars)

pdf(NULL)

test_dataframes <- list(
    list(
        data.frame(
            home_team_score = c(8, 4, 4, 1, 5, 0, 1, 2, NA, NA, NA),
            away_team_score = c(0, 1, 4, 4, 0, 0, 1, 3, NA, NA, NA),
            is_away_game = c(
                FALSE, TRUE, FALSE, TRUE, FALSE, TRUE,
                FALSE, TRUE, FALSE, TRUE, FALSE
            )
        ),
        list(
            list(
                home_team_score = 8, away_team_score = 0, is_away_game = FALSE
            ),
            list(
                home_team_score = 4, away_team_score = 1, is_away_game = TRUE
            ),
            list(
                home_team_score = 4, away_team_score = 4, is_away_game = FALSE
            ),
            list(
                home_team_score = 1, away_team_score = 4, is_away_game = TRUE
            ),
            list(
                home_team_score = 5, away_team_score = 0, is_away_game = FALSE
            ),
            list(
                home_team_score = 0, away_team_score = 0, is_away_game = TRUE
            ),
            list(
                home_team_score = 1, away_team_score = 1, is_away_game = FALSE
            ),
            list(
                home_team_score = 2, away_team_score = 3, is_away_game = TRUE
            ),
            list(
                home_team_score = NA_integer_,
                away_team_score = NA_integer_,
                is_away_game = FALSE
            ),
            list(
                home_team_score = NA_integer_,
                away_team_score = NA_integer_,
                is_away_game = TRUE
            ),
            list(
                home_team_score = NA_integer_,
                away_team_score = NA_integer_,
                is_away_game = FALSE
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
                is_away_game = c(FALSE, TRUE, FALSE)
            ),
            data.frame(
                home_team_score = c(NA, NA, NA),
                away_team_score = c(NA, NA, NA),
                is_away_game = c(FALSE, TRUE, FALSE)
            )
        ),
        list(
            list(
                list(
                    home_team_score = 8,
                    away_team_score = 0,
                    is_away_game = FALSE
                ),
                list(
                    home_team_score = 4,
                    away_team_score = 1,
                    is_away_game = TRUE
                ),
                list(
                    home_team_score = 4,
                    away_team_score = 4,
                    is_away_game = FALSE
                )
            ),
            list(
                list(
                    home_team_score = NA,
                    away_team_score = NA,
                    is_away_game = FALSE
                ),
                list(
                    home_team_score = NA,
                    away_team_score = NA,
                    is_away_game = TRUE
                ),
                list(
                    home_team_score = NA,
                    away_team_score = NA,
                    is_away_game = FALSE
                )
            )
        )
    )
)

vector_test_inputs <- list(
    list(
        list(
            c(1, 2, 3),
            c(4, 5, 6),
            c(TRUE, FALSE, TRUE)
        ),
        list(
            list(c.1..2..3. = 1, c.4..5..6. = 4, c.TRUE..FALSE..TRUE. = TRUE),
            list(c.1..2..3. = 2, c.4..5..6. = 5, c.TRUE..FALSE..TRUE. = FALSE),
            list(c.1..2..3. = 3, c.4..5..6. = 6, c.TRUE..FALSE..TRUE. = TRUE)
        )
    ),
    list(
        list(
            list(
                c(1, NA),
                c(3, NA),
                c(TRUE, FALSE)
            ),
            list(
                c(5, 6),
                c(7, 8),
                c(TRUE, FALSE)
            )
        ),
        list(
            list(
                list(c.1..NA. = 1, c.3..NA. = 3, c.TRUE..FALSE. = TRUE),
                list(
                    c.1..NA. = NA_integer_,
                    c.3..NA. = NA_integer_,
                    c.TRUE..FALSE. = FALSE
                )
            ),
            list(
                list(c.5..6. = 5, c.7..8. = 7, c.TRUE..FALSE. = TRUE),
                list(c.5..6. = 6, c.7..8. = 8, c.TRUE..FALSE. = FALSE)
            )
        )
    ),
    list(
        list(
            list(1, 4, TRUE),
            list(2, 5, FALSE),
            list(3, 6, TRUE)
        ),
        list(
            list(1, 4, TRUE),
            list(2, 5, FALSE),
            list(3, 6, TRUE)
        )
    ),
    list(
        list(
            list(
                list(1, 3, TRUE),
                list(NA, NA, FALSE)
            ),
            list(
                list(5, 7, TRUE),
                list(6, 8, FALSE)
            )
        ),
        list(
            list(
                list(1, 3, TRUE),
                list(NA, NA, FALSE)
            ),
            list(
                list(5, 7, TRUE),
                list(6, 8, FALSE)
            )
        )
    )
)

bad_scores <- list(
    list(3, strwrap("Assertion on 'scores' failed: Must
        be of type 'list', not 'double'.")),
    list(list(), strwrap("Assertion on 'scores' failed:
        Must have length >= 1, but has length 0.")),
    list(list(list(1, "A", FALSE)), strwrap("Assertion on 'Score
    of the away team' failed: Must be of type 'single integerish
    value', not 'character'.", width = 1200)),
    list(list(list(list(1, 2, 3), list(1, 2, 3))), strwrap("Assertion
    on 'Away flag' failed: Must be of type 'logical', not
    'double'.", width = 1200)),
    list(list(list(list(1, 2, 3), list(1, 2, 3), list(1, 2, 3))),
    strwrap("Assertion on 'Away flag' failed: Must be of type 'logical',
    not 'double'.", width = 1200)),
    list(list(c("1", "2", "3"), c(1, 2, 3), c(TRUE, FALSE, TRUE)),
    strwrap("Assertion on 'scores' failed: May only contain the following
    types: \\{list,numeric,logical\\}, but element 1 has type 'character'.",
    width = 1200)),
    list(list(list(c(1, 2, 3), c(1, 2, 3, 4), c(TRUE, FALSE, TRUE))),
    strwrap("Assertion on 'Scores of the away team' failed: Must have
    length 3, but has length 4.", width = 1200)),
    list(list(list(c(1, 2, 3), c(1, 2, 3), c(1, 2, 3))),
    strwrap("Assertion on 'Away flags' failed: Must be of type 'logical',
    not 'double'.", width = 1200))
)

bad_color_inputs <- list(
    list(
        c("red", "red"),
        NULL,
        list(list(8, 0, FALSE), list(4, 1, TRUE), list(4, 4, FALSE)),
        ".+? length 3, but has length 2."
    ),
    list(
        c("red", "red", "red"),
        NULL,
        list(list(list(8, 0, FALSE), list(4, 1, TRUE), list(4, 4, FALSE))),
        ".+? type 'list', not 'character'.+?"
    ),
    list(
        c("rod", "red", "red"),
        NULL,
        list(list(list(8, 0, FALSE), list(4, 1, TRUE), list(4, 4, FALSE))),
        ".+? 'list', not 'character'.+?"
    ),
    list(
        c("re", "red", "red"),
        NULL,
        list(list(8, 0, FALSE), list(4, 1, TRUE), list(4, 4, FALSE)),
        "'re' is not a valid color.+?"
    ),
    list(
        list(c("re", "red", "red")),
        NULL,
        list(list(list(8, 0, FALSE), list(4, 1, TRUE), list(4, 4, FALSE))),
        "'re' is not a valid color.+?"
    ),
    list(
        c("red", "red"),
        c("img.png", "img2.png"),
        list(list(8, 0, FALSE), list(4, 1, TRUE), list(4, 4, FALSE)),
        ".+? length 1, but has length 2.+?"
    ),
    list(
        c("re", "red", "red"),
        1234,
        list(list(8, 0, FALSE), list(4, 1, TRUE), list(4, 4, FALSE)),
        ".+? type 'character', not 'double'.+?"
    ),
    list(
        list(c("re", "red", "red")),
        c("img.png", "img2.png"),
        list(list(list(8, 0, FALSE), list(4, 1, TRUE), list(4, 4, FALSE))),
        ".+? length 1, but has length 2.+?"
    )
)

dummy_file_name <- ".dummy.png"

test_inputs <- list(
    list(
        list(home_team_score = 8, away_team_score = 0, is_away_game = FALSE),
        list(home_team_score = 4, away_team_score = 1, is_away_game = TRUE),
        list(home_team_score = 4, away_team_score = 4, is_away_game = FALSE),
        list(home_team_score = 1, away_team_score = 4, is_away_game = TRUE),
        list(home_team_score = 5, away_team_score = 0, is_away_game = FALSE),
        list(home_team_score = 0, away_team_score = 0, is_away_game = TRUE),
        list(home_team_score = 1, away_team_score = 1, is_away_game = FALSE),
        list(home_team_score = 2, away_team_score = 3, is_away_game = TRUE),
        list(
            home_team_score = NA,
            away_team_score = NA,
            is_away_game = FALSE
        ),
        list(
            home_team_score = NA,
            away_team_score = NA,
            is_away_game = TRUE
        ),
        list(
            home_team_score = NA,
            away_team_score = NA,
            is_away_game = FALSE
        )
    ),
    list(
        list(
            list(
                home_team_score = 8,
                away_team_score = 0,
                is_away_game = FALSE
            ),
            list(
                home_team_score = 4,
                away_team_score = 1,
                is_away_game = TRUE
            ),
            list(
                home_team_score = 4,
                away_team_score = 4,
                is_away_game = FALSE
            )
        ),
        list(
            list(
                home_team_score = NA,
                away_team_score = NA,
                is_away_game = FALSE
            ),
            list(
                home_team_score = NA,
                away_team_score = NA,
                is_away_game = TRUE
            ),
            list(
                home_team_score = NA,
                away_team_score = NA,
                is_away_game = FALSE
            )
        )
    )
)

test_parameters <- list(
    list(outlined = FALSE, twogoalline = FALSE,
        zerodots = FALSE, show = FALSE, output_path = NULL),
    list(outlined = FALSE, twogoalline = FALSE,
        zerodots = FALSE, show = TRUE, output_path = NULL),
    list(outlined = FALSE, twogoalline = FALSE,
        zerodots = TRUE, show = FALSE, output_path = NULL),
    list(outlined = FALSE, twogoalline = FALSE,
        zerodots = TRUE, show = TRUE, output_path = NULL),
    list(outlined = FALSE, twogoalline = TRUE,
        zerodots = FALSE, show = FALSE, output_path = NULL),
    list(outlined = FALSE, twogoalline = TRUE,
        zerodots = FALSE, show = TRUE, output_path = NULL),
    list(outlined = FALSE, twogoalline = TRUE,
        zerodots = TRUE, show = FALSE, output_path = NULL),
    list(outlined = FALSE, twogoalline = TRUE,
        zerodots = TRUE, show = TRUE, output_path = NULL),
    list(outlined = TRUE, twogoalline = FALSE, transparent_background = TRUE,
        zerodots = FALSE, show = FALSE, output_path = dummy_file_name),
    list(outlined = TRUE, twogoalline = FALSE,
        zerodots = FALSE, show = TRUE, output_path = dummy_file_name),
    list(outlined = TRUE, twogoalline = FALSE,
        zerodots = TRUE, show = FALSE, output_path = dummy_file_name),
    list(outlined = TRUE, twogoalline = FALSE,
        zerodots = TRUE, show = TRUE, output_path = dummy_file_name),
    list(outlined = TRUE, twogoalline = TRUE,
        zerodots = FALSE, show = FALSE, output_path = dummy_file_name),
    list(outlined = TRUE, twogoalline = TRUE,
        zerodots = FALSE, show = TRUE, output_path = dummy_file_name),
    list(outlined = TRUE, twogoalline = TRUE,
        zerodots = TRUE, show = FALSE, output_path = dummy_file_name),
    list(outlined = TRUE, twogoalline = TRUE,
        zerodots = TRUE, show = TRUE, output_path = dummy_file_name)
)

test_that("scores are lazily converted from data.frame", {
        for (example in test_dataframes) {
            expect_equal(maybe_convert_dataframe(example[[1]]), example[[2]])
        }
    }
)

test_that("vector inputs are lazily coerced to lists", {
        for (example in vector_test_inputs) {
            expect_equal(maybe_flatten_vectors(example[[1]]), example[[2]])
        }
    }
)

test_that("bad inputs are correctly identified", {
        for (example in bad_scores) {
            expect_error(check_scores(example[[1]]), example[[2]])
        }
    }
)

test_that("bad color and output path configs are correctly identified", {
        for (example in bad_color_inputs) {
            expect_error(
                check_color_and_output_path(
                    example[[1]],
                    example[[2]],
                    example[[3]]
                ), example[[4]]
            )
        }
    }
)

test_that("config factory produces correct configuration lists", {
        expect_equal(config_factory(TRUE), default_config)
        expect_equal(config_factory(FALSE)[["edge_thickness"]], 0)
        expect_equal(config_factory(TRUE,
            edge_thickness = 10)[["edge_thickness"]],
            10 * default_config[["thickness"]])
        expect_equal(config_factory(TRUE,
            edge_thickness = 10, thickness = 3)[["edge_thickness"]],
            10 * 3)
        expect_equal(config_factory(TRUE, slant = 20)[["slant"]],
            sin(20 * pi / 180))
        expect_equal(config_factory(TRUE, zerodot = 10)[["zerodot"]],
            10 * default_config[["thickness"]])
        expect_equal(config_factory(TRUE, zerodot = 10,
            thickness = 5)[["zerodot"]], 10 * 5)
        expect_equal(config_factory(TRUE, fill_color = "red")[["fill_color"]],
            col2rgb("red", alpha = TRUE))
        expect_equal(
            config_factory(TRUE, fill_color = "#FF0000")[["fill_color"]],
            col2rgb("red", alpha = TRUE))
        expect_equal(config_factory(TRUE,
            fill_color = "#FF0000FF")[["fill_color"]],
            col2rgb("red", alpha = TRUE))
        expect_equal(config_factory(TRUE,
            clip_slanted_lines = FALSE)[["clip_slanted_lines"]], FALSE)
        expect_error(config_factory(TRUE, fake_argument = FALSE),
            ".+?is not a valid configuration parameter.+")
    }
)

test_that("colors are consistent with the match and config", {
        matchcolor <- "#2BAC3648"
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

        expect_equal(
            get_colors(FALSE, FALSE, default_config),
            list(home_color, home_color)
        )
        expect_equal(get_colors(TRUE, FALSE, default_config),
            list(bright_away_color, bright_away_color))
        expect_equal(
            get_colors(FALSE, TRUE, default_config),
            list(home_color, home_color)
        )
        expect_equal(
            get_colors(TRUE, TRUE, default_config), list(fill_color, away_color)
        )

        expect_equal(
            get_colors(FALSE, FALSE, default_config, matchcolor = matchcolor),
            list(matchcolor, matchcolor)
        )
        expect_equal(
            get_colors(
                TRUE, FALSE, default_config,
                matchcolor = matchcolor
            ),
            list(matchcolor, matchcolor))
        expect_equal(
            get_colors(FALSE, TRUE, default_config, matchcolor = matchcolor),
            list(matchcolor, matchcolor)
        )
        expect_equal(
            get_colors(TRUE, TRUE, default_config, matchcolor = matchcolor),
            list(fill_color, matchcolor)
        )
    }
)

test_that("soccerbar run without any errors, warnings or prints", {
        for (example in test_inputs) {
            for (parameters in test_parameters) {
                if (
                    is.list(example[[1]][[1]]) &&
                    !is.null(parameters[["output_path"]])
                ) {
                    parameters[["output_path"]] <- c(
                        parameters[["output_path"]],
                        parameters[["output_path"]]
                    )
                }
                expect_silent(
                    do.call(soccerbar, append(list(example), parameters))
                )
            }
        }
        if (file.exists(dummy_file_name)) {
            file.remove(dummy_file_name)
        }
    }
)
