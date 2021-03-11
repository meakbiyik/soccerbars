#' @import ggplot2
#' @importFrom grDevices col2rgb rgb

ppi <- 72
max_height <- 3.75
height_mapping <- c(0, 1, 1.7, 2.25, 2.65, 2.95, 3.20, 3.40, 3.60)
goal_to_height <- function(x) {
    if (x %in% c(0, 1, 2, 3, 4, 5, 6, 7, 8)) height_mapping[[x + 1]] else 3.75
}

default_config <- list(
    dpi = 300,
    thickness = 0.36,
    edge_thickness = 0.35 * 0.36,
    goalless_edge_thickness = 0.5 * 0.36,
    zerodot = 0.4 * 0.36,
    slant = sin(14 * pi / 180),
    spacing = 0.8,
    padding = 0.25,
    baseline_factor = 0.2,
    brighten = 33,
    transparent_background = FALSE,
    home_color = col2rgb("black", alpha = TRUE),
    away_color = col2rgb("black", alpha = TRUE),
    baseline_color = col2rgb("black", alpha = TRUE),
    fill_color = col2rgb(NA, alpha = TRUE),
    clip_slanted_lines = TRUE
)

#' Plot Multivariate Sparklines
#'
#' Plot the given list or lists of match results as multivariate sparklines.
#' This function can either show the plot, or save it to a given path.
#'
#' @param scores A list, a list of lists, a data.frame or a list of data.frames
#' of match scores. A match score list has the elements (matches) in either
#' of two alternative forms: three vectors of format
#'      <home team score: int, away team score: int, is away game: logical>
#' or lists of three length-one elements with these datatypes. If the matches
#' are given as data.frames, then each column is expected to have the same
#' structure and data types as these alternatives. Some valid examples:
#'  - list(c(1,2,3), c(4,5,6), c(T,F,T))
#'  - list(list(1,4,T), list(2,5,F), list(3,6,T))
#'  - data.frame(list(home=c(1,2,3), away=c(4,5,6), flag=c(T,F,T)))
#'  - list(list(c(1,2), c(3,4), c(T,F)), list(c(5,6), c(7,8), c(T,F)))
#' @param twogoalline A logical, draws lines for two-goal levels,
#' by default FALSE.
#' @param zerodots A logical, marks no goals scored with a small dot,
#' by default FALSE.
#' @param outlined A logical, only plots the outlines of the sparklines,
#' by default FALSE.
#' @param color A vector or a list of vectors, specifying the color of each
#' soccerbar. This option is provided to be consistent with LaTeX package API
#' and allows high configurability, but `home_color`, `away_color` and
#' `fill_color` options should already be sufficient for most of the use cases,
#' by default NULL. This parameter is expected to be structurally identical
#' with the `scores`: if a list of match score lists is given, then `color`
#' must also be a list of vectors.
#' @param show A logical, prints the plot, by default TRUE.
#' @param output_path A string or a vector of strings, path to save the plot
#' at (image type is inferred from the path), by default NULL. This parameter
#' is expected to be structurally identical with the `scores`: if a list of
#' match score lists is given, then `output_path` must have the same number
#' of elements with the number of score sets.
#' @param ... A named list, additional configuration keywords for the
#' visualization. Not necessarily consistent with its latex counterpart,
#' but mostly a superset of it.
#'  - dpi: Dots per inch resolution, by default 300
#'  - thickness: Line thickness in cartesian coordinates, by default 0.18
#'  - edge_thickness: Edge thickness for outlined patches (when outlined=TRUE)
#'      as the ratio to the line thickness, by default 0.35
#'  - goalless_edge_thickness: Edge thickness for outlined no-goal patches
#'      (when outlined=TRUE) as the ratio to the line thickness, by default 0.5
#'  - zerodot: Zero-dot radius ratio to thickness (when zerodots=TRUE),
#'      by default 0.4
#'  - slant: Slope for unbalanced scores in degrees, by default 14
#'  - spacing: Spacing between matches in cartesian coordinates, by default 0.8
#'  - padding: Padding before and after the matches in cartesian coordinates,
#'      by default 0.25
#'  - baseline_factor: Thickness of baseline with respect to line thickness,
#'      by default 0.2
#'  - brighten: Brightness percentage of the away games (when outlined=FALSE),
#'      by default 33
#'  - transparent_background: Set the background transparent instead of white,
#'      by default FALSE
#'  - home_color: Color of home match lines in any of the three kinds of R
#'      color specifications (i.e. either a color name as listed by colors(),
#'      a hexadecimal string of the form "#rrggbb" or "#rrggbbaa",
#'      or a positive integer i meaning `palette()[i]`), by default "black"
#'  - away_color: Color of away match lines in any of the three kinds of R
#'      color specifications, by default "black"
#'  - baseline_color: Color of baselines in any of the three kinds of R
#'      color specifications, by default "black"
#'  - fill_color: Fill color for the outlined sparklines in any of the three
#'      kinds of R color specifications, by default NA.
#'  - clip_slanted_lines: Clip the ends of the slanted lines, by default True
#' @return ggplot objects of the plots created. If the input is a list of
#' multiple matches, then the return is an iterable of ggplot objects, one per
#' each match score list.
#' @examples
#' \dontrun{
#' soccerbar(list(
#'     list(1, 2, TRUE), list(3, 3, FALSE), list(0, 2, TRUE),
#'     list(0, 0, FALSE), list(6, 6, TRUE)
#' ))
#' soccerbar(list(
#'     list(1, 2, TRUE), list(3, 3, FALSE), list(0, 2, TRUE), list(0, 0, FALSE),
#'     list(6, 6, TRUE), list(0, 2, FALSE), list(0, 0, FALSE), list(6, 6, TRUE),
#'     list(6, 3, TRUE)
#' ), outlined = TRUE, output_path = "out.png")
#' soccerbar(list(
#'     list(8, 0, FALSE), list(4, 1, TRUE), list(4, 4, FALSE), list(1, 4, TRUE),
#'     list(5, 0, FALSE), list(0, 0, TRUE), list(1, 1, FALSE), list(2, 3, TRUE),
#'     list(NA, NA, FALSE), list(NA, NA, TRUE), list(NA, NA, FALSE)
#' ), home_color = "red")
#' }
#' @export
soccerbar <- function(scores,
                     twogoalline = FALSE,
                     zerodots = FALSE,
                     outlined = FALSE,
                     color = NULL,
                     show = TRUE,
                     output_path = NULL,
                     ...) {
    scores <- maybe_convert_dataframe(scores)

    scores <- maybe_flatten_vectors(scores)

    check_scores(scores)
    check_color_and_output_path(color, output_path, scores)

    config <- config_factory(outlined, ...)
    is_multiple <- is.list(scores[[1]][[1]]) || length(scores[[1]][[1]]) > 1

    if (is_multiple) {
        matchlists <- scores
        matchcolors <- color
    } else {
        matchlists <- list(scores)
        matchcolors <- list(color)
    }

    axes <- list()
    for (matches_index in seq_along(matchlists)) {
        patches <- list()
        baseline_jumps <- c()
        matches <- matchlists[[matches_index]]
        colors <- if (!is.null(color)) matchcolors[[matches_index]] else NULL
        outpath <- if (!is.null(output_path)) output_path[[matches_index]]
            else NULL
        match_count <- length(matches)

        for (index in seq_along(matches)) {
            match <- matches[[index]]
            matchcolor <- if (!is.null(color)) colors[[index]] else NULL
            away_game <- as.logical(match[[3]])
            scores <- match[-3]
            scores <- if (away_game) rev(scores) else scores
            match_index <- index * config[["spacing"]]

            line_colors <- get_colors(away_game, outlined, config, matchcolor)
            facecolor <- line_colors[[1]]
            edgecolor <- line_colors[[2]]

            if (is.na(scores[[1]])) {
                patches <- append(
                    patches,
                    circle_polygon(
                        match_index, 1 - config[["zerodot"]],
                        radius = config[["zerodot"]],
                        facecolor = edgecolor,
                        edgecolor = edgecolor
                    )
                )
                patches <- append(
                    patches,
                    circle_polygon(
                        match_index, -1 + config[["zerodot"]],
                        radius = config[["zerodot"]],
                        facecolor = edgecolor,
                        edgecolor = edgecolor
                    )
                )
                next
            }

            if (scores[[1]] > 0 || scores[[2]] > 0) {
                slope <- config[["slant"]] * sign(scores[[1]] - scores[[2]])
                offset0 <- match_index + slope * goal_to_height(scores[[1]])
                offset1 <- match_index - slope * goal_to_height(scores[[2]])
                height0 <- goal_to_height(scores[[1]])
                height1 <- -goal_to_height(scores[[2]])
                patches <- append(
                    patches,
                    line_polygon(
                        c(offset1, height1),
                        c(offset0, height0),
                        facecolor,
                        edgecolor,
                        config
                    )
                )
            } else {
                patches <- append(patches, circle_polygon(
                    match_index, 0,
                    radius = config[["thickness"]],
                    facecolor = facecolor,
                    edgecolor = edgecolor,
                    edgesize = config[["goalless_edge_thickness"]]
                ))
            }

            if (scores[[1]] > 0 && scores[[2]] > 0) {
                baseline_jumps <- c(
                    baseline_jumps,
                    c(
                        match_index - config[["thickness"]] * 0.95 / 2,
                        match_index + config[["thickness"]] * 0.95 / 2
                    )
                )
            } else if (scores[[1]] == 0 && scores[[2]] == 0) {
                baseline_jumps <- c(
                    baseline_jumps,
                    c(
                        match_index - config[["thickness"]] * 0.95,
                        match_index + config[["thickness"]] * 0.95
                    )
                )
            }

            if (zerodots) {
                if (!scores[[1]]) {
                    patches <- append(patches, circle_polygon(
                        match_index, 1 - config[["zerodot"]],
                        radius = config[["zerodot"]],
                        facecolor = edgecolor,
                        edgecolor = edgecolor
                    ))
                }
                if (!scores[[2]]) {
                    patches <- append(patches, circle_polygon(
                        match_index, -1 + config[["zerodot"]],
                        radius = config[["zerodot"]],
                        facecolor = edgecolor,
                        edgecolor = edgecolor
                    ))
                }
            }
        }

        axes <- append(
            axes,
            plot(
                patches,
                baseline_jumps,
                match_count,
                config,
                show = show,
                twogoalline = twogoalline,
                output_path = outpath
            )
        )
    }

    return(if (is_multiple) axes else axes[[1]])
}

maybe_convert_dataframe <- function(scores) {
    if (is.data.frame(scores)) {
        scores <- lapply(
            seq_len(nrow(scores)),
            function(i) lapply(scores, "[", i)
        )
    }

    if (is.list(scores)) {
        for (index in seq_along(scores)) {
            if (is.data.frame(scores[[index]])) {
                scores[[index]] <- lapply(
                    seq_len(nrow(scores[[index]])),
                    function(i) lapply(scores[[index]], "[", i)
                )
            }
        }
    }
    return(scores)
}

maybe_flatten_vectors <- function(scores) {

    if (is.list(scores)) {

        if (
            length(scores) == 3 &&
            is.atomic(scores[[1]]) &&
            is.atomic(scores[[2]]) &&
            is.atomic(scores[[3]]) &&
            length(scores[[1]]) == length(scores[[2]]) &&
            length(scores[[2]]) == length(scores[[3]])
        ) {
            scores <- maybe_convert_dataframe(data.frame(scores))
        } else {
            for (index in seq_along(scores)) {
                scores_elem <- scores[[index]]
                if (
                    is.list(scores_elem) &&
                    length(scores_elem) == 3 &&
                    is.atomic(scores_elem[[1]]) &&
                    length(scores_elem[[1]]) > 1 &&
                    is.atomic(scores_elem[[2]]) &&
                    length(scores_elem[[2]]) > 1 &&
                    is.atomic(scores_elem[[3]]) &&
                    length(scores_elem[[3]]) > 1 &&
                    length(scores_elem[[1]]) == length(scores_elem[[2]]) &&
                    length(scores_elem[[2]]) == length(scores_elem[[3]])
                ) {
                    scores[[index]] <- maybe_convert_dataframe(
                        data.frame(scores_elem)
                    )
                }
            }
        }
    }
    return(scores)
}

check_scores <- function(scores) {

    checkmate::assert_list(
        scores, types = c("list", "numeric", "logical"), min.len = 1
    )

    is_listofmatchlists <- (
        is.list(scores[[1]][[1]]) || length(scores[[1]][[1]]) > 1
    )

    if (!is_listofmatchlists) {
        scores <- list(scores)
    }

    for (index in seq_along(scores)) {
        scores_elem <- scores[[index]]

        is_vectorlist <- checkmate::test_list(
            scores_elem, len = 3, types = c("atomic")
        )

        if (is_vectorlist) {
            first_length <- length(scores_elem[[1]])
            checkmate::assert_integerish(
                scores_elem[[1]], .var.name = "Scores of the home team"
            )
            checkmate::assert_integerish(
                scores_elem[[2]],
                len = first_length,
                .var.name = "Scores of the away team"
            )
            checkmate::assert_logical(
                scores_elem[[3]], len = first_length, .var.name = "Away flags"
            )
        } else {
            checkmate::assert_list(
                scores_elem, min.len = 1, types = c("list", "numeric"),
                .var.name = "scores"
            )
            first_match <- scores_elem[[1]]
            checkmate::assert_vector(
                first_match,
                len = 3,
                .var.name = "Match vector"
            )
            checkmate::assert_int(first_match[[1]],
                na.ok = TRUE,
                null.ok = FALSE,
                .var.name = "Score of the home team"
            )
            checkmate::assert_int(first_match[[2]],
                na.ok = TRUE,
                null.ok = FALSE,
                .var.name = "Score of the away team"
            )
            checkmate::assert_logical(first_match[[3]],
                len = 1, null.ok = FALSE,
                any.missing = FALSE, .var.name = "Away flag"
            )
        }
    }
}

check_color_and_output_path <- function(color, output_path, scores) {

    if (is.list(scores[[1]][[1]]) || length(scores[[1]][[1]]) > 1) {
        matchlists <- scores
        matchcolors <- color
    } else {
        matchlists <- list(scores)
        matchcolors <- list(color)
    }

    if (!is.null(output_path)) {
        outpaths_ischarvector_or_error <- checkmate::check_character(
            output_path, len = length(matchlists)
        )
        if (!is.logical(outpaths_ischarvector_or_error)) {
            stop(
                sprintf(
                    strwrap(
                        "%s: output_path is expected to be structurally
                        identical with the `scores`. If a list of match
                        score lists is given, then `output_path` must
                        also be a vector of strings.",
                        prefix = " ", width = 1200
                    ),
                    outpaths_ischarvector_or_error
                )
            )
        }
    }

    if (is.null(color)) {
        return(NULL)
    }

    colors_islist_or_error <- checkmate::check_list(
        matchcolors, len = length(matchlists)
    )
    if (!is.logical(colors_islist_or_error)) {
        stop(
            sprintf(
                strwrap(
                    "%s: if multiple lists of scores are given,
                    'colors' argument must also have the same structure,
                    with same-length lists of color vectors.",
                    prefix = " ", width = 1200
                ),
                colors_islist_or_error
            )
        )
    }

    for (matches_index in seq_along(matchlists)) {
        matches <- matchlists[[matches_index]]
        colors <- if (!is.null(color)) matchcolors[[matches_index]] else NULL
        checkmate::assert_vector(colors, len = length(matches))
        for (color in colors) {
            tryCatch({
                col2rgb(color)
            }, error = function(e) {
                stop(
                    sprintf(
                        strwrap(
                            "'%s' is not a valid color. Colors need to be
                            either a color name as listed by colors(), a
                            hexadecimal string of the form '#rrggbb' or
                            '#rrggbbaa', or a positive integer i meaning
                            `palette()[i]`", prefix = " ", width = 1200
                        ),
                        color
                    )
                )
            })
        }
    }
}

config_factory <- function(outlined, ...) {
    kwargs <- list(...)
    config <- default_config

    if (!outlined) {
        config[["edge_thickness"]] <- 0
        config[["goalless_edge_thickness"]] <- 0
    }

    for (key in names(kwargs)) {
        value <- kwargs[[key]]

        if (!(key %in% names(config))) {
            stop(
                sprintf(
                    strwrap(
                        "Keyword argument '%s' is not a valid configuration
                        parameter. Available configuration parameters
                        are (%s)", prefix = " "
                    ),
                    key, paste(names(config), collapse = ", ")
                )
            )
        }

        if (key == "slant") {
            config[[key]] <- sin(value * pi / 180)
            next
        }

        if (key == "zerodot") {
            if (!is.null(kwargs[["thickness"]])) {
                thickness <- kwargs[["thickness"]]
            } else {
                thickness <- config[["thickness"]]
            }
            config[[key]] <- value * thickness
            next
        }

        if (grepl("edge_thickness", key, fixed = TRUE) && outlined) {
            if (!is.null(kwargs[["thickness"]])) {
                config[[key]] <- value * kwargs[["thickness"]]
            } else {
                config[[key]] <- value * config[["thickness"]]
            }
            next
        }

        if (grepl("color", key, fixed = TRUE)) {
            config[[key]] <- col2rgb(value, alpha = TRUE)
            next
        }

        config[[key]] <- value
    }

    return(config)
}

get_colors <- function(away_game, outlined, config, matchcolor=NULL) {

    if (!is.null(matchcolor)) {
        main_color <- col2rgb(matchcolor, alpha = TRUE)
    } else {
        if (away_game) {
            main_color <- config[["away_color"]]
        } else {
            main_color <- config[["home_color"]]
        }

        if (away_game && config[["brighten"]] != 0 && !outlined) {
            main_color <- list(
                red = main_color[[1]] +
                    (255 - main_color[[1]]) * config[["brighten"]] / 100,
                green = main_color[[2]] +
                    (255 - main_color[[2]]) * config[["brighten"]] / 100,
                blue = main_color[[3]] +
                    (255 - main_color[[3]]) * config[["brighten"]] / 100,
                alpha = main_color[[4]]
            )
        }
    }

    if (away_game && outlined) {
        facecolor <- do.call(
            rgb, append(config[["fill_color"]], list(maxColorValue = 255))
        )
    } else {
        facecolor <- do.call(
            rgb, append(main_color, list(maxColorValue = 255))
        )
    }
    edgecolor <- do.call(
        rgb, append(main_color, list(maxColorValue = 255))
    )

    return(list(facecolor, edgecolor))
}

line_polygon <- function(start_xy, end_xy, facecolor, edgecolor, config) {
    thickness <- config[["thickness"]]
    slanted <- sign(end_xy[[1]] - start_xy[[1]])
    slant_degree <- slanted * asin(config[["slant"]])

    inner_start_xy <- c(
        start_xy[[1]] + config[["edge_thickness"]] * sin(slant_degree),
        start_xy[[2]] + config[["edge_thickness"]] * cos(slant_degree)
    )
    inner_end_xy <- c(
        end_xy[[1]] - config[["edge_thickness"]] * sin(slant_degree),
        end_xy[[2]] - config[["edge_thickness"]] * cos(slant_degree)
    )

    main_path <- line_path(start_xy, end_xy, thickness, config)
    inner_path <- line_path(
        inner_start_xy,
        inner_end_xy,
        thickness - 2 * config[["edge_thickness"]],
        config
    )
    main_path$subid <- 1L
    inner_path$subid <- 2L

    outer_polygon <- rbind(main_path, inner_path)
    inner_polygon <- inner_path

    polygon <- list(
        geom_polygon(
            aes(
                x = outer_polygon$x,
                y = outer_polygon$y,
                subgroup = outer_polygon$subid
            ),
            size = 0, fill = edgecolor
        ),
        geom_polygon(
            aes(
                x = inner_polygon$x,
                y = inner_polygon$y
            ),
            size = 0, fill = facecolor
        )
    )
    return(polygon)
}

line_path <- function(start_xy, end_xy, thickness, config) {
    clipped <- start_xy[[1]] != end_xy[[1]]
    half_th <- thickness / 2
    if (clipped && config[["clip_slanted_lines"]]) {
        path_data <- data.frame(
            x = c(
                start_xy[[1]] - half_th,
                start_xy[[1]] + half_th,
                end_xy[[1]] + half_th,
                end_xy[[1]] - half_th
            ),
            y = c(
                start_xy[[2]],
                start_xy[[2]],
                end_xy[[2]],
                end_xy[[2]]
            )
        )
    } else {
        left_path_data <- data.frame(
            x = c(
                start_xy[[1]] + half_th,
                end_xy[[1]] + half_th
            ),
            y = c(
                start_xy[[2]] + half_th,
                end_xy[[2]] - half_th
            )
        )
        top_half_circle_path_data <- circle(
            end_xy[[1]], end_xy[[2]] - half_th, half_th, 0, pi
        )
        right_path_data <- data.frame(
            x = c(
                end_xy[[1]] - half_th,
                start_xy[[1]] - half_th
            ),
            y = c(
                end_xy[[2]] - half_th,
                start_xy[[2]] + half_th
            )
        )
        bottom_half_circle_path_data <- circle(
            start_xy[[1]], start_xy[[2]] + half_th, half_th, pi, 2 * pi
        )
        path_data <- rbind(
            left_path_data,
            top_half_circle_path_data,
            right_path_data,
            bottom_half_circle_path_data
        )
    }
    return(path_data)
}

circle_polygon <- function(x, y, radius, facecolor, edgecolor, edgesize = 0) {

    main_path <- circle(x, y, radius)
    inner_path <- circle(x, y, radius - edgesize)

    main_path$subid <- 1L
    inner_path$subid <- 2L

    outer_polygon <- rbind(main_path, inner_path)
    inner_polygon <- inner_path

    outer_polygon <- rbind(main_path, inner_path)
    inner_polygon <- inner_path

    polygon <- list(
        geom_polygon(
            aes(
                x = outer_polygon$x,
                y = outer_polygon$y,
                subgroup = outer_polygon$subid
            ),
            size = 0, fill = edgecolor
        ),
        geom_polygon(
            aes(
                x = inner_polygon$x,
                y = inner_polygon$y
            ),
            size = 0, fill = facecolor
        )
    )
    return(polygon)
}

circle <- function(x, y, radius = 1, start_rad = 0, end_rad = 2 * pi) {
    tt <- seq(
        start_rad,
        end_rad,
        length.out = as.integer((end_rad - start_rad) / pi) * 100
    )
    return(
        data.frame(
            x = x + radius * cos(tt),
            y = y + radius * sin(tt)
        )
    )
}

plot <- function(patches,
                 baseline_jumps,
                 match_count,
                 config,
                 show,
                 twogoalline = FALSE,
                 output_path = NULL) {
    padding <- config[["padding"]]
    plot_width <- (match_count + 1) * config[["spacing"]] + padding
    baseline_width <- config[["thickness"]] * config[["baseline_factor"]] *
        ppi / 2
    baseline_color <- config[["baseline_color"]]

    background <- if (config[["transparent_background"]]) {
        "transparent"
    } else {
        "white"
    }

    ax <- ggplot(dpi = config[["dpi"]]) +
        coord_fixed(
            xlim = c(0, plot_width),
            ylim = c(-max_height, max_height), expand = FALSE
        ) +
        labs(x = NULL, y = NULL, title = NULL)

    baseline_endpoints <- c(0, baseline_jumps, plot_width)
    baseline_seg_count <- length(baseline_endpoints)
    ax <- ax + geom_path(
        aes(x = baseline_endpoints,
            y = rep(0, each = baseline_seg_count),
            group = rep(1:(baseline_seg_count / 2), each = 2)
        ),
        colour = do.call(
            rgb, append(baseline_color, list(maxColorValue = 255))
        ),
        size = baseline_width
    )

    if (twogoalline) {
        two_goals <- goal_to_height(2)
        twogoalline_width <- baseline_width * 0.5

        ax <- ax + geom_path(
            aes(x = c(0, plot_width), y = c(two_goals, two_goals)),
            colour = do.call(
                rgb, append(baseline_color, list(maxColorValue = 255))
            ), size = twogoalline_width
        ) + geom_path(
            aes(x = c(0, plot_width), y = c(-two_goals, -two_goals)),
            colour = do.call(
                rgb, append(baseline_color, list(maxColorValue = 255))
            ), size = twogoalline_width
        )
    }

    ax <- Reduce(`+`, append(list(ax), patches)) +
        theme_void() +
        theme(
            legend.position = "none",
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.spacing = unit(0, "mm"),
            plot.margin = unit(c(0, 0, 0, 0), "mm"),
            panel.background = element_rect(fill = background, colour = NA),
            plot.background = element_rect(fill = background, colour = NA)
        )

    if (!is.null(output_path)) {
        ggsave(
            output_path,
            plot = ax,
            height = 2 * max_height,
            width = plot_width,
            bg = background,
            dpi = config[["dpi"]]
        )
    }
    if (show) {
        print(ax)
    }

    return(ax)
}
