#' @import ggplot2

height_mapping <- c(0, 1, 1.7, 2.25, 2.65, 2.85, 2.925)
goal_to_height <- function(x) {
    if (x %in% c(0, 1, 2, 3, 4, 5, 6)) height_mapping[[x + 1]] else 3
}

default_config <- list(
    figure_height = 3.2,
    figure_width_per_match = 0.5,
    dpi = 300,
    thickness = 0.36,
    edge_thickness = 2,
    zerodot = 0.4 * 0.36,
    slant = sin(14 * pi / 180),
    spacing = 0.9,
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
#' of match scores. Each match score has the form of a list (or a data.frame
#' row) of (home team score: int, away team score: int, is away game: logical)
#' @param twogoalline A logical, draws lines for two-goal levels,
#' by default FALSE.
#' @param nozerodots A logical, removes the dots placed for zero scores,
#' by default FALSE.
#' @param outlined A logical, only plots the outlines of the sparklines,
#' by default FALSE.
#' @param show A logical, prints the plot, by default TRUE.
#' @param output_path A string, Path to save the plot at (image type
#' is inferred from the path), by default NULL.
#' @param ... A named list, additional configuration keywords for the
#' visualization. Not necessarily consistent with its latex counterpart,
#' but mostly a superset of it.
#'  - figure_height: Figure height in inches, by default 4
#'  - figure_width_per_match: Figure width per match in inches, by default 0.5
#'  - dpi: Dots per inch resolution, by default 300
#'  - thickness: Line thickness in cartesian coordinates, by default 0.18
#'  - edge_thickness: Edge thickness for outlined away games
#'      (when outlined = True), by default 3
#'  - zerodot: Zero-dot radius ratio to thickness (when nozerodots = False),
#'      by default 0.4
#'  - slant: Slope for unbalanced scores in degrees, by default 14
#'  - spacing: Spacing between matches in cartesian coordinates, by default 0.9
#'  - padding: Padding before and after the matches in cartesian coordinates,
#'      by default 0.9
#'  - baseline_factor: Thickness of baseline with respect to line thickness,
#'      by default 0.2
#'  - brighten: Brightness percentage of the two-goal lines (when
#'      twogoalline = True) and away games (when outlined = False),
#'      by default 33
#'  - transparent_background: Set the background transparent instead of white,
#'      by default False
#'  - home_color: Color of home match lines in any of the three kinds of R
#'      color specifications (i.e. either a color name as listed by colors(),
#'      a hexadecimal string of the form "#rrggbb" or "#rrggbbaa",
#'      or a positive integer i meaning palette()[i]), by default "black"
#'  - away_color: Color of away match lines in any of the three kinds of R
#'      color specifications, by default "black"
#'  - baseline_color: Color of baselines in any of the three kinds of R
#'      color specifications, by default "black"
#'  - fill_color: Fill color for the outlined sparklines in any of the three
#'      kinds of R color specifications, by default NA.
#'  - clip_slanted_lines: Clip the ends of the slanted lines, by default True
#' @return ggplot objects of the plots created.
#' @examples
#' plot_scores(list(
#'     list(1, 2, TRUE), list(3, 3, FALSE), list(0, 2, TRUE),
#'     list(0, 0, FALSE), list(6, 6, TRUE)
#' ))
#' plot_scores(list(
#'     list(1, 2, TRUE), list(3, 3, FALSE), list(0, 2, TRUE), list(0, 0, FALSE),
#'     list(6, 6, TRUE), list(0, 2, FALSE), list(0, 0, FALSE), list(6, 6, TRUE),
#'     list(6, 3, TRUE)
#' ), outlined = TRUE, output_path = "out.png")
#' plot_scores(list(
#'     list(8, 0, FALSE), list(4, 1, TRUE), list(4, 4, FALSE), list(1, 4, TRUE),
#'     list(5, 0, FALSE), list(0, 0, TRUE), list(1, 1, FALSE), list(2, 3, TRUE),
#'     list(NA, NA, FALSE), list(NA, NA, TRUE), list(NA, NA, FALSE)
#' ), home_color = "red")
#' @export
plot_scores <- function(scores,
                        twogoalline = FALSE,
                        nozerodots = FALSE,
                        outlined = FALSE,
                        show = TRUE,
                        output_path = NULL,
                        ...) {

    scores <- maybe_convert_dataframe(scores)

    check_scores(scores)

    config <- config_factory(outlined, ...)

    if (is.list(scores[[1]][[1]]) || length(scores[[1]][[1]]) > 1) {
        matchlists <- scores
    } else {
        matchlists <- list(scores)
    }

    axes <- list()
    for (matches in matchlists) {

        patches <- list()
        match_count <- length(matches)

        for (index in seq_along(matches)) {
            match <- matches[[index]]
            away_game <- as.logical(match[[3]])
            scores <- match[-3]
            scores <- if (away_game) rev(scores) else scores
            match_index <- (index - 0.5) * config[["spacing"]]

            line_colors <- colors(away_game, outlined, config)
            facecolor <- line_colors[[1]]
            edgecolor <- line_colors[[2]]

            if (is.na(scores[[1]])) {
                if (!nozerodots) {
                    patches <- append(patches,
                        circle_polygon(
                            match_index, 1,
                            radius = config[["zerodot"]],
                            facecolor = edgecolor,
                            edgecolor = edgecolor
                        )
                    )
                    patches <- append(patches,
                        circle_polygon(
                            match_index, -1,
                            radius = config[["zerodot"]],
                            facecolor = edgecolor,
                            edgecolor = edgecolor
                        )
                    )
                }
                next
            }

            if (scores[[1]] > 0 || scores[[2]] > 0) {
                slope <- config[["slant"]] * sign(scores[[1]] - scores[[2]])
                offset0 <- match_index + slope * goal_to_height(scores[[1]])
                offset1 <- match_index - slope * goal_to_height(scores[[2]])
                height0 <- goal_to_height(scores[[1]])
                height1 <- -goal_to_height(scores[[2]])
                patches <- append(patches,
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
                        linewidth = config[["edge_thickness"]]
                    ))
            }

            if (!nozerodots) {
                if (!scores[[1]]) {
                    patches <- append(patches, circle_polygon(
                        match_index, 1,
                        radius = config[["zerodot"]],
                        facecolor = edgecolor,
                        edgecolor = edgecolor))
                }
                if (!scores[[2]]) {
                    patches <- append(patches, circle_polygon(
                        match_index, -1,
                        radius = config[["zerodot"]],
                        facecolor = edgecolor,
                        edgecolor = edgecolor))
                }
            }
        }

        axes <- append(
            axes,
            plot(
                patches,
                match_count,
                config,
                show = show,
                twogoalline = twogoalline,
                output_path = output_path
            )
        )
    }

    return(if (length(axes) > 1) axes else axes[[1]])
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

check_scores <- function(scores) {

    checkmate::assert_list(scores, min.len = 1, types = c("list", "numeric"))

    if (is.list(scores[[1]][[1]]) || length(scores[[1]][[1]]) > 1) {
        first_match <- scores[[1]][[1]]
    } else {
        first_match <- scores[[1]]
    }
    checkmate::assert_vector(
        first_match,
        len = 3,
        .var.name = "Match vector"
    )
    checkmate::assert_int(first_match[[1]],
        null.ok = FALSE,
        .var.name = "Scores of the home team"
    )
    checkmate::assert_int(first_match[[2]],
        null.ok = FALSE,
        .var.name = "Scores of the away team"
    )
    checkmate::assert_logical(first_match[[3]],
        len = 1, null.ok = FALSE,
        any.missing = FALSE, .var.name = "Away flag"
    )
}

config_factory <- function(outlined, ...) {

    kwargs <- list(...)
    config <- default_config

    if (!outlined) {
        config[["edge_thickness"]] <- 0
    }

    for (key in names(kwargs)) {

        value <- kwargs[[key]]

        if (!(key %in% names(config))) {
            stop(
                sprintf(
                    strwrap(
                        "Keyword argument '%s' is not a valid configuration
                        parameter. Available configuration parameters
                        are (%s)", width = 1200
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

        if (grepl("color", key, fixed = TRUE)) {
            config[[key]] <- col2rgb(value, alpha = TRUE)
            next
        }

        config[[key]] <- value
    }

    return(config)
}

colors <- function(away_game, outlined, config) {

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

    clipped <- start_xy[[1]] != end_xy[[1]]
    thickness <- config[["thickness"]]
    edge_th <- if (facecolor != edgecolor) config[["edge_thickness"]] else 0
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
    polygon <- geom_polygon(
        aes(
            x = x, y = y,
        ), data = path_data, fill = facecolor,
        colour = edgecolor, size = edge_th
    )
    return(polygon)
}

circle_polygon <- function(x, y, radius, facecolor, edgecolor, linewidth = 0) {

    path_data <- circle(x, y, radius)
    return(
        geom_polygon(
            aes(
                x = x, y = y
            ), data = path_data, fill = facecolor,
            colour = edgecolor, size = linewidth
        )
    )
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
                 match_count,
                 config,
                 show,
                 twogoalline = FALSE,
                 output_path = NULL) {

    padding <- config[["padding"]]
    plot_width <- match_count * config[["spacing"]]
    linewidth_factor <- 72 / (plot_width + 2 * padding)
    baseline_width <- config[["thickness"]] *
        config[["baseline_factor"]] * linewidth_factor
    baseline_color <- config[["baseline_color"]]

    background <- if (config[["transparent_background"]]) "transparent"
        else "white"

    ax <- ggplot(dpi = config[["dpi"]]) +
        coord_cartesian(
            xlim = c(-padding, plot_width + padding),
            ylim = c(-3.2, 3.2), expand = FALSE, default = TRUE
        ) +
        coord_fixed() +
        labs(x = NULL, y = NULL) +
        geom_path(
            aes(x = x, y = y),
            data.frame(x = c(0, plot_width), y = c(0, 0)),
            colour = do.call(
                rgb, append(baseline_color, list(maxColorValue = 255))
            ),
            size = baseline_width,
        )

    if (twogoalline) {
        two_goals <- goal_to_height(2)
        twogoalline_color <- list(
            red = baseline_color[[1]] +
                (255 - baseline_color[[1]]) * config[["brighten"]] / 100,
            green = baseline_color[[2]] +
                (255 - baseline_color[[2]]) * config[["brighten"]] / 100,
            blue = baseline_color[[3]] +
                (255 - baseline_color[[3]]) * config[["brighten"]] / 100,
            alpha = baseline_color[[4]]
        )
        twogoalline_color <- do.call(
            rgb, append(twogoalline_color, list(maxColorValue = 255))
        )
        twogoalline_width <- baseline_width * 0.5

        ax <- ax + geom_path(
            aes(x = x, y = y),
            data.frame(x = c(0, plot_width), y = c(two_goals, two_goals)),
            colour = twogoalline_color, size = twogoalline_width
        ) + geom_path(
            aes(x = x, y = y),
            data.frame(x = c(0, plot_width), y = c(-two_goals, -two_goals)),
            colour = twogoalline_color, size = twogoalline_width
        )
    }

    ax <- Reduce(`+`, append(list(ax), patches)) +
        theme_void() +
        theme(
            legend.position = "none",
            panel.grid.major = element_blank(),
            panel.grid.minor = element_blank(),
            panel.background = element_rect(fill = background, colour = NA),
            plot.background = element_rect(fill = background, colour = NA)
        )

    if (!is.null(output_path)) {
        ggsave(output_path,
            plot = ax,
            height = config[["figure_height"]],
            width = config[["figure_width_per_match"]] * match_count,
            type = "cairo",  bg = background
        )
    }
    if (show) {
        print(ax)
    }

    return(ax)
}
