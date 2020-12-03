#' @import ggplot2

height_mapping <- c(0, 1, 1.7, 2.25, 2.65, 2.85, 2.925)
goal_to_height <- function(x) {
    if (x %in% c(0, 1, 2, 3, 4, 5, 6)) height_mapping[[x + 1]] else 3
}

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
#' @return ggplot objects of the plots created.
#' @examples
#' plot_scores(list(
#'      list(1,2,T), list(3,3,F), list(0,2,T), list(0,0,F), list(6,6,T)
#' ))
#' plot_scores(list(
#'      list(1,2,T), list(3,3,F), list(0,2,T), list(0,0,F), list(6,6,T),
#'      list(0,2,F), list(0,0,F), list(6,6,T), list(6,3,T)
#' ), outlined=T, output_path = "out.png")
#' @export
plot_scores <- function(scores,
                        twogoalline = FALSE,
                        nozerodots = FALSE,
                        outlined = FALSE,
                        show = TRUE,
                        output_path = NULL) {

    scores <- maybe_convert_dataframe(scores)

    check_scores(scores)

    if (is.list(scores[[1]][[1]]) || length(scores[[1]][[1]]) > 1) {
        matchlists <- scores
    } else {
        matchlists <- list(scores)
    }

    axes <- list()
    for (matches in matchlists) {
        lines <- list()
        match_count <- length(matches)
        lines <- append(lines, list(c(0, 0, match_count, 0)))

        if (twogoalline) {
            two_goals <- goal_to_height(2)
            lines <- append(lines, list(c(0, two_goals,
                                          match_count, two_goals)))
            lines <- append(lines, list(c(0, -two_goals,
                                          match_count, -two_goals)))
        }

        for (index in seq_along(matches)) {
            match <- matches[[index]]
            away_game <- match[[3]]
            scores <- match[-3]
            scores <- if (away_game) rev(scores) else scores
            match_index <- index - 0.5

            if (scores[[1]] > 0 || scores[[2]] > 0) {
                slope <- 0.1 * sign(scores[[1]] - scores[[2]])
                offset0 <- match_index + slope * goal_to_height(scores[[1]])
                offset1 <- match_index - slope * goal_to_height(scores[[2]])
                height0 <- goal_to_height(scores[[1]])
                height1 <- -goal_to_height(scores[[2]])
                lines <- append(lines, list(c(offset1, height1,
                                              offset0, height0)))
            } else {
                lines <- append(lines, circle(match_index, 0))
            }

            if (!nozerodots) {
                if (!scores[[1]]) {
                    lines <- append(lines, dot(match_index, 1))
                }
                if (!scores[[2]]) {
                    lines <- append(lines, dot(match_index, -1))
                }
            }
        }

        axes <- append(
            axes,
            plot(
                lines,
                match_count,
                show = show,
                outlined = outlined,
                twogoalline = twogoalline,
                output_path = output_path
            )
        )
    }

    return(if (length(axes) > 1) axes else axes[[0]])
}

maybe_convert_dataframe <- function(scores) {
    if (is.data.frame(scores)) {
        scores <- unname(do.call(list, data.frame(t(scores))))
    }

    if (is.list(scores)) {
        for (index in seq_along(scores)) {
            if (is.data.frame(scores[[index]])) {
                scores[[index]] <- unname(do.call(list, data.frame(t(scores))))
            }
        }
    }
    return(scores)
}

check_scores <- function(scores) {
    checkmate::assert_list(scores, min.len = 1, types = c("list"))

    if (is.list(scores[[1]][[1]]) || length(scores[[1]][[1]]) > 1) {
        first_match <- scores[[1]][[1]]
    } else {
        first_match <- scores[[1]]
    }
    checkmate::assert_list(first_match, min.len = 1, .var.name = "match vector")
    checkmate::assert_int(first_match[[1]], null.ok = FALSE,
                          .var.name = "Scores of the home team")
    checkmate::assert_int(first_match[[2]], null.ok = FALSE,
                          .var.name = "Scores of the away team")
    checkmate::assert_logical(first_match[[3]], len = 1, null.ok = FALSE,
                              any.missing = FALSE, .var.name = "Away flag")
}

dot <- function(x, y) circle(x, y, radius = 0.1)

circle <- function(x, y, radius = 0.25) {
    tt <- seq(0, 2 * pi, length.out = 20)
    xx <- x + radius * cos(tt)
    yy <- y + radius * sin(tt)
    x_start <- xx[1:(length(xx) - 1)]
    y_start <- yy[1:(length(yy) - 1)]
    x_end <- xx[2:length(xx)]
    y_end <- yy[2:length(yy)]
    line_frame <- data.frame(x_start, y_start, x_end, y_end)
    listified <- unname(do.call(list, data.frame(t(line_frame))))
    return(listified)
}

plot <- function(lines,
                 match_count,
                 show,
                 outlined = FALSE,
                 twogoalline = FALSE,
                 output_path = NULL) {
    h_line_count <- if (twogoalline) 3 else 1

    lines_dataframe <- as.data.frame(do.call(rbind, lines))
    linewidths <- c(rep_len(c(0.2), h_line_count),
                    rep_len(c(5), length(lines) - h_line_count))

    ax <- ggplot(lines_dataframe, dpi = 300) +
        aes(x = V1, y = V2, xend = V3, yend = V4) +
        coord_fixed() +
        geom_segment(size = linewidths, lineend = "round") +
        xlim(-0.5, match_count + 0.5) + ylim(-3.2, 3.2) +
        theme_void() +
        labs(x = NULL, y = NULL)

    if (outlined) {
        outline_linewidths <- c(rep_len(c(0.2), h_line_count),
                              rep_len(c(2.5), length(lines) - h_line_count))
        outline_colors <- c(rep_len(c("black"), h_line_count),
                          rep_len(c("white"), length(lines) - h_line_count))
        ax <- ax + geom_segment(size = outline_linewidths,
                                         lineend = "round",
                                         color = outline_colors)
    }

    if (!is.null(output_path)) {
        ggsave(output_path, plot = ax,
                        height = 4, width = 0.5 * match_count,
                        type = "cairo")
    }
    if (show) {
        print(ax)
    }

    return(ax)
}