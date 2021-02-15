from typing import Dict, Union, Tuple, Iterable, List
from collections import defaultdict
import math

import numpy as np
from matplotlib import pyplot as plt
from matplotlib.patches import Circle, Patch, PathPatch
from matplotlib.path import Path
from matplotlib.axes import Axes
from matplotlib.colors import to_rgba

MatchScore = Tuple[int, int, bool]
Matches = Iterable[MatchScore]

GOAL_TO_HEIGHT: Dict[int, float] = defaultdict(
    lambda: 3.75,
    {0: 0, 1: 1, 2: 1.7, 3: 2.25, 4: 2.65, 5: 2.95, 6: 3.20, 7: 3.40, 8: 3.60},
)

DEFAULT_CONFIG = {
    "figure_height": 4,
    "figure_width_per_match": 0.5,
    "dpi": 300,
    "thickness": 0.36,
    "edge_thickness": 10,
    "zerodot": 0.4 * 0.36,
    "slant": math.sin(math.radians(14)),
    "spacing": 0.9,
    "padding": 0.25,
    "baseline_factor": 0.2,
    "brighten": 33,
    "transparent_background": False,
    "home_color": (0, 0, 0, 1),
    "away_color": (0, 0, 0, 1),
    "baseline_color": (0, 0, 0, 1),
    "fill_color": (0, 0, 0, 0),
    "clip_slanted_lines": True,
}


def scorebar(
    scores: Union[Matches, Iterable[Matches]],
    twogoalline: bool = False,
    zerodots: bool = False,
    outlined: bool = False,
    color: list = None,
    show: bool = True,
    output_path: str = None,
    **plot_kwargs,
) -> Union[Axes, List[Axes]]:
    """Plot Multivariate Sparklines

    Plot the given list or lists of match results as multivariate sparklines.
    This function can either show the plot, or save it to a given path.

    Parameters
    ----------
    scores : Union[Matches, Iterable[Matches]]
        Matches, or an iterable of matches. Matches are either a pandas DataFrame
        or a list of tuples, each tuple (or row) in the form
        (home_team_score: int, away_team_score: int, is_away_game: bool), or
        (None, None, is_away_game: bool) for matches that are not played yet.
    twogoalline : bool, optional
        Draw lines for two-goal levels, by default False
    zerodots : bool, optional
        Mark no goals scored with a small dot, by default False
    outlined : bool, optional
        Only plot the outlines of the away games, by default False
    color : list, optional
        A list or a list of lists of valid matplotlib colors, specifying the color of each
        scorebar. This option is provided to be consistent with LaTeX package API
        and allows high configurability, but `home_color`, `away_color` and `fill_color`
        options should already be sufficient for most of the use cases, by default None.
    show : bool, optional
        Show the plot with pyplot.show, by default True
    output_path : str, optional
        Path to save the plot at (image type is inferred from the path), by default None
    **plot_kwargs
        Additional configuration keywords for the visualization. Not necessarily
        consistent with its latex counterpart, but mostly a superset of it.
            - figure_height: Figure height in inches, by default 4
            - figure_width_per_match: Figure width per match in inches, by default 0.5
            - dpi: Dots per inch resolution, by default 300
            - thickness: Line thickness in cartesian coordinates, by default 0.18
            - edge_thickness: Edge thickness for outlined patches (when outlined=True), by default 3
            - zerodot: Zero-dot radius ratio to thickness (when zerodots=True), by default 0.4
            - slant: Slope for unbalanced scores in degrees, by default 14
            - spacing: Spacing between matches in cartesian coordinates, by default 0.9
            - padding: Padding before and after the matches in cartesian coordinates, by default 0.9
            - baseline_factor: Thickness of baseline with respect to line thickness, by default 0.2
            - brighten: Brightness percentage of the two-goal lines (when twogoalline=True)
                and away games (when outlined=False), by default 33
            - transparent_background: Set the background transparent instead of white, by default False
            - home_color: Color of home match lines in matplotlib-acceptable formats, by default rgba(0,0,0,1)
            - away_color: Color of away match lines in matplotlib-acceptable formats, by default rgba(0,0,0,1)
            - baseline_color: Color of baselines in matplotlib-acceptable formats, by default rgba(0,0,0,1)
            - fill_color: Fill color for the outlined sparklines, by default rgba(0,0,0,0).
            - clip_slanted_lines: Clip the ends of the slanted lines, by default True

    Returns
    -------
    Union[Axes, List[Axes]]
        Axes objects of the plot(s) created.

    Examples
    --------
    >>> scorebar([
            (1,2,True), (3,3,False), (0,2,True), (0,0,False), (6,6,True)
        ])
    >>> scorebar([
            (1,2,True), (3,3,False), (0,2,True), (0,0,False), (6,6,True),
            (0,2,False), (0,0,False), (6,6,True), (6,3,True)
        ], outlined=True, output_path = "out.png")
    """

    scores = _maybe_convert_dataframe(scores)

    _check_scores(scores)
    _check_color(color, scores)

    config = _config_factory(outlined, **plot_kwargs)

    if isinstance(scores[0][0], int):
        matchlists = [scores]
        matchcolors = [color]
    else:
        matchlists = scores
        matchcolors = color

    axes: List[Axes] = []
    for matches_index, matches in enumerate(matchlists):

        patches = []
        colors = matchcolors[matches_index] if color is not None else None
        match_count = len(matches)

        for index, match in enumerate(matches):

            away_game, scores = match[2], match[:2]
            scores = scores[::-1] if away_game else scores
            match_index = (index + 0.5) * config["spacing"]
            matchcolor = colors[index] if color is not None else None
            facecolor, edgecolor = _colors(away_game, outlined, config, matchcolor)

            if scores[0] is None or np.isnan(scores[0]):
                patches.append(
                    Circle(
                        (match_index, 1 - config["zerodot"]),
                        radius=config["zerodot"],
                        facecolor=edgecolor,
                        edgecolor=edgecolor,
                    )
                )
                patches.append(
                    Circle(
                        (match_index, -1 + config["zerodot"]),
                        radius=config["zerodot"],
                        facecolor=edgecolor,
                        edgecolor=edgecolor,
                    )
                )
                continue

            if scores[0] or scores[1]:
                slope = config["slant"] * np.sign(scores[0] - scores[1])
                offset0 = match_index + slope * GOAL_TO_HEIGHT[scores[0]]
                offset1 = match_index - slope * GOAL_TO_HEIGHT[scores[1]]
                height0 = GOAL_TO_HEIGHT[scores[0]]
                height1 = -GOAL_TO_HEIGHT[scores[1]]
                patches.append(
                    _line(
                        (offset1, height1),
                        (offset0, height0),
                        facecolor,
                        edgecolor,
                        config,
                    )
                )
            else:
                patches.append(
                    Circle(
                        (match_index, 0),
                        radius=config["thickness"],
                        facecolor=facecolor,
                        edgecolor=edgecolor,
                        linewidth=config["edge_thickness"],
                    )
                )

            if zerodots:
                if not scores[0]:
                    patches.append(
                        Circle(
                            (match_index, 1 - config["zerodot"]),
                            radius=config["zerodot"],
                            facecolor=edgecolor,
                            edgecolor=edgecolor,
                        )
                    )
                if not scores[1]:
                    patches.append(
                        Circle(
                            (match_index, -1 + config["zerodot"]),
                            radius=config["zerodot"],
                            facecolor=edgecolor,
                            edgecolor=edgecolor,
                        )
                    )

        axes.append(
            _plot(
                patches,
                match_count,
                config,
                show=show,
                twogoalline=twogoalline,
                output_path=output_path,
            )
        )

    return axes if len(axes) > 1 else axes[0]


def _maybe_convert_dataframe(scores):

    if type(scores).__name__ == "DataFrame":
        scores = scores.values.tolist()

    if hasattr(scores, "__iter__"):
        for index, item in enumerate(scores):
            if type(item).__name__ == "DataFrame":
                scores[index] = item.values.tolist()

    return scores


def _check_scores(scores) -> None:

    if not hasattr(scores, "__iter__"):
        raise TypeError(f"'scores' must be an iterable, not {type(scores)}")

    if not scores:
        raise ValueError(f"'scores' cannot be empty")

    first_match = scores[0] if not hasattr(scores[0][0], "__iter__") else scores[0][0]
    elem1, elem2, elem3 = first_match

    if not (
        isinstance(elem1, int) and isinstance(elem2, int) and isinstance(elem3, bool)
    ):
        raise TypeError(
            f"A game in 'scores' must be represented with Tuple[int, int, bool], "
            f"not Tuple[{type(elem1).__name__}, { type(elem2).__name__}, {type(elem3).__name__}]"
        )


def _check_color(color, scores) -> None:

    if color is None:
        return

    if isinstance(scores[0][0], int):
        matchlists = [scores]
        matchcolors = [color]
    else:
        matchlists = scores
        matchcolors = color
    for matches, colors in zip(matchlists, matchcolors):
        if len(matches) != len(colors):
            raise ValueError(
                f"Length of matches {len(matches)} is not consistent with the length "
                f"of colors {len(colors)}"
            )


def _config_factory(outlined, **kwargs):

    config = DEFAULT_CONFIG.copy()

    if not outlined:
        config["edge_thickness"] = 0

    for key, value in kwargs.items():

        if key not in config:
            raise KeyError(
                f"Keyword argument '{key}' is not a valid configuration parameter. Available configuration parameters are {list(config.keys())}"
            )

        if key == "slant":
            config[key] = math.sin(math.radians(value))
            continue

        if key == "zerodot":
            config[key] = value * kwargs.get("thickness", config["thickness"])
            continue

        if "color" in key:
            config[key] = to_rgba(value)
            continue

        config[key] = value

    return config


def _colors(away_game, outlined, config, matchcolor=None):

    if matchcolor is not None:
        main_color = to_rgba(matchcolor)
    else:
        main_color = config["away_color"] if away_game else config["home_color"]

        if away_game and config["brighten"] != 0 and not outlined:
            main_color = (
                main_color[0] + (1 - main_color[0]) * config["brighten"] / 100,
                main_color[1] + (1 - main_color[1]) * config["brighten"] / 100,
                main_color[2] + (1 - main_color[2]) * config["brighten"] / 100,
                main_color[3],
            )

    facecolor = config["fill_color"] if away_game and outlined else main_color
    edgecolor = main_color

    return facecolor, edgecolor


def _line(start_xy, end_xy, facecolor, edgecolor, config):

    clipped = start_xy[0] != end_xy[0]
    thickness = config["thickness"]
    edge_thickness = config["edge_thickness"]
    half_th = thickness / 2

    if clipped and config["clip_slanted_lines"]:
        path_data = [
            (Path.MOVETO, (start_xy[0] - half_th, start_xy[1])),
            (Path.LINETO, (start_xy[0] + half_th, start_xy[1])),
            (Path.LINETO, (end_xy[0] + half_th, end_xy[1])),
            (Path.LINETO, (end_xy[0] - half_th, end_xy[1])),
            (Path.CLOSEPOLY, (None, None)),
        ]
    else:
        path_data = [
            (Path.MOVETO, (start_xy[0] - half_th, start_xy[1] + half_th)),
            (Path.CURVE4, (start_xy[0] - half_th, start_xy[1] - 3 / 8 * half_th)),
            (Path.CURVE4, (start_xy[0] + half_th, start_xy[1] - 3 / 8 * half_th)),
            (Path.CURVE4, (start_xy[0] + half_th, start_xy[1] + half_th)),
            (Path.LINETO, (end_xy[0] + half_th, end_xy[1] - half_th)),
            (Path.CURVE4, (end_xy[0] + half_th, end_xy[1] + 3 / 8 * half_th)),
            (Path.CURVE4, (end_xy[0] - half_th, end_xy[1] + 3 / 8 * half_th)),
            (Path.CURVE4, (end_xy[0] - half_th, end_xy[1] - half_th)),
            (Path.CLOSEPOLY, (None, None)),
        ]
    codes, verts = zip(*path_data)
    path = Path(verts, codes)
    return PathPatch(
        path, facecolor=facecolor, edgecolor=edgecolor, linewidth=edge_thickness
    )


def _plot(
    patches: List[Patch],
    match_count,
    config,
    show=True,
    twogoalline=False,
    output_path: str = None,
) -> Axes:

    padding = config["padding"]
    plot_width = match_count * config["spacing"]

    fig = plt.figure(
        figsize=(
            config["figure_width_per_match"] * match_count,
            config["figure_height"],
        ),
        dpi=config["dpi"],
    )
    ax: Axes = plt.axes([0, 0, 1, 1], frameon=False)

    ax.grid(False)
    ax.get_xaxis().set_visible(False)
    ax.get_yaxis().set_visible(False)
    ax.set_aspect("equal")
    ax.autoscale(tight=True)
    ax.set_xlim(-padding, plot_width + padding)
    ax.set_ylim(-3.2, 3.2)

    linewidth_factor = (
        fig.bbox_inches.height
        * ax.get_position().height
        * 72
        / np.diff(ax.get_ylim())[0]
    )
    baseline_width = config["thickness"] * config["baseline_factor"] * linewidth_factor
    baseline_color = config["baseline_color"]

    ax.plot([0, plot_width], [0, 0], lw=baseline_width, color=baseline_color, zorder=-1)

    if twogoalline:
        two_goals = GOAL_TO_HEIGHT[2]
        twogoalline_color = (
            baseline_color[0] * (1 - baseline_color[0]) * config["brighten"] / 100,
            baseline_color[1] * (1 - baseline_color[1]) * config["brighten"] / 100,
            baseline_color[2] * (1 - baseline_color[2]) * config["brighten"] / 100,
            baseline_color[3],
        )
        twogoalline_width = baseline_width * 0.5
        ax.plot(
            [0, plot_width],
            [two_goals, two_goals],
            lw=twogoalline_width,
            color=twogoalline_color,
            zorder=-1,
        )
        ax.plot(
            [0, plot_width],
            [-two_goals, -two_goals],
            lw=twogoalline_width,
            color=twogoalline_color,
            zorder=-1,
        )

    for patch in patches:
        ax.add_patch(patch)
        patch.set_clip_path(patch)

    if output_path:
        plt.savefig(
            fname=output_path,
            bbox_inches="tight",
            pad_inches=0,
            transparent=config["transparent_background"],
        )
    if show:
        plt.show()

    return ax
