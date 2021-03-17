from typing import Dict, Union, Tuple, Iterable, List
from collections import defaultdict
import math
import numbers
import pathlib

import numpy as np
from matplotlib import pyplot as plt
from matplotlib.patches import Circle, Patch, PathPatch
from matplotlib.path import Path
from matplotlib.axes import Axes
from matplotlib.colors import to_rgba

MatchScore = Tuple[int, int, bool]
Matches = Iterable[MatchScore]
ScoreLists = Tuple[Tuple[int], Tuple[int], Tuple[bool]]

PPI = 72
MAX_HEIGHT = 3.75
GOAL_TO_HEIGHT: Dict[int, float] = defaultdict(
    lambda: MAX_HEIGHT,
    {0: 0, 1: 1, 2: 1.7, 3: 2.25, 4: 2.65, 5: 2.95, 6: 3.20, 7: 3.40, 8: 3.60},
)

DEFAULT_CONFIG = {
    "dpi": 300,
    "thickness": 0.36,
    "edge_thickness": 0.35 * 2 * 0.36,
    "goalless_edge_thickness": 0.5 * 2 * 0.36,
    "zerodot": 0.6 / 2 * 0.36,
    "slant": math.sin(math.radians(14)),
    "spacing": 0.8,
    "padding": 0.2,
    "baseline_factor": 0.2,
    "brighten": 33,
    "transparent_background": False,
    "home_color": (0, 0, 0, 1),
    "away_color": (0, 0, 0, 1),
    "baseline_color": (0, 0, 0, 1),
    "fill_color": (0, 0, 0, 0),
    "clip_slanted_lines": True,
}


def soccerbar(
    scores: Union[Matches, Iterable[Matches], ScoreLists, Iterable[ScoreLists]],
    twogoalline: bool = False,
    zerodots: bool = False,
    outlined: bool = False,
    color: list = None,
    show: bool = True,
    output_path: Union[str, List[str]] = None,
    **plot_kwargs,
) -> Union[Axes, List[Axes]]:
    """Plot Multivariate Sparklines

    Plot the given list or lists of match results as multivariate sparklines.
    This function can either show the plot, or save it to a given path.

    Parameters
    ----------
    scores : Union[Matches, Iterable[Matches], ScoreLists, Iterable[ScoreLists]]
        Matches, ScoreLists, or an iterable of those. Matches are either a pandas DataFrame
        or a list of tuples, each tuple (or row) in the form
        (home_team_score: int, away_team_score: int, is_away_game: bool), or
        (None, None, is_away_game: bool) for matches that are not played yet. ScoreLists
        are basically the same entities in a columnar format, with a tuple of
        the lists with types int, int and bool representing the same components
        as each row of Matches. Some valid examples:
            >>> [(1,4,True), (2,5,False), (3,6,True)]
            >>> [(1,2,3), (4,5,6), (True,False,True)]
            >>> pd.DataFrame([(1,4,True), (2,5,False), (3,6,True)])
            >>> [[(1,4,True), (2,5,False)], [(3,6,True), (4,7,False)]]
    twogoalline : bool, optional
        Draw lines for two-goal levels, by default False
    zerodots : bool, optional
        Mark no goals scored with a small dot, by default False
    outlined : bool, optional
        Only plot the outlines of the away games, by default False
    color : list, optional
        A list or a list of lists of valid matplotlib colors, specifying the color of each
        soccerbar. This option is provided to be consistent with LaTeX package API
        and allows high configurability, but `home_color`, `away_color` and `fill_color`
        options should already be sufficient for most of the use cases, by default None.
        This argument is expected to be structurally identical with the `scores`: if a list of
        match score lists is given, then `color` must also be a list of color lists.
    show : bool, optional
        Show the plot with pyplot.show, by default True
    output_path : Union[str, List[str]], optional
        Path to save the plot at (image type is inferred from the path), by default None.
        This argument is expected to be structurally identical with the `scores`: if a list of
        match score lists is given, then `output_path` must also be a list of strings.
    **plot_kwargs
        Additional configuration keywords for the visualization. Not necessarily
        consistent with its latex counterpart, but mostly a superset of it.
            - dpi: Dots per inch resolution, by default 300
            - thickness: Line thickness in cartesian coordinates, by default 0.18
            - edge_thickness: Edge thickness for outlined patches (when outlined=True) as the ratio to the
            line thickness, by default 0.35
            - goalless_edge_thickness: Edge thickness for outlined no-goal patches (when outlined=True) as
            the ratio to the line thickness, by default 0.5
            - zerodot: Zero-dot diameter ratio to thickness (when zerodots=True), by default 0.6
            - slant: Slope for unbalanced scores in degrees, by default 14
            - spacing: Spacing between matches in cartesian coordinates, by default 0.8
            - padding: Padding before and after the matches in cartesian coordinates, by default 0.2
            - baseline_factor: Thickness of baseline with respect to line thickness, by default 0.2
            - brighten: Brightness percentage of the away games (when outlined=False), by default 33
            - transparent_background: Set the background transparent instead of white, by default False
            - home_color: Color of home match lines in matplotlib-acceptable formats, by default rgba(0,0,0,1)
            - away_color: Color of away match lines in matplotlib-acceptable formats, by default rgba(0,0,0,1)
            - baseline_color: Color of baselines in matplotlib-acceptable formats, by default rgba(0,0,0,1)
            - fill_color: Fill color for the outlined sparklines, by default rgba(0,0,0,0).
            - clip_slanted_lines: Clip the ends of the slanted lines, by default True

    Returns
    -------
    Union[Axes, List[Axes]]
        Axes objects of the plot(s) created. If the input is an iterable of multiple matches,
        then the return is an iterable of Axes objects, one per each match score list.

    Examples
    --------
    >>> soccerbar([
            (1,2,True), (3,3,False), (0,2,True), (0,0,False), (6,6,True)
        ])
    >>> soccerbar([
            (1,2,True), (3,3,False), (0,2,True), (0,0,False), (6,6,True),
            (0,2,False), (0,0,False), (6,6,True), (6,3,True)
        ], outlined=True, output_path = "out.png")
    """

    scores = _maybe_convert_dataframe(scores)

    scores = _maybe_flatten_vectors(scores)

    _check_scores(scores)
    _check_color_and_output_path(color, output_path, scores)

    config = _config_factory(outlined, **plot_kwargs)
    is_multiple = _is_listlike(scores[0][0])

    if is_multiple:
        matchlists = scores
        matchcolors = color
        outpaths = output_path
    else:
        matchlists = [scores]
        matchcolors = [color]
        outpaths = [output_path]

    axes: List[Axes] = []
    for matches_index, matches in enumerate(matchlists):

        patches = []
        baseline_jumps = []
        colors = matchcolors[matches_index] if color is not None else None
        outpath = outpaths[matches_index] if output_path is not None else None
        match_count = len(matches)

        for index, match in enumerate(matches):

            away_game, scores = match[2], match[:2]
            scores = scores[::-1] if away_game else scores
            match_index = (index + 1) * config["spacing"]
            matchcolor = colors[index] if color is not None else None
            facecolor, edgecolor = _colors(away_game, outlined, config, matchcolor)

            if scores[0] is None or np.isnan(scores[0]):
                patches.append(
                    Circle(
                        (match_index, 0),
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
                        linewidth=config["goalless_edge_thickness"] * PPI,
                    )
                )

            if scores[0] and scores[1]:
                baseline_jumps.extend(
                    [
                        match_index - config["thickness"] * 0.95 / 2,
                        match_index + config["thickness"] * 0.95 / 2,
                    ]
                )
            elif not (scores[0] or scores[1]):
                baseline_jumps.extend(
                    [
                        match_index - config["thickness"] * 0.95,
                        match_index + config["thickness"] * 0.95,
                    ]
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
                baseline_jumps,
                match_count,
                config,
                show=show,
                twogoalline=twogoalline,
                output_path=outpath,
            )
        )

    return axes if is_multiple else axes[0]


def _is_listlike(val, ensure_nonempty=False, ensure_type: str = None):
    is_listlike = hasattr(val, "__iter__") and not isinstance(val, str)
    if not is_listlike:
        return False
    elif ensure_nonempty and not val:
        return False
    elif ensure_type == "integerish" and not all(_is_integerish(item) for item in val):
        return False
    elif ensure_type == "bool" and not all(isinstance(item, bool) for item in val):
        return False
    return True


def _maybe_convert_dataframe(scores):

    if type(scores).__name__ == "DataFrame":
        scores = scores.values.tolist()

    if _is_listlike(scores):
        for index, item in enumerate(scores):
            if type(item).__name__ == "DataFrame":
                scores[index] = item.values.tolist()

    return scores


def _is_integerish(val, allow_nan=True):
    if isinstance(val, int):
        return True
    else:
        if allow_nan and (
            val is None or (isinstance(val, numbers.Number) and math.isnan(val))
        ):
            return True
        elif isinstance(val, float) and val.is_integer():
            return True
        else:
            return False


def _maybe_flatten_vectors(scores):

    if _is_listlike(scores):

        if (
            len(scores) == 3
            and _is_listlike(scores[0], ensure_nonempty=True, ensure_type="integerish")
            and _is_listlike(scores[1], ensure_nonempty=True, ensure_type="integerish")
            and _is_listlike(scores[2], ensure_nonempty=True, ensure_type="bool")
            and len(scores[0]) == len(scores[1]) == len(scores[2])
        ):
            scores = list(zip(*scores))
        else:
            for index, item in enumerate(scores):
                if (
                    len(item) == 3
                    and _is_listlike(
                        item[0], ensure_nonempty=True, ensure_type="integerish"
                    )
                    and _is_listlike(
                        item[1], ensure_nonempty=True, ensure_type="integerish"
                    )
                    and _is_listlike(item[2], ensure_nonempty=True, ensure_type="bool")
                    and len(item[0]) == len(item[1]) == len(item[2])
                ):
                    scores[index] = list(zip(*item))

    return scores


def _check_scores(scores) -> None:

    if not _is_listlike(scores):
        raise TypeError(f"'scores' must be an iterable, not {type(scores)}")

    if not scores:
        raise ValueError(f"'scores' cannot be empty")

    is_listofmatchlists = _is_listlike(
        scores[0], ensure_nonempty=True
    ) and _is_listlike(scores[0][0])
    if not is_listofmatchlists:
        scores = [scores]

    for item in scores:

        is_possibly_vectorlist = len(item) == 3 and all(
            len(elem) != 3 or not isinstance(elem[-1], bool) for elem in item[:-1]
        )

        if is_possibly_vectorlist:
            first_elem_length = len(item[0])
            if not all(_is_integerish(elem) for elem in item[0]):
                raise TypeError(
                    f"In columnwise match inputs, all values in the home scores column is expected "
                    f"to have type int, not {[type(elem).__name__ for elem in item[0]]}"
                )
            if not all(_is_integerish(elem) for elem in item[1]):
                raise TypeError(
                    f"In columnwise match inputs, all values in the away scores column is expected "
                    f"to have type int, not {[type(elem).__name__ for elem in item[1]]}"
                )
            if not len(item[1]) == first_elem_length:
                raise ValueError(
                    f"In columnwise match inputs, all columns must have equal lengths, "
                    f"but away scores column had length {len(item[1])} instead of {first_elem_length}"
                )
            if not all(isinstance(elem, bool) for elem in item[2]):
                raise TypeError(
                    f"In columnwise match inputs, all values in the away flags column is expected "
                    f"to have type bool, not {[type(elem).__name__ for elem in item[2]]}"
                )
            if not len(item[2]) == first_elem_length:
                raise ValueError(
                    f"In columnwise match inputs, all columns must have equal lengths, "
                    f"but away flags column had length {len(item[2])} instead of {first_elem_length}"
                )

        else:
            first_match = item[0]
            elem1, elem2, elem3 = first_match

            if not (
                _is_integerish(elem1)
                and _is_integerish(elem2)
                and isinstance(elem3, bool)
            ):
                raise TypeError(
                    f"A game in 'scores' must be represented with Tuple[int, int, bool], "
                    f"not Tuple[{type(elem1).__name__}, {type(elem2).__name__}, {type(elem3).__name__}]"
                )


def _check_color_and_output_path(color, output_path, scores) -> None:

    if _is_listlike(scores[0][0]):
        matchlists = scores
        matchcolors = color
        outpaths = output_path
    else:
        matchlists = [scores]
        matchcolors = [color]
        outpaths = [output_path]

    if output_path is not None:
        if not _is_listlike(outpaths):
            raise ValueError(
                f"Output paths need to have the same structure with the scores. "
                f"If a list of score sets are given, the paths also must be an "
                f"iterable of paths, not {type(outpaths)}."
            )
        if len(outpaths) != len(matchlists):
            raise ValueError(
                f"One output path needs to be specified per match score set. "
                f"Currently {len(outpaths)} output paths are specified while "
                f"there are {len(matchlists)} set(s) of match scores."
            )
        if not all(
            (isinstance(p, str) or isinstance(p, pathlib.Path)) for p in outpaths
        ):
            raise TypeError(
                f"Output paths need to be string or of type pathlib.Path, not "
                f"{[type(p) for p in outpaths]}."
            )

    if color is None:
        return

    for matches, colors in zip(matchlists, matchcolors):
        if len(matches) != len(colors):
            raise ValueError(
                f"Length of matches {len(matches)} is not consistent with the length "
                f"of colors {len(colors)}. Beware that if the scores you have provided are "
                f"nested (i.e. includes multiple lists of matches), the colors must be nested too."
            )
        for color in colors:
            try:
                to_rgba(color)
            except ValueError:
                raise ValueError(
                    f"'{color}' is not a valid matplotlib color. For more information on "
                    f"valid matplotlib colors, see "
                    f"https://matplotlib.org/stable/tutorials/colors/colors.html"
                )


def _config_factory(outlined, **kwargs):

    config = DEFAULT_CONFIG.copy()

    if not outlined:
        config["edge_thickness"] = 0
        config["goalless_edge_thickness"] = 0

    for key, value in kwargs.items():

        if key not in config:
            raise KeyError(
                f"Keyword argument '{key}' is not a valid configuration parameter. Available configuration parameters are {list(config.keys())}"
            )

        if key == "slant":
            config[key] = math.sin(math.radians(value))
            continue

        if key == "zerodot":
            config[key] = value / 2 * kwargs.get("thickness", config["thickness"])
            continue

        if "edge_thickness" in key and outlined:
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
    edge_thickness = config["edge_thickness"] * PPI
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
    baseline_jumps: List[float],
    match_count,
    config,
    show=True,
    twogoalline=False,
    output_path: str = None,
) -> Axes:

    padding = config["padding"]
    plot_width = padding + (match_count + 1) * config["spacing"] + padding

    fig = plt.figure(
        figsize=(
            plot_width,
            2 * MAX_HEIGHT,
        ),
        dpi=config["dpi"],
    )
    ax: Axes = plt.axes([0, 0, 1, 1], frameon=False)

    ax.grid(False)
    ax.get_xaxis().set_visible(False)
    ax.get_yaxis().set_visible(False)
    ax.set_aspect("equal")
    ax.autoscale(tight=True)
    ax.set_xlim(-padding, plot_width - padding)
    ax.set_ylim(-MAX_HEIGHT, MAX_HEIGHT)

    baseline_width = config["thickness"] * config["baseline_factor"] * PPI
    baseline_color = config["baseline_color"]

    baseline_endpoints = [-padding] + baseline_jumps + [plot_width - padding]
    baseline_segments = zip(baseline_endpoints[::2], baseline_endpoints[1::2])
    for x1, x2 in baseline_segments:
        ax.plot(
            [x1, x2],
            [0, 0],
            lw=baseline_width,
            color=baseline_color,
            zorder=-1,
            solid_capstyle="butt",
        )

    if twogoalline:
        two_goals = GOAL_TO_HEIGHT[2]
        twogoalline_width = baseline_width * 0.5
        ax.plot(
            [-padding, plot_width - padding],
            [two_goals, two_goals],
            lw=twogoalline_width,
            color=baseline_color,
            zorder=-1,
        )
        ax.plot(
            [-padding, plot_width - padding],
            [-two_goals, -two_goals],
            lw=twogoalline_width,
            color=baseline_color,
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
