from typing import Dict, Union, Tuple, Iterable, List
from collections import defaultdict

from matplotlib.collections import LineCollection
from matplotlib import pyplot as plt
from matplotlib.patches import CirclePolygon
from matplotlib.axes import Axes

MatchScore = Tuple[int, int, bool]
Matches = Iterable[MatchScore]

GOAL_TO_HEIGHT: Dict[int, float] = defaultdict(
    lambda: 3, {0: 0, 1: 1, 2: 1.7, 3: 2.25, 4: 2.65, 5: 2.85, 6: 2.925}
)


def plot_scores(
    scores: Union[Matches, Iterable[Matches]],
    twogoalline: bool = False,
    nozerodots: bool = False,
    outlined: bool = False,
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
        (home_team_score: int, away_team_score: int, is_away_game: bool)
    twogoalline : bool, optional
        Draw lines for two-goal levels, by default False
    nozerodots : bool, optional
        Remove the dots placed for zero scores, by default False
    outlined : bool, optional
        Only plot the outlines of the sparklines, by default False
    show : bool, optional
        Show the plot with pyplot.show, by default True
    output_path : str, optional
        Path to save the plot at (image type is inferred from the path), by default None

    Returns
    -------
    Union[Axes, List[Axes]]
        Axes objects of the plot(s) created.

    Examples
    --------
    >>> plot_scores([
            (1,2,True), (3,3,False), (0,2,True), (0,0,False), (6,6,True)
        ])
    >>> plot_scores([
            (1,2,True), (3,3,False), (0,2,True), (0,0,False), (6,6,True),
            (0,2,False), (0,0,False), (6,6,True), (6,3,True)
        ], outlined=True, output_path = "out.png")
    """

    scores = _maybe_convert_dataframe(scores)

    _check_scores(scores)

    if isinstance(scores[0][0], int):
        matchlists = [scores]
    else:
        matchlists = scores

    axes: List[Axes] = []
    for matches in matchlists:

        lines = []
        match_count = len(matches)
        lines.append([(0, 0), (match_count, 0)])

        if twogoalline:
            two_goals = GOAL_TO_HEIGHT[2]
            lines.append([(0, two_goals), (match_count, two_goals)])
            lines.append([(0, -two_goals), (match_count, -two_goals)])

        for index, match in enumerate(matches):

            away_game, scores = match[2], match[:2]
            scores = scores[::-1] if away_game else scores
            match_index = index + 0.5

            if scores[0] or scores[1]:
                slope = 0.1 * _sign(scores[0] - scores[1])
                offset0 = match_index + slope * GOAL_TO_HEIGHT[scores[0]]
                offset1 = match_index - slope * GOAL_TO_HEIGHT[scores[1]]
                height0 = GOAL_TO_HEIGHT[scores[0]]
                height1 = -GOAL_TO_HEIGHT[scores[1]]
                lines.append([(offset1, height1), (offset0, height0)])

            else:
                lines.extend(_circle(match_index, 0))

            if not nozerodots:
                if not scores[0]:
                    lines.extend(_dot(match_index, 1))
                if not scores[1]:
                    lines.extend(_dot(match_index, -1))

        axes.append(
            _plot(
                lines,
                match_count,
                show=show,
                outlined=outlined,
                twogoalline=twogoalline,
                output_path=output_path,
            )
        )

    return axes if len(axes) > 1 else axes[0]


def _maybe_convert_dataframe(scores):

    if type(scores).__name__ == "DataFrame":
        scores = scores.values

    if hasattr(scores, "__iter__"):
        for index, item in enumerate(scores):
            if type(item).__name__ == "DataFrame":
                scores[index] = item.values

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
            f"A game in 'scores' must be represented with Tuple[int, int, bool], not Tuple[{type(elem1).__name__}, { type(elem2).__name__}, {type(elem3).__name__}]"
        )


_sign: int = lambda x: x and [-1, 1][x > 0]


def _dot(x, y) -> List[Tuple]:
    return _circle(x, y, radius=0.1)


def _circle(x, y, radius=0.25) -> List[Tuple]:
    no_goal_circle = CirclePolygon((x, y), radius=radius)
    verts = no_goal_circle.get_verts()
    edges = [(elem, verts[ind - 1]) for ind, elem in enumerate(verts)]
    return edges


def _plot(
    lines: List[List[Tuple[int, int]]],
    match_count,
    show,
    outlined=False,
    twogoalline=False,
    output_path: str = None,
) -> Axes:

    h_line_count = 3 if twogoalline else 1

    _, ax = plt.subplots(figsize=(0.5 * match_count, 4))
    ax: Axes
    line_segments = LineCollection(
        lines,
        linewidths=[0.2] * h_line_count + [5] * (len(lines) - h_line_count),
        capstyle="round",
        color="black",
    )
    ax.add_collection(line_segments)

    if outlined:
        white_line_segments = LineCollection(
            lines[h_line_count:],
            linewidths=[2.5] * (len(lines) - 1),
            capstyle="round",
            color="white",
        )
        ax.add_collection(white_line_segments)

    ax.autoscale()
    ax.grid(False)
    ax.set_xticks([])
    ax.set_yticks([])
    ax.set_xlim(-0.5, match_count + 0.5)
    ax.set_ylim(-3.2, 3.2)
    ax.spines["right"].set_visible(False)
    ax.spines["top"].set_visible(False)
    ax.spines["left"].set_visible(False)
    ax.spines["bottom"].set_visible(False)

    if output_path:
        plt.savefig(fname=output_path, bbox_inches="tight", pad_inches=0)
    if show:
        plt.show()

    return ax
