import math
import itertools
from pathlib import Path
from mock import patch

import pytest
import numpy as np
import pandas as pd
from matplotlib import pyplot as plt

from soccerbars.core import (
    _colors,
    _is_listlike,
    _is_integerish,
    _maybe_convert_dataframe,
    _maybe_flatten_vectors,
    _check_scores,
    _check_color_and_output_path,
    _config_factory,
    DEFAULT_CONFIG,
    soccerbar,
)

# TODO: Check out pytest-mpl for image comparison tests.

test_dataframes = [
    (
        pd.DataFrame(
            {
                "home_team_score": [8, 4, 4, 1, 5, 0, 1, 2, None, None, None],
                "away_team_score": [0, 1, 4, 4, 0, 0, 1, 3, None, None, None],
                "is_away_game": [
                    False,
                    True,
                    False,
                    True,
                    False,
                    True,
                    False,
                    True,
                    False,
                    True,
                    False,
                ],
            },
        ),
        [
            [8, 0, False],
            [4, 1, True],
            [4, 4, False],
            [1, 4, True],
            [5, 0, False],
            [0, 0, True],
            [1, 1, False],
            [2, 3, True],
            [np.nan, np.nan, False],
            [np.nan, np.nan, True],
            [np.nan, np.nan, False],
        ],
    ),
    (
        pd.DataFrame(
            {"home_team_score": [], "away_team_score": [], "is_away_game": []},
        ),
        [],
    ),
    (
        [
            pd.DataFrame(
                {
                    "home_team_score": [8, 4, 4],
                    "away_team_score": [0, 1, 4],
                    "is_away_game": [
                        False,
                        True,
                        False,
                    ],
                },
            ),
            pd.DataFrame(
                {
                    "home_team_score": [np.nan, np.nan, np.nan],
                    "away_team_score": [np.nan, np.nan, np.nan],
                    "is_away_game": [
                        False,
                        True,
                        False,
                    ],
                },
            ),
        ],
        [
            [
                [8, 0, False],
                [4, 1, True],
                [4, 4, False],
            ],
            [
                [np.nan, np.nan, False],
                [np.nan, np.nan, True],
                [np.nan, np.nan, False],
            ],
        ],
    ),
]

vector_test_inputs = [
    (
        [[1, 2, 3], [4, 5, 6], [True, False, True]],
        [[1, 4, True], [2, 5, False], [3, 6, True]],
    ),
    (
        [[[1, np.nan], [3, np.nan], [True, False]], [[5, 6], [7, 8], [True, False]]],
        [[[1, 3, True], [np.nan, np.nan, False]], [[5, 7, True], [6, 8, False]]],
    ),
    (
        [[1, 4, True], [2, 5, False], [3, 6, True]],
        [[1, 4, True], [2, 5, False], [3, 6, True]],
    ),
    (
        [[[1, 3, True], [2, 4, False]], [[5, 7, True], [6, 8, False]]],
        [[[1, 3, True], [2, 4, False]], [[5, 7, True], [6, 8, False]]],
    ),
]

bad_scores = [
    (3, TypeError, r"'scores' must be an iterable, not <class 'int'>"),
    ([], ValueError, r"'scores' cannot be empty"),
    (
        [(1, "A", False)],
        TypeError,
        r".+? with Tuple\[int, int, bool\], not Tuple\[int, str, bool\]",
    ),
    (
        [[(1, 2, 3), (1, 2, 3)]],
        TypeError,
        r".+? with Tuple\[int, int, bool\], not Tuple\[int, int, int\]",
    ),
    (
        [[(1, 2, 3), (1, 2, 3), (1, 2, 3)]],
        TypeError,
        r"In columnwise match inputs, .+? away flags column is expected "
        r"to have type bool, not \['int', 'int', 'int'\]",
    ),
    (
        [("1", "2", "3"), (1, 2, 3), (True, False, True)],
        TypeError,
        r"In columnwise match inputs, .+? home scores column is expected "
        r"to have type int, not \['str', 'str', 'str'\]",
    ),
    (
        [(1, 2, 3), ("1", "2", "3"), (True, False, True)],
        TypeError,
        r"In columnwise match inputs, .+? away scores column is expected "
        r"to have type int, not \['str', 'str', 'str'\]",
    ),
    (
        [(1, 2, 3), (1, 2, 3, 4), (True, False, True)],
        ValueError,
        r"In columnwise match inputs, all columns must have equal lengths, "
        r"but away scores column had length 4 instead of 3",
    ),
    (
        [[(1, 2, 3), (1, 2, 3), (True, False, "True")]],
        TypeError,
        r"In columnwise match inputs, all values in the away flags column is expected "
        r"to have type bool, not \['bool', 'bool', 'str'\]",
    ),
    (
        [[(1, 2, 3), (1, 2, 3), (True, False, True, False)]],
        ValueError,
        r"In columnwise match inputs, all columns must have equal lengths, "
        r"but away flags column had length 4 instead of 3",
    ),
]

bad_color_path_inputs = [
    (
        ["r", "r"],
        None,
        [[8, 0, False], [4, 1, True], [4, 4, False]],
        ValueError,
        r"Length of matches .+",
    ),
    (
        ["r", "r", "r"],
        None,
        [[[8, 0, False], [4, 1, True]], [[4, 4, False]]],
        ValueError,
        r"Length of matches .+",
    ),
    (
        ["i am an invalid color", "r", "r"],
        None,
        [[[8, 0, False], [4, 1, True]], [[4, 4, False]]],
        ValueError,
        r"Length of matches .+",
    ),
    (
        ["i am an invalid color", "r", "r"],
        None,
        [[8, 0, False], [4, 1, True], [4, 4, False]],
        ValueError,
        r".+?is not a valid matplotlib color.+",
    ),
    (
        [["i am an invalid color", "r", "r"]],
        None,
        [[[8, 0, False], [4, 1, True], [4, 4, False]]],
        ValueError,
        r".+?is not a valid matplotlib color.+",
    ),
    (
        ["r", "r"],
        ["path.png"],
        [[8, 0, False], [4, 1, True], [4, 4, False]],
        TypeError,
        r".+? not \[<class 'list'>\].",
    ),
    (
        ["r", "r"],
        "path.png",
        [[[8, 0, False], [4, 1, True], [4, 4, False]]],
        ValueError,
        r".+? iterable of paths, not <class 'str'>.",
    ),
    (
        ["r", "r"],
        ["path.png", "path.png"],
        [[[8, 0, False], [4, 1, True], [4, 4, False]]],
        ValueError,
        r".+? 2 output paths.+? 1 set\(s\) of.+",
    ),
    (
        None,
        [1234],
        [[[8, 0, False], [4, 1, True], [4, 4, False]]],
        TypeError,
        r".+? not \[<class 'int'>\].",
    ),
    (
        ["r", "r"],
        1234,
        [[8, 0, False], [4, 1, True], [4, 4, False]],
        TypeError,
        r".+? not \[<class 'int'>\].",
    ),
]

dummy_file_name = Path(".dummy.png")

test_inputs = [
    [
        [8, 0, False],
        [4, 1, True],
        [4, 4, False],
        [1, 4, True],
        [5, 0, False],
        [0, 0, True],
        [1, 1, False],
        [2, 3, True],
        [np.nan, np.nan, False],
        [np.nan, np.nan, True],
        [np.nan, np.nan, False],
    ],
    [
        [
            [8, 0, False],
            [4, 1, True],
            [4, 4, False],
        ],
        [
            [np.nan, np.nan, False],
            [np.nan, np.nan, True],
            [np.nan, np.nan, False],
        ],
    ],
]

test_parameters = [
    {
        "outlined": False,
        "twogoalline": False,
        "zerodots": False,
        "show": False,
        "output_path": None,
    },
    {
        "outlined": False,
        "twogoalline": False,
        "zerodots": False,
        "show": True,
        "output_path": None,
    },
    {
        "outlined": False,
        "twogoalline": False,
        "zerodots": True,
        "show": False,
        "output_path": None,
    },
    {
        "outlined": False,
        "twogoalline": False,
        "zerodots": True,
        "show": True,
        "output_path": None,
    },
    {
        "outlined": False,
        "twogoalline": True,
        "zerodots": False,
        "show": False,
        "output_path": None,
    },
    {
        "outlined": False,
        "twogoalline": True,
        "zerodots": False,
        "show": True,
        "output_path": None,
    },
    {
        "outlined": False,
        "twogoalline": True,
        "zerodots": True,
        "show": False,
        "output_path": None,
    },
    {
        "outlined": True,
        "twogoalline": True,
        "zerodots": True,
        "show": True,
        "output_path": None,
    },
    {
        "outlined": True,
        "twogoalline": False,
        "zerodots": False,
        "show": False,
        "output_path": dummy_file_name,
    },
    {
        "outlined": True,
        "twogoalline": False,
        "zerodots": False,
        "show": True,
        "output_path": dummy_file_name,
    },
    {
        "outlined": True,
        "twogoalline": False,
        "zerodots": True,
        "show": False,
        "output_path": dummy_file_name,
    },
    {
        "outlined": True,
        "twogoalline": False,
        "zerodots": True,
        "show": True,
        "output_path": dummy_file_name,
    },
    {
        "outlined": True,
        "twogoalline": True,
        "zerodots": False,
        "show": False,
        "output_path": dummy_file_name,
    },
    {
        "outlined": True,
        "twogoalline": True,
        "zerodots": False,
        "show": True,
        "output_path": dummy_file_name,
    },
    {
        "outlined": True,
        "twogoalline": True,
        "zerodots": True,
        "show": False,
        "output_path": dummy_file_name,
    },
    {
        "outlined": True,
        "twogoalline": True,
        "zerodots": True,
        "show": True,
        "output_path": dummy_file_name,
    },
]


listlike_test_values = [
    (1, False, dict()),
    ("1", False, dict()),
    ([1], True, dict()),
    ((1,), True, dict()),
    ([], False, dict(ensure_nonempty=True)),
    ([1], False, dict(ensure_type="bool")),
    ([1.0], True, dict(ensure_type="integerish")),
    ([1, 1.1], False, dict(ensure_type="integerish")),
    ([], False, dict(ensure_nonempty=True, ensure_type="integerish")),
    ([1, 2.0], True, dict(ensure_nonempty=True, ensure_type="integerish")),
    ([1, 2.1], False, dict(ensure_nonempty=True, ensure_type="integerish")),
]


integerish_test_values = [
    (1, True),
    (np.nan, True),
    (None, True),
    (1.0, True),
    (1.1, False),
    (np.array([1]), False),
]


@pytest.mark.parametrize("input,expected,params", listlike_test_values)
def test__is_listlike(input, expected, params):
    assert _is_listlike(input, **params) == expected


@pytest.mark.parametrize("input,expected", test_dataframes)
def test__maybe_convert_dataframe(input, expected):
    output = _maybe_convert_dataframe(input)
    for elem1, elem2 in zip(output, expected):
        assert np.array_equal(elem1, elem2, equal_nan=True)


@pytest.mark.parametrize("input,expected", integerish_test_values)
def test__is_integerish(input, expected):
    assert _is_integerish(input) == expected


@pytest.mark.parametrize("input,expected", vector_test_inputs)
def test__maybe_flatten_vectors(input, expected):
    output = _maybe_flatten_vectors(input)
    for elem1, elem2 in zip(output, expected):
        assert np.array_equal(elem1, elem2, equal_nan=True)


@pytest.mark.parametrize("input,expected_error,match", bad_scores)
def test__check_scores(input, expected_error, match):
    with pytest.raises(expected_error, match=match):
        _check_scores(input)


@pytest.mark.parametrize(
    "color,output_path,scores,expected_error,match", bad_color_path_inputs
)
def test__check_color_and_output_path(
    color, output_path, scores, expected_error, match
):
    with pytest.raises(expected_error, match=match):
        _check_color_and_output_path(color, output_path, scores)


def test__config_factory():
    assert _config_factory(True) == DEFAULT_CONFIG
    assert _config_factory(False)["edge_thickness"] == 0
    assert (
        _config_factory(True, edge_thickness=10)["edge_thickness"]
        == 10 * DEFAULT_CONFIG["thickness"]
    )
    assert _config_factory(True, slant=20)["slant"] == math.sin(math.radians(20))
    assert (
        _config_factory(True, zerodot=10)["zerodot"] == 10 * DEFAULT_CONFIG["thickness"]
    )
    assert _config_factory(True, zerodot=10, thickness=5)["zerodot"] == 10 * 5
    assert _config_factory(True, fill_color="red")["fill_color"] == (1, 0, 0, 1)
    assert _config_factory(True, fill_color=(1, 0, 0, 1))["fill_color"] == (1, 0, 0, 1)
    assert _config_factory(True, fill_color=(1, 0, 0))["fill_color"] == (1, 0, 0, 1)
    assert _config_factory(True, fill_color="#FF0000")["fill_color"] == (1, 0, 0, 1)
    assert _config_factory(True, fill_color="#FF0000FF")["fill_color"] == (1, 0, 0, 1)
    assert (
        _config_factory(True, clip_slanted_lines=False)["clip_slanted_lines"] == False
    )
    with pytest.raises(KeyError):
        _config_factory(True, fake_argument=False)


def test__colors():
    matchcolor = (0.12, 0.24, 0.36, 1)
    bright_away_color = (
        DEFAULT_CONFIG["away_color"][0]
        + (1 - DEFAULT_CONFIG["away_color"][0]) * DEFAULT_CONFIG["brighten"] / 100,
        DEFAULT_CONFIG["away_color"][1]
        + (1 - DEFAULT_CONFIG["away_color"][1]) * DEFAULT_CONFIG["brighten"] / 100,
        DEFAULT_CONFIG["away_color"][2]
        + (1 - DEFAULT_CONFIG["away_color"][2]) * DEFAULT_CONFIG["brighten"] / 100,
        DEFAULT_CONFIG["away_color"][3],
    )

    assert _colors(False, False, DEFAULT_CONFIG) == (
        DEFAULT_CONFIG["home_color"],
        DEFAULT_CONFIG["home_color"],
    )
    assert _colors(True, False, DEFAULT_CONFIG) == (
        bright_away_color,
        bright_away_color,
    )
    assert _colors(False, True, DEFAULT_CONFIG) == (
        DEFAULT_CONFIG["home_color"],
        DEFAULT_CONFIG["home_color"],
    )
    assert _colors(True, True, DEFAULT_CONFIG) == (
        DEFAULT_CONFIG["fill_color"],
        DEFAULT_CONFIG["away_color"],
    )

    assert _colors(False, False, DEFAULT_CONFIG, matchcolor=matchcolor) == (
        matchcolor,
        matchcolor,
    )
    assert _colors(True, False, DEFAULT_CONFIG, matchcolor=matchcolor) == (
        matchcolor,
        matchcolor,
    )
    assert _colors(False, True, DEFAULT_CONFIG, matchcolor=matchcolor) == (
        matchcolor,
        matchcolor,
    )
    assert _colors(True, True, DEFAULT_CONFIG, matchcolor=matchcolor) == (
        DEFAULT_CONFIG["fill_color"],
        matchcolor,
    )


@pytest.mark.filterwarnings("error")
@pytest.mark.parametrize(
    "input,params", itertools.product(test_inputs, test_parameters)
)
@patch("matplotlib.pyplot.show")
def test_soccerbar(_, input, params, capsys):
    if not _is_integerish(input[0][0]) and params["output_path"] is not None:
        params["output_path"] = [params["output_path"]] * 2
    soccerbar(input, **params)
    outputs = capsys.readouterr()
    assert not outputs.out
    assert not outputs.err
    plt.close("all")


if dummy_file_name.exists():
    dummy_file_name.unlink()
