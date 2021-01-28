import math
import itertools
from pathlib import Path
from mock import patch

import pytest
import numpy as np
import pandas as pd
from matplotlib import pyplot as plt

from scorebars.core import (
    _colors,
    _maybe_convert_dataframe,
    _check_scores,
    _config_factory,
    DEFAULT_CONFIG,
    plot_scores,
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

bad_scores = [
    (3, TypeError),
    ([], ValueError),
    ([(1, "A", False)], TypeError),
    ([[(1, 2, 3), (1, 2, 3)]], TypeError),
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
        "nozerodots": False,
        "show": False,
        "output_path": None,
    },
    {
        "outlined": False,
        "twogoalline": False,
        "nozerodots": False,
        "show": True,
        "output_path": None,
    },
    {
        "outlined": False,
        "twogoalline": False,
        "nozerodots": True,
        "show": False,
        "output_path": None,
    },
    {
        "outlined": False,
        "twogoalline": False,
        "nozerodots": True,
        "show": True,
        "output_path": None,
    },
    {
        "outlined": False,
        "twogoalline": True,
        "nozerodots": False,
        "show": False,
        "output_path": None,
    },
    {
        "outlined": False,
        "twogoalline": True,
        "nozerodots": False,
        "show": True,
        "output_path": None,
    },
    {
        "outlined": False,
        "twogoalline": True,
        "nozerodots": True,
        "show": False,
        "output_path": None,
    },
    {
        "outlined": True,
        "twogoalline": True,
        "nozerodots": True,
        "show": True,
        "output_path": None,
    },
    {
        "outlined": True,
        "twogoalline": False,
        "nozerodots": False,
        "show": False,
        "output_path": dummy_file_name,
    },
    {
        "outlined": True,
        "twogoalline": False,
        "nozerodots": False,
        "show": True,
        "output_path": dummy_file_name,
    },
    {
        "outlined": True,
        "twogoalline": False,
        "nozerodots": True,
        "show": False,
        "output_path": dummy_file_name,
    },
    {
        "outlined": True,
        "twogoalline": False,
        "nozerodots": True,
        "show": True,
        "output_path": dummy_file_name,
    },
    {
        "outlined": True,
        "twogoalline": True,
        "nozerodots": False,
        "show": False,
        "output_path": dummy_file_name,
    },
    {
        "outlined": True,
        "twogoalline": True,
        "nozerodots": False,
        "show": True,
        "output_path": dummy_file_name,
    },
    {
        "outlined": True,
        "twogoalline": True,
        "nozerodots": True,
        "show": False,
        "output_path": dummy_file_name,
    },
    {
        "outlined": True,
        "twogoalline": True,
        "nozerodots": True,
        "show": True,
        "output_path": dummy_file_name,
    },
]


@pytest.mark.parametrize("input,expected", test_dataframes)
def test__maybe_convert_dataframe(input, expected):
    output = _maybe_convert_dataframe(input)
    for elem1, elem2 in zip(output, expected):
        assert np.array_equal(elem1, elem2, equal_nan=True)


@pytest.mark.parametrize("input,expected_error", bad_scores)
def test__check_scores(input, expected_error):
    with pytest.raises(expected_error):
        _check_scores(input)


def test__config_factory():
    assert _config_factory(True) == DEFAULT_CONFIG
    assert _config_factory(False)["edge_thickness"] == 0
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


@pytest.mark.filterwarnings("error")
@pytest.mark.parametrize(
    "input,params", itertools.product(test_inputs, test_parameters)
)
@patch("matplotlib.pyplot.show")
def test_plot_scores(_, input, params, capsys):
    plot_scores(input, **params)
    outputs = capsys.readouterr()
    assert not outputs.out
    assert not outputs.err
    plt.close("all")


if dummy_file_name.exists():
    dummy_file_name.unlink()
