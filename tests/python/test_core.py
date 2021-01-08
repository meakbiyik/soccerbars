import math
import pytest
import numpy as np
import pandas as pd

from scorebars.core import (
    _maybe_convert_dataframe,
    _check_scores,
    _config_factory,
    DEFAULT_CONFIG,
)

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

bad_dataframes = [
    (3, TypeError),
    ([], ValueError),
    ([(1, "A", False)], TypeError),
    ([(1, 2, 3)], TypeError),
]


@pytest.mark.parametrize("input,expected", test_dataframes)
def test__maybe_convert_dataframe(input, expected):
    output = _maybe_convert_dataframe(input)
    for elem1, elem2 in zip(output, expected):
        assert np.array_equal(elem1, elem2, equal_nan=True)


@pytest.mark.parametrize("input,expected_error", bad_dataframes)
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
