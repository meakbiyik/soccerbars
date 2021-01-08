# scorebars

<!-- To include when the repo becomes public.
<!-- badges: start 
[![Codecov test coverage](https://codecov.io/gh/snlab-eakbiyik/scorebars/branch/master/graph/badge.svg)](https://codecov.io/gh/snlab-eakbiyik/scorebars?branch=master)
<!-- badges: end 
-->

Multivariate sparklines making use of Gestalt theory (gestaltlines) for sequences of sports results.

This repository contains a Python package, an R package, and a cmd tool based on Python.

## Usage

### In Python

Give the matches to the ```plot_scores``` function as an iterable, or a list of iterables of tuples in the form ```(score_1: int, score_2: int, away_game: bool)```. pandas DataFrames (or a list of DataFrames for multiple teams) are also supported with the same form and datatypes.
```python
from scorebars import plot_scores

plot_scores([(1, 2, True), (3, 3, False)])
```
It is possible to use the same flags from the Latex package by just passing them as booleans set to True.
```python
plot_scores(
    [(1, 2, True), (3, 3, False)], 
    twogoalline = True,
    nozerodots = True,
    outlined = True,
)
```
You can either show the resulting plot by default, or save it by specifying an output path.
```python
plot_scores(
    [(1, 2, True), (3, 3, False)], 
    output_path = "out.png"
)
```

### in R

Similarly with its Python counterpart, ```plot_scores``` takes a list, a list of lists, a data.frame or a list of data.frames of match scores, with each match score having the form of a list (or a data.frame row) of (home_team_score: int, away_team_score: int, is_away_game: logical). The latex syntax and flags also apply.
```R
library(scorebars)

plot_scores(list(list(1,2,T), list(3,3,F)))
```
With the possible flags and paths:
```R
plot_scores(
    list(list(1,2,T), list(3,3,F)), 
    twogoalline = T, nozerodots = T,
    outlined = T, output_path = "out.png"
)
```

### via cmd 

After installing the package (see the instructions below), navigate into the top directory and run
```bash
poetry run scorebars
```
A prompt will appear to enter the matches in a string format. Enter `--help` to see the required structure (identical with the Latex syntax).

The command line tool uses the same naming scheme with the Python API, and writes the output image to the specified location with the `--output-path` argument (default location is `output.png`).

## Installation

### As a Python package

Since this package is not yet uploaded to PyPI, it needs to be installed as a Git package via pip 
```bash
pip install git+https://github.com/snlab-eakbiyik/scorebars.git 
```

### As an R package

1. Install the `devtools` package via
    ```R
    install.packages("devtools")
    ```
2. Install the package from Github
    ```R
    library(devtools)
    install_github("snlab-eakbiyik/scorebars")
    ```

### As a cmd tool

1. Install Python from [here](https://www.python.org/downloads/)
2. Install [Poetry](https://python-poetry.org/), a dependency management tool for Python, via the following command
    ```bash
    curl -sSL https://raw.githubusercontent.com/python-poetry/poetry/master/get-poetry.py | python
    ```
3. Clone this repository to your local, navigate into the folder and run
    ```bash
    poetry install
    ```