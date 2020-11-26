# scorebars

Multivariate sparklines making use of Gestalt theory (gestaltlines) for sequences of sports results.

## Usage

### In Python

Give the matches to the ```plot_scores``` function as an iterable, or a list of iterables of tuples in the form ```(score_1: int, score_2: int, away_game: bool)```.
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