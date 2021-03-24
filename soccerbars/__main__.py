import os
from pathlib import Path
import re
from ast import literal_eval
import glob
from typing import Tuple
from unittest.mock import Mock

try:
    import click
    from click_help_colors import HelpColorsCommand
    import pandas as pd
except:
    if __name__ == "__main__":
        raise ImportError(
            "Some of the optional dependencies for the cli module are not installed. "
            "Please install them either via `poetry install --no-dev -E cli` if you "
            "are working with the cloned repository, or via `pip install soccerbars[cli]` "
            "with pip."
        )
    else:
        click = Mock()
        HelpColorsCommand = Mock()

from soccerbars import soccerbar


DESCRIPTION = """Multivariate sparklines making use of Gestalt theory (gestaltlines) 
for sequences of sports results.

Matches can either follow the LaTeX format, e.g. "(1-2) (3-4)*", or can be a file name 
or a glob pattern that matches with valid .csv files. Output file type can be changed 
by specifying it in the output path as the extension.

\b
Examples:
    poetry run soccerbars "(1-2) (3-1)* (2-2)"
    poetry run soccerbars -o out.tiff "(1-2) (3-1)* (2-2)"
    poetry run soccerbars matches.csv -o matches.pdf
    poetry run soccerbars *.csv -o ".\output-dir"
    poetry run soccerbars -z -ol -p fill_color:"(1,1,1,1)" -p home_color:red *.csv
"""
MATCH_REGEX = re.compile(r"\((\d+)?[,-](\d+)?\)(\*)?")


@click.command(
    help=DESCRIPTION,
    cls=HelpColorsCommand,
    help_headers_color="yellow",
    help_options_color="green",
)
@click.argument("matches")
@click.option(
    "-t",
    "--twogoalline",
    default=False,
    help="Add lines to two goal levels",
    is_flag=True,
)
@click.option(
    "-z",
    "--zerodots",
    default=False,
    help="Add dots in place of no goals",
    is_flag=True,
)
@click.option(
    "-ol",
    "--outlined",
    default=False,
    help="Plot the outlines",
    is_flag=True,
)
@click.option(
    "-o",
    "--output-path",
    default=None,
    help="Output path of the image. If the input is a glob "
    "pattern, this parameter is interpreted as a directory path to "
    "save the output images, with the names identical to the input "
    ".csv files. Default behavior is to save text and file path inputs "
    "into 'output.png' at the working directory, and glob patterns "
    "under the directory '.\out' with the extension '.png'.",
    type=click.Path(),
)
@click.option(
    "-p",
    "--plot-kwargs",
    multiple=True,
    type=click.STRING,
    help="Key value tuple (without spaces) in the form key:value to modify "
    "the visualization. See https://github.com/meakbiyik/soccerbars or "
    "the Python function docstring for the available parameters.",
)
def cli(
    matches: str,
    twogoalline: bool,
    zerodots: bool,
    outlined: bool,
    output_path: str,
    plot_kwargs: Tuple[str],
):

    if not matches:
        matches = click.prompt("Matches")

    match_scores = [
        (
            int(score1) if score1 != "" else None,
            int(score2) if score2 != "" else None,
            bool(star),
        )
        for score1, score2, star in MATCH_REGEX.findall(matches)
    ]

    if not match_scores:
        if os.path.exists(matches):
            match_scores = pd.read_csv(Path(matches))
            if not output_path:
                output_path = Path(os.getcwd()).joinpath("output.png")
        else:
            filepaths = glob.glob(matches)
            match_scores = [pd.read_csv(filepath) for filepath in filepaths]
            if output_path:
                output_dir = Path(output_path)
            else:
                output_dir = Path(os.getcwd()).joinpath("out")
            output_dir.mkdir(exist_ok=True)
            output_path = [
                output_dir.joinpath(Path(path).with_suffix(".png").name)
                for path in filepaths
            ]
    else:
        if not output_path:
            output_path = Path(os.getcwd()).joinpath("output.png")

    plot_kwargs = dict(arg.split(":") for arg in plot_kwargs)
    for k, v in plot_kwargs.items():
        try:
            plot_kwargs[k] = literal_eval(v)
        except ValueError:
            pass

    soccerbar(
        match_scores,
        show=False,
        twogoalline=twogoalline,
        zerodots=zerodots,
        outlined=outlined,
        output_path=output_path,
        **plot_kwargs,
    )


if __name__ == "__main__":

    cli()
