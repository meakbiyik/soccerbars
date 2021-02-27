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
            "are working with the cloned repository, or via `pip install scorebars[cli]` "
            "with pip."
        )
    else:
        click = Mock()
        HelpColorsCommand = Mock()

from scorebars import scorebar


DESCRIPTION = """Multivariate sparklines making use of Gestalt theory (gestaltlines) for sequences of sports results.

Matches can either follow the LaTeX format, e.g. "(1-2) (3-4)*", or can be a glob pattern that matches with valid .csv files.
Output file type can be changed by specifying it in the output path as the extension.

\b
Examples:
    poetry run scorebars "(1-2) (3-1)* (2-2)"
    poetry run scorebars -o "out.tiff" "(1-2) (3-1)* (2-2)"
    poetry run scorebars matches.csv -o "matches.pdf"
    poetry run scorebars *.csv -o ".\output-dir"
    poetry run scorebars -z -ol -p fill_color:"(1,1,1,1)" -p home_color:red -o .\out *.csv
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
    "pattern that matches with multiple .csv files, this parameter "
    "can be given as a directory path to save the output images, with the "
    "names identical to the input .csv files. Default behavior is to "
    "save text inputs into 'output.png' and csv inputs to the same path "
    "with the extension '.png'.",
    type=click.Path(),
)
@click.option(
    "-p",
    "--plot-kwargs",
    multiple=True,
    type=click.STRING,
    help="Key value tuple (without spaces) in the form key:value to modify "
    "the visualization. See https://github.com/snlab-eakbiyik/scorebars or "
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
        filepaths = glob.glob(matches)
        match_scores = [pd.read_csv(filepath) for filepath in filepaths]
        if len(match_scores) > 1:
            if output_path:
                Path(output_path).mkdir(exist_ok=True)
                output_path = [
                    Path(output_path).joinpath(Path(path).with_suffix(".png").name)
                    for path in filepaths
                ]
            else:
                output_path = [
                    Path(".").joinpath(Path(path).with_suffix(".png").name)
                    for path in filepaths
                ]
        elif output_path:
            output_path = [output_path]
    else:
        if not output_path:
            output_path = "output.png"

    plot_kwargs = dict(arg.split(":") for arg in plot_kwargs)
    for k, v in plot_kwargs.items():
        try:
            plot_kwargs[k] = literal_eval(v)
        except ValueError:
            pass

    scorebar(
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
