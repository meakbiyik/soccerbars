import re
from ast import literal_eval

import click
from click_help_colors import HelpColorsCommand

from scorebars import scorebar

DESCRIPTION = """Multivariate sparklines making use of Gestalt theory (gestaltlines) for sequences of sports results.

Keyword arguments in the form 'name:value' can be used to further configure the output.
"""
MATCH_REGEX = re.compile(r"\((\d+)?[,-](\d+)?\)(\*)?")


@click.command(
    help=DESCRIPTION,
    cls=HelpColorsCommand,
    help_headers_color="yellow",
    help_options_color="green",
)
@click.option(
    "--matches",
    prompt="Matches",
    help="Matches in the form ([s_1],[s_2])[* {if away game}]",
)
@click.option(
    "-t",
    "--twogoalline",
    prompt="Two-goal line",
    default=False,
    help="Add lines to two goal levels",
    is_flag=True,
)
@click.option(
    "-n",
    "--zerodots",
    prompt="Zero dots",
    default=False,
    help="Add dots in place of no goals",
    is_flag=True,
)
@click.option(
    "-ol",
    "--outlined",
    prompt="Outlined",
    default=False,
    help="Plot the outlines",
    is_flag=True,
)
@click.option(
    "-o",
    "--output-path",
    default="output.png",
    help="Output path of the image, 'output.png' by default",
    type=click.Path(),
)
@click.argument("plot_kwargs", nargs=-1)
def cli(
    matches: str,
    twogoalline: bool,
    zerodots: bool,
    outlined: bool,
    output_path: str,
    plot_kwargs: str,
):

    matches = [
        (
            int(score1) if score1 != "" else None,
            int(score2) if score2 != "" else None,
            bool(star),
        )
        for score1, score2, star in MATCH_REGEX.findall(matches)
    ]

    plot_kwargs = dict(arg.split(":") for arg in plot_kwargs)
    for k, v in plot_kwargs.items():
        try:
            plot_kwargs[k] = literal_eval(v)
        except ValueError:
            pass

    scorebar(
        matches,
        show=False,
        twogoalline=twogoalline,
        zerodots=zerodots,
        outlined=outlined,
        output_path=output_path,
        **plot_kwargs,
    )


if __name__ == "__main__":

    cli()
