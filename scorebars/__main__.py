import re

import click
from click_help_colors import HelpColorsCommand

from scorebars import plot_scores

DESCRIPTION = "Multivariate sparklines making use of Gestalt theory (gestaltlines) for sequences of sports results."
MATCH_REGEX = re.compile(r"\((\d+),(\d+)\)(\*)?")


@click.command(
    help=DESCRIPTION,
    cls=HelpColorsCommand,
    help_headers_color="yellow",
    help_options_color="green",
)
@click.option(
    "--matches",
    prompt="Matches",
    help="Matches in the form ([s_1],[s_2])[* {if away game}].",
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
    "--nozerodots",
    prompt="No zero dots",
    default=False,
    help="Do not add dots in place of zero goals.",
    is_flag=True,
)
@click.option(
    "-ol",
    "--outlined",
    prompt="Outlined",
    default=False,
    help="Plot the outlines.",
    is_flag=True,
)
@click.option(
    "-o",
    "--output-path",
    default="output.png",
    help="Output path of the image, 'output.png' by default.",
    type=click.Path(),
)
def cli(
    matches: str, twogoalline: bool, nozerodots: bool, outlined: bool, output_path: str
):

    matches = [
        (int(score1), int(score2), bool(star))
        for score1, score2, star in MATCH_REGEX.findall(matches)
    ]

    plot_scores(
        matches,
        show=False,
        twogoalline=twogoalline,
        nozerodots=nozerodots,
        outlined=outlined,
        output_path=output_path,
    )


if __name__ == "__main__":

    cli()
