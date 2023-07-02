<!-- markdownlint-disable MD022 MD024 MD032 -->
# Changelog
All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [0.2.1] - 02.07.2022
### Fixed
- Inner path seam artifacts in low resolution
- Numpy build issues

## [0.2.0] - 09.04.2021
### Added
- When outlined=True and twogoalline=True, twogoallines now also jump across the bars (as baseline already did) to make real transparency possible
- Package is available on PyPI

### Changed
- Default spacing from 0.8 to 0.9.
- Tip thicknesses in outlined cases are now equal to the baseline width (except when twogoalline=True, at which point if score is 2, thickness is halved to remove visual artifacts).

### Fixed
- Default documented thickness being inconsistent with the effective value

## [0.1.0] - 24.03.2021
### Added
- Initial release of the soccerbars package.

[Unreleased]: https://github.com/meakbiyik/soccerbars/compare/v0.2.1...HEAD
[0.2.1]: https://github.com/meakbiyik/soccerbars/compare/v0.2.0...v0.2.1
[0.2.0]: https://github.com/meakbiyik/soccerbars/compare/v0.1.0...v0.2.0
[0.1.0]: https://github.com/meakbiyik/soccerbars/releases/tag/v0.1.0
