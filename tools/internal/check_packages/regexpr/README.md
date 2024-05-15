# Regular Expression library

There are a few regular expression libraries for FPC and Delphi.

To be compatible with both FPC and Delphi, we include here the latest (as of 2024-01-19) sources of the https://github.com/andgineer/TRegExpr library, by _Andrey V. Sorokin_ and others. It is

- compatible with both FPC and Delphi
- actively developed (see GitHub commits and https://wiki.freepascal.org/RegEx_packages )

Note that latest FPC 3.2.2 also features a version of this unit. But older FPC 3.2.0 has a different, less funtional RegExpr. And Delphi has yet another different regexp API (see RegularExpressions, Character units). In CGE, we'll strive to use this library, https://github.com/andgineer/TRegExpr , everywhere, to make it simpler.

For now, we test this approach within the check_packages tool.

## Should this be in CGE core?

We considered it. Either as `CastleRegexpr` or `CastleInternalRegexps`. But:

- Maintaining regexpr code is not a critical task in game engine.

- In fact we don't really need regexps in CGE. We use regexps only for minor utilities (`FormatNameCounter`) and tests (`check_packages` etc.). We could even remove their usage from CGE with small effort, without any loss for users.

- FPC (>= 3.2.2) and Delphi already have great solutions for regexps, just not compatible, but for many users this is probably not a problem. (Since many users decide to use either FPC or Delphi, and stick to it.)

- So we just don't want to maintain "regexps in CGE" more now.
