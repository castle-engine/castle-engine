# Regular Expression library

There are a few regular expression libraries for FPC and Delphi.

To be compatible with both FPC and Delphi, we include here the latest (as of 2024-01-19) sources of the https://github.com/andgineer/TRegExpr library, by _Andrey V. Sorokin_ and others. It is

- compatible with both FPC and Delphi
- actively developed (see GitHub commits and https://wiki.freepascal.org/RegEx_packages )

Note that latest FPC 3.2.2 also features a version of this unit. But older FPC 3.2.0 has a different, less funtional RegExpr. And Delphi has yet another different regexp API (see RegularExpressions, Character units). In CGE, we'll strive to use this library, https://github.com/andgineer/TRegExpr , everywhere, to make it simpler.

For now, we test this approach within the check_packages tool.
