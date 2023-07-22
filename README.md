# Stringendo - a string parsing library

String parsing functionalites for generating plotnames, filenames and path. Used by most of my other packages, including [CodeAndRoll2](https://github.com/vertesy/CodeAndRoll2) and [ggExpress](https://github.com/vertesy/ggExpress). 
Many functionalities were part of the formerly used [CodeAndRoll (v1)](https://github.com/vertesy/CodeAndRoll).



<br><br>

## Installation

Install directly from **GitHub** via **devtools** with one R command:

```R
# install.packages("devtools"); # If you don't have it.
require("devtools")
devtools::install_github(repo = "vertesy/Stringendo")
```

...then simply load the package:

```R
require("Stringendo")
```

Alternatively, you simply source it from the web. 
*This way function help will not work, and you will have no local copy of the code on your hard drive.*

```R
source("https://raw.githubusercontent.com/vertesy/Stringendo/main/R/Stringendo.R")
```

<br>

### Troubleshooting

*If you encounter a **bug**, something doesn't work or unclear, please let me know by raising an issue on [Stringendo ](https://github.com/vertesy/Stringendo/issues) – Please check if it has been asked.*

<br>

## List of Functions

- #### 1 `iprint()`

  A more intelligent printing function that collapses any variable passed to it by white spaces.

- #### 2 `substrRight()`

  Parse current date, dot separated.

- #### 3 `kollapse()`

  Collapses values and strings to one string (without a white space).  It also prints the results (good for a quick check)

- #### 4 `ppp()`

  Paste by point

- #### 5 `pps()`

  Paste by (forward) slash

- #### 6 `ppu()`

  Paste by underscore

- #### 7 `ppd()`

  Paste by dash

- #### 8 `kpp()`

  Collapse by point

- #### 9 `kppu()`

  Collapse by underscore

- #### 10 `kpps()`

  Collapse by (forward) slash

- #### 11 `kppd()`

  Collapse by dash

- #### 12 `sppp()`

  Simplified Paste by point

- #### 13 `spps()`

  Simplified Paste by fwd slash

- #### 14 `percentile2value()`

  Calculate what is the actual value of the N-th percentile in a distribution or set of numbers. Useful for calculating cutoffs, and displaying them by whist()s "vline" paramter.

- #### 15 `parsepvalue()`

  Parse p-value from a number to a string.

- #### 16 `percentage_formatter()`

  evaluate and parse (dyn_var_caller)

- #### 17 `ReplaceSpecialCharacters()`

  ReplaceSpecialCharacters replaces '[]$@()' with dots

- #### 18 `AddTrailingDot()`

  Adds a final slash '/', if missing from a string (file path).

- #### 19 `RemoveDoubleDot()`

  RemoveDoubleDot removes multiple consecutive slashes (e.g. '..') from a string (file path). Also works for 2,3 consecutive slashes

- #### 20 `RemoveFinalDot()`

  RemoveFinalDot removes the final dot from a string

- #### 21 `RemoveTrailingDots()`

  RemoveTrailingDots removes the trailing dots from a string

- #### 22 `AddTrailingSlash()`

  Adds a final slash '/', if missing from a string (file path).

- #### 23 `RemoveDoubleSlash()`

  RemoveDoubleSlash removes multiple consecutive slashes (e.g. '//') from a string (file path). Also works for 2,3 consecutive slashes

- #### 24 `RemoveFinalSlash()`

  RemoveFinalSlash removes the final slash from a string

- #### 25 `FixUnderscores()`

  FixUnderscores removes multiple consecutive underscores (e.g. '_') from a string, and optionally also removes a final '_'.

- #### 26 `FixPath()`

  FixPath removes multiple consecutive slashes (e.g. '//') from a string and adds a final '/' if missing from a file path.

- #### 27 `FixPlotName()`

  FixPlotName replaces special characters in an input string (dollar-, at-, bracket-signs)

- #### 28 `ww.FnP_parser()`

  ParseFilePath pastes elements by slash, then removes Double Slashes '//' from a string and adds a final '/' if missing from a file path.

- #### 29 `PasteDirNameFromFlags()`

  Paste a dot (point) separated string from a list of inputs (that can be empty), and clean up the output string from dot multiplets (e.g: ..).

- #### 30 `extPDF()`

  add pdf as extension to a file name

- #### 31 `extPNG()`

  add pdf as extension to a file name

- #### 32 `param.list.2.fname()`

  Take a list of parameters and parse a string from their names and values.

- #### 33 `PasteOutdirFromFlags()`

  Paste OutDir from (1) a path and (2) a from a list of inputs (that can be empty), and clean up the output string from dot and forward slash multiplets (e.g: ..).

- #### 34 `flag.name_value()`

  Returns the name and its value, if its not FALSE.

- #### 35 `flag.nameiftrue()`

  Returns the name and its value, if its TRUE.

- #### 36 `flag.names_list()`

  Returns the name and value of each element in a list of parameters.

- #### 37 `flag.names_list.all.new()`

  Returns the name and value of each element in a list of parameters.

