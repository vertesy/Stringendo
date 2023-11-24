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

## List of Functions (47) 

Updated: 2023/11/24 16:40

- #### 1 `iprint()`

  iprint. A more intelligent printing function that collapses any variable passed to it by white spaces.

- #### 2 `idate()`

  Parse current date, dot separated.. Parse current date, dot separated.

- #### 3 `substrRight()`

  substrRight. Take the right substring of a string

- #### 4 `paste_w_names()`

  . This function takes a named vector and returns a string where each element is pasted with its name. Elements are separated by a specified string, and name-element pairs are also separated by a specified string. The default named vector is `c('a' = 1, 'b' = 2)`.

- #### 5 `kollapse()`

  kollapse. Collapses values and strings to one string (without a white space).  It also prints the results (good for a quick check)

- #### 6 `ppp()`

  Paste by point. Paste by point

- #### 7 `pps()`

  Paste by (forward) slash. Paste by (forward) slash

- #### 8 `ppu()`

  Paste by underscore. Paste by underscore

- #### 9 `ppd()`

  Paste by dash. Paste by dash

- #### 10 `kpp()`

  Collapse by point. Collapse by point

- #### 11 `kppu()`

  Collapse by underscore. Collapse by underscore

- #### 12 `kpps()`

  Collapse by (forward) slash. Collapse by (forward) slash

- #### 13 `kppd()`

  Collapse by dash. Collapse by dash

- #### 14 `sppp()`

  Simplified Paste by point. Simplified Paste by point

- #### 15 `spps()`

  Simplified Paste by fwd slash. Simplified Paste by fwd slash

- #### 16 `percentile2value()`

  percentile2value. Calculate what is the actual value of the N-th percentile in a distribution or set of numbers.  Useful for calculating cutoffs, and displaying them by whist()s "vline" paramter.

- #### 17 `parsepvalue()`

  parsepvalue. Parse p-value from a number to a string.

- #### 18 `eval_parse_kollapse()`

  eval_parse_kollapse. evaluate and parse (dyn_var_caller)

- #### 19 `percentage_formatter()`

  percentage_formatter. Parse a string of 0-100% from a number between 0 and 1. 

- #### 20 `toCamelCase()`

  Convert a String to camelCase. This function takes a string as input and converts it to camelCase format.  It splits the string into words using dots as separators, capitalizes the first letter of  each word (except the first word), and then concatenates them back together. 

- #### 21 `toUnderscoreSeparated()`

  Convert a String to underscore_separated Format. This function converts a string from camelCase or dot-separated format to an underscore-separated format.  It can handle strings that are a combination of camelCase and dot-separated formats. The function replaces  dots with underscores and inserts an underscore before any uppercase letter that follows a lowercase letter.  It then converts all characters to lowercase. 

- #### 22 `toDotSeparated()`

  Convert String to Dot Separated Name. Converts a string from camelCase or underscore_separated format to dot.separated.name format.  Inserts dots before each uppercase letter (except if it's the first character) or replaces underscores with dots,  and then converts the entire string to lowercase. 

- #### 23 `fix_special_characters_bash()`

  Fix Special Characters for Bash. This function takes a string representing a path and escapes certain special  characters to make it compatible with Bash. Specifically, it escapes spaces,  opening parentheses, and closing parentheses by placing a backslash before them.

- #### 24 `ReplaceSpecialCharacters()`

  ReplaceSpecialCharacters. ReplaceSpecialCharacters replaces '[]$@()' with dots

- #### 25 `AddTrailingDot()`

  AddTrailingDot. Adds a final slash '/', if missing from a string (file path).

- #### 26 `RemoveDoubleDot()`

  RemoveDoubleDot. RemoveDoubleDot removes multiple consecutive slashes (e.g. '..') from a string (file path). Also works for 2,3 consecutive slashes

- #### 27 `RemoveFinalDot()`

  RemoveFinalDot. RemoveFinalDot removes the final dot from a string

- #### 28 `RemoveTrailingDots()`

  RemoveTrailingDots. RemoveTrailingDots removes the trailing dots from a string

- #### 29 `AddTrailingSlash()`

  AddTrailingSlash. Adds a final slash '/', if missing from a string (file path).

- #### 30 `RemoveDoubleSlash()`

  RemoveDoubleSlash. RemoveDoubleSlash removes multiple consecutive slashes (e.g. '//') from a string (file path). Also works for 2,3 consecutive slashes

- #### 31 `RemoveFinalSlash()`

  RemoveFinalSlash. RemoveFinalSlash removes the final slash from a string

- #### 32 `FixUnderscores()`

  FixUnderscores. FixUnderscores removes multiple consecutive underscores (e.g. '_') from a string, and optionally also removes a final '_'.

- #### 33 `FixPath()`

  FixPath. FixPath removes multiple consecutive slashes (e.g. '//') from a string and adds a final '/' if missing from a file path.

- #### 34 `FixPlotName()`

  FixPlotName. FixPlotName replaces special characters in an input string (dollar-, at-, bracket-signs)

- #### 35 `ParseFilePath()`

  ParseFilePath. ParseFilePath pastes elements by slash, then removes Double Slashes '//' from a string and adds a final '/' if missing from a file path.

- #### 36 `ww.FnP_parser()`

  ww.FnP_parser. Internal Function. Parses the full path from the filename & location of the file.

- #### 37 `PasteDirNameFromFlags()`

  PasteDirNameFromFlags. Paste a dot (point) separated string from a list of inputs (that can be empty), and clean up the output string from dot multiplets (e.g: ..).

- #### 38 `extPDF()`

  extPDF. add '.pdf' as extension to a file name

- #### 39 `extPNG()`

  extPNG. add '.png' as extension to a file name

- #### 40 `param.list.2.fname()`

  param.list.2.fname. Take a list of parameters and parse a string from their names and values.

- #### 41 `PasteOutdirFromFlags()`

  PasteOutdirFromFlags. Paste OutDir from (1) a path and (2) a from a list of inputs (that can be empty), and clean up the output string from dot and forward slash multiplets (e.g: ..).

- #### 42 `flag.name_value()`

  flag.name_value. Returns the name and its value, if its not FALSE.

- #### 43 `flag.nameiftrue()`

  flag.nameiftrue. Returns the name and its value, if its TRUE.

- #### 44 `flag.names_list()`

  flag.names_list. Returns the name and value of each element in a list of parameters.

- #### 45 `flag.names_list.all.new()`

  flag.names_list.all.new. Returns the name and value of each element in a list of parameters.

- #### 46 `param.list.flag()`

  param.list.flag. Returns the name and value of each element in a list of parameters.

- #### 47 `parFlags()`

  parFlags. Create a string from the names of the (boolean) parameters (TRUE or FALSE) of true values.  Use it for Suffixing plot names with the parameters that were used for that plot.



