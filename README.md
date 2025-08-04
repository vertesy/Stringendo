# Stringendo - a string parsing library ![status: active](https://raw.githubusercontent.com/vertesy/TheCorvinas/master/GitHub/Badges/active.svg)

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

## List of Functions in Stringendo.R (67) 
Updated: 2024/10/24 11:02

- #### 1 `stopif()`
Stop execution if condition is TRUE.   The `stopif()` function stops the execution if the condition is `TRUE`. It is the opposite of  `stopifnot()`, which stops if the condition is not `TRUE`. This function is useful to increase  clarity in the code by removing double negations. 

- #### 2 `warnifnot()`
Issue warnings if conditions are not TRUE.   The `warningifnot()` function checks whether each condition passed to it is `TRUE`. If any  condition is not met, a warning is issued but the execution continues. This is similar to  `stopifnot()`, which throws an error and halts execution, but `warningifnot()` only issues a  warning, allowing the program to proceed. 

- #### 3 `warnif()`
Issue a warning if condition is TRUE.   The `warnif()` function issues a warning if the condition is `TRUE`. It is the opposite of  `warningifnot()`, which warns if the condition is not `TRUE`. This function is useful for issuing  warnings when a certain condition is met. 

- #### 4 `testNumericCompatible()`
Test if a Variable is Inherently Numeric ('0.1' as numeric).   This function checks if a given variable is inherently numeric. It returns TRUE if the variable  can be converted to a numeric value without loss of information and is not inherently a  character string, otherwise it returns FALSE. 

- #### 5 `"%!in%"()`
Negation of the `in` (w. grapes) operator. `%!in%` is used to test if elements of one vector are not present in another vector.  It is the negation of the `%in%` operator. This operator returns `TRUE` for elements  of `x` that are not in `y`. 

- #### 6 `message2()`
Message without collapsing. This function prints a message for each element in a character vector, instead of  collapsing them into a single line as done by, `message()`.

- #### 7 `imessage()`
imessage. A variant to message() pasting with white space, sibling of iprint().

- #### 8 `HasNames()`
iprint. A more intelligent printing function that collapses any variable passed to it by white spaces.

- #### 9 `substrRight()`
substrRight. Take the right substring of a string

- #### 10 `ReplaceRepeatedDots()`
ReplaceRepeatedDots. ReplaceRepeatedDots removes multiple consecutive slashes (e.g. '..') from a string (file path).

- #### 11 `RemoveFinalDot()`
RemoveFinalDot. RemoveFinalDot removes the final dot from a string

- #### 12 `RemoveInitialDot()`
RemoveInitialDot. RemoveInitialDot removes the initial dot from a string.

- #### 13 `RemoveTrailingDots()`
RemoveTrailingDots. RemoveTrailingDots removes dots at the beginning and end of a string.

- #### 14 `ReplaceRepeatedSlashes()`
ReplaceRepeatedSlashes. ReplaceRepeatedSlashes replaces multiple consecutive slashes with a single slash.

- #### 15 `RemoveFinalSlash()`
RemoveFinalSlash. RemoveFinalSlash removes the final slash(es) from a string (file path).

- #### 16 `ReplaceRepeatedUnderscores()`
ReplaceRepeatedUnderscores. ReplaceRepeatedUnderscores replaces multiple consecutive slashes with a single slash.

- #### 17 `RemoveFinalUnderscores()`
RemoveFinalUnderscores. RemoveFinalUnderscores removes the final slash(es) from a string (file path).

- #### 18 `RemoveWhitespaces()`
RemoveWhitespaces. RemoveWhitespaces replaces any nr of white spaces.

- #### 19 `ReplaceRepeatedWhitespaces()`
ReplaceRepeatedWhitespaces. ReplaceRepeatedWhitespaces replaces multiple consecutive white spaces with a single one.

- #### 20 `ReplaceSpecialCharacters()`
ReplaceSpecialCharacters. ReplaceSpecialCharacters replaces special characters '[]$@()' with dots.

- #### 21 `AddTrailingDotIfNonePresent()`
AddTrailingDotIfNonePresent. Adds a final slash '/', if missing from a string (file path).

- #### 22 `AddTrailingSlashfNonePresent()`
AddTrailingSlashfNonePresent. Adds a final slash '/', if missing from a string (file path).

- #### 23 `ppp()`
Paste by point. Paste by point

- #### 24 `pps()`
Paste by (forward) slash. Paste by (forward) slash

- #### 25 `ppu()`
Paste by underscore. Paste by underscore

- #### 26 `pnl()`
Paste by dash. Paste by dash

- #### 27 `kpp()`
Collapse and paste by point. Collapse by point

- #### 28 `kppu()`
Collapse and paste by underscore. Collapse by underscore

- #### 29 `kpps()`
Collapse and paste by (forward) slash. Collapse by (forward) slash

- #### 30 `kppd()`
Collapse and paste by dash. Collapse by dash

- #### 31 `kppws()`
Collapse and paste by white space. Collapse by white space

- #### 32 `kppc()`
Collapse and paste by comma (and white space). Collapse by white space

- #### 33 `kpipe()`
Collapse and paste by pipe (|) and white spaces around it. Collapse by white space

- #### 34 `knl()`
Collapse and paste by newline (`\n`) preceded by a white space. Collapse by white space

- #### 35 `kpwNames()`
Collapse and paste Elements With Names. This function takes a named vector and returns a string where each element is pasted   with its name. Elements are separated by a specified string, and name-element pairs are also   separated by a specified string. The default named vector is `c('a' = 1, 'b' = 2)`.

- #### 36 `kollapse()`
Kollapse. Collapses values and strings to one string (without a white space).  It also prints the results (good for a quick check)

- #### 37 `sppp()`
Simplified Paste by point. Simplified Paste by point

- #### 38 `spps()`
Simplified Paste by fwd slash. Simplified Paste by fwd slash

- #### 39 `sppu()`
Simplified Paste by underscore. Simplified Paste by underscore

- #### 40 `percentile2value()`
percentile2value. Calculate what is the actual value of the N-th percentile in a distribution or set of numbers.  Useful for calculating cutoffs, and displaying them by whist()s "vline" parameter.

- #### 41 `parsepvalue()`
parsepvalue. Parse p-value from a number to a string.

- #### 42 `format_number_h()`
percentage_formatter. Parse a string of 0-100% from a number between 0 and 1. 

- #### 43 `countDotOrUnderscoreSeparated()`
Count Dots or Underscores in a String and return.   This function counts the number of "." characters in a given string. 

- #### 44 `toCamelCase()`
Convert a String to camelCase. This function takes a string as input and converts it to camelCase format.  It splits the string into words using dots as separators, capitalizes the first letter of  each word (except the first word), and then concatenates them back together. 

- #### 45 `toUnderscoreSeparated()`
Convert a String to underscore_separated Format. This function converts a string from camelCase or dot-separated format to an underscore-separated format.  It can handle strings that are a combination of camelCase and dot-separated formats. The function replaces  dots with underscores and inserts an underscore before any uppercase letter that follows a lowercase letter.  It then converts all characters to lowercase. 

- #### 46 `toDotSeparated()`
Convert String to Dot Separated Name. Converts a string from camelCase or underscore_separated format to dot.separated.name format.  Inserts dots before each uppercase letter (except if it's the first character) or replaces underscores with dots,  and then converts the entire string to lowercase. 

- #### 47 `toSentence()`
Convert CamelCase to Sentence. Takes a camelCase string and converts it to a sentence format: space-separated,  with the first letter capitalized and no period at the end. 

- #### 48 `fix_special_characters_bash()`
Fix Special Characters for Bash. This function takes a string representing a path and escapes certain special  characters to make it compatible with Bash. Specifically, it escapes spaces,  opening parentheses, and closing parentheses by placing a backslash before them.

- #### 49 `ParseFullFilePath()`
Parse Full File Path. Constructs a full file path by combining a path, file name, and extension. It applies    string clean-up operations to each component and ensures proper formatting. 

- #### 50 `FixUnderscores()`
FixUnderscores. FixUnderscores removes multiple consecutive underscores (e.g. '_') from a string, and optionally also removes a final '_'.

- #### 51 `FixPath()`
FixPath. FixPath removes multiple consecutive slashes (e.g. '//') from a string and adds a final '/' if missing from a file path.

- #### 52 `FixPlotName()`
FixPlotName. FixPlotName replaces special characters in an input string (dollar-, at-, bracket-signs)

- #### 53 `ParseDirPath()`
ParseDirPath. ParseDirPath pastes elements by slash, then removes Double Slashes '//' from a    string and adds a final '/' if missing from a file path.

- #### 54 `PasteDirNameFromFlags()`
PasteDirNameFromFlags. Paste a dot (point) separated string from a list of inputs (that can be empty), and clean up the output string from dot multiplets (e.g: ..).

- #### 55 `extPDF()`
extPDF. add '.pdf' as extension to a file name

- #### 56 `extPNG()`
extPNG. add '.png' as extension to a file name

- #### 57 `parseParamStringWNames()`
Parse Parameter String with Names. This function parses a named vector and intermingles the names and values  into a single string, with specified separators for the odd and even elements. 

- #### 58 `params.2.fname()`
Convert Named Parameters to Filename. This function takes named parameters and converts them into a filename string with  specified separators and collapse characters. It excludes any parameters with NULL values. 

- #### 59 `param.list.2.fname()`
param.list.2.fname. Take a list of parameters and parse a string from their names and values.

- #### 60 `PasteOutdirFromFlags()`
PasteOutdirFromFlags. Paste OutDir from (1) a path and (2) a from a list of inputs (that can be empty), and clean up the output string from dot and forward slash multiplets (e.g: ..).

- #### 61 `flag.name_value()`
flag.name_value. Returns the name and its value, if its not FALSE.

- #### 62 `flag.nameiftrue()`
flag.nameiftrue. Returns the name and its value, if its TRUE.

- #### 63 `flag.names_list()`
flag.names_list. Returns the name and value of each element in a list of parameters.

- #### 64 `flag.names_list.all.new()`
flag.names_list.all.new. Returns the name and value of each element in a list of parameters.

- #### 65 `param.list.flag()`
param.list.flag. Returns the name and value of each element in a list of parameters.

- #### 66 `()`
parFlags. Create a string from the names of the (boolean) parameters (TRUE or FALSE) of true values.  Use it for Suffixing plot names with the parameters that were used for that plot.

- #### 67 `FormatAsExcelLink()`
parFlags2. Create a string from the names of the (boolean) parameters (TRUE or FALSE) of true values.  Use it for Suffixing plot names with the parameters that were used for that plot.




