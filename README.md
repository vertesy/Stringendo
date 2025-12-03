# Stringendo - a string parsing library ![status: active](https://raw.githubusercontent.com/vertesy/TheCorvinas/master/GitHub/Badges/active.svg)

String parsing functionalites for generating plotnames, filenames and path. Used by most of my other packages, including [CodeAndRoll2](https://github.com/vertesy/CodeAndRoll2) and [ggExpress](https://github.com/vertesy/ggExpress). 
Many functionalities were part of the formerly used [CodeAndRoll (v1)](https://github.com/vertesy/CodeAndRoll).



<br><br>

## Installation

Install directly from **GitHub** via **devtools** with one R command:

```R
# install.packages("devtools"); # If you don't have it.
require("devtools")
devtools::install_github(repo = "vertesy/Stringendo", ref = "main")
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

## List of Functions in Stringendo.R (76) 
Updated: 2025/12/03 10:48

- #### 1 `stopif()`
Stop execution if condition is TRUE.   The `stopif()` function stops the execution if the condition is `TRUE`. It is the opposite of  `stopifnot()`, which stops if the condition is not `TRUE`. This function is useful to increase  clarity in the code by removing double negations. 

- #### 2 `warnifnot()`
Issue warnings if conditions are not TRUE.   The `warnifnot()` function checks whether each condition passed to it is `TRUE`. If any  condition is not met, a warning is issued but execution continues. This is similar to  `stopifnot()`, which throws an error and halts execution, but `warnifnot()` only issues a  warning, allowing the program to proceed. 

- #### 3 `warnif()`
Issue a warning if condition is TRUE.   The `warnif()` function issues a warning if the condition is `TRUE`. It is the opposite of  `warnifnot()`, which warns if the condition is not `TRUE`. This function is useful for issuing  warnings when a certain condition is met. 

- #### 4 `ifExistsAndTrue()`
Check whether a variable exists and is TRUE. Returns `TRUE` if `varname` exists in the current environment and evaluates to  `TRUE`. If the variable is missing or not `TRUE`, the function returns `FALSE` and prints a  message describing the problem.

- #### 5 `ifExistsElse()`
Return a variable's value or a default if it does not exist. Returns the value of `varname` when it exists; otherwise returns `alternative`.  When `v` is `TRUE`, a message is printed indicating whether the variable was found.

- #### 6 `is.character.or.NULL()`
Check if Input is Character or NULL.   `is.character.or.NULL()` verifies if the provided input is either a character vector or NULL.

- #### 7 `is.numeric.or.logical()`
Check if Input is Numeric or Logical.   `is.numeric.or.logical()` checks if the provided input is either numeric or logical. 

- #### 8 `testNumericCompatible()`
Test if a Variable is Inherently Numeric ('0.1' as numeric).   This function checks if a given variable is inherently numeric. It returns TRUE if the variable  can be converted to a numeric value without loss of information and is not inherently a  character string, otherwise it returns FALSE. 

- #### 9 `"%!in%"()`
Negation of the `in` (w. grapes) operator. `%!in%` is used to test if elements of one vector are not present in another vector.  It is the negation of the `%in%` operator. This operator returns `TRUE` for elements  of `x` that are not in `y`. 

- #### 10 `substitute_deparse()`
Get Object Name as String.   `get_object_name()` captures the name of an input object and returns it as a string.  Replace `deparse\s*\(\s*substitute\s*\(([^()]+)\)\s*\)` to `substitute_deparse($1)`, then  `substitute\s*\(([^()]+)\)\s*\)` to the same. 

- #### 11 `message2()`
Message without collapsing. This function prints a message for each element in a character vector, instead of  collapsing them into a single line as done by, `message()`.

- #### 12 `imessage()`
imessage. A variant to message() pasting with white space, sibling of iprint().

- #### 13 `iprint()`
iprint. A more intelligent printing function that collapses any variable passed to it by white spaces.

- #### 14 `HasNames()`
Parse current date, dot separated..   Returns the current system date and time formatted as a character  string. The default format uses dot separated components, but any  format recognised by [base::format] can be supplied. 

- #### 15 `substrRight()`
substrRight. Take the right substring of a string

- #### 16 `ReplaceRepeatedDots()`
ReplaceRepeatedDots. ReplaceRepeatedDots collapses multiple consecutive dots (periods) in a string into a single dot.

- #### 17 `RemoveFinalDot()`
RemoveFinalDot. RemoveFinalDot removes the final dot from a string

- #### 18 `RemoveInitialDot()`
RemoveInitialDot. RemoveInitialDot removes the initial dot from a string.

- #### 19 `RemoveTrailingDots()`
RemoveTrailingDots. RemoveTrailingDots removes dots at the beginning and end of a string.

- #### 20 `ReplaceRepeatedSlashes()`
ReplaceRepeatedSlashes. ReplaceRepeatedSlashes replaces multiple consecutive slashes with a single slash.

- #### 21 `RemoveFinalSlash()`
RemoveFinalSlash. RemoveFinalSlash removes the final slash(es) from a string (file path).

- #### 22 `ReplaceRepeatedUnderscores()`
ReplaceRepeatedUnderscores. ReplaceRepeatedUnderscores replaces multiple consecutive underscores with a single underscore.

- #### 23 `RemoveFinalUnderscores()`
RemoveFinalUnderscores. RemoveFinalUnderscores removes trailing underscore(s) from a string.

- #### 24 `RemoveWhitespaces()`
RemoveWhitespaces. RemoveWhitespaces removes all whitespace characters from a string or replaces them with a specified value.

- #### 25 `ReplaceRepeatedWhitespaces()`
ReplaceRepeatedWhitespaces. ReplaceRepeatedWhitespaces collapses multiple consecutive whitespace characters into a single replacement.

- #### 26 `ReplaceSpecialCharacters()`
ReplaceSpecialCharacters. ReplaceSpecialCharacters replaces special characters '[]$@()' with dots.

- #### 27 `AddTrailingDotIfMissing()`
AddTrailingDotIfMissing. Adds a trailing dot ('.') to a string if it is missing.

- #### 28 `AddTrailingSlashIfMissing()`
AddTrailingSlashIfMissing. Adds a trailing slash ('/') to a string if it is missing.

- #### 29 `ppp()`
Paste by point. Paste by point

- #### 30 `pps()`
Paste by (forward) slash. Paste by (forward) slash

- #### 31 `ppcol()`
Paste by colon symbol.. Paste by colon symbol. "ppc" reserved for "comma".

- #### 32 `ppu()`
Paste by underscore. Paste by underscore

- #### 33 `ppnl()`
Paste by dash. Paste by dash

- #### 34 `kpp()`
Collapse and paste by point. Collapse by period (`.`)

- #### 35 `kppu()`
Collapse and paste by underscore. Collapse by underscore (`_`)

- #### 36 `kpps()`
Collapse and paste by (forward) slash. Collapse by forward slash (`/`)

- #### 37 `kppd()`
Collapse and paste by dash. Collapse by dash (`-`)

- #### 38 `kppws()`
Collapse and paste by white space. Collapse by white space (` `)

- #### 39 `kppc()`
Collapse and paste by a comma followed by a white space. Collapse by comma and white space (`, `)

- #### 40 `kpipe()`
Collapse and paste by pipe (|) and white spaces around it. Collapse by pipe (`|`) with surrounding spaces

- #### 41 `kpnl()`
Collapse and paste by newline (`\n`) preceded by a white space. Collapse by newline (`\n`) preceded by a white space

- #### 42 `kpwNames()`
Collapse and paste Elements With Names. This function takes a named vector and returns a string where each element is pasted   with its name. Elements are separated by a specified string, and name-element pairs are also   separated by a specified string. The default named vector is `c('a' = 1, 'b' = 2)`.

- #### 43 `kollapse()`
Kollapse. Collapses values and strings to one string (without a white space).  It also prints the results (good for a quick check)

- #### 44 `sppp()`
Simplified Paste by point. Simplified Paste by point

- #### 45 `spps()`
Simplified Paste by fwd slash. Simplified Paste by fwd slash

- #### 46 `sppu()`
Simplified Paste by underscore. Simplified Paste by underscore

- #### 47 `pad.na()`
pad.na. This function fills up a vector to a given length by appending NA-values at the end.    If the input vector's length is less than the provided length, the function pads the vector    with NA. If the vector's length is already equal to or greater than the given length, no change    will be made.

- #### 48 `percentile2value()`
percentile2value. Calculate what is the actual value of the N-th percentile in a distribution or set of numbers.  Useful for calculating cutoffs, and displaying them by whist()s "vline" parameter.

- #### 49 `parsepvalue()`
parsepvalue. Parse p-value from a number to a string.

- #### 50 `percentage_formatter()`
percentage_formatter. Parse a string of 0-100% from a number between 0 and 1. 

- #### 51 `format_number_h()`
Format numbers for human readability. Convert numeric input to character strings with  thousands separators and configurable decimal marks. 

- #### 52 `countDotOrUnderscoreSeparated()`
Identify the dominant separator in a string. Count dots, underscores, and white spaces in a string  to guess the most prevalent separator. 

- #### 53 `toCamelCase()`
Convert a String to camelCase. This function takes a string as input and converts it to camelCase format.  It splits the string into words using dots as separators, capitalizes the first letter of  each word (except the first word), and then concatenates them back together. 

- #### 54 `toUnderscoreSeparated()`
Convert a String to underscore_separated Format. This function converts a string from camelCase or dot-separated format to an underscore-separated format.  It can handle strings that are a combination of camelCase and dot-separated formats. The function replaces  dots with underscores and inserts an underscore before any uppercase letter that follows a lowercase letter.  It then converts all characters to lowercase. 

- #### 55 `toDotSeparated()`
Convert String to Dot Separated Name. Converts a string from camelCase or underscore_separated format to dot.separated.name format.  Inserts dots before each uppercase letter (except if it's the first character) or replaces underscores with dots,  and then converts the entire string to lowercase. 

- #### 56 `toSentence()`
Convert CamelCase to Sentence. Takes a camelCase string and converts it to a sentence format: space-separated,  with the first letter capitalized and no period at the end. 

- #### 57 `fix_special_characters_bash()`
Fix Special Characters for Bash. This function takes a string representing a path and escapes certain special  characters to make it compatible with Bash. Specifically, it escapes spaces,  opening parentheses, and closing parentheses by placing a backslash before them.

- #### 58 `ParseFullFilePath()`
Parse Full File Path. Constructs a full file path by combining a path, file name, and extension. It applies    string clean-up operations to each component and ensures proper formatting. 

- #### 59 `FixUnderscores()`
FixUnderscores. FixUnderscores removes multiple consecutive underscores from a string and optionally trims a trailing underscore.

- #### 60 `FixPath()`
FixPath. FixPath removes multiple consecutive slashes (e.g. '//') from a string and adds a final '/' if missing from a file path.

- #### 61 `FixPlotName()`
FixPlotName. FixPlotName replaces special characters in an input string (dollar-, at-, bracket-signs)

- #### 62 `ParseDirPath()`
ParseDirPath. ParseDirPath pastes elements by slash, then removes Double Slashes '//' from a    string and adds a final '/' if missing from a file path.

- #### 63 `PasteDirNameFromFlags()`
PasteDirNameFromFlags. Paste a dot (point) separated string from a list of inputs (that can be empty), and clean up the output string from dot multiplets (e.g: ..).

- #### 64 `extPDF()`
extPDF. add '.pdf' as extension to a file name

- #### 65 `extPNG()`
extPNG. add '.png' as extension to a file name

- #### 66 `parseParamStringWNames()`
Parse Parameter String with Names. This function parses a named vector and intermingles the names and values  into a single string, with specified separators for the odd and even elements. 

- #### 67 `params.2.fname()`
Convert Named Parameters to Filename. This function takes named parameters and converts them into a filename string with  specified separators and collapse characters. It excludes any parameters with NULL values. 

- #### 68 `param.list.2.fname()`
param.list.2.fname. Take a list of parameters and parse a string from their names and values.

- #### 69 `PasteOutdirFromFlags()`
PasteOutdirFromFlags. Paste OutDir from (1) a path and (2) a from a list of inputs (that can be empty), and clean up the output string from dot and forward slash multiplets (e.g: ..).

- #### 70 `flag.name_value()`
flag.name_value. Returns the name and its value, if its not FALSE.

- #### 71 `flag.nameiftrue()`
flag.nameiftrue. Returns the name and its value, if its TRUE.

- #### 72 `flag.names_list()`
flag.names_list. Returns the name and value of each element in a list of parameters.

- #### 73 `flag.names_list.all.new()`
flag.names_list.all.new. Returns the name and value of each element in a list of parameters.

- #### 74 `param.list.flag()`
param.list.flag. Returns the name and value of each element in a list of parameters.

- #### 75 `()`
parFlags. Create a string from the names of the (boolean) parameters (TRUE or FALSE) of true values.  Use it for Suffixing plot names with the parameters that were used for that plot.

- #### 76 `FormatAsExcelLink()`
parFlags2. Create a string from the names of the (boolean) parameters (TRUE or FALSE) of true values.  Use it for Suffixing plot names with the parameters that were used for that plot.

