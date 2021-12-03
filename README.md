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

## List of functions

- #### idate 
Parse current date, dot separated.

- #### ppp 
Paste by point

- #### pps 
Paste by (forward) slash

- #### ppu 
Paste by underscore

- #### ppd 
Paste by dash

- #### kpp 
kollapse by point

- #### kppu 
kollapse by underscore

- #### kpps 
kollapse by (forward) slash

- #### kppd 
kollapse by dash

- #### sppp 
Simplified Paste by point

- #### percentile2value 
Calculate what is the actual value of the N-th percentile in a distribution or set of numbers. Useful for calculating cutoffs, and displaying them by whist()'s "vline" paramter.

- #### parsepvalue 
Parse p-value from a number to a string.

- #### eval_parse_kollapse 
evaluate and parse (dyn_var_caller)

- #### param.list.2.fname 
Take a list of parameters and parse a string from their names and values.

- #### PasteDirNameFromFlags 
Paste a dot (point) separated string from a list of inputs (that can be empty), and clean up the output string from dot multiplets (e.g: ..).

- #### PasteOutdirFromFlags 
Paste OutDir from (1) a path and (2) a from a list of inputs (that can be empty), and clean up the output string from dot and forward slash multiplets (e.g: ..).

- #### flag.name_value 
Returns the name and its value, if its not FALSE.

- #### flag.nameiftrue 
Returns the name and its value, if its TRUE.

- #### flag.names_list 
Returns the name and value of each element in a list of parameters.

- #### flag.names_list.all.new 
Returns the name and value of each element in a list of parameters.

- #### param.list.flag 
Returns the name and value of each element in a list of parameters.
