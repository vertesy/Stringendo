# ____________________________________________________________________
# Stringendo ----
# ____________________________________________________________________
# String parsing functions and flow control.
# Used by all of my packages, for input assertions and generating plotnames, filenames, etc.

# devtools::check_man("~/GitHub/Packages/Stringendo")
# devtools::load_all("~/GitHub/Packages/Stringendo")
# devtools::document("~/GitHub/Packages/Stringendo")
# file.edit("~/GitHub/Packages/Stringendo/Development/Create_the_Stringendo_Package.R")

# try(source("~/GitHub/Packages/Stringendo/R/Stringendo.R"), silent = TRUE)
# try(source("https://raw.githubusercontent.com/vertesy/Stringendo/main/Stringendo.R"), silent = TRUE)


# ______________________________________________________________________________________________----
# Flow control functions ----
# _________________________________________________________________________________________________



# ______________________________________________________________________________________________________________________________
#' @title Stop execution if condition is TRUE
#'
#' @description
#' The `stopif()` function stops the execution if the condition is `TRUE`. It is the opposite of
#' `stopifnot()`, which stops if the condition is not `TRUE`. This function is useful to increase
#' clarity in the code by removing double negations.
#'
#' @param ... Logical conditions to be checked. Each condition must evaluate to a logical vector.
#'   Default: none. Named arguments will be used as error messages.
#'
#' @return The function stops execution if any condition evaluates to `TRUE`.
#'
#' @examples
#' stopif(6 < 4, 6 > 5)
#' stopif("custom message" = 6 > 5)
#'
#' @export
stopif <- function(...) {
  args <- list(...)  # Capture all conditions

  for (i in seq_along(args)) {
    condition <- args[[i]]
    name <- names(args)[i]

    # Check if condition is logical and TRUE
    if (is.logical(condition) && all(condition, na.rm = TRUE)) {
      # Use the provided name as a custom error message if it exists,
      # otherwise, use the condition's expression
      message <- if (!is.null(name) && nzchar(name)) {
        paste(name, "is TRUE")
      } else {
        paste(deparse(match.call()[[i + 1]]), "is TRUE")
      }
      stop(message, call. = FALSE)
    }
  }

  invisible()  # No visible output
}



# ______________________________________________________________________________________________________________________________
#' @title Issue warnings if conditions are not TRUE
#'
#' @description
#' The `warnifnot()` function checks whether each condition passed to it is `TRUE`. If any
#' condition is not met, a warning is issued but execution continues. This is similar to
#' `stopifnot()`, which throws an error and halts execution, but `warnifnot()` only issues a
#' warning, allowing the program to proceed.
#'
#' @param ... Logical conditions to be checked. Each condition must evaluate to a logical vector.
#'   Named arguments will use the name as the warning message.
#'
#' @return The function returns `invisible()`, but issues warnings for each condition that evaluates
#'   to `FALSE` or contains `NA`.
#'
#' @examples
#' warnifnot(6 < 4, 6 > 5)
#' warnifnot("custom message" = 6 < 4)
#'
#' @export
warnifnot <- function(...) {
  args <- list(...)  # Capture all conditions

  for (i in seq_along(args)) {
    condition <- args[[i]]
    name <- names(args)[i]

    # Issue a warning if the condition is FALSE or contains NA
    if (!(is.logical(condition) && all(condition, na.rm = TRUE))) {
      # Use the provided name as a custom warning message if it exists,
      # otherwise, use the condition's expression
      message <- if (!is.null(name) && nzchar(name)) {
        paste(name, "is not TRUE")
      } else {
        paste(deparse(match.call()[[i + 1]]), "is not TRUE")
      }
      warning(message, call. = FALSE, immediate. = TRUE)
    }
  }

  invisible()  # No visible output
}


# ______________________________________________________________________________________________________________________________
#' @title Issue a warning if condition is TRUE
#'
#' @description
#' The `warnif()` function issues a warning if the condition is `TRUE`. It is the opposite of
#' `warnifnot()`, which warns if the condition is not `TRUE`. This function is useful for issuing
#' warnings when a certain condition is met.
#'
#' @param ... Logical conditions to be checked. Each condition must evaluate to a logical vector.
#'   Named arguments will use the name as the warning message.
#'
#' @return The function returns `invisible()`, but issues warnings for each condition that evaluates
#'   to `TRUE`.
#'
#' @examples
#' warnif(6 > 4, 6 > 5)
#' warnif("custom message" = 6 > 5)
#'
#' @export
warnif <- function(...) {
  args <- list(...)  # Capture all conditions

  for (i in seq_along(args)) {
    condition <- args[[i]]
    name <- names(args)[i]

    # Issue a warning if the condition is TRUE
    if (is.logical(condition) && all(condition, na.rm = TRUE)) {
      # Use the provided name as a custom warning message if it exists,
      # otherwise, use the condition's expression
      message <- if (!is.null(name) && nzchar(name)) {
        paste(name, "is TRUE")
      } else {
        paste(deparse(match.call()[[i + 1]]), "is TRUE")
      }
      warning(message, call. = FALSE, immediate. = TRUE)
    }
  }

  invisible()  # No visible output
}



# _______________________________________________________________________________________
#' @title ifExistsAndTrue
#'
#' @description Checks if a variable is defined, and its value is TRUE, else returns FALSE, and
#' prints a message.
#' @param varname Name of the variable
#'
#' @examples ifExistsAndTrue("pi"); ifExistsAndTrue("pi22")
#'
#' @export

ifExistsAndTrue <- function(varname = "pi" ) {
  x = FALSE
  if (exists(varname)) {
    if (isTRUE(get(varname)))  {x = TRUE} else {x = FALSE; iprint(varname, " exists, but != TRUE; ", get(varname))}
  }
  return(x)
}


# _______________________________________________________________________________________
#' @title ifExistsElse
#'
#' @description Checks if a variable is defined, else returns an alternative value.
#' @param varname Name of the variable
#' @param alternative Alternative value to return if the variable is not defined
#' @param v Print messages. Default is FALSE.
#'
#' @examples ifExistsAndTrue("pi"); ifExistsAndTrue("pi22")
#'
#' @export
ifExistsElse <- function(varname, alternative = "define an alternative", v = F ) {
  if(!is.character(varname)) varname <- substitute(varname)
  if(v) message("Checking if ", varname, " exists.")
  if(exists(varname)) get(varname) else alternative
}


# ______________________________________________________________________________________________________________________________
#' @title Check if Input is Character or NULL
#'
#' @description
#' `is.character.or.NULL()` verifies if the provided input is either a character vector or NULL.
#' @param x The input to check.
#'
#' @return Returns `TRUE` if `x` is either a character vector or `NULL`, otherwise `FALSE`.
#'
#' @examples
#' is.character.or.NULL(NULL)      # TRUE
#' is.character.or.NULL("example") # TRUE
#' is.character.or.NULL(123)       # FALSE
#'
#' @export
is.character.or.NULL <- function(x) is.null(x) || is.character(x)


# ______________________________________________________________________________________________________________________________
#' @title Check if Input is Numeric or Logical
#'
#' @description
#' `is.numeric.or.logical()` checks if the provided input is either numeric or logical.
#'
#' @param x The input to check.
#'
#' @return Returns `TRUE` if `x` is either numeric or logical, otherwise `FALSE`.
#'
#' @examples
#' is.numeric.or.logical(123)      # TRUE
#' is.numeric.or.logical(TRUE)     # TRUE
#' is.numeric.or.logical("text")   # FALSE
#'
#' @export
is.numeric.or.logical <- function(x) {
  is.numeric(x) || is.logical(x)
}



# ______________________________________________________________________________________________________________________________
#' @title Test if a Variable is Inherently Numeric ('0.1' as numeric)
#'
#' @description
#' This function checks if a given variable is inherently numeric. It returns TRUE if the variable
#' can be converted to a numeric value without loss of information and is not inherently a
#' character string, otherwise it returns FALSE.
#'
#' @param x The variable to be tested. Default: NA.
#'
#' @return A logical value indicating whether the input is inherently numeric.
#'
#' @examples
#' testNumericCompatible(0.1) # Should return TRUE
#' testNumericCompatible("0.1") # Should return TRUE
#' testNumericCompatible("apple") # Should return FALSE
#' testNumericCompatible("arma.0.1") # Should return FALSE
testNumericCompatible <- function(x) {
  stopifnot(is.numeric(x) || is.character(x))
  suppressWarnings({
    x_is_numeric <- !is.na(as.numeric(x)) & is.numeric(as.numeric(x))
  })
  return(x_is_numeric)
}


# _____________________________________________________________________________________________________________________________ f f_
#' @title Negation of the `in` (w. grapes) operator
#'
#' @description `%!in%` is used to test if elements of one vector are not present in another vector.
#' It is the negation of the `%in%` operator. This operator returns `TRUE` for elements
#' of `x` that are not in `y`.
#'
#' @param x A vector of values to be matched.
#' @param y A vector of values to be matched against.
#' @return A logical vector indicating if elements in `x` are not present in `y`.
#' @examples
#' c(1, 2, 3) %!in% c(2, 4, 6)
#' # [1]  TRUE FALSE  TRUE
#' @export
"%!in%" <- function(x, y) !("%in%"(x, y))



# ______________________________________________________________________________________________----
# Generic auxiliary functions ----
# _________________________________________________________________________________________________


#' @title Get Object Name as String
#'
#' @description
#' `get_object_name()` captures the name of an input object and returns it as a string.
#' Replace `deparse\s*\(\s*substitute\s*\(([^()]+)\)\s*\)` to `substitute_deparse($1)`, then
#' `substitute\s*\(([^()]+)\)\s*\)` to the same.
#'
#' @param x The object whose name you want to capture.
#'
#' @return Returns the name of `obj` as a string.
#'
#' @examples
#' my_var <- 10
#' get_object_name(my_var)  # "my_var"
#'
#' @export
substitute_deparse <- function(x) deparse(substitute(x))


# ______________________________________________________________________________________________________________________________
#' @title Message without collapsing
#'
#' @description This function prints a message for each element in a character vector, instead of
#' collapsing them into a single line as done by, `message()`.
#' @param vec A character vector to be printed.
#'
#' @examples message2(c("Hello", "world", "!", "I", "am", "here"))
#' @export
message2 <- function(vec) for (item in vec) message(item)

# ______________________________________________________________________________________________________________________________
#' @title imessage
#' @description A variant to message() pasting with white space, sibling of iprint().
#' @param ... Variables (strings, vectors) to be collapsed in consecutively.
#' @param collapse Separator to be used for collapsing. Default: " "
#'
#' @examples iprint("Hello ", "you ", 3, ", ", 11, " year old kids.")
#' @export

imessage <- function(..., collapse = " ") {
  argument_list <- c(...)
  message(paste(argument_list, collapse = collapse))
}


# ______________________________________________________________________________________________________________________________
#' @title iprint
#' @description A more intelligent printing function that collapses any variable passed to it by white spaces.
#' @param ... Variables (strings, vectors) to be collapsed in consecutively.
#' @examples iprint("Hello ", "you ", 3, ", ", 11, " year old kids.")
#' @export

iprint <- function(...) {
  argument_list <- c(...)
  print(paste(argument_list, collapse = " "))
}

# _________________________________________________________________________________________________
#' @title Parse current date, dot separated.
#'
#' @param Format Date format. Default: c("%Y.%m.%d_%H.%M", "%Y.%m.%d_%Hh")[2]
#' @export

idate <- function(Format = c("%Y.%m.%d_%H.%M", "%Y.%m.%d_%Hh")[2]) {
  format(Sys.time(), format = Format)
}


# _________________________________________________________________________________________________
#' @title Check if Vector Has Names
#'
#' @param x Vector to check.
#' @return Logical indicating if `x` has names.
#' @examples
#' HasNames(c(a = 1, b = 2))
#' HasNames(1:3)
#' @export
HasNames <- function(x) !is.null(names(x))


# _________________________________________________________________________________________________
#' @title substrRight
#'
#' @description Take the right substring of a string
#' @param x a character vector.
#' @param n integer. The number of elements on the right to be kept.
#' @export
#' @examples substrRight("Not cool", n = 4)
substrRight <- function(x, n) {
  substr(x, nchar(x) - n + 1, nchar(x))
}



# ______________________________________________________________________________________________----
# Special character removal  -------------------------------------------------------------------------------------------------


#' @title ReplaceRepeatedDots
#'
#' @description ReplaceRepeatedDots removes multiple consecutive slashes (e.g. '..') from a string (file path).
#' @param string The string (file name or path) potentially having multiple dots
#' @examples ReplaceRepeatedDots(string = "stairway..to...heaven....") # replace by a single dot.
#'
#' @export
ReplaceRepeatedDots <- function(string) {
  gsub(pattern = "\\.+", replacement = "\\.", x = string)
}

# _________________________________________________________________________________________________
#' @title RemoveFinalDot
#'
#' @description RemoveFinalDot removes the final dot from a string
#' @param string The file path potentially having Final Dot
#' @examples RemoveFinalDot(string = "stairway..to...heaven...")
#'
#' @export
RemoveFinalDot <- function(string) {
  gsub(pattern = "\\.+$", replacement = "", x = string)
}

# _________________________________________________________________________________________________
#' @title RemoveInitialDot
#'
#' @description RemoveInitialDot removes the initial dot from a string.
#' @param string The string potentially having an initial dot.
#' @examples RemoveInitialDot(string = ".example...")
#' @return A string with the initial dot removed.
#' @export
RemoveInitialDot <- function(string) {
  gsub(pattern = "^\\.+", replacement = "", x = string)
}

# _________________________________________________________________________________________________
#' @title RemoveTrailingDots
#'
#' @description RemoveTrailingDots removes dots at the beginning and end of a string.
#' @param string The string potentially having unnecessary dots at the beginning or end.
#' @examples RemoveTrailingDots(string = "...stairway.to..heaven.")
#' @return A string with the trailing dots removed.
#' @export
RemoveTrailingDots <- function(string = "...stairway.to..heaven.") {
  RemoveFinalDot(RemoveInitialDot(string))
}


# _________________________________________________________________________________________________
#' @title ReplaceRepeatedSlashes
#'
#' @description ReplaceRepeatedSlashes replaces multiple consecutive slashes with a single slash.
#' @param string The string (file path) potentially having repeated slashes.
#' @examples ReplaceRepeatedSlashes(string = "path//to//folder")
#' @return A string with repeated slashes replaced by a single slash.
#' @export
ReplaceRepeatedSlashes <- function(string) {
  gsub(pattern = "//+", replacement = "/", x = string)
}


# _________________________________________________________________________________________________
#' @title RemoveFinalSlash
#'
#' @description RemoveFinalSlash removes the final slash(es) from a string (file path).
#' @param string The string (file path) potentially having a final slash.
#' @examples RemoveFinalSlash(string = "path/to/folder/")
#' @return A string with the final slash removed.
#' @export
RemoveFinalSlash <- function(string) {
  gsub(pattern = "/+$", replacement = "", x = string)
}


# _________________________________________________________________________________________________
#' @title ReplaceRepeatedUnderscores
#'
#' @description ReplaceRepeatedUnderscores replaces multiple consecutive slashes with a single slash.
#' @param string The string (file path) potentially having repeated slashes.
#' @examples ReplaceRepeatedUnderscores(string = "path//to//folder")
#' @return A string with repeated slashes replaced by a single slash.
#' @export
ReplaceRepeatedUnderscores <- function(string) {
  gsub(pattern = "_+", replacement = "_", x = string)
}


# _________________________________________________________________________________________________
#' @title RemoveFinalUnderscores
#'
#' @description RemoveFinalUnderscores removes the final slash(es) from a string (file path).
#' @param string The string (file path) potentially having a final slash.
#' @examples RemoveFinalUnderscores(string = "path/to/folder/")
#' @return A string with the final slash removed.
#' @export
RemoveFinalUnderscores <- function(string) {
  gsub(pattern = "_+$", replacement = "", x = string)
}


# _________________________________________________________________________________________________
#' @title RemoveWhitespaces
#'
#' @description RemoveWhitespaces replaces any nr of white spaces.
#' @param string The string (file path) potentially having repeated slashes.
#' @param replacement The string to replace the white spaces with. Default: ''.
#'
#' @examples RemoveWhitespaces(string = "path   To    Folder")
#' @return A string with repeated slashes replaced by a single slash.
#' @export
RemoveWhitespaces <- function(string, replacement = "") {
  gsub(pattern = " +", replacement = replacement, x = string)
}



# _________________________________________________________________________________________________
#' @title ReplaceRepeatedWhitespaces
#'
#' @description ReplaceRepeatedWhitespaces replaces multiple consecutive white spaces with a single one.
#' @param string The string (file path) potentially having repeated slashes.
#' @param replacement The string to replace the white spaces with. Default: ''.
#'
#' @examples ReplaceRepeatedWhitespaces(string = "path   to    folder")
#' @return A string with repeated slashes replaced by a single slash.
#' @export
ReplaceRepeatedWhitespaces <- function(string, replacement = " ") {
  gsub(pattern = " +", replacement = replacement, x = string)
}



# _________________________________________________________________________________________________
#' @title ReplaceSpecialCharacters
#'
#' @description ReplaceSpecialCharacters replaces special characters '[]$@()' with dots.
#' @param string The string potentially having special characters.
#' @param replacement The character to replace special characters with.
#' @param remove_dots If TRUE, all dots are removed from the string (overwrites if replacement is a dot).
#' @examples ReplaceSpecialCharacters(string = "obj@meta$alpha[[3]]")
#' @return A string with special characters replaced by dots.
#' @export

ReplaceSpecialCharacters <- function(string = "obj@meta$alpha[[3]]", replacement = ".", remove_dots = FALSE) {
  x <- gsub(x = string, pattern = ",|\\||\\@|\\[|\\]|\\$|\\/\\(\\)|\\\\", replacement = replacement)
  x <- ReplaceRepeatedWhitespaces(x)
  if (remove_dots) x <- gsub(x = x, pattern = "\\.", replacement = "")
  ReplaceRepeatedDots(x)
}


# ______________________________________________________________________________________________----
# Special character addition -------------------------------------------------------------------------------------------------


#' @title AddTrailingDotIfNonePresent
#'
#' @description Adds a final dot '.', if missing from a string (file path).
#' @param string The file path potentially missing the trailing dot
#' @examples AddTrailingDotIfNonePresent(string = "stairway.to.heaven")
#'
#' @export
AddTrailingDotIfNonePresent <- function(string = "stairway.to.heaven") {
  LastChr <- substr(string, nchar(string), nchar(string))
  if (LastChr != ".") {
    string <- paste0(string, ".")
  }
  return(string)
}



#' @title AddTrailingSlashfNonePresent
#'
#' @description Adds a final slash '/', if missing from a string (file path).
#' @param string The file path potentially missing the trailing slash
#' @examples AddTrailingSlashfNonePresent(string = "stairway/to/heaven")
#'
#' @export
AddTrailingSlashfNonePresent <- function(string = "stairway/to/heaven") {
  LastChr <- substr(string, nchar(string), nchar(string))
  if (!LastChr == "/") {
    string <- paste0(string, "/")
  }
  return(string)
}


# ______________________________________________________________________________________________----
# Paste -----------------------------------------------------------------------


#' @title Paste by point
#' @description Paste by point
#' @param ... Multiple simple variables to parse.
#' @export
ppp <- function(...) {
  paste(..., sep = ".")
}


# _________________________________________________________________________________________________
#' @title Paste by (forward) slash
#' @description Paste by (forward) slash
#' @param ... Multiple simple variables to parse.
#' @export
pps <- function(...) {
  paste(..., sep = "/")
}

# _________________________________________________________________________________________________
#' @title Paste by colon symbol.
#' @description Paste by colon symbol. "ppc" reserved for "comma".
#' @param ... Multiple simple variables to parse.
#' @export
ppcol <- function(...) {
  paste(..., sep = ":")
}

# _________________________________________________________________________________________________
#' @title Paste by underscore
#' @description Paste by underscore
#' @param ... Multiple simple variables to parse.
#' @export
ppu <- function(...) {
  paste(..., sep = "_")
}

# _________________________________________________________________________________________________
#' @title Paste by dash
#' @description Paste by dash
#' @param ... Multiple simple variables to parse.
#' @export
ppd <- function(...) {
  paste(..., sep = "-")
}


# _________________________________________________________________________________________________
#' @title Paste by pipe (|) and white space around it
#'
#' @param ... Multiple simple variables to parse.
#' @export
ppipe <- function(...) {
  paste(..., sep = " | ")
}


# _________________________________________________________________________________________________
#' @title Paste by new line
#'
#' @param ... Multiple simple variables to parse.
#' @export
pnl <- function(...) {
  paste(..., sep = " \n")
}




# ______________________________________________________________________________________________----
# Collapse (and paste) -----------------------------------------------------------------------

#' @title Collapse and paste by point
#' @description Collapse by point
#' @param ... Multiple simple variables to parse.
#' @examples kpp("A", 1:2, "end")
#' @export
kpp <- function(...) {
  paste(c(...), sep = ".", collapse = ".")
}

# _________________________________________________________________________________________________
#' @title Collapse and paste by underscore
#' @description Collapse by underscore
#' @param ... Multiple simple variables to parse.
#' @examples kppu("A", 1:2, "end")
#' @export
kppu <- function(...) {
  paste(c(...), sep = "_", collapse = "_")
}

# _________________________________________________________________________________________________
#' @title Collapse and paste by (forward) slash
#' @description Collapse by (forward) slash
#' @param ... Multiple simple variables to parse.
#' @examples kpps("A", 1:2, "end")
#' @export
kpps <- function(...) {
  paste(c(...), sep = "/", collapse = "/")
}


# _________________________________________________________________________________________________
#' @title Collapse and paste by dash
#' @description Collapse by dash
#' @param ... Multiple simple variables to parse.
#' @examples kppd("A", 1:2, "end")
#' @export
kppd <- function(...) {
  paste(c(...), sep = "-", collapse = "-")
}

# _________________________________________________________________________________________________
#' @title Collapse and paste by white space
#' @description Collapse by white space
#' @param ... Multiple simple variables to parse.
#' @examples kppws("A", 1:2, "end")
#' @export
kppws <- function(...) {
  paste(c(...), sep = " ", collapse = " ")
}


# _________________________________________________________________________________________________
#' @title Collapse and paste by comma (and white space)
#' @description Collapse by white space
#' @param ... Multiple simple variables to parse.
#' @examples kppc("A", 1:2, "end")
#' @export
kppc <- function(...) {
  paste(c(...), sep = ", ", collapse = ", ")
}

# _________________________________________________________________________________________________
#' @title Collapse and paste by pipe (|) and white spaces around it
#' @description Collapse by white space
#' @param ... Multiple simple variables to parse.
#' @examples kpipe("A", 1:2, "end")
#' @export
kpipe <- function(...) {
  paste(c(...), sep = " | ", collapse = " | ")
}

# _________________________________________________________________________________________________
#' @title Collapse and paste by newline (`\n`) preceded by a white space
#' @description Collapse by white space
#' @param ... Multiple simple variables to parse.
#' @examples knl("A", 1:2, "end")
#' @export
knl <- function(...) {
  paste(c(...), sep = " \n", collapse = " \n")
}


# _________________________________________________________________________________________________
#' @title Collapse and paste Elements With Names
#'
#' @description This function takes a named vector and returns a string where each element is pasted
#'  with its name. Elements are separated by a specified string, and name-element pairs are also
#'  separated by a specified string. The default named vector is `c('a' = 1, 'b' = 2)`.
#' @param x A named vector. Default is `c('a' = 1, 'b' = 2)`.
#' @param sep1 A character string to separate the names from the elements. Default is ":".
#' @param sep2 A character string to separate the name-element pairs in the
#' resulting string. Default is " ".
#' @param prefix A character string to add to the beginning of the resulting string. Default is NULL.
#' @param suffix A character string to add to the end of the resulting string. Default is NULL.
#'
#' @examples kpwNames(c("a" = 1, "b" = 2))
#' @export
kpwNames <- function(x = c("a" = 1, "b" = 2), sep1 = ": ", sep2 = " | ", prefix = NULL, suffix = NULL) {

  if(is.table(x) & length(dim(x))) {
    # Convert one dimensional table to vector preserving the names
    nmz <- names(x)
    x <- as.vector(x)
    names(x) <- nmz
  }

  stopifnot(
    is.vector(x),
    HasNames(x)
  )
  x <- paste0(names(x), sep1, x, collapse = sep2)
  paste0(prefix, x, suffix)
}



# _________________________________________________________________________________________________
#' @title Kollapse
#'
#' @description Collapses values and strings to one string (without a white space).
#' It also prints the results (good for a quick check)
#' @param ... Variables (strings, vectors) to be collapsed in consecutively.
#' @param collapseby collapse elements into a string separated by this character
#' @param print Print the results to the terminal. Default is 1 for `print()`.
#' Set to 2 for `message()`.
#'
#' @examples kollapse(
#'   "Hello ", LETTERS[24],
#'   ", the winning numbers are ", c(1, 3, 5, 65, 11), " . Yay!"
#' )
#' @export

kollapse <- function(...,
                     collapseby = "",
                     print = 1) {
  if (print == 1) {
    print(paste0(c(...), collapse = collapseby))
  } else if (print == 2) {
    message(paste0(c(...), collapse =  collapseby))
  }
  paste0(c(...), collapse = collapseby)
}

# ______________________________________________________________________________________________----
# Lazy collapse and paste -----------------------------------------------------------------------


# _________________________________________________________________________________________________
#' @title Simplified Paste by point
#' @description Simplified Paste by point
#' @param ... Multiple simple string variables to parse.
#' @param make.names Should make.names applied to the concatenated string? Default is FALSE.
#' @examples sppp("A", 1:2, "end", "", NULL)
#' kpp("A", 1:2, "end", "", NULL)
#' @export
sppp <- function(..., make.names = FALSE) {
  string <- kpp(...)
  if (make.names) string <- make.names(string)
  string <- ReplaceRepeatedDots(string)
  string <- RemoveFinalDot(string)
  string <- RemoveInitialDot(string)
  return(string)
}

# _________________________________________________________________________________________________
#' @title Simplified Paste by fwd slash
#' @description Simplified Paste by fwd slash
#' @param ... Multiple simple variables to parse.
#' @param make.names Should make.names applied to the concatenated string? Default is FALSE.
#' @examples spps("A", 1:2, "end", "", NULL)
#' kpps("A", 1:2, "end", "", NULL)
#' @export

spps <- function(..., make.names = FALSE) {
  string <- kpps(...)
  if (make.names) string <- make.names(string)
  string <- ReplaceRepeatedSlashes(string)
  string <- RemoveFinalSlash(string)
  return(string)
}


# _________________________________________________________________________________________________
#' @title Simplified Paste by underscore
#' @description Simplified Paste by underscore
#' @param ... Multiple simple variables to parse.
#' @param make.names Should make.names applied to the concatenated string? Default is FALSE.
#' @examples sppu("A", 1:2, "end", "", NULL)
#' kppu("A", 1:2, "end", "", NULL)
#' @export

sppu <- function(..., make.names = FALSE) {
  string <- kppu(...)
  if (make.names) string <- make.names(string)
  string <- ReplaceRepeatedUnderscores(string)
  string <- RemoveFinalUnderscores(string)
  return(string)
}

# ______________________________________________________________________________________________----
# Padding  ----------------------------------------------------------------------------------



# _________________________________________________________________________________________________
#' @title pad.na
#' @description This function fills up a vector to a given length by appending NA-values at the end.
#'   If the input vector's length is less than the provided length, the function pads the vector
#'   with NA. If the vector's length is already equal to or greater than the given length, no change
#'   will be made.
#' @param x A vector that needs to be padded with NA. This can be of any type (numeric, etc.)
#' @param len The target length for the vector. If the provided length is less than the length of
#'   the input vector, the function does not make any change to the input vector.
#' @return Returns a vector of the same type as the input, but with its length adjusted to the
#'   specified len, padding with NA values at the end if necessary.
#'
#' @export
pad.na <- function(x, len) {
  c(x, rep(NA, len - length(x)))
}
# See str_pad



# ______________________________________________________________________________________________----
# Pretty Strings  ----------------------------------------------------------------------------------


#' @title percentile2value
#' @description Calculate what is the actual value of the N-th percentile in a distribution or set of numbers.
#' Useful for calculating cutoffs, and displaying them by whist()s "vline" paramter.
#' @param distribution A numeric vector
#' @param percentile percentile, Default: 0.95
#' @param FirstValOverPercentile PARAM_DESCRIPTION, Default: TRUE
#' @export


percentile2value <- function(distribution, percentile = 0.95, FirstValOverPercentile = TRUE) {
  index <- percentile * length(distribution)
  if (FirstValOverPercentile) {
    index <- ceiling(index)
  } else {
    index <- floor(index)
  }
  value <- sort(distribution)[index]
  return(value)
}

# _________________________________________________________________________________________________
#' @title parsepvalue
#' @description Parse p-value from a number to a string.
#' @param pvalue pvalue to parse. Default: 0.01

#' @export
parsepvalue <- function(pvalue = 0.01) paste0("(p<", pvalue, ")")
# Parse p-value from a number to a string.


# _________________________________________________________________________________________________
#' @title percentage_formatter
#'
#' @description Parse a string of 0-100% from a number between 0 and 1.
#'
#' @param x A vector of numbers between 0-1.
#' @param digitz Number of digits to keep. 3 by default.
#' @param keep.names Keep vector names
#' @param prefix prefix added before the string, Default: NULL
#' @param sign_sep Need space before % sign?
#' @param suffix suffix added after the string, Default: NULL
#'
#' @export
#' @examples percentage_formatter(x = 4.2822212, digitz = 3)
percentage_formatter <- function(x, digitz = 3, keep.names = FALSE, prefix = NULL, suffix = NULL, sign_sep = "") {
  if (keep.names) nmz <- names(x)
  pc_sign <- paste(100 * signif(x, digitz), "%", sep = sign_sep)
  a <- trimws(paste(prefix, pc_sign, suffix, sep = " "))
  a[is.nan(x)] <- "NaN"
  a[is.na(x) & !is.nan(x)] <- "NA"
  if (keep.names) names(a) <- nmz
  return(a)
}


# Format numbers as human readable strings
format_number_h <- function(x, digits = 1, big.mark = " ", decimal.mark = ".") {
  stopifnot(is.numeric(x))
  x <- format(x, big.mark = big.mark, decimal.mark = decimal.mark, digits = digits)
  return(x)
}


# _________________________________________________________________________________________________
#' @title Count Dots or Underscores in a String and return
#'
#' @description
#' This function counts the number of "." characters in a given string.
#'
#' @param string A character string in which the number of "." characters will be counted. Default: None.
#' @return An integer representing the number of "." characters in the string.
#' @export
#' @examples
#' \dontrun{
#' countDotOrUnderscoreSeparated("Hello.World...")
#' countDotOrUnderscoreSeparated("add_translated_metadata")
#' countDotOrUnderscoreSeparated("add_translated.metadata")
#' countDotOrUnderscoreSeparated("add translated metadata")
#' countDotOrUnderscoreSeparated("addTranslatedMetadata")
#' }
#' @importFrom dplyr case_when
#' @return An integer representing the number of "." characters in the string.
#' @export
countDotOrUnderscoreSeparated <- function(string) {
  stopifnot(is.character(string), length(string) == 1)

  # Count the number of dots, underscores, and white spaces in the string
  {
    dot_count <- sum(strsplit(string, "")[[1]] == ".")
    message(paste("Number of dots in the string:", dot_count))

    usc_count <- sum(strsplit(string, "")[[1]] == "_")
    message(paste("Number of underscores in the string:", usc_count))

    ws_count <- sum(strsplit(string, "")[[1]] == " ")
    message(paste("Number of white spaces in the string:", ws_count))
  }

  estimated_separator <- dplyr::case_when(
    dot_count > max(usc_count, ws_count) ~ "dot",
    usc_count > max(usc_count, ws_count) ~ "underscore",
    ws_count > max(dot_count, usc_count) ~ "white space",
    dot_count == 0 & usc_count == 0 & ws_count == 0 ~ "none",
    dot_count == usc_count & dot_count == usc_count ~ "undecided"
  )

  message("Estimated separator: ", estimated_separator)

  return(estimated_separator)
}


# _________________________________________________________________________________________________
#' @title Convert a String to camelCase
#'
#' @description This function takes a string as input and converts it to camelCase format.
#' It splits the string into words using dots as separators, capitalizes the first letter of
#' each word (except the first word), and then concatenates them back together.
#'
#' @param input_string A character string to be converted to camelCase. The function expects a
#' string where words are separated by dots. There is no default value for this parameter;
#' a string must be provided.
#' @param estimated_separator A character string representing the separator used in the input string.
#' Default: countDotOrUnderscoreSeparated(input_string)
#' @param toclipboard Copy to clipboard? Default: TRUE
#' @return A character string converted to camelCase.
#'
#' @examples
#' toCamelCase("plot.metadata.cor.heatMap")
#' toCamelCase("plot_metadata_cor_heat_map")
#' @importFrom clipr write_clip
#'
#' @export
toCamelCase <- function(input_string,
                        estimated_separator = countDotOrUnderscoreSeparated(input_string),
                        toclipboard = TRUE) {
  stopifnot(is.character(input_string), length(input_string) == 1)

  # Split the string into words using the appropriate separator
  words <- if (estimated_separator == "underscore") {
    strsplit(input_string, "_")[[1]] # split by underscore
  } else if (estimated_separator == "dot") {
    strsplit(input_string, "\\.")[[1]] # split by dot
  } else if (estimated_separator == "white space") {
    strsplit(input_string, " ")[[1]] # split by white space
  } else {
    stop("Cannot guess separator: provide it explicitly in 'estimated_separator'.")
  }

  # Capitalize the first letter of each word except the first one
  words[-1] <- sapply(words[-1], function(word) {
    paste0(toupper(substr(word, 1, 1)), tolower(substr(word, 2, nchar(word))))
  })
  if (toclipboard & require(clipr)) try(clipr::write_clip(words), silent = TRUE)

  # Concatenate the words back together
  return(paste0(words, collapse = ""))
}


# _________________________________________________________________________________________________
#' @title Convert a String to underscore_separated Format
#'
#' @description This function converts a string from camelCase or dot-separated format to an underscore-separated format.
#' It can handle strings that are a combination of camelCase and dot-separated formats. The function replaces
#' dots with underscores and inserts an underscore before any uppercase letter that follows a lowercase letter.
#' It then converts all characters to lowercase.
#'
#' @param input_string A character string in camelCase, dot-separated format, or a combination of both.
#'                     There is no default value for this parameter; a string must be provided.
#' @param toclipboard Copy to clipboard? Default: TRUE
#' @return A character string converted to underscore_separated format.
#'
#' @examples
#' toUnderscoreSeparated("plot.Metadata.cor.heatMap")
#' toUnderscoreSeparated("plotMetadataCorHeatMap")
#' toUnderscoreSeparated("plot.metadataCor.heatMap")
#' @importFrom clipr write_clip
#'
#' @export
toUnderscoreSeparated <- function(input_string, toclipboard = FALSE) {
  stopifnot(is.character(input_string), length(input_string) > 0, !any(is.na(input_string)))

  # Handle white space-separated input
  input_string <- gsub("\\s+", "_", input_string)

  # Replace dots with underscores
  temp_string <- gsub("\\.", "_", input_string)

  # Insert underscores before uppercase letters followed by lowercase letters and convert to lowercase
  result <- tolower(gsub("([a-z0-9])([A-Z])", "\\1_\\2", temp_string))
  stopifnot(is.character(result), nchar(result) > 0)

  if (toclipboard) try(clipr::write_clip(result), silent = TRUE)

  message(result)
  invisible(result)
}


# _____________________________________________________________________________________________
#' @title Convert String to Dot Separated Name
#'
#' @description Converts a string from camelCase or underscore_separated format to dot.separated.name format.
#' Inserts dots before each uppercase letter (except if it's the first character) or replaces underscores with dots,
#' and then converts the entire string to lowercase.
#'
#' @param input_string A character string in camelCase or underscore_separated format to be converted.
#'                     Default: No default value, a string must be provided.
#' @param toclipboard Copy to clipboard? Default: TRUE
#' @return A character string converted to dot-separated format. The result is always in lowercase.
#' @examples
#' toDotSeparated("plotMetadataCorHeatMap")
#' toDotSeparated("plot_Metadata_Cor_HeatMap")
#' @importFrom clipr write_clip
#' @export

toDotSeparated <- function(input_string, toclipboard = TRUE) {
  stopifnot(is.character(input_string), length(input_string) > 0, !any(is.na(input_string)))

  # Handle white space-separated input
  input_string <- gsub("\\s+", ".", input_string)

  # Handle underscore-separated input
  input_string <- gsub("_", ".", input_string)

  # Insert a dot before each uppercase letter (except the first character)
  separated <- gsub("([A-Z])", ".\\1", input_string, perl = TRUE)

  # Convert the entire string to lowercase, and remove duplicated / starting dots (see sppp).
  result <- sppp(tolower(separated))

  stopifnot(is.character(result), nchar(result) > 0)

  # Handle clipboard functionality
  if (toclipboard & requireNamespace("clipr", quietly = TRUE)) try(clipr::write_clip(result), silent = TRUE)

  message(result)
}

# _____________________________________________________________________________________________
#' @title Convert CamelCase to Sentence
#'
#' @description Takes a camelCase string and converts it to a sentence format: space-separated,
#' with the first letter capitalized and no period at the end.
#'
#' @param camelCaseString A character string in camelCase format.
#' @return A character string converted to sentence format.
#' @examples
#' toSentence("mergeSmallCategories")
#' @export
#'
#' @importFrom stringr str_replace_all str_to_title
toSentence <- function(camelCaseString) {
  stopifnot(is.character(camelCaseString))

  # Insert a space before each uppercase letter, except the first character
  sentence <- gsub("([a-z])([A-Z])", "\\1 \\2", camelCaseString)

  # Capitalize the first letter of the sentence
  sentence <- tolower(sentence)
  paste0(toupper(substr(sentence, 1, 1)), substr(sentence, 2, nchar(sentence)))
}



# ______________________________________________________________________________________________----
# Path parsing ----
# _________________________________________________________________________________________________

#' @title Fix Special Characters for Bash
#'
#' @description This function takes a string representing a path and escapes certain special
#' characters to make it compatible with Bash. Specifically, it escapes spaces,
#' opening parentheses, and closing parentheses by placing a backslash before them.
#' @param path A character string representing the path to be fixed.
#' @return A character string with special characters escaped for Bash.
#'
#' @examples
#' path <- "~/Dropbox (VBC)/Abel.IMBA/Data.dropbo"
#' fixed_path <- fix_special_characters_bash(path)
#' print(fixed_path) # Outputs: ~/Dropbox\ \(VBC\)/Abel.IMBA/Data.dropbo
#'
#' @export
fix_special_characters_bash <- function(path) {
  # Replace spaces with '\ '
  path <- gsub(" ", "\\\\ ", path)

  # Replace '(' with '\('
  path <- gsub("\\(", "\\\\(", path)

  # Replace ')' with '\)'
  path <- gsub("\\)", "\\\\)", path)

  return(path)
}


# _________________________________________________________________________________________________
#' @title Parse Full File Path
#'
#' @description Constructs a full file path by combining a path, file name, and extension. It applies
#'   string clean-up operations to each component and ensures proper formatting.
#'
#' @param path The directory path. If not provided, only file name and extension are used.
#'   Default: NULL.
#' @param file_name The name of the file. Clean-up operations are applied to remove special
#'   characters and repeated dots. Default: Empty string.
#' @param extension The file extension. If provided, it is appended to the file name with a
#'   preceding dot. Clean-up operations remove any initial dots. Default: NULL.
#' @return A string representing the full file path.
#' @examples
#' ParseFullFilePath(path = "home/user/docs/", file_name = "report@final", extension = ".txt")
#' ParseFullFilePath(file_name = "report", extension = "txt")
#'
#' @export
ParseFullFilePath <- function(path, file_name, extension) {
  file_name <- ReplaceRepeatedDots(ReplaceSpecialCharacters(file_name))

  if (hasArg(path)) {
    path <- AddTrailingSlashfNonePresent(ReplaceRepeatedSlashes(path))
    full_path <- paste0(path, file_name)
  } else {
    full_path <- file_name
  }

  if (hasArg(extension)) {
    extension <- RemoveInitialDot(extension)
    full_path <- paste0(full_path, ".", extension)
  }

  return(full_path)
}



# _________________________________________________________________________________________________
#' @title FixUnderscores
#'
#' @description FixUnderscores removes multiple consecutive underscores (e.g. '_') from a string, and optionally also removes a final '_'.
#' @param string The file path potentially having Double Slash
#' @param trimFinal Remove final undescore?
#' @export
#'
#' @examples FixUnderscores(string = "stairway//to/heaven")
FixUnderscores <- function(string = "stairway__to_heaven_", trimFinal = TRUE) {
  string <- gsub(x = string, pattern = "_+", replacement = "_")
  LastChr <- substr(string, nchar(string), nchar(string))
  if (trimFinal && LastChr == "_") {
    print(paste("LastChr: ", LastChr))
    string <- substr(string, 1, (nchar(string) - 1))
  }
  return(string)
}


# _________________________________________________________________________________________________
#' @title FixPath
#'
#' @description FixPath removes multiple consecutive slashes (e.g. '//') from a string and adds a final '/' if missing from a file path.
#' @param string The file path potentially having Double Slash
#' @param ... Additional strings to concatenate after the first one
#' @param is.file Do not add last slash if this string ends in a filename. Def: FALSE
#' @examples FixPath(string = "stairway//to/heaven")
#'
#' @export
FixPath <- function(string = "stairway//to/heaven", ..., is.file = FALSE) {
  string <- spps(string, ...)
  string <- RemoveTrailingDots(string)
  string <- ReplaceRepeatedDots(string)
  string <- ReplaceRepeatedSlashes(string)
  LastChr <- substr(string, nchar(string), nchar(string))
  if (!is.file & !LastChr == "/") {
    string <- paste0(string, "/")
  }
  return(string)
}


# _________________________________________________________________________________________________
#' @title FixPlotName
#'
#' @description FixPlotName replaces special characters in an input string (dollar-, at-, bracket-signs)
#' @param string Input string
#' @param ... Additional strings to concatenate after the first one
#' @examples FixPlotName(string = "obj at meta$alpha[[3]]")
#'
#' @export
FixPlotName <- function(string = "obj@meta$alpha[[3]]", ...) {
  string <- sppp(string, ...) # add suffices
  string <- ReplaceSpecialCharacters(string)
  string <- RemoveTrailingDots(string)
  ReplaceRepeatedDots(string)
}


# _________________________________________________________________________________________________
#' @title ParseDirPath
#'
#' @description ParseDirPath pastes elements by slash, then removes Double Slashes '//' from a
#'   string and adds a final '/' if missing from a file path.
#' @param ...  The set of strings (character vectors) to be parsed into a file path, and potentially
#'   having Double Slashes, potentially missing a trailing slash.
#' @examples ParseDirPath(string = "stairway///to/heaven")
#'
#' @export
ParseDirPath <- function(...) {
  string <- kpps(...)
  string <- ReplaceRepeatedSlashes(string)
  string <- AddTrailingSlashfNonePresent(string)
  return(string)
}


# _________________________________________________________________________________________________
#' @title PasteDirNameFromFlags
#' @description Paste a dot (point) separated string from a list of inputs (that can be empty), and clean up the output string from dot multiplets (e.g: ..).
#' @param ... Multiple simple variables to parse.
#' @export
PasteDirNameFromFlags <- function(...) {
  flagList <- c(...)
  pastedFlagList <- kpp(flagList)
  CleanDirName <- gsub(x = pastedFlagList, pattern = "[\\..] + ", replacement = "\\.")
  return(CleanDirName)
}



# _________________________________________________________________________________________________
#' @title extPDF
#' @description add '.pdf' as extension to a file name
#' @param vec Filename basis.
#' @examples extPDF("mypltt")
#' @export
extPDF <- function(vec) {
  ppp(vec, "pdf")
}


# _________________________________________________________________________________________________
#' @title extPNG
#' @description add '.png' as extension to a file name
#' @param vec Filename basis.
#' @examples extPNG("mypltt")
#' @export
extPNG <- function(vec) {
  ppp(vec, "png")
}

# ______________________________________________________________________________________________----
# Flag parsing for path / directory naming ----
# _________________________________________________________________________________________________


#' @title Parse Parameter String with Names
#'
#' @description This function parses a named vector and intermingles the names and values
#' into a single string, with specified separators for the odd and even elements.
#'
#' @param named.vec A named vector to be parsed. Default: `NA`.
#' @param sep1 A string separator for odd elements. Default: `": "`.
#' @param sep2 A string separator for even elements. Default: `" | "`.
#'
#' @return A single string with intermingled names and values from the named vector.
#'
#' @examples
#' named.vec <- c(ULm = "15", DLm = "67", Matm = "33", `EN-Lineage.m` = "21")
#' parseParamStringWNames(named.vec)
#' # "ULm: 15 | DLm: 67 | Matm: 33 | EN-Lineage.m: 21"
#'
#' @export
parseParamStringWNames <- function(named.vec, sep1 = ": ", sep2 = " | ") {
  stopifnot(
    is.vector(named.vec), is.character(sep1), is.character(sep2),
    length(names(named.vec)) == length(named.vec)
  )

  # Combine names and values using sapply
  pairs <- sapply(seq_along(named.vec), function(i) paste(names(named.vec)[i], named.vec[i], sep = sep1))

  # Collapse the pairs with the second separator
  return(paste(pairs, collapse = sep2))
}

# _________________________________________________________________________________________________
#' @title Convert Named Parameters to Filename
#'
#' @description This function takes named parameters and converts them into a filename string with
#' specified separators and collapse characters. It excludes any parameters with NULL values.
#'
#' @param ... Named parameters to be converted. Default: None.
#' @param sep A string to separate parameter names and their values. Default: ".".
#' @param collapse A string to separate different parameters in the output string. Default: "_".
#'
#' @return A character string that represents the combined parameter names and values, separated
#' by the specified `sep` and `collapse` characters.
#'
#' @examples
#' params.2.fname(aa = 1, cc = 2, d = NULL, sep = ".", collapse = "_")
#' # Returns "aa.1_cc.2"
params.2.fname <- function(..., sep = ".", collapse = "_") {
  x <- list(...)
  nmz <- as.character(substitute(list(...))[-1])

  # Filter out NULL values
  idx.empty <- sapply(x, is.null)
  x <- x[!idx.empty]
  nmz <- nmz[!idx.empty]

  # Fix if not a single value
  x <- sapply(x, sppp)

  result <- paste(nmz, x, sep = sep, collapse = collapse)

  return(result)
}


# _________________________________________________________________________________________________
#' @title param.list.2.fname
#' @description Take a list of parameters and parse a string from their names and values.
#' @param ls.of.params List of parameters, Default: p
#' @param sep Separator name-2-value, Default: "."
#' @param collapse Separator between elements, Default: "_"
#'
#' @export
param.list.2.fname <- function(ls.of.params = p, sep = ".", collapse = "_") {
  paste(names(ls.of.params), ls.of.params, sep = sep, collapse = collapse)
}


# _________________________________________________________________________________________________
#' @title PasteOutdirFromFlags
#' @description Paste OutDir from (1) a path and (2) a from a list of inputs (that can be empty), and clean up the output string from dot and forward slash multiplets (e.g: ..).
#' @param path path, Default: '~/Dropbox/Abel.IMBA/AnalysisD'
#' @param ... Multiple simple variables to parse.
#'
#' @export
PasteOutdirFromFlags <- function(path = "~/Dropbox/Abel.IMBA/AnalysisD", ...) {
  flagList <- c(path, ...)
  pastedFlagList <- kpp(flagList)
  CleanDirName <- gsub(x = pastedFlagList, pattern = "[\\..] + ", replacement = "\\.")

  pastedOutDir <- paste0(CleanDirName, "/")
  CleanDirName <- gsub(x = pastedOutDir, pattern = "[//] + ", replacement = "/")
  CleanDirName <- gsub(x = pastedOutDir, pattern = "[/] + ", replacement = "/")
  CleanDirName <- gsub(x = pastedOutDir, pattern = "/\\.+", replacement = "/") # remove invisible directories '/.dirname'
  return(CleanDirName)
}



# _________________________________________________________________________________________________
#' @title flag.name_value
#' @description Returns the name and its value, if its not FALSE.
#' @param toggle Binary variable
#' @param Separator Separator, Default: '_'
#' @examples Xseed <- 1212
#' p <- list()
#' p$"seed" <- 1212
#' flag.name_value(Xseed)
#' flag.name_value(p$"seed")
#'
#' @export
flag.name_value <- function(toggle, Separator = "_") {
  if (!isFALSE(toggle)) {
    output <- paste(substitute(toggle), toggle, sep = Separator)
    if (length(output) > 1) output <- output[length(output)] # fix for when input is a list element like p$'myparam'
    return(output)
  }
}

# _________________________________________________________________________________________________
#' @title flag.nameiftrue
#' @description Returns the name and its value, if its TRUE.
#' @param toggle Binary variable
#' @param prefix prefix added before the string, Default: NULL
#' @param suffix suffix added after the string, Default: NULL
#' @param name.if.not Alternative name., Default: ''
#'
#' @export
flag.nameiftrue <- function(toggle, prefix = NULL, suffix = NULL, name.if.not = "") {
  output <- if (toggle) {
    paste0(prefix, (substitute(toggle)), suffix)
  } else {
    paste0(prefix, name.if.not, suffix)
  }
  if (length(output) > 1) output <- output[length(output)] # fix for when input is a list element like p$'myparam'
  return(output)
} # returns the name if its value is true


# _________________________________________________________________________________________________
#' @title flag.names_list
#' @description Returns the name and value of each element in a list of parameters.
#' @param par A list element e.g.: p$umap
#' @examples # flag.names_list(par = p$'umap.n_neighbors')
#'
#' @export
flag.names_list <- function(par) {
  if (length(par)) paste(substitute(par), kppu(par), sep = "_")[[3]]
}


# _________________________________________________________________________________________________
#' @title flag.names_list.all.new
#' @description Returns the name and value of each element in a list of parameters.
#' @param pl List of parameters, Default: p.hm
#'
#' @export
flag.names_list.all.new <- function(pl = p.hm) {
  # if (length(pl)) paste(kppu(names(pl)), kppu(pl) , sep = "_")
  if (length(pl)) kppd(paste(names(pl), pl, sep = "_"))
}


# _________________________________________________________________________________________________
#' @title param.list.flag
#' @description Returns the name and value of each element in a list of parameters.
#' @param par parameter, Default: p$umap.min_dist
#'
#' @export
param.list.flag <- function(par = p$"umap.min_dist") {
  paste(substitute(par), par, sep = "_")[[3]]
} # param.list.flag(par = p$umap.n_neighbors)


# _________________________________________________________________________________________________
#' @title parFlags
#'
#' @description Create a string from the names of the (boolean) parameters (TRUE or FALSE) of true values.
#' Use it for Suffixing plot names with the parameters that were used for that plot.
#' @param ... Paramter variables
#' @param prefix Append something before?
#' @param pasteflg Boolean: paste the parameters-flags together?
#' @param collapsechar Separating character between each parameters-flag
#' @examples pearson <- TRUE
#' filtered <- TRUE
#' normalized <- FALSE
#' MyPlotname <- parFlags(prefix = "MyPlot", pearson, filtered, normalized)
#' MyPlotname
#'
#' @export
parFlags <-
  function(prefix = "",
           ...,
           pasteflg = TRUE,
           collapsechar = ".") {
    namez <- as.character(as.list(match.call())[-(1:2)])
    val <- c(...)
    names(val) <- namez
    # flg = names(which(as.logical.wNames(val))) # which_names()
    flg <- names(val)[val]
    print(flg)
    flg <- if (pasteflg) {
      paste0(prefix, collapsechar, paste0(flg, collapse = collapsechar))
    }
    return(flg)
  }


# _________________________________________________________________________________________________
#' @title parFlags2
#'
#' @description Create a string from the names of the (boolean) parameters (TRUE or FALSE) of true values.
#' Use it for Suffixing plot names with the parameters that were used for that plot.
#' @param ... Paramter variables
#' @param prefix Append something before?
#' @param pasteflg Boolean: paste the parameters-flags together?
#' @param coll.char Separating character between each parameters-flag
#' @param coll.char.intra Separating character between parameters and its value
#' @examples pearson <- TRUE
#' filtered <- 3
#' normalized <- FALSE
#' MyPlotname <- parFlags2(prefix = "MyPlot", pearson, filtered, normalized)
#' MyPlotname
#'
#' @export
parFlags2 <-
  function(prefix = ".",
           ...,
           pasteflg = TRUE,
           coll.char = ".",
           coll.char.intra = "_") {
    val <- c(...)
    namez <- as.character(as.list(match.call())[-(1:2)])
    names(val) <- namez
    flg <- if (pasteflg) {
      paste0(
        prefix,
        coll.char,
        paste0(namez, coll.char.intra, val, collapse = coll.char)
      )
    }
    return(flg)
  }




# _________________________________________________________________________________________________
#' @title break.lines for plot titles
#'
#' @param char.vec A long sentence
#' @param max.char Max characters per line
#' @examples ww.break.lines(char.vec = kppd(LETTERS))
#'
#' @export
ww.break.lines <- function(char.vec, max.char = 50) {
  gsub(pattern = paste0("(.{", max.char, "})"), "\\1\n", char.vec)
}




# _________________________________________________________________________________________________
#' @title FormatAsExcelLink
#'
#' @param site_name Text shown.
#' @param site_url Hyperlink url.
#' @examples FormatAsExcelLink(
#'   site_name = c("Zero Hedge", "Free Software Foundation"),
#'   site_url = c("https://www.zerohedge.com", "https://www.fsf.org")
#' )
#'
#' @export
FormatAsExcelLink <- function(site_name, site_url) {
  paste0(
    "=HYPERLINK(\"",
    site_url,
    "\", \"",
    site_name,
    "\")"
  )
}




# ______________________________________________________________________________________________----
# Misc   -------------------------------------------------------------------------------------------

#' @title eval_parse_kollapse
#' @description evaluate and parse (dyn_var_caller)
#' @param ... Multiple simple variables to parse.
#'
#' @export
eval_parse_kollapse <- function(...) {
  substitute(eval(parse(text = kollapse(..., print = FALSE))))
}




# _________________________________________________________________________________________________

# _________________________________________________________________________________________________



# #' @title Stop Execution If Condition is True
# #'
# #' @description This function stops the execution of the script if the provided condition evaluates to TRUE.
# #' It is the complement of the `stopifnot()` function and is used for asserting conditions where
# #' an error should be thrown if the condition is TRUE, rather than FALSE.
# #' @param condition A logical condition to be tested. If TRUE, an error message is thrown and execution is stopped.
# #' @param message An optional error message to display if the condition is TRUE.
# #'
# #' @examples a <- 1
# #' stopif(a != 1, message = "A is 1")
# #' @export
# stopif <- function(condition, message = 'Condition is TRUE.') {
#   if (isTRUE(condition)) stop(message)
# }
