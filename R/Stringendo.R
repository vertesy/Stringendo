# ____________________________________________________________________
# Stringendo ----
# ____________________________________________________________________
# String parsing functionalites for generating plotnames, filenames and path.
# Used by MarkdownReports and ggExpress.
# Many functionalities were formerly part of CodeAndRoll.

# devtools::load_all("~/GitHub/Packages/Stringendo")
# devtools::document("~/GitHub/Packages/Stringendo")
# try(source("~/GitHub/Packages/Stringendo/R/Stringendo.R"), silent = T)
# try(source("https://raw.githubusercontent.com/vertesy/Stringendo/main/Stringendo.R"), silent = T)


# ______________________________________________________________________________________________----
# Generic auxiliary functions ----
# _________________________________________________________________________________________________

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
# any_print = iprint # for compatibility

# _________________________________________________________________________________________________
#' @title Parse current date, dot separated.
#' @description Parse current date, dot separated.
#' @param Format Date format. Default: c("%Y.%m.%d_%H.%M", "%Y.%m.%d_%Hh")[2]
#' @export

idate <- function(Format = c("%Y.%m.%d_%H.%M", "%Y.%m.%d_%Hh")[2]) {
  format(Sys.time(), format = Format)
}

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

# _________________________________________________________________________________________________
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
#' @title ReplaceSpecialCharacters
#'
#' @description ReplaceSpecialCharacters replaces special characters '[]$@()' with dots.
#' @param string The string potentially having special characters.
#' @examples ReplaceSpecialCharacters(string = "obj@meta$alpha[[3]]")
#' @return A string with special characters replaced by dots.
#' @export

ReplaceSpecialCharacters <- function(string = "obj@meta$alpha[[3]]") {
  gsub(x = string, pattern = "\\@|\\[|\\]|\\$|\\/\\(\\)", replacement = ".")
}


# ______________________________________________________________________________________________----
# Special character addition -------------------------------------------------------------------------------------------------


#' @title AddTrailingDotIfNonePresent
#'
#' @description Adds a final slash '/', if missing from a string (file path).
#' @param string The file path potentially missing the trailing slash
#' @examples AddTrailingDotIfNonePresent(string = "stairway.to.heaven")
#'
#' @export
AddTrailingDotIfNonePresent <- function(string = "stairway.to.heaven") {
  LastChr <- substr(string, nchar(string), nchar(string))
  if (!LastChr == "\\.") {
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
#' @title Paste Elements With Names
#'
#' @description This function takes a named vector and returns a string where each element is pasted
#'  with its name. Elements are separated by a specified string, and name-element pairs are also
#'  separated by a specified string. The default named vector is `c('a' = 1, 'b' = 2)`.
#' @param named_vec A named vector. Default is `c('a' = 1, 'b' = 2)`.
#' @param separator_names A character string to separate the names from the elements. Default is ":".
#' @param separator_elements A character string to separate the name-element pairs in the
#' resulting string. Default is " ".
#' @examples paste_w_names(c("a" = 1, "b" = 2))
#' @export
paste_w_names <- function(
    named_vec = c("a" = 1, "b" = 2),
    separator_names = ":", separator_elements = " | ") {
  paste0(names(named_vec), separator_names, named_vec, collapse = separator_elements)
}




# ______________________________________________________________________________________________----
# Collapse (and paste) -----------------------------------------------------------------------

#' @title Collapse by point
#' @description Collapse by point
#' @param ... Multiple simple variables to parse.
#' @export
kpp <- function(...) {
  paste(..., sep = ".", collapse = ".")
}

# _________________________________________________________________________________________________
#' @title Collapse by underscore
#' @description Collapse by underscore
#' @param ... Multiple simple variables to parse.
#' @export
kppu <- function(...) {
  paste(..., sep = "_", collapse = "_")
}

# _________________________________________________________________________________________________
#' @title Collapse by (forward) slash
#' @description Collapse by (forward) slash
#' @param ... Multiple simple variables to parse.
#' @export
kpps <- function(...) {
  paste(..., sep = "/", collapse = "/")
}


# _________________________________________________________________________________________________
#' @title Collapse by dash
#' @description Collapse by dash
#' @param ... Multiple simple variables to parse.
#' @export
kppd <- function(...) {
  paste(..., sep = "-", collapse = "-")
}


# _________________________________________________________________________________________________
#' @title Kollapse
#'
#' @description Collapses values and strings to one string (without a white space).
#' It also prints the results (good for a quick check)
#' @param ... Variables (strings, vectors) to be collapsed in consecutively.
#' @param collapseby collapse elements into a string separated by this character
#' @param print Print the results to the terminal. TRUE by default.
#' @examples kollapse(
#'   "Hello ", LETTERS[24],
#'   ", the winning numbers are ", c(1, 3, 5, 65, 11), " . Yay!"
#' )
#' @export

kollapse <- function(...,
                     collapseby = "",
                     print = TRUE) {
  if (print == TRUE) {
    print(paste0(c(...), collapse = collapseby))
  }
  paste0(c(...), collapse = collapseby)
}

# ______________________________________________________________________________________________----
# Lazy collapse and paste -----------------------------------------------------------------------


# _________________________________________________________________________________________________
#' @title Simplified Paste by point
#' @description Simplified Paste by point
#' @param ... Multiple simple variables to parse.
#' @examples sppp("Apples..are...sweet.....")
#' @export
sppp <- function(...) {
  string <- kpp(...)
  string <- ReplaceRepeatedDots(string)
  string <- RemoveFinalDot(string)
  string <- RemoveInitialDot(string)
  return(string)
}

# _________________________________________________________________________________________________
#' @title Simplified Paste by fwd slash
#' @description Simplified Paste by fwd slash
#' @param ... Multiple simple variables to parse.
#' @examples spps("Apples//are///sweet//")
#' @export

spps <- function(...) {
  string <- kpps(...)
  string <- ReplaceRepeatedSlashes(string)
  string <- RemoveFinalSlash(string)
  return(string)
}









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
percentage_formatter <- function(x, digitz = 3, keep.names = F, prefix = NULL, suffix = NULL, sign_sep = "") {
  if (keep.names) nmz <- names(x)
  pc_sign <- paste(100 * signif(x, digitz), "%", sep = sign_sep)
  a <- trimws(paste(prefix, pc_sign, suffix, sep = " "))
  a[a == "NaN %"] <- NaN
  a[a == "NA %"] <- NA
  if (keep.names) names(a) <- nmz
  return(a)
}



# _________________________________________________________________________________________________
#' @title Convert a String to camelCase
#'
#' @description This function takes a string as input and converts it to camelCase format.
#' It splits the string into words using dots as separators, capitalizes the first letter of
#' each word (except the first word), and then concatenates them back together.
#'
#' @param input_string A character string to be converted to camelCase. The function expects a string where words
#'                     are separated by dots. There is no default value for this parameter; a string must be
#'                     provided.
#' @param toclipboard Copy to clipboard? Default: TRUE
#' @return A character string converted to camelCase.
#'
#' @examples
#' toCamelCase("plot.metadata.cor.heatMap")
#' @importFrom clipr write_clip
#'
#' @export
toCamelCase <- function(input_string, toclipboard = TRUE) {
  stopifnot(is.character(input_string), length(input_string) == 1)

  # Split the string into words using the dot as a separator
  words <- strsplit(input_string, "\\.")[[1]]

  # Capitalize the first letter of each word except the first one
  words[-1] <- sapply(words[-1], function(word) {
    paste0(toupper(substr(word, 1, 1)), tolower(substr(word, 2, nchar(word))))
  })
  if (toclipboard & require(clipr)) try(clipr::write_clip(words), silent = T)

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
toUnderscoreSeparated <- function(input_string, toclipboard = TRUE) {
  stopifnot(is.character(input_string), length(input_string) > 0, !any(is.na(input_string)))

  # Replace dots with underscores
  temp_string <- gsub("\\.", "_", input_string)

  # Insert underscores before uppercase letters followed by lowercase letters and convert to lowercase
  result <- tolower(gsub("([a-z0-9])([A-Z])", "\\1_\\2", temp_string))
  stopifnot(is.character(result), nchar(result) > 0)

  if (toclipboard & require("clipr")) try(clipr::write_clip(result), silent = T)

  return(result)
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

  # Handle underscore-separated input
  input_string <- gsub("_", ".", input_string)

  # Insert a dot before each uppercase letter (except the first character)
  separated <- gsub("([A-Z])", ".\\1", input_string, perl = TRUE)

  # Convert the entire string to lowercase, and remove starting dot
  result <- sub("^\\.", "", tolower(separated))

  stopifnot(is.character(result), nchar(result) > 0)

  # Handle clipboard functionality
  if (toclipboard & require("clipr")) try(clipr::write_clip(result), silent = TRUE)

  return(result)
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
  string <- sppp(string, ...)
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
  string <- ReplaceSpecialCharacters(string)
  string <- sppp(string, ...) # add suffices
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
# PasteDirNameFromFlags("HCAB"
#                       , flag.nameiftrue(p$'premRNA')
#                       , flag.nameiftrue(p$"dSample.Organoids")
#                       , flag.names_list(p$'variables.2.regress')
#                       ,  flag.nameiftrue(p$'Man.Int.Order') )



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


# _________________________________________________________________________________________________
#' @title param.list.2.fname
#' @description Take a list of parameters and parse a string from their names and values.
#' @param ls.of.params List of parameters, Default: p
#'
#' @export
param.list.2.fname <- function(ls.of.params = p) {
  paste(names(ls.of.params), ls.of.params, sep = ".", collapse = "_")
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
  # pastedOutDir <- kpps(path, CleanDirName, "/")
  pastedOutDir <- paste0(CleanDirName, "/")
  CleanDirName <- gsub(x = pastedOutDir, pattern = "[//] + ", replacement = "/")
  CleanDirName <- gsub(x = pastedOutDir, pattern = "[/] + ", replacement = "/")
  CleanDirName <- gsub(x = pastedOutDir, pattern = "/\\.+", replacement = "/") # remove invisible directories '/.dirname'
  return(CleanDirName)
}
# PasteOutdirFromFlags("~/Dropbox/Abel.IMBA/AnalysisD/HCAB"
#                      , flag.nameiftrue(p$'premRNA')
#                      , flag.nameiftrue(p$"dSample.Organoids")
#                      , flag.names_list(p$'variables.2.regress')
#                      ,  flag.nameiftrue(p$'Man.Int.Order') )



# _________________________________________________________________________________________________
#' @title flag.name_value
#' @description Returns the name and its value, if its not FALSE.
#' @param toggle Binary variable
#' @param Separator Separator, Default: '_'
#' @examples Xseed <- 1212; p <- list(); p$"seed" <- 1212
#' flag.name_value(Xseed); flag.name_value(p$"seed")
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
#' @examples pearson <- TRUE; filtered <- TRUE;  normalized <- FALSE
#' MyPlotname <- parFlags(prefix = "MyPlot", pearson, filtered, normalized); MyPlotname
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
#' @examples pearson <- TRUE; filtered <- 3;  normalized <- FALSE
#' MyPlotname <- parFlags2(prefix = "MyPlot", pearson, filtered, normalized); MyPlotname
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
#' @examples FormatAsExcelLink(site_name = c("Zero Hedge", "Free Software Foundation")
#' , site_url = c("https://www.zerohedge.com", "https://www.fsf.org"))
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
