######################################################################
# Stringendo
######################################################################
# String parsing functionalites for generating plotnames, filenames and path.
# Used by MarkdownReports and ggExpress.
# Many functionalities were formerly part of CodeAndRoll.


# try(source("~/GitHub/Packages/Stringendo/R/Stringendo.R"), silent = T)
# try(source("https://raw.githubusercontent.com/vertesy/Stringendo/main/Stringendo.R"), silent = T)



# Auxiliary functions --------------------------------------------------------------------

# ------------------------------------------------------------------------------------------------------------------------------------------------
#' @title iprint
#' @description A more intelligent printing function that collapses any variable passed to it by white spaces.
#' @param ... Variables (strings, vectors) to be collapsed in consecutively.
#' @examples iprint ("Hello ", "you ", 3, ", ", 11, " year old kids.")
#' @export

iprint <- function(...) {
  argument_list <- c(...)
  print(paste(argument_list, collapse = " "))
}


# ------------------------------------------------------------------------------------------------------------------------------------------------
#' @title Parse current date, dot separated.
#' @description Parse current date, dot separated.
#' @param Format PARAM_DESCRIPTION, Default: c("%Y.%m.%d_%H.%M", "%Y.%m.%d_%Hh")[2]
#' @export
idate <- function(Format = c("%Y.%m.%d_%H.%M", "%Y.%m.%d_%Hh")[2]) { format(Sys.time(), format = Format ) }

# ------------------------------------------------------------------------------------------------------------------------------------------------
#' @title kollapse
#' @description Collapses values and strings to one string (without a white space).
#' It also prints the results (good for a quick check)
#' @param ... Variables (strings, vectors) to be collapsed in consecutively.
#' @param collapseby collapse elements into a string separated by this character
#' @param print Print the results to the terminal. TRUE by default.
#' @examples kollapse("Hello ", LETTERS[24],
#' ", the winning numbers are ", c(1, 3, 5, 65, 11), " . Yay!")
#' @export

kollapse <- function(...,
                     collapseby = "",
                     print = TRUE) {
  if (print == TRUE) {
    print(paste0(c(...), collapse = collapseby))
  }
  paste0(c(...), collapse = collapseby)
}


# ------------------------------------------------------------------------------------------------------------------------------------------------
#' @title Paste by point
#' @description Paste by point
#' @param ... Multiple simple variables to parse.
#' @export
ppp <- function(...) { paste(..., sep = '.') }


# ------------------------------------------------------------------------------------------------------------------------------------------------
#' @title Paste by (forward) slash
#' @description Paste by (forward) slash
#' @param ... Multiple simple variables to parse.
#' @export
pps <- function(...) { paste(..., sep = '/') }

# ------------------------------------------------------------------------------------------------------------------------------------------------
#' @title Paste by underscore
#' @description Paste by underscore
#' @param ... Multiple simple variables to parse.
#' @export
ppu <- function(...) { paste(..., sep = '_') }


# ------------------------------------------------------------------------------------------------------------------------------------------------
#' @title Paste by dash
#' @description Paste by dash
#' @param ... Multiple simple variables to parse.
#' @export
ppd <- function(...) { paste(..., sep = '-') }

# ------------------------------------------------------------------------------------------------------------------------------------------------
#' @title Collapse by point
#' @description Collapse by point
#' @param ... Multiple simple variables to parse.
#' @export
kpp <- function(...) { paste(..., sep = '.', collapse = '.') }

# ------------------------------------------------------------------------------------------------------------------------------------------------
#' @title Collapse by underscore
#' @description Collapse by underscore
#' @param ... Multiple simple variables to parse.
#' @export
kppu <- function(...) { paste(..., sep = '_',  collapse = '_') }

# ------------------------------------------------------------------------------------------------------------------------------------------------
#' @title Collapse by (forward) slash
#' @description Collapse by (forward) slash
#' @param ... Multiple simple variables to parse.
#' @export
kpps <- function(...) { paste(..., sep = '/', collapse = '/') }

# ------------------------------------------------------------------------------------------------------------------------------------------------
#' @title Collapse by dash
#' @description Collapse by dash
#' @param ... Multiple simple variables to parse.
#' @export
kppd <- function(...) { paste(..., sep = '-', collapse = '-') }

# ------------------------------------------------------------------------------------------------------------------------------------------------
#' @title Simplified Paste by point
#' @description Simplified Paste by point
#' @param ... Multiple simple variables to parse.
#' @export
sppp <- function(...) {
  string <- paste(..., sep = '.')
  gsub(pattern = '\\.+', replacement = '\\.', x = string)
}



## String operations  -------------------------------------------------------------------------------------------------

# ------------------------------------------------------------------------------------------------------------------------------------------------
#' @title percentile2value
#' @description Calculate what is the actual value of the N-th percentile in a distribution or set of numbers. Useful for calculating cutoffs, and displaying them by whist()s "vline" paramter.
#' @param distribution A numeric vector
#' @param percentile percentile, Default: 0.95
#' @param FirstValOverPercentile PARAM_DESCRIPTION, Default: TRUE
#' @export


percentile2value <- function(distribution, percentile = 0.95, FirstValOverPercentile = TRUE) {
  index = percentile * length(distribution)
  if (FirstValOverPercentile) { index = ceiling(index)
  } else {index = floor(index) }
  value = sort(distribution)[index]
  return(value)
}

# ------------------------------------------------------------------------------------------------------------------------------------------------
#' @title parsepvalue
#' @description Parse p-value from a number to a string.
#' @param pvalue pvalue to parse. Default: 0.01

#' @export
parsepvalue <- function(pvalue = 0.01) paste0("(p<",pvalue,")"); # Parse p-value from a number to a string.


# ------------------------------------------------------------------------------------------------------------------------------------------------
#' @title eval_parse_kollapse
#' @description evaluate and parse (dyn_var_caller)
#' @param ... Multiple simple variables to parse.

#' @export
eval_parse_kollapse <- function(...) {
  substitute(eval(parse(text = kollapse( ... , print = FALSE))))
}


# ------------------------------------------------------------------------------------------------------------------------------------------------
#' @title param.list.2.fname
#' @description Take a list of parameters and parse a string from their names and values.
#' @param ls.of.params PARAM_DESCRIPTION, Default: p
#' @export


param.list.2.fname <- function(ls.of.params = p) {
  paste(names(ls.of.params), ls.of.params, sep = ".", collapse = "_")
}

# ------------------------------------------------------------------------------------------------------------------------------------------------
#' @title PasteDirNameFromFlags
#' @description Paste a dot (point) separated string from a list of inputs (that can be empty), and clean up the output string from dot multiplets (e.g: ..).
#' @param ... Multiple simple variables to parse.
#' @export


PasteDirNameFromFlags <- function(...) {
  flagList <- c(...)
  pastedFlagList <- kpp(flagList)
  CleanDirName <- gsub(x = pastedFlagList, pattern = '[\\..] + ',replacement = '\\.' )
  return(CleanDirName)
}
# PasteDirNameFromFlags("HCAB"
#                       , flag.nameiftrue(p$'premRNA')
#                       , flag.nameiftrue(p$"dSample.Organoids")
#                       , flag.names_list(p$'variables.2.regress')
#                       ,  flag.nameiftrue(p$'Man.Int.Order') )


### File name and path parsing ------------------------------------------------------------------------------------------------

# ------------------------------------------------------------------------------------------------------------------------------------------------
#' @title PasteOutdirFromFlags
#' @description Paste OutDir from (1) a path and (2) a from a list of inputs (that can be empty), and clean up the output string from dot and forward slash multiplets (e.g: ..).
#' @param path path, Default: '~/Dropbox/Abel.IMBA/AnalysisD'
#' @param ... Multiple simple variables to parse.
#' @export


PasteOutdirFromFlags <- function(path = "~/Dropbox/Abel.IMBA/AnalysisD", ...) {
  flagList <- c(path, ...)
  pastedFlagList <- kpp(flagList)
  CleanDirName <- gsub(x = pastedFlagList, pattern = '[\\..] + ',replacement = '\\.' )
  # pastedOutDir <- kpps(path, CleanDirName, "/")
  pastedOutDir <- paste0(CleanDirName, "/")
  CleanDirName <- gsub(x = pastedOutDir, pattern = '[//] + ',replacement = '/' )
  return(CleanDirName)
}
# PasteOutdirFromFlags("~/Dropbox/Abel.IMBA/AnalysisD/HCAB"
#                      , flag.nameiftrue(p$'premRNA')
#                      , flag.nameiftrue(p$"dSample.Organoids")
#                      , flag.names_list(p$'variables.2.regress')
#                      ,  flag.nameiftrue(p$'Man.Int.Order') )



# ------------------------------------------------------------------------------------------------------------------------------------------------
#' @title flag.name_value
#' @description Returns the name and its value, if its not FALSE.
#' @param toggle Binary variable
#' @param Separator Separator, Default: '_'
#' @example # Xseed = p$'seed' = F; flag.name_value(Xseed); flag.name_value(p$'seed')
#' @export

flag.name_value <- function(toggle, Separator = "_") {
  if (!isFALSE(toggle)) {
    output = paste(substitute(toggle), toggle, sep = Separator)
    if (length(output) > 1) output = output[length(output)]  # fix for when input is a list element like p$'myparam'
    return(output)
  }
}

# ------------------------------------------------------------------------------------------------------------------------------------------------
#' @title flag.nameiftrue
#' @description Returns the name and its value, if its TRUE.
#' @param toggle Binary variable
#' @param prefix prefix, Default: NULL
#' @param suffix suffix, Default: NULL
#' @param name.if.not Alternative name., Default: ''
#' @export


flag.nameiftrue <- function(toggle, prefix = NULL, suffix = NULL, name.if.not = "") {
  output = if (toggle) { paste0(prefix, (substitute(toggle)), suffix)
  } else {paste0(prefix, name.if.not, suffix)}
  if (length(output) > 1) output = output[length(output)]  # fix for when input is a list element like p$'myparam'
  return(output)
} # returns the name if its value is true


# ------------------------------------------------------------------------------------------------------------------------------------------------
#' @title flag.names_list
#' @description Returns the name and value of each element in a list of parameters.
#' @param par PARAM_DESCRIPTION, Default: p$umap.min_dist
#' @export


flag.names_list <- function(par = p$'umap.min_dist') {
  if (length(par)) paste(substitute(par), kppu(par) , sep = "_")[[3]]
};  # param.list.flag(par = p$umap.n_neighbors)

# ------------------------------------------------------------------------------------------------------------------------------------------------
#' @title flag.names_list.all.new
#' @description Returns the name and value of each element in a list of parameters.
#' @param pl List of parameters, Default: p.hm
#' @export


flag.names_list.all.new <- function(pl = p.hm) {
  # if (length(pl)) paste(kppu(names(pl)), kppu(pl) , sep = "_")
  if (length(pl)) kppd(paste(names(pl), pl, sep = "_"))
}

# ------------------------------------------------------------------------------------------------------------------------------------------------
#' @title param.list.flag
#' @description Returns the name and value of each element in a list of parameters.
#' @param par parameter, Default: p$umap.min_dist
#' @export


param.list.flag <- function(par = p$'umap.min_dist') {
  paste(substitute(par), par, sep = "_")[[3]]
}  # param.list.flag(par = p$umap.n_neighbors)


#  --------------------------------------------------------------------
#  --------------------------------------------------------------------
#  --------------------------------------------------------------------

# ------------------------------------------------------------------------------------------------------------------------------------------------
#' @title extPDF
#' @description add pdf as extension to a file name
#' @param vec Filename basis.
#' @examples extPDF("mypltt")
#' @export
extPDF <- function(vec) ppp(vec, "pdf") # add pdf as extension to a file name


# ------------------------------------------------------------------------------------------------------------------------------------------------
#' @title extPNG
#' @description FUNCTION_DESCRIPTION
#' @param vec Filename basis.
#' @examples extPNG("mypltt")
#' @export
extPNG <- function(vec) ppp(vec, "png") # add png as extension to a file name




#' AddTrailingSlash
#'
#' @description Adds a final slash '/', if missing from a string (file path).
#' @param string The file path potentially missing the trailing slash
#' @export
#'
#' @examples AddTrailingSlash (string = "stairway/to/heaven")

AddTrailingSlash <- function(string = "stairway/to/heaven") { #
  LastChr <- substr(string, nchar(string), nchar(string))
  if (!LastChr == "/")
    string = paste0(string, "/")
  return(string)
}

#' @title RemoveDoubleSlash
#'
#' @description RemoveDoubleSlash removes multiple consecutive slashes (e.g. '//') from a string (file path). Also works for 2,3 consecutive slashes
#' @param string The file path potentially having Double Slash
#' @export
#'
#' @examples RemoveDoubleSlash (string = "stairway//to///heaven")

RemoveDoubleSlash <- function(string = "stairway//to/heaven") { #
  gsub(x = string, pattern = '//|///|////', replacement = '/')
}


#' @title RemoveFinalSlash
#'
#' @description RemoveFinalSlash removes the final slash from a string
#' @param string The file path potentially having Final Slash
#' @export
#'
#' @examples RemoveDoubleSlash (string = "stairway//to///heaven")

RemoveFinalSlash <- function(string = "stairway/to/heaven/") { #
  gsub(x = string, pattern = '/$', replacement = '')
}



#' @title FixPath
#'
#' @description FixPath removes multiple consecutive slashes (e.g. '//') from a string and adds a final '/' if missing from a file path.
#' @param string The file path potentially having Double Slash
#' @export
#'
#' @examples FixPath(string = "stairway//to/heaven")

FixPath <- function(string = "stairway//to/heaven") { #
  string <- gsub(x = string, pattern = '//|///|////', replacement = '/')
  LastChr <- substr(string, nchar(string), nchar(string))
  if (!LastChr == "/")
    string = paste0(string, "/")
  return(string)
}




#' @title ParseFilePath
#'
#' @description ParseFilePath pastes elements by slash, then removes Double Slashes '//' from a string and adds a final '/' if missing from a file path.
#' @param ...  The set of strings (character vectors) to be parsed into a file path, and potentially having Double Slashes, potentially missing a trailing slash.
#' @export
#'
#' @examples ParseFilePath(string = "stairway///to/heaven")

ParseFilePath <- function(...) { #
  string <- paste(..., sep = '/', collapse = '/')  # kollapse by (forward) slash
  string <- gsub(x = string, pattern = '//', replacement = '/') # RemoveDoubleSlash
  LastChr <- substr(string, nchar(string), nchar(string)) # AddTrailingSlash
  if (!LastChr == "/")
    string = paste0(string, "/")
  return(string)
}



# ------------------------------------------------------------------------------------------------


#' @title FixUnderscores
#'
#' @description FixUnderscores removes multiple consecutive underscores (e.g. '_') from a string, and optionally also removes a final '_'.
#' @param string The file path potentially having Double Slash
#' @param trimFinal Remove final undescore?
#' @export
#'
#' @examples FixUnderscores(string = "stairway//to/heaven")

FixUnderscores <- function(string = "stairway__to_heaven_", trimFinal = TRUE) { #
  string <- gsub(x = string, pattern = '_+', replacement = '_')
  LastChr <- substr(string, nchar(string), nchar(string))
  if (trimFinal && LastChr == "_") {
    print(paste('LastChr: ', LastChr))
    string = substr(string, 1, (nchar(string)-1))
  }
  return(string)
}


#' substrRight
#'
#' Take the right substring of a string
#' @param x a character vector.
#' @param n integer. The number of elements on the right to be kept.
#' @export
#' @examples substrRight  ("Not cool", n = 4)

substrRight <- function(x, n) {
  substr(x, nchar(x) - n + 1, nchar(x))
}

#' percentage_formatter
#'
#' Parse a string of 0-100% from a number between 0 and 1.
#' @param x A vector of numbers between 0-1.
#' @param digitz Number of digits to keep. 3 by default.
#' @param keep.names Keep vector names
#' @export
#' @examples percentage_formatter (x = 4.2822212, digitz = 3)

percentage_formatter <- function(x, digitz = 3, keep.names = F) {
  if (keep.names) nmz <- names(x)
  a = paste(100 * signif(x, digitz), "%", sep = " ")
  a[a == "NaN %"] = NaN
  a[a == "NA %"] = NA
  if (keep.names) names(a) <- nmz
  return(a)
}

