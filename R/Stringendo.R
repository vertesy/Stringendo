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
#' @title Parse current date, dot separated.
#' @description Parse current date, dot separated.
#' @param Format PARAM_DESCRIPTION, Default: c("%Y.%m.%d_%H.%M", "%Y.%m.%d_%Hh")[2]
#' @export
idate <- function(Format = c("%Y.%m.%d_%H.%M", "%Y.%m.%d_%Hh")[2]) { format(Sys.time(), format = Format ) }

# ------------------------------------------------------------------------------------------------------------------------------------------------
#' @title Paste by point
#' @description Paste by point
#' @param ... Multiple simple variables to parse,
#' @export
ppp <- function(...) { paste(..., sep = '.') }


# ------------------------------------------------------------------------------------------------------------------------------------------------
#' @title Paste by (forward) slash
#' @description Paste by (forward) slash
#' @param ... Multiple simple variables to parse,
#' @export
pps <- function(...) { paste(..., sep = '/') }

# ------------------------------------------------------------------------------------------------------------------------------------------------
#' @title Paste by underscore
#' @description Paste by underscore
#' @param ... Multiple simple variables to parse,
#' @export
ppu <- function(...) { paste(..., sep = '_') }


# ------------------------------------------------------------------------------------------------------------------------------------------------
#' @title Paste by dash
#' @description Paste by dash
#' @param ... Multiple simple variables to parse,
#' @export
ppd <- function(...) { paste(..., sep = '-') }

# ------------------------------------------------------------------------------------------------------------------------------------------------
#' @title Collapse by point
#' @description Collapse by point
#' @param ... Multiple simple variables to parse,
#' @export
kpp <- function(...) { paste(..., sep = '.', collapse = '.') }

# ------------------------------------------------------------------------------------------------------------------------------------------------
#' @title Collapse by underscore
#' @description Collapse by underscore
#' @param ... Multiple simple variables to parse,
#' @export
kppu <- function(...) { paste(..., sep = '_',  collapse = '_') }

# ------------------------------------------------------------------------------------------------------------------------------------------------
#' @title Collapse by (forward) slash
#' @description Collapse by (forward) slash
#' @param ... Multiple simple variables to parse,
#' @export
kpps <- function(...) { paste(..., sep = '/', collapse = '/') }

# ------------------------------------------------------------------------------------------------------------------------------------------------
#' @title Collapse by dash
#' @description Collapse by dash
#' @param ... Multiple simple variables to parse,
#' @export
kppd <- function(...) { paste(..., sep = '-', collapse = '-') }

# ------------------------------------------------------------------------------------------------------------------------------------------------
#' @title Simplified Paste by point
#' @description Simplified Paste by point
#' @param ... Multiple simple variables to parse,
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
#' @param pvalue pvalue to parse, Default: 0.01

#' @export
parsepvalue <- function(pvalue = 0.01) paste0("(p<",pvalue,")"); # Parse p-value from a number to a string.


# ------------------------------------------------------------------------------------------------------------------------------------------------
#' @title eval_parse_kollapse
#' @description evaluate and parse (dyn_var_caller)
#' @param ... Multiple simple variables to parse,

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
#' @param ... Multiple simple variables to parse,
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
#' @param ... Multiple simple variables to parse,
#' @export


PasteOutdirFromFlags <- function(path = "~/Dropbox/Abel.IMBA/AnalysisD", ...) {
  flagList <- c(path, ...)
  pastedFlagList <- kpp(flagList)
  CleanDirName <- gsub(x = pastedFlagList, pattern = '[\\..] + ',replacement = '\\.' )
  # pastedOutDir <- kpps(path, CleanDirName, "/")
  pastedOutDir <- p0(CleanDirName, "/")
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

