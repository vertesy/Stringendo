######################################################################
# StringParser
######################################################################
# String parsing functionalites for generating plotnames, filenames and path.
# Used by MarkdownReports and ggExpress.
# Many functionalities were formerly part of CodeAndRoll.
# try(source("~/GitHub/Packages/StringParser/R/StringParser.functions.R"), silent = T)
# try(source("https://raw.githubusercontent.com/vertesy/StringParser/main/StringParser.functions.R"), silent = T)




# Auxiliary functions --------------------------------------------------------------------
idate <- function(Format = c("%Y.%m.%d_%H.%M", "%Y.%m.%d_%Hh")[2]) { format(Sys.time(), format = Format ) } # Parse current date, dot separated.

ppp <- function(...) { paste(..., sep = '.') } # Paste by point
pps <- function(...) { paste(..., sep = '/') } # Paste by (forward) slash
ppu <- function(...) { paste(..., sep = '_') } # Paste by underscore
ppd <- function(...) { paste(..., sep = '-') } # Paste by dash

kpp <- function(...) { paste(..., sep = '.', collapse = '.') } # kollapse by point
kppu <- function(...) { paste(..., sep = '_',  collapse = '_') } # kollapse by underscore
kpps <- function(...) { paste(..., sep = '/', collapse = '/') } # kollapse by (forward) slash
kppd <- function(...) { paste(..., sep = '-', collapse = '-') } # kollapse by dash

sppp <- function(...) { # Simplified Paste by point
  string <- paste(..., sep = '.')
  gsub(pattern = '\\.+', replacement = '\\.', x = string)
}


## String operations  -------------------------------------------------------------------------------------------------
percentile2value <- function(distribution, percentile = 0.95, FirstValOverPercentile = TRUE) { # Calculate what is the actual value of the N-th percentile in a distribution or set of numbers. Useful for calculating cutoffs, and displaying them by whist()'s "vline" paramter.
  index = percentile * length(distribution)
  if (FirstValOverPercentile) { index = ceiling(index)
  } else {index = floor(index) }
  value = sort(distribution)[index]
  return(value)
}


parsepvalue <- function(pvalue = 0.01) paste0("(p<",pvalue,")"); # Parse p-value from a number to a string.

eval_parse_kollapse <- function(...) { # evaluate and parse (dyn_var_caller)
  substitute(eval(parse(text = kollapse( ... , print = FALSE))))
}


param.list.2.fname <- function(ls.of.params = p) { # Take a list of parameters and parse a string from their names and values.
  paste(names(ls.of.params), ls.of.params, sep = ".", collapse = "_")
}


PasteDirNameFromFlags <- function(...) { # Paste a dot (point) separated string from a list of inputs (that can be empty), and clean up the output string from dot multiplets (e.g: ..).
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
PasteOutdirFromFlags <- function(path = "~/Dropbox/Abel.IMBA/AnalysisD", ...) { # Paste OutDir from (1) a path and (2) a from a list of inputs (that can be empty), and clean up the output string from dot and forward slash multiplets (e.g: ..).
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

flag.name_value <- function(toggle, Separator = "_") { # Returns the name and its value, if its not FALSE.
  if (!isFALSE(toggle)) {
    output = paste(substitute(toggle), toggle, sep = Separator)
    if (length(output) > 1) output = output[length(output)]  # fix for when input is a list element like p$'myparam'
    return(output)
  }
}
# Xseed = p$'seed' = F; flag.name_value(Xseed); flag.name_value(p$'seed')

flag.nameiftrue <- function(toggle, prefix = NULL, suffix = NULL, name.if.not = "") { # Returns the name and its value, if its TRUE.
  output = if (toggle) { paste0(prefix, (substitute(toggle)), suffix)
  } else {paste0(prefix, name.if.not, suffix)}
  if (length(output) > 1) output = output[length(output)]  # fix for when input is a list element like p$'myparam'
  return(output)
} # returns the name if its value is true
nameiftrue = flag.nameiftrue # backward compatible

flag.names_list <- function(par = p$'umap.min_dist') { # Returns the name and value of each element in a list of parameters.
  if (length(par)) paste(substitute(par), kppu(par) , sep = "_")[[3]]
};  # param.list.flag(par = p$umap.n_neighbors)


flag.names_list.all.new <- function(pl = p.hm) { # Returns the name and value of each element in a list of parameters.
  # if (length(pl)) paste(kppu(names(pl)), kppu(pl) , sep = "_")
  if (length(pl)) kppd(paste(names(pl), pl, sep = "_"))
}


param.list.flag <- function(par = p$'umap.min_dist') { # Returns the name and value of each element in a list of parameters.
  paste(substitute(par), par, sep = "_")[[3]]
};  # param.list.flag(par = p$umap.n_neighbors)


#  --------------------------------------------------------------------



#  --------------------------------------------------------------------



#  --------------------------------------------------------------------



#  --------------------------------------------------------------------



#  --------------------------------------------------------------------



#  --------------------------------------------------------------------




