######################################################################################################
# Create_the_Stringendo_Package.R
# 2023.08.19_13h
######################################################################################################
# source("/Users/abel.vertesy/GitHub/Packages/Stringendo/Development/Create_the_Stringendo_Package.R")
rm(list = ls(all.names = TRUE));
try(dev.off(), silent = TRUE)


# Functions ------------------------
# require("devtools")
# require("roxygen2")
# require("stringr")

# # devtools::install_github(repo = "vertesy/CodeAndRoll2")
# require('CodeAndRoll2')
# # try (source('~/GitHub/Packages/CodeAndRoll/CodeAndRoll.R'),silent= FALSE) # ONLY If Stringendo not yet exist
# require('Stringendo')


# kollapse <- function(..., collapseby = "", print = TRUE) {
#     if (print == TRUE) { print(paste0(c(...), collapse = collapseby))}
#     paste0(c(...), collapse = collapseby)
# }


# Setup ------------------------
package.name <- 	"Stringendo"
package.version <- "0.3.8"
setwd("~/GitHub/Packages/")

RepositoryDir <- paste0("~/GitHub/Packages/", package.name, "/")
fname <-	paste0(package.name, ".R")
Package_FnP <-		paste0(RepositoryDir, "R/", fname)

BackupDir <- "~/GitHub/Packages/Stringendo/Development/"
dir.create(BackupDir)

# devtools::use_package("vioplot")
DESCRIPTION <- list("Title" = "Stringendo - string parser"
    , "Author" = person(given = "Abel", family = "Vertesy", email = "abel.vertesy@imba.oeaw.ac.at", role =  c("aut", "cre") )
    , "Authors@R" = 'person(given = "Abel", family = "Vertesy", email = "a.vertesy@imba.oeaw.ac.at", role =  c("aut", "cre") )'
    , "Description" = "Stringendo is a set of R functions to parse strings from variables and to manipulate strings."
    , "License" = "GPL-3 + file LICENSE"
    , "Version" = package.version
    , "Packaged" =  Sys.time()
    # , "Repository" =  "CRAN"
    # , "Depends" =  ""
    # , "Imports" = "devtools, grDevices, usethis, MarkdownReports"
    , "Imports" = "methods, utils, clipr"
    , "Suggests" = "MarkdownHelpers, MarkdownReports"
    , "BugReports"= "https://github.com/vertesy/Stringendo/issues"
)


setwd(RepositoryDir)
if ( !dir.exists(RepositoryDir) ) { create(path = RepositoryDir, description = DESCRIPTION, rstudio = TRUE)
} else {
    getwd()
    try(file.remove(c("DESCRIPTION","NAMESPACE", "Stringendo.Rproj")))
    usethis::create_package(path = RepositoryDir, fields = DESCRIPTION, open = F)
}


# go and write fun's ------------------------------------------------------------------------
# file.edit(Package_FnP)

# Create Roxygen Skeletons ------------------------
# RoxygenReady(Package_FnP)

# replace output files ------------------------------------------------
BackupOldFile <-	(paste0(BackupDir, "Development", ".bac"))
AnnotatedFile <-	(paste0(BackupDir, "Development", ".annot.R"))
file.copy(from = Package_FnP, to = BackupOldFile, overwrite = TRUE)
# file.copy(from = AnnotatedFile, to = Package_FnP, overwrite = TRUE)

# Manual editing of descriptors ------------------------------------------------
# file.edit(Package_FnP)

# Compile a package ------------------------------------------------
setwd(RepositoryDir)
getwd()
devtools::document()
warnings()


{
  "update cff version"
  citpath <- paste0(RepositoryDir, 'CITATION.cff')
  xfun::gsub_file(file = citpath, perl = T
                  , "^version: v.+", paste0("version: v", package.version))
}


# Install your package ------------------------------------------------
# # setwd(RepositoryDir)
# unload("Stringendo")
devtools::install(RepositoryDir, upgrade = F)

'after uploading'
# devtools::install_github('vertesy/Stringendo', upgrade = F)

# require("Stringendo")
# # remove.packages("Stringendo")
# # Test your package ------------------------------------------------
# help("wplot")
# cat("\014")
# devtools::run_examples()


# Test if you can install from github ------------------------------------------------
# devtools::install_github(repo = "vertesy/Stringendo")

# require("Stringendo")

# Clean up if not needed anymore ------------------------------------------------
# View(installed.packages())
# remove.packages("Stringendo")

check(RepositoryDir, cran = TRUE)
# as.package(RepositoryDir)
#
#
# # source("https://install-github.me/r-lib/desc")
# # library(desc)
# # desc$set("Stringendo", "foo")
# # desc$get(Stringendo)
#
#
# system("cd ~/GitHub/Stringendo/; ls -a; open .Rbuildignore")

# Check package dependencies ------------------------------------------------
depFile = paste0(RepositoryDir, 'Development/Dependencies.R')

(f.deps <- NCmisc::list.functions.in.file(filename = Package_FnP))
# clipr::write_clip(f.deps)

sink(file = depFile); print(f.deps); sink()
p.deps <- gsub(x = names(f.deps), pattern = 'package:', replacement = '')
write(x = p.deps, file = depFile, append = T)
p.dep.declared <- trimws(unlist(strsplit(DESCRIPTION$Imports, ",")))
p.dep.new <- sort(union( p.deps, p.dep.declared))
# clipr::write_clip(p.dep.new)


