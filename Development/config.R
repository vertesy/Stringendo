# Configuration for the Package
# file.edit("~/GitHub/Packages/Stringendo/Development/config.R")

DESCRIPTION <- list(
  package.name = "Stringendo",
  version = "0.8.5",
  title = "Stringendo - string parser",
  description = "Stringendo is a set of R functions to parse strings from variables and to manipulate strings.",

  depends = "base",
  imports = "dplyr, clipr",
  suggests = "MarkdownHelpers, MarkdownReports, devtools, testthat",

  author.given = "Abel",
  author.family = "Vertesy",
  author.email = "av@imba.oeaw.ac.at",
  github.user = "vertesy",
  license = "GPL-3 + file LICENSE"
)
