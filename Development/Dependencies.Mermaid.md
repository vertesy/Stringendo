## Function relationships
> (of connected functions)
```mermaid
 flowchart LR 

  toDotSeparated(toDotSeparated) --> sppp(sppp)
  sppu(sppu) --> kppu(kppu)
  sppu(sppu) --> ReplaceRepeatedUnderscores(ReplaceRepeatedUnderscores)
  sppu(sppu) --> RemoveFinalUnderscores(RemoveFinalUnderscores)
  params.2.fname(params.2.fname) --> sppp(sppp)
  kpwNames(kpwNames) --> HasNames(HasNames)
  ifExistsAndTrue(ifExistsAndTrue) --> iprint(iprint)
  extPNG(extPNG) --> ppp(ppp)
  extPDF(extPDF) --> ppp(ppp)
  eval_parse_kollapse(eval_parse_kollapse) --> kollapse(kollapse)
  PasteOutdirFromFlags(PasteOutdirFromFlags) --> kpp(kpp)
  PasteDirNameFromFlags(PasteDirNameFromFlags) --> kpp(kpp)
  ParseFullFilePath(ParseFullFilePath) --> RemoveInitialDot(RemoveInitialDot)
  ParseFullFilePath(ParseFullFilePath) --> AddTrailingSlashIfMissing(AddTrailingSlashIfMissing)
  ParseFullFilePath(ParseFullFilePath) --> ReplaceSpecialCharacters(ReplaceSpecialCharacters)
  ParseFullFilePath(ParseFullFilePath) --> ReplaceRepeatedSlashes(ReplaceRepeatedSlashes)
  ParseFullFilePath(ParseFullFilePath) --> ReplaceRepeatedDots(ReplaceRepeatedDots)
  ParseDirPath(ParseDirPath) --> kpps(kpps)
  ParseDirPath(ParseDirPath) --> AddTrailingSlashIfMissing(AddTrailingSlashIfMissing)
  ParseDirPath(ParseDirPath) --> ReplaceRepeatedSlashes(ReplaceRepeatedSlashes)
  sppp(sppp) --> RemoveFinalDot(RemoveFinalDot)
  sppp(sppp) --> kpp(kpp)
  sppp(sppp) --> RemoveInitialDot(RemoveInitialDot)
  sppp(sppp) --> ReplaceRepeatedDots(ReplaceRepeatedDots)
  ReplaceSpecialCharacters(ReplaceSpecialCharacters) --> ReplaceRepeatedWhitespaces(ReplaceRepeatedWhitespaces)
  ReplaceSpecialCharacters(ReplaceSpecialCharacters) --> ReplaceRepeatedDots(ReplaceRepeatedDots)
  FixPlotName(FixPlotName) --> sppp(sppp)
  FixPlotName(FixPlotName) --> ReplaceSpecialCharacters(ReplaceSpecialCharacters)
  FixPlotName(FixPlotName) --> ReplaceRepeatedDots(ReplaceRepeatedDots)
  FixPlotName(FixPlotName) --> RemoveTrailingDots(RemoveTrailingDots)
  spps(spps) --> RemoveFinalSlash(RemoveFinalSlash)
  spps(spps) --> kpps(kpps)
  spps(spps) --> ReplaceRepeatedSlashes(ReplaceRepeatedSlashes)
  RemoveTrailingDots(RemoveTrailingDots) --> RemoveFinalDot(RemoveFinalDot)
  RemoveTrailingDots(RemoveTrailingDots) --> RemoveInitialDot(RemoveInitialDot)
  FixPath(FixPath) --> spps(spps)
  FixPath(FixPath) --> ReplaceRepeatedSlashes(ReplaceRepeatedSlashes)
  FixPath(FixPath) --> ReplaceRepeatedDots(ReplaceRepeatedDots)
  FixPath(FixPath) --> RemoveTrailingDots(RemoveTrailingDots)
subgraph "."

end
```
*created by `convert_igraph_to_mermaid()`*
