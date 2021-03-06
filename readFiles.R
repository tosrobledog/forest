library(bibliometrix)


readISI <- function(...) {
  files.0 <- do.call(readFiles, as.list(...))
  files <- gsub("^null", "", files.0)
  M <- convert2df(files, dbsource = "isi", format = "plaintext")
  M
}

readScopus <- function(...) {
  Documents <- readFiles(...)
  M <- convert2df(Documents, dbsource = "scopus", format = "bibtex")
  M
}


