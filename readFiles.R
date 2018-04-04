library(bibliometrix)


readISI <- function(...) {
  files <- do.call(readFiles, as.list(...))
  M <- convert2df(files, dbsource = "isi", format = "plaintext")
  M
}

readScopus <- function(...) {
  files <- do.call(readFiles, as.list(...))
  M <- convert2df(files, dbsource = "scopus", format = "bibtex")
  M
}
