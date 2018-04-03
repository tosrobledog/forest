library(bibliometrix)

readISI <- function(...){
  D <- readFiles(...)
  M <- convert2df(D, dbsource = "isi", format = "plaintext")
  M
}

readScopus <- function(...) {
  Documents <- readFiles(...)
  M <- convert2df(Documents, dbsource = "scopus", format = "bibtex")
  M
}


