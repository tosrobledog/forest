library(bibliometrix)   ### load bibliometrix package

Documents <- readFiles("scopus.bib")
M <- convert2df(Documents, dbsource = "scopus", format = "bibtex")
