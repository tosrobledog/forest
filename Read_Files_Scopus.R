# load bibliometrix package
library(bibliometrix)

# Scopus source file should be named 'scopus.bib'
Documents <- readFiles("scopus.bib")
M <- convert2df(Documents, dbsource = "scopus", format = "bibtex")
