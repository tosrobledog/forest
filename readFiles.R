library(bibliometrix)
library(DBI)
library(RS)
library(RSQLite)

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

# Creating forest database

forest <- dbConnect(RSQLite::SQLite(), "forest.sqlite")
dbDisconnect(forest)
unlink("forest.sqlite")



## Getting Data

wos.data <- readISI("data/ISI/savedrecs.txt")

## Creting Entities
### Creating paper entity

paper <- wos.data[,c("SR", "TI", "PY", "VL", "DI", "AF", "AB", "DE", "SO", "DT")]
names(paper) <- c("id", "title", "year", "volume", "doi", "affilitation", "abstract",
                  "keywords", "publisher", "document_type")


# Loading data into forest database

dbWriteTable(forest, "Paper", paper)
dbWriteTable(forest, "ReferenceLink", references)
dbWriteTable(forest, "PaperAuthor", paperauthor)
dbWriteTable(forest, "Author", author)
dbWriteTable(forest, "PaperJournal", paperjournal)
dbWriteTable(forest, "Journal", journal)


