library(RSQLite)

source("readFiles.R")

# Connecting with database

forest <- dbConnect(SQLite(), "forest.sqlite")

# Getting Data

wos.data <- readISI("data/ISI/savedrecs.txt")

## Creting Entities
### Creating paper entity

paper <- wos.data[,c("SR", "TI", "PY", "VL", "DI", "AF", "AB", "DE", "SO", "DT")]
names(paper) <- c("id", "title", "year", "volume", "doi", "affiliation", "abstract",
                  "keywords", "publisher", "document_type")

# Creating author and PaperAuthor entities 

author <- wos.data[c("AU", "AF", "EM")]
author$id_paper <- row.names(author)
names(author) <- c("name", "short_name", "email", "id_paper")

author.df <- data.frame(id_paper = character(),
                        id_author = character(), 
                        short_name = character(),
                        email = character(),
                        stringsAsFactors = FALSE)

id_papers <- author$id_paper

for (i in id_papers) {
        
        row_1 = author[author$id_paper == i,]
        new_row_1 = data.frame(id_paper = i,
                               strsplit(row_1$name, split = ";"),
                               strsplit(row_1$short_name, split = ";"),
                               strsplit(row_1$email, split = ";+", fixed = TRUE))
        colnames(new_row_1) = c("id_paper", "id_author", "short_name", "email")
        author.df = rbind(author.df, new_row_1)
}

author.df.1 <- data.frame(apply(author.df, 2, trim), 
                          stringsAsFactors = FALSE)
author <- author.df.1[,c("id_author", "short_name", "email")]
author$name <- author$id_author
author <- author[,c("id_author", "name", "short_name", "email")]
names(author) <- c("id", "name", "short_name", "email")

author.1 <- unique(author)

paperauthor <- author.df.1[,c("id_paper", "id_author")]

# Creating Journal and PaperJournal entities

journal.df <- wos.data[,c("SO","JI")]
names(journal.df) <- c("name", "short_name")
journal.df$id_journal <- journal.df$short_name
journal.df$id_paper <- row.names(journal.df)

journal <- journal.df[, c("id_journal", "name", "short_name")]
paperjournal <- journal.df[,c("id_journal", "id_paper")]

# Creating ReferenceLink entity

referencelink.df <- wos.data[,c("SR","CR")]
names(referencelink.df) <- c("id_paper", "reference")

referencelink.df.1 <- data.frame(id_paper = character(),
                                 reference = character(),
                                 stringsAsFactors = FALSE)
id_papers <- referencelink.df$id_paper

for (i in id_papers) {
        
        row_1 = referencelink.df[referencelink.df$id_paper == i,]
        new_row_1 = data.frame(id_paper = i,
                               strsplit(row_1$reference, split = ";"),
                               stringsAsFactors = FALSE)
        colnames(new_row_1) = c("id_paper", "reference")
        referencelink.df.1 = rbind(referencelink.df.1, new_row_1)
}

referencelink.df.1$reference <- trim(referencelink.df.1$reference)

names(referencelink.df.1) <- c("id_paper0", "id_paper1")

referencelink <- referencelink.df.1

# Loading data into forest database

dbWriteTable(forest, "Paper", paper, append = TRUE)
dbWriteTable(forest, "ReferenceLink", referencelink, append = TRUE)
dbWriteTable(forest, "PaperAuthor", paperauthor, append = TRUE)
dbWriteTable(forest, "Author", author, append = TRUE)
dbWriteTable(forest, "PaperJournal", paperjournal, append = TRUE)
dbWriteTable(forest, "Journal", journal, append = TRUE)


## Disconnecting database

dbDisconnect(forest)
unlink("forest.sqlite")
