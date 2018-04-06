library(RSQLite)

source("readFiles.R")

# Connecting with database

forest <- dbConnect(SQLite(), "forest.sqlite")

# Getting Data

wos.data <- readISI("data/ISI/savedrecs.txt")
row.names(wos.data) <- NULL

# files <- list.files("data/ISI/Literacy media/", full.names = TRUE)
# wos.data.all <- readISI(files)

# Cleaning Data

# Reorganizing names

names(wos.data) <- fields$forest

## Creting Entities
### Creating paper entity

paper <- wos.data[c("SR", "publisher", "title", "year", "volume", 
                     "abstract", "document_type", "publication_type", "language",
                     "authors_keywords", "reprint_address", "issn", "eissn", "29_character_source_abbreviation",
                     "iso_source_abbreviation", "publication_date", "issue", "beginning_page", 
                     "ending_page", "page_count", "web_of_science_categories", "research_areas", 
                     "document_delivery_number", "accession_number", "keywords_plus", "funding_text", 
                     "open_accesss_indicator", "pubmed_id", "special_issue", "part_number", "book_series_title",
                     "conference_title", "conference_date", "conference_location", "conference_sponsor",
                     "meeting_abstract", "editors", "book_series_title"),]

# pendiente ids de publisher y funding , affiliate, 

# Creating author and PaperAuthor entities 
 
author <- wos.data[c("authors", "authors_full_name", "authors_email", "orcid",
                     "research_id", "SR")]

author$id_paper <- author$SR
author$SR  <- NULL

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
