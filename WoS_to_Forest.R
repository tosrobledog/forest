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
                        author_full_name = character(),
                        email = character(),
                        ordic = character(),
                        research_id = character(),
                        stringsAsFactors = FALSE)

id_papers <- author$id_paper

for (i in id_papers) {
        
        row_1 = author[author$id_paper == i,]
        new_row_1 = data.frame(id_paper = i,
                               strsplit(row_1$authors, split = ";"),
                               strsplit(row_1$authors_full_name, split = ";"),
                               strsplit(row_1$authors_email, split = ";+", fixed = TRUE),
                               ordic = row_1$orcid,
                               research_id = row_1$research_id,
                               stringsAsFactors = FALSE)
        colnames(new_row_1) = c("id_paper", "id_author", "full_name", "email", "orcid", "research_id")
        author.df = rbind(author.df, new_row_1)
}

author.df.1 <- data.frame(apply(author.df, 2, trim), 
                          stringsAsFactors = FALSE)

author <- author.df.1[,c("id_author", "full_name", "email", "orcid", "research_id")]

author <- unique(author)

paperauthor <- author.df.1[,c("id_paper", "id_author")]

# Creating Journal and PaperJournal entities

journal.df <- wos.data[,c("iso_source_abbreviation", "publication_name", "SR")]
journal.df$id_journal <- journal.df$iso_source_abbreviation
journal.df$id_paper <- journal.df$SR
journal.df$SR <- NULL
journal <- journal.df[,c("id_journal","iso_source_abbreviation", "publication_name")]

paperjournal <- journal.df[,c("id_paper", "id_journal")]

journal.df$id_journal <- journal.df$iso_source_abbreviation

# Creating ReferenceLink entity

referencelink.df <- wos.data[,c("SR","cited_references")]
referencelink.df$id_paper0 <- referencelink.df$SR
referencelink.df$SR <- NULL
names(referencelink.df)[1] <- "id_paper1" 

referencelink.df.1 <- data.frame(id_paper0 = character(),
                                 id_paper1 = character(),
                                 stringsAsFactors = FALSE)
id_papers <- referencelink.df$id_paper0

for (i in id_papers) {
        
        row_1 = referencelink.df[referencelink.df$id_paper0 == i,]
        new_row_1 = data.frame(id_paper0 = i,
                               strsplit(row_1$id_paper1, split = ";"),
                               stringsAsFactors = FALSE)
        colnames(new_row_1) = c("id_paper0", "id_paper1")
        referencelink.df.1 = rbind(referencelink.df.1, new_row_1)
}

referencelink.df.1$id_paper1 <- trim(referencelink.df.1$id_paper1)

referencelink <- referencelink.df.1

# Publisher entity



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
