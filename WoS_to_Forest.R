library(RSQLite)

source("readFiles.R")

# Connecting with database

forest <- dbConnect(SQLite(), "forest.sqlite")

# Getting Data

wos.data <- readISI("data/ISI/savedrecs.txt") 
row.names(wos.data) <- NULL
        
#files <- list.files("data/ISI/Literacy media/", full.names = TRUE)
# wos.data.all <- readISI(files)

# Cleaning Data

# Reorganizing names

fields <- read.csv("fields_wos.csv")

names(wos.data) <- fields$forest

# Completing ids of paper

wos.data$SR.new <- ifelse(!is.na(wos.data$volume), paste0(wos.data$SR, sep = ", V", wos.data$volume), wos.data$SR)
wos.data$SR.new <- ifelse(!is.na(wos.data$beginning_page), paste0(wos.data$SR.new, sep = ", P", wos.data$beginning_page), wos.data$SR.new)
wos.data$SR.new <- ifelse(!is.na(wos.data$doi), paste0(wos.data$SR.new, sep = ", DOI ", wos.data$doi), wos.data$SR.new)

wos.data$SR <- wos.data$SR.new
wos.data$SR.new <- NULL

## Creting Entities
### Creating paper entity

paper <- wos.data[, c("SR", "doi", "title", "year_published", "volume", "abstract", "authors_keywords",
                    "document_type", "publication_type", "language", "reprint_address",
                    "issn", "eissn", "source_abbreviation_29_character", "iso_source_abbreviation",
                    "publication_date", "issue", "beginning_page", "ending_page", "page_count",
                    "web_of_science_categories", "research_areas", "document_delivery_number",
                    "accession_number", "keyword_plus", "open_access_indicator",
                    "pudmed_id", "special_issue", "book_series_title", "meeting_abstract", "editors",
                    "funding_agency_grant_number","conference_title", "publication_name")]



y <- 
        c("id", "doi", "title", "year_published", "volume", "abstract", "authors_keywords",
          "affiliation", "document_type", "publication_type", "language", "reprint_address",
          "issn", "eissn", "source_abbreviation_29_character", "iso_source_abbreviation",
          "publication_date", "issue", "beginning_page", "ending_page", "page_count",
          "web_of_science_categories", "research_areas", "document_delivery_number",
          "accession_number", "keyword_plus", "open_access_indicator", "pudmed_id",
          "special_issue", "book_series_title", "meeting_abstract", "editors", "id_funging",
          "id_conference", "id_journal")

# pendiente ids de publisher y funding , affiliate, 

# Creating author and PaperAuthor entities 
 
author <- wos.data[c("authors", "authors_full_name", "authors_email", 
                     "authors_address", "orcid",
                     "research_id", "SR")]

author$id_paper <- author$SR
author$SR  <- NULL

author.df <- data.frame(id_paper = character(),
                        id_author = character(), 
                        author_full_name = character(),
                        email = character(),
                        author_address = character(),
                        ordic = character(),
                        research_id = character(),
                        stringsAsFactors = FALSE)

id_papers <- author$id_paper

for (i in id_paper) {
        
        row_1 = author[author$id_paper == i,]
        authors_row = strsplit(row_1$authors, split = ";")
        authors_full_row = strsplit(row_1$authors_full_name, split = ";")
        email_row = strsplit(row_1$authors_email, ";")
        address_row = strsplit(row_1$authors_address, ";")
}

for (i in id_papers) {
        
        row_1 = author[author$id_paper == i,]
        new_row_1 = data.frame(id_paper = i,
                               strsplit(row_1$authors, split = ";"),
                               strsplit(row_1$authors_full_name, split = ";"),
                               strsplit(row_1$authors_email, split = ";+", fixed = TRUE),
                               strsplit(row_1$authors_address, split = ";+", fixed = TRUE),
                               ordic = row_1$orcid,
                               research_id = row_1$research_id,
                               stringsAsFactors = FALSE)
        colnames(new_row_1) = c("id_paper", "id_author", "full_name", "email", "author_address",
                                "orcid", "research_id")
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

publisher <- wos.data[,c("publisher", "publisher_city", "publisher_address", "SR")]
publisher$id_paper <- publisher$SR
publisher$SR <- NULL
publisher$id_publisher <- publisher$publisher

publisher.df.1 <- data.frame(id_paper = character(),
                             id_publisher = character(),
                             publisher = character(),
                             publisher_city = character(),
                             publisher_address = character(),
                             stringsAsFactors = FALSE
                             )

id_papers <- publisher$id_paper

for (i in id_papers) {
        
        row_1 = publisher[publisher$id_paper == i,]
        new_row_1 = data.frame(id_paper = i,
                               strsplit(row_1$publisher, split = ","),
                               strsplit(row_1$publisher, split = ","),
                               publisher_city = row_1$publisher_city,
                               publisher_address = row_1$publisher_address,
                               stringsAsFactors = FALSE)
                               
        colnames(new_row_1) = c("id_paper", "id_publisher", "publisher", "publisher_city", "publisher_address")
        publisher.df.1 = rbind(publisher.df.1, new_row_1)
}

paperpublisher <- publisher.df.1[, c("id_paper", "id_publisher")]

publisher <- publisher.df.1[,c("id_publisher", "publisher", "publisher_city", "publisher_address")]
publisher <- unique(publisher)

# Conference entity

conference <- wos.data[,c("conference_title", "conference_date", "conference_location", 
                          "conference_sponsor")]

conference <- conference[complete.cases(conference) == TRUE,]

# Fundinng entity

funding <- wos.data[,c("funding_agency_grant_number", "funding_text")]
funding <- funding[complete.cases(funding) == TRUE, ]


# Changing id_names of the main tables

names(paper)[1] <- "id"
names(author)[1] <- "id"
names(journal)[1] <- "id"
names(publisher)[1] <- "id"
names(funding)[1] <- "id"

# Organizing paper entity names



# Loading data into forest database

dbWriteTable(forest, "Paper", paper, append = TRUE)
dbWriteTable(forest, "PaperAuthor", paperauthor, append = TRUE)
dbWriteTable(forest, "Author", author, append = TRUE)
dbWriteTable(forest, "Jorunal", journal, append = TRUE)
dbWriteTable(forest, "PaperJournal", paperjournal, append = TRUE)
dbWriteTable(forest, "ReferenceLink", referencelink, append = TRUE)
dbWriteTable(forest, "PaperPublisher", paperpublisher, append = TRUE)
dbWriteTable(forest, "Publisher", publisher, append = TRUE)

## Disconnecting database

dbDisconnect(forest)
unlink("forest.sqlite")
