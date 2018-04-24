library(tidyverse)
library(zoo)
source("readFiles.R")


# Getting Data

scopus.data <- readScopus("data/Scopus/scopus.bib") 
row.names(scopus.data) <- NULL

# files <- list.files("data/ISI/Literacy media/", full.names = TRUE)
# scopus.data.all <- readISI(files)

# Cleaning Data

# Reorganizing names

fields <- read.csv2("fields_scopus.csv")

names(scopus.data) <- fields$forest

# Completing ids of paper

scopus.data$SR.new <- ifelse(!is.na(scopus.data$volume), paste0(scopus.data$SR, sep = ", V", scopus.data$volume), scopus.data$SR)
scopus.data$SR.new <- ifelse(!is.na(scopus.data$beginning_page), paste0(scopus.data$SR.new, sep = ", P", scopus.data$beginning_page), scopus.data$SR.new)
scopus.data$SR.new <- ifelse(!is.na(scopus.data$doi), paste0(scopus.data$SR.new, sep = ", DOI ", scopus.data$doi), scopus.data$SR.new)

scopus.data$SR <- scopus.data$SR.new
scopus.data$SR.new <- NULL

## Creting Entities
### Creating paper entity

paper <- scopus.data[, c("SR", "doi", "title", "year_published",  "abstract", "authors_keywords",
                         "document_type", "reprint_address", "publication_name", "iso_source_abbreviation", 
                         "conference_title")]





# pendiente ids de publisher y funding , affiliate, 

# Creating author and PaperAuthor entities 

author <- scopus.data[c("authors", "authors_email", 
                     "authors_address", "orcid",
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

address.df <- data.frame(author_full_name = character(),
                         address = character(),
                         stringsAsFactors = FALSE
)


id_papers <- author$id_paper


for (i in id_papers) {
  
  row_1 = author[author$id_paper == i,]
  authors_row = strsplit(row_1$authors, split = ";")
  authors_full_row = strsplit(row_1$authors_full_name, split = ";   ")
  email_row = strsplit(row_1$authors_email, "; ")
  orcid_row = row_1$orcid
  research_id_row = row_1$research_id
  
  if ((!is.na(row_1$authors_address))) { 
    
    
    if (substring(row_1$authors_addres, 1, 1) != "[") {
      
      authors_full_name <- strsplit(row_1$authors_full_name, split = ";")
      authors_address <- strsplit(row_1$authors_address, ";   ",
                                  fixed = TRUE)
      authors_list <- cbind(authors_full_name, authors_address)
      author_mx_rows = t(plyr::ldply(authors_list, rbind))
      author_df_rows = data.frame(author_mx_rows, 
                                  stringsAsFactors = FALSE)
      
      names(author_df_rows) = c("author_full_name", "address")
      address.df = rbind(address.df, author_df_rows)
    } else
      
      address_split = strsplit(row_1$authors_address, ";   ", fixed = TRUE)
    df <- t(data.frame(lapply(address_split, strsplit, "]")))
    df <- data.frame(df, stringsAsFactors = FALSE)
    df$X1 <- gsub("\\[", "", df$X1)
    for (j in 1:length(df$X1)) {
      df.y <- data.frame(strsplit(df$X1, split = "; ")[[j]],
                         df$X2[[j]])
      names(df.y) = c("author_full_name", "address")
      address.df = rbind(address.df, df.y)
    }
  }
  
  authors_list = cbind(i, authors_row, authors_full_row, email_row, orcid_row,
                       research_id_row)
  
  author_mx_rows = t(plyr::ldply(authors_list, rbind))
  
  author_df_rows = data.frame(author_mx_rows, 
                              stringsAsFactors = FALSE)
  
  colnames(author_df_rows) = c("id_paper", "id_author", "full_name", "email", 
                               "orcid", "research_id")
  author.df = rbind(author.df, author_df_rows)
  
}

author.df$id_paper <- na.locf(author.df$id_paper)

author.df.1 <- data.frame(apply(author.df, 2, trim), 
                          stringsAsFactors = FALSE)

author <- author.df.1[,c("id_author", "id_author", "full_name", "email", "orcid", "research_id")]

author <- unique(author)

names(author)[1] <- "id"
names(author)[2] <- "name"

paperauthor <- author.df.1[,c("id_paper", "id_author")]

# Creating Address entity

address <- address.df

# Creating Journal and PaperJournal entities

journal.df <- scopus.data[,c("iso_source_abbreviation", "publication_name", "SR")]
journal.df$id_journal <- journal.df$iso_source_abbreviation
journal.df$id_paper <- journal.df$SR
journal.df$SR <- NULL
journal <- journal.df[,c("id_journal","iso_source_abbreviation", "publication_name")]
names(journal)[1] <- "id"

# Creating ReferenceLink entity

referencelink.df <- scopus.data[,c("SR","cited_references")]
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

publisher <- scopus.data[,c("publisher", "publisher_city", "publisher_address", "SR")]
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

conference <- scopus.data[,c("conference_title", "conference_date", "conference_location", 
                          "conference_sponsor")]

conference <- conference[complete.cases(conference) == TRUE,]

# Fundinng entity

funding <- scopus.data[,c("funding_agency_grant_number", "funding_text")]
funding <- funding[complete.cases(funding) == TRUE, ]


# Changing id_names of the main tables

names(paper)[1] <- "id"
names(publisher)[1] <- "id"
names(funding)[1] <- "id"

names(paper)[33] <- "id_conference"
names(paper)[34] <- "id_journal"

# Deleting dataframes

rm(address_split)
rm(address.df)
rm(author_df_rows)
rm(author_mx_rows)
rm(author.df.1)
rm(authors_address)
rm(authors_full_name)
rm(authors_full_row)
rm(authors_list)
rm(authors_row)
rm(df)
rm(df.y)
rm(email_row)
rm(fields)
rm(journal.df)
rm(new_row_1)
rm(publisher.df.1)
rm(referencelink.df)
rm(referencelink.df.1)
rm(row_1)
rm(author.df)

