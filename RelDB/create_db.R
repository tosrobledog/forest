# Required libraries
install.packages("RSQLite")

# Import
library(RSQLite)

# Connection to the database
## For now, SQLite will be used
conn <- dbConnect(SQLite(), "forest.sqlite")

# Create tables

## Read file with SQL code
sql.statements.0 <- c(
  readLines("RelDB/models/journal.sql"),
  readLines("RelDB/models/funding.sql"),
  readLines("RelDB/models/conference.sql"),
  readLines("RelDB/models/paper.sql"),
  readLines("RelDB/models/referenceLink.sql"),
  readLines("RelDB/models/author.sql"),
  readLines("RelDB/models/paperAuthor.sql"),
  readLines("RelDB/models/publisher.sql"),
  readLines("RelDB/models/paperPublisher.sql")
)

sql.statements.1 <- paste(sql.statements.0, collapse = '\n')

## Snippets (delimited by semicolons) for each table
sql.statements <- strsplit(sql.statements.1, "(?<=\\);)\\n", perl = TRUE)[[1]]

## Executing each query
for (statement in sql.statements) {
  dbSendQuery(conn, statement)
}

# Check created tables
dbListTables(conn)

# Disconnect the database
dbDisconnect(conn)
