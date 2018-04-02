# Required libraries
install.packages("RSQLite")

# Import
library(RSQLite)

# Connection to the database
## For now, SQLite will be used
conn <- dbConnect(SQLite(), "forest.sqlite")

# Create tables

## Read file with SQL code
sql.0 <- readLines("tables.sql")
sql.1 <- paste(sql.0, collapse = '\n')

## Snippets (delimited by semicolons) for each table
sql_statements <- strsplit(sql.1, "(?<=\\);)\\n", perl = TRUE)[[1]]

## Executing each query
for (statement in sql_statements) {
  dbSendQuery(conn, statement)
}

# Check created tables
dbListTables(conn)

# Disconnect the database
dbDisconnect(conn)
