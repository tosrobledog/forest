CREATE TABLE Paper (
  id VARCHAR(70) PRIMARY KEY,
  title TEXT,
  year INTEGER,
  volume INTEGER,
  doi VARCHAR(50),
  affiliation TEXT,
  abstract TEXT,
  keywords TEXT,
  publisher TEXT,
  document_type VARCHAR(15)
);

CREATE TABLE Author (
  id VARCHAR(70) PRIMARY KEY,
  name TEXT,
  short_name VARCHAR(20),
  email VARCHAR(255)
);

CREATE TABLE Journal (
  id VARCHAR(70) PRIMARY KEY,
  name TEXT,
  short_name VARCHAR(20)
);

CREATE TABLE PaperAuthor (
  id_paper VARCHAR(70),
  id_author VARCHAR(70),
  FOREIGN KEY(id_paper) REFERENCES Paper(id),
  FOREIGN KEY(id_author) REFERENCES Author(id)
);

CREATE TABLE PaperJournal (
  id_paper VARCHAR(70),
  id_journal VARCHAR(70),
  FOREIGN KEY(id_paper) REFERENCES Paper(id),
  FOREIGN KEY(id_journal) REFERENCES Author(id)
);

CREATE TABLE ReferenceLink (
  id_paper0 VARCHAR(70),
  id_paper1 VARCHAR(70),
  FOREIGN KEY(id_paper0) REFERENCES Paper(id),
  FOREIGN KEY(id_paper1) REFERENCES Paper(id)
);
