CREATE TABLE PaperAuthor (
  id_paper TEXT,
  id_author TEXT,
  FOREIGN KEY (id_paper) REFERENCES Paper(id),
  FOREIGN KEY (id_author) REFERENCES Author(id)
);
