CREATE TABLE PaperPublisher (
  id_paper TEXT,
  id_publisher TEXT,
  FOREIGN KEY (id_paper) REFERENCES Paper(id),
  FOREIGN KEY (id_publisher) REFERENCES Publisher(id)
);
