CREATE TABLE ReferenceLink (
  id_paper0 TEXT,
  id_paper1 TEXT,
  FOREIGN KEY (id_paper0) REFERENCES Paper(id),
  FOREIGN KEY (id_paper1) REFERENCES Paper(id)
);
