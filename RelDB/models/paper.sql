CREATE TABLE Paper (

  id TEXT,
  doi TEXT,
  title TEXT,
  year_published NUMERIC,
  volume NUMERIC,
  abstract TEXT,
  authors_keywords TEXT,
  affiliation TEXT,
  document_type TEXT,
  publication_type TEXT,
  language TEXT,
  reprint_address TEXT,
  issn TEXT,
  eissn TEXT,
  source_abbreviation_29_character TEXT,
  iso_source_abbreviation TEXT,
  publication_date TEXT,
  issue TEXT,
  beginning_page NUMERIC,
  ending_page NUMERIC,
  page_count NUMERIC
  web_of_science_categories TEXT,
  research_areas TEXT,
  document_delivery_number TEXT,
  accession_number TEXT,
  keyword_plus TEXT,
  open_access_indicator TEXT,
  pudmed_id TEXT,
  special_issue TEXT,
  book_series_title TEXT,
  meeting_abstract TEXT,
  editors TEXT,
  
  id_funging TEXT,
  id_conference TEXT,
  id_journal TEXT,
  
  FOREIGN KEY (id_funging) REFERENCES Funding(id),
  FOREIGN KEY (id_conference) REFERENCES Conference(id),
  FOREIGN KEY (id_journal) REFERENCES Journal(id)
);
