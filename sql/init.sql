drop database if exists stocker;
drop role if exists stocker;

-- create the stocker user

CREATE USER stocker createdb createuser password 'welcome1';

-- create a sinfonifry database
CREATE database stocker owner stocker;
/CONNECT stocker

DROP TABLE IF EXISTS company;

CREATE TABLE company
(
  cik character(10) NOT NULL,
  ticker character varying (25) NOT NULL,
  company_name TEXT,
  PRIMARY KEY (cik)
);

ALTER TABLE company OWNER TO stocker;

DROP TABLE IF EXISTS document;

CREATE TABLE document
(
  id BIGSERIAL NOT NULL,
  hash UUID NOT NULL,
  cik INTEGER NOT NULL,
  document_period_end_date DATE NOT NULL,
  document_fiscal_year_focus SMALLINT NOT NULL,
  current_fiscal_year_end_date DATE,
  common_stock_shares_outstanding BIGINT NOT NULL,
  document_path TEXT NOT NULL,
  document_fiscal_period_focus TEXT NOT NULL,
  entity_filer_category TEXT NOT NULL,
  document_type TEXT NOT NULL,
  amendment_flag BOOLEAN NOT NULL,
  PRIMARY KEY (id),
  UNIQUE (hash),
  FOREIGN KEY (cik) REFERENCES company (cik)
);

ALTER TABLE document OWNER TO stocker;
