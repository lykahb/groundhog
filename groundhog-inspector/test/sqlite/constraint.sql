-- the referenced unique constraint should turn into key, the other should not
CREATE TABLE tbl1 (a INTEGER, b INTEGER, c INTEGER, d INTEGER, UNIQUE(a, b), UNIQUE(c, d));
CREATE TABLE tbl2 (a INTEGER, b INTEGER, FOREIGN KEY (a, b) REFERENCES tbl1 (c, d));
