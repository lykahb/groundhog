-- the referenced unique index should turn into key, the other should not
CREATE TABLE tbl1 (a INTEGER, b INTEGER, c INTEGER, d INTEGER, UNIQUE(a, b), UNIQUE(c, d));
CREATE UNIQUE INDEX ind1 ON tbl1 (a, b);
CREATE UNIQUE INDEX ind2 ON tbl1 (c, d);
CREATE TABLE tbl2 (a INTEGER, b INTEGER, FOREIGN KEY (a, b) REFERENCES tbl1 (c, d));
