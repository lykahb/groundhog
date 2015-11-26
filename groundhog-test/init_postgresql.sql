create database test;
create user test password 'test';
grant all on database test to test;
\c test
create extension if not exists hstore;
alter schema public owner to test;

