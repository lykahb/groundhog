create database test;
create user test password 'test';
grant all on database test to test;
\c test
alter schema public owner to test;

