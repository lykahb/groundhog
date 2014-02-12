create database if not exists test;
use test;
create user 'test'@'localhost' identified by 'test';
grant all privileges on *.* to 'test'@'localhost' with grant option;
flush privileges;
