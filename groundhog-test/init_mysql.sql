create database if not exists test;
use test;
create user test identified by 'test';
grant all privileges on *.* to 'test'@'%' with grant option;
flush privileges;
