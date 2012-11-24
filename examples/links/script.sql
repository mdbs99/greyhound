DROP TABLE IF EXISTS "access";
CREATE TABLE "access" ("id" integer primary key autoincrement, "name" varchar(30));
insert into access (name) values ('admin');
insert into access (name) values ('default');

DROP TABLE IF EXISTS "user";
CREATE TABLE "user" ("id" integer primary key autoincrement, "login" varchar(30), "passwd" varchar(10), "name" varchar(30), "access_id" integer);
insert into user (login,passwd,name,access_id) values ('admin','admin','admin',1);
insert into user (login,passwd,name,access_id) values ('guest','123','guest',2);
insert into user (login,passwd,name,access_id) values ('mark','mark12','Mark',2);

