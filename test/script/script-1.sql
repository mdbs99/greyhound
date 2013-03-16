DROP TABLE IF EXISTS "role";
CREATE TABLE "role" ("id" integer primary key autoincrement, "name" varchar(30));
insert into role (name) values ('admin');
insert into role (name) values ('public');

DROP TABLE IF EXISTS "user";
CREATE TABLE "user" ("id" integer primary key autoincrement, "login" varchar(30), "passwd" varchar(10), "name" varchar(30), "role_id" integer);
insert into user (login,passwd,name,role_id) values ('admin','admin','admin',1);
insert into user (login,passwd,name,role_id) values ('guest','123','guest',2);
insert into user (login,passwd,name,role_id) values ('mark','mark12','Mark',2);

