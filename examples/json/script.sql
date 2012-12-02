DROP TABLE IF EXISTS "user";

CREATE TABLE "user" ("id" integer primary key autoincrement, "login" varchar(30), "passwd" varchar(10), "name" varchar(30));

insert into user (login,passwd,name) values ('admin','admin','admin');

