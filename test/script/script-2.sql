DROP TABLE IF EXISTS "role";
CREATE TABLE "role" ("id" integer primary key autoincrement, "name" varchar(30));
insert into role (name) values ('admin'); -- 1
insert into role (name) values ('analyst'); -- 2
insert into role (name) values ('programmer'); -- 3
insert into role (name) values ('usert'); -- 4

DROP TABLE IF EXISTS "user";
CREATE TABLE "user" ("id" integer primary key autoincrement, "login" varchar(30), "passwd" varchar(10), "name" varchar(30));
insert into user (login,passwd,name) values ('admin','admin','admin'); -- 1
insert into user (login,passwd,name) values ('jonh','123','jonh'); -- 2
insert into user (login,passwd,name) values ('user','123','user'); -- 3

DROP TABLE IF EXISTS "role_user";
CREATE TABLE "role_user" ("id" integer primary key autoincrement, "role_id" integer, "user_id" integer);
insert into role_user (role_id,user_id) values (1,1);
insert into role_user (role_id,user_id) values (2,1);
insert into role_user (role_id,user_id) values (2,2);
insert into role_user (role_id,user_id) values (3,2);
insert into role_user (role_id,user_id) values (4,3);
