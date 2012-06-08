DROP TABLE IF EXISTS "access";
CREATE TABLE "access" ("id" int, "name" varchar(40));
INSERT INTO "access" VALUES(1,'admin');
INSERT INTO "access" VALUES(2,'default');
DROP TABLE IF EXISTS "user";
CREATE TABLE user (id int, login varchar(30), "access_id" int);
INSERT INTO "user" VALUES(1,'bob',2);
INSERT INTO "user" VALUES(2,'paul',1);
