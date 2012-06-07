Greyhound Project
Copyright (c) 2012 by Marcos Douglas B. dos Santos

Make your code more simple, fast and clean.
This project uses the DRY concept or Don't Repeat Yourself.

Compiles with FPC 2.6.0 (better with 2.6.1)


Features
====================================================

Database (aka DB) module
----------------------------------------------------
- Use one simple interface, but you can choose any Lib (aka Broker) to connect in a DBMS like SQLdb, Zeos or whatever you want;
- CRUD: TghDBTable class to manipulate one table easily (like a ActiveRecord, but you can work with many records at the same object);
- Links: You can use Table Links to access tables that have relationship;
- Scripts and Tables: TghDBConnector class to manipulate all tables on the DB and execute Script SQL as well;
- Memory management automatic for many objects;
- JSON Support (FPC 2.6.1);


Many other features will come.


Best regards,

Marcos Douglas