Greyhound
Copyright (c) 2012 by Marcos Douglas B. dos Santos

Make your code more simple, fast and clean.
This project uses the DRY concept or Don't Repeat Yourself.

Compiles with FPC 2.6.0 (better with 2.6.1)


Features
====================================================


Data Module
----------------------------------------------------
- Use one simple interface, but you can choose any Lib (aka Broker) to connect in a DBMS like SQLdb, Zeos or whatever you want;
- CRUD: TghSQLTable class to manipulate one table easily, like a ActiveRecord, 
but you can work with many records at the same Table instance;
- Relations: You can define relationship between tables easily;
- Links: You can use Table Links to access tables that have relationship;
- Scripts and Tables: TghSQLConnector class to manipulate all tables on the DBMS and execute Script SQL as well;
- Automatic memory management for many objects;
- JSON Support (FPC 2.6.1);

----------------------------------------------------


Many other features will come. Follow the updates.


Best regards,
Marcos Douglas