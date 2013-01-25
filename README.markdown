#Greyhound Project

Greyhound is a tiny ORM-ish for [Free Pascal](http://freepascal.org/).
It is fast, clean and simple to use.


##Features
* It will help you with a thin layer to read/write data in a DBMS, but don't trying to simulate a pure object model;
* It use SQL as query language and does not try to create a complex abstraction between objects and tables;
* It implements relationship between tables using "Links" property;
* It implements constraints for tables (default, check and unique);
* It was inspired in ActiveRecord pattern but have dynamic columns, not "attributes";
* It allows developers to have greater control of SQL rather than relying on the framework to generate it automatically;
* It has easy transaction support;
* It has the ability to add support for other databases libraries (SQLdb is default) like, ZEOS or whatever you want;
* It works with IDENTITY columns (AutoInc) automatic and Sequence values (generators);


##System requirements
Greyhound requires Free Pascal 2.6.0 or newer.


##Who uses it?
* [Brook framework](http://brookframework.org)