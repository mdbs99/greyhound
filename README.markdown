#Greyhound Project

##What is?
* Greyhound is a tiny ORM-ish for [Free Pascal](http://freepascal.org/);
* It will help you with a thin layer to read/write data in a DBMS, but don't trying to simulate a pure object model;
* It use SQL as query language and does not try to create a complex abstraction between objects and tables;
* It implements relationship between tables using "Links" property;
* It implements constraints for tables (default, check and unique);
* It was inspired in ActiveRecord pattern but have dynamic columns, not "attributes";
* It allows developers to have greater control of SQL rather than relying on the framework to generate it automatically;
* It has easy transaction support;
* It has the ability to add support for other databases libraries (SQLdb is default) like, ZEOS or whatever you want;
* It is fast, clean and simple to use.


##What FPC version it will need?
Greyhound compiles with FPC 2.6.0 (better with 2.6.1 or above).


##Who uses it?
[Brook framework](http://brookframework.org)