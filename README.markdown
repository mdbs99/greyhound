#Greyhound

Greyhound is a tiny ORM-ish for [Free Pascal](http://freepascal.org/).

It is fast, clean and simple to use.

``` pascal
program t1;
{$mode objfpc}{$H+}
uses
  SysUtils, ghSQL, ghSQLdbLib;
var
  Co: TghSQLConnector;
  User: TghSQLTable;
begin
  Co := TghSQLConnector.Create(TghSQLite3Lib);
  try
    Co.Database := 'DB.sqlite';
    User := Co.Tables['user'];
    User.Select('*').Where('id = %d', [2]).Open;
    User.Edit;
    User['name'].AsString := 'Free Pascal';
    if User.Post.HasErrors then
    begin
      writeln('ERROR: ' + User.GetErrors.Text);
      User.Cancel;
    end
    else
      User.Commit;
  finally
    Co.Free;
  end;
  writeln('Done.');
  writeln;
end.
```

##Features
* It will help you with a thin layer to read/write data in a DBMS, but don't trying to simulate a pure object model;
* It use SQL as query language and does not try to create a complex abstraction between objects and tables;
* It implements relationship between tables using "Links" property (1-n, n-1, m-n... whatever you want);
* It implements constraints for tables (default, check and unique);
* It was inspired in ActiveRecord pattern;
* It allows developers to have greater control of SQL rather than relying on the framework to generate it automatically;
* It has easy transaction support;
* It has the ability to add support for other databases libraries like SQLdb (default), ZEOS or whatever you want;
* It works with IDENTITY columns (AutoInc) automatic and Sequence values (generators);


##System requirements
Greyhound requires Free Pascal 2.6.0 or newer.


##Who uses?
* [ghORM](https://github.com/leledumbo/ghorm)
* [Brook framework](http://brookframework.org)
* [fpcup](https://bitbucket.org/reiniero/fpcup/)
* [luipack](http://code.google.com/p/luipack/)
* [codetyphon](http://www.pilotlogic.com/)
