{
    Greyhound
    Copyright (C) 2012  -  Marcos Douglas B. dos Santos

    See the files COPYING.GH, included in this
    distribution, for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

{$IFDEF FPC}
  {$MODE objfpc}{$H+}
  {$INTERFACES corba}
  {$IF ((FPC_VERSION >= 2) and (FPC_RELEASE >= 6) and (FPC_PATCH >= 1))}
    {$DEFINE SQLDB_LIB}
    {$IFDEF SQLDB_LIB}
      {$DEFINE SQLDB_LIB}
      {$DEFINE SQLITE3_LIB}
      {$DEFINE MSSQL_LIB}
    {$ENDIF}
  {$ELSE}
    {$DEFINE FPC2_6_0}
  {$ENDIF}
{$ENDIF}

