{
    Greyhound
    Copyright (C) 2012-2013  -  Marcos Douglas B. dos Santos

    See the file LICENSE.txt, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

{$IFDEF FPC}
  {$MODE objfpc}{$H+}
  {$INTERFACES CORBA}
  {$IF ((FPC_VERSION >= 2) and (FPC_RELEASE >= 6) and (FPC_PATCH >= 1))}
    {$DEFINE MSSQL_LIB}
  {$ELSE}
    {$DEFINE FPC2_6_0}
  {$ENDIF}
{$ENDIF}

