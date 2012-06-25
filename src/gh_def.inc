{
    Greyhound
    Copyright (c) 2012

    See the files COPYING.GH, included in this
    distribution, for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

{$IFDEF FPC}
  {$MODE objfpc}{$H+}
  {$INTERFACES corba}
  {$DEFINE SQLite3Broker}
  {.$DEFINE MSSQLBroker}
  {$IF ((FPC_VERSION >= 2) and (FPC_RELEASE >= 6) and (FPC_PATCH >= 1))}
    {$DEFINE JSON_SUPPORT}
  {$ENDIF}
{$ENDIF}

