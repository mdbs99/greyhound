program test;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, gh_DataTest, gh_SQLTest, gh_JSONTest;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

