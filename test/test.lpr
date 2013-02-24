program test;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, gh_SQLTest, gh_DataTest, gh_JSONTest;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

