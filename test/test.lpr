program test;

{$mode objfpc}{$H+}

uses
  Interfaces, Forms, GuiTestRunner, gh_DataTest, gh_SQLTest, gh_JSONTest,
  zcomponent;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TGuiTestRunner, TestRunner);
  Application.Run;
end.

