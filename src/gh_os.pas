{
    Greyhound
    Copyright (c) 2012

    See the files COPYING.GH, included in this
    distribution, for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

// This unit was inspired by fpGUI solution to platform encoding.

unit gh_OS;

{$i gh_def.inc}

interface

uses
  // fpc
  Classes, SysUtils,
  // laz
  FileUtil, LazUTF8, LCLIntf,
{$IFDEF WINDOWS}
  Windows,
{$ENDIF}
  // gh
  gh_Global;

// encoding
function gh_SysEncoding(const S: string): string;
function gh_ExeEncoding(const S: string): string;

// exe
function gh_GetExeParam(AIndex: Integer): string;
function gh_GetExePath: string;
function gh_GetExeName: string;

// file and path
function gh_AddPathDelimiter(const APath: string): string;
function gh_FindFirst(const APath: string; AAttr: LongInt; out ARec: TSearchRec): LongInt;
function gh_FindNext(var ARec: TSearchRec): LongInt;
function gh_GetCurrentDir: string;
function gh_SetCurrentDir(const ANewDir: string): Boolean;
function gh_ExpandFileName(const AFileName: string): string;
function gh_FileExists(const AFileName: string): Boolean;
function gh_FileSize(const AFileName: string): Integer;
function gh_RenameFile(const AOldName, ANewName: string): Boolean;
function gh_DeleteFile(const AFileName: string): Boolean;
function gh_ExtractFileDir(const AFileName: string): string;
function gh_ExtractFilePath(const AFileName: string): string;
function gh_ExtractFileName(const AFileName: string): string;
function gh_ExtractFileNameOnly(const AFileName: string): string;
function gh_ExtractFileExt(const AFileName: string): string;
function gh_ChangeFileExt(const AFileName, AExt: string): string;
function gh_DirExists(const ADir: string): Boolean;
function gh_ForceDir(const ADir: string): Boolean;

// util
function gh_OpenURL(const AUrl: string): Boolean;

implementation

function gh_SysEncoding(const S: string): string;
begin
  {$IFDEF WINDOWS}
  Result := Utf8ToAnsi(S);
  {$ELSE}
  Result := S;
  {$ENDIF}
end;

function gh_ExeEncoding(const S: string): string;
begin
  {$IFDEF WINDOWS}
  Result := AnsiToUtf8(S);
  {$ELSE}
  Result := S;
  {$ENDIF}
end;

function gh_GetExeParam(AIndex: Integer): string;
begin
  Result := gh_ExeEncoding(ParamStr(AIndex));
end;

function gh_GetExePath: string;
begin
  Result := gh_AddPathDelimiter(gh_ExtractFileDir(gh_GetExeParam(0)));
end;

function gh_GetExeName: string;
begin
  Result := gh_ChangeFileExt(gh_ExtractFileName(gh_GetExeParam(0)), '');
end;

function gh_AddPathDelimiter(const APath: string): string;
begin
  Result := IncludeTrailingPathDelimiter(APath);
end;

function gh_FindFirst(const APath: string; AAttr: LongInt; out ARec: TSearchRec): LongInt;
begin
  Result := FindFirst(gh_SysEncoding(APath), AAttr, ARec);
  ARec.Name := gh_ExeEncoding(ARec.Name);
end;

function gh_FindNext(var ARec: TSearchRec): LongInt;
begin
  Result := FindNext(ARec);
  ARec.Name := gh_ExeEncoding(ARec.Name);
end;

function gh_GetCurrentDir: string;
begin
  Result := gh_ExeEncoding(GetCurrentDir);
end;

function gh_SetCurrentDir(const ANewDir: string): Boolean;
begin
  Result := SetCurrentDir(gh_SysEncoding(ANewDir));
end;

function gh_ExpandFileName(const AFileName: string): string;
begin
  Result := gh_ExeEncoding(ExpandFileName(gh_SysEncoding(AFileName)));
end;

function gh_FileExists(const AFileName: string): Boolean;
begin
  Result := FileExists(gh_SysEncoding(AFileName));
end;

function gh_FileSize(const AFileName: string): Integer;
begin
  // TODO
end;

function gh_RenameFile(const AOldName, ANewName: string): Boolean;
begin
  Result := RenameFile(gh_SysEncoding(AOldName), gh_SysEncoding(ANewName));
end;

function gh_DeleteFile(const AFileName: string): Boolean;
begin
  Result := SysUtils.DeleteFile(gh_SysEncoding(AFileName));
end;

function gh_ExtractFileDir(const AFileName: string): string;
begin
  Result := gh_ExeEncoding(ExtractFileDir(gh_SysEncoding(AFileName)));
end;

function gh_ExtractFilePath(const AFileName: string): string;
begin
  Result := gh_ExeEncoding(gh_AddPathDelimiter(ExtractFilePath(gh_SysEncoding(AFilename))));
end;

function gh_ExtractFileName(const AFileName: string): string;
begin
  Result := gh_ExeEncoding(ExtractFileName(gh_SysEncoding(AFilename)));
end;

function gh_ExtractFileNameOnly(const AFileName: string): string;
begin
  Result := gh_ExeEncoding(ExtractFileNameOnly(gh_SysEncoding(AFilename)));
end;

function gh_ExtractFileExt(const AFileName: string): string;
begin
  Result := gh_ExeEncoding(ExtractFileExt(gh_SysEncoding(AFileName)));
end;

function gh_ChangeFileExt(const AFileName, AExt: string): string;
begin
  Result := gh_ExeEncoding(ChangeFileExt(gh_SysEncoding(AFilename), AExt));
end;

function gh_DirExists(const ADir: string): Boolean;
begin
  Result := DirectoryExists(gh_SysEncoding(ADir));
end;

function gh_ForceDir(const ADir: string): Boolean;
begin
  Result := ForceDirectories(gh_SysEncoding(ADir));
end;

function gh_OpenURL(const AUrl: string): Boolean;
begin
  Result := LCLIntf.OpenURL(AUrl);
end;

end.
