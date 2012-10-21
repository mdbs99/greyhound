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

function gh_OSSysEncoding(const AStr: string): string;
function gh_OSExeEncoding(const AStr: string): string;

function gh_OSParamStr(AIndex: Integer): string;
function gh_OSOpenURL(const AUrl: string): Boolean;
function gh_OSFileSize(const AFileName: string): Integer;

function gh_OSAppendPathDelimiter(const APath: string): string;
function gh_OSGetExePath: string;
function gh_OSGetExeName: string;

function gh_OSFindFirst(const APath: string; AAttr: LongInt; out ARec: TSearchRec): LongInt;
function gh_OSFindNext(var ARec: TSearchRec): LongInt;
function gh_OSGetCurrentDir: string;
function gh_OSSetCurrentDir(const ANewDir: string): Boolean;
function gh_OSExpandFileName(const AFileName: string): string;
function gh_OSFileExists(const AFileName: string): Boolean;
function gh_OSRenameFile(const AOldName, ANewName: string): Boolean;
function gh_OSDeleteFile(const AFileName: string): Boolean;
function gh_OSExtractFileDir(const AFileName: string): string;
function gh_OSExtractFilePath(const AFileName: string): string;
function gh_OSExtractFileName(const AFileName: string): string;
function gh_OSExtractFileNameOnly(const AFileName: string): string;
function gh_OSExtractFileExt(const AFileName: string): string;
function gh_OSChangeFileExt(const AFileName, AExt: string): string;
function gh_OSDirExists(const ADir: string): Boolean;
function gh_OSForceDir(const ADir: string): Boolean;

implementation

function gh_OSSysEncoding(const AStr: string): string;
begin
  {$IFDEF WINDOWS}
  Result := Utf8ToAnsi(AStr);
  {$ELSE}
  Result := AStr;
  {$ENDIF}
end;

function gh_OSExeEncoding(const AStr: string): string;
begin
  {$IFDEF WINDOWS}
  Result := AnsiToUtf8(AStr);
  {$ELSE}
  Result := AStr;
  {$ENDIF}
end;

function gh_OSParamStr(AIndex: Integer): string;
begin
  Result := gh_OSExeEncoding(ParamStr(AIndex));
end;

function gh_OSOpenURL(const AUrl: string): Boolean;
begin
  Result := LCLIntf.OpenURL(AUrl);
end;

function gh_OSFileSize(const AFileName: string): Integer;
begin
  // TODO
end;

function gh_OSAppendPathDelimiter(const APath: string): string;
begin
  Result := IncludeTrailingPathDelimiter(APath);
end;

function gh_OSGetExePath: string;
begin
  Result := gh_OSAppendPathDelimiter(gh_OSExtractFileDir(gh_OSParamStr(0)));
end;

function gh_OSGetExeName: string;
begin
  Result := gh_OSChangeFileExt(gh_OSExtractFileName(gh_OSParamStr(0)), '');
end;

function gh_OSFindFirst(const APath: string; AAttr: LongInt; out ARec: TSearchRec): LongInt;
begin
  Result := FindFirst(gh_OSSysEncoding(APath), AAttr, ARec);
  ARec.Name := gh_OSExeEncoding(ARec.Name);
end;

function gh_OSFindNext(var ARec: TSearchRec): LongInt;
begin
  Result := FindNext(ARec);
  ARec.Name := gh_OSExeEncoding(ARec.Name);
end;

function gh_OSGetCurrentDir: string;
begin
  Result := gh_OSExeEncoding(GetCurrentDir);
end;

function gh_OSSetCurrentDir(const ANewDir: string): Boolean;
begin
  Result := SetCurrentDir(gh_OSSysEncoding(ANewDir));
end;

function gh_OSExpandFileName(const AFileName: string): string;
begin
  Result := gh_OSExeEncoding(ExpandFileName(gh_OSSysEncoding(AFileName)));
end;

function gh_OSFileExists(const AFileName: string): Boolean;
begin
  Result := FileExists(gh_OSSysEncoding(AFileName));
end;

function gh_OSRenameFile(const AOldName, ANewName: string): Boolean;
begin
  Result := RenameFile(gh_OSSysEncoding(AOldName), gh_OSSysEncoding(ANewName));
end;

function gh_OSDeleteFile(const AFileName: string): Boolean;
begin
  Result := SysUtils.DeleteFile(gh_OSSysEncoding(AFileName));
end;

function gh_OSExtractFileDir(const AFileName: string): string;
begin
  Result := gh_OSExeEncoding(ExtractFileDir(gh_OSSysEncoding(AFileName)));
end;

function gh_OSExtractFilePath(const AFileName: string): string;
begin
  Result := gh_OSExeEncoding(gh_OSAppendPathDelimiter(ExtractFilePath(gh_OSSysEncoding(AFilename))));
end;

function gh_OSExtractFileName(const AFileName: string): string;
begin
  Result := gh_OSExeEncoding(ExtractFileName(gh_OSSysEncoding(AFilename)));
end;

function gh_OSExtractFileNameOnly(const AFileName: string): string;
begin
  Result := gh_OSExeEncoding(ExtractFileNameOnly(gh_OSSysEncoding(AFilename)));
end;

function gh_OSExtractFileExt(const AFileName: string): string;
begin
  Result := gh_OSExeEncoding(ExtractFileExt(gh_OSSysEncoding(AFileName)));
end;

function gh_OSChangeFileExt(const AFileName, AExt: string): string;
begin
  Result := gh_OSExeEncoding(ChangeFileExt(gh_OSSysEncoding(AFilename), AExt));
end;

function gh_OSDirExists(const ADir: string): Boolean;
begin
  Result := DirectoryExists(gh_OSSysEncoding(ADir));
end;

function gh_OSForceDir(const ADir: string): Boolean;
begin
  Result := ForceDirectories(gh_OSSysEncoding(ADir));
end;

end.
