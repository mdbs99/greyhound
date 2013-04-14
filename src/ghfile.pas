{
    Greyhound
    Copyright (C) 2012-2013  -  Marcos Douglas B. dos Santos

    See the files COPYING.GH, included in this
    distribution, for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
}

// This unit was inspired by fpGUI solution to platform encoding.

unit ghFile;

interface

uses
  // fpc
  Classes, SysUtils,
  // laz
  FileUtil, LazUTF8, LCLIntf;

// encoding
function ghSysEncoding(const S: string): string; inline;
function ghExeEncoding(const S: string): string; inline;

// exe
function ghExeGetParam(AIndex: Integer): string;
function ghExeGetPath: string;
function ghExeGetName: string;
function ghExeGetCurrDir: string;
function ghExeSetCurrDir(const ANewDir: string): Boolean;

// util
function ghUrlOpen(const AUrl: string): Boolean;

// file and path
function ghDirExists(const ADir: string): Boolean;
function ghDirForce(const ADir: string): Boolean;
function ghDirAddDelim(const APath: string): string;
function ghFileFindFirst(const APath: string; AAttr: LongInt; out ARec: TSearchRec): LongInt;
function ghFileFindNext(var ARec: TSearchRec): LongInt;
function ghFileExpandName(const AFileName: string): string;
function ghFileExists(const AFileName: string): Boolean;
function ghFileSize(const AFileName: string): Int64;
function ghFileRename(const AOldName, ANewName: string): Boolean;
function ghFileDelete(const AFileName: string): Boolean;
function ghFileExtractDir(const AFileName: string): string;
function ghFileExtractPath(const AFileName: string): string;
function ghFileExtractName(const AFileName: string): string;
function ghFileExtractNameOnly(const AFileName: string): string;
function ghFileExtractExt(const AFileName: string): string;
function ghFileChangeExt(const AFileName, AExt: string): string;

implementation

function ghSysEncoding(const S: string): string;
begin
  Result := UTF8ToSys(S);
end;

function ghExeEncoding(const S: string): string;
begin
  Result := SysToUTF8(S);
end;

function ghExeGetParam(AIndex: Integer): string;
begin
  Result := ghExeEncoding(ParamStr(AIndex));
end;

function ghExeGetPath: string;
begin
  Result := ghDirAddDelim(ghFileExtractDir(ghExeGetParam(0)));
end;

function ghExeGetName: string;
begin
  Result := ghFileChangeExt(ghFileExtractName(ghExeGetParam(0)), '');
end;

function ghExeGetCurrDir: string;
begin
  Result := ghExeEncoding(SysUtils.GetCurrentDir);
end;

function ghExeSetCurrDir(const ANewDir: string): Boolean;
begin
  Result := SysUtils.SetCurrentDir(ghSysEncoding(ANewDir));
end;

function ghUrlOpen(const AUrl: string): Boolean;
begin
  Result := LCLIntf.OpenURL(AUrl);
end;

function ghDirExists(const ADir: string): Boolean;
begin
  Result := SysUtils.DirectoryExists(ghSysEncoding(ADir));
end;

function ghDirForce(const ADir: string): Boolean;
begin
  Result := SysUtils.ForceDirectories(ghSysEncoding(ADir));
end;

function ghDirAddDelim(const APath: string): string;
begin
  Result := IncludeTrailingPathDelimiter(APath);
end;

function ghFileFindFirst(const APath: string; AAttr: LongInt; out ARec: TSearchRec): LongInt;
begin
  Result := SysUtils.FindFirst(ghSysEncoding(APath), AAttr, ARec);
  ARec.Name := ghExeEncoding(ARec.Name);
end;

function ghFileFindNext(var ARec: TSearchRec): LongInt;
begin
  Result := SysUtils.FindNext(ARec);
  ARec.Name := ghExeEncoding(ARec.Name);
end;

function ghFileExpandName(const AFileName: string): string;
begin
  Result := ghExeEncoding(SysUtils.ExpandFileName(ghSysEncoding(AFileName)));
end;

function ghFileExists(const AFileName: string): Boolean;
begin
  Result := SysUtils.FileExists(ghSysEncoding(AFileName));
end;

function ghFileSize(const AFileName: string): Int64;
begin
  Result := FileUtil.FileSize(AFileName);
end;

function ghFileRename(const AOldName, ANewName: string): Boolean;
begin
  Result := SysUtils.RenameFile(ghSysEncoding(AOldName), ghSysEncoding(ANewName));
end;

function ghFileDelete(const AFileName: string): Boolean;
begin
  Result := SysUtils.DeleteFile(ghSysEncoding(AFileName));
end;

function ghFileExtractDir(const AFileName: string): string;
begin
  Result := ghExeEncoding(SysUtils.ExtractFileDir(ghSysEncoding(AFileName)));
end;

function ghFileExtractPath(const AFileName: string): string;
begin
  Result := ghExeEncoding(SysUtils.ExtractFilePath(ghSysEncoding(AFilename)));
end;

function ghFileExtractName(const AFileName: string): string;
begin
  Result := ghExeEncoding(SysUtils.ExtractFileName(ghSysEncoding(AFilename)));
end;

function ghFileExtractNameOnly(const AFileName: string): string;
begin
  Result := ghExeEncoding(FileUtil.ExtractFileNameOnly(ghSysEncoding(AFilename)));
end;

function ghFileExtractExt(const AFileName: string): string;
begin
  Result := ghExeEncoding(SysUtils.ExtractFileExt(ghSysEncoding(AFileName)));
end;

function ghFileChangeExt(const AFileName, AExt: string): string;
begin
  Result := ghExeEncoding(SysUtils.ChangeFileExt(ghSysEncoding(AFilename), AExt));
end;

end.
