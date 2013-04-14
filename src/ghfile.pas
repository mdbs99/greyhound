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
function gh_SysEncoding(const S: string): string; inline;
function gh_ExeEncoding(const S: string): string; inline;

// exe
function gh_ExeGetParam(AIndex: Integer): string;
function gh_ExeGetPath: string;
function gh_ExeGetName: string;
function gh_ExeGetCurrDir: string;
function gh_ExeSetCurrDir(const ANewDir: string): Boolean;

// util
function gh_UrlOpen(const AUrl: string): Boolean;

// file and path
function gh_DirExists(const ADir: string): Boolean;
function gh_DirForce(const ADir: string): Boolean;
function gh_DirAddDelim(const APath: string): string;
function gh_FileFindFirst(const APath: string; AAttr: LongInt; out ARec: TSearchRec): LongInt;
function gh_FileFindNext(var ARec: TSearchRec): LongInt;
function gh_FileExpandName(const AFileName: string): string;
function gh_FileExists(const AFileName: string): Boolean;
function gh_FileSize(const AFileName: string): Int64;
function gh_FileRename(const AOldName, ANewName: string): Boolean;
function gh_FileDelete(const AFileName: string): Boolean;
function gh_FileExtractDir(const AFileName: string): string;
function gh_FileExtractPath(const AFileName: string): string;
function gh_FileExtractName(const AFileName: string): string;
function gh_FileExtractNameOnly(const AFileName: string): string;
function gh_FileExtractExt(const AFileName: string): string;
function gh_FileChangeExt(const AFileName, AExt: string): string;

implementation

function gh_SysEncoding(const S: string): string;
begin
  Result := UTF8ToSys(S);
end;

function gh_ExeEncoding(const S: string): string;
begin
  Result := SysToUTF8(S);
end;

function gh_ExeGetParam(AIndex: Integer): string;
begin
  Result := gh_ExeEncoding(ParamStr(AIndex));
end;

function gh_ExeGetPath: string;
begin
  Result := gh_DirAddDelim(gh_FileExtractDir(gh_ExeGetParam(0)));
end;

function gh_ExeGetName: string;
begin
  Result := gh_FileChangeExt(gh_FileExtractName(gh_ExeGetParam(0)), '');
end;

function gh_ExeGetCurrDir: string;
begin
  Result := gh_ExeEncoding(SysUtils.GetCurrentDir);
end;

function gh_ExeSetCurrDir(const ANewDir: string): Boolean;
begin
  Result := SysUtils.SetCurrentDir(gh_SysEncoding(ANewDir));
end;

function gh_UrlOpen(const AUrl: string): Boolean;
begin
  Result := LCLIntf.OpenURL(AUrl);
end;

function gh_DirExists(const ADir: string): Boolean;
begin
  Result := SysUtils.DirectoryExists(gh_SysEncoding(ADir));
end;

function gh_DirForce(const ADir: string): Boolean;
begin
  Result := SysUtils.ForceDirectories(gh_SysEncoding(ADir));
end;

function gh_DirAddDelim(const APath: string): string;
begin
  Result := IncludeTrailingPathDelimiter(APath);
end;

function gh_FileFindFirst(const APath: string; AAttr: LongInt; out ARec: TSearchRec): LongInt;
begin
  Result := SysUtils.FindFirst(gh_SysEncoding(APath), AAttr, ARec);
  ARec.Name := gh_ExeEncoding(ARec.Name);
end;

function gh_FileFindNext(var ARec: TSearchRec): LongInt;
begin
  Result := SysUtils.FindNext(ARec);
  ARec.Name := gh_ExeEncoding(ARec.Name);
end;

function gh_FileExpandName(const AFileName: string): string;
begin
  Result := gh_ExeEncoding(SysUtils.ExpandFileName(gh_SysEncoding(AFileName)));
end;

function gh_FileExists(const AFileName: string): Boolean;
begin
  Result := SysUtils.FileExists(gh_SysEncoding(AFileName));
end;

function gh_FileSize(const AFileName: string): Int64;
begin
  Result := FileUtil.FileSize(AFileName);
end;

function gh_FileRename(const AOldName, ANewName: string): Boolean;
begin
  Result := SysUtils.RenameFile(gh_SysEncoding(AOldName), gh_SysEncoding(ANewName));
end;

function gh_FileDelete(const AFileName: string): Boolean;
begin
  Result := SysUtils.DeleteFile(gh_SysEncoding(AFileName));
end;

function gh_FileExtractDir(const AFileName: string): string;
begin
  Result := gh_ExeEncoding(SysUtils.ExtractFileDir(gh_SysEncoding(AFileName)));
end;

function gh_FileExtractPath(const AFileName: string): string;
begin
  Result := gh_ExeEncoding(SysUtils.ExtractFilePath(gh_SysEncoding(AFilename)));
end;

function gh_FileExtractName(const AFileName: string): string;
begin
  Result := gh_ExeEncoding(SysUtils.ExtractFileName(gh_SysEncoding(AFilename)));
end;

function gh_FileExtractNameOnly(const AFileName: string): string;
begin
  Result := gh_ExeEncoding(FileUtil.ExtractFileNameOnly(gh_SysEncoding(AFilename)));
end;

function gh_FileExtractExt(const AFileName: string): string;
begin
  Result := gh_ExeEncoding(SysUtils.ExtractFileExt(gh_SysEncoding(AFileName)));
end;

function gh_FileChangeExt(const AFileName, AExt: string): string;
begin
  Result := gh_ExeEncoding(SysUtils.ChangeFileExt(gh_SysEncoding(AFilename), AExt));
end;

end.
