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
function ghAppEncoding(const S: string): string; inline;

// app
function ghAppGetParam(AIndex: Integer): string;
function ghAppGetPath: string;
function ghAppGetName: string;
function ghAppGetCurDir: string;
function ghAppSetCurDir(const ANewDir: string): Boolean;
function ghAppGetVersion: string;

// dir
function ghDirExists(const ADir: string): Boolean;
function ghDirForce(const ADir: string): Boolean;
function ghDirAddDelim(const APath: string): string;

// file
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

// util
function ghUrlOpen(const AUrl: string): Boolean;

implementation

function ghSysEncoding(const S: string): string;
begin
  Result := UTF8ToSys(S);
end;

function ghAppEncoding(const S: string): string;
begin
  Result := SysToUTF8(S);
end;

function ghAppGetParam(AIndex: Integer): string;
begin
  Result := ghAppEncoding(ParamStr(AIndex));
end;

function ghAppGetPath: string;
begin
  Result := ghDirAddDelim(ghFileExtractDir(ghAppGetParam(0)));
end;

function ghAppGetName: string;
begin
  Result := ghFileChangeExt(ghFileExtractName(ghAppGetParam(0)), '');
end;

function ghAppGetCurDir: string;
begin
  Result := ghAppEncoding(SysUtils.GetCurrentDir);
end;

function ghAppSetCurDir(const ANewDir: string): Boolean;
begin
  Result := SysUtils.SetCurrentDir(ghSysEncoding(ANewDir));
end;

function ghAppGetVersion: string;
const
  NOVIDATA = '';
var
  dwInfoSize,
  dwVerSize,
  dwWnd: DWORD;
  pffi: PVSFixedFileInfo;
  ptrVerBuf: Pointer;
  strFileName,
  strVersion: string;
begin
  strFileName := ParamStr(0);
  dwWnd := 0;
  dwVerSize := 0;
  dwInfoSize := GetFileVersionInfoSize(PChar(strFileName), dwWnd);

  ZeroMemory(@pffi, SizeOf(pffi));
  if (dwInfoSize = 0) then
    Result := NOVIDATA
  else
  begin
    GetMem(ptrVerBuf, dwInfoSize);
    try
      if GetFileVersionInfo(PChar(strFileName), dwWnd, dwInfoSize, ptrVerBuf) then
      begin
        if VerQueryValue(ptrVerBuf, '\', pffi, dwVerSize) then
          strVersion :=
            Format('%d.%d.%d.%d', [hiWord(pffi^.dwFileVersionMS),
            loWord(pffi^.dwFileVersionMS), hiWord(pffi^.dwFileVersionLS),
            loWord(pffi^.dwFileVersionLS)]);
      end;
    finally
      FreeMem(ptrVerBuf);
    end;
  end;
  Result := strVersion;
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
  ARec.Name := ghAppEncoding(ARec.Name);
end;

function ghFileFindNext(var ARec: TSearchRec): LongInt;
begin
  Result := SysUtils.FindNext(ARec);
  ARec.Name := ghAppEncoding(ARec.Name);
end;

function ghFileExpandName(const AFileName: string): string;
begin
  Result := ghAppEncoding(SysUtils.ExpandFileName(ghSysEncoding(AFileName)));
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
  Result := ghAppEncoding(SysUtils.ExtractFileDir(ghSysEncoding(AFileName)));
end;

function ghFileExtractPath(const AFileName: string): string;
begin
  Result := ghAppEncoding(SysUtils.ExtractFilePath(ghSysEncoding(AFilename)));
end;

function ghFileExtractName(const AFileName: string): string;
begin
  Result := ghAppEncoding(SysUtils.ExtractFileName(ghSysEncoding(AFilename)));
end;

function ghFileExtractNameOnly(const AFileName: string): string;
begin
  Result := ghAppEncoding(FileUtil.ExtractFileNameOnly(ghSysEncoding(AFilename)));
end;

function ghFileExtractExt(const AFileName: string): string;
begin
  Result := ghAppEncoding(SysUtils.ExtractFileExt(ghSysEncoding(AFileName)));
end;

function ghFileChangeExt(const AFileName, AExt: string): string;
begin
  Result := ghAppEncoding(SysUtils.ChangeFileExt(ghSysEncoding(AFilename), AExt));
end;

function ghUrlOpen(const AUrl: string): Boolean;
begin
  Result := LCLIntf.OpenURL(AUrl);
end;

end.
