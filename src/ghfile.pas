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
  {$IFDEF WINDOWS}
  Windows,
  {$ENDIF}
  // fpc
  Classes, SysUtils,
  // laz
  FileUtil, LazUTF8, LCLIntf;

// encoding
function ghOSEncoding(const S: string): string; inline;
function ghAppEncoding(const S: string): string; inline;

// app
function ghAppGetParam(AIndex: Integer): string;
function ghAppGetPath: string;
function ghAppGetName: string;
function ghAppGetCurDir: string;
function ghAppSetCurDir(const ANewDir: string): Boolean;
function ghAppGetVersionStr: string;

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
function ghFileGetVersionStr(const AFileName: string): string;

// util
function ghUrlOpen(const AUrl: string): Boolean;

implementation

function ghOSEncoding(const S: string): string;
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
  Result := ghAppEncoding(GetCurrentDir);
end;

function ghAppSetCurDir(const ANewDir: string): Boolean;
begin
  Result := SetCurrentDir(ghOSEncoding(ANewDir));
end;

function ghAppGetVersionStr: string;
begin
  Result := ghFileGetVersionStr(ParamStr(0))
end;

function ghDirExists(const ADir: string): Boolean;
begin
  Result := DirectoryExists(ghOSEncoding(ADir));
end;

function ghDirForce(const ADir: string): Boolean;
begin
  Result := ForceDirectories(ExtractFilePath(ghOSEncoding(ADir)));
end;

function ghDirAddDelim(const APath: string): string;
begin
  Result := IncludeTrailingPathDelimiter(APath);
end;

function ghFileFindFirst(const APath: string; AAttr: LongInt; out ARec: TSearchRec): LongInt;
begin
  Result := FindFirst(ghOSEncoding(APath), AAttr, ARec);
  ARec.Name := ghAppEncoding(ARec.Name);
end;

function ghFileFindNext(var ARec: TSearchRec): LongInt;
begin
  Result := FindNext(ARec);
  ARec.Name := ghAppEncoding(ARec.Name);
end;

function ghFileExpandName(const AFileName: string): string;
begin
  Result := ghAppEncoding(ExpandFileName(ghOSEncoding(AFileName)));
end;

function ghFileExists(const AFileName: string): Boolean;
begin
  Result := FileExists(ghOSEncoding(AFileName));
end;

function ghFileSize(const AFileName: string): Int64;
begin
  Result := FileUtil.FileSize(AFileName);
end;

function ghFileRename(const AOldName, ANewName: string): Boolean;
begin
  Result := RenameFile(ghOSEncoding(AOldName), ghOSEncoding(ANewName));
end;

function ghFileDelete(const AFileName: string): Boolean;
begin
  Result := DeleteFile(ghOSEncoding(AFileName));
end;

function ghFileExtractDir(const AFileName: string): string;
begin
  Result := ghAppEncoding(ExtractFileDir(ghOSEncoding(AFileName)));
end;

function ghFileExtractPath(const AFileName: string): string;
begin
  Result := ghAppEncoding(ExtractFilePath(ghOSEncoding(AFilename)));
end;

function ghFileExtractName(const AFileName: string): string;
begin
  Result := ghAppEncoding(ExtractFileName(ghOSEncoding(AFilename)));
end;

function ghFileExtractNameOnly(const AFileName: string): string;
begin
  Result := ghAppEncoding(FileUtil.ExtractFileNameOnly(ghOSEncoding(AFilename)));
end;

function ghFileExtractExt(const AFileName: string): string;
begin
  Result := ghAppEncoding(ExtractFileExt(ghOSEncoding(AFileName)));
end;

function ghFileChangeExt(const AFileName, AExt: string): string;
begin
  Result := ghAppEncoding(ChangeFileExt(ghOSEncoding(AFilename), AExt));
end;

function ghFileGetVersionStr(const AFileName: string): string;
{$IFDEF WINDOWS}
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
{$ELSE}
  Result := '';
{$ENDIF}
end;

function ghUrlOpen(const AUrl: string): Boolean;
begin
  Result := LCLIntf.OpenURL(AUrl);
end;

end.
