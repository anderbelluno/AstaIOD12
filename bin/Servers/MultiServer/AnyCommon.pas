unit AnyCommon;

interface

{$I Compiler.inc}

uses Classes,
    {$IFDEF D6ANDUP}
      Variants,
    {$ELSE}
      Forms, FileCtrl,
    {$ENDIF}
     SysUtils;


type
  TDMServerLogEvent = procedure (Sender: TObject; AMessage: string) of object;

function CreateGuidString: string;

function DelLastSlash(const AText: string): string;
function RemoveBackSlash(const DirName: string): string;
function ReplaceStr(const S, Srch, Replace: string): string;

procedure ForceDirectories(Dir: string);

procedure LogToFile(AMessage: string; const AFilePrefix: string = '');

implementation

function CreateGuidString: string;
begin
  Result := CreateGuidString;
  // Remove { and }
  if (Result <> '') and (Result[1] = '{') then Delete(Result, 1, 1);
  if (Result <> '') and (Result[Length(Result)] = '}') then Delete(Result, Length(Result), 1);
end;

function DelLastSlash(const AText: string): string;
begin
  Result := AText;

  case Result[Length(Result)] of
    '/', '\': Delete(Result, Length(Result), 1);
  end;
end;

function RemoveBackSlash(const DirName: string): string;
begin
  Result := DirName;
{$IFDEF LINUX}
  if (Length(Result) > 1) and (AnsiLastChar(Result)^ = '/') then
    Delete(Result, Length(Result), 1);
{$ELSE}
  if (Length(Result) > 1) and (AnsiLastChar(Result)^ = '\') then
  begin
    if not ((Length(Result) = 3) and (UpCase(Result[1]) in ['A'..'Z']) and
      (Result[2] = ':')) then
      Delete(Result, Length(Result), 1);
  end;
{$ENDIF}
end;

function ReplaceStr(const S, Srch, Replace: string): string;
var
  I: Integer;
  Source: string;
begin
  Source := S;
  Result := '';
  repeat
    I := Pos(Srch, Source);
    if I > 0 then begin
      Result := Result + Copy(Source, 1, I - 1) + Replace;
      Source := Copy(Source, I + Length(Srch), MaxInt);
    end
    else Result := Result + Source;
  until I <= 0;
end;

procedure ForceDirectories(Dir: string);
begin
  if Length(Dir) = 0 then Exit;
{$IFDEF LINUX}
  if (AnsiLastChar(Dir) <> nil) and (AnsiLastChar(Dir)^ = '/') then Delete(Dir, Length(Dir), 1);
{$ELSE}
  if (AnsiLastChar(Dir) <> nil) and (AnsiLastChar(Dir)^ = '\') then Delete(Dir, Length(Dir), 1);
{$ENDIF}
  if (Length(Dir) < 3) or DirectoryExists(Dir) or (ExtractFilePath(Dir) = Dir) then Exit;

  ForceDirectories(ExtractFilePath(Dir));
  CreateDir(Dir);
end;

procedure LogToFile(AMessage: string; const AFilePrefix: string);
var
  F: TFileStream;
  FileName, FileDir: string;
begin
  try
{$IFDEF LINUX}
    FileDir := RemoveBackSlash(ExtractFilePath(ParamStr(0))) + '/Log/';
{$ELSE}
    FileDir := RemoveBackSlash(ExtractFilePath(ParamStr(0))) + '\Log\';
{$ENDIF}
    try
      ForceDirectories(FileDir);
    except
{$IFDEF LINUX}
      FileDir := RemoveBackSlash(ExtractFilePath(ParamStr(0))) + '/';
{$ELSE}
      FileDir := RemoveBackSlash(ExtractFilePath(ParamStr(0))) + '\';
{$ENDIF}
    end;

    FileName := ExtractFileName(ParamStr(0));
    if Pos('.', FileName) > 0 then Delete(FileName, Pos('.', FileName), Length(FileName)); // Remove Extension
    if AFilePrefix = '' then
      FileName := FileDir + FormatDateTime('yyyymmdd', Now) + '_' + FileName + '.Log'
    else
      FileName := FileDir + FormatDateTime('yyyymmdd', Now) + '_' + FileName + '_' + AFilePrefix + '.Log';

    if not FileExists(FileName) then TFileStream.Create(Filename, fmCreate).Destroy;
    F := TFileStream.Create(FileName, fmOpenWrite + fmShareDenyNone);
    try
      F.Seek(0, soFromEnd);
      AMessage := Format('[%s]: %s'#13#10, [UpperCase(FormatDateTime('dd-mmm-yyyy hh:nn:ss', Now)), AMessage]);
      F.Write(AMessage[1], Length(AMessage));
    finally
      F.Destroy;
    end;
  except
  end;
end;

end.
