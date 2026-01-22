unit ASTAIORemConMessages;

interface

uses SysUtils, Classes, Windows, Graphics, Forms;


const
  // Messages
  // All numbers are 4-byte integers
  // Strings include a null terminating zero

  MSG_LOGON = 1;     // Client logs on to the server
  // Data = password

  MSG_REFRESH = 2;     // Client wants the entire screen
  // Data = Compressed BMP

  MSG_SCREEN_UPDATE = 3;     // Server sends client a screen update
  // Data = Compressed BMP (to XOR)

  MSG_CLICK = 4;     // Mouse click (single or double, left or right)
  // Data = X, Y, Single (1) / Double (2), Left (1) / Right (2)

  MSG_DRAG = 5;     // Mouse drag
  // Data = Left (1) / Right (2), Num Pts, X/Y/time sets

  MSG_KEYS = 6;     // Send keystrokes
  // Data = KeyCode(s)

  MSG_DIRECTORY = 7;     // Request a directory
  // Data = directory (client -> server)
  // Data = File StringList (dirs end in "\"), Size StringList, Date/Time StringList (server -> client)

  MSG_FILE = 8;     // Request a file
  // Data = Filename (client -> server)
  // Data = File Contents

  MSG_SEVER_DELAY = 9;
  // Set the amount of time the server should wait before sending back the screen image
  // Data = Delay in milliseconds

  MSG_FOCUS_SERVER = 10;    // To restore & focus the Server Window

  MSG_VIEW_MODE = 11;    // Set the view mode for the graphics
  // Data = View Mode (TViewMode as integer)

  MSG_STAT_MSG = 12;    // Generic Status Msg
  // Data = Status Message (string)

  MSG_COMP_MODE = 13;    // Screen Compression Mode
  // Data = CompMode (TCompressionLevel as integer)

  MSG_PRIORITY_MODE = 14;    // Server Thread Priority
  // Data = Priority (integer)

  MSG_PROCESS_LIST = 15;    // List of running processes
  // Data = Process StringList

  MSG_CLOSE_WIN = 16;    // Close one of the running processes (gracefully)
  // Data = Process Name (actually Window Name)

  MSG_KILL_WIN = 17;    // Kill one of the running processes (NOT graceful)
  // Data = Process Name (actually Window Name)

  MSG_DRIVE_LIST = 18;    // Get a list of all the Logical Drives
  // Data = Drives StringList

  MSG_REMOTE_LAUNCH = 19;    // Launch (ShellExecute) a remote file (of any type)
  // Data = Filename

type
  TSendIntf = class(TObject)
  public
    procedure SendMessage(const S: string); virtual; abstract;
  end;

  TViewMode = (vmColor4, vmGray4, vmGray8, vmColor24, vmDefault);

procedure GetScreen(var bmp: TBitmap; ViewMode: TViewMode);
procedure CompressBitmap(bmp: TGraphic; var Data: string; comp: integer);
procedure UnCompressBitmap(const Data: string; bmp: TBitmap);
function IntToByteStr(Value: integer): string;
function IsValidMessage(const Msg: string): boolean;
function CurTime: string;
function MsgLen(const Msg: string): integer;
function TrimFirstMsg(var Msg: string): string;

  // Debugging
procedure SaveString(const s, FileName: string);


implementation
uses AstaIOZLibCompress;

type
  TRGBCol = record
    Blu, Grn, Red: byte;
  end;
  TRGBArray = array[0..0] of TRGBCol;
  PRGBArray = ^TRGBArray;

  TByteArray = array[0..0] of byte;
  PByteArray = ^TByteArray;


procedure SaveString(const s, FileName: string);
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(FileName, fmCreate);
  fs.Write(s[1], Length(s));
  fs.Free;
end;

function GammaConv(Value: double; Gamma: double): double;
begin
  if Value <> 0 then Result := Exp(Ln(Value) / Gamma)
  else 
    Result := 0;
end;

function CreateGrayPalette(Num: integer; Gamma: double): HPalette;
var
  lPal: PLogPalette;
  i: integer;
begin
  // Add the Grayscale palette
  lPal := AllocMem(sizeof(TLogPalette) + Num * sizeof(TPaletteEntry));
  lPal.palVersion := $300;
  lPal.palNumEntries := Num;
  for i := 0 to Num - 1 do with lPal.palPalEntry[i] do 
    begin
      peRed := Round(255 * GammaConv(i / (Num - 1), Gamma));
      peGreen := Round(255 * GammaConv(i / (Num - 1), Gamma));
      peBlue := Round(255 * GammaConv(i / (Num - 1), Gamma));
      peFlags := 0;
    end;
  Result := CreatePalette(lPal^);
  FreeMem(lPal);
  Win32Check(longbool(Result));
end;

procedure ConvertToGray_256(bmp: TBitmap);
var
  gm: TBitmap;  // Destination grayscale bitmap
  x, y: integer;
  p1: PRGBArray;
  p2: PByteArray;
begin
  bmp.PixelFormat := pf24bit;

  // Convert to Grayscale
  gm := TBitmap.Create;
  gm.PixelFormat := pf8bit;
  gm.Width := bmp.Width;
  gm.Height := bmp.Height;

  gm.Palette := CreateGrayPalette(256, 1.4);

  for y := 0 to bmp.Height - 1 do 
  begin
    p1 := bmp.ScanLine[y];
    p2 := gm.ScanLine[y];
    for x := 0 to bmp.Width - 1 do with p1^[x] do 
      begin
        p2^[x] := (Red * 3 + Grn * 4 + Blu) div 8;
      end;
  end;

  bmp.Assign(gm);
  gm.Free;
end;

procedure ConvertToGray_16(bmp: TBitmap);
var
  gm: TBitmap;  // Destination grayscale bitmap
  x, y: integer;
  p1: PRGBArray;
  p2: PByteArray;
  c: integer;
begin
  bmp.PixelFormat := pf24bit;

  // Convert to Grayscale
  gm := TBitmap.Create;
  gm.PixelFormat := pf4bit;
  gm.Width := bmp.Width;
  gm.Height := bmp.Height;

  gm.Palette := CreateGrayPalette(16, 1.4);

  for y := 0 to bmp.Height - 1 do 
  begin
    p1 := bmp.ScanLine[y];
    p2 := gm.ScanLine[y];
    for x := 0 to bmp.Width - 1 do with p1^[x] do 
      begin
        c := (Red * 3 + Grn * 4 + Blu) div (8 * 16);
        if (x and 1) = 1 then 
        begin
          p2^[x div 2] := p2^[x div 2] and (not 15) or c;
        end 
        else 
        begin
          p2^[x div 2] := p2^[x div 2] and (15) or (c shl 4);
        end;
      end;
  end;

  bmp.Assign(gm);
  gm.Free;
end;


procedure GetScreen(var bmp: TBitmap; ViewMode: TViewMode);
var
  dc: integer;
  c: TCanvas;
  R: TRect;
begin
  bmp := TBitmap.Create;

  dc := GetWindowDC(0);
  try
    c := TCanvas.Create;
    c.Handle := dc;
    R := Rect(0, 0, Screen.Width, Screen.Height);
    bmp.Width := R.Right;
    bmp.Height := R.Bottom;
    bmp.Canvas.CopyRect(R, c, R);
    c.Handle := 0;
    c.Free;
  finally
    ReleaseDC(0, dc);
  end;

  case ViewMode of
    vmColor4: bmp.PixelFormat := pf4bit;
    vmGray4: ConvertToGray_16(bmp);
    vmGray8: ConvertToGray_256(bmp);
    vmColor24: bmp.PixelFormat := pf24bit;
    vmDefault: bmp.HandleType := bmDIB;
  end;
end;

procedure CompressBitmap(bmp: TGraphic; var Data: string; comp: integer);
var

  ms: TMemoryStream;
begin
  try
    ms := TMemoryStream.Create;
    bmp.SaveToStream(ms);
    ms := AstaIOZLibCompress.CompressStream(ms, comp);
    SetLength(Data, ms.Size);
    Move(ms.Memory^, Data[1], ms.Size);
    ms.Free;
  except
    on Exception do;
  end;
end;

procedure UnCompressBitmap(const Data: string; bmp: TBitmap);
var
  ms: TMemoryStream;
  buf: pointer;
  size: integer;
begin
  try
    AstaIOZLibCompress.DecompressBuf(@Data[1], Length(Data), buf, size);
  except
    on E: Exception do
    begin
      E.Message := Format('Error Decompressing Buffer (Len = %d):'#13#10'%s',
        [Length(Data), e.Message]);
      raise;
    end;
  end;

  ms := TMemoryStream.Create;
  ms.Write(buf^, size);
  FreeMem(buf);
  ms.Position := 0;
  Assert(bmp <> nil);
  bmp.LoadFromStream(ms);

  ms.Free;
end;

function IntToByteStr(Value: integer): string;
begin
  SetLength(Result, 4);
  Move(Value, Result[1], sizeof(integer));
end;

function IsValidMessage(const Msg: string): boolean;
var
  len: integer;
begin
  Result := False;

  len := Length(Msg);
  if len < 8 then exit;

  if MsgLen(Msg) > len then exit;

  Result := True;
end;

function MsgLen(const Msg: string): integer;
var
  len, mlen: integer;
begin
  len := Length(Msg);
  Assert(len >= 8);

  Move(Msg[5], mlen, sizeof(integer));

  Result := mlen + 8;
end;

function TrimFirstMsg(var Msg: string): string;
begin
  Result := Copy(Msg, 1, MsgLen(Msg));
  Msg := Copy(Msg, MsgLen(Msg) + 1, Length(Msg));
end;

function CurTime: string;
begin
  Result := FormatDateTime('mmm d, yyyy  hh:nn:ss ampm', Now);
end;


end.