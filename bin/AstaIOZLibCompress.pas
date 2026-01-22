{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10377: AstaIOZLibCompress.pas 
{
{   Rev 1.0    4/10/2003 6:32:44 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:38:36 PM  Steve    Version: 1.505
}
unit AstaIOZLibCompress;
{$I AstaIO.inc}

{*********************************************************}
{* This unit is provided soley for distribution with     *}
{* Asta client/server development software. Other uses   *}
{* are strictly prohibited.                              *}
{*                                                       *}
{* As noted in the source, portions are copyrighted by   *}
{* the following parties:                                *}
{*   Copyright (c) 1997 Borland International            *}
{*   Copyright (c) 2000 David O. Martin                  *}
{*********************************************************}

{---------------------------------------------------------}
{ Version history:                                        }
{   09/19/00  Version 1.0                                 }
{   10/30/00  Version 1.1. Moved CompressBuf and          }
{             DecompressBuf to the interface section.     }
{---------------------------------------------------------}
{$HINTS OFF}
{$WARNINGS OFF}
interface
uses Classes;

const
  Version = '1.1';

procedure ZLibCompressString(var s: AnsiString; compressionLevel: Integer);
{ Compress string s using zlib (thread-safe). This is based on the
  same algorithm used in PKZip. compressionLevel may vary from 1 (best
  speed but worst compression) to 9 (slowest speed but best compression).
  A compressionLevel of 1 will usually give good speed with good
  compression. However, a higher compression level may yield better
  performance when bandwith is at a premium because the total data transfer
  time is given by the sum of compression time + download time + decompression
  time.

  NOTE: Errors may be trapped by placing this routine between a
  try..except block. (This ***should definitely*** be done on an ASTA
  server to prevent an exception dialog from halting the server.) }

procedure ZLibDecompressString(var s: AnsiString);
{ Decompress string s that was compressed using ZLibCompressString.

  NOTE: Errors may be trapped by placing this routine between a
  try..except block. (This ***should definitely*** be done on an ASTA
  server to prevent an exception dialog from halting the server.) }

{------------------------------------------------------------------}
{ Driver routines for the above (which can be useful on there own) }
{------------------------------------------------------------------}

procedure CompressBuf(
  const inBuf: Pointer;     { input: pointer to source data }
        inBytes: Integer;   { input: # of bytes in inBuf }
        level: Integer;     { input: compression level }
    var outBuf: Pointer;    { output: pointer to newly allocated buffer with compressed data }
    var outBytes: Integer); { output: # of bytes in outBuf }
{ This routine compresses data. Level may vary from 0 to 9 (3 to 6 may
  be a reasonable trade-off between speed and compression).

  Compression levels:
    0 = no compression                 (Z_NO_COMPRESSION constant)
    1 = best speed                     (Z_BEST_SPEED constant)
    9 = best compression, but slowest  (Z_BEST_COMPRESSION constant) }

procedure DecompressBuf(
  const inBuf: Pointer;     { input: pointer to compressed data }
        inBytes: Integer;   { input: # of bytes in inBuf }
    var outBuf: Pointer;    { output: pointer to newly allocated buffer with decompressed data }
    var outBytes: Integer); { output: # of bytes in outBuf }
{ This routine decompresses data that was compressed with CompressBuf. Note
  that this works for all compression levels. }

procedure ZLibCompressMemStream(s: TMemoryStream; compressionLevel: integer);
{ Compresses a memory stream. compressionLevel is as defined in
  ZLibCompressString. }


function CompressStream(inpStream: TmemoryStream;compressionlevel:integer):TmemoryStream;
function ExpandStream(inpStream: TmemoryStream):tmemorystream;

procedure ZLibDecompressMemStream(s: TMemoryStream);
{ Decompresses a memory stream. }

var
 AstaZLibCompressionSetting:Integer;

implementation

uses
{$ifdef mswindows}
Windows,
{$endif}
{$ifdef Linux}
libc,
{$endif}
SysUtils, AstaIOZLib;

{----------------------------------------------------------}
{ The CompressBuf and DecompressBuf routines that were     }
{ shipped with Delphi 3-5 are based on zlib 1.0.4 and are  }
{ not compatible with zlib 1.1.2. The following are        }
{ rewritten versions of CompressBuf and DecompressBuf that }
{ take into account the differences between zlib 1.0.4 and }
{ 1.1.2.                                                   }
{                                                          }
{ Copyright (c) 1997 Borland International                 }
{ Copyright (c) 2000 David O. Martin                       }
{----------------------------------------------------------}

procedure CompressBuf(
  const inBuf: Pointer;     { input: pointer to source data }
        inBytes: Integer;   { input: # of bytes in inBuf }
        level: Integer;     { input: compression level }
    var outBuf: Pointer;    { output: pointer to newly allocated buffer with compressed data }
    var outBytes: Integer); { output: # of bytes in outBuf }
{ This routine compresses data. Level may vary from 0 to 9 (3 to 6 may
  be a reasonable trade-off between speed and compression).

  Compression levels:
    0 = no compression                 (Z_NO_COMPRESSION constant)
    1 = best speed                     (Z_BEST_SPEED constant)
    9 = best compression, but slowest  (Z_BEST_COMPRESSION constant) }

  function Check(code: Integer): Integer;
  begin
    Result := code;
    if (code < 0) then
      raise Exception.Create('Compression error: '+z_errmsg[2-code]);
  end;

var
  strm: z_stream;
begin
  FillChar(strm, sizeof(strm), 0);
  outBytes := ((inBytes + (inBytes div 10) + 12) + 255) and not 255;
  GetMem(outBuf, outBytes);
  try
    strm.next_in := inBuf;
    strm.avail_in := inBytes;
    strm.next_out := outBuf;
    strm.avail_out := outBytes;
    Check(deflateInit(strm, level));
    try
      while Check(deflate(strm, Z_FINISH)) <> Z_STREAM_END do begin
        Inc(outBytes, 256);
        ReallocMem(outBuf, outBytes);
        strm.next_out := pBytef(Integer(outBuf) + strm.total_out);
        strm.avail_out := 256;
      end;
    finally
      Check(deflateEnd(strm));
    end;
    ReallocMem(outBuf, strm.total_out);
    outBytes := strm.total_out;
  except
    FreeMem(outBuf);
    raise;
  end;
end;

procedure DecompressBuf(
  const inBuf: Pointer;     { input: pointer to compressed data }
        inBytes: Integer;   { input: # of bytes in inBuf }
    var outBuf: Pointer;    { output: pointer to newly allocated buffer with decompressed data }
    var outBytes: Integer); { output: # of bytes in outBuf }
{ This routine decompresses data that was compressed with CompressBuf. }

  function Check(code: Integer): Integer;
  begin
    Result := code;
    if (code < 0) then
      raise Exception.Create('Decompression error: '+z_errmsg[2-code]);
  end;

var
  strm: z_stream;
  bufInc: Integer;
begin
  FillChar(strm, sizeof(strm), 0);
  bufInc := (inBytes + 255) and not 255;
  outBytes := bufInc;
  GetMem(outBuf, outBytes);
  try
    strm.next_in := inBuf;
    strm.avail_in := inBytes;
    strm.next_out := outBuf;
    strm.avail_out := outBytes;
    Check(inflateInit(strm));
    try
      while Check(inflate(strm, Z_NO_FLUSH)) <> Z_STREAM_END do begin
        Inc(outBytes, bufInc);
        ReallocMem(outBuf, outBytes);
        strm.next_out := pBytef(Integer(outBuf) + strm.total_out);
        strm.avail_out := bufInc;
      end;
    finally
      Check(inflateEnd(strm));
    end;
    ReallocMem(outBuf, strm.total_out);
    outBytes := strm.total_out;
  except
    FreeMem(outBuf);
    raise;
  end;
end;

{--------------------------------------------------------------}
{ The following routines were suggested by Stefan Wolf as      }
{ posted on the ASTA users' group hosted by eGroups.com.       }
{ ZLibCompressString() has been modified to have an additional }
{ parameter that allows the user to adjust the compression     }
{ level. Note the use of the lower level CopyMemory() which    }
{ is faster than the nearly equivalent Move().                 }
{--------------------------------------------------------------}

procedure ZLibCompressString{(See interface)};
var
  outBuf: Pointer;
  outBytes: Integer;
begin
  CompressBuf(@s[1], Length(s), compressionLevel, outBuf, outBytes);
  SetLength(s, outBytes);
  Move(outBuf^, s[1], outBytes);
  FreeMem(outBuf);
end;

procedure ZLibDecompressString{(See interface)};
var
  outBuf: Pointer;
  outBytes: Integer;
begin
  if Length(s) > 0 then begin
    DecompressBuf(Pointer(s), Length(s), outBuf, outBytes);
    SetLength(s, outBytes);
    Move(outBuf^, s[1], outBytes);
    FreeMem(outBuf);
  end;
end;
function CompressStream(inpStream: TmemoryStream;compressionlevel:integer):TmemoryStream;
  var InpBuf,OutBuf: Pointer;
  var InpBytes,OutBytes: integer;
  begin
    result:=tmemorystream.create;
    InpBuf := nil;
    OutBuf := nil;
    try
      GetMem(InpBuf,inpStream.size);
      inpStream.Position := 0;
      InpBytes := inpStream.Read(InpBuf^,inpStream.size);
      CompressBuf(InpBuf,InpBytes,compressionlevel,OutBuf,OutBytes);
      result.Write(OutBuf^,OutBytes);
    finally
      if InpBuf <> nil then FreeMem(InpBuf);
      if OutBuf <> nil then FreeMem(OutBuf);
    end;
  end;

  function ExpandStream(inpStream: TmemoryStream):Tmemorystream;
  var InpBuf,OutBuf: Pointer;
  var OutBytes,sz: integer;
  begin
    result:= tmemorystream.create;
    InpBuf := nil;
    OutBuf := nil;
    sz := inpStream.size-inpStream.Position;
    if sz > 0 then try
      GetMem(InpBuf,sz);
      inpStream.Read(InpBuf^,sz);
      DecompressBuf(InpBuf,sz,OutBuf,OutBytes);
      result.Write(OutBuf^,OutBytes);
    finally
      if InpBuf <> nil then FreeMem(InpBuf);
      if OutBuf <> nil then FreeMem(OutBuf);
    end;
    result.Position := 0;
  end;




procedure ZLibCompressMemStream{(See interface)};
var
  inBuf, outBuf: Pointer;
  inBytes,outBytes: Integer;
begin
  inBuf:=nil;
  outBuf:=nil;
  try
    inBytes:=s.Size;
    GetMem(inBuf, inBytes);
    s.Position:=0;
    s.ReadBuffer(inBuf^, inBytes);
    s.Clear;
    CompressBuf(inBuf, inBytes, compressionLevel, outBuf, outBytes);
    s.WriteBuffer(outBuf^, outBytes);
  finally
    FreeMem(inBuf);
    FreeMem(outBuf);
  end;
end;

procedure ZLibDecompressMemStream{(See interface)};
var
  inBuf, outBuf: Pointer;
  inBytes, outBytes: Integer;
begin
  inBuf:=nil;
  outBuf:=nil;
  try
    inBytes:=s.Size;
    GetMem(inBuf, inBytes);
    s.Position:=0;
    s.ReadBuffer(inBuf^, inBytes);
    s.Clear;
    DecompressBuf(inBuf, inBytes, outBuf, outBytes);
    s.WriteBuffer(outBuf^, outBytes);
  finally
    FreeMem(inBuf);
    FreeMem(outBuf);
  end;
end;

{$HINTS ON}
{$WARNINGS ON}
initialization
 AstaZLibCompressionSetting:=1;

end.
