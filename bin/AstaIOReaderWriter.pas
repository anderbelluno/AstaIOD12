{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10283: AstaIOReaderWriter.pas 
{
{   Rev 1.0    4/10/2003 6:31:58 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:38:04 PM  Steve    Version: 1.505
}
unit AstaIOReaderWriter;
{*********************************************************}
{*   Copyright (c) 1997-2002 Asta Technology Group Inc.  *}
{*               All rights reserved.                    *}
{*               www.astatech.com                        *}
{*********************************************************}

{$I AstaIO.inc}

interface
uses Classes, SysUtils;

type
  TAstaIOWriter = class(TWriter)
  private
  protected
  public
    procedure WriteString(const Value: AnsiString); overload;
    procedure WriteString(const Value: string); overload;
  end;

  TAstaIOReader = class(TReader)
  private
  protected
  public
    function ReadString: AnsiString;
  end;

implementation
uses AstaIOResources;
{ TAstaIOWriter }

procedure TAstaIOWriter.WriteString(const Value: AnsiString);
var
  L: Integer;
begin
  L := Length(Value);
  if L <= 255 then
  begin
    WriteValue(vaString);
    Write(L, SizeOf(Byte));
  end else
  begin
    WriteValue(vaLString);
    Write(L, SizeOf(Integer));
  end;
  Write(Pointer(Value)^, L);
end;

procedure TAstaIOWriter.WriteString(const Value: string);
begin
  WriteString(AnsiString(Value));
end;

{ TAstaIOReader }

function TAstaIOReader.ReadString: AnsiString;
var
  L: Integer;
  U: string;
begin
  L := 0;
  case ReadValue of
    vaString:
      begin
        Read(L, SizeOf(Byte));
        SetString(Result, PAnsiChar(nil), L);
        Read(Pointer(Result)^, L);
      end;
    vaLString:
      begin
        Read(L, SizeOf(Integer));
        SetString(Result, PAnsiChar(nil), L);
        Read(Pointer(Result)^, L);
      end;
    vaUTF8String:
      Result := AnsiString(inherited ReadString);
    {$if declared(vaUString)}
    vaUString,
    {$ifend}
    vaWString:
      begin
        U := inherited ReadString;
        Result := AnsiString(U);
      end;
  else
    raise EReadError.Create(SInvalidPropertyValue);
  end;
end;

end.
