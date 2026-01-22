{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10061: AstaIOBits.pas 
{
{   Rev 1.0    4/10/2003 6:30:06 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:36:40 PM  Steve    Version: 1.505
}
unit AstaIOBits;
{*********************************************************}
{*   Copyright (c) 1997-2002 Asta Technology Group Inc.  *}
{*               All rights reserved.                    *}
{*               www.astatech.com                        *}
{*********************************************************}

interface

function TestBit(l, bitnum: integer): Boolean;
procedure SetBit(var target: Integer; bitnum: integer);
procedure ClearBit(var target: Integer; bitnum: integer);
function BinWrite(target: Integer): string;

implementation

function TestBit(l, bitnum: integer): Boolean;
begin
  result := odd(l shr bitnum);
end;

procedure SetBit(var target: Integer; bitnum: integer);
var
  mask: Integer;
begin
  mask := 1 shl bitnum;
  target := target or mask;
end;

procedure ClearBit(var target: Integer; bitnum: integer);
var
  mask: integer;
begin
  mask := not (1 shl bitnum);
  target := target and mask;
end;


function BinWrite(target: Integer): string;
var
  bt: integer;
  i, j, k: integer;
begin
  bt := target;
  result := '';
  k := -1;
  for j := 1 to 8 do
  begin
    for i := 0 to 3 do
    begin
      if TestBit(bt, i + j + k) then
        result := '1' + result
      else
        result := '0' + result;
    end;
    k := k + 3;
    result := ' ' + result;
  end;
end;

end.
