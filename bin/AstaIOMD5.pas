{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10231: AstaIOMD5.pas 
{
{   Rev 1.0    4/10/2003 6:31:32 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:37:44 PM  Steve    Version: 1.505
}
(**************************************************)
(*                                                *)
(*     Message Digest 5 (MD5)                     *)
(*                                                *)
(*     Copyright (c) 1998-2001                    *)
(*     EldoS, Alexander Ionov                     *)
(*                                                *)
(**************************************************)

unit AstaIOMD5;

interface

uses
  Classes;

type
  TMessageDigest128 = packed record
    A, B, C, D: longword;
  end;

// RSA MD5
// Copyright:    R.L. Rivest, A. Shamir, and L. Adleman (RSA)
// RFC:          1321
// Digest Size : 16B / 128b
function HashMD5(Stream: TStream; Count: integer = 0): TMessageDigest128; overload;
function HashMD5(const S: AnsiString): TMessageDigest128; overload;
function HashMD5(Buffer: pointer; Size: integer): TMessageDigest128; overload;

// Converting functions
function DigestToStr(Digest: TMessageDigest128; LowerCase: boolean = True): string; overload;

implementation

uses
  Math;

const
  LowerAlphabet = '0123456789abcdef';
  UpperAlphabet = '0123456789ABCDEF';

function DigestToStr(Digest: TMessageDigest128; LowerCase: boolean = True): string;
var
  Buffer: array [0..15] of byte absolute Digest;
  I: integer;
  Alphabet: PChar;
begin
  Result := '';
  if LowerCase then
    Alphabet := LowerAlphabet
  else
    Alphabet := UpperAlphabet;
  for I := 0 to 15 do
    Result := Result + (Alphabet + (Buffer[I] div 16))^ +
      (Alphabet + (Buffer[I] mod 16))^;
end;

type
  THackMemoryStream = class(TMemoryStream)
  public
    procedure SetPointer(Ptr: pointer; Size: longint);
  end;

procedure THackMemoryStream.SetPointer(Ptr: pointer; Size: longint);
begin
  inherited SetPointer(Ptr, Size);
end;

function HashMD5(Stream: TStream; Count: integer = 0): TMessageDigest128;
var
  FA, FB, FC, FD: longword;
  FAA, FBB, FCC, FDD: longword;
  Buffer: array [0..4159] of byte;
  Temp: array [0..15] of longword;
  I, Read: integer;
  Count64: int64;
  Done: boolean;
begin
  if Count = 0 then
  begin
    Stream.Position := 0;
    Count := Stream.Size;
  end
  else Count := Min(Stream.Size - Stream.Position, Count);
  FillChar(Result, SizeOf(Result), 0);
  // intializing
  Done := False;
  FA := $67452301; FB := $EFCDAB89; FC := $98BADCFE; FD := $10325476;
  Count64 := Count;
  // processing
  repeat
    Read := Stream.Read(Buffer, Min(4096, Count));
    Dec(Count, Read);
    if Read < 4096 then
    begin
      // the end of stream is reached
      Buffer[Read] := $80;
      Inc(Read);
      while (Read mod 64) <> 56 do
      begin
        Buffer[Read] := 0;
        Inc(Read);
      end;
      Count64 := Count64 * 8;
      Move(Count64, Buffer[Read], 8);
      Inc(Read, 8);
      Done := True;
    end;
    I := 0;
    repeat
      // transforming
      Move(Buffer[I], Temp, SizeOf(Temp));
      FAA := FA; FBB := FB; FCC := FC; FDD := FD;
      // round 1
      Inc(FA, ((FB and FC) or (not FB and FD)) + Temp[0] + $D76AA478);
      FA := ((FA shl 7) or (FA shr 25)) + FB;
      Inc(FD, ((FA and FB) or (not FA and FC)) + Temp[1] + $E8C7B756);
      FD := ((FD shl 12) or (FD shr 20)) + FA;
      Inc(FC, ((FD and FA) or (not FD and FB)) + Temp[2] + $242070DB);
      FC := ((FC shl 17) or (FC shr 15)) + FD;
      Inc(FB, ((FC and FD) or (not FC and FA)) + Temp[3] + $C1BDCEEE);
      FB := ((FB shl 22) or (FB shr 10)) + FC;
      Inc(FA, ((FB and FC) or (not FB and FD)) + Temp[4] + $F57C0FAF);
      FA := ((FA shl 7) or (FA shr 25)) + FB;
      Inc(FD, ((FA and FB) or (not FA and FC)) + Temp[5] + $4787C62A);
      FD := ((FD shl 12) or (FD shr 20)) + FA;
      Inc(FC, ((FD and FA) or (not FD and FB)) + Temp[6] + $A8304613);
      FC := ((FC shl 17) or (FC shr 15)) + FD;
      Inc(FB, ((FC and FD) or (not FC and FA)) + Temp[7] + $FD469501);
      FB := ((FB shl 22) or (FB shr 10)) + FC;
      Inc(FA, ((FB and FC) or (not FB and FD)) + Temp[8] + $698098D8);
      FA := ((FA shl 7) or (FA shr 25)) + FB;
      Inc(FD, ((FA and FB) or (not FA and FC)) + Temp[9] + $8B44F7AF);
      FD := ((FD shl 12) or (FD shr 20)) + FA;
      Inc(FC, ((FD and FA) or (not FD and FB)) + Temp[10] + $FFFF5BB1);
      FC := ((FC shl 17) or (FC shr 15)) + FD;
      Inc(FB, ((FC and FD) or (not FC and FA)) + Temp[11] + $895CD7BE);
      FB := ((FB shl 22) or (FB shr 10)) + FC;
      Inc(FA, ((FB and FC) or (not FB and FD)) + Temp[12] + $6B901122);
      FA := ((FA shl 7) or (FA shr 25)) + FB;
      Inc(FD, ((FA and FB) or (not FA and FC)) + Temp[13] + $FD987193);
      FD := ((FD shl 12) or (FD shr 20)) + FA;
      Inc(FC, ((FD and FA) or (not FD and FB)) + Temp[14] + $A679438E);
      FC := ((FC shl 17) or (FC shr 15)) + FD;
      Inc(FB, ((FC and FD) or (not FC and FA)) + Temp[15] + $49B40821);
      FB := ((FB shl 22) or (FB shr 10)) + FC;
      // round 2
      Inc(FA, ((FB and FD) or (FC and not FD)) + Temp[1] + $F61E2562);
      FA := ((FA shl 5) or (FA shr 27)) + FB;
      Inc(FD, ((FA and FC) or (FB and not FC)) + Temp [6] + $C040B340);
      FD := ((FD shl 9) or (FD shr 23)) + FA;
      Inc(FC, ((FD and FB) or (FA and not FB)) + Temp[11] + $265E5A51);
      FC := ((FC shl 14) or (FC shr 18)) + FD;
      Inc(FB, ((FC and FA) or (FD and not FA)) + Temp[0] + $E9B6C7AA);
      FB := ((FB shl 20) or (FB shr 12)) + FC;
      Inc(FA, ((FB and FD) or (FC and not FD)) + Temp[5] + $D62F105D);
      FA := ((FA shl 5) or (FA shr 27)) + FB;
      Inc(FD, ((FA and FC) or (FB and not FC)) + Temp [10] + $2441453);
      FD := ((FD shl 9) or (FD shr 23)) + FA;
      Inc(FC, ((FD and FB) or (FA and not FB)) + Temp[15] + $D8A1E681);
      FC := ((FC shl 14) or (FC shr 18)) + FD;
      Inc(FB, ((FC and FA) or (FD and not FA)) + Temp[4] + $E7D3FBC8);
      FB := ((FB shl 20) or (FB shr 12)) + FC;
      Inc(FA, ((FB and FD) or (FC and not FD)) + Temp[9] + $21E1CDE6);
      FA := ((FA shl 5) or (FA shr 27)) + FB;
      Inc(FD, ((FA and FC) or (FB and not FC)) + Temp [14] + $C33707D6);
      FD := ((FD shl 9) or (FD shr 23)) + FA;
      Inc(FC, ((FD and FB) or (FA and not FB)) + Temp[3] + $F4D50D87);
      FC := ((FC shl 14) or (FC shr 18)) + FD;
      Inc(FB, ((FC and FA) or (FD and not FA)) + Temp[8] + $455A14ED);
      FB := ((FB shl 20) or (FB shr 12)) + FC;
      Inc(FA, ((FB and FD) or (FC and not FD)) + Temp[13] + $A9E3E905);
      FA := ((FA shl 5) or (FA shr 27)) + FB;
      Inc(FD, ((FA and FC) or (FB and not FC)) + Temp [2] + $FCEFA3F8);
      FD := ((FD shl 9) or (FD shr 23)) + FA;
      Inc(FC, ((FD and FB) or (FA and not FB)) + Temp[7] + $676F02D9);
      FC := ((FC shl 14) or (FC shr 18)) + FD;
      Inc(FB, ((FC and FA) or (FD and not FA)) + Temp[12] + $8D2A4C8A);
      FB := ((FB shl 20) or (FB shr 12)) + FC;
      // round 3
      Inc(FA, (FB xor FC xor FD) + Temp[5] + $FFFA3942);
      FA := FB + ((FA shl 4) or (FA shr 28));
      Inc(FD, (FA xor FB xor FC) + Temp[8] + $8771F681);
      FD := FA + ((FD shl 11) or (FD shr 21));
      Inc(FC, (FD xor FA xor FB) + Temp[11] + $6D9D6122);
      FC := FD + ((FC shl 16) or (FC shr 16));
      Inc(FB, (FC xor FD xor FA) + Temp[14] + $FDE5380C);
      FB := FC + ((FB shl 23) or (FB shr 9));
      Inc(FA, (FB xor FC xor FD) + Temp[1] + $A4BEEA44);
      FA := FB + ((FA shl 4) or (FA shr 28));
      Inc(FD, (FA xor FB xor FC) + Temp[4] + $4BDECFA9);
      FD := FA + ((FD shl 11) or (FD shr 21));
      Inc(FC, (FD xor FA xor FB) + Temp[7] + $F6BB4B60);
      FC := FD + ((FC shl 16) or (FC shr 16));
      Inc(FB, (FC xor FD xor FA) + Temp[10] + $BEBFBC70);
      FB := FC + ((FB shl 23) or (FB shr 9));
      Inc(FA, (FB xor FC xor FD) + Temp[13] + $289B7EC6);
      FA := FB + ((FA shl 4) or (FA shr 28));
      Inc(FD, (FA xor FB xor FC) + Temp[0] + $EAA127FA);
      FD := FA + ((FD shl 11) or (FD shr 21));
      Inc(FC, (FD xor FA xor FB) + Temp[3] + $D4EF3085);
      FC := FD + ((FC shl 16) or (FC shr 16));
      Inc(FB, (FC xor FD xor FA) + Temp[6] + $4881D05);
      FB := FC + ((FB shl 23) or (FB shr 9));
      Inc(FA, (FB xor FC xor FD) + Temp[9] + $D9D4D039);
      FA := FB + ((FA shl 4) or (FA shr 28));
      Inc(FD, (FA xor FB xor FC) + Temp[12] + $E6DB99E5);
      FD := FA + ((FD shl 11) or (FD shr 21));
      Inc(FC, (FD xor FA xor FB) + Temp[15] + $1FA27CF8);
      FC := FD + ((FC shl 16) or (FC shr 16));
      Inc(FB, (FC xor FD xor FA) + Temp[2] + $C4AC5665);
      FB := FC + ((FB shl 23) or (FB shr 9));
      // round 4
      Inc(FA, (FC xor (FB or not FD)) + Temp[0] + $F4292244);
      FA := FB + ((FA shl 6) or (FA shr 26));
      Inc(FD, (FB xor (FA or not FC)) + Temp[7] + $432AFF97);
      FD := FA + ((FD shl 10) or (FD shr 22));
      Inc(FC, (FA xor (FD or not FB)) + Temp[14] + $AB9423A7);
      FC := FD + ((FC shl 15) or (FC shr 17));
      Inc(FB, (FD xor (FC or not FA)) + Temp[5] + $FC93A039);
      FB := FC + ((FB shl 21) or (FB shr 11));
      Inc(FA, (FC xor (FB or not FD)) + Temp[12] + $655B59C3);
      FA := FB + ((FA shl 6) or (FA shr 26));
      Inc(FD, (FB xor (FA or not FC)) + Temp[3] + $8F0CCC92);
      FD := FA + ((FD shl 10) or (FD shr 22));
      Inc(FC, (FA xor (FD or not FB)) + Temp[10] + $FFEFF47D);
      FC := FD + ((FC shl 15) or (FC shr 17));
      Inc(FB, (FD xor (FC or not FA)) + Temp[1] + $85845DD1);
      FB := FC + ((FB shl 21) or (FB shr 11));
      Inc(FA, (FC xor (FB or not FD)) + Temp[8] + $6FA87E4F);
      FA := FB + ((FA shl 6) or (FA shr 26));
      Inc(FD, (FB xor (FA or not FC)) + Temp[15] + $FE2CE6E0);
      FD := FA + ((FD shl 10) or (FD shr 22));
      Inc(FC, (FA xor (FD or not FB)) + Temp[6] + $A3014314);
      FC := FD + ((FC shl 15) or (FC shr 17));
      Inc(FB, (FD xor (FC or not FA)) + Temp[13] + $4E0811A1);
      FB := FC + ((FB shl 21) or (FB shr 11));
      Inc(FA, (FC xor (FB or not FD)) + Temp[4] + $F7537E82);
      FA := FB + ((FA shl 6) or (FA shr 26));
      Inc(FD, (FB xor (FA or not FC)) + Temp[11] + $BD3AF235);
      FD := FA + ((FD shl 10) or (FD shr 22));
      Inc(FC, (FA xor (FD or not FB)) + Temp[2] + $2AD7D2BB);
      FC := FD + ((FC shl 15) or (FC shr 17));
      Inc(FB, (FD xor (FC or not FA)) + Temp[9] + $EB86D391);
      FB := FC + ((FB shl 21) or (FB shr 11));

      Inc(FA, FAA); Inc(FB, FBB); Inc(FC, FCC); Inc(FD, FDD);
      Inc(I, 64);
    until I = Read;
  until Done;
  // finalizing
  Result.A := FA; Result.B := FB; Result.C := FC; Result.D := FD;
end;

function HashMD5(const S: AnsiString): TMessageDigest128; overload;
begin
  Result := HashMD5(PAnsiChar(S), Length(S));
end;

function HashMD5(Buffer: pointer; Size: integer): TMessageDigest128; overload;
var
  Stream: THackMemoryStream;
begin
  Stream := THackMemoryStream.Create;
  try
    Stream.SetPointer(Buffer, Size);
    Stream.Position := 0;
    Result := HashMD5(Stream, Size);
  finally
    Stream.Free;
  end;
end;

end.
