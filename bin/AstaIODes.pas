{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10135: AstaIODes.pas 
{
{   Rev 1.0    4/10/2003 6:30:46 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:37:08 PM  Steve    Version: 1.505
}
(**************************************************)
(*                                                *)
(*     Data Encryption Standard (DES)             *)
(*                                                *)
(*     Copyright (c) 1998-2001                    *)
(*     EldoS, Alexander Ionov                     *)
(*                                                *)
(**************************************************)

{ $B-,C-,F-,G+,H+,I-,J+,L-,M-,O+,Q-,R-,T-,U-,W-,X+,Y-,Z1}

unit AstaIODes;

interface

uses
  Classes, SysUtils;

type
  EDESError = class(Exception);

  TDESBuffer = array [0..7] of byte;
  TDESKey = array [0..7] of byte;
  TDESExpandedKey = array [0..15, 0..47] of byte;
  PDESBuffer = ^TDESBuffer;
  PDESKey = ^TDESKey;
  PDESExpandedKey = ^TDESExpandedKey;

  DesKeyType = (desBothKey);
// Key expansion

procedure ExpandDESKey(const Key: TDESKey; var ExpandedKey: TDESExpandedKey);

// Block encryption

procedure EncryptDES(const InBuf: TDESBuffer; const ExpandedKey: TDESExpandedKey;
  var OutBuf: TDESBuffer);

// Block decryption

procedure DecryptDES(const InBuf: TDESBuffer; const ExpandedKey: TDESExpandedKey;
  var OutBuf: TDESBuffer);

// Stream encryption routines (ECB mode)

procedure EncryptDESStreamECB(Source: TStream; Count: cardinal;
  const Key: TDESKey; Dest: TStream); overload;
procedure EncryptDESStreamECB(Source: TStream; Count: cardinal;
  const ExpandedKey: TDESExpandedKey; Dest: TStream); overload;

// Stream encryption routines (CBC mode)

procedure EncryptDESStreamCBC(Source: TStream; Count: cardinal;
  const Key: TDESKey; const InitVector: TDESBuffer; Dest: TStream); overload;
procedure EncryptDESStreamCBC(Source: TStream; Count: cardinal;
  const ExpandedKey: TDESExpandedKey; const InitVector: TDESBuffer;
  Dest: TStream); overload;

// Stream decryption routines (ECB mode)

procedure DecryptDESStreamECB(Source: TStream; Count: cardinal;
  const Key: TDESKey; Dest: TStream); overload;
procedure DecryptDESStreamECB(Source: TStream; Count: cardinal;
  const ExpandedKey: TDESExpandedKey; Dest: TStream); overload;

// Stream decryption routines (CBC mode)

procedure DecryptDESStreamCBC(Source: TStream; Count: cardinal;
  const Key: TDESKey; const InitVector: TDESBuffer; Dest: TStream); overload;
procedure DecryptDESStreamCBC(Source: TStream; Count: cardinal;
  const ExpandedKey: TDESExpandedKey;  const InitVector: TDESBuffer;
  Dest: TStream); overload;
Procedure _SetDESStringKey(Const AKey:AnsiString;var desKey:PDESExpandedKey);

resourcestring
  SInvalidInBufSize = 'Invalid buffer size for decryption';
  SReadError = 'Stream read error';
  SWriteError = 'Stream write error';

implementation

uses
  Math;

type
  PLongWord = ^LongWord;

const
  PC1: array [1..56] of byte = (
    57, 49, 41, 33, 25, 17,  9,
     1, 58, 50, 42, 34, 26, 18,
    10,  2, 59, 51, 43, 35, 27,
    19, 11,  3, 60, 52, 44, 36,
    63, 55, 47, 39, 31, 23, 15,
     7, 62, 54, 46, 38, 30, 22,
    14,  6, 61, 53, 45, 37, 29,
    21, 13,  5, 28, 20, 12,  4
  );
  PC2: array [1..48] of byte = (
    14, 17, 11, 24,  1,  5,
     3, 28, 15,  6, 21, 10,
    23, 19, 12,  4, 26,  8,
    16,  7, 27, 20, 13,  2,
    41, 52, 31, 37, 47, 55,
    30, 40, 51, 45, 33, 48,
    44, 49, 39, 56, 34, 53,
    46, 42, 50, 36, 29, 32
  );
  Shifts: array [1..16] of byte =
    (1, 1, 2, 2, 2, 2, 2, 2, 1, 2, 2, 2, 2, 2, 2, 1);

procedure ExpandDESKey(const Key: TDESKey; var ExpandedKey: TDESExpandedKey);
var
  C, D: array [1..28] of byte;
  I, J: integer;
  T: word;
begin
  // permuted choise 1
  FillChar(C, SizeOf(C), 0);
  FillChar(D, SizeOf(D), 0);
  for I := 1 to 28 do
  begin
    if (Key[(PC1[I] - 1) shr 3] and (128 shr ((PC1[I] - 1) and $07))) > 0 then
      C[I] := 1;
    if (Key[(PC1[I + 28] - 1) shr 3] and (128 shr ((PC1[I + 28] - 1) and $07))) > 0 then
      D[I] := 1;
  end;
  // producing subkeys
  for I := 1 to 16 do
  begin
    // shifting C
    T := 0;
    Move(C[1], T, Shifts[I]);
    Move(C[Shifts[I] + 1], C[1], 28 - Shifts[I]);
    Move(T, C[29 - Shifts[I]], Shifts[I]);
    // shifting D
    T := 0;
    Move(D[1], T, Shifts[I]);
    Move(D[Shifts[I] + 1], D[1], 28 - Shifts[I]);
    Move(T, D[29 - Shifts[I]], Shifts[I]);
    // permuted choise 2
    for J := 1 to 48 do
      if PC2[J] <= 28 then
        ExpandedKey[I - 1, J - 1] := C[PC2[J]]
      else
        ExpandedKey[I - 1, J - 1] := D[PC2[J] - 28];
  end;
end;

const
  IP: array [1..64] of byte = (
    58, 50, 42, 34, 26, 18, 10,  2,
    60, 52, 44, 36, 28, 20, 12,  4,
    62, 54, 46, 38, 30, 22, 14,  6,
    64, 56, 48, 40, 32, 24, 16,  8,
    57, 49, 41, 33, 25, 17,  9,  1,
    59, 51, 43, 35, 27, 19, 11,  3,
    61, 53, 45, 37, 29, 21, 13,  5,
    63, 55, 47, 39, 31, 23, 15,  7
  );

  IPR: array [1..64] of byte = (
    40,  8, 48, 16, 56, 24, 64, 32,
    39,  7, 47, 15, 55, 23, 63, 31,
    38,  6, 46, 14, 54, 22, 62, 30,
    37,  5, 45, 13, 53, 21, 61, 29,
    36,  4, 44, 12, 52, 20, 60, 28,
    35,  3, 43, 11, 51, 19, 59, 27,
    34,  2, 42, 10, 50, 18, 58, 26,
    33,  1, 41,  9, 49, 17, 57, 25
  );

  E: array [1..48] of byte = (
    32,  1,  2,  3,  4,  5,
     4,  5,  6,  7,  8,  9,
     8,  9, 10, 11, 12, 13,
    12, 13, 14, 15, 16, 17,
    16, 17, 18, 19, 20, 21,
    20, 21, 22, 23, 24, 25,
    24, 25, 26, 27, 28, 29,
    28, 29, 30, 31, 32,  1
  );

  P: array [1..32] of byte = (
    16,  7, 20, 21, 29, 12, 28, 17,
     1, 15, 23, 26,  5, 18, 31, 10,
     2,  8, 24, 14, 32, 27,  3,  9,
    19, 13, 30,  6, 22, 11,  4, 25);

  S1: array [0..3, 0..15] of byte = (
    (14,  4, 13,  1,  2, 15, 11,  8,  3, 10,  6, 12,  5,  9,  0,  7),
    ( 0, 15,  7,  4, 14,  2, 13,  1, 10,  6, 12, 11,  9,  5,  3,  8),
    ( 4,  1, 14,  8, 13,  6,  2, 11, 15, 12,  9,  7,  3, 10,  5,  0),
    (15, 12,  8,  2,  4,  9,  1,  7,  5, 11,  3, 14, 10,  0,  6, 13)
  );

  S2: array [0..3, 0..15] of byte = (
    (15,  1,  8, 14,  6, 11,  3,  4,  9,  7,  2, 13, 12,  0,  5, 10),
    ( 3, 13,  4,  7, 15,  2,  8, 14, 12,  0,  1, 10,  6,  9, 11,  5),
    ( 0, 14,  7, 11, 10,  4, 13,  1,  5,  8, 12,  6,  9,  3,  2, 15),
    (13,  8, 10,  1,  3, 15,  4,  2, 11,  6,  7, 12,  0,  5, 14,  9)
  );

  S3: array [0..3, 0..15] of byte = (
    (10,  0,  9, 14,  6,  3, 15,  5,  1, 13, 12,  7, 11,  4,  2,  8),
    (13,  7,  0,  9,  3,  4,  6, 10,  2,  8,  5, 14, 12, 11, 15,  1),
    (13,  6,  4,  9,  8, 15,  3,  0, 11,  1,  2, 12,  5, 10, 14,  7),
    ( 1, 10, 13,  0,  6,  9,  8,  7,  4, 15, 14,  3, 11,  5,  2, 12)
  );

  S4: array [0..3, 0..15] of byte = (
    ( 7, 13, 14,  3,  0,  6,  9, 10,  1,  2,  8,  5, 11, 12,  4, 15),
    (13,  8, 11,  5,  6, 15,  0,  3,  4,  7,  2, 12,  1, 10, 14,  9),
    (10,  6,  9,  0, 12, 11,  7, 13, 15,  1,  3, 14,  5,  2,  8,  4),
    ( 3, 15,  0,  6, 10,  1, 13,  8,  9,  4,  5, 11, 12,  7,  2, 14)
  );

  S5: array [0..3, 0..15] of byte = (
    ( 2, 12,  4,  1,  7, 10, 11,  6,  8,  5,  3, 15, 13,  0, 14,  9),
    (14, 11,  2, 12,  4,  7, 13,  1,  5,  0, 15, 10,  3,  9,  8,  6),
    ( 4,  2,  1, 11, 10, 13,  7,  8, 15,  9, 12,  5,  6,  3,  0, 14),
    (11,  8, 12,  7,  1, 14,  2, 13,  6, 15,  0,  9, 10,  4,  5,  3)
  );

  S6: array [0..3, 0..15] of byte = (
    (12,  1, 10, 15,  9,  2,  6,  8,  0, 13,  3,  4, 14,  7,  5, 11),
    (10, 15,  4,  2,  7, 12,  9,  5,  6,  1, 13, 14,  0, 11,  3,  8),
    ( 9, 14, 15,  5,  2,  8, 12,  3,  7,  0,  4, 10,  1, 13, 11,  6),
    ( 4,  3,  2, 12,  9,  5, 15, 10, 11, 14,  1,  7,  6,  0,  8, 13)
  );

  S7: array [0..3, 0..15] of byte = (
    ( 4, 11,  2, 14, 15,  0,  8, 13,  3, 12,  9,  7,  5, 10,  6,  1),
    (13,  0, 11,  7,  4,  9,  1, 10, 14,  3,  5, 12,  2, 15,  8,  6),
    ( 1,  4, 11, 13, 12,  3,  7, 14, 10, 15,  6,  8,  0,  5,  9,  2),
    ( 6, 11, 13,  8,  1,  4, 10,  7,  9,  5,  0, 15, 14,  2,  3, 12)
  );

  S8: array [0..3, 0..15] of byte = (
    (13,  2,  8,  4,  6, 15, 11,  1, 10,  9,  3, 14,  5,  0, 12,  7),
    ( 1, 15, 13,  8, 10,  3,  7,  4, 12,  5,  6, 11,  0, 14,  9,  2),
    ( 7, 11,  4,  1,  9, 12, 14,  2,  0,  6, 10, 13, 15,  3,  5,  8),
    ( 2,  1, 14,  7,  4, 10,  8, 13, 15, 12,  9,  0,  3,  5,  6, 11)
  );

procedure EncryptDES(const InBuf: TDESBuffer; const ExpandedKey: TDESExpandedKey;
  var OutBuf: TDESBuffer);
var
  L, R, T2, T3: array [1..32] of byte;
  I, J: integer;
  B: byte;
  T: array [1..48] of byte;
  Row, Col: integer;
begin
  // dividing into bits and performing initial permutation
  for I := 1 to 64 do
  begin
    B := (InBuf[(IP[I] - 1) shr 3] and (128 shr ((IP[I] - 1) and $07)));
    if B > 0 then B := 1;
    if I <= 32 then
      L[I] := B
    else
      R[I - 32] := B;
  end;
  for I := 1 to 16 do
  begin
    // expanding and XORing
    for J := 1 to 48 do
      T[J] := R[E[J]] xor ExpandedKey[I - 1, J - 1];
    // selecting
    // step 1
    Row := (T[1] shl 1) or T[6];
    Col := (T[2] shl 3) or (T[3] shl 2) or (T[4] shl 1) or T[5];
    T2[1] := (S1[Row, Col] and $08) shr 3; T2[2] := (S1[Row, Col] and $04) shr 2;
    T2[3] := (S1[Row, Col] and $02) shr 1; T2[4] := S1[Row, Col] and $01;
    // step 2
    Row := (T[7] shl 1) or T[12];
    Col := (T[8] shl 3) or (T[9] shl 2) or (T[10] shl 1) or T[11];
    T2[5] := (S2[Row, Col] and $08) shr 3; T2[6] := (S2[Row, Col] and $04) shr 2;
    T2[7] := (S2[Row, Col] and $02) shr 1; T2[8] := S2[Row, Col] and $01;
    // step 3
    Row := (T[13] shl 1) or T[18];
    Col := (T[14] shl 3) or (T[15] shl 2) or (T[16] shl 1) or T[17];
    T2[9] := (S3[Row, Col] and $08) shr 3; T2[10] := (S3[Row, Col] and $04) shr 2;
    T2[11] := (S3[Row, Col] and $02) shr 1; T2[12] := S3[Row, Col] and $01;
    // step 4
    Row := (T[19] shl 1) or T[24];
    Col := (T[20] shl 3) or (T[21] shl 2) or (T[22] shl 1) or T[23];
    T2[13] := (S4[Row, Col] and $08) shr 3; T2[14] := (S4[Row, Col] and $04) shr 2;
    T2[15] := (S4[Row, Col] and $02) shr 1; T2[16] := S4[Row, Col] and $01;
    // step 5
    Row := (T[25] shl 1) or T[30];
    Col := (T[26] shl 3) or (T[27] shl 2) or (T[28] shl 1) or T[29];
    T2[17] := (S5[Row, Col] and $08) shr 3; T2[18] := (S5[Row, Col] and $04) shr 2;
    T2[19] := (S5[Row, Col] and $02) shr 1; T2[20] := S5[Row, Col] and $01;
    // step 6
    Row := (T[31] shl 1) or T[36];
    Col := (T[32] shl 3) or (T[33] shl 2) or (T[34] shl 1) or T[35];
    T2[21] := (S6[Row, Col] and $08) shr 3; T2[22] := (S6[Row, Col] and $04) shr 2;
    T2[23] := (S6[Row, Col] and $02) shr 1; T2[24] := S6[Row, Col] and $01;
    // step 7
    Row := (T[37] shl 1) or T[42];
    Col := (T[38] shl 3) or (T[39] shl 2) or (T[40] shl 1) or T[41];
    T2[25] := (S7[Row, Col] and $08) shr 3; T2[26] := (S7[Row, Col] and $04) shr 2;
    T2[27] := (S7[Row, Col] and $02) shr 1; T2[28] := S7[Row, Col] and $01;
    // step 8
    Row := (T[43] shl 1) or T[48];
    Col := (T[44] shl 3) or (T[45] shl 2) or (T[46] shl 1) or T[47];
    T2[29] := (S8[Row, Col] and $08) shr 3; T2[30] := (S8[Row, Col] and $04) shr 2;
    T2[31] := (S8[Row, Col] and $02) shr 1; T2[32] := S8[Row, Col] and $01;
    // permutation
    for J := 1 to 32 do
      T3[J] := T2[P[J]] xor L[J];
    // exchanging
    Move(R, L, SizeOf(R));
    Move(T3, R, SizeOf(T3));
  end;
  // finalization
  FillChar(OutBuf, SizeOf(OutBuf), 0);
  for I := 1 to 64 do
  begin
    B := IPR[I];
    if B <= 32 then B := R[B] else B := L[B - 32];
    if B > 0 then
      OutBuf[(I - 1) shr 3] := OutBuf[(I - 1) shr 3] or (128 shr ((I - 1) and $07));
  end;
end;

procedure DecryptDES(const InBuf: TDESBuffer; const ExpandedKey: TDESExpandedKey;
  var OutBuf: TDESBuffer);
var
  L, R, T2, T3: array [1..32] of byte;
  I, J: integer;
  B: byte;
  T: array [1..48] of byte;
  Row, Col: integer;
begin
  // dividing into bits and performing initial permutation
  for I := 1 to 64 do
  begin
    B := (InBuf[(IP[I] - 1) shr 3] and (128 shr ((IP[I] - 1) and $07)));
    if B > 0 then B := 1;
    if I <= 32 then
      L[I] := B
    else
      R[I - 32] := B;
  end;
  for I := 1 to 16 do
  begin
    // expanding and XORing
    for J := 1 to 48 do
      T[J] := R[E[J]] xor ExpandedKey[16 - I, J - 1];
    // selecting
    // step 1
    Row := (T[1] shl 1) or T[6];
    Col := (T[2] shl 3) or (T[3] shl 2) or (T[4] shl 1) or T[5];
    T2[1] := (S1[Row, Col] and $08) shr 3; T2[2] := (S1[Row, Col] and $04) shr 2;
    T2[3] := (S1[Row, Col] and $02) shr 1; T2[4] := S1[Row, Col] and $01;
    // step 2
    Row := (T[7] shl 1) or T[12];
    Col := (T[8] shl 3) or (T[9] shl 2) or (T[10] shl 1) or T[11];
    T2[5] := (S2[Row, Col] and $08) shr 3; T2[6] := (S2[Row, Col] and $04) shr 2;
    T2[7] := (S2[Row, Col] and $02) shr 1; T2[8] := S2[Row, Col] and $01;
    // step 3
    Row := (T[13] shl 1) or T[18];
    Col := (T[14] shl 3) or (T[15] shl 2) or (T[16] shl 1) or T[17];
    T2[9] := (S3[Row, Col] and $08) shr 3; T2[10] := (S3[Row, Col] and $04) shr 2;
    T2[11] := (S3[Row, Col] and $02) shr 1; T2[12] := S3[Row, Col] and $01;
    // step 4
    Row := (T[19] shl 1) or T[24];
    Col := (T[20] shl 3) or (T[21] shl 2) or (T[22] shl 1) or T[23];
    T2[13] := (S4[Row, Col] and $08) shr 3; T2[14] := (S4[Row, Col] and $04) shr 2;
    T2[15] := (S4[Row, Col] and $02) shr 1; T2[16] := S4[Row, Col] and $01;
    // step 5
    Row := (T[25] shl 1) or T[30];
    Col := (T[26] shl 3) or (T[27] shl 2) or (T[28] shl 1) or T[29];
    T2[17] := (S5[Row, Col] and $08) shr 3; T2[18] := (S5[Row, Col] and $04) shr 2;
    T2[19] := (S5[Row, Col] and $02) shr 1; T2[20] := S5[Row, Col] and $01;
    // step 6
    Row := (T[31] shl 1) or T[36];
    Col := (T[32] shl 3) or (T[33] shl 2) or (T[34] shl 1) or T[35];
    T2[21] := (S6[Row, Col] and $08) shr 3; T2[22] := (S6[Row, Col] and $04) shr 2;
    T2[23] := (S6[Row, Col] and $02) shr 1; T2[24] := S6[Row, Col] and $01;
    // step 7
    Row := (T[37] shl 1) or T[42];
    Col := (T[38] shl 3) or (T[39] shl 2) or (T[40] shl 1) or T[41];
    T2[25] := (S7[Row, Col] and $08) shr 3; T2[26] := (S7[Row, Col] and $04) shr 2;
    T2[27] := (S7[Row, Col] and $02) shr 1; T2[28] := S7[Row, Col] and $01;
    // step 8
    Row := (T[43] shl 1) or T[48];
    Col := (T[44] shl 3) or (T[45] shl 2) or (T[46] shl 1) or T[47];
    T2[29] := (S8[Row, Col] and $08) shr 3; T2[30] := (S8[Row, Col] and $04) shr 2;
    T2[31] := (S8[Row, Col] and $02) shr 1; T2[32] := S8[Row, Col] and $01;
    // permutation
    for J := 1 to 32 do
      T3[J] := T2[P[J]] xor L[J];
    // exchanging
    Move(R, L, SizeOf(R));
    Move(T3, R, SizeOf(T3));
  end;
  // finalization
  FillChar(OutBuf, SizeOf(OutBuf), 0);
  for I := 1 to 64 do
  begin
    B := IPR[I];
    if B <= 32 then B := R[B] else B := L[B - 32];
    if B > 0 then
      OutBuf[(I - 1) shr 3] := OutBuf[(I - 1) shr 3] or (128 shr ((I - 1) and $07));
  end;
end;

procedure EncryptDESStreamECB(Source: TStream; Count: cardinal;
  const Key: TDESKey; Dest: TStream);
var
  ExpandedKey: TDESExpandedKey;
begin
  ExpandDESKey(Key, ExpandedKey);
  EncryptDESStreamECB(Source, Count, ExpandedKey, Dest);
end;

procedure EncryptDESStreamECB(Source: TStream; Count: cardinal;
  const ExpandedKey: TDESExpandedKey; Dest: TStream);
var
  TempIn, TempOut: TDESBuffer;
  Done: cardinal;
begin
  if Count = 0 then
  begin
    Source.Position := 0;
    Count := Source.Size;
  end
  else Count := Min(Count, Source.Size - Source.Position);
  if Count = 0 then exit;
  while Count >= SizeOf(TDESBuffer) do
  begin
    Done := Source.Read(TempIn, SizeOf(TempIn));
    if Done < SizeOf(TempIn) then
      raise EStreamError.Create(SReadError);
    EncryptDES(TempIn, ExpandedKey, TempOut);
    Done := Dest.Write(TempOut, SizeOf(TempOut));
    if Done < SizeOf(TempOut) then
      raise EStreamError.Create(SWriteError);
    Dec(Count, SizeOf(TDESBuffer));
  end;
  if Count > 0 then
  begin
    Done := Source.Read(TempIn, Count);
    if Done < Count then
      raise EStreamError.Create(SReadError);
    FillChar(TempIn[Count], SizeOf(TempIn) - Count, 0);
    EncryptDES(TempIn, ExpandedKey, TempOut);
    Done := Dest.Write(TempOut, SizeOf(TempOut));
    if Done < SizeOf(TempOut) then
      raise EStreamError.Create(SWriteError);
  end;
end;

procedure DecryptDESStreamECB(Source: TStream; Count: cardinal;
  const Key: TDESKey; Dest: TStream);
var
  ExpandedKey: TDESExpandedKey;
begin
  ExpandDESKey(Key, ExpandedKey);
  DecryptDESStreamECB(Source, Count, ExpandedKey, Dest);
end;

procedure DecryptDESStreamECB(Source: TStream; Count: cardinal;
  const ExpandedKey: TDESExpandedKey; Dest: TStream);
var
  TempIn, TempOut: TDESBuffer;
  Done: cardinal;
begin
  if Count = 0 then
  begin
    Source.Position := 0;
    Count := Source.Size;
  end
  else Count := Min(Count, Source.Size - Source.Position);
  if Count = 0 then exit;
  if (Count mod SizeOf(TDESBuffer)) > 0 then
    raise EDESError.Create(SInvalidInBufSize);
  while Count >= SizeOf(TDESBuffer) do
  begin
    Done := Source.Read(TempIn, SizeOf(TempIn));
    if Done < SizeOf(TempIn) then
      raise EStreamError.Create(SReadError);
    DecryptDES(TempIn, ExpandedKey, TempOut);
    Done := Dest.Write(TempOut, SizeOf(TempOut));
    if Done < SizeOf(TempOut) then
      raise EStreamError.Create(SWriteError);
    Dec(Count, SizeOf(TDESBuffer));
  end;
end;

procedure EncryptDESStreamCBC(Source: TStream; Count: cardinal;
  const Key: TDESKey; const InitVector: TDESBuffer; Dest: TStream);
var
  ExpandedKey: TDESExpandedKey;
begin
  ExpandDESKey(Key, ExpandedKey);
  EncryptDESStreamCBC(Source, Count, ExpandedKey, InitVector, Dest);
end;

procedure EncryptDESStreamCBC(Source: TStream; Count: cardinal;
  const ExpandedKey: TDESExpandedKey; const InitVector: TDESBuffer;
  Dest: TStream);
var
  TempIn, TempOut, Vector: TDESBuffer;
  Done: cardinal;
begin
  if Count = 0 then
  begin
    Source.Position := 0;
    Count := Source.Size;
  end
  else Count := Min(Count, Source.Size - Source.Position);
  if Count = 0 then exit;
  Vector := InitVector;
  while Count >= SizeOf(TDESBuffer) do
  begin
    Done := Source.Read(TempIn, SizeOf(TempIn));
    if Done < SizeOf(TempIn) then
      raise EStreamError.Create(SReadError);
    PLongWord(@TempIn[0])^ := PLongWord(@TempIn[0])^ xor PLongWord(@Vector[0])^;
    PLongWord(@TempIn[4])^ := PLongWord(@TempIn[4])^ xor PLongWord(@Vector[4])^;
    EncryptDES(TempIn, ExpandedKey, TempOut);
    Done := Dest.Write(TempOut, SizeOf(TempOut));
    if Done < SizeOf(TempOut) then
      raise EStreamError.Create(SWriteError);
    Vector := TempOut;
    Dec(Count, SizeOf(TDESBuffer));
  end;
  if Count > 0 then
  begin
    Done := Source.Read(TempIn, Count);
    if Done < Count then
      raise EStreamError.Create(SReadError);
    FillChar(TempIn[Count], SizeOf(TempIn) - Count, 0);
    PLongWord(@TempIn[0])^ := PLongWord(@TempIn[0])^ xor PLongWord(@Vector[0])^;
    PLongWord(@TempIn[4])^ := PLongWord(@TempIn[4])^ xor PLongWord(@Vector[4])^;
    EncryptDES(TempIn, ExpandedKey, TempOut);
    Done := Dest.Write(TempOut, SizeOf(TempOut));
    if Done < SizeOf(TempOut) then
      raise EStreamError.Create(SWriteError);
  end;
end;

procedure DecryptDESStreamCBC(Source: TStream; Count: cardinal;
  const Key: TDESKey; const InitVector: TDESBuffer; Dest: TStream);
var
  ExpandedKey: TDESExpandedKey;
begin
  ExpandDESKey(Key, ExpandedKey);
  DecryptDESStreamCBC(Source, Count, ExpandedKey, InitVector, Dest);
end;

procedure DecryptDESStreamCBC(Source: TStream; Count: cardinal;
  const ExpandedKey: TDESExpandedKey;  const InitVector: TDESBuffer;
  Dest: TStream);
var
  TempIn, TempOut: TDESBuffer;
  Vector1, Vector2: TDESBuffer;
  Done: cardinal;
begin
  if Count = 0 then
  begin
    Source.Position := 0;
    Count := Source.Size;
  end
  else Count := Min(Count, Source.Size - Source.Position);
  if Count = 0 then exit;
  if (Count mod SizeOf(TDESBuffer)) > 0 then
    raise EDESError.Create(SInvalidInBufSize);
  Vector1 := InitVector;
  while Count >= SizeOf(TDESBuffer) do
  begin
    Done := Source.Read(TempIn, SizeOf(TempIn));
    if Done < SizeOf(TempIn) then
      raise EStreamError(SReadError);
    Vector2 := TempIn;
    DecryptDES(TempIn, ExpandedKey, TempOut);
    PLongWord(@TempOut[0])^ := PLongWord(@TempOut[0])^ xor PLongWord(@Vector1[0])^;
    PLongWord(@TempOut[4])^ := PLongWord(@TempOut[4])^ xor PLongWord(@Vector1[4])^;
    Done := Dest.Write(TempOut, SizeOf(TempOut));
    if Done < SizeOf(TempOut) then
      raise EStreamError(SWriteError);
    Vector1 := Vector2;
    Dec(Count, SizeOf(TDESBuffer));
  end;
end;
Procedure _SetDESStringKey(Const AKey:AnsiString;var desKey:PDESExpandedKey);
var
TempDESKey:TDESKey;
len:Integer;
begin
   len:=Length(AKey);
   Fillchar(TEmpDesKey,sizeof(TDesKey),#0);
   if len>sizeof(TDESKey) then len:=sizeof(TDESKey);
   move(AKey[1],TempDeskey,len);
   ExpandDESKey(TempDesKey,deskey^);

end;

end.
