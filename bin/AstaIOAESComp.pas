{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10049: AstaIOAESComp.pas 
{
{   Rev 1.0    4/10/2003 6:30:02 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:36:36 PM  Steve    Version: 1.505
}
unit AstaIOAESComp;

interface

uses Classes,AstaIOAES,SysUtils;

Type
 TAstaAESComponent=Class(TObject)
   private
    FAESInKey: Pointer;
    FAESOutKey: Pointer;
    procedure FreeAES;
    public
    Constructor Create;virtual;
    Destructor Destroy;override;
    function  AESEncrypt(Data: String): String;
    function  AESDecrypt(Data: String): String;
    procedure SetAESKey(AKey : Pointer; KeyType : aes_key);
 end;

implementation

Constructor TAstaAESComponent.Create;
begin
 inherited;
 FAESInKey:=nil;
 FAESOutKey:=nil;
end;

Destructor TAstaAESComponent.Destroy;
begin
 FreeAES;
 inherited;
end;

function TAstaAESComponent.AESEncrypt(Data : string): string;
var l,
    m,
    rl : integer;
    H  : TAESHandle;
begin
  if FAESInKey = nil then
    result := #0#0#0#0
  else
  begin
    FillChar(H, sizeof(TAESHandle), 0);
    l := Length(Data);
    m := l mod AES_BLOCK_SIZE;
    if m = 0
      then rl := l
      else rl := l - m + AES_BLOCK_SIZE;
    SetLength(result, rl + sizeof(Integer));
    try
      if not AESSetKey(@H, FAESInKey, AES_BLOCK_SIZE, kmEncode) then
        raise Exception.Create('');

      //added by EM 7 Mar 2001
      SetLength(Data, rl);
      if rl > l then  FillChar(Data[l+1], rl - l, 0);

      if not AESEncryptBuffer(@H, PChar(Data), @Result[5], rl, rl) then
        raise Exception.Create('');
      Move(l, Result[1], sizeof(integer)); //set source length
    except
      on E : Exception do
      begin
        result := #0#0#0#0;
      end;
    end;
  end;
end;

procedure TAstaAESComponent.FreeAES;
begin
  if FAESInKey <> nil then
  begin
    FreeMem(FAESInKey);
    FAESInKey := nil;
  end;
  if FAESOutKey <> nil then
  begin
    FreeMem(FAESOutKey);
    FAESOutKey := nil;
  end;
end;

procedure TAstaAESComponent.SetAESKey(AKey : Pointer; KeyType : aes_key);
begin
  if (AKey <> nil) then
  begin
    if KeyType in [kmEncode, kmBoth] then
    begin
      if FAESInKey = nil then
        GetMem(FAESInKey, AES_BLOCK_SIZE);
      //MoveMemory(FAESInKey, AKey, AES_BLOCK_SIZE);
      move(PChar(AKey)^, PChar(FAESInKey)^, AES_BLOCK_SIZE);
    end;
    if KeyType in [kmDecode, kmBoth] then
    begin
      if FAESOutKey = nil then
        GetMem(FAESOutKey, AES_BLOCK_SIZE);
      //MoveMemory(FAESOutKey, AKey, AES_BLOCK_SIZE);
      move(PChar(AKey)^, PChar(FAESOutKey)^, AES_BLOCK_SIZE);
    end;
  end
  else
  begin
    if KeyType in [kmEncode, kmBoth] then
    begin
      if FAESInKey <> nil then
        FreeMem(FAESInKey);
      FAESInKey := nil;
    end;
    if KeyType in [kmDecode, kmBoth] then
    begin
      if FAESOutKey <> nil then
        FreeMem(FAESOutKey);
      FAESOutKey := nil;
    end;
  end;
end;
function TAstaAESComponent.AESDecrypt(Data : string): string;
var l, rl : integer;
    H  : TAESHandle;
    needRl: Integer;
begin
  result := '';
  if FAESOutKey <> nil then
  begin
    FillChar(H, sizeof(TAESHandle), 0);
    l := Length(Data);
    if l > 4 then
    begin
      Move(Data[1], rl, sizeof(Integer));
      if (l - 4 >= rl) then
      begin
        SetLength(result, l-4);
        FillChar(result[1], l-4, 0);
        if AESSetKey(@H, FAESOutKey, AES_BLOCK_SIZE, kmDecode) then
        begin
          if not AESDecryptBuffer(@H, @Data[5], PChar(Result), l - 4, l - 4)
            then result := ''
            else if l-4>rl
              then Delete(Result, rl+1, l-4-rl);
        end;
      end;
    end;
  end;
end;

end.
