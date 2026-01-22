{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10055: AstaIOAutoUpgrade.pas 
{
{   Rev 1.0    4/10/2003 6:30:04 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:36:38 PM  Steve    Version: 1.505
}
unit AstaIOAutoUpgrade;

{*********************************************************}
{*   Copyright (c) 1997-2002 Asta Technology Group Inc.  *}
{*               All rights reserved.                    *}
{*               www.astatech.com                        *}
{*********************************************************}

interface
{$I AstaIO.inc}

uses Classes,
  AstaIOParamList,
  AstaIOUserList,
  AstaIOConst;

type
  TAutoUpgradeEvent = procedure(Sender: TObject; U: TUserRecord; AppName,AppVersion:String;var UpgradeInfo: TAutoUpgradeResponse;
    var Host,UpgradeFile,MessageForClient: string; ClientParams: TAstaParamList) of object;
  TAutoUpgradeFormat=(auInifile,AuRegistry,AuXML,AuStreamedDataSet);
  TAstaIOAutoUpgrade = class(TComponent)
  private
    FUpgradeEvent: TAutoUpgradeEvent;
    FServerWire: TComponent;
    Function UpgradeInfoInteger(U:TAutoUpgradeResponse):Integer;
  protected
    procedure DoUpgradeDecide(U: TUserRecord; ClientParams: TAstaParamList);
    procedure SetServerWire(Value: TComponent);
    function GetServerWire: TComponent;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Process(U: TUserRecord; ClientParams: TAstaParamList);
    property ServerWire: TComponent read GetServerWire write SetServerWire;
  published
    property OnUpgradeDecide: TAutoUpgradeEvent read FUpgradeEvent write FUpgradeEvent;
  end;
Function AstaIOUpgradeTempFile:String;
procedure ReplaceClientforUpgrade;

implementation
uses AstaIOServerWire,
     AstaIOUtil,
     AstaIOBits,
     SysUtils
     {$ifdef mswindows},
     ShellAPI,
     Windows,
     Forms
     {$endif}
     ;

Function TAstaIOAutoUpgrade.UpgradeInfoInteger(u:TAutoUpgradeResponse):Integer;
var
 i:TAutoUpgradeResponseBits;
begin
 result:=0;
 for i:=low(TAutoUpgradeResponseBits) to high(TAutoUpgradeResponseBits) do
  if (i in U) then  SetBit(result,ord(i));
end;

procedure TAstaIOAutoUpgrade.DoUpgradeDecide(U: TUserRecord; ClientParams : TAstaParamList);
var
  UpgradeInfo: TAutoUpgradeResponse;
  UpgradeMsg,UpgradeFile,Host: string;
  UpgradeParams,P:TAstaParamList;
begin
  if not Assigned(FUpgradeEvent) then exit;
  UpgradeParams:=TAstaParamList.Create;
  try
  UpgradeMsg := '';
  UpgradeFile:='';
  Host:='';
  UpgradeInfo := [];
  FUpgradeEvent(FServerWire, U,U.AppName,U.AppVersion, UpgradeInfo, Host,UpgradeFile,UpgradeMsg,UpGradeParams);
  if UpgradeInfo<>[] then begin
   p:=TAstaParamList.Create;
   try
    p.FastAdd(UpgradeInfoConst,UpgradeInfoInteger(UpgradeInfo));
    if Host<>'' then p.FastAdd(UpgradeHostConst,Host);
    if Upgradefile<>'' then p.FastAdd(UpgradeFileConst,UpgradeFile);
    if UpgradeMsg<>'' then p.FastAdd(UpgradeMsgConst,UpgradeMsg);
    if UpgradeParams.Count>0 then p.FastAdd(UpgradeExtraParams,Upgradeparams.AsTokenizedString(False));
    ClientParams.FastAdd(UpgradeInfoParamConst,P.AsTokenizedString(False));
    finally
    p.free;
   end;
  end;
   finally
    UpgradeParams.free;
   end;
end;

constructor TAstaIOAutoUpGrade.Create(AOwner: TComponent);
begin
  inherited Create(Aowner);
  FServerWire := nil;
end;

procedure TAstaIOAutoUpGrade.Process(U: TUserRecord; ClientParams: TAstaParamList);
begin
  DoUpgradeDecide(U, ClientParams);
end;

procedure TAstaIOAutoUpGrade.SetServerWire(Value: TComponent);
begin
  FServerWire := Value;
end;

function TAstaIOAutoUpGrade.GetServerWire: TComponent;
begin
  result := FServerWire;
end;

Function AstaIOUpgradeTempFile:String;
begin
result:=ExtractFilePath(ParamStr(0))+'~astaiotemp.exe';
end;


procedure ReplaceClientforUpgrade;
var
  FOut : TextFile;
  FN, FNOld :String;
begin
  FN := ExtractFileName(ParamStr(0));
  FNOld := FN + '.OLD';
  System.Assign(FOut,'ReStart.Bat');
  Rewrite(FOut);
//Get rid of the prior updates backup, the TheFile.exe.old will always hold the previous version
  Writeln(FOut,'DEL ' + FNOld);
//Windows 95/98 lock the file so a loop must be setup.
  Writeln(FOut,':xRetry'); // Robert Noble
//Rename the current version (1.0 which is getting updated by a version > 1.0) to
//TheFile.exe.old  -- it now becomes the latest previous version
  Writeln(FOut,'ReName ' + FN + ' ' + FNOld);
//The Win95/98 loop
  Writeln(FOut,'if not exist ' + FNOld + ' goto xRetry'); // rjn fix
//The new file was streamed to disk as TempFile.exe, now it is renamed to to the
//name of the actual running program
  Writeln(FOut,'Rename ' + AstaIOUpgradeTempFile + ' ' + FN);
//The Call will launch the new program
  Writeln(FOut,'Call ' +cmdline);  // ParamsFromProgram was not getting all the Params // JAM 11/13/2000
  //+ FN+' '+ParamsFromProgram);//added so that original params get executed
//Because the last chars in the .Bat are not CRLF, it can delete itself
  Write(FOut,'Del ReStart.Bat');
  CloseFile(FOut);
// SW_Hide -- keeps the annoying shell from popping up in the user's face
  {$ifdef mswindows}
  ShellExecute(0, nil, PChar('Restart.bat'), nil, nil, SW_HIDE);
  Application.Terminate;
  {$endif}
end;

end.

