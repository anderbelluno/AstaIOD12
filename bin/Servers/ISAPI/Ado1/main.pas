unit main;
{*********************************************************}
{*   Copyright (c) 2000-2003 Asta Technology Group Inc.  *}
{*               All rights reserved.                    *}
{*               www.astatech.com                        *}
{*********************************************************}


interface

uses
  Windows, Classes, HTTPApp, AstaIOClientMsgWire, AstaIOClientWire,
  AstaIOStringClientWire,AstaIOAdoSupplementDM,dm,AstaIOIsapiStringServerUtils;


type
  TAstaModule = class(TWebModule)
    Client: TAstaIOStringClientWire;
    procedure WebModuleCreate(Sender: TObject);
    procedure WebModuleDestroy(Sender: TObject);
    procedure AstaModuleTransferAction(Sender: TObject;
    Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
  private
  public
    { Public declarations }
    FAstaAdoDataModule:TAstaIOADODBPluginDM;
    FAstaIOIsapiHelper:TAstaIOStringISAPIHelper;
  end;

var
  AstaModule: TAstaModule;


implementation
uses  SysUtils,AstaIOStringServerWire;

{$R *.DFM}


procedure TAstaModule.WebModuleCreate(Sender: TObject);
begin
  FAstaAdoDataModule:=TAstaIOADODBPluginDM.Create(Self);
  FAstaIOIsapiHelper:=TAstaIOStringISAPIHelper.Create(Client);
  Client.ServerWire:=FAstaAdoDataModule.Serverwire;
  Client.Serverwire.Active:=True;
  Client.Serverwire.SetDataBaseSessionPool(1);
  Client.Active:=True;
end;

procedure TAstaModule.AstaModuleTransferAction(Sender: TObject;
  Request: TWebRequest; Response: TWebResponse; var Handled: Boolean);
begin
  FAstaIOIsapiHelper.AstaModuleTransferAction(Sender,Request,Response,Handled);
end;


procedure TAstaModule.WebModuleDestroy(Sender: TObject);
begin
  FAstaAdoDataModule.Free;
  FAstaIOIsapiHelper.Free;
end;

initialization

end.
