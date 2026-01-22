{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10191: AstaIOIsapiLogin.pas 
{
{   Rev 1.0    4/10/2003 6:31:12 AM  Steve
}
unit AstaIOIsapiLogin;
{$I AstaIO.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, Buttons,AstaIOWebClientWire;

type
  TAstaIsapiSetupForm = class(TForm)
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    PageControl: TPageControl;
    TabSheet2: TTabSheet;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    ServerAddressEdit: TEdit;
    ServerPortEdit: TEdit;
    ServerUserNameEdit: TEdit;
    ServerPasswordEdit: TEdit;
    IsapiDLLPathEdit: TEdit;
    Label5: TLabel;
    WebServerAddressEdit: TEdit;
    WebServerPortEdit: TEdit;
    WinInetCheckBox: TCheckBox;
  private
    { Private declarations }
  public
    { Public declarations }
    FWire:TAstaIOWebClientWire;
    Procedure Setup(ClientWire:TAstaIOWebClientWire);
    Procedure UpdateClientSocket(ClientWire:TAstaIOWebClientWire);
  end;


Procedure AstaIsapiSetup(ClientWire:TAstaIOWebClientWire);
implementation
uses AstaIOUtil;
{$R *.DFM}

Procedure AstaIsapiSetup(ClientWire:TAstaIOWebClientWire);
var
  AstaIsapiSetupForm: TAstaIsapiSetupForm;
begin
  AstaIsapiSetupForm:=TAstaIsapiSetupForm.Create(nil);
  AstaIsapiSetupForm.Setup(ClientWire);
  AstaIsapiSetupForm.ShowModal;
  if AstaIsapiSetupForm.modalresult=mrok then
  AstaIsapiSetupForm.UpdateClientSocket(ClientWire);
  AstaIsapiSetupForm.Free;
end;

Procedure TAstaIsapiSetupForm.UpdateClientSocket(ClientWire:TAstaIOWebClientWire);
begin
 with ClientWire do begin
   WebServer.Address           := WebServerAddressEdit.Text;
   WebServer.Port              := StringToInteger(WebServerPortEdit.Text);
   WebServer.Address           := ServerAddressEdit.Text;
   WebServer.Port              := StrToIntDef(trim(ServerPortEdit.Text),9000);
   UserName                    := ServerUserNameEdit.Text;
   Password                    := ServerPassWordEdit.Text;
   WebServer.script            := IsapiDllPathEdit.Text;
   UseWinInet                  := WinInetCheckBox.Checked;
   if WebServer.Address<>'' then webServer.UseWebServer:=True;
 end;
end;

Procedure TAstaIsapiSetupForm.Setup(ClientWire:TAstaIOWebClientWire);
begin
 FWire:=ClientWire;
 with FWire do begin
   WebServerAddressEdit.Text   := WebServer.Address;
   if WebServerAddressEdit.text='' then WebServerAddressEdit.text:=GetThePcsIpAddress;
   WebServerPortEdit.Text      := InttoStr(WebServer.Port);
   ServerAddressEdit.Text      := Address;
   ServerPortEdit.Text         := IntToStr(Port);
   ServerUserNameEdit.Text     := UserName;
   ServerPassWordEdit.Text     := Password;
   IsapiDllPathEdit.Text       := WebServer.Script;
   WinInetCheckBox.Checked     := UseWinInet;
  end;
end;

end.
