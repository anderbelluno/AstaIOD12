unit SocketDM;
{$I Asta.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ScktComp,
  syncobjs,
  AstaIOServiceUtils, AstaIODataBasePlugin, AstaIOServerWire,
  AstaIOSocketServer,AstaIOConst;

type
  TServerDM = class(TDataModule)
    ServerWire: TAstaIOSocketServerWire;
    DataBasePlugin: TAstaIODataBasePlugin;
    procedure DataModuleCreate(Sender: TObject);
  private
    procedure LogException(Sender: TObject; Topic, ErrorMsg: string);
    procedure AstaException(Sender: TObject; E: Exception);
  protected
  public
    procedure Log(Msg: string);
    procedure SetGui(FromService:Boolean);
    procedure ToggleServerWireActive;
  end;

var
  ServerDM         : TServerDM;

implementation
uses mainUnit,AstaIOUtil
  {$ifdef Ver140}
  ,variants
  {$endif};


{$R *.DFM}

procedure TServerDM.Log(Msg: string);
begin
  AstaServiceForm.Log(msg);
end;

procedure TServerDM.AstaException(Sender: TObject; E: Exception);
begin
  if E is ESocketError then
    Log('Socket Error: ' + E.Message)  // don't try to send to client
  else
    LogException(Sender, 'General Error', E.Message);
end;

procedure TServerDM.LogException(Sender: TObject; Topic, ErrorMsg: string);
begin
  Log(Topic + ': ' + ErrorMsg);
end;


procedure TServerDM.SetGui(FromService:Boolean);
begin
//  AstaServerServiceForm.UserDS.DataSet        := UserdataSet;
//  AstaServerServiceForm.UserHistoryDS.DataSet := UserHistoryDataSet;
end;


procedure TServerDM.ToggleServerWireActive;
begin
  if not ServerWire.Active then begin
//    ServerWire.LogOn                 := AstaServerServiceForm.LogCheckBox.Checked ;
    ServerWire.Port                  := StrToIntDef(AstaServerServiceForm.PortEdit.Text, 9000);
//    ServerWire.Address               := AstaServerServiceForm.IpAddressEdit.Text;
//    ServerWire.TrustedAddresses.Text := AstaServerServiceForm.TrustedAddressesMemo.Text ;

    if AstaServerServiceForm.CompressionCheckBox.checked then
      ServerWire.Compression := acAstaZlib
    else
     ServerWire.Compression := acNoCompression ;
    if AstaServerServiceForm.EncryptCheckBox.Checked then
      ServerWire.SetDesStringKey(AstaServerServiceForm.DesEdit.Text)
    else
      ServerWire.Encryption := etNoEncryption;
  end;
  if ServerWire.Active and (ServerWire.UserList.Count > 0)
    and not MessageboxYes('There are connected users. ' + #13 +
      'ShutDown Server?') then
    exit;

  ServerWire.Active := not ServerWire.Active;

  if ServerWire.Active then begin
    ServerWire.RecordServerActivity(nil,'Server Started at '+DateTimeToStr(now));
    AstaServerServiceForm.Caption                 := 'ASTA Server serving on  port ' + IntToStr(ServerWire.Port);
    AstaServerServiceForm.ApplyButton.Caption     := 'Stop Server';
    AstaServerServiceForm.StatusBar.SimpleText    := 'Server Started at ' + DateTimeToStr(now) + ' AstaIO Version ' + ASTAIOVersion ;
    AstaServerServiceForm.SocketGroupBox.Visible  := False;
    ServerWire.RecordServerActivity(nil, 'Server Started ' + DateTimeToStr(now));
    if ServerWire.Encryption<>etNoEncryption then
      ServerWire.RecordServerActivity(nil, 'Running Encrypted');
  end else begin
    AstaServerServiceForm.Caption                := 'ASTA Server NOT Active';
    AstaServerServiceForm.ApplyButton.Caption    := 'Start Server';
    AstaServerServiceForm.StatusBar.SimpleText   := 'Server stopped at ' + DateTimeToStr(now);
    AstaServerServiceForm.SocketGroupBox.Visible := True;
    ServerWire.RecordServerActivity(nil, 'Server Stopped ' + DateTimeToStr(now));
  end;
end;

procedure TServerDM.DataModuleCreate(Sender: TObject);
begin
  Application.OnException := AstaException;
end;


Initialization
 ServerDM:=nil;
end.

