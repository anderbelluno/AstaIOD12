unit mainunit;

interface

uses
  SvcMgr, Windows, Messages, SysUtils, Classes, Graphics, Controls,
  Dialogs, Menus, ShellAPI, ExtCtrls, StdCtrls, ComCtrls, Registry, Db,
  Grids, DBGrids,  CheckLst,  AstaIOServiceUtils;

type
  TRegServerSettings = record
    UseCompression : Boolean ;
    AutoStart : Boolean;
    Port : integer;
    IPAddress: string;
    Sessions : integer;
    RemotePort : integer;
    UseRemoteControl: Boolean;
    UseEncryption : boolean;
    DesText : string;
  end;

  TAstaIOIBObjectsServiceForm = class(TAstaIOServiceForm)
  private
    ////
  protected
    ////////////
    function ConnectedClients: Boolean; override;
    procedure ReadSettings; override;
  public
    RegserverSettings : TRegServerSettings;
    procedure Initialize(FromService: Boolean); override;
  end;


implementation
uses SocketDM, AstaIOUtil, AstaIOIBObjectsSupplement, dm;


{ TAstaIOIBObjectsServiceForm }


procedure TAstaIOIBObjectsServiceForm.Initialize(FromService: Boolean);
begin
  inherited Initialize(FromService);
  ServerDM := TServerDM.Create(Application);
  ServerDM.FService := FromService; {RJB - gotta have this!}
  ServerDM.ServerWire.Port := RegServerSettings.Port;
  if RegServerSettings.AutoStart then 
    ServerDM.ToggleServerSocketActive;
end;

function TAstaIOIBObjectsServiceForm.ConnectedClients: Boolean;
begin
  result := (ServerDM <> nil) and (ServerDm.ServerWire.UserList.Count > 0);
end;


procedure TAstaIOIBObjectsServiceForm.ReadSettings;
var
  Reg: TRegINIFile;
begin
  Reg := AstaIOServiceRegIniFile;
  try
    RegServerSettings.UseCompression := Reg.ReadBool('Server', 'Compression', False);
    RegServerSettings.AutoStart := Reg.ReadBool('Server', 'AutoStart', True);
    RegServerSettings.Port := Reg.ReadInteger('Server', 'Port', 9050);
    RegServerSettings.IPAddress := Reg.ReadString('Server', 'IpAddress', GetThePcsIpAddress);
    RegServerSettings.Sessions := Reg.ReadInteger('Server', 'DatabaseSessions', 3);
    RegServerSettings.RemotePort := Reg.ReadInteger('Server', 'RemotePort', 12000);
    RegServerSettings.UseRemoteControl := Reg.ReadBool('Server', 'RemoteControl', False);
    RegServerSettings.UseEncryption := Reg.ReadBool('Server', 'Encryption', False);
    RegServerSettings.DesText := Reg.ReadString('Server', 'DESKey', '');
  finally
    Reg.Free;
  end;
end;


end.

