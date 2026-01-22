library AstaIOHttpADOServer;

uses
  WebBroker,
  ISAPIApp,
  main in 'main.pas' {AstaWebModule: TWebModule},
  AstaIOadoSupplementDM in 'AstaIOADOSupplementDM.pas' {AstaIOADODBPluginDM: TDataModule},
  dm in 'dm.pas' {AstaDataModule: TDataModule};

{$R *.RES}

exports
  GetExtensionVersion,
  HttpExtensionProc,
  TerminateExtension;

begin
  Application.Initialize;
  Application.CreateForm(TAstaDataModule, AstaDataModule);
  Application.CreateForm(TAstaWebModule, AstaWebModule);
  Application.Run;
end.
