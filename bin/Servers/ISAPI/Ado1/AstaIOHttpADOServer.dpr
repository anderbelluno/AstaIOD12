library AstaIOHttpADOServer;

uses
  WebBroker,
  ISAPIApp,
  main in 'main.pas' {AstaModule: TWebModule};

{$R *.RES}

exports
  GetExtensionVersion,
  HttpExtensionProc,
  TerminateExtension;

begin
  Application.Initialize;
  Application.CreateForm(TAstaModule, AstaModule);
  Application.Run;
end.
