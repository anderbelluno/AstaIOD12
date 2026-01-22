program AstaIOIBXServer;

uses
  Forms,
  u_main in 'u_main.pas' {f_main},
  dm_main in 'dm_main.pas' {AstaDataModule: TDataModule};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TAstaDataModule, AstaDataModule);
  Application.CreateForm(Tf_main, f_main);
  Application.Run;
end.
