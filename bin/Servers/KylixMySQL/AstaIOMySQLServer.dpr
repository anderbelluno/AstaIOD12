program AstaIOMySQLServer;

uses
  QForms,
  u_main in 'u_main.pas' {f_main},
  dm_main in 'dm_main.pas' {AstaDataModule: TDataModule};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(Tf_main, f_main);
  Application.CreateForm(TAstaDataModule, AstaDataModule);
  Application.Run;
end.
