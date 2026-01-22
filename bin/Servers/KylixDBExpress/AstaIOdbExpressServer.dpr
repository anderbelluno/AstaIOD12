program AstaIOdbExpressServer;

uses
  QForms,
  dm_main in 'dm_main.pas' {AstaDataModule: TDataModule},
  u_main in 'u_main.pas' {f_main};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TAstaDataModule, AstaDataModule);
  Application.CreateForm(Tf_main, f_main);
  Application.Run;
end.
