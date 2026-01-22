program AstaIODOAServer;

uses
  Forms,
  u_main in 'u_main.pas' {f_main},
  dm_main in 'dm_main.pas' {AstaDataModule: TDataModule};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(Tf_main, f_main);
  Application.CreateForm(TAstaDataModule, AstaDataModule);
  Application.Run;
end.
