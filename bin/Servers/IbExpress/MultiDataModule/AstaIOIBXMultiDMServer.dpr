program AstaIOIBXMultiDMServer;

uses
  Forms,
  u_main in 'u_main.pas' {f_main},
  dm2 in 'dm2.pas' {AstaExtraDataModule: TDataModule},
  dm_main in '..\dm_main.pas' {AstaDataModule: TDataModule};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TAstaDataModule, AstaDataModule);
  Application.CreateForm(TAstaExtraDataModule, AstaExtraDataModule);
  Application.CreateForm(Tf_main, f_main);
  Application.Run;
end.
