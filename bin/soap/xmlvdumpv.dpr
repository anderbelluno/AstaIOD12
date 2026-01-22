program xmlvdumpv;

uses
  Forms,
  xmlv_dump in 'xmlv_dump.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
