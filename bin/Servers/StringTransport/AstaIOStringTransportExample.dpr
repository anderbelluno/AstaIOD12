{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10400: AstaIOStringTransportExample.dpr 
{
{   Rev 1.0    4/10/2003 6:34:00 AM  Steve
}
program AstaIOStringTransportExample;

uses
  Forms,
  Unit1 in 'Unit1.pas' {Form1},
  StringDM in 'StringDM.pas' {AstaStringDataModule: TDataModule},
  dbisam_main in 'dbisam_main.pas' {AstaDbisamDataModule: TDataModule};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TAstaStringDataModule, AstaStringDataModule);
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
