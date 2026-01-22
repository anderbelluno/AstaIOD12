{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10139: AstaIODisconnectedMsgClient.dpr 
{
{   Rev 1.0    4/10/2003 6:30:48 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:37:10 PM  Steve    Version: 1.505
}
program AstaIODisconnectedMsgClient;

uses
  Forms,
  clientUnit in 'AstaNative\Demo\DisconnectedMessage\clientUnit.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
