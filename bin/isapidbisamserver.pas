{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10391: isapidbisamserver.pas 
{
{   Rev 1.0    4/10/2003 6:32:50 AM  Steve
}
unit isapidbisamserver;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Db, AstaIOCustomDataSet, AstaIOClientRemoteDataSet, Grids,
  DBGrids, AstaIOClientMsgWire, AstaIOClientWire, AstaIOWebClientWire;

type
  TForm1 = class(TForm)
    Clientwire: TAstaIOWebClientWire;
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    Query: TAstaIOClientQuery;
    Memo1: TMemo;
    Button1: TButton;
    Label1: TLabel;
    Edit1: TEdit;
    Label2: TLabel;
    Button2: TButton;
    Memo2: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.DFM}

procedure TForm1.Button1Click(Sender: TObject);
begin
query.close;
query.UpdateTablename:=Edit1.Text;
query.PrimeFields.Text:=Memo2.text;
query.sql.text:=memo1.text;
query.open;

end;

procedure TForm1.FormCreate(Sender: TObject);
begin
clientWire.Active:=True;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
query.ApplyUpdates;
end;

end.
