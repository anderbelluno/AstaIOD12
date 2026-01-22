unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Db, AstaIOCustomDataSet, AstaIOClientRemoteDataSet, AstaIOClientMsgWire,
  AstaIOClientWire, AstaIONativeClientWire,
  ExtCtrls, Grids,  StdCtrls,
  DBCtrls, AstaIODBConst, AstaIOParamList, AstaIOExecServerMethod, DBGrids,
  AstaIOClientIProvider;


type
  TForm1 = class(TForm)
    AstaIONativeClientWire: TAstaIONativeClientWire;
    AstaIOProviderDataSet: TAstaIOProviderDataSet;
    DBNavigator1: TDBNavigator;
    Panel1: TPanel;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    DBGrid1: TDBGrid;
    DataSource1: TDataSource;
    AstaIOExecServerMethod: TAstaIOExecServerMethod;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
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
   AstaIONativeClientWire.Active := not AstaIONativeClientWire.Active;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
   AstaIOExecServerMethod.Execute;
   AstaIOProviderDataSet.Active := not AstaIOProviderDataSet.Active;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
   AstaIOProviderDataSet.ApplyUpdates;
end;

procedure TForm1.FormClose(Sender: TObject; var Action: TCloseAction);
begin
   AstaIONativeClientWire.Active := False;
end;

end.
