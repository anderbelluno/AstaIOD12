unit Unit1;

interface

uses
  Windows, Messages, SysUtils, {$ifdef Ver140}Variants, {$endif}Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, DB, AstaIOCustomDataSet, AstaIOClientRemoteDataSet,
  Grids, DBGrids, AstaIOClientMsgWire, AstaIOClientWire,
  AstaIONativeClientWire, AstaIOLowCore, ComCtrls, AstaIOStatusBar;

type
  TForm1 = class(TForm)
    AstaIONativeClientWire1: TAstaIONativeClientWire;
    DataSource1: TDataSource;
    DBGrid1: TDBGrid;
    Provider: TAstaIOProviderDataSet;
    Button1: TButton;
    AstaIOStatusBar1: TAstaIOStatusBar;
    Label1: TLabel;
    Edit1: TEdit;
    Button2: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
Provider.ApplyUpdates;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
with Provider do begin
 ParamByName('cust_no').AsInteger:=StrToIntDef(Edit1.text,1001);
 close;
 Open;
end
end;

end.
