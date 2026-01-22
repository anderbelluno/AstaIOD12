unit clientunit;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, DB, Grids, DBGrids, AstaIOCustomDataSet,
  AstaIOClientRemoteDataSet, AstaIOClientMsgWire, AstaIOClientWire,
  AstaIONativeClientWire;

type
  TForm1 = class(TForm)
    AstaIONativeClientWire1: TAstaIONativeClientWire;
    Prov: TAstaIOProviderDataSet;
    DBGrid1: TDBGrid;
    DataSource1: TDataSource;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
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
prov.applyUpdates();
end;

end.
