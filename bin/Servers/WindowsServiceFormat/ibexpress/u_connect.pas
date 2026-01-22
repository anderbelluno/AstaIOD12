unit u_connect;

interface

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls, Dialogs, Mask, ComCtrls, ImgList, Db, Grids,
  DBGrids, DBCtrls, AstaIOCustomDataSet;

type
  Tfrm_connect = class(TForm)
    OpenDialog: TOpenDialog;
    PageControl1: TPageControl;
    pg_Connect: TTabSheet;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    OKBtn: TButton;
    CancelBtn: TButton;
    Label3: TLabel;
    Label2: TLabel;
    Label1: TLabel;
    e_username: TDBEdit;
    e_password: TDBEdit;
    btn_browse: TButton;
    DBGrid1: TDBGrid;
    DataSource1: TDataSource;
    Label6: TLabel;
    GroupBox1: TGroupBox;
    DBMemo1: TDBMemo;
    DBEdit1: TDBEdit;
    Label4: TLabel;
    DBMemo2: TDBMemo;
    DBEdit2: TDBEdit;
    Label5: TLabel;
    Panel4: TPanel;
    AliasDataSet: TAstaIODataSet;
    AliasDataSetAlias: TStringField;
    AliasDataSetPath: TMemoField;
    AliasDataSetServer: TStringField;
    AliasDataSetPassword: TStringField;
    AliasDataSetConnectionType: TSmallintField;
    AliasDataSetParams: TMemoField;
    AliasDataSetUser: TStringField;
    procedure btn_browseClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frm_connect: Tfrm_connect;

implementation

{$R *.DFM}

procedure Tfrm_connect.btn_browseClick(Sender: TObject);
begin
  if not OpenDialog.Execute then exit;
  if AliasDataSet.RecordCount=0 then AliasDataSet.Append
   else if not (AliasDataSet.State in [dsinsert,dsedit]) then AliasDataSet.Edit;
  AliasDataSet.FieldByName('Path').AsString:=OpenDialog.FileName;
end;

end.
