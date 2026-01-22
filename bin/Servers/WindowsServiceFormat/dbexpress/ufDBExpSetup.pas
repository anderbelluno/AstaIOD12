unit ufDBExpSetup;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DB, AstaIOCustomDataSet, DBXpress, StdCtrls, SqlExpr, ExtCtrls;

type
  TfrmSetupDBExpressConnection = class(TForm)
    AstaIODataSet: TAstaIODataSet;
    SQLConnection1: TSQLConnection;
    GroupBox: TGroupBox;
    LblConnectionName: TLabel;
    CbxConnectionName: TComboBox;
    LblDriverName: TLabel;
    CbxDriverName: TComboBox;
    lblLibName: TLabel;
    edtLibName: TEdit;
    LblGetDriverFunc: TLabel;
    edtGetDrvFunc: TEdit;
    LblVendorLib: TLabel;
    Edit1: TEdit;
    Button1: TButton;
    Panel1: TPanel;
    ButtonApply: TButton;
    ButtonCancel: TButton;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  frmSetupDBExpressConnection: TfrmSetupDBExpressConnection;

implementation

{$R *.dfm}

end.
