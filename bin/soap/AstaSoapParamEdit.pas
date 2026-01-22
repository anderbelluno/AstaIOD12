unit AstaSoapParamEdit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  DsgnIntf, AstaSoapMethod, ComCtrls, ToolWin, StdCtrls, ExtCtrls,
  AstaSoapParams, AstaSoapElmEdit;

type
  TAstaSoapParamEditor = class(TComponentEditor)
  public
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): string; override;
    procedure ExecuteVerb(Index: Integer); override;
    procedure Edit; override;
  end;

  TfrmAstaSoapParamEdit = class(TForm)
    pnlTop: TPanel;
    pnlClient: TPanel;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    Panel2: TPanel;
    edtComplexTypeName: TEdit;
    rbnStruct: TRadioButton;
    rbnArray: TRadioButton;
    Label2: TLabel;
    aseInputParams: TfrmAstaSoapElmEdit;
    ToolBar1: TToolBar;
    tbnAdd: TToolButton;
    tbnDelete: TToolButton;
    lvwComplexTypes: TListView;
    aseStruct: TfrmAstaSoapElmEdit;
    aseOutputParams: TfrmAstaSoapElmEdit;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure rbnStructClick(Sender: TObject);
    procedure lvwComplexTypesSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure tbnAddClick(Sender: TObject);
    procedure tbnDeleteClick(Sender: TObject);
  private
    { Private declarations }
    Method: TAstaSoapMethod;
    procedure SetComplexTypeView(li: TListItem; ct: TAstaSoapComplexType);
    procedure LoadComplexTypes;
  public
    { Public declarations }
    constructor CreateEditor(AMethod: TAstaSoapMethod);
    procedure SetMethod(AMethod: TAstaSoapMethod);
  end;

var
  frmAstaSoapParamEdit: TfrmAstaSoapParamEdit;

implementation

{$R *.DFM}

{ TAstaSoapParamEditor }

function TAstaSoapParamEditor.GetVerbCount: Integer;
begin
  Result:= 1;
end;

function TAstaSoapParamEditor.GetVerb(Index: Integer): string;
begin
  if Index = 0 then Result:= 'Edit Parameters...' else Result:= '';
end;

procedure TAstaSoapParamEditor.ExecuteVerb(Index: Integer);
begin
  if Index <> 0 then Exit;
  with TfrmAstaSoapParamEdit.CreateEditor(Component as TAstaSoapMethod) do
  begin
    Show;
  end;
end;

procedure TAstaSoapParamEditor.Edit;
begin
  ExecuteVerb(0);
end;


{ TfrmAstaSoapParamEdit }

constructor TfrmAstaSoapParamEdit.CreateEditor(AMethod: TAstaSoapMethod);
begin
  inherited Create(Application);
  SetMethod(AMethod);
end;

procedure TfrmAstaSoapParamEdit.FormClose(Sender: TObject;  var Action: TCloseAction);
begin
  Action:= caFree;
end;

procedure TfrmAstaSoapParamEdit.SetMethod(AMethod: TAstaSoapMethod);
begin
  Method:= AMethod;
  Caption:= 'Editing ' + Method.Owner.Name + '.' + Method.Name;
  aseInputParams.LoadParamTypes(Method.ComplexTypes);
  aseInputParams.SetElements(Method.InputParams);
  aseOutputParams.LoadParamTypes(Method.ComplexTypes);
  aseOutputParams.SetElements(Method.OutputParams);
  LoadComplexTypes;
end;

procedure TfrmAstaSoapParamEdit.rbnStructClick(Sender: TObject);
begin
  aseStruct.Visible:= rbnStruct.Checked;
end;

procedure TfrmAstaSoapParamEdit.SetComplexTypeView(li: TListItem; ct: TAstaSoapComplexType);
begin
  li.Caption:= ct.Name;
  li.Data:= ct;

end;

procedure TfrmAstaSoapParamEdit.LoadComplexTypes;
var
  i: Integer;
begin
  lvwComplexTypes.Items.BeginUpdate;
  try
    lvwComplexTypes.Items.Clear;
    for i:= 0 to Method.ComplexTypes.Count - 1 do
      SetComplexTypeView(lvwComplexTypes.Items.Add, Method.ComplexTypes[i]);
  finally
    lvwComplexTypes.Items.EndUpdate;
  end;
end;

procedure TfrmAstaSoapParamEdit.lvwComplexTypesSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
var
  ct: TAstaSoapComplexType;
begin
  if Selected then
  begin
    ct:= Item.Data;
    edtComplexTypeName.Text:= ct.Name;
    rbnStruct.Checked:= not ct.IsArray;
    rbnArray.Checked:= ct.IsArray;
    aseStruct.SetElements(ct.Elements);
  end
  else begin
    edtComplexTypeName.Text:= '';
    rbnStruct.Checked:= False;
    rbnArray.Checked:= False;
  end;
end;

procedure TfrmAstaSoapParamEdit.tbnAddClick(Sender: TObject);
begin
  SetComplexTypeView(lvwComplexTypes.Items.Add,
      Method.ComplexTypes.AddType('ComplexType'));
end;

procedure TfrmAstaSoapParamEdit.tbnDeleteClick(Sender: TObject);
var
  S: String;
  i: Integer;
begin
  if lvwComplexTypes.Selected = nil then Exit;
  S:= 'Do you want to delete selected type?';
  if MessageDlg(S, mtConfirmation, [mbYes, mbNo], 0) <> mrYes then Exit;
  TObject(lvwComplexTypes.Selected.Data).Free;
  i:= lvwComplexTypes.Selected.Index;
  lvwComplexTypes.Selected.Delete;
  if i < lvwComplexTypes.Items.Count then lvwComplexTypes.Items[i].Selected:= True
  else begin
    while i >= lvwComplexTypes.Items.Count do Dec(i);
    if (i >= 0) and (lvwComplexTypes.Items.Count > 0) then lvwComplexTypes.Items[i].Selected:= True;
  end;
end;

end.
