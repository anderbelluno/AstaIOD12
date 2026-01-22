unit AstaSoapTypeEditor;

interface
{$ifdef Ver150}
 {$define Delphi6AndUp}
{$endif}
{$ifdef Ver140}
 {$define Delphi6AndUp}
{$endif}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  AstaSoapMethod, ComCtrls, ToolWin, StdCtrls, ExtCtrls,
  AstaSoapParams, AstaSoapElmEditor,
  {$ifdef Delphi6andUp}
  DesignEditors, DesignIntf
  {$else}
  DsgnIntf
 {$endif}  ;

type
  TfrmAstaSoapTypeEditor = class(TForm)
    ToolBar1: TToolBar;
    tbnAddStruct: TToolButton;
    tbnDelete: TToolButton;
    lvwTypes: TListView;
    Panel2: TPanel;
    lblName: TLabel;
    edtName: TEdit;
    tbnClose: TToolButton;
    tbnAddArray: TToolButton;
    gbxArray: TGroupBox;
    rbnSimpleArray: TRadioButton;
    rbnComplexArray: TRadioButton;
    cbxSimpleTypes: TComboBox;
    cbxComplexTypes: TComboBox;
    gbxStruct: TGroupBox;
    btnEditStruct: TButton;
    procedure lvwTypesSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure tbnAddStructClick(Sender: TObject);
    procedure tbnDeleteClick(Sender: TObject);
    procedure tbnCloseClick(Sender: TObject);
    procedure btnEditStructClick(Sender: TObject);
    procedure tbnAddArrayClick(Sender: TObject);
    procedure lvwTypesKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure rbnSimpleArrayClick(Sender: TObject);
    procedure rbnComplexArrayClick(Sender: TObject);
    procedure edtNameChange(Sender: TObject);
    procedure cbxSimpleTypesChange(Sender: TObject);
    procedure cbxComplexTypesChange(Sender: TObject);
  private
    { Private declarations }
    Types: TAstaSoapComplexTypeList;
    FModified: Boolean;
    procedure SetTypeView(li: TListItem; ct: TAstaSoapComplexType);
    procedure LoadTypes;
    procedure SetComplexBox;
    procedure SetControlState;
  public
    { Public declarations }
    procedure Edit(ATypes: TAstaSoapComplexTypeList);
    property Modified: Boolean read FModified write FModified;
  end;

implementation

{$R *.DFM}

{ TfrmAstaSoapTypeEditor }

procedure TfrmAstaSoapTypeEditor.Edit(ATypes: TAstaSoapComplexTypeList);
var
  i: Integer;
begin
  gbxStruct.Top:= gbxArray.Top;
  Types:= ATypes;
  LoadTypes;
  cbxSimpleTypes.Clear;
  for i:= 0 to XDataTypes.Count - 1 do cbxSimpleTypes.Items.Add(XDataTypes[i].Name);
  SetControlState;
  Modified:= False;
  ShowModal;
end;

procedure TfrmAstaSoapTypeEditor.SetTypeView(li: TListItem; ct: TAstaSoapComplexType);
begin
  li.Caption:= ct.Name;
  li.Data:= ct;
  while li.SubItems.Count < 1 do li.SubItems.Add('');
  if ct.IsArray then li.SubItems[0]:= 'Array' else li.SubItems[0]:= 'Struct';
end;

procedure TfrmAstaSoapTypeEditor.LoadTypes;
var
  i: Integer;
begin
  lvwTypes.Items.BeginUpdate;
  try
    lvwTypes.Items.Clear;
    for i:= 0 to Types.Count - 1 do
      SetTypeView(lvwTypes.Items.Add, Types[i]);
  finally
    lvwTypes.Items.EndUpdate;
  end;
end;

procedure TfrmAstaSoapTypeEditor.SetComplexBox;
var
  i: Integer;
begin
  cbxComplexTypes.Clear;
  if lvwTypes.Selected = nil then Exit;
  for i:= 0 to Types.Count - 1 do
  begin
    if lvwTypes.Selected.Data <> Types[i] then
      cbxComplexTypes.Items.Add(Types[i].Name);
  end;
end;

procedure TfrmAstaSoapTypeEditor.SetControlState;
var
  fl: Boolean;
  ct: TAstaSoapComplexType;
begin
  fl:= lvwTypes.Selected <> nil;
  lblName.Visible:= fl;
  edtName.Visible:= fl;
  cbxSimpleTypes.Enabled:= rbnSimpleArray.Checked;
  cbxComplexTypes.Enabled:= rbnComplexArray.Checked;
  if fl then
  begin
    ct:= lvwTypes.Selected.Data;
    gbxStruct.Visible:= not ct.IsArray;
    gbxArray.Visible:= ct.IsArray;
  end
  else begin
    gbxStruct.Hide;
    gbxArray.Hide;
  end;
end;

procedure TfrmAstaSoapTypeEditor.lvwTypesSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
var
  ct: TAstaSoapComplexType;
  ModState: Boolean;
begin
  ModState:= Modified;
  SetComplexBox;
  if Selected then
  begin
    ct:= Item.Data;
    edtName.Text:= ct.Name;
    if ct.IsArray then
    begin
      rbnSimpleArray.Checked:= not ct.Elements[0].IsComplex;
      rbnComplexArray.Checked:= ct.Elements[0].IsComplex;
      cbxSimpleTypes.ItemIndex:= XDataTypes.IndexOf(ct.Elements[0].SimpleType);
      cbxComplexTypes.ItemIndex:= cbxComplexTypes.Items.IndexOf(ct.Elements[0].ComplexType);
    end;
  end;
  SetControlState;
  Modified:= ModState;  
end;

procedure TfrmAstaSoapTypeEditor.tbnAddStructClick(Sender: TObject);
var
  li: TListItem;
begin
  li:= lvwTypes.Items.Add;
  SetTypeView(li, Types.AddStruct('NewStruct'));
  li.Selected:= True;
  li.Focused:= True;
  Modified:= True;
end;

procedure TfrmAstaSoapTypeEditor.tbnAddArrayClick(Sender: TObject);
var
  li: TListItem;
begin
  li:= lvwTypes.Items.Add;
  SetTypeView(li, Types.AddArray('NewArray', xdString));
  li.Selected:= True;
  li.Focused:= True;
  Modified:= True;
end;

procedure TfrmAstaSoapTypeEditor.tbnDeleteClick(Sender: TObject);
var
  S: String;
  i: Integer;
begin
  if lvwTypes.Selected = nil then Exit;
  S:= 'Do you want to delete selected type?';
  if MessageDlg(S, mtConfirmation, [mbYes, mbNo], 0) <> mrYes then Exit;
  TObject(lvwTypes.Selected.Data).Free;
  i:= lvwTypes.Selected.Index;
  lvwTypes.Selected.Delete;
  if i < lvwTypes.Items.Count then lvwTypes.Items[i].Selected:= True
  else begin
    while i >= lvwTypes.Items.Count do Dec(i);
    if (i >= 0) and (lvwTypes.Items.Count > 0) then lvwTypes.Items[i].Selected:= True;
  end;
  Modified:= True;  
end;

procedure TfrmAstaSoapTypeEditor.lvwTypesKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if Key = VK_DELETE then begin tbnDeleteClick(self); Key:= 0; end;
end;

procedure TfrmAstaSoapTypeEditor.tbnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmAstaSoapTypeEditor.edtNameChange(Sender: TObject);
var
  ct: TAstaSoapComplexType;
begin
  if lvwTypes.Selected = nil then Exit;
  ct:= lvwTypes.Selected.Data;
  ct.Name:= edtName.Text;
  SetTypeView(lvwTypes.Selected, ct);
  Modified:= True;
end;

procedure TfrmAstaSoapTypeEditor.btnEditStructClick(Sender: TObject);
var
  ct: TAstaSoapComplexType;
  ElmEditor: TfrmAstaSoapElmEditor;
begin
  if lvwTypes.Selected = nil then Exit;
  ct:= lvwTypes.Selected.Data;
  ElmEditor:= TfrmAstaSoapElmEditor.Create(Application);
  try
    ElmEditor.Caption:= ct.Name;
    ElmEditor.Edit(ct.Elements, Types);
    if ElmEditor.Modified then Modified:= True;
  finally
    ElmEditor.Free;
  end;
end;

procedure TfrmAstaSoapTypeEditor.rbnSimpleArrayClick(Sender: TObject);
var
  ct: TAstaSoapComplexType;
begin
  if lvwTypes.Selected = nil then Exit;
  ct:= lvwTypes.Selected.Data;
  ct.Elements[0].IsComplex:= False;
  SetControlState;
  Modified:= True;  
end;

procedure TfrmAstaSoapTypeEditor.rbnComplexArrayClick(Sender: TObject);
var
  ct: TAstaSoapComplexType;
begin
  if lvwTypes.Selected = nil then Exit;
  ct:= lvwTypes.Selected.Data;
  ct.Elements[0].IsComplex:= True;
  SetControlState;
  Modified:= True;
end;

procedure TfrmAstaSoapTypeEditor.cbxSimpleTypesChange(Sender: TObject);
var
  ct: TAstaSoapComplexType;
begin
  if lvwTypes.Selected = nil then Exit;
  ct:= lvwTypes.Selected.Data;
  ct.Elements[0].SimpleType:= XDataTypes[cbxSimpleTypes.ItemIndex].Datatype;
  Modified:= True;
end;

procedure TfrmAstaSoapTypeEditor.cbxComplexTypesChange(Sender: TObject);
var
  ct: TAstaSoapComplexType;
begin
  if lvwTypes.Selected = nil then Exit;
  ct:= lvwTypes.Selected.Data;
  ct.Elements[0].ComplexType:= cbxComplexTypes.Text;
  Modified:= True;  
end;

end.
