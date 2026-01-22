unit AstaSoapElmEditor;

interface

uses 
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls, Contnrs, ToolWin, AstaSoapParams,
  AstaSoapMethod;

type
  TfrmAstaSoapElmEditor = class(TForm)
    lvwElements: TListView;
    Panel1: TPanel;
    lblName: TLabel;
    edtName: TEdit;
    rbnSimple: TRadioButton;
    rbnComplex: TRadioButton;
    cbxSimple: TComboBox;
    cbxComplex: TComboBox;
    ToolBar1: TToolBar;
    tbnAdd: TToolButton;
    tbnDelete: TToolButton;
    tbnClose: TToolButton;
    procedure tbnAddClick(Sender: TObject);
    procedure tbnDeleteClick(Sender: TObject);
    procedure tbnCloseClick(Sender: TObject);
    procedure lvwElementsKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure lvwElementsSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure edtNameChange(Sender: TObject);
    procedure rbnSimpleClick(Sender: TObject);
    procedure rbnComplexClick(Sender: TObject);
    procedure cbxSimpleChange(Sender: TObject);
    procedure cbxComplexChange(Sender: TObject);
  private
    { Private declarations }
    Elements: TAstaSoapMethodElementList;
    FModified: Boolean;
    procedure SetElementView(el: TAstaSoapMethodElement; li: TListItem);
    procedure SetControlState;
    procedure LoadParamTypes(ATypes: TAstaSoapComplexTypeList);
    procedure LoadElements;
    procedure AddElement;
    procedure DeleteElement;
  public
    { Public declarations }
    procedure Edit(AElements: TAstaSoapMethodElementList; ATypes: TAstaSoapComplexTypeList);
    property Modified: Boolean read FModified write FModified;
  end;

  TXDatatypeName = class
  private
    FDatatype: TXmlDatatype;
    FName: String;
  public
    property Datatype: TXmlDatatype read FDatatype write FDatatype;
    property Name: String read FName write FName;
  end;

  TXDatatypeNameList = class(TObjectList)
  private
    procedure AddType(Datatype: TXmlDatatype; Name: String);
    function GetItem(Index: Integer): TXDatatypeName;
  public
    constructor Create;
    function IndexOf(Datatype: TXmlDatatype): Integer;

    property Items[Index: Integer]: TXDatatypeName read GetItem; default;
  end;

var
  XDataTypes: TXDatatypeNameList;

implementation

{$R *.DFM}

{ TfrmAstaSoapElmEditor }

procedure TfrmAstaSoapElmEditor.Edit(AElements: TAstaSoapMethodElementList;
    ATypes: TAstaSoapComplexTypeList);
begin
  Elements:= AElements;
  LoadParamTypes(ATypes);
  LoadElements;
  SetControlState;
  Modified:= False;
  ShowModal;
end;

procedure TfrmAstaSoapElmEditor.LoadParamTypes(ATypes: TAstaSoapComplexTypeList);
var
  i: Integer;
begin
  cbxComplex.Clear;
  if ATypes <> nil then
    for i:= 0 to ATypes.Count - 1 do
      cbxComplex.Items.Add(ATypes[i].Name);

  cbxSimple.Clear;
  for i:= 0 to XDataTypes.Count - 1 do cbxSimple.Items.Add(XDataTypes[i].Name);
end;

procedure TfrmAstaSoapElmEditor.SetControlState;
var
  fl: Boolean;
begin
  fl:= lvwElements.Selected <> nil;
  lblName.Enabled:= fl;
  edtName.Enabled:= fl;
  rbnSimple.Enabled:= fl;
  cbxSimple.Enabled:= fl and rbnSimple.Checked;
  rbnComplex.Enabled:= fl;
  cbxComplex.Enabled:= fl and rbnComplex.Checked;
end;

procedure TfrmAstaSoapElmEditor.SetElementView(el: TAstaSoapMethodElement; li: TListItem);
begin
  li.Caption:= el.Name;
  while li.SubItems.Count < 1 do li.SubItems.Add('');
  if el.IsComplex then li.SubItems[0]:= el.ComplexType
  else li.SubItems[0]:= XDataTypes.Items[XDataTypes.IndexOf(el.SimpleType)].Name;
end;

procedure TfrmAstaSoapElmEditor.LoadElements;
var
  i: Integer;
  li: TListItem;
  el: TAstaSoapMethodElement;
begin
  lvwElements.Items.BeginUpdate;
  try
    lvwElements.Items.Clear;
    for i:= 0 to Elements.Count - 1 do
    begin
      el:= Elements[i];
      li:= lvwElements.Items.Add;
      SetElementView(el, li);
      li.Data:= el;
    end;
  finally
    lvwElements.Items.EndUpdate;
  end;
end;

procedure TfrmAstaSoapElmEditor.AddElement;
var
  li: TListItem;
  el: TAstaSoapMethodElement;
begin
  el:= Elements.AddSimpleElement('NewElement', xdString);
  li:= lvwElements.Items.Add;
  li.Caption:= el.Name;
  li.Data:= el;
  li.Selected:= True;
  li.Focused:= True;
  Modified:= True;
end;

procedure TfrmAstaSoapElmEditor.DeleteElement;
var
  S: String;
  i: Integer;
begin
  if lvwElements.Selected = nil then Exit;
  S:= 'Do you want to delete selected element?';
  if MessageDlg(S, mtConfirmation, [mbYes, mbNo], 0) <> mrYes then Exit;
  TObject(lvwElements.Selected.Data).Free;
  i:= lvwElements.Selected.Index;
  lvwElements.Selected.Delete;
  if i < lvwElements.Items.Count then lvwElements.Items[i].Selected:= True
  else begin
    while i >= lvwElements.Items.Count do Dec(i);
    if (i >= 0) and (lvwElements.Items.Count > 0) then lvwElements.Items[i].Selected:= True;
  end;
  Modified:= True;  
end;

procedure TfrmAstaSoapElmEditor.tbnAddClick(Sender: TObject);
begin
  AddElement;
end;

procedure TfrmAstaSoapElmEditor.tbnDeleteClick(Sender: TObject);
begin
  DeleteElement;
end;

procedure TfrmAstaSoapElmEditor.tbnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmAstaSoapElmEditor.lvwElementsKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  if Key = VK_Delete then begin tbnDeleteClick(self); Key:= 0; end;
end;

procedure TfrmAstaSoapElmEditor.lvwElementsSelectItem(Sender: TObject;
  Item: TListItem; Selected: Boolean);
var
  el: TAstaSoapMethodElement;
  ModState: Boolean;
begin
  ModState:= Modified;
  SetControlState;
  if Selected then
  begin
    el:= Item.Data;
    edtName.Text:= el.Name;
    if el.IsComplex then
    begin
      rbnComplex.Checked:= True;
    end
    else begin
      rbnSimple.Checked:= True;    
    end;
    cbxSimple.ItemIndex:= XDataTypes.IndexOf(el.SimpleType);
    cbxComplex.ItemIndex:= cbxComplex.Items.IndexOf(el.ComplexType);
  end
  else begin
    edtName.Text:= '';
    rbnSimple.Checked:= True;
    cbxSimple.ItemIndex:= -1;
    cbxComplex.ItemIndex:= -1;
  end;
  Modified:= ModState;    
end;

procedure TfrmAstaSoapElmEditor.edtNameChange(Sender: TObject);
var
  el: TAstaSoapMethodElement;
begin
  if lvwElements.Selected = nil then Exit;
  el:= lvwElements.Selected.Data;
  el.Name:= edtName.Text;
  SetElementView(el, lvwElements.Selected);
  Modified:= True;
end;

procedure TfrmAstaSoapElmEditor.rbnSimpleClick(Sender: TObject);
var
  el: TAstaSoapMethodElement;
begin
  if lvwElements.Selected = nil then Exit;
  el:= lvwElements.Selected.Data;
  el.IsComplex:= False;
  SetElementView(el, lvwElements.Selected);
  SetControlState;
  Modified:= True;
end;

procedure TfrmAstaSoapElmEditor.rbnComplexClick(Sender: TObject);
var
  el: TAstaSoapMethodElement;
begin
  if lvwElements.Selected = nil then Exit;
  el:= lvwElements.Selected.Data;
  el.IsComplex:= True;
  SetElementView(el, lvwElements.Selected);
  SetControlState;
  Modified:= True;
end;

procedure TfrmAstaSoapElmEditor.cbxSimpleChange(Sender: TObject);
var
  el: TAstaSoapMethodElement;
begin
  if lvwElements.Selected = nil then Exit;
  el:= lvwElements.Selected.Data;
  el.SimpleType:= XDataTypes[cbxSimple.ItemIndex].Datatype;
  SetElementView(el, lvwElements.Selected);
  Modified:= True;
end;

procedure TfrmAstaSoapElmEditor.cbxComplexChange(Sender: TObject);
var
  el: TAstaSoapMethodElement;
begin
  if lvwElements.Selected = nil then Exit;
  el:= lvwElements.Selected.Data;
  el.ComplexType:= cbxComplex.Text;
  SetElementView(el, lvwElements.Selected);
  Modified:= True;
end;

{ TXDatatypeNameList }

constructor TXDatatypeNameList.Create;
begin
  inherited;
  AddType(xdString, 'String');
  AddType(xdBoolean, 'Boolean');
  AddType(xdInteger, 'Integer');
  AddType(xdDecimal, 'Decimal');
  AddType(xdFloat, 'Float');
  AddType(xdDouble, 'Double');
  AddType(xdDateTime, 'DateTime');
  AddType(xdTime, 'Time');
  AddType(xdDate, 'Date');
  AddType(xdDuration, 'Duration');
  AddType(xdYearMonth, 'Year and Month');
  AddType(xdYear, 'Year');
  AddType(xdMonthDay, 'Month and Day');
  AddType(xdDay, 'Day');
  AddType(xdMonth, 'Month');
  AddType(xdHexBinary, 'Hex-Encoding Binary');
  AddType(xdBase64Binary, 'Base64-Encoding Binary');
end;

procedure TXDatatypeNameList.AddType(Datatype: TXmlDatatype; Name: String);
var
  dt: TXDatatypeName;
begin
  dt:= TXDatatypeName.Create;
  dt.Datatype:= Datatype;
  dt.Name:= Name;
  Add(dt);
end;

function TXDatatypeNameList.GetItem(Index: Integer): TXDatatypeName;
begin
  Result:= inherited Items[Index] as TXDatatypeName;
end;

function TXDatatypeNameList.IndexOf(Datatype: TXmlDatatype): Integer;
var
  i: Integer;
begin
  Result:= -1;
  for i:= 0 to Count - 1 do
    if Datatype = Items[i].Datatype then
    begin
      Result:= i;
      Break;
    end;
end;


initialization
  if XDataTypes = nil then XDataTypes:= TXDatatypeNameList.Create;

finalization
  XDataTypes.Free;
  XDataTypes:= nil;

end.
