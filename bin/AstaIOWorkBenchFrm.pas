{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10371: AstaIOWorkBenchFrm.pas 
{
{   Rev 1.0    4/10/2003 6:32:40 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:38:32 PM  Steve    Version: 1.505
}
unit AstaIOWorkBenchFrm;

interface

uses SysUtils, DB, Classes,
     Graphics, Controls, Forms, Dialogs,
     ToolWin, ComCtrls, ExtCtrls, StdCtrls, Grids,
     DBGrids, ActnList, ImgList, DBCtrls, CheckLst, Buttons,
     AstaIOMetadataTreeView,
     AstaIOCustomDataSet, 
     AstaIOClientRemoteDataSet, AstaIOSQLParser;

type
  TAstaCustomClientSQLDataSetHackHack = class(TAstaCustomClientSQLDataSet);

  TWorkBenchForm = class(TForm)
    tlb_main: TToolBar;
    pc_main: TPageControl;
    ts_properties: TTabSheet;
    ts_sql: TTabSheet;
    pan_sql_treeview: TPanel;
    tv_main: TAstaIOMetadataTreeView;
    spl_sql: TSplitter;
    pan_sql_right: TPanel;
    tlb_sql: TToolBar;
    btn_sql_cut: TToolButton;
    btn_exit: TToolButton;
    m_sql: TMemo;
    dbg_sql: TDBGrid;
    spl_data: TSplitter;
    btn_sql_copy: TToolButton;
    btn_sql_paste: TToolButton;
    btn_sql_delete: TToolButton;
    btn_sql_clear: TToolButton;
    ToolButton7: TToolButton;
    btn_sql_select: TToolButton;
    ToolButton1: TToolButton;
    btn_sql_savetofile: TToolButton;
    btn_sql_print: TToolButton;
    ToolButton4: TToolButton;
    im_sql: TImageList;
    al_main: TActionList;
    a_sql_cut: TAction;
    a_sql_copy: TAction;
    a_sql_paste: TAction;
    a_sql_delete: TAction;
    a_sql_clear: TAction;
    a_sql_select_all: TAction;
    a_sql_print: TAction;
    a_sql_save: TAction;
    a_sql_open: TAction;
    btn_sql_open: TToolButton;
    tlb_data: TToolBar;
    dbn_data: TDBNavigator;
    ToolButton3: TToolButton;
    btn_populate: TToolButton;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    DS_data: TDataSource;
    btn_sql_run: TToolButton;
    ToolButton6: TToolButton;
    a_sql_run: TAction;
    a_populate: TAction;
    a_exit: TAction;
    sb_data: TStatusBar;
    pc_properties: TPageControl;
    ts_fields: TTabSheet;
    ts_general: TTabSheet;
    gb_PrimeFields: TGroupBox;
    gb_RefetchFields: TGroupBox;
    clb_primefields: TCheckListBox;
    gb_NoSQLFields: TGroupBox;
    clb_nosqlfields: TCheckListBox;
    clb_refetchfields: TCheckListBox;
    btn_apply_fields: TBitBtn;
    a_apply_fields: TAction;
    l_AutoIncrementField: TLabel;
    l_Sequence: TLabel;
    cmb_AutoIncrementField: TComboBox;
    cmb_Sequence: TComboBox;
    q_data: TAstaIOClientQuery;
    procedure a_sql_cutExecute(Sender: TObject);
    procedure a_sql_copyExecute(Sender: TObject);
    procedure a_sql_pasteExecute(Sender: TObject);
    procedure a_sql_deleteExecute(Sender: TObject);
    procedure a_sql_clearExecute(Sender: TObject);
    procedure a_sql_select_allExecute(Sender: TObject);
    procedure a_sql_printExecute(Sender: TObject);
    procedure a_sql_saveExecute(Sender: TObject);
    procedure a_sql_openExecute(Sender: TObject);
    procedure a_sql_runExecute(Sender: TObject);
    procedure a_populateExecute(Sender: TObject);
    procedure a_exitExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure q_dataAfterOpen(DataSet: TDataSet);
    procedure a_apply_fieldsExecute(Sender: TObject);
  private
    SQLParser :TAstaIOSQLParser;
    FFirstTableName: String;
    procedure AssignFields;
  public
    DataSet :TComponent;
    NoSQLFields: TStrings;
    PrimeFields: TStrings;
    RefetchFields: TStrings;
    AutoIncrementField: String;
    Sequence: String;
    UpdateTableName: String;
  end;

var
  WorkBenchForm: TWorkBenchForm;

implementation
{$R *.dfm}

function RemoveCR(Value :String) :String;
var i   :Integer;
begin
  Result:='';
  for i:=1 to Length(Value) do
    if Ord(Value[i]) in [10, 13] then
      Result:=Result + ' '
    else
      Result:=Result + Value[i];
end;

function CreateList(TheList :TStrings) :String;
var i  :Integer;
begin
  Result:='';
  for i:=0 to TheList.Count - 1 do
    Result:=Result + TheList[i] + ',';
  if TheList.Count > 0 then
    Result:=Copy(Result, 1, Length(Result)-1);  
end;

procedure TWorkBenchForm.a_sql_cutExecute(Sender: TObject);
begin
  m_sql.CutToClipboard;
end;

procedure TWorkBenchForm.a_sql_copyExecute(Sender: TObject);
begin
  m_sql.CopyToClipboard;
end;

procedure TWorkBenchForm.a_sql_pasteExecute(Sender: TObject);
begin
  m_sql.PasteFromClipboard;
end;

procedure TWorkBenchForm.a_sql_deleteExecute(Sender: TObject);
begin
  m_sql.ClearSelection;
end;

procedure TWorkBenchForm.a_sql_clearExecute(Sender: TObject);
begin
  m_sql.Clear;
end;

procedure TWorkBenchForm.a_sql_select_allExecute(Sender: TObject);
begin
  m_sql.SelectAll;
end;

procedure TWorkBenchForm.a_sql_printExecute(Sender: TObject);
begin
//  m_sql.Lines.
end;

procedure TWorkBenchForm.a_sql_saveExecute(Sender: TObject);
begin
  if not SaveDialog.Execute then exit;
  m_sql.Lines.SaveToFile(SaveDialog.FileName);
end;

procedure TWorkBenchForm.a_sql_openExecute(Sender: TObject);
begin
  if not OpenDialog.Execute then exit;
  m_sql.Lines.LoadFromFile(OpenDialog.FileName);
end;

procedure TWorkBenchForm.a_sql_runExecute(Sender: TObject);
begin
  q_data.SQL.Assign(m_sql.Lines);

  // Do some parsing here
  SQLParser.SQL.Assign(m_sql.Lines);
  SQLParser.Deconstruct;

  if SQLParser.SQLStatementType = stSelect then
  begin
    if SQLParser.Tables.Count > 0 then
      FFirstTableName:=SQLParser.Tables[0];
      
    // Get prime fields and decide if the dataset must be readonly etc.

    q_data.Close;
    q_data.Open;
    if (UpperCase(FFirstTableName) <> UpperCase(UpdateTableName)) or (clb_PrimeFields.Items.Count = 0) then
      AssignFields;

    if UpperCase(FFirstTableName) = UpperCase(UpdateTableName) then
    begin
      cmb_AutoIncrementField.Text:=AutoIncrementField;
      cmb_Sequence.Text:=Sequence;
    end
    else
    begin
      cmb_AutoIncrementField.Text:='';
      cmb_Sequence.Text:='';
    end;
  end
  else
  begin
    q_data.Close;
    q_data.ExecSQL;
  end;

end;

procedure TWorkBenchForm.a_populateExecute(Sender: TObject);
var TableName :String;
    OrderBy,
    GroupBy,
    Having  :String;
begin
  TableName:='';
  SQLParser.SQL.Assign(m_sql.Lines);
  SQLParser.Deconstruct;
  if SQLParser.Tables.Count > 0 then
    TableName:=SQLParser.Tables[0];

  OrderBy:=CreateList(SQLParser.Order);
  GroupBy:=CreateList(SQLParser.Group);
  Having:=Trim(SQLParser.Having);

  // Do we need to do the UpdateTableName??
  TAstaCustomClientSQLDataSetHackHack(DataSet).UpdateTableName:=TableName;

  if DataSet is TAstaIOClientQuery then
  begin
    TAstaIOClientQuery(DataSet).SQL.Assign(m_sql.Lines);
  end
  else
  if DataSet is TAstaIOClientTable then
  begin

    TAstaIOClientTable(DataSet).SelectOptions.OrderBy:=OrderBy;
    TAstaIOClientTable(DataSet).SelectOptions.Having:=Having;
    TAstaIOClientTable(DataSet).SelectOptions.GroupBy:=GroupBy;
    TAstaIOClientTable(DataSet).TableName:=TableName;
  end;

  TAstaCustomClientSQLDataSetHackHack(DataSet).PrimeFields.Assign(PrimeFields);
  TAstaCustomClientSQLDataSetHackHack(DataSet).NoSQLFields.Assign(NoSQLFields);
  TAstaCustomClientSQLDataSetHackHack(DataSet).RefetchFields.Assign(RefetchFields);

  TAstaCustomClientSQLDataSetHackHack(DataSet).AutoIncrementField:=AutoIncrementField;
  TAstaCustomClientSQLDataSetHackHack(DataSet).Sequence:=Sequence;

end;

procedure TWorkBenchForm.a_exitExecute(Sender: TObject);
begin
  // Need to do more here....
  Close;
end;

procedure TWorkBenchForm.FormCreate(Sender: TObject);
begin
  SQLParser:=TAstaIOSQLParser.Create(nil);
  NoSQLFields:=TStringList.Create;
  PrimeFields:=TStringList.Create;
  RefetchFields:=TStringList.Create;
end;

procedure TWorkBenchForm.FormDestroy(Sender: TObject);
begin
  SQLParser.Free;
  NoSQLFields.Free;
  PrimeFields.Free;
  RefetchFields.Free;
end;

procedure TWorkBenchForm.FormActivate(Sender: TObject);
begin
  tv_main.Execute;
end;

procedure TWorkBenchForm.AssignFields;
var i    :Integer;
begin
  clb_PrimeFields.Clear;
  clb_NoSQLFields.Clear;
  clb_RefetchFields.Clear;

  cmb_AutoIncrementField.Items.Clear;
  cmb_Sequence.Items.Clear;

  for i:=0 to q_data.Fields.Count-1 do
  begin
    if q_data.Fields[i].DataType in [ftSmallInt, ftInteger, ftWord, ftLargeInt] then
    begin
      cmb_AutoIncrementField.Items.Add(q_data.Fields[i].FieldName);
      cmb_Sequence.Items.Add(q_data.Fields[i].FieldName);
    end;

    clb_PrimeFields.Items.Add(q_data.Fields[i].FieldName);
    if PrimeFields.IndexOf(q_data.Fields[i].FieldName) >= 0 then
      clb_PrimeFields.Checked[i]:=True;
  end;

  for i:=0 to q_data.Fields.Count-1 do
  begin
    clb_NoSQLFields.Items.Add(q_data.Fields[i].FieldName);
    if NoSQLFields.IndexOf(q_data.Fields[i].FieldName) >= 0 then
      clb_NoSQLFields.Checked[i]:=True;
  end;

  for i:=0 to q_data.Fields.Count-1 do
  begin
    clb_ReFetchFields.Items.Add(q_data.Fields[i].FieldName);
    if ReFetchFields.IndexOf(q_data.Fields[i].FieldName) >= 0 then
      clb_ReFetchFields.Checked[i]:=True;
  end;
end;

procedure TWorkBenchForm.q_dataAfterOpen(DataSet: TDataSet);
begin
  sb_data.Panels[0].Text:=IntToStr(q_data.RecordCount) + ' Rows';
end;

procedure TWorkBenchForm.a_apply_fieldsExecute(Sender: TObject);
var i    :Integer;
begin
  PrimeFields.Clear;
  NoSQLFields.Clear;
  RefetchFields.Clear;

  for i:=0 to clb_PrimeFields.Items.Count-1 do
  begin
    if clb_PrimeFields.Checked[i] then
      PrimeFields.Add(clb_PrimeFields.Items[i]);
  end;

  for i:=0 to clb_NoSQLFields.Items.Count-1 do
  begin
    if clb_NoSQLFields.Checked[i] then
      NoSQLFields.Add(clb_NoSQLFields.Items[i]);
  end;

  for i:=0 to clb_RefetchFields.Items.Count-1 do
  begin
    if clb_RefetchFields.Checked[i] then
      RefetchFields.Add(clb_RefetchFields.Items[i]);
  end;

  AutoIncrementField:=cmb_AutoIncrementField.Text;
  Sequence:=cmb_Sequence.Text;
end;

end.
