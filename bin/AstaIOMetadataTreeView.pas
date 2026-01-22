{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10243: AstaIOMetadataTreeView.pas 
{
{   Rev 1.0    4/10/2003 6:31:38 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:37:48 PM  Steve    Version: 1.505
}
unit AstaIOMetadataTreeView;
{*********************************************************}
{*   Copyright (c) 1997-2002 Asta Technology Group Inc.  *}
{*               All rights reserved.                    *}
{*               www.astatech.com                        *}
{*********************************************************}

{$I AstaIO.inc}

interface

uses DB, SysUtils, Classes, TypInfo,
{$IFDEF LINUX}
  QDialogs, QForms, QStdCtrls, QControls, QComCtrls,
{$ELSE}
  Forms, StdCtrls, Controls, ComCtrls, Dialogs,
{$ENDIF}
  AstaIODBConst,
  AstaIOMetadataListView,
  AstaIOClientWire,
  AstaIOClientRemoteDataSet;

const
{$IFDEF LINUX}
  FirstNodeLevel = 1;
{$ELSE}
  FirstNodeLevel = 0;
{$ENDIF}

type
  TAstaIOMetadataTreeView = class(TTreeView)
  private
    FAbout: String;
    FExecCursor: TCursor;
    FUseExecCursor: Boolean;
    FClientWire: TAstaIOClientWire;
    AInfo: TAstaIOMetaDataDataSet;
    FCursor: TCursor;
    FCurrentDbName: string;
    FSelectedName: string;
    FSingleSelected: Boolean;
    FSelObjectType: TAstaMetaData;
    FMetadataListView: TAstaIOMetadataListView;
    FIsExpanding: Boolean;
    procedure OnExpandingProc(Sender: TObject;
      Node: TTreeNode;
      var AllowExpansion: Boolean);
    {procedure OnMouseDownProc(Sender: TObject;
      Button: TMouseButton;
      Shift: TShiftState;
      X, Y: Integer);}
  protected
    DataBaseNames: TStrings;
    procedure GetIProviders(TheNode: TTreeNode);
    procedure GetParams(TheNode: TTreeNode; ObjectName: string; MetadataReq: TAstaMetadata);

    procedure GetServerMethods(TheNode: TTreeNode);
    procedure GetExecServerMethods(TheNode: TTreeNode);
    procedure GetProviders(TheNode: TTreeNode);
    procedure GetTables(TheNode: TTreeNode; MetaDataRequest: TAstaMetaData);
    procedure GetTriggers(TheNode: TTreeNode);
    procedure GetTableTriggers(TheNode: TTreeNode; TableName: string);
    procedure GetStoredProcedures(TheNode: TTreeNode);
    procedure GetuserList(TheNode: TTreeNode);
    procedure GetViews(TheNode: TTreeNode);
    procedure GetDBMSNames(TheNode: TTreeNode);
    procedure GetFields(TheNode: TTreeNode; TableName: string);
    procedure GetPrimeFields(TheNode: TTreeNode; TableName: string);
    procedure GetIndexes(TheNode: TTreeNode; TableName: string);
    procedure GetProcColumns(TheNode: TTreeNode; StoredProcedure: string);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    FBiz_DataModule,
      FBiz_Method,
      FBiz_Param_Name,
      FBiz_Param_ParamType,
      FBiz_Param_DataType,

    FPrv_DataModule,
      FPrv_Provider,
      FPrv_Param_Name,
      FPrv_Param_ParamType,
      FPrv_Param_DataType,

    FIPrv_Provider,
      FIPrv_Param_Name,
      FIPrv_Param_ParamType,
      FIPrv_Param_DataType,

    FSM_DataSet,
      FSM_Class,

    FDBMSName,

    FVCL_FieldName,
      FVCL_DataType,
      FVCL_Size,

    FSP_Name,
      FSP_ColName,
      FSP_InputType,
      FSP_DataType,

    FTab_Name,
      FTrigger_Name,
      FTrigger_Relation_Name,
      FSTab_Name,
      FVew_Name,
      FUsr_Name,
      FIdx_Name: string;
    constructor Create(AOwner: tComponent); override;
    destructor Destroy; override;
    property CurrentDbName: string read FCurrentDbName write FCurrentDbName;
    property SelectedName: string read FSelectedName write FSelectedName;
    property SingleSelected: Boolean read FSingleSelected;
    procedure Execute;
    procedure DblClick; override;
    procedure Click; override;
    procedure Clear;
    procedure CollapseSelected;
    property SelObjectType: TAstaMetaData read FSelObjectType write FSelObjectType;
  published
    property About: String read FAbout write FAbout;
    property ExecCursor: TCursor read FExecCursor write FExecCursor;
    property UseExecCursor: Boolean read FUseExecCursor write FUseExecCursor default True;
    property ClientWire: TAstaIOClientWire read FClientWire write FClientWire;
    property MetadataListView: TAstaIOMetadataListView read FMetadataListView write FMetadataListView;
  end;

implementation
uses AstaIOParamList;

function NameOnly(Value: string): string;
var
  idx: Integer;
begin
  Result := Value;
  idx := Pos('.', Value);
  if idx > 0 then
    Result := System.Copy(Value, idx + 1, Length(Value) - idx);
end;

{ TAstaIOMetadataTreeView }

procedure TAstaIOMetadataTreeView.Clear;
begin
  Items.BeginUpdate;
  Items.Clear;
  Items.EndUpdate;
  AInfo.Close;
end;

procedure TAstaIOMetadataTreeView.Click;
begin
  FSelObjectType := mdNormalQuery;
  FSingleSelected := False;
  if not Assigned(Selected) then
  begin
    inherited;
    exit;
  end;
  FSelectedName := Selected.Text;

  DblClick;

  if not FSingleSelected and not FIsExpanding then
  begin
    if Assigned(FMetadataListView) then
    begin
      if FMetadataListView.ClientWire = nil then
        FMetadataListView.ClientWire := ClientWire;

      FMetadataListView.SelObjectType := FSelObjectType;
      FMetadataListView.ObjectName := '';
      if FSelectedName = 'Fields' then
      begin
        FMetadataListView.ObjectName := Selected.Parent.Text;
        FMetadataListView.SelObjectType := mdFields;
      end
      else
        if FSelectedName = 'Parameters' then
        begin
          FMetadataListView.ObjectName := NameOnly(Selected.Parent.Text);

          if Selected.Parent.Parent.Text = 'Providers' then
            FMetadataListView.SelObjectType := mdProviderParams
          else
          if Selected.Parent.Parent.Text = 'IProviders' then
            FMetadataListView.SelObjectType := mdIProviderParams
          else
          if Selected.Parent.Parent.Text = 'Server Methods' then
            FMetadataListView.SelObjectType := mdServerMethodParams
          else
          if Selected.Parent.Parent.Text = 'Exec Server Methods' then
            FMetadataListView.SelObjectType := mdServerMethodExecParams
          else
          if Selected.Parent.Parent.Text = 'Stored Procedures' then
            FMetadataListView.SelObjectType := mdStoredProcColumns;


        end
        else
          if FSelectedName = 'Primary Key Fields' then
          begin
            FMetadataListView.ObjectName := Selected.Parent.Text;
            FMetadataListView.SelObjectType := mdPrimeKeys;
          end
          else
            if (FSelectedName = 'Triggers') then
            begin
              FMetadataListView.ObjectName := '';
              if Selected.Level <> FirstNodeLevel then
                FMetadataListView.ObjectName := Selected.Parent.Text;
              FMetadataListView.SelObjectType := mdTriggers;
            end
            else
              if (FSelectedName = 'Indexes') then
              begin
                if Selected.Level <> FirstNodeLevel then
                  FMetadataListView.ObjectName := Selected.Parent.Text;
                FMetadataListView.SelObjectType := mdIndexes;
              end
              else
                FMetadataListView.ObjectName := '';
      FMetadataListView.Execute;
    end;
  end;
  inherited;
end;

procedure TAstaIOMetadataTreeView.CollapseSelected;
begin
  try
    Items.BeginUpdate;
    case Selected.Level of
      0: Selected.Collapse(True);
      1: Selected.Parent.Collapse(True);
      2: Selected.Parent.Parent.Collapse(True);
      3: Selected.Parent.Parent.Parent.Collapse(True);
      4: Selected.Parent.Parent.Parent.Parent.Collapse(True);
    end;
  finally
    Items.EndUpdate;
  end;
end;

constructor TAstaIOMetadataTreeView.Create(AOwner: tComponent);
begin
  inherited Create(AOwner);

  ReadOnly := True;
{$IFNDEF LINUX}
  // HotTrack := True;
{$ENDIF}
  FUseExecCursor := True;
  FExecCursor := crSQLWait;

  AInfo := TAstaIOMetaDataDataSet.Create(Self);
  AInfo.MetaDataRequest := mdOtherMetaData;

  DataBaseNames := TStringList.Create;

  OnExpanding := OnExpandingProc;

  FBiz_DataModule := 'DataModule';
  FBiz_Method := 'ServerMethod';
  FBiz_Param_Name := 'Name';
  FBiz_Param_ParamType := 'ParamType';
  FBiz_Param_DataType := 'DataType';

  FIPrv_Provider := 'IProviderName';
  FPrv_DataModule := 'DataModule';
  FPrv_Provider := 'ProviderName';
  FPrv_Param_Name := 'Name';
  FPrv_Param_ParamType := 'ParamType';
  FPrv_Param_DataType := 'DataType';

  FSM_DataSet := 'DataSet';
  FSM_Class := 'Class';

  FDBMSName := 'DataBase';

  FVCL_FieldName := 'FieldName';
  FVCL_DataType := 'FieldType';
  FVCL_Size := 'FieldSize';

  FSP_Name := 'SProcName';
  FSP_ColName := 'ColumnName';
  FSP_InputType := 'InputType';
  FSP_DataType := 'ColumnType';

  FTab_Name := 'TableName';
  FSTab_Name := 'TableName';
  FVew_Name := 'ViewName';
  FUsr_Name := 'UserName';

  FIdx_Name := 'IndexName';

  FTrigger_Name := 'TriggerName';
  FTrigger_Relation_Name := 'RelationName';
end;

procedure TAstaIOMetadataTreeView.DblClick;
begin
  FSelObjectType := mdNormalQuery;
  FSingleSelected := False;

  if not Assigned(Selected) then
  begin
    inherited;
    exit;
  end;

  case Selected.Level of
    FirstNodeLevel:
      begin
        FSingleSelected := False;
        case Selected.Index of
          0: FSelObjectType := mdServerMethods;
          1: FSelObjectType := mdServerMethodsExec;
          2: FSelObjectType := mdProviders;
          3: FSelObjectType := mdIProviders;
          4: FSelObjectType := mdDBMSName;
          5: FSelObjectType := mdUserList;
          6: FSelObjectType := mdStoredProcs;
          7: FSelObjectType := mdSystemTables;
          8: FSelObjectType := mdTables;
          9: FSelObjectType := mdViews;
         10: FSelObjectType := mdTriggers;
        end;
      end;
    FirstNodeLevel + 1:
      begin
        FSingleSelected := True;
        case Selected.Parent.Index of
          0: FSelObjectType := mdServerMethods;
          1: FSelObjectType := mdServerMethodsExec;
          2: FSelObjectType := mdProviders;
          3: FSelObjectType := mdIProviders;
          4: FSelObjectType := mdDBMSName;
          5: FSelObjectType := mdUserList;
          6: FSelObjectType := mdStoredProcs;
          7: FSelObjectType := mdSystemTables;
          8: FSelObjectType := mdTables;
          9: FSelObjectType := mdViews;
         10: FSelObjectType := mdTriggers;
        end;
      end;
    FirstNodeLevel + 2:
      begin
        FSingleSelected := False;
        case Selected.Parent.Parent.Index of
          0: FSelObjectType := mdServerMethodParams;
          1: FSelObjectType := mdProviderParams;
          2: FSelObjectType := mdIProviderParams;
        end;
      end;
  end;
  inherited;
end;

destructor TAstaIOMetadataTreeView.Destroy;
begin
  AInfo.Free;
  inherited;
end;

procedure TAstaIOMetadataTreeView.Execute;
var Tn   :TTreeNode;
begin
  Clear;
  AInfo.Close;
  AInfo.AstaClientWire := ClientWire;

  if Assigned(FMetadataListView) then
    FMetadataListView.SelObjectType := mdNormalQuery;

  DataBaseNames.Clear;
  FCursor := Screen.Cursor;
  Items.BeginUpdate;
  try
    if UseExecCursor then
      Screen.Cursor := ExecCursor;
    Items.Clear;

    Tn:=Items.AddChild(nil, 'Server Methods');
    Tn.ImageIndex:=0;
    Tn.SelectedIndex:=0;
    Items.Addchild(Tn, '');

    Tn:=Items.AddChild(nil, 'Exec Server Methods');
    Tn.ImageIndex:=0;
    Tn.SelectedIndex:=0;
    Items.Addchild(Tn, '');

    Tn:=Items.AddChild(nil, 'Providers');
    Tn.ImageIndex:=1;
    Tn.SelectedIndex:=1;
    Items.Addchild(Tn, '');

    Tn:=Items.AddChild(nil, 'IProviders');
    Tn.ImageIndex:=1;
    Tn.SelectedIndex:=1;
    Items.Addchild(Tn, '');

    Tn:=Items.AddChild(nil, 'DBMSName');
    Tn.ImageIndex:=2;
    Tn.SelectedIndex:=2;
    Items.Addchild(Tn, '');

    Tn:=Items.AddChild(nil, 'User List');
    Tn.ImageIndex:=3;
    Tn.SelectedIndex:=3;
    Items.Addchild(Tn, '');

    Tn:=Items.AddChild(nil, 'Stored Procedures');
    Tn.ImageIndex:=4;
    Tn.SelectedIndex:=4;
    Items.Addchild(Tn, '');

    Tn:=Items.AddChild(nil, 'System Tables');
    Tn.ImageIndex:=5;
    Tn.SelectedIndex:=5;
    Items.Addchild(Tn, '');

    Tn:=Items.AddChild(nil, 'User Tables');
    Tn.ImageIndex:=6;
    Tn.SelectedIndex:=6;
    Items.Addchild(Tn, '');

    Tn:=Items.AddChild(nil, 'Views');
    Tn.ImageIndex:=7;
    Tn.SelectedIndex:=7;
    Items.Addchild(Tn, '');

    Tn:=Items.AddChild(nil, 'Triggers');
    Tn.ImageIndex:=11;
    Tn.SelectedIndex:=11;
    Items.Addchild(Tn, '');
  finally
    Screen.Cursor := FCursor;
    Items.EndUpdate;
  end;
end;

procedure TAstaIOMetadataTreeView.GetServerMethods(TheNode: TTreeNode);
begin
  if (TheNode.Count > 0)
    and (TheNode.Item[0].Text <> '') then exit;
  TheNode.DeleteChildren;

  AInfo.MetaDataRequest := mdServerMethods;
  AInfo.ObjectName := '';
  AInfo.Close;
  AInfo.Open;

  while not AInfo.Eof do
  begin
    with Items.Addchild(TheNode, AInfo.FieldByName(FBiz_DataModule).Asstring + '.' + AInfo.FieldByName(FBiz_Method).AsString) do
    begin
      ImageIndex := 0;
      SelectedIndex := 0;
    end;

    with Items.Addchild(TheNode[TheNode.count - 1], 'Parameters') do
    begin
      ImageIndex := -1;
      SelectedIndex := -1;
    end;
    with Items.Addchild(TheNode[TheNode.count - 1][0], '') do
    begin
      ImageIndex := -1;
      SelectedIndex := -1;
    end;
    AInfo.Next;
  end;
end;

procedure TAstaIOMetadataTreeView.GetExecServerMethods(TheNode: TTreeNode);
begin
  if (TheNode.Count > 0)
    and (TheNode.Item[0].Text <> '') then exit;
  TheNode.DeleteChildren;

  AInfo.MetaDataRequest := mdServerMethodsExec;
  AInfo.ObjectName := '';

  AInfo.Close;
  AInfo.Open;

  while not AInfo.Eof do
  begin
    with Items.Addchild(TheNode, AInfo.FieldByName(FBiz_DataModule).Asstring + '.' + AInfo.FieldByName(FBiz_Method).AsString ) do
    begin
      ImageIndex := 0;
      SelectedIndex := 0;
    end;

    with Items.Addchild(TheNode[TheNode.count - 1], 'Parameters') do
    begin
      ImageIndex := -1;
      SelectedIndex := -1;
    end;

    with Items.Addchild(TheNode[TheNode.count - 1][0], '') do
    begin
      ImageIndex := -1;
      SelectedIndex := -1;
    end;
    AInfo.Next;
  end;
end;

procedure TAstaIOMetadataTreeView.GetParams(TheNode: TTreeNode; ObjectName: string; MetadataReq: TAstaMetadata);
begin
  if (TheNode.Count > 0)
    and (TheNode.Item[0].Text <> '') then exit;

  TheNode.DeleteChildren;
  AInfo.ObjectName := ObjectName;
  AInfo.MetaDataRequest := MetadataReq;
  AInfo.Close;
  AInfo.Open;

  while not AInfo.Eof do
  begin
    with Items.Addchild(TheNode, Trim(AInfo.FieldByName('Name').Asstring)
      + ' - '
      + GetEnumName(TypeInfo(TFieldType), AInfo.FieldbyName('DataType').AsInteger)
      + ' - '
      + GetEnumName(TypeInfo(TParamType), AInfo.FieldbyName('ParamType').AsInteger)) do
    begin
      ImageIndex := 13;
      SelectedIndex := 13;
    end;
    AInfo.Next;
  end;
end;

procedure TAstaIOMetadataTreeView.GetProviders(TheNode: TTreeNode);
begin
  if (TheNode.Count > 0)
    and (TheNode.Item[0].Text <> '') then exit;

  TheNode.DeleteChildren;

  AInfo.MetaDataRequest := mdProviders;
  AInfo.ObjectName := '';
  AInfo.Close;
  AInfo.Open;
  while not AInfo.Eof do
  begin
    with Items.Addchild(TheNode, AInfo.FieldByName(FPrv_DataModule).Asstring + '.' + AInfo.FieldByName(FPrv_Provider).AsString) do
    begin
      ImageIndex := 1;
      SelectedIndex := 1;
    end;

    with Items.Addchild(TheNode[TheNode.count - 1], 'Parameters') do
    begin
      ImageIndex := -1;
      SelectedIndex := -1;
    end;
    Items.Addchild(TheNode[TheNode.count - 1][0], '');
    AInfo.Next;
  end;
end;

procedure TAstaIOMetadataTreeView.GetIProviders(TheNode: TTreeNode);
begin
  if (TheNode.Count > 0)
    and (TheNode.Item[0].Text <> '') then exit;

  TheNode.DeleteChildren;

  AInfo.MetaDataRequest := mdIProviders;
  AInfo.ObjectName := '';
  AInfo.Close;
  AInfo.Open;
  while not AInfo.Eof do
  begin
    with Items.Addchild(TheNode, AInfo.FieldByName(FPrv_DataModule).Asstring + '.' + AInfo.FieldByName(FIPrv_Provider).AsString) do
    begin
      ImageIndex := 1;
      SelectedIndex := 1;
    end;

    with Items.Addchild(TheNode[TheNode.count - 1], 'Parameters') do
    begin
      ImageIndex := -1;
      SelectedIndex := -1;
    end;
    Items.Addchild(TheNode[TheNode.count - 1][0], '');
    AInfo.Next;
  end;
end;

procedure TAstaIOMetadataTreeView.OnExpandingProc(Sender: TObject;
  Node: TTreeNode; var AllowExpansion: Boolean);
begin
  //MessageDlg('expand', mtWarning, [mbOk], 0);
  FIsExpanding := True;
  Selected := Node;
  FCursor := Screen.Cursor;
  if UseExecCursor then
    Screen.Cursor := ExecCursor;
  Items.BeginUpdate;
  try
    case Node.Level of
      FirstNodeLevel:
        begin
          FSingleSelected := False;
          case Node.Index of
            0:
              begin
                // Server Methods
                GetServerMethods(Node);
              end;
            1:
              begin
                // Exec Server Methods
                GetExecServerMethods(Node);
              end;
            2:
              begin
                // Providers
                GetProviders(Node);
              end;
            3:
              begin
                // IProviders
                GetIProviders(Node);
              end;
            4:
              begin
                // DBMSName
                GetDBMSNames(Node);
              end;
            5:
              begin
                // User List
                GetUserList(Node);
              end;
            6:
              begin
                // Stored Procedures
                GetStoredProcedures(Node);
              end;
            7:
              begin
                // System Tables
                GetTables(Node, mdSystemTables);
              end;
            8:
              begin
                // Tables
                GetTables(Node, mdTables);
              end;
            9:
              begin
                // Views
                GetViews(Node);
              end;
           10:
              begin
                // Triggers
                GetTriggers(Node);
              end;
          end;
        end;
      FirstNodeLevel + 1:
        begin
        end;
      FirstNodeLevel + 2:
        begin
          FSingleSelected := True;
          case Node.Parent.Parent.Index of
            0:
              begin
                // Server Methods
                FSelObjectType := mdServerMethodParams;
                GetParams(Node, NameOnly(Node.Parent.Text), mdServerMethodParams);
              end;
            1:
              begin
                // Exec Server Methods
                FSelObjectType := mdServerMethodExecParams;
                GetParams(Node, NameOnly(Node.Parent.Text), mdServerMethodExecParams);
              end;
            2:
              begin
                // Providers
                FSelObjectType := mdProviderParams;
                GetParams(Node, NameOnly(Node.Parent.Text), mdProviderParams);
              end;
            3:
              begin
                // IProviders
                FSelObjectType := mdIProviderParams;
                GetParams(Node, NameOnly(Node.Parent.Text), mdIProviderParams);
              end;
            6:
              begin
                // Stored Procedures
                FSelObjectType := mdStoredProcColumns;
                GetProcColumns(Node, NameOnly(Node.Parent.Text));
              end;
            7, 8:
              begin
                // Tables
                case Node.Index of
                  0:
                    begin
                      // Fields
                      GetFields(Node, NameOnly(Node.Parent.Text));
                    end;
                  1:
                    begin
                      // Indexes
                      GetIndexes(Node, Node.Parent.Text);
                    end;
                  2:
                    begin
                      // Prime Fields
                      GetPrimeFields(Node, NameOnly(Node.Parent.Text));
                    end;
                  3:
                    begin
                      // Triggers
                      GetTableTriggers(Node, NameOnly(Node.Parent.Text));
                    end;
                end;
              end;
          end;
        end;
    end;
  finally
    FIsExpanding := False;
    Items.EndUpdate;
    Screen.Cursor := FCursor;
  end;
end;

{procedure TAstaIOMetadataTreeView.OnMouseDownProc(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Tn: TTreeNode;
  Params: TAstaParamList;
begin
  Tn := GetnodeAt(X, Y);
  if not Assigned(Tn) then exit;
  case Tn.Level of
    0: FCurrentDbName := Tn.Text;
    1: FCurrentDbName := Tn.Parent.Text;
    2: FCurrentDbName := Tn.Parent.Parent.Text;
    3: FCurrentDbName := Tn.Parent.Parent.Parent.Text;
    4: FCurrentDbName := Tn.Parent.Parent.Parent.Parent.Text;
  end;
  Params := TastaParamList.Create;
  Params.FastAdd(FCurrentDbName);
  try
  //xx    RetParams:=FAstaClientSocket.SendGetCodedParamList(1035, Params);
  finally
    Params.Free;
  end;
end;}

procedure TAstaIOMetadataTreeView.GetStoredProcedures(TheNode: TTreeNode);
var
  Tn: TTreeNode;
begin
  if (TheNode.Count > 0)
    and (TheNode.Item[0].Text <> '') then exit;

  TheNode.DeleteChildren;

  AInfo.MetaDataRequest := mdStoredProcs;
  AInfo.Close;
  AInfo.Open;
  while not AInfo.Eof do
  begin
    Tn := Items.Addchild(TheNode, Trim(AInfo.FieldByName(FSP_Name).Asstring));
    Tn.ImageIndex := 4;
    Tn.SelectedIndex := 4;

    Tn := Items.Addchild(TheNode[TheNode.Count - 1], 'Parameters');
    Tn.ImageIndex := 13;
    Tn.SelectedIndex := 13;
    with Items.Addchild(TheNode[TheNode.Count - 1][0], '') do
    begin
      ImageIndex := -1;
      SelectedIndex := -1;
    end;
    AInfo.Next;
  end;
end;

procedure TAstaIOMetadataTreeView.GetTables(TheNode: TTreeNode; MetaDataRequest: TAstaMetaData);
var
  Tn: TTreeNode;
  Fieldn: TTreeNode;
  Indexn: TTreeNode;
begin
  if (TheNode.Count > 0)
    and (TheNode.Item[0].Text <> '') then exit;

  TheNode.DeleteChildren;

  AInfo.MetaDataRequest := MetaDataRequest;
  AInfo.Close;
  AInfo.Open;

  while not AInfo.Eof do
  begin
    Tn := Items.Addchild(TheNode, Trim(AInfo.FieldByName(FTab_Name).Asstring));
    if MetaDataRequest = mdTables then
    begin
      Tn.ImageIndex := 6;
      Tn.SelectedIndex := 6;
    end
    else
    begin
      Tn.ImageIndex := 5;
      Tn.SelectedIndex := 5;
    end;

    Fieldn := Items.Addchild(Tn, 'Fields');
    Fieldn.ImageIndex := 13;
    Fieldn.SelectedIndex := 13;
    with Items.Addchild(Fieldn, '') do
    begin
      ImageIndex := -1;
      SelectedIndex := -1;
    end;

    Indexn := Items.Addchild(Tn, 'Indexes');
    Indexn.ImageIndex := 9;
    Indexn.SelectedIndex := 9;
    with Items.Addchild(Indexn, '') do
    begin
      ImageIndex := -1;
      SelectedIndex := -1;
    end;

    Indexn := Items.Addchild(Tn, 'Primary Key Fields');
    Indexn.ImageIndex := 10;
    Indexn.SelectedIndex := 10;
    with Items.Addchild(Indexn, '') do
    begin
      ImageIndex := -1;
      SelectedIndex := -1;
    end;

    Indexn := Items.Addchild(Tn, 'Triggers');
    Indexn.ImageIndex := 12;
    Indexn.SelectedIndex := 12;
    with Items.Addchild(Indexn, '') do
    begin
      ImageIndex := -1;
      SelectedIndex := -1;
    end;

    AInfo.Next;
  end;
end;

procedure TAstaIOMetadataTreeView.GetTriggers(TheNode: TTreeNode);
var
  Tn: TTreeNode;
begin
  if (TheNode.Count > 0)
    and (TheNode.Item[0].Text <> '') then exit;

  TheNode.DeleteChildren;

  AInfo.MetaDataRequest := mdTriggers;
  AInfo.ObjectName := '';
  AInfo.Close;
  AInfo.Open;
  while not AInfo.Eof do
  begin
    Tn := Items.Addchild(TheNode, Trim(AInfo.FieldByName(FTrigger_Name).Asstring) + ' - ' + Trim(AInfo.FieldByName(FTrigger_Relation_Name).Asstring));
    Tn.ImageIndex := 12;
    Tn.SelectedIndex := 12;
    AInfo.Next;
  end;
end;

procedure TAstaIOMetadataTreeView.GetTableTriggers(TheNode: TTreeNode; TableName: string);
begin
  if (TheNode.Count > 0)
    and (TheNode.Item[0].Text <> '') then exit;

  TheNode.DeleteChildren;
  AInfo.ObjectName := TableName;
  AInfo.MetaDataRequest := mdTriggers;
  AInfo.Close;
  AInfo.Open;
  while not AInfo.Eof do
  begin
    with Items.Addchild(TheNode, Trim(AInfo.FieldByName(FTrigger_Name).Asstring)) do
    begin
      ImageIndex := 12;
      SelectedIndex := 12;
    end;
    AInfo.Next;
  end;
end;

procedure TAstaIOMetadataTreeView.GetuserList(TheNode: TTreeNode);
var
  Tn: TTreeNode;
begin
  if (TheNode.Count > 0)
    and (TheNode.Item[0].Text <> '') then exit;

  TheNode.DeleteChildren;
  AInfo.MetaDataRequest := mdUserList;
  AInfo.Close;
  AInfo.Open;
  while not AInfo.Eof do
  begin
    Tn := Items.Addchild(TheNode, AInfo.FieldByName(FUsr_Name).Asstring);
    Tn.ImageIndex := 3;
    Tn.SelectedIndex := 3;
    AInfo.Next;
  end;
end;

procedure TAstaIOMetadataTreeView.GetViews(TheNode: TTreeNode);
var
  Tn: TTreeNode;
begin
  if (TheNode.Count > 0)
    and (TheNode.Item[0].Text <> '') then exit;

  TheNode.DeleteChildren;
  AInfo.MetaDataRequest := mdViews;
  AInfo.Close;
  AInfo.Open;
  while not AInfo.Eof do
  begin
    Tn := Items.Addchild(TheNode, Trim(AInfo.FieldByName(FVew_Name).Asstring));
    Tn.ImageIndex := 7;
    Tn.SelectedIndex := 7;
    AInfo.Next;
  end;
end;

procedure TAstaIOMetadataTreeView.GetDBMSNames(TheNode: TTreeNode);
var
  Tn: TTreeNode;
begin
  if (TheNode.Count > 0)
    and (TheNode.Item[0].Text <> '') then exit;

  TheNode.DeleteChildren;

  AInfo.MetaDataRequest := mdDBMSName;
  AInfo.Close;
  AInfo.Open;
  while not AInfo.Eof do
  begin
    Tn := Items.Addchild(TheNode, AInfo.FieldByName(FDBMSName).Asstring);
    Tn.ImageIndex := 2;
    Tn.SelectedIndex := 2;
    AInfo.Next;
  end;
end;

procedure TAstaIOMetadataTreeView.GetFields(TheNode: TTreeNode; TableName: string);
begin
  if (TheNode.Count > 0)
    and (TheNode.Item[0].Text <> '') then exit;

  TheNode.DeleteChildren;
  AInfo.ObjectName := TableName;
  AInfo.MetaDataRequest := mdFields;
  AInfo.Close;
  AInfo.Open;
  while not AInfo.Eof do
  begin
    with Items.Addchild(TheNode, Trim(AInfo.FieldByName(FVCL_FieldName).Asstring)
      + ' - '
      + GetEnumName(TypeInfo(TFieldType), AInfo.FieldbyName(FVCL_DataType).AsInteger)
      + ' ('
      + AInfo.FieldByName(FVCL_Size).AsString
      + ' )') do
    begin
      ImageIndex := 13;
      SelectedIndex := 13;
    end;
    AInfo.Next;
  end;
end;

procedure TAstaIOMetadataTreeView.GetPrimeFields(TheNode: TTreeNode; TableName: string);
begin
  if (TheNode.Count > 0)
    and (TheNode.Item[0].Text <> '') then exit;

  TheNode.DeleteChildren;
  AInfo.ObjectName := TableName;
  AInfo.MetaDataRequest := mdPrimeKeys;
  AInfo.Close;
  AInfo.Open;
  while not AInfo.Eof do
  begin
    with Items.Addchild(TheNode, Trim(AInfo.FieldByName(FVCL_FieldName).Asstring)) do
    begin
      ImageIndex := 10;
      SelectedIndex := 10;
    end;
    AInfo.Next;
  end;
end;

procedure TAstaIOMetadataTreeView.GetIndexes(TheNode: TTreeNode; TableName: string);
begin
  if (TheNode.Count > 0)
    and (TheNode.Item[0].Text <> '') then exit;

  TheNode.DeleteChildren;
  AInfo.ObjectName := TableName;
  AInfo.MetaDataRequest := mdIndexes;
  AInfo.Close;
  AInfo.Open;

  while not AInfo.Eof do
  begin
    with Items.Addchild(TheNode, Trim(AInfo.FieldByName(FIdx_Name).Asstring)) do
    begin
      ImageIndex := 9;
      SelectedIndex := 9;
    end;
    AInfo.Next;
  end;
end;

procedure TAstaIOMetadataTreeView.GetProcColumns(TheNode: TTreeNode; StoredProcedure: string);
begin
  if (TheNode.Count > 0)
    and (TheNode.Item[0].Text <> '') then exit;

  TheNode.DeleteChildren;
  AInfo.ObjectName := StoredProcedure;
  AInfo.MetaDataRequest := mdStoredProcColumns;
  AInfo.Close;
  AInfo.Open;

  while not AInfo.Eof do
  begin
    with Items.Addchild(TheNode, Trim(AInfo.FieldByName(FSP_ColName).Asstring)
      + ' - '
      + GetEnumName(TypeInfo(TFieldType), AInfo.FieldbyName(FSP_DataType).AsInteger)) do
    begin
      ImageIndex := 13;
      SelectedIndex := 13;
    end;
    AInfo.Next;
  end;
end;

procedure TAstaIOMetadataTreeView.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = FClientWire) and (Operation = opRemove) then
    FClientWire := nil;
end;

end.

