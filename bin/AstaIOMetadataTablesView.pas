{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10241: AstaIOMetadataTablesView.pas 
{
{   Rev 1.0    4/10/2003 6:31:38 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:37:48 PM  Steve    Version: 1.505
}
unit AstaIOMetadataTablesView;
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
  TAstaIOMetadataTablesView = class(TTreeView)
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
    procedure GetTables(TheNode: TTreeNode; MetaDataRequest: TAstaMetaData);
    procedure GetViews(TheNode: TTreeNode);
    procedure GetFields(TheNode: TTreeNode; TableName: string);
    procedure GetPrimeFields(TheNode: TTreeNode; TableName: string);
    procedure GetIndexes(TheNode: TTreeNode; TableName: string);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    FVCL_FieldName,
      FVCL_DataType,
      FVCL_Size,

    FTab_Name,
      FIdx_Name,
      FVew_Name: string;
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

procedure Register;

implementation
uses AstaIOParamList;

procedure Register;
begin
  RegisterComponents('AstaIO', [TAstaIOMetadataTablesView]);
end;

function NameOnly(Value: string): string;
var
  idx: Integer;
begin
  Result := Value;
  idx := Pos('.', Value);
  if idx > 0 then
    Result := System.Copy(Value, idx + 1, Length(Value) - idx);
end;

{ TAstaIOMetadataTablesView }

procedure TAstaIOMetadataTablesView.Clear;
begin
  Items.BeginUpdate;
  Items.Clear;
  Items.EndUpdate;
  AInfo.Close;
end;

procedure TAstaIOMetadataTablesView.Click;
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
        if FSelectedName = 'Primary Key Fields' then
        begin
          FMetadataListView.ObjectName := Selected.Parent.Text;
          FMetadataListView.SelObjectType := mdPrimeKeys;
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

procedure TAstaIOMetadataTablesView.CollapseSelected;
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

constructor TAstaIOMetadataTablesView.Create(AOwner: tComponent);
begin
  inherited Create(AOwner);

  ReadOnly := True;
  FUseExecCursor := True;
  FExecCursor := crSQLWait;

  AInfo := TAstaIOMetaDataDataSet.Create(Self);
  AInfo.MetaDataRequest := mdOtherMetaData;

  DataBaseNames := TStringList.Create;

  OnExpanding := OnExpandingProc;

  FVCL_FieldName := 'FieldName';
  FVCL_DataType := 'FieldType';
  FVCL_Size := 'FieldSize';

  FTab_Name := 'TableName';
  FVew_Name := 'ViewName';

  FIdx_Name := 'IndexName';
end;

procedure TAstaIOMetadataTablesView.DblClick;
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
          0: FSelObjectType := mdSystemTables;
          1: FSelObjectType := mdTables;
          2: FSelObjectType := mdViews;
        end;
      end;
    FirstNodeLevel + 1:
      begin
        FSingleSelected := True;
        case Selected.Parent.Index of
          0: FSelObjectType := mdSystemTables;
          1: FSelObjectType := mdTables;
          2: FSelObjectType := mdViews;
        end;
      end;
  end;
  inherited;
end;

destructor TAstaIOMetadataTablesView.Destroy;
begin
  AInfo.Free;
  inherited;
end;

procedure TAstaIOMetadataTablesView.Execute;
var
  Tn: TTreeNode;
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

    Tn := TTreeNode.Create(Items);
    Tn.Text := 'Asta SQL Explorer';

    Items.AddChild(Tn, 'System Tables');
    Tn.Item[0].ImageIndex := 0;
    Tn.Item[0].SelectedIndex := 0;
    Items.Addchild(Tn.Item[0], '');

    Items.AddChild(Tn, 'User Tables');
    Tn.Item[1].ImageIndex := 1;
    Tn.Item[1].SelectedIndex := 1;
    Items.Addchild(Tn.Item[1], '');

    Items.AddChild(Tn, 'Views');
    Tn.Item[2].ImageIndex := 2;
    Tn.Item[2].SelectedIndex := 2;
    Items.Addchild(Tn.Item[2], '');

    Tn.Expand(False);
  finally
    Screen.Cursor := FCursor;
    Items.EndUpdate;
  end;
end;

procedure TAstaIOMetadataTablesView.OnExpandingProc(Sender: TObject;
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
                // System Tables
                GetTables(Node, mdSystemTables);
              end;
            1:
              begin
                // Tables
                GetTables(Node, mdTables);
              end;
            2:
              begin
                // Views
                GetViews(Node);
              end;
          end;
        end;
      FirstNodeLevel + 2:
        begin
          FSingleSelected := True;
          case Node.Parent.Parent.Index of
            0, 1:
              begin
                // System Tables
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

{procedure TAstaIOMetadataTablesView.OnMouseDownProc(Sender: TObject;
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
  //  RetParams:=FAstaClientSocket.SendGetCodedParamList(1035, Params);
  finally
    Params.Free;
  end;
end;}

procedure TAstaIOMetadataTablesView.GetTables(TheNode: TTreeNode; MetaDataRequest: TAstaMetaData);
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
      Tn.ImageIndex := 1;
      Tn.SelectedIndex := 1;
    end
    else
    begin
      Tn.ImageIndex := 0;
      Tn.SelectedIndex := 0;
    end;

    Fieldn := Items.Addchild(Tn, 'Fields');
    Fieldn.ImageIndex := 3;
    Fieldn.SelectedIndex := 3;
    with Items.Addchild(Fieldn, '') do
    begin
      ImageIndex := -1;
      SelectedIndex := -1;
    end;

    Indexn := Items.Addchild(Tn, 'Indexes');
    Indexn.ImageIndex := 4;
    Indexn.SelectedIndex := 4;
    with Items.Addchild(Indexn, '') do
    begin
      ImageIndex := -1;
      SelectedIndex := -1;
    end;

    Indexn := Items.Addchild(Tn, 'Primary Key Fields');
    Indexn.ImageIndex := 5;
    Indexn.SelectedIndex := 5;
    with Items.Addchild(Indexn, '') do
    begin
      ImageIndex := -1;
      SelectedIndex := -1;
    end;
    AInfo.Next;
  end;
end;

procedure TAstaIOMetadataTablesView.GetViews(TheNode: TTreeNode);
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
    Tn.ImageIndex := 2;
    Tn.SelectedIndex := 2;
    AInfo.Next;
  end;
end;

procedure TAstaIOMetadataTablesView.GetFields(TheNode: TTreeNode; TableName: string);
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
        //          + AInfo.FieldByName(FVCL_DataType).AsString
      + ' ('
      + AInfo.FieldByName(FVCL_Size).AsString
      + ' )') do
    begin
      ImageIndex := 3;
      SelectedIndex := 3;
    end;
    AInfo.Next;
  end;
end;

procedure TAstaIOMetadataTablesView.GetPrimeFields(TheNode: TTreeNode; TableName: string);
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
      ImageIndex := 5;
      SelectedIndex := 5;
    end;
    AInfo.Next;
  end;
end;

procedure TAstaIOMetadataTablesView.GetIndexes(TheNode: TTreeNode; TableName: string);
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
      ImageIndex := 4;
      SelectedIndex := 4;
    end;
    AInfo.Next;
  end;
end;

procedure TAstaIOMetadataTablesView.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = FClientWire) and (Operation = opRemove) then
    FClientWire := nil;
end;

end.

