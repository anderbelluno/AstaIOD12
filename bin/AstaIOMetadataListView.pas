{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10239: AstaIOMetadataListView.pas 
{
{   Rev 1.0    4/10/2003 6:31:36 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:37:48 PM  Steve    Version: 1.505
}
unit AstaIOMetadataListView;
{*********************************************************}
{*   Copyright (c) 1997-2002 Asta Technology Group Inc.  *}
{*               All rights reserved.                    *}
{*               www.astatech.com                        *}
{*********************************************************}

{$I AstaIO.inc}

interface

uses
  DB, SysUtils, Classes, TypInfo,
{$IFDEF LINUX}
  QDialogs, QForms, QStdCtrls, QControls, QComCtrls,
{$ELSE}
  Forms, StdCtrls, Controls, ComCtrls, Dialogs,
{$ENDIF}
  AstaIODBCOnst,
  AstaIOClientRemoteDataSet,
  AstaIOClientWire;

const
{$IFDEF LINUX}
  FirstColumn = 1;
{$ELSE}
  FirstColumn = 0;
{$ENDIF}

type
  TAstaIOMetadataListView = class(TListView)
  private
    FAbout: String;
    FClientWire: TAstaIOClientWire;
    FSelObjectType: TAstaMetaData;
    AInfo: TAstaIOMetaDataDataSet;
    FImageIndex: Integer;
    FDatabase: string;
    FObjectName: string;
    procedure SetData(ClearIt: Boolean = True);
  protected
    procedure GetServerMethods;
    procedure GetServerMethodsExec;
    procedure GetProviders;
    procedure GetIProviders;
    procedure GetParams(ObjectName :String; MetadataReq :TAstaMetadata);
    procedure GetTables;
    procedure GetTriggers;
    procedure GetTableTriggers(TableName: string);
    procedure GetSystemTables;
    procedure GetStoredProcedures;
    procedure GetuserList;
    procedure GetViews;
    procedure GetDBMSNames;
    procedure GetFields(TableName: string);
    procedure GetPrimeFields(TableName: string);
    procedure GetIndexes(TableName: string);
    procedure GetProcColumns(StoredProcedure: string);
    procedure GetDirectory(Path: string);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: tComponent); override;
    destructor Destroy; override;
    procedure Execute;
    procedure Clear; {$IFNDEF VER130} {$ifndef Linux} override; {$ENDIF}{$endif}
  published
    property About: String read FAbout write FAbout;
    property SelObjectType: TAstaMetaData read FSelObjectType write FSelObjectType;
    property ClientWire: TAstaIOClientWire read FClientWire write FClientWire;
    property DataBase: string read FDatabase write FDatabase;
    property ObjectName: string read FObjectName write FObjectName;
  end;

implementation

{ AstaIOMetadataListView }

constructor TAstaIOMetadataListView.Create(AOwner: tComponent);
begin
  inherited Create(AOwner);
  FSelObjectType := mdTables;

  AInfo := TAstaIOMetaDataDataSet.Create(Self);
end;

destructor TAstaIOMetadataListView.Destroy;
begin
  AInfo.Free;
  inherited Destroy;
end;

procedure TAstaIOMetadataListView.GetDBMSNames;
begin
  AInfo.MetaDataRequest := mdDBMSName;
  FImageIndex := 2;
  SetData;
end;

procedure TAstaIOMetadataListView.GetFields(TableName: string);
begin
  AInfo.MetaDataRequest := mdFields;
  AInfo.ObjectName := TableName;
  FImageIndex := 13;
  SetData;
end;

procedure TAstaIOMetadataListView.GetIndexes(TableName: string);
begin
  AInfo.MetaDataRequest := mdIndexes;
  AInfo.ObjectName := TableName;
  FImageIndex := 9;
  SetData;
end;

procedure TAstaIOMetadataListView.GetPrimeFields(TableName: string);
begin
  AInfo.MetaDataRequest := mdPrimeKeys;
  AInfo.ObjectName := TableName;
  FImageIndex := 10;
  SetData;
end;

procedure TAstaIOMetadataListView.GetProcColumns(StoredProcedure: string);
begin
  AInfo.MetaDataRequest := mdStoredProcColumns;
  AInfo.ObjectName := StoredProcedure;
  FImageIndex := 13;
  SetData;
end;

procedure TAstaIOMetadataListView.GetProviders;
begin
  AInfo.MetaDataRequest := mdProviders;
  FImageIndex := 1;
  SetData;
end;

procedure TAstaIOMetadataListView.GetIProviders;
begin
  AInfo.MetaDataRequest := mdIProviders;
  FImageIndex := 1;
  SetData;
end;

procedure TAstaIOMetadataListView.GetServerMethods;
begin
  AInfo.MetaDataRequest := mdServerMethods;
  FImageIndex := 0;
  SetData;
end;

procedure TAstaIOMetadataListView.GetServerMethodsExec;
begin
  AInfo.MetaDataRequest := mdServerMethodsExec;
  FImageIndex := 0;
  SetData;
end;

procedure TAstaIOMetadataListView.GetStoredProcedures;
begin
  AInfo.MetaDataRequest := mdStoredProcs;
  FImageIndex := 4;
  SetData;
end;

procedure TAstaIOMetadataListView.GetSystemTables;
begin
  AInfo.MetaDataRequest := mdSystemTables;
  FImageIndex := 5;
  SetData;
end;

procedure TAstaIOMetadataListView.GetTables;
begin
  AInfo.MetaDataRequest := mdTables;
  FImageIndex := 6;
  SetData;
end;

procedure TAstaIOMetadataListView.GetTableTriggers(TableName: string);
begin
  AInfo.MetaDataRequest := mdTriggers;
  AInfo.ObjectName := TableName;
  FImageIndex := 12;
  SetData;
end;

procedure TAstaIOMetadataListView.GetTriggers;
begin
  AInfo.MetaDataRequest := mdTriggers;
  FImageIndex := 12;
  SetData;
end;

procedure TAstaIOMetadataListView.GetuserList;
begin
  AInfo.MetaDataRequest := mdUserList;
  FImageIndex := 3;
  SetData;
end;

procedure TAstaIOMetadataListView.GetViews;
begin
  AInfo.MetaDataRequest := mdViews;
  FImageIndex := 7;
  SetData;
end;

procedure TAstaIOMetadataListView.Execute;
begin
  Clear;
  AInfo.Close;
  AInfo.AstaClientWire := ClientWire;
  AInfo.DataBase := FDatabase;
  AInfo.ObjectName := '';
  Items.BeginUpdate;
  try
    case FSelObjectType of
      mdServerMethods: GetServerMethods;
      mdServerMethodsExec: GetServerMethodsExec;
      mdProviders: GetProviders;
      mdIProviders: GetIProviders;
      mdServerMethodExecParams,
      mdServerMethodParams,
      mdProviderParams,
      mdIProviderParams: GetParams(FObjectName, FSelObjectType);
      mdTables: GetTables;
      mdTriggers:
        if Trim(FObjectName) = '' then
          GetTriggers
        else
          GetTableTriggers(FObjectName);
      mdSystemTables: GetSystemTables;
      mdStoredProcs: GetStoredProcedures;
      mdUserList: GetuserList;
      mdViews: GetViews;
      mdDBMSName: GetDBMSNames;
      mdFields: GetFields(FObjectName);
      mdPrimeKeys: GetPrimeFields(FObjectName);
      mdIndexes: GetIndexes(FObjectName);
      mdStoredProcColumns: GetProcColumns(FObjectName);
      mdDirectory: GetDirectory(FObjectName);
    end;
  finally
    Items.EndUpdate;
  end;
end;

procedure TAstaIOMetadataListView.Clear;
begin
  {$IFNDEF VER130}
  {$IfDef MSWindows}
  inherited Clear;
  {$ENDIF}
  {$endif}
  Items.BeginUpdate;
  Items.Clear;
  Items.EndUpdate;
  AInfo.Close;
end;

procedure TAstaIOMetadataListView.SetData(ClearIt: Boolean = True);
var
  i: Integer;
  NewColumn: TListColumn;
  FColWidth: Integer;
  NewListItem: TListItem;
begin
  AInfo.Open;

  Items.BeginUpdate;
  try
    if ClearIt then
    begin
      Items.Clear;
      {$IFDEF LINUX}
      for i := Columns.Count - 1 downto 1 do
        Columns.Delete(i);
      {$ELSE}
      Columns.Clear;
      {$ENDIF}

      {$IFDEF LINUX}
      Columns[0].Caption := AInfo.Fields[i].DisplayLabel;
      FColWidth := AInfo.Fields[i].Size * Font.Size;
      if Length(AInfo.Fields[i].DisplayLabel) * Font.Size > FColWidth then
        FColWidth := Length(AInfo.Fields[i].DisplayLabel) * Font.Size;
      Columns[0].Width := FColWidth;
      {$ENDIF}

      for i := FirstColumn to AInfo.FieldCount - 1 do
      begin
        if AInfo.Fields[i].DisplayLabel = 'Params' then Continue;
        NewColumn := Columns.Add;
        NewColumn.Caption := AInfo.Fields[i].DisplayLabel;
        FColWidth := AInfo.Fields[i].Size * Font.Size;
        if Length(AInfo.Fields[i].DisplayLabel) * Font.Size > FColWidth then
          FColWidth := Length(AInfo.Fields[i].DisplayLabel) * Font.Size;
        NewColumn.Width := FColWidth;
      end;
    end;

    AInfo.First;
    while not AInfo.Eof do
    begin
      NewListItem := Items.Add;
      NewListItem.Caption := AInfo.Fields[0].AsString;
      NewListItem.ImageIndex := FImageIndex;

      {$IFNDEF LINUX}
      for i := 1 to AInfo.FieldCount - 1 do
      begin
        if (AInfo.Fields[i].FieldName = 'FieldType')
          or (AInfo.Fields[i].FieldName = 'ColumnType')
          or (AInfo.Fields[i].FieldName = 'DataType') then
        begin
         NewListItem.SubItems.Add(GetEnumName(TypeInfo(TFieldType), AInfo.Fields[i].AsInteger));
        end
        else
        if (AInfo.Fields[i].FieldName = 'ParamType') then
        begin
         NewListItem.SubItems.Add(GetEnumName(TypeInfo(TParamType), AInfo.Fields[i].AsInteger));
        end
        else
          NewListItem.SubItems.Add(Trim(AInfo.Fields[i].AsString));
        NewListItem.SubItemImages[0] := -1;
      end;
      {$ENDIF}

      AInfo.Next;
    end;
  finally
    Items.EndUpdate;
  end;
end;

procedure TAstaIOMetadataListView.GetDirectory(Path: string);
begin
  AInfo.MetaDataRequest := mdDirectory;
  AInfo.ObjectName := Path;
  FImageIndex := 13;
  SetData;
end;

procedure TAstaIOMetadataListView.GetParams(ObjectName: String; MetadataReq: TAstaMetadata);
begin
  AInfo.MetaDataRequest := MetadataReq;
  Ainfo.ObjectName := ObjectName;
  FImageIndex := 2;
  SetData;
end;

procedure TAstaIOMetadataListView.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = FClientWire) and (Operation = opRemove) then
    FClientWire := nil;
end;

end.

