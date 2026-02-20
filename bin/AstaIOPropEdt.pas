{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10277: AstaIOPropEdt.pas 
{
{   Rev 1.0    4/10/2003 6:31:54 AM  Steve
}
{
{   Rev 1.0    11/5/2002 5:59:30 PM  Steve
}
{
{   Rev 1.0    10/30/2002 8:38:00 PM  Steve    Version: 1.505
}
unit AstaIOPropEdt;

{*********************************************************}
{*   Copyright (c) 1997-2002 Asta Technology Group Inc.  *}
{*               All rights reserved.                    *}
{*               www.astatech.com                        *}
{*********************************************************}

{$I AstaIO.inc}

interface
uses Classes, Controls, SysUtils, DB, TypInfo,
     AstaIOClientRemoteDataSet,
     AstaIOExecServerMethod,
     AstaIOCustomDataSet,
     AstaIOIndexes,
     AstaIOClientDataSet,
     AstaIOFieldLinks,
     AstaIOFieldsSelect,
     AstaIOMetadataTablesView,
     AstaIOMetadataTreeView,
     AstaIOMetadataListView,
     AstaIOUpdateObjectBase,
     AstaIODataBasePlugin,
     AstaIOMetaData,
     AstaIOProvider,
     AstaIODBInfo,
     AstaIOBaseRdbmsInfo,
     AstaIOClientMsgWire,
     AstaIOServerWire,
     AstaIOSocketServer,
     AstaIOLowCore,
     AstaIOIProvider,
     AstaIOServerMethod,
     {$ifdef AstaIOPdaCompile}
     AstaIOPdaBase,
     AstaIOPdaDataListProducer,
     AstaIOPdaServerPlugin,
     AstaIOPdaUtils, 
     {$endif}
     Dialogs,
     DesignEditors, DesignIntf
     ;

type
  TAstaIOCustomDatasetHack = class(TAstaIOCustomDataset);

type
  TAstaFileName = string;
  TAstaCustomClientSQLDataSetHack = class(TAstaCustomClientSQLDataSet);
  TAstaCustomAuditDataSetHack = class(TAstaCustomAuditDataSet);
  TAstaParamsDataSetHack = class(TAstaParamsDataSet);

type
  TAboutProperty = class(TPropertyEditor)
  public
    function GetValue:String; override;
    procedure SetValue(const Value :String); override;
    procedure Edit;override;
    function GetAttributes:TPropertyAttributes; override;
  end;

  TOffLineProperty = class(TPropertyEditor)
  public
    function GetValue:String; override;
    procedure SetValue(const Value :String); override;
    procedure Edit;override;
    function GetAttributes:TPropertyAttributes; override;
  end;

  TWorkBenchProperty = class(TPropertyEditor)
  public
    function GetValue:String; override;
    procedure SetValue(const Value :String); override;
    procedure Edit;override;
    function GetAttributes:TPropertyAttributes; override;
  end;

type
  TMetaDataProperty = class(TStringProperty)
  public
    procedure GetValues(TheProc: TGetStrProc); override;
    function GetAttributes:TPropertyAttributes; override;
  end;

type
  TAutoIncSequenceProperty = class(TStringProperty)
  public
    procedure GetValues(TheProc: TGetStrProc); override;
    function GetAttributes:TPropertyAttributes; override;
  end;

  TFileNameProperty = class(TPropertyEditor)
  public
    function GetValue :String; override;
    procedure SetValue(const Value :String); override;
    procedure Edit;override;
    function GetAttributes:TPropertyAttributes; override;
  end;

type
  TFieldsSelectProperty = class(TPropertyEditor)
  public
    function GetValue:string; override;
    procedure SetValue(const Value :String); override;
    procedure Edit ;override;
    function GetAttributes :TPropertyAttributes; override;
  end;

type
  TAstaFieldLink = class(TFieldLinkProperty)
  public
    function GetParent :TAstaIOCustomDatasetHack;
    property FParent :TAstaIOCustomDatasetHack read GetParent;

    procedure GetValues(TheProc: TGetStrProc); override;
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;

    procedure GetFieldNamesForIndex(List: TStrings); override;
    function GetIndexBased: Boolean; override;
    function GetIndexDefs: TIndexDefs; override;
    function GetIndexFieldNames: String; override;
    function GetIndexName: String; override;
    function GetMasterFields: String; override;
    procedure SetIndexFieldNames(const Value: String); override;
    procedure SetIndexName(const Value: String); override;
    procedure SetMasterFields(const Value: String); override;
    constructor CreateWith(ADataSet: TDataSet); override;
  end;

  TAstaIndexFieldNamesProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TAstaIndexNameProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

  TAstaFilterProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
  end;
procedure Register;

implementation
uses AstaIODBConst,
     AstaIOCompAbout,
     AstaIOOffLine,
     AstaIOWorkBenchFrm,
     AstaIOClientIProvider,
     AstaIODataSetProvider,
     AstaIOUtil,
     AstaIOResources;

procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(String), TAstaIOClientTable, 'MasterFields', TAstaFieldLink);
  RegisterPropertyEditor(TypeInfo(String), TAstaIOClientIProvider, 'MasterFields', TAstaFieldLink);
  RegisterPropertyEditor(TypeInfo(String), TAstaIOProviderDataSet, 'MasterFields', TAstaFieldLink);
  RegisterPropertyEditor(TypeInfo(String), TAstaIOClientDataSet, 'MasterFields', TAstaFieldLink);
  RegisterPropertyEditor(TypeInfo(String), TAstaIOServerMethodDataSet, 'MasterFields', TAstaFieldLink);

  RegisterPropertyEditor(TypeInfo(TOffLineString), nil, 'OffLine', TOffLineProperty);
  RegisterPropertyEditor(TypeInfo(TWorkBenchString), nil, 'WorkBench', TWorkBenchProperty);
  RegisterPropertyEditor(TypeInfo(TAstaFileName),TAstaIOCustomDataset,'FileName',TFileNameProperty);

  RegisterPropertyEditor(TypeInfo(String), TAstaClientRemoteDataSet, 'Database', TMetaDataProperty);
  RegisterPropertyEditor(TypeInfo(String), TAstaIOClientQuery, 'Database', TMetaDataProperty);
  RegisterPropertyEditor(TypeInfo(String), TAstaIOClientTable, 'Database', TMetaDataProperty);
  RegisterPropertyEditor(TypeInfo(String), TAstaIOClientStoredProc, 'Database', TMetaDataProperty);
  RegisterPropertyEditor(TypeInfo(String), TAstaIOServerMethodDataSet, 'Database', TMetaDataProperty);
  RegisterPropertyEditor(TypeInfo(String), TAstaIOExecServerMethod, 'Database', TMetaDataProperty);

  RegisterPropertyEditor(TypeInfo(String), TAstaIOClientQuery, 'UpdateTableName', TMetaDataProperty);
  RegisterPropertyEditor(TypeInfo(String), TAstaIOClientTable, 'UpdateTableName', TMetaDataProperty);
  RegisterPropertyEditor(TypeInfo(String), TAstaIOClientStoredProc, 'UpdateTableName', TMetaDataProperty);

  RegisterPropertyEditor(TypeInfo(String), TAstaIOClientTable, 'TableName', TMetaDataProperty);

  RegisterPropertyEditor(TypeInfo(String), TAstaIOClientStoredProc, 'StoredProcName', TMetaDataProperty);

  RegisterPropertyEditor(TypeInfo(String), TAstaIOClientIProvider, 'IProviderName', TMetaDataProperty);
  RegisterPropertyEditor(TypeInfo(String), TAstaIOProviderDataSet, 'ProviderName', TMetaDataProperty);

  RegisterPropertyEditor(TypeInfo(String), TAstaIOServerMethodDataSet, 'ServerMethodName', TMetaDataProperty);
  RegisterPropertyEditor(TypeInfo(String), TAstaIOExecServerMethod, 'ServerMethodName', TMetaDataProperty);

  RegisterPropertyEditor(TypeInfo(TStrings), TAstaIOClientQuery, 'PrimeFields', TFieldsSelectProperty);
  RegisterPropertyEditor(TypeInfo(TStrings), TAstaIOClientTable, 'PrimeFields', TFieldsSelectProperty);
  RegisterPropertyEditor(TypeInfo(TStrings), TAstaIOClientStoredProc, 'PrimeFields', TFieldsSelectProperty);

  RegisterPropertyEditor(TypeInfo(TStrings), TAstaIOClientQuery, 'NoSQLFields', TFieldsSelectProperty);
  RegisterPropertyEditor(TypeInfo(TStrings), TAstaIOClientTable, 'NoSQLFields', TFieldsSelectProperty);
  RegisterPropertyEditor(TypeInfo(TStrings), TAstaIOClientStoredProc, 'NoSQLFields', TFieldsSelectProperty);

  RegisterPropertyEditor(TypeInfo(TStrings),   TAstaIOServerMethodDataSet, 'RefetchFields', TFieldsSelectProperty);
  RegisterPropertyEditor(TypeInfo(TStrings), TAstaIOProviderDataSet, 'RefetchFields', TFieldsSelectProperty);
  RegisterPropertyEditor(TypeInfo(TStrings), TAstaIOClientQuery, 'RefetchFields', TFieldsSelectProperty);
  RegisterPropertyEditor(TypeInfo(TStrings), TAstaIOClientTable, 'RefetchFields', TFieldsSelectProperty);
  RegisterPropertyEditor(TypeInfo(TStrings), TAstaIOClientStoredProc, 'RefetchFields', TFieldsSelectProperty);

  RegisterPropertyEditor(TypeInfo(String), TAstaIOServerMethodDataSet, 'AutoIncrementField', TAutoIncSequenceProperty);
  RegisterPropertyEditor(TypeInfo(String), TAstaIOProviderDataSet, 'AutoIncrementField', TAutoIncSequenceProperty);
  RegisterPropertyEditor(TypeInfo(String), TAstaIOClientQuery, 'AutoIncrementField', TAutoIncSequenceProperty);
  RegisterPropertyEditor(TypeInfo(String), TAstaIOClientTable, 'AutoIncrementField', TAutoIncSequenceProperty);
  RegisterPropertyEditor(TypeInfo(String), TAstaIOServerMethodDataSet, 'Sequence', TAutoIncSequenceProperty);
  RegisterPropertyEditor(TypeInfo(String), TAstaIOProviderDataSet, 'Sequence', TAutoIncSequenceProperty);
  RegisterPropertyEditor(TypeInfo(String), TAstaIOClientQuery, 'Sequence', TAutoIncSequenceProperty);
  RegisterPropertyEditor(TypeInfo(String), TAstaIOClientTable, 'Sequence', TAutoIncSequenceProperty);

  RegisterPropertyEditor(TypeInfo(String), TAstaIOMetadataTablesView, 'About', TAboutProperty);
  RegisterPropertyEditor(TypeInfo(String), TAstaIOMetadataTreeView, 'About', TAboutProperty);
  RegisterPropertyEditor(TypeInfo(String), TAstaIOMetadataListView, 'About', TAboutProperty);
  RegisterPropertyEditor(TypeInfo(String), TAstaIOCustomDataset, 'About', TAboutProperty);
  RegisterPropertyEditor(TypeInfo(String), TAstaIOExecServerMethod, 'About', TAboutProperty);
  RegisterPropertyEditor(TypeInfo(String), TAstaIOUpdateObjectBase, 'About', TAboutProperty);
  RegisterPropertyEditor(TypeInfo(String), TAstaIODataBasePlugin, 'About', TAboutProperty);
  RegisterPropertyEditor(TypeInfo(String), TAstaIOMetaData, 'About', TAboutProperty);
  RegisterPropertyEditor(TypeInfo(String), TAstaIOCustomProvider, 'About', TAboutProperty);
  RegisterPropertyEditor(TypeInfo(String), TAstaIODBInfo, 'About', TAboutProperty);
  RegisterPropertyEditor(TypeInfo(String), TAstaCustomMsgClientwire, 'About', TAboutProperty);
  RegisterPropertyEditor(TypeInfo(String), TCustomAstaServerWire, 'About', TAboutProperty);
  RegisterPropertyEditor(TypeInfo(String), TAstaSocketServer, 'About', TAboutProperty);
  RegisterPropertyEditor(TypeInfo(String), TAstaIOClientSocket, 'About', TAboutProperty);
  RegisterPropertyEditor(TypeInfo(String), TAstaIOBaseServerMethod, 'About', TAboutProperty);
  RegisterPropertyEditor(TypeInfo(String), TAstaIOBaseRdbmsInfo, 'About', TAboutProperty);
  RegisterPropertyEditor(TypeInfo(String), TAstaIOIProvider, 'About', TAboutProperty);
  RegisterPropertyEditor(TypeInfo(String), TAstaIODataSetProvider, 'About', TAboutProperty);

  RegisterPropertyEditor(TypeInfo(string), TAstaIOCustomDataSet, 'IndexFieldNames', TAstaIndexFieldNamesProperty);
  RegisterPropertyEditor(TypeInfo(string), TAstaIOCustomDataSet, 'IndexName', TAstaIndexNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TAstaIOCustomDataSet, 'Filter', TAstaFilterProperty);
end;

function IsNumeric(FieldType :TFieldType): Boolean;
begin
  Result:=FieldType in [ftSmallInt, ftInteger, ftWord, ftLargeInt, ftFloat,
    ftCurrency, ftBCD, ftAutoInc, ftFmtBcd];
end;

// TAboutProperty

procedure TAboutProperty.Edit;
var acomp        :TComponent;
    F_CompAbout  :TF_CompAbout;
begin
  acomp:=TComponent(GetComponent(0));
  F_CompAbout:=TF_CompAbout.Create(nil,acomp.ClassName, AstaIOVersion);
  try
    F_CompAbout.ShowModal;
  finally
    F_CompAbout.Free;
  end;
end;

function TAboutProperty.GetAttributes: TPropertyAttributes;
begin
  Result:=[paDialog];
end;

function TAboutProperty.GetValue: String;
begin
  Result:=GetStrValue;
  Result:='Press ... to Display';
end;

procedure TAboutProperty.SetValue(const Value: String);
begin
  SetStrValue(Value);
end;

// TOffLineProperty

procedure TOffLineProperty.Edit;
var ADataSet      :TAstaCustomClientSQLDataSet;
    OffLineDialog :TOffLineDialog;
    UpdateMode :TUpdateMode;
    UpdateKind :TAstaUpdateMethod;
begin
  if (GetComponent(0) is TAstaCustomClientSQLDataSet) or
    (GetComponent(0) is TAstaIOProviderDataSet) or
    (GetComponent(0) is TAstaIOClientStoredProc) or
    (GetComponent(0) is TAstaCustomAuditDataSet) then
//    (GetComponent(0) is TAstaIONestedDataSet) then
  begin
    ADataSet:=TAstaCustomClientSQLDataSet(GetComponent(0));
    OffLineDialog:=TOffLineDialog.Create(nil);
    try
      for UpdateMode:=Low(TUpdateMode) to High(TUpdateMode) do
        OffLineDialog.cmb_updatemode.Items.Add(GetEnumName(TypeInfo(TUpdateMode), Ord(UpdateMode)));
      OffLineDialog.cmb_updatemode.ItemIndex:=Ord(TAstaCustomAuditDataSetHack(ADataSet).UpdateMode);

      for UpdateKind:=Low(TAstaUpdateMethod) to High(TAstaUpdateMethod) do
        OffLineDialog.cmb_updatemethod.Items.Add(GetEnumName(TypeInfo(TAstaUpdateMethod), Ord(UpdateKind)));
      OffLineDialog.cmb_updatemethod.ItemIndex:=Ord(TAstaCustomAuditDataSetHack(ADataSet).UpdateMethod);

      if (GetComponent(0) is TAstaCustomAuditDataSet) then
      begin
        OffLineDialog.PrimeFields.Assign(TAstaCustomAuditDataSetHack(ADataSet).PrimeFields);
        OffLineDialog.NoSQLFields.Assign(TAstaCustomAuditDataSetHack(ADataSet).NoSQLFields);
        OffLineDialog.RefetchFields.Assign(TAstaCustomAuditDataSetHack(ADataSet).RefetchFields);
        OffLineDialog.e_updatetablename.Text:=TAstaCustomAuditDataSetHack(ADataSet).UpdateTableName;
        OffLineDialog.e_AutoIncrement.Text:=TAstaCustomAuditDataSetHack(ADataSet).AutoIncrementField;
      end
      else
      begin
        OffLineDialog.PrimeFields.Assign(TAstaCustomClientSQLDataSetHack(ADataSet).PrimeFields);
        OffLineDialog.NoSQLFields.Assign(TAstaCustomClientSQLDataSetHack(ADataSet).NoSQLFields);
        OffLineDialog.RefetchFields.Assign(TAstaCustomClientSQLDataSetHack(ADataSet).RefetchFields);
        OffLineDialog.e_database.Text:=ADataSet.Database;
        OffLineDialog.e_updatetablename.Text:=TAstaCustomClientSQLDataSetHack(ADataSet).UpdateTableName;
        OffLineDialog.e_AutoIncrement.Text:=TAstaCustomClientSQLDataSetHack(ADataSet).AutoIncrementField;
      end;

      OffLineDialog.e_tablename.Enabled:=(GetComponent(0) is TAstaIOClientTable);
      OffLineDialog.lab_tablename.Enabled:=(GetComponent(0) is TAstaIOClientTable);
      OffLineDialog.e_database.Enabled:=not (GetComponent(0) is TAstaIOCustomClientDataSet);
      OffLineDialog.lab_database.Enabled:=not (GetComponent(0) is TAstaIOCustomClientDataSet);

      if GetComponent(0) is TAstaIOCLientTable then
        OffLineDialog.e_tablename.Text:=TAstaIOCLientTable(ADataSet).TableName
      else
        OffLineDialog.e_tablename.Text:='';

      OffLineDialog.Caption:='Off-Line DataSet properties for ' + TAstaCustomClientSQLDataSetHack(ADataSet).Name;
      OffLineDialog.ShowModal;
      if OffLineDialog.ModalResult = mrOk then
      begin
        TAstaCustomAuditDataSetHack(ADataSet).UpdateMethod:=TAstaUpdateMethod(OffLineDialog.cmb_updatemethod.ItemIndex);
        TAstaCustomAuditDataSetHack(ADataSet).UpdateMode:=TUpdateMode(OffLineDialog.cmb_updatemode.ItemIndex);

        if (GetComponent(0) is TAstaCustomAuditDataSet) then
        begin
          TAstaCustomAuditDataSetHack(ADataSet).PrimeFields.Assign(OffLineDialog.PrimeFields);
          TAstaCustomAuditDataSetHack(ADataSet).NoSQLFields.Assign(OffLineDialog.NoSQLFields);
          TAstaCustomAuditDataSetHack(ADataSet).RefetchFields.Assign(OffLineDialog.RefetchFields);
          TAstaCustomAuditDataSetHack(ADataSet).UpdateTableName:=OffLineDialog.e_updatetablename.Text;
          TAstaCustomAuditDataSetHack(ADataSet).AutoIncrementField:=OffLineDialog.e_AutoIncrement.Text;
        end
        else
        begin
          TAstaCustomClientSQLDataSetHack(ADataSet).PrimeFields.Assign(OffLineDialog.PrimeFields);
          TAstaCustomClientSQLDataSetHack(ADataSet).NoSQLFields.Assign(OffLineDialog.NoSQLFields);
          TAstaCustomClientSQLDataSetHack(ADataSet).RefetchFields.Assign(OffLineDialog.RefetchFields);
          TAstaCustomClientSQLDataSetHack(ADataSet).Database:=OffLineDialog.e_database.Text;
          TAstaCustomClientSQLDataSetHack(ADataSet).UpdateTableName:=OffLineDialog.e_updatetablename.Text;
          TAstaCustomClientSQLDataSetHack(ADataSet).AutoIncrementField:=OffLineDialog.e_AutoIncrement.Text;
        end;

        if GetComponent(0) is TAstaIOCLientTable then
          TAstaIOCLientTable(ADataSet).TableName:=OffLineDialog.e_tablename.Text;

        if Assigned(Self.Designer) then
          Self.Designer.Modified;
      end;
    finally
      OffLineDialog.Free;
    end;
  end;
end;

function TOffLineProperty.GetAttributes: TPropertyAttributes;
begin
  Result:=[paDialog];
end;

function TOffLineProperty.GetValue: String;
begin
  Result:=GetStrValue;
  Result:='Press ... to Display';
end;

procedure TOffLineProperty.SetValue(const Value: String);
begin
  SetStrValue(Value);
end;

{ TMetaDataProperty }

function TMetaDataProperty.GetAttributes: TPropertyAttributes;
begin
  Result:=[paValueList, paAutoUpdate, paMultiSelect];
end;

procedure TMetaDataProperty.GetValues(TheProc: TGetStrProc);
var MetaData        :TAstaIOMetaDataDataSet;
    AComp           :TComponent;
    TheFieldName    :String;
    IsExecSMethod   :Boolean;

begin
  AComp := nil;   // changed by EM, 22 May 2001
  MetaData:=TAstaIOMetaDataDataSet.Create(nil);
  try
    IsExecSMethod:=False;
    if GetComponent(0) is TAstaCustomClientSQLDataSet then
    begin
      AComp:=TAstaCustomClientSQLDataSet(GetComponent(0));
      if not Assigned(TAstaCustomClientSQLDataSet(AComp).AstaClientWire) then  DatabaseError(SNoAssignedSocket, AComp);
      MetaData.DataBase:=TAstaCustomClientSQLDataSet(AComp).DataBase;
      MetaData.AstaClientWire:=TAstaCustomClientSQLDataSet(AComp).AstaClientWire;
    end
    else
    if GetComponent(0) is TAstaIOExecServerMethod then
    begin
      IsExecSMethod:=True;
      AComp:=TAstaIOExecServerMethod(GetComponent(0));
      if not Assigned(TAstaIOExecServerMethod(AComp).AstaClientWire) then  DatabaseError(SNoAssignedSocket, AComp);
      MetaData.DataBase:=TAstaIOExecServerMethod(AComp).DataBase;
      MetaData.AstaClientWire:=TAstaIOExecServerMethod(AComp).AstaClientWire;
    end
    else
    if GetComponent(0) is TAstaParamsDataSet then
    begin
      AComp:=TAstaParamsDataSet(GetComponent(0));
      if not Assigned(TAstaParamsDataSet(AComp).AstaClientWire) then  DatabaseError(SNoAssignedSocket, AComp);
      MetaData.DataBase:=TAstaParamsDataSet(AComp).DataBase;
      MetaData.AstaClientWire:=TAstaParamsDataSet(AComp).AstaClientWire;
    end
    else
    if GetComponent(0) is TAstaIOClientTable then
    begin
      AComp:=TAstaIOClientTable(GetComponent(0));
      if not Assigned(TAstaIOClientTable(AComp).AstaClientWire) then  DatabaseError(SNoAssignedSocket, AComp);
      MetaData.DataBase:=TAstaIOClientTable(AComp).DataBase;
      MetaData.AstaClientWire:=TAstaIOClientTable(AComp).AstaClientWire;
    end
    else
    if GetComponent(0) is TAstaIOMetaDataDataSet then
    begin
      AComp:=TAstaIOMetaDataDataSet(GetComponent(0));
      if not Assigned(TAstaIOMetaDataDataSet(AComp).AstaClientWire) then  DatabaseError(SNoAssignedSocket, AComp);
      MetaData.DataBase:=TAstaIOMetaDataDataSet(AComp).DataBase;
      MetaData.AstaClientWire:=TAstaIOMetaDataDataSet(AComp).AstaClientWire;
    end
    else
    if GetComponent(0) is TAstaIOClientIProvider then
    begin
      AComp:=TAstaIOClientIProvider(GetComponent(0));
      if not Assigned(TAstaIOClientIProvider(AComp).AstaClientWire) then  DatabaseError(SNoAssignedSocket, AComp);
      MetaData.DataBase:=TAstaIOClientIProvider(AComp).DataBase;
      MetaData.AstaClientWire:=TAstaIOClientIProvider(AComp).AstaClientWire;
    end;
    if Uppercase(GetName)='DATABASE' then
    begin
      MetaData.MetaDataRequest:=mdDBMSName;
      TheFieldName:='Database';
    end
    else
    if (Uppercase(GetName)='UPDATETABLENAME')
      or (Uppercase(GetName)='TABLENAME') then
    begin
      MetaData.MetaDataRequest:=mdTables;
      TheFieldName:='TableName';
    end
    else
    if Uppercase(GetName)='STOREDPROCNAME' then
    begin
      MetaData.MetaDataRequest:=mdStoredProcs;
      TheFieldName:='SProcName';
    end
    else
    if Uppercase(GetName)='PROVIDERNAME' then
    begin
      MetaData.MetaDataRequest:=mdProviders;
      TheFieldName:='ProviderName';
    end
    else
    if Uppercase(GetName)='SERVERMETHODNAME' then
    begin
      if IsExecSMethod then
        MetaData.MetaDataRequest:=mdServerMethodsExec
      else
        MetaData.MetaDataRequest:=mdServerMethods;
      TheFieldName:='ServerMethod';
    end
    else
    if Uppercase(GetName)='IPROVIDERNAME' then
    begin
      MetaData.MetaDataRequest:=mdIProviders;
      TheFieldName:='IProviderName';
    end;
    if metadata.AstaClientWire=nil then raise exception.Create(SNoAssignedSocket)
     else begin
      MetaData.DesignTimeConnect;
      MetaData.Active:=True;
      end;
    while not MetaData.Eof do
    begin
      TheProc(Trim(MetaData.FieldByName(TheFieldName).AsString));
      MetaData.Next;
    end;
  finally
    MetaData.DesignTimeDisconnect;
    MetaData.Free;
 end;
 //if the tablename property editor is invoked close the DataSet
// changed by EM, 22 May 2001
  if Assigned(AComp) AND (Uppercase(GetName)='TABLENAME') then
    TAstaCustomClientSQLDataSet(AComp).Close;
end;

{ TFileNameProperty }

procedure TFileNameProperty.Edit;
var Dialog    :TOpenDialog;
begin
  Dialog:=TOpenDialog.Create(nil);
  try
    Dialog.Title:='Open file';

    Dialog.Filter:='Asta DataSet Files (*.ads)|*.ctd|Text files (*.txt)|*.txt|Compressed Files (*.cmp)|*.cmp)|XML Files (*.xml)|*.xml|Binary Files (*.bin)|*.bin|All files (*.*)|*.*';

    if Uppercase(GetName) = 'FILENAME' then
      Dialog.FilterIndex:=1;

    if Dialog.Execute then
      SetStrValue(Dialog.FileName);
  finally
    Dialog.Free;
  end;
end;

function TFileNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result:=[padialog];
end;

function TFileNameProperty.GetValue: String;
begin
  Result:=GetStrValue;
end;

procedure TFileNameProperty.SetValue(const Value: String);
begin
  SetStrValue(Value);
end;

{ TFieldsSelectProperty }

function TFieldsSelectProperty.GetValue :String;
begin
  if UpperCase(GetName) = 'PRIMEFIELDS' then Result:='(PrimeFields)'
  else
  if UpperCase(GetName) = 'NOSQLFIELDS' then Result:='(NoSQLFields)'
  else
  if UpperCase(GetName) = 'REFETCHFIELDS' then Result:='(RefetchFields)';
end;

procedure TFieldsSelectProperty.SetValue(const Value :String);
begin
  SetStrValue(Value);
end;

procedure TFieldsSelectProperty.Edit;
var ADataSet             :TAstaCustomClientSQLDataSet;
    FieldsSelectDlg      :TAstaIOFieldsSelectDialog;
    Fields               :TStrings;
    CapStr               :String;
    i                    :Integer;
begin
  if (GetComponent(0) is TAstaCustomClientSQLDataSet) or
     (GetComponent(0) is TAstaIOClientStoredProc) or
     (GetComponent(0) is TAstaIOProviderDataSet) then
  begin
    Fields:=TStringList.Create;   // changed by EM, 22 May 2001
    try
      ADataSet:=TAstaCustomClientSQLDataSet(GetComponent(0));
      if not ADataSet.Active then raise Exception.Create(SUseOffLine);

      FieldsSelectDlg:=TAstaIOFieldsSelectDialog.Create(nil);
      if UpperCase(GetName) = 'PRIMEFIELDS' then
      begin
        CapStr:='Prime Fields for ';
        Fields.Assign(TAstaCustomClientSQLDataSetHack(ADataSet).PrimeFields);
      end
      else
      if UpperCase(GetName) = 'NOSQLFIELDS' then
      begin
        CapStr:='NoSQL Fields for ';
        Fields.Assign(TAstaCustomClientSQLDataSetHack(ADataSet).NoSQLFields);
      end
      else
      if UpperCase(GetName) = 'REFETCHFIELDS' then
      begin
        CapStr:='Refetch Fields for ';
        Fields.Assign(TAstaCustomClientSQLDataSetHack(ADataSet).RefetchFields);
      end;

      FieldsSelectDlg.Caption:=CapStr + ADataSet.Name;
      try
        for i:=0 to ADataSet.FieldCount-1 do
        begin
          FieldsSelectDlg.clb_fields.Items.Add(ADataSet.Fields[i].FieldName);
            if Fields.IndexOf(ADataSet.Fields[i].FieldName) >=0 then
              FieldsSelectDlg.clb_fields.Checked[i]:=True;
        end;

        FieldsSelectDlg.ShowModal;
        if FieldsSelectDlg.ModalResult = mrOk then
        begin
          Fields.Clear;
          for i:=FieldsSelectDlg.clb_fields.Items.Count-1 downto 0 do
            if FieldsSelectDlg.clb_fields.Checked[i] then
              Fields.Add(FieldsSelectDlg.clb_fields.Items[i]);
          if Assigned(Self.Designer) then
            Self.Designer.Modified;
        end;
      finally
        FieldsSelectDlg.Free;
      end;
      if UpperCase(GetName) = 'PRIMEFIELDS' then
        TAstaCustomClientSQLDataSetHack(ADataSet).PrimeFields.Assign(Fields)
      else
      if UpperCase(GetName) = 'NOSQLFIELDS' then
        TAstaCustomClientSQLDataSetHack(ADataSet).NoSQLFields.Assign(Fields)
      else
      if UpperCase(GetName) = 'REFETCHFIELDS' then
        TAstaCustomClientSQLDataSetHack(ADataSet).RefetchFields.Assign(Fields);
    finally
      FIelds.Free;
    end;
  end;
end;

function TFieldsSelectProperty.GetAttributes:TPropertyAttributes;
begin
  Result:=[paDialog];
end;

{ TAutoIncSequenceProperty }

function TAutoIncSequenceProperty.GetAttributes: TPropertyAttributes;
begin
  Result:=[paValueList, paAutoUpdate, paMultiSelect];
end;

procedure TAutoIncSequenceProperty.GetValues(TheProc: TGetStrProc);
var ADataSet        :TAstaParamsDataSetHack;
    i               :Integer;
begin
  if (GetComponent(0) is TAstaParamsDataSet) then
  begin
    ADataSet:=TAstaParamsDataSetHack(GetComponent(0));
    if ADataSet.Fields.Count = 0 then exit;

    for i:=0 to ADataSet.Fields.Count-1 do
    begin
      if IsNumeric(ADataSet.Fields[i].DataType) then
        TheProc(Trim(ADataSet.Fields[i].FieldName));
    end;
  end;
end;

// TWorkBenchProperty

procedure TWorkBenchProperty.Edit;
var ADataSet      :TAstaCustomClientSQLDataSet;
    WasConnected  :Boolean;
    WorkBenchForm :TWorkBenchForm;
begin
  if GetComponent(0) is TAstaCustomClientSQLDataSet then
  begin
    ADataSet:=TAstaCustomClientSQLDataSet(GetComponent(0));
    if ADataSet.AstaClientWire = nil then Raise Exception.Create(SNoAssignedSocket);
    WorkBenchForm:=TWorkBenchForm.Create(nil);
    try
      WasConnected:=ADataSet.AstaClientWire.Active;
      if not WasConnected then
        ADataSet.DesignTimeConnect;
      try
        WorkBenchForm.q_data.AstaClientWire:=ADataSet.AstaClientWire;
        WorkBenchForm.tv_main.ClientWire:=ADataSet.AstaClientWire;
        WorkBenchForm.DataSet:=ADataSet;
        WorkBenchForm.Caption:='AstaIO WorkBench - Properties for ' + ADataSet.Name;

        if GetComponent(0) is TAstaIOClientQuery then
        begin
          WorkBenchForm.m_sql.Lines.Assign(TAstaIOClientQuery(GetComponent(0)).SQL);
        end
        else
        if GetComponent(0) is TAstaIOClientTable then
        begin
          WorkBenchForm.m_sql.Lines.Text:='SELECT * FROM ' + TAstaIOClientTable(GetComponent(0)).TableName;
          if TAstaIOClientTable(GetComponent(0)).SelectOptions.GroupBy <> '' then
            WorkBenchForm.m_sql.Lines.Add('GROUP BY ' + TAstaIOClientTable(GetComponent(0)).SelectOptions.GroupBy);

          if TAstaIOClientTable(GetComponent(0)).SelectOptions.Having <> '' then
            WorkBenchForm.m_sql.Lines.Add('HAVING ' + TAstaIOClientTable(GetComponent(0)).SelectOptions.Having);

          if TAstaIOClientTable(GetComponent(0)).SelectOptions.OrderBy <> '' then
            WorkBenchForm.m_sql.Lines.Add('ORDER BY ' + TAstaIOClientTable(GetComponent(0)).SelectOptions.OrderBy);
        end;

        WorkBenchForm.PrimeFields.Assign(TAstaCustomClientSQLDataSetHack(ADataSet).PrimeFields);
        WorkBenchForm.NoSQLFields.Assign(TAstaCustomClientSQLDataSetHack(ADataSet).NOSQLFields);
        WorkBenchForm.RefetchFields.Assign(TAstaCustomClientSQLDataSetHack(ADataSet).RefetchFields);
        WorkBenchForm.AutoIncrementField:=TAstaParamsDataSetHack(ADataSet).AutoIncrementField;
        WorkBenchForm.Sequence:=TAstaParamsDataSetHack(ADataSet).Sequence;
        WorkBenchForm.UpdateTableName:=TAstaCustomClientSQLDataSetHack(ADataSet).UpdateTableName;

        WorkBenchForm.ShowModal;
        if WorkBenchForm.ModalResult = mrOk then
        begin
          if Assigned(Self.Designer) then
            Self.Designer.Modified;
        end;
      finally
        if not WasConnected then
          TAstaCustomClientSQLDataSetHack(ADataSet).DesignTimeDisConnect;
      end;
    finally
      WorkBenchForm.Free;
    end;
  end;
end;

function TWorkBenchProperty.GetAttributes: TPropertyAttributes;
begin
  Result:=[paDialog];
end;

function TWorkBenchProperty.GetValue: String;
begin
  Result:=GetStrValue;
  Result:='Press ... to Display';
end;

procedure TWorkBenchProperty.SetValue(const Value: String);
begin
  SetStrValue(Value);
end;

{ TAstaFieldLink }
// **************** Property editor for Master / Detail Link fields **********************

procedure TAstaFieldLink.Edit;
begin

{  if FParent is TAstaIOClientTable then
  begin
    if Trim(TAstaIOClientTable(FParent).TableName)='' then
      DatabaseError(SNoTableName);
//    if not Assigned(TAstaIOClientTable(FParent).MasterSource) then
//      DatabaseError(SNoMSrcDSrc);
  end
  else
  if FParent is TAstaIOClientIProvider then
  begin
    if Trim(TAstaIOClientIProvider(FParent).IProviderName)='' then
      DatabaseError(SNoProviderName);
//    if not Assigned(TAstaIOClientIProvider(FParent).MasterSource) then
//      DatabaseError(SNoMSrcDSrc);
  end;}
  if not Assigned(TAstaIOCustomDatasetHack(FParent).MasterSource) then
    DatabaseError(SNoMSrcDSrc);

  inherited;
end;

function TAstaFieldLink.GetAttributes: TPropertyAttributes;
begin
  Result:=[paDialog, paAutoUpdate];
end;

procedure TAstaFieldLink.GetValues(TheProc: TGetStrProc);
begin
  inherited;
end;

procedure TAstaFieldLink.GetFieldNamesForIndex(List: TStrings);
begin
  inherited;
end;

function TAstaFieldLink.GetIndexBased: Boolean;
begin
  Result:=False;
end;

function TAstaFieldLink.GetIndexDefs: TIndexDefs;
begin
  Result:=TAstaIOCustomDatasetHack(FParent).IndexDefs

  {if FParent is TAstaIOClientTable then
    Result:=TAstaIOClientTable(FParent).IndexDefs
  else
  if FParent is TAstaIOClientIProvider then
    Result:=TAstaIOClientIProvider(FParent).IndexDefs}
end;

function TAstaFieldLink.GetIndexFieldNames: String;
begin
  Result:=TAstaIOCustomDatasetHack(FParent).FDetailFields

  {if FParent is TAstaIOClientTable then
    Result:=TAstaIOClientTable(FParent).DetailFields
  else
  if FParent is TAstaIOClientIProvider then
    Result:=TAstaIOClientIProvider(FParent).DetailFields}
end;

function TAstaFieldLink.GetIndexName: String;
begin
  Result:='';
end;

function TAstaFieldLink.GetMasterFields: String;
begin
  Result:=TAstaIOCustomDatasetHack(FParent).MasterFields

  {if FParent is TAstaIOClientTable then
    Result:=TAstaIOClientTable(FParent).MasterFields
  else
  if FParent is TAstaIOClientIProvider then
    Result:=TAstaIOClientIProvider(FParent).MasterFields;}
end;

procedure TAstaFieldLink.SetIndexFieldNames(const Value: String);
begin
  TAstaIOCustomDatasetHack(FParent).DetailFields:=Value

  {if FParent is TAstaIOClientTable then
    TAstaIOClientTable(FParent).DetailFields:=Value
  else
  if FParent is TAstaIOClientIProvider then
    TAstaIOClientIProvider(FParent).DetailFields:=Value}
end;

procedure TAstaFieldLink.SetIndexName(const Value: String);
begin
  TAstaIOCustomDatasetHack(FParent).IndexName:=Value

  {if FParent is TAstaIOClientTable then
    TAstaIOClientTable(FParent).IndexName:=Value
  else
  if FParent is TAstaIOClientIProvider then
    TAstaIOClientIProvider(FParent).IndexName:=Value}
end;

procedure TAstaFieldLink.SetMasterFields(const Value: String);
begin
  TAstaIOCustomDatasetHack(FParent).MasterFields:=Value

  {if FParent is TAstaIOClientTable then
    TAstaIOClientTable(FParent).MasterFields:=Value
  else
  if FParent is TAstaIOClientIProvider then
    TAstaIOClientIProvider(FParent).MasterFields:=Value}
end;

constructor TAstaFieldLink.CreateWith(ADataSet: TDataSet);
begin
  inherited;
end;

function TAstaFieldLink.GetParent: TAstaIOCustomDatasetHack;
begin
  Result:=TAstaIOCustomDatasetHack(GetComponent(0));
end;

function TAstaIndexFieldNamesProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList, paMultiSelect];
end;

procedure TAstaIndexFieldNamesProperty.GetValues(Proc: TGetStrProc);
var
  ds: TAstaIOCustomDataSet;
  i: Integer;
begin
  ds := GetComponent(0) as TAstaIOCustomDataSet;
  for i := 0 to ds.Indexes.Count - 1 do
    if ds.Indexes[i].Fields <> '' then
      Proc(ds.Indexes[i].Fields);
end;

function TAstaIndexNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList, paMultiSelect];
end;

procedure TAstaIndexNameProperty.GetValues(Proc: TGetStrProc);
var
  ds: TAstaIOCustomDataSet;
  i: Integer;
begin
  ds := GetComponent(0) as TAstaIOCustomDataSet;
  for i := 0 to ds.Indexes.Count - 1 do
    if ds.Indexes[i].Name <> '' then
      Proc(ds.Indexes[i].Name);
end;

function TAstaFilterProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes - [paAutoUpdate];
end;


end.
