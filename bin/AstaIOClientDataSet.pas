{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10071: AstaIOClientDataSet.pas 
{
{   Rev 1.0    4/10/2003 6:30:10 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:36:42 PM  Steve    Version: 1.505
}
unit AstaIOClientDataSet;
{*********************************************************}
{*   Copyright (c) 1997-2002 Asta Technology Group Inc.  *}
{*               All rights reserved.                    *}
{*               www.astatech.com                        *}
{*********************************************************}

{$I AstaIO.inc}

interface

uses
  SysUtils, Classes, DB,
   {$IFDEF FRAMEWORK_FMX }
     FMX.Dialogs,
   {$ELSE}
  VCL.Dialogs,
  {$ENDIF}
  AstaIOCustomDataSet, AstaIODBConst,
  AstaIOParamList,
  AstaIODataSetProvider,
  AstaIOSQLDataSet;

type
  TAstaIONestedDSManager = class;

  TAstaIORemoteEvent = procedure(Sender: TObject) of object;

  TAstaIOReconcileAction = (raSkip, raAbort, raMerge, raCorrect, raCancel, raRefresh);
  TAstaIOUpdateKind = dtEdit .. dtAppend; 
  TAstaIOReconcileEvent = procedure(DataSet: TAstaIODataSet; E: Exception;
    UpdateKind: TAstaIOUpdateKind; var Action: TAstaIOReconcileAction) of object;

  TAstaIOCustomClientDataSet = class(TAstaCustomAuditDataSet)
  private
    FProvider: TAstaIODataSetProvider;
    FProviderName: String;
    procedure SetProviderName(const Value: String);
    function GetProviderName: String;
    function GetData: IAstaIOData;
    procedure SetData(const Value: IAstaIOData);
    function GetDelta: IAstaIOData;
    function DoFetchPacket(ARecords: Integer; AReset: Boolean = False): Integer;
    procedure CheckProviderEOF;
    procedure SetDelta(const Value: IAstaIOData);
    { Private declarations }
  protected
    FBeforeApplyUpdates: TAstaIORemoteEvent;
    FAfterApplyUpdates: TAstaIORemoteEvent;
    FDetailCloneDataSet: TAstaIODataSet;
    FDetailCloneStream:TMemoryStream;
    FOnReconcileError: TAstaIOReconcileEvent;

    FStatusFilter: TUpdateStatusSet;
    FParamCheck: Boolean;
    FCommandText: TStrings;
    FFetchOnDemand: Boolean;
    FLogChanges: Boolean;
    FServerProviderOptions: TAstaIProviderOptionSet;
    FPacketRecords: Integer;

    // Block read
    FBlockBufSize: Integer;
    FBlockBufOfs: Integer;
    FBlockBufCount: Integer;
    FBlockReadCount: Integer;
    FProviderEOF: Boolean;
    FSourceEof: Boolean;
    FManager: TAstaIONestedDSManager;
    FDisableStringTrim: Boolean;
    procedure InternalTransfer(DataSet :TDataSet; StartFirst :Boolean);
//    procedure TransferFromSource;
    procedure DoBeforeInsert; override;
    procedure DoBeforeDelete; override;
    procedure DoBeforeEdit; override;
    procedure QueryChanged(Sender: TObject);
    procedure SetCommandText(Value: TStrings);
    procedure SourceChanged; override;
    procedure DoAfterOpen; override;
    procedure DoBeforeClose; override;
    procedure InternalOpen; override;
    procedure InternalClose; override;
    procedure InternalLast; override;
    procedure Execute; virtual;
    procedure FetchParams; virtual;
    function GetProviderEOF: Boolean;
    procedure SetProviderEOF(Value: Boolean);
    procedure SetProvider(Value: TAstaIODataSetProvider);
    function GetProvider :TAstaIODataSetProvider;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure AddNestedDataSetFields;
    procedure FetchNestedDetails;
//    function GetRecord(Buffer: PChar; GetMode: TGetMode; DoCheck: Boolean): TGetResult; override;
    function GetNextPacket :Integer; virtual;
    procedure SetBlockReadSize(Value: Integer); override;
    procedure SetPacketRecords(Value :Integer);
    procedure AppendData(const Data: TDataSet; HitEOF: Boolean); virtual;
    function InternalFetchAll(AForce: Boolean): Boolean; override;
    function InternalFetchPacket: Boolean; override;
    procedure InternalRefresh; override;
    function Reconcile: Boolean;
    // NestedDataSet stuff
    function GetParentDataSet: TAstaIOCustomDataset;
    procedure SetDataSetField(const Value: TDataSetField); override;
    procedure OpenCursor(InfoQuery: Boolean = False); override;
    procedure CheckParentDataSet; virtual;
    procedure LoadDataFromParentDataSet; virtual;
    procedure DataEvent(Event: TDataEvent; Info: Integer); override;
    function GetContentDefined: Boolean; virtual;
    procedure PostDataToParentDataSet; override;
    procedure PushRowData(Value: TDeltaType); override;
//    procedure Assign(Source: TPersistent); override;
    procedure FetchDetails; virtual;
    procedure GetNestedFieldDefs(DataSet :TDataSet);
//    function GetNextRecord: Boolean; override;
//    procedure SetNestingLevels;

    property ParentDataSet: TAstaIOCustomDataset read GetParentDataSet;
    property ContentDefined: Boolean read GetContentDefined;

    property CommandText: TStrings read FCommandText write SetCommandText;
    property ServerProviderOptions: TAstaIProviderOptionSet read FServerProviderOptions;
    property FetchOnDemand: Boolean read FFetchOnDemand write FFetchOnDemand;
    property LogChanges: Boolean read FLogChanges write FLogChanges;
    property PacketRecords: Integer read FPacketRecords write SetPacketRecords;
    property StatusFilter: TUpdateStatusSet read FStatusFilter write FStatusFilter;
    property ProviderEOF: Boolean read GetProviderEOF write SetProviderEOF;
    property DisableStringTrim: Boolean read FDisableStringTrim write FDisableStringTrim;
    property Data: IAstaIOData read GetData write SetData;
    property Delta: IAstaIOData read GetDelta write SetDelta;
    property BeforeApplyUpdates: TAstaIORemoteEvent read FBeforeApplyUpdates write FBeforeApplyUpdates;
    property AfterApplyUpdates: TAstaIORemoteEvent read FAfterApplyUpdates write FAfterApplyUpdates;
    property OnReconcileError: TAstaIOReconcileEvent read FOnReconcileError write FOnReconcileError;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function UpdateStatus :TUpdateStatus; override;
    procedure BlockReadNext; override;
    procedure FetchNestedForUpdates(var NestedList :TAstaParamList);
    function PrepareForUpdate :Boolean;
    function ApplyUpdates(MaxErrors: Integer = -1) :Integer;
    procedure CreateDataSet;
    procedure MergeChangeLog;
    function InitInfoProvider: TAstaIODataSetProvider;
    procedure FinishInfoProvider(AInfoProv: TAstaIODataSetProvider);

    property Manager :TAstaIONestedDSManager read FManager;
  published
    property Provider: TAstaIODataSetProvider read GetProvider write SetProvider;
    property ProviderName: String read GetProviderName write SetProviderName;
    property PrimeFields;
    property NoSQLFields;
    property RefetchFields;
    property UpdateTableName;
    property Sequence;
    property AutoIncrementField;
    property OffLine;
  end;

  TAstaIOClientDataSet = class(TAstaIOCustomClientDataSet)
  private
  protected
  public
    property Data;
property Delta;
    property StatusFilter;
    property LogChanges;
    procedure Execute; override;
    procedure FetchParams; override;
    function UpdateStatus :TUpdateStatus; override;
    function GetNextPacket :Integer; override;
    procedure AppendData(const Data: TDataSet; HitEOF: Boolean); override;
//    procedure FetchAll; //override;
    procedure FetchDetails; override;

    property ParentDataSet;
    property ContentDefined;
 published
    property DataSetField;
    property MasterFields;
    property MasterSource;
    property DetailFields;
    property IndexDefs;
    property Params;
    property CommandText;
    property FetchOnDemand;
    property PacketRecords;
    property DisableStringTrim;
    property BeforeApplyUpdates;
    property AfterApplyUpdates;
    property OnReconcileError;
  end;

  TAstaIONestedDSManager = class(TAstaIOCustomDataset)
  private
    FPrepared: Boolean; // Indicates if the oldvalues datasets have current values - if PrepareForUpdate was called
  protected
  public
    constructor Create(AOwner :TComponent); override;
    destructor Destroy; override;
    procedure Init;
    procedure PrepareDataSetForUpdate(NestedDataSet :TAstaIOCustomClientDataSet);

    property Prepared: Boolean read FPrepared write FPrepared;
  published
    { Published declarations }
  end;

const
  AllRecords = -1;

implementation

uses
  AstaIOResources, AstaIODBList, AstaIOConst;

type
  TAstaIOCustomDatasetHack = class(TAstaIOCustomDataset);
{ TAstaIOClientDataSet }
constructor TAstaIOCustomClientDataSet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FParamCheck:=True;
  FStoreDefs:=True;
  FCommandText:=TStringList.Create;
  TStringList(FCommandText).OnChange:=QueryChanged;
  FFetchOnDemand:=True;
  FLogChanges:=True;
  FPacketRecords:=AllRecords;
  NestedDataSetClass:=TAstaIOCustomClientDataSet;
  FDetailCloneDataSet:=TAstaIODataSet.Create(nil);
  FDetailCloneStream:=TMemoryStream.Create;
  FManager:=TAstaIONestedDSManager.Create(Self);
  FDisableStringTrim:=False;
end;

destructor TAstaIOCustomClientDataSet.Destroy;
begin
  Destroying;
  if ParentDataSet <> nil then
    ParentDataSet.RemoveNestedActivation(Self);
  FCommandText.Free;
  FDetailCloneDataSet.Free;
  FDetailCloneStream.Free;
  FManager.Free;
  inherited Destroy;
end;

procedure TAstaIOCustomClientDataSet.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = Provider) and (Operation = opRemove) then
    Provider:=nil;
end;

procedure TAstaIOCustomClientDataSet.GetNestedFieldDefs(DataSet: TDataSet);
var i, j   :Integer;
begin
  for i:=0 to NestedDataSets.Count - 1 do
  begin
    for j:=0 to Provider.DetailDataSets.Count - 1 do
    begin
      if TAstaIOCustomClientDataSet(DataSet).DataSetField.FullName = TDataSet(Provider.DetailDataSets[j]).Name then
      begin
        DataSet.FieldDefs.Assign(TDataSet(Provider.DetailDataSets[j]).FieldDefs);
        exit;
      end;
    end;
  end;
end;

procedure TAstaIOCustomClientDataSet.AddNestedDataSetFields;
var
  i: Integer;
begin
  for i := 0 to Provider.DetailDataSets.Count - 1 do
    TFieldDef.Create(FieldDefs, TDataSet(Provider.DetailDataSets[i]).Name,
      ftDataSet, 0, False, FieldDefs.Count);
end;

// Nested DataSet
function TAstaIOCustomClientDataSet.GetParentDataSet: TAstaIOCustomDataset;
begin
  if DataSetField <> nil then
    Result:=DataSetField.DataSet as TAstaIOCustomDataset
  else
    Result:=nil;
end;

// Nested DataSet
procedure TAstaIOCustomClientDataSet.SetDataSetField(const Value: TDataSetField);
begin
  if DataSetField <> Value then
  begin
    if ParentDataSet <> nil then
      TAstaIOCustomDatasetHack(ParentDataSet).RemoveNestedActivation(Self);
    if (Value <> nil) and (Value.DataSet <> nil) then
    begin
      // here we should setup properties of TAstaIONestedDataSet,
      // using propeties of parent dataset. For example, clientsocket ...
    end;
    inherited SetDataSetField(Value);
    if ParentDataSet <> nil then
      TAstaIOCustomDatasetHack(ParentDataSet).FHasNestedDataSets:=True;
  end;
end;

// Nested DataSet
procedure TAstaIOCustomClientDataSet.OpenCursor(InfoQuery: Boolean = False);
begin
  //CheckParentDataSet;
  if Assigned(ParentDataSet) then
  begin
    {if not ParentDataSet.Active then
    begin
      FOpening:=False;
      OpenParentDataSet(ParentDataSet);
    end;}
    if FieldDefs.Count = 0 then
    begin
      FOpening:=False;
      OpenParentDataSet(ParentDataSet);
    end;

    if not ParentDataSet.Active then
    begin
      TAstaIOCustomDatasetHack(ParentDataSet).RequestNestedActivation(Self);
      SetState(dsOpening);
    end
    else
    begin
      SetState(dsInactive);
      LoadDataFromParentDataSet;
      inherited OpenCursor(InfoQuery);
    end;
  end
  else
    inherited OpenCursor(InfoQuery);
  if Assigned(Provider) and not InfoQuery and not ProviderEOF then
  begin
    CursorPosChanged;
    Resync([]);
  end;
end;

// Nested DataSet
procedure TAstaIOCustomClientDataSet.CheckParentDataSet;
begin
  if (ParentDataSet = nil) and not Assigned(Provider) then
    raise Exception.Create(SDataSetFieldUndefined);
end;

// Nested DataSet
procedure TAstaIOCustomClientDataSet.LoadDataFromParentDataSet;
var
  m: TMemoryStream;
  BmInt: Integer;
begin
  if Assigned(Provider) then exit;
  CheckParentDataSet;
  BeginBatch(False);
  try
    ParentDataSet.UpdateCursorPos;
    if ContentDefined then
    begin
      m := ParentDataSet.GetMemoryStream(DataSetField);
      if FieldDefs.Count = 0 then
        InternalLoadFromStreamWithFields(m)
      else
        LoadFromStream(m);
      if Assigned(FOldValuesDataSet) then begin
        FOldValuesDataSet.Empty;
        if (FManager <> nil) and FManager.Active then begin
          BmInt := ParentDataSet.GetBookmarkAsInteger;
          if FManager.Locate('ParentBookmark', BmInt, [loCaseInsensitive]) then
            FOldValuesDataSet.LoadFromStringWithFields(FManager.FieldByName('OldValuesMemo').AsString);
        end;
      end;
    end
    else if Active and IsCursorOpen then
      Empty;
  finally
    ChangesSaved;
    EndBatch;
  end;
end;

// Nested DataSet
procedure TAstaIOCustomClientDataSet.DataEvent(Event: TDataEvent; Info: Integer);
begin
  if InBatch then exit;
  if (Event = deParentScroll) and not Assigned(Provider) and ParentDataSet.Active then
  begin
    LoadDataFromParentDataSet;
  end;

  inherited DataEvent(Event, Info);
end;

// Nested DataSet
function TAstaIOCustomClientDataSet.GetContentDefined: Boolean;
begin
  Result := (ParentDataSet.State in [dsBrowse, dsEdit, dsCalcFields, dsFilter,
    dsNewValue, dsOldValue, dsCurValue, dsBlockRead, dsInternalCalc]) and
    ParentDataSet.GetFieldData(DataSetField, nil);
end;

// Nested DataSet
procedure TAstaIOCustomClientDataSet.PostDataToParentDataSet;
var
  m: TMemoryStream;
begin
  if Assigned(Provider) then exit;
  CheckParentDataSet;
  if (ParentDataSet.State in [dsEdit, dsInsert]) and SaveChanges then
  begin
    DisableControls;
    try
      ParentDataSet.UpdateCursorPos;
      m := TMemoryStream.Create;
      try
        SaveToStream(m);
        ParentDataSet.StoreMemoryStream(DataSetField, m);
      finally
        m.Free;
      end;
      TAstaIOCustomDatasetHack(ParentDataSet).DataEvent(deFieldChange, Longint(DataSetField));
    finally
      ChangesSaved;
      EnableControls;
    end;
  end;
end;

// Nested DataSet
procedure TAstaIOCustomClientDataSet.PushRowData(Value: TDeltaType);
var BmInt :Integer;
begin
  inherited PushRowData(Value);
  if Assigned(Provider) then exit;

  if not Assigned(ParentDataSet) then exit;

  if not Assigned(FManager) then exit;
  if not FManager.Active then FManager.Init;

  BmInt:=ParentDataSet.GetBookmarkAsInteger;
  if not FManager.Locate('ParentBookmark', BmInt, [loCaseInsensitive]) then
  begin
    FManager.AppendRecord([BmInt, Name, DataSetToString(FOldValuesDataSet)]);
  end
  else
  begin
    FManager.Edit;
    FManager.FieldByName('OldValuesMemo').AsString:=DataSetToString(FOldValuesDataSet);
    FManager.Post;
  end;
end;

// Nested DataSet
procedure TAstaIOCustomClientDataSet.Assign(Source: TPersistent);
begin
  if Assigned(Provider) then exit;
  if Source is TDataSetField then
    DataSetField:=TDataSetField(Source)
  else
    inherited Assign(Source);
end;

// Nested DataSet
function TAstaIOCustomClientDataSet.PrepareForUpdate :Boolean;
var
  i: Integer;
begin
//  if Assigned(Provider) then exit;
  Result := True;
  if not Assigned(ParentDataSet) then begin
    for i := 0 to NestedDataSets.Count - 1 do
      if TDataSet(NestedDataSets[i]) is TAstaIOCustomClientDataSet then
        if not TAstaIOCustomClientDataSet(NestedDataSets[i]).PrepareForUpdate then begin
          Result := False;
          Exit;
        end;
  end
  else if Assigned(FManager) then begin
    if FManager.Active then
    FManager.PrepareDataSetForUpdate(Self);
  end
  else
    Result := False;
end;

procedure TAstaIOCustomClientDataSet.FetchNestedDetails;
var
  i: Integer;
begin
  for i:=0 to Provider.DetailDataSets.Count - 1 do
  begin
    if FindField(TDataSet(Provider.DetailDataSets[i]).Name) <> nil then
    begin
      FDetailCloneStream.Clear;
      FDetailCloneDataSet.CleanCloneFromDataSet(TDataSet(Provider.DetailDataSets[i]), True);
      FDetailCloneDataSet.SaveToStream(FDetailCloneStream);
      FDetailCloneStream.Position:=0;
      StoreMemoryStream(FieldByName(TDataSet(Provider.DetailDataSets[i]).Name), FDetailCloneStream);
    end;
  end;
end;

procedure TAstaIOCustomClientDataSet.FetchNestedForUpdates(var NestedList: TAstaParamList);
var i   :Integer;
    NestedOldValues :TAstaIOCustomDataset;
    NestedCurrentValues :TAstaIOCustomDataset;
begin
  NestedList:=nil;
  NestedOldValues:=TAstaIOCustomDataset.Create(nil);
  NestedCurrentValues:=TAstaIOCustomDataset.Create(nil);
  try
    for i := 0 to NestedDataSets.Count - 1 do
    begin
      if not (TDataSet(NestedDataSets[i]) as TAstaIOCustomClientDataSet).PrepareForUpdate then Continue;

      (TDataSet(NestedDataSets[i]) as TAstaIOCustomClientDataSet).Manager.First;
      while not (TDataSet(NestedDataSets[i]) as TAstaIOCustomClientDataSet).Manager.Eof Do
      begin
        if (TDataSet(NestedDataSets[i]) as TAstaIOCustomClientDataSet).Manager.FieldByName('DataSetName').AsString = (TDataSet(NestedDataSets[i]) as TAstaIOCustomClientDataSet).Name then
        begin
          NestedOldValues.Close;
          NestedOldValues.LoadFromStringWithFields((TDataSet(NestedDataSets[i]) as TAstaIOCustomClientDataSet).Manager.FieldByName('OldValuesMemo').AsString);
          NestedOldValues.Open;

          NestedCurrentValues.Close;
          NestedCurrentValues.LoadFromStringWithFields((TDataSet(NestedDataSets[i]) as TAstaIOCustomClientDataSet).Manager.FieldByName('CurrentValuesMemo').AsString);
          NestedCurrentValues.Open;

          {TAstaIOSQLGenerator(TheGenerator).OldValuesDataSet:=TAstaIODataSet(NestedOldValues);
          TAstaIOSQLGenerator(TheGenerator).CurrentDataSet:=TAstaIODataSet(NestedCurrentValues);
          TAstaIOSQLGenerator(TheGenerator).PrimeFields.Assign((TDataSet(NestedDataSets[i]) as TAstaIOCustomClientDataSet).PrimeFields);
          TAstaIOSQLGenerator(TheGenerator).RefetchFields.Assign((TDataSet(NestedDataSets[i]) as TAstaIOCustomClientDataSet).RefetchFields);
          TAstaIOSQLGenerator(TheGenerator).NoSQLFields.Assign((TDataSet(NestedDataSets[i]) as TAstaIOCustomClientDataSet).NoSQLFields);
          TAstaIOSQLGenerator(TheGenerator).UpdateTableName:=(TDataSet(NestedDataSets[i]) as TAstaIOCustomClientDataSet).UpdateTableName;
          TAstaIOSQLGenerator(TheGenerator).AutoIncrementField:=(TDataSet(NestedDataSets[i]) as TAstaIOCustomClientDataSet).AutoIncrementField;
          TAstaIOSQLGenerator(TheGenerator).Sequence:=(TDataSet(NestedDataSets[i]) as TAstaIOCustomClientDataSet).Sequence;

          NestedList := TAstaIOSQLGenerator(TheGenerator).GenerateClientSideSQL(Self, False, False);
          }
        end;
        (TDataSet(NestedDataSets[i]) as TAstaIOCustomClientDataSet).Manager.Next;
      end;
    end;
  finally
    NestedOldValues.Free;
    NestedCurrentValues.Free;
  end;
end;

procedure TAstaIOCustomClientDataSet.FetchDetails;
var
  OldUpdateMethod: TAstaUpdateMethod;
begin
  if not Assigned(Provider) then exit;
  OldUpdateMethod := FUpdateMethod;
  FUpdateMethod := umManual;
  try
    if not Provider.LocateRecord(Provider.DataSet, Self) then exit;
    Edit;
    FetchNestedDetails;
    Post;
  finally
    FUpdateMethod := OldUpdateMethod;
  end;
  Refresh;
end;

{ Get / Set props }
procedure TAstaIOCustomClientDataSet.FetchParams;
begin
  if Assigned(Provider) then
  begin
    FParams.Assign(Provider.GetParams);
  end;
end;

procedure TAstaIOCustomClientDataSet.SetCommandText(Value: TStrings);
begin
  FCommandText.Assign(Value);
end;

procedure TAstaIOCustomClientDataSet.QueryChanged(Sender: TObject);
var
  List: TParams;
  i: Integer;
begin
  if not (csReading in ComponentState) then
  begin
    if FParamCheck or (csDesigning in ComponentState) then
    begin
      List:=TParams.Create(Self);
      try
        List.ParseSQL(FCommandText.Text, True);
        List.AssignValues(Params);
        Params.Clear;
        Params.Assign(List);
      finally
        List.Free;
      end;
    end;
    DataEvent(dePropertyChange, 0);
  end
  else
    Params.ParseSQL(FCommandText.Text, False);

  if FParamCheck then
    for i:=0 to Params.Count - 1 do
      Params[i].ParamType:=DB.ptInput;
end;

function TAstaIOCustomClientDataSet.GetProvider: TAstaIODataSetProvider;
var
  ProvComp: TComponent;
  DS: TObject;
begin
  if (FProvider = nil) and (FProviderName <> '') then begin
    if Owner <> nil then begin
      ProvComp := Owner.FindComponent(FProviderName);
      if (ProvComp <> nil) and (ProvComp is TAstaIODataSetProvider) then begin
        DS := TAstaIODataSetProvider(ProvComp).DataSet;
        if (DS <> nil) and (DS = Self) then
          DatabaseError(SNoCircularReference, Self);
        FProvider := TAstaIODataSetProvider(ProvComp);
      end;
    end;
  end;
  Result := FProvider;
end;

procedure TAstaIOCustomClientDataSet.SetProvider(Value: TAstaIODataSetProvider);
begin
  if FProvider <> Value then begin
    if FProvider <> nil then
      FProvider.ClientDataSets.Remove(Self);
    FProvider := Value;
    FProviderName := '';
    if FProvider <> nil then begin
      FProviderName := FProvider.Name;
      FProvider.FreeNotification(Self);
      FProvider.AddClientDataSet(Self);
    end;
  end;
end;

function TAstaIOCustomClientDataSet.GetProviderName: String;
begin
  if (Provider <> nil) and (FProviderName <> Provider.Name) then
    FProviderName := Provider.Name;
  Result := FProviderName;
end;

procedure TAstaIOCustomClientDataSet.SetProviderName(const Value: String);
begin
  if ProviderName <> Value then begin
    Provider := nil;
    FProviderName := Value;
  end;
end;

function TAstaIOCustomClientDataSet.GetProviderEOF: Boolean;
begin
  if Assigned(Provider) then
    FProviderEOF := Provider.DataSet.Eof;
  Result := FProviderEOF;
end;

procedure TAstaIOCustomClientDataSet.SetProviderEOF(Value: Boolean);
begin
  FProviderEOF:=Value;
  if Assigned(Provider) then
    Provider.DataSet.Last;
end;

procedure TAstaIOCustomClientDataSet.SetPacketRecords(Value: Integer);
begin
  FPacketRecords:=Value;
end;

function TAstaIOCustomClientDataSet.GetData: IAstaIOData;
begin
  Result := TAstaIODataObj.Create(Self, False);
end;

procedure TAstaIOCustomClientDataSet.SetData(const Value: IAstaIOData);
var
  fieldsExclude: TStrings;
  prevUpdateMethod: TAstaUpdateMethod;
begin
  if Value <> nil then begin
    fieldsExclude := TStringList.Create;
    prevUpdateMethod := UpdateMethod;
    UpdateMethod := umManual;
    try
      fieldsExclude.Add(sfld_BookMark);
      fieldsExclude.Add(sfld_Delta);
      fieldsExclude.Add(sfld_SavePoint);
      CleanCloneFromDataSet(Value.DataSet, True, True, fieldsExclude);
    finally
      UpdateMethod := prevUpdateMethod;
      fieldsExclude.Free;
    end;
  end
  else begin
    Close;
    DisposeAstaList;
  end;
end;

function TAstaIOCustomClientDataSet.GetDelta: IAstaIOData;
begin
  Result := TAstaIODataObj.Create(FOldValuesDataSet, True);
end;

procedure TAstaIOCustomClientDataSet.SetDelta(const Value: IAstaIOData);
begin
  if Value <> nil then begin
    if FOldValuesDataSet = nil then SetupDeltaDataSet;
    FOldValuesDataSet.CleanCloneFromDataSet(Value.DataSet, True, True, nil);
  end
  else begin
    EmptyCache;
  end;
end;

{ Create / open / close / execute }

procedure TAstaIOCustomClientDataSet.CreateDataSet;
begin
  Open;
end;

function TAstaIOCustomClientDataSet.InitInfoProvider: TAstaIODataSetProvider;
var
  parDS, curDS: TAstaIOCustomDataSet;
  path: TList;
begin
  parDS := ParentDataSet;
  curDS := Self;
  path := TList.Create;
  Result := nil;
  try
    path.Insert(0, Pointer(TAstaIOCustomDatasetHack(parDS).NestedDataSets.IndexOf(curDS)));
    while (parDS <> nil) and (TAstaIOCustomClientDataSet(parDS).Provider = nil) do begin
      curDS := parDS;
      parDS := ParentDataSet;
      path.Insert(0, Pointer(TAstaIOCustomDatasetHack(parDS).NestedDataSets.IndexOf(curDS)));
    end;
    if parDS <> nil then begin
      Result := TAstaIOCustomClientDataSet(parDS).Provider;
      Result.SetInfoDataSetPath(path);
    end;
  finally
    path.Free;
  end;
end;

procedure TAstaIOCustomClientDataSet.FinishInfoProvider(AInfoProv: TAstaIODataSetProvider);
begin
  if AInfoProv <> nil then
    AInfoProv.SetInfoDataSetPath(nil);
end;

procedure TAstaIOCustomClientDataSet.InternalOpen;
var
  sKeyFields: String;
  i: Integer;
  infoProv: TAstaIODataSetProvider;
begin
  sKeyFields := '';
  if (FAstaList <> nil) and (FAstaList.Count > 0) then begin
    FProviderEOF := True;
    FSourceEof := True;
  end
  else begin
    if Assigned(Provider) then
    begin
      if Length(FCommandText.Text) > 0 then
      begin
        if (poAllowCommandText in Provider.Options) then
            Provider.SetCommandText(FCommandText.Text)
        else
          DatabaseError(SProvCannotChangeCommandText, Self);
      end;
      FSourceEof := False;
      DisposeAstaList;
      Provider.SetParams(FParams);
      Provider.Open(Self);
      FieldDefs.Clear;
      FieldDefs.Assign(Provider.DataPacket.FieldDefs);
      sKeyFields := Provider.PSGetKeyFields;
    end
    else if Assigned(DataSetField) then begin
      TAstaIOCustomClientDataSet(ParentDataSet).GetNestedFieldDefs(Self); // ???
      infoProv := InitInfoProvider;
      if infoProv <> nil then
        try
          sKeyFields := infoProv.PSGetKeyFields;
        finally
          FinishInfoProvider(infoProv);
        end;
    end;
  end;
  if sKeyFields <> '' then begin
    PrimeFields.Clear;
    i := 1;
    while i <= Length(sKeyFields) do
      PrimeFields.Add(ExtractFieldName(sKeyFields, i));
  end;
  inherited InternalOpen;
  if sKeyFields <> '' then begin
    i := 1;
    while i <= Length(sKeyFields) do
      with FieldByName(ExtractFieldName(sKeyFields, i)) do
        ProviderFlags := ProviderFlags + [pfInKey];
  end;
end;

procedure TAstaIOCustomClientDataSet.DoAfterOpen;
begin
  inherited DoAfterOpen;
  if Assigned(Provider) then
  begin
    try
      SuspendEvents := True;
      if (FPacketRecords <> 0) and (RecordCount = 0) then
        GetNextPacket;
      First;
    finally
      SuspendEvents := False;
    end;
  end;
//  EmptyCache;
//  Resync([]);
end;

procedure TAstaIOCustomClientDataSet.DoBeforeClose;
begin
  inherited DoBeforeClose;
  if Assigned(Provider) then
  begin
    if Fields.Count = 0 then exit;
    Provider.Close;
    Empty;
  end;
end;

procedure TAstaIOCustomClientDataSet.InternalClose;
begin
  if Assigned(FManager) and FManager.Active then
  begin
    FManager.First;
    While not FManager.Eof do
    begin
      if FManager.FieldByName('DataSetName').AsString = Name then
        FManager.Delete;
      FManager.Next;
    end;
  end;
  inherited InternalClose;
end;

procedure TAstaIOCustomClientDataSet.Execute;
begin
  if Assigned(Provider) then
    Provider.ExecuteStatement(FCommandText.Text, FParams, nil);
end;

procedure TAstaIOCustomClientDataSet.SourceChanged;
begin
  if not Active then exit;
  Refresh;
end;

{ Updates }

procedure TAstaIOCustomClientDataSet.DoBeforeEdit;
begin
  if Assigned(Provider) then
    if TrackDeltas and (poDisableEdits in Provider.Options) then
      DatabaseError(SPRovDataSetNoEdit, Self);

  inherited DoBeforeEdit;
end;

procedure TAstaIOCustomClientDataSet.DoBeforeDelete;
begin
  if Assigned(Provider) then
    if TrackDeltas and (poDisableDeletes in Provider.Options) then
      DatabaseError(SPRovDataSetNoDelete, Self);

  inherited DoBeforeDelete;
end;

procedure TAstaIOCustomClientDataSet.DoBeforeInsert;
begin
  if Assigned(Provider) then
    if TrackDeltas and (poDisableInserts in Provider.Options) then
      DatabaseError(SPRovDataSetNoInsert, Self);

  inherited DoBeforeInsert;
end;

function TAstaIOCustomClientDataSet.UpdateStatus: TUpdateStatus;
begin
  Result:=usUnmodified;
  if not RowHasBeenChanged then exit;
  if State = dsInsert then
    Result:=usInserted
  else
  case OldValuesdataSet.FieldbyName(sfld_Delta).AsInteger of
    Ord(dtAppend) :Result:=usInserted;
    Ord(dtEdit)   :Result:=usModified;
    Ord(dtDelete) :Result:=usDeleted;
  end;
end;

function TAstaIOCustomClientDataSet.ApplyUpdates(MaxErrors: Integer = -1) :Integer;
begin
  if Assigned(FBeforeApplyUpdates) then
    FBeforeApplyUpdates(Self);

  Result:=0;
  if not Assigned(Provider) then exit;
  if not PrepareForUpdate then exit;

  Provider.DisableStringTrim := FDisableStringTrim;
  Provider.ApplyUpdates(Self, MaxErrors, Result);

  if Result > 0 then
    Reconcile
  else
    MergeChangeLog;

  if Assigned(FAfterApplyUpdates) then
    FAfterApplyUpdates(Self);
end;

procedure TAstaIOCustomClientDataSet.MergeChangeLog;
begin
  EmptyCache;
end;

function TAstaIOCustomClientDataSet.Reconcile: Boolean;
var
  {$IFDEF Delphi2009AndUp}
  bmk: TBookmark;
  {$ELSE}
  bmk: TBookmarkStr;
  {$ENDIF}
  tempDS: TAstaIODataSet;
  prevUpdateMethod: TAstaUpdateMethod;
  E: Exception;
  Action: TAstaIOReconcileAction;
begin
  if not Assigned(FOnReconcileError) then begin
    MergeChangeLog;
    Result := True;
    Exit;
  end;

  bmk := Bookmark;
  prevUpdateMethod := UpdateMethod;
  UpdateMethod := umManual;
  tempDS := TAstaIODataSet.Create(nil);
  try

    tempDS.DisposeAstaList;
    tempDS.FieldDefs.Assign(FieldDefs);
    tempDS.Open;
    FOldValuesDataSet.First;

    while not FOldValuesDataSet.EOF do begin
      if not FOldValuesDataSet.FieldByName(sfld_ErrorCode).IsNull then begin

        tempDS.Empty;
        case FOldValuesDataSet.FieldByName(sfld_Delta).AsInteger of
          Ord(dtEdit):
            begin
              tempDS.Append;
              tempDS.CloneSingleRecordFromSource(FOldValuesDataSet, True);
              MoveToBookMarkFromDelta;
              tempDS.Append;
              tempDS.CloneSingleRecordFromSource(Self, True);
            end;
          Ord(dtDelete):
            begin
              tempDS.Append;
              tempDS.CloneSingleRecordFromSource(FOldValuesDataSet, True);
              tempDS.Append;
              tempDS.Post;
            end;
          Ord(dtAppend):
            begin
              tempDS.Append;
              tempDS.Post;
              MoveToBookMarkFromDelta;
              tempDS.Append;
              tempDS.CloneSingleRecordFromSource(Self, True);
            end;
        end;

        E := Exception.Create(FOldValuesDataSet.FieldByName(sfld_ErrorInfo).AsString);
        try
          FOnReconcileError(tempDS, E, TAstaIOUpdateKind(FOldValuesDataSet.FieldByName(sfld_Delta).AsInteger), Action);
        finally
          E.Free;
        end;

        case Action of
        raSkip:
          FOldValuesDataSet.Next;
        raAbort:
          Break;
        raRefresh:
          FOldValuesDataSet.Delete; // refetch record ???
        raMerge:
          FOldValuesDataSet.Delete;
        raCorrect:
          begin
            if FOldValuesDataSet.FieldByName(sfld_Delta).AsInteger in [Ord(dtEdit), Ord(dtAppend)] then begin
              tempDS.RecNo := 2;
              CloneSingleRecordFromSource(tempDS, True);
            end;
            FOldValuesDataSet.Next;
          end;
        raCancel:
          RevertRecord;
        end;
      end
      else
        FOldValuesDataSet.Delete;
    end;

  finally
    tempDS.Free;
    UpdateMethod := prevUpdateMethod;
    Bookmark := bmk;
  end;

  Result := (FOldValuesDataSet.RecordCount = 0);
end;

{ Fetching }

procedure TAstaIOCustomClientDataSet.InternalTransfer(DataSet: TDataSet;StartFirst:Boolean);
var
  i: Integer;
  srcFld, destFld: TField;
begin
  if DataSet.Eof then exit;
  BeginBatch(False);
  DataSet.DisableControls;
  try
    if StartFirst then DataSet.First;
    while not DataSet.Eof do
    begin
      BeginDirectAppend;
      try
        for i := 0 to DataSet.FieldCount - 1 do
        begin
          srcFld := DataSet.Fields[i];
          if i < FieldCount then begin
            destFld := Fields[i];
            if AnsiCompareText(srcFld.FieldName, destFld.FieldName) <> 0 then
              destFld := nil;
          end
          else
              destFld := nil;
          if destFld = nil then
            destFld := FieldByName(srcFld.FieldName);
          if (srcFld.DataType <> ftDataSet) or (poFetchDetailsOnDemand in Provider.Options) then
            DBFieldsAssign(destFld, srcFld);
        end;
      finally
        EndDirectWrite;
      end;
      DataSet.Next;
    end;
  finally
    DataSet.EnableControls;
    EndBatch;
  end;
end;

procedure TAstaIOCustomClientDataSet.AppendData(const Data: TDataSet; HitEOF: Boolean);
begin
  InternalTransfer(Data,True);
end;

function TAstaIOCustomClientDataSet.DoFetchPacket(ARecords: Integer; AReset: Boolean): Integer;
var
  options: TAstaIOGetRecordOptions;
begin
  Result := 0;
  if not Assigned(Provider) then exit;
  if ARecords = 0 then exit;
  if FSourceEof and not AReset then exit;
  options := [];
  if AReset then
    options := options + [grReset];
  Provider.GetRecords(ARecords, Result, options, '', FParams);
  FSourceEof := (Provider.ReachedEof); // and (Result > 0);
  AppendData(Provider.DataPacket, True);
end;

function TAstaIOCustomClientDataSet.GetNextPacket: Integer;
begin
  CheckActive;
  Result := DoFetchPacket(FPacketRecords);
end;

procedure TAstaIOCustomClientDataSet.CheckProviderEOF;
begin
  if not InBatch and not ProviderEOF and FFetchOnDemand and (FPacketRecords <> 0) then
    DoFetchPacket(AllRecords);
end;

function TAstaIOCustomClientDataSet.InternalFetchAll(AForce: Boolean): Boolean;
var
  n: Integer;
begin
  n := RecordCount;
  CheckProviderEOF;
  Result := (n < RecordCount);
end;

procedure TAstaIOCustomClientDataSet.InternalLast;
begin
  CheckProviderEOF;
  inherited InternalLast;
end;

function TAstaIOCustomClientDataSet.InternalFetchPacket: Boolean;
begin
  Result := (DoFetchPacket(FPacketRecords) > 0);
end;

procedure TAstaIOCustomClientDataSet.InternalRefresh;
var
  RecCount, pos: Integer;
begin
  if InBatch then exit;
  CheckBrowseMode;
  if ChangeCount > 0 then
    DatabaseError(SRefreshError, Self);
  BeginBatch;
  pos := FIndexes.Position;
  try
    if not ProviderEOF then
      RecCount := FAstaList.Count
    else
      RecCount := AllRecords;
    Empty;
    DoFetchPacket(RecCount, True);
  finally
    FIndexes.Position := pos;
    EndBatch;
  end;
end;

{ Block reading }

procedure TAstaIOCustomClientDataSet.BlockReadNext;
begin
  if FBlockBufOfs >= FBlockBufCount - 1 then
  begin
    if FBlockBufCount < FBlockBufSize then
      Last
    else
    begin
      GetNextPacket;
      if (FBlockBufCount = 0) then
        Last;
      Inc(FBlockReadCount, FBlockBufCount);
      FBlockBufOfs:=0;
    end
  end
  else
  begin
    MoveBy(1);
    Inc(FBlockBufOfs);
  end;
end;

procedure TAstaIOCustomClientDataSet.SetBlockReadSize(Value: Integer);
const
  DEFBLOCKSIZE  = 64 * 1024;
var
  Size: Integer;
begin
  if Value <> BlockReadSize then
  begin
    if Value > 0 then
    begin
      if EOF then Exit;
      UpdateCursorPos;
      inherited SetBlockReadSize(Value);
      SetState(dsBrowse);

      if Value = MaxInt then
        Size:=DEFBLOCKSIZE
      else
        Size:=Value * FRawBufSize;

      FBlockBufSize:=Size div FRawBufSize;
      FBlockBufOfs:=FBlockBufSize;
      FBlockBufCount:=FBlockBufSize;
      FBlockReadCount:=0;

      BlockReadNext;
    end
    else
    if Active then
    begin
      inherited SetBlockReadSize(Value);
      SetState(dsBrowse);
    end;
  end
end;

{ TAstaIONestedDSManager }

constructor TAstaIONestedDSManager.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPrepared:=False;
end;

destructor TAstaIONestedDSManager.Destroy;
begin

  inherited Destroy;
end;

procedure TAstaIONestedDSManager.Init;
begin
  Empty;
  Close;
  NukeAllFieldInfo;
  AddField('ParentBookmark', ftInteger, 0, True);
  AddField('DataSetName', ftString, 60, True);
  AddField('OldValuesMemo', ftMemo, 0, False);
  AddField('CurrentValuesMemo', ftMemo, 0, False);
  Open;
  FPrepared:=False;
end;

procedure TAstaIONestedDSManager.PrepareDataSetForUpdate(NestedDataSet :TAstaIOCustomClientDataSet);
var BmInt :Integer;
    Delta :Integer;
    {$IFDEF Delphi12AndUp}
    ParentBM :TBookmark;
    {$ELSE}
    ParentBM :TBookmarkStr;
    {$ENDIF}
    ParentDataSetFieldDS: TAstaIOCustomDataset;
    NestedOldValues: TAstaIOCustomDataset;
    DeltaCurrentValues: TAstaIOCustomDataset;
    srcField, destField: TField;

    m: TMemoryStream;
    OldValuesStream: TMemoryStream;
    i, j :Integer;

  function MoveParentToBookMarkFromDelta(DataSet :TAstaIOCustomDataset; BmInt :Integer): Boolean;
  {$IFDEF Delphi12AndUp}
  var Bm :TBytes;
  begin
    SetLength(Bm, SizeOf(Integer));
    move(BmInt, Bm[0], SizeOf(Integer));
    DataSet.BookMark := Bm;
    result := True;
  end;
  {$ELSE}
  var Bm :string;
  begin
    SetLength(Bm, SizeOf(Integer));
    move(BmInt, Bm[1], SizeOf(Integer));
    DataSet.BookMark := Bm;
    result := True;
  end;
  {$ENDIF}

begin
  // We need to go to each "current" dataset and get the current values
  // Put these values/datasets into the CurrentValuesMemo field

  ParentDataSetFieldDS:=TAstaIOCustomDataset.Create(nil);
  NestedOldValues:=TAstaIOCustomDataset.Create(nil);
  DeltaCurrentValues:=TAstaIOCustomDataset.Create(nil);
  OldValuesStream:=TMemoryStream.Create;
  try
    NestedDataSet.ParentDataSet.DisableControls;
    ParentBM:=NestedDataSet.ParentDataSet.Bookmark;
    DisableControls;
    try
      First;
      While not Eof do // We need a LocateNext
      begin
        if FieldByName('DataSetName').AsString = NestedDataSet.Name then
        begin
          // Need to go to the parent record
          // Get the equivalent of DeltaDataSetCurrentValueDataSet and put it in CurrentValuesMemo field
          // Now we have the old values and the current values match

          BmInt:=FieldByName('ParentBookMark').AsInteger;
          //if MoveParentToBookMarkFromDelta(BmInt) then
          MoveParentToBookMarkFromDelta(NestedDataSet.ParentDataSet, BmInt);
          begin
            // Get the Old values
            //OldValuesStream.Clear;
            //StringToStream(FieldByName('OldValuesMemo').AsString, OldValuesStream);
            //OldValuesStream.Position:=0;
            //NestedOldValues.LoadFromStreamWithFields(OldValuesStream);
            NestedOldValues.LoadFromStringWithFields(FieldByName('OldValuesMemo').AsString);

            // Get the Current dataset
            m:=NestedDataSet.ParentDataSet.GetMemoryStream(NestedDataSet.DataSetField);
            m.Position:=0;
            ParentDataSetFieldDS.LoadFromStreamWithFields(m);
            DeltaCurrentValues.Close;
            DeltaCurrentValues.NukeAllFieldInfo;
            for j:=0 to ParentDataSetFieldDS.Fields.Count - 1 do
              DeltaCurrentValues.AddField(ParentDataSetFieldDS.Fields[j].FieldName,
                                          ParentDataSetFieldDS.Fields[j].DataType,
                                          ParentDataSetFieldDS.Fields[j].Size);
            DeltaCurrentValues.AddField(sfld_BookMark, ftInteger, 0);
            DeltaCurrentValues.Open;


            // Now that we have what we need, get the current values, equivalent of DeltaDataSetCurrentValueDataSet
            NestedOldValues.First;
            while not NestedOldValues.Eof do
            begin
              Delta:=NestedOldValues.FieldByName(sfld_Delta).AsInteger;
              if Delta <> Ord(dtDelete) then
              begin
                BmInt:=NestedOldValues.FieldByName(sfld_BookMark).AsInteger;
                if MoveParentToBookMarkFromDelta(ParentDataSetFieldDS, BmInt) then
                begin
                  DeltaCurrentValues.Append;
                  for i := 0 to ParentDataSetFieldDS.Fields.Count - 1 do
                  begin
                    srcField := ParentDataSetFieldDS.Fields[i];
                    destField := DeltaCurrentValues.FieldByName(srcField.FieldName);
                    DBFieldsAssign(destField, srcField);
                  end;
                  DeltaCurrentValues.FieldByName(sfld_BookMark).AsInteger:=BmInt;
                  DeltaCurrentValues.Post;
                end;
              end;
              NestedOldValues.Next;
            end;
            Edit;
            FieldByName('CurrentValuesMemo').AsString:=DataSetToString(TAstaIODataSet(DeltaCurrentValues));
            Post;
          end;
        end;
        Next;
      end;
      FPrepared:=True;

    finally
      EnableControls;
      NestedDataSet.ParentDataSet.EnableControls;
      NestedDataSet.ParentDataSet.Bookmark:=ParentBM;
    end;
  finally

    ParentDataSetFieldDS.Free;
    NestedOldValues.Free;
    OldValuesStream.Free;
    DeltaCurrentValues.Free;
  end;
end;

{ TAstaIOClientDataSet }

procedure TAstaIOClientDataSet.FetchParams;
begin
  inherited FetchParams;
end;

function TAstaIOClientDataSet.UpdateStatus: TUpdateStatus;
begin
  Result:=inherited UpdateStatus;
end;

function TAstaIOClientDataSet.GetNextPacket: Integer;
begin 
  Result:=inherited GetNextPacket;
end;

procedure TAstaIOClientDataSet.AppendData(const Data: TDataSet; HitEOF: Boolean);
begin
  inherited AppendData(Data, HitEOF);
end;

procedure TAstaIOClientDataSet.FetchDetails;
begin
  inherited FetchDetails;
end;

procedure TAstaIOClientDataSet.Execute;
begin
  inherited Execute;
end;

end.
