{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10117: AstaIODataSetProvider.pas 
{
{   Rev 1.0    4/10/2003 6:30:38 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:37:02 PM  Steve    Version: 1.505
}
unit AstaIODataSetProvider;

{*********************************************************}
{*   Copyright (c) 1997-2002 Asta Technology Group Inc.  *}
{*               All rights reserved.                    *}
{*               www.astatech.com                        *}
{*********************************************************}

interface

uses
  DB,
  SysUtils,
  Classes,
  {$IFDEF WIN32}
  Dialogs,
  {$ENDIF}
 {$IFNDEF VER130}
  Variants,
 {$ENDIF}
  AstaIODBConst,
  AstaIOParamList,
  AstaIOSQLGenerator,
  AstaIOCustomDataSet,
  AstaIOSQLParser;

type
  TProviderEvent = procedure(Sender: TObject) of object;
  TGetParamsEvent = procedure(Sender: TObject) of object;
  TSetParamsEvent = procedure(Sender: TObject; Params: TParams) of object;
  TEndTransactionEvent = procedure(Self: TObject; Commit: Boolean) of object;

  TOpenEvent = procedure(Sender: TObject; var Params: TParams) of object;
  TBeforeUpdateEvent = procedure(Sender: TObject; OriginalValueDataSet, CurrentValueDataSet: TAstaIODataSet; var Handled: Boolean) of object;
  TAfterUpdateEvent = procedure(Sender: TObject; OriginalValueDataSet, CurrentValueDataSet : TAstaIODataSet) of object;

  TBeforeUpdateRecordEvent = procedure(Sender: TObject; OriginalValueDataSet: TAstaIODataSet; CurrentValueDataSet: TAstaIODataSet; UpdateKind: TDeltaType; var Applied: Boolean) of object;
  TAfterUpdateRecordEvent = procedure(Sender: TObject; OriginalValueDataSet: TAstaIODataSet; CurrentValueDataSet: TAstaIODataSet; UpdateKind: TDeltaType) of object;
  TGetTableNameEvent = procedure(Sender: TObject; DataSet: TDataSet; var TableName: String) of object;
  TOnGetDataEvent = procedure(Sender: TObject; DataSet: TDataSet) of object;

  TBeforeDeleteEvent = procedure(Sender: TObject; OriginalValueDataSet, ServerValueDataSet: TAstaIODataSet; var Handled: Boolean) of object;
  TBeforeInsertEvent = procedure(Sender: TObject; CurrentValueDataSet : TAstaIODataSet; var Handled: Boolean) of object;
  TAfterDeleteEvent = procedure(Sender: TObject; OriginalValueDataSet : TAstaIODataSet) of object;
  TAfterInsertEvent = procedure(Sender: TObject; CurrentValueDataSet : TAstaIODataSet) of object;
  TBeforeTransactionEvent = procedure(Sender: TObject; OriginalValueDataSet, CurrentValueDataSet: TAstaIODataSet; var Handled: Boolean) of object;
  TAfterTransactionEvent = procedure(Sender: TObject; CurrentValueDataSet: TAstaIODataSet; TransactionFailed: Boolean) of object;


type
  IAstaIOData = interface({$ifdef Delphi6AndUp} IInterface{$else} IUnknown{$endif})
    function GetDataSet: TAstaIOCustomDataSet;
    property DataSet: TAstaIOCustomDataSet read GetDataSet;
  end;

  TAstaIODataObj = class(TInterfacedObject, IAstaIOData)
  private
    FDataSet: TAstaIOCustomDataSet;
    FOwned: Boolean;
  protected
    function GetDataSet: TAstaIOCustomDataSet;
  public
    constructor Create(ADataSet: TAstaIOCustomDataSet; AOwned: Boolean);
    destructor Destroy; override;
  end;

type
  TAstaIOCustomDataSetProvider = class;

  TAstaIOCustomResolver = class
  private
    { Private declarations }
  protected
    FProvider :TAstaIOCustomDataSetProvider;
    FSQLGenerator :TAstaIOSQLGenerator;
    FSQLList :TAstaParamList;
    procedure ApplyUpdates(ADataSet :TDataSet; MaxErrors: Integer; var ErrorCount :Integer); virtual;
    procedure BeginUpdate; virtual;
    procedure EndUpdate; virtual;

    property SQLGenerator :TAstaIOSQLGenerator read FSQLGenerator write FSQLGenerator;
  public
    constructor Create(AProvider :TAstaIOCustomDataSetProvider); virtual;
    destructor Destroy; override;
    procedure GetTableName(DataSet :TDataSet; var TableName :String);

    property Provider :TAstaIOCustomDataSetProvider read FProvider write FProvider;
  end;

  TAstaIODataSetResolver = class(TAstaIOCustomResolver)
  private
    procedure BaseApplyUpdates(ADataSet :TDataSet; MaxErrors: Integer; var ErrorCount :Integer);
  protected
    procedure ApplyUpdates(ADataSet :TDataSet; MaxErrors: Integer; var ErrorCount :Integer); override;
  public
    { Public declarations }
  end;

  TAstaIOSQLResolver = class(TAstaIOCustomResolver)
  private
    procedure ApplyDetailUpdates(ASender: TObject; ADataSet: TDataSet; ASQL: TAstaParamList);
  protected
    procedure ApplyUpdates(ADataSet :TDataSet; MaxErrors: Integer; var ErrorCount :Integer); override;
  public
    property SQLGenerator;
  end;

  TAstaIOCustomDataSetProvider = class(TComponent)
  private
    FBeforeExecEvent: TProviderEvent;
    FAfterExecEvent: TProviderEvent;

    FBeforeGetParamsEvent: TGetParamsEvent;
    FAfterGetParamsEvent: TGetParamsEvent;

    FBeforeSetParamsEvent: TSetParamsEvent;
    FAfterSetParamsEvent: TSetParamsEvent;

    FBeforeEndTransactionEvent: TEndTransactionEvent;
    FAfterEndTransactionEvent: TEndTransactionEvent;

    FBeforeStartTransactionEvent: TProviderEvent;
    FAfterStartTransactionEvent: TProviderEvent;

    FBeforeRowRequest: TProviderEvent;
    FAfterRowRequest: TProviderEvent;

    FBeforeUpdateRecordEvent: TBeforeUpdateRecordEvent;
    FAfterUpdateRecordEvent: TAfterUpdateRecordEvent;
    FOnGetTableNameEvent: TGetTableNameEvent;
    FOnGetDataEvent: TOnGetDataEvent;
    FBeforeGetRecords :TProviderEvent;
    FAfterGetRecords :TProviderEvent;
    FOnGetData: TProviderEvent;
    FOnUpdateData: TProviderEvent;
    FAbout: String;
    FNoSQLFields: TStrings;
    FExported: Boolean;
    FDisableStringTrim: Boolean;
    FUseNULLSyntax    :Boolean;
    procedure SetDataSet(const Value: TDataSet);
  protected
    FConstraints: Boolean;
    FUpdateMode: TUpdateMode;
    FDataSet: TDataSet;
    FInfoDataSet: TDataSet;
    FDataPacket: TAstaIODataSet;
    FOptions: TAstaIProviderOptionSet;
    FRecordsSent: Integer;
    FResolver: TAstaIOCustomResolver;
    FCommandText: WideString;
    FActive: Boolean;
    FResolveToDataSet: Boolean;
    FParser: TAstaIOSQLParser;
    FDetailDataSets: TList;
    FClientDataSets: TList;
    FParams: TParams;
    FReachedEof: Boolean;

    FBeforeOpenEvent :TOpenEvent;
    FAfterOpenEvent :TOpenEvent;
    FAfterApplyUpdates: TAfterUpdateEvent;
    FBeforeApplyUpdates: TBeforeUpdateEvent;
    FDetailCloneDataSet: TAstaIODataSet;
    FDetailCloneStream:TMemoryStream;

    function LocateRecord(Source, Delta: TDataSet) :Boolean; virtual;
    procedure UpdateRecord(Source, Delta: TDataSet; BlobsOnly, KeyOnly: Boolean); virtual;
    //procedure FetchDetails(Source, Delta: TDataSet); virtual;
    procedure UpdateSourceRecord(Source, OldDelta, NewDelta: TDataSet; DeltaType :TDeltaType); virtual;
    procedure SetResolveToDataSet(Value: Boolean);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure ApplyUpdates(ADataSet :TDataSet; MaxErrors: Integer; var ErrorCount :Integer); virtual;
    procedure CheckDataSet;
    procedure SetActive(Value: Boolean);
    procedure OpenDetailDataSets(DetailList :TList);
    procedure CloseDetailDataSets;
    procedure ParseSQL(SQL: string);
    function AddClientDataSet(ADataSet :TDataSet) :Integer; virtual;
    procedure RemoveClientDataSet(ADataSet :TDataSet); virtual;
    procedure InternalExecute(const CommandText: WideString; var Params: OleVariant);
    function ExecuteStatement(const ASQL: string; AParams: TParams; ResultSet: Pointer = nil): Integer; virtual;
    procedure Execute; virtual;
    function GetKeyFields: string; virtual;
    function PSGetIndexDefs(IndexTypes: TIndexOptions = [ixPrimary..ixNonMaintained]): TIndexDefs; virtual;
    function PSGetKeyFields: String; virtual;
    function GetParams: TParams; virtual;
    function GetQuoteChar: string; virtual;
    function GetTableName: string; virtual;
    function InTransaction: Boolean; virtual;
    procedure SetParams(AParams: TParams); virtual;
    procedure StartTransaction; virtual;
    function FindRecord(Source, Delta: TDataSet; UpdateMode: TUpdateMode): Boolean;
    procedure EndTransaction(Commit: Boolean); virtual;
    function InternalGetRecords(Count :Integer; var RecsOut :Integer; Options :TAstaIOGetRecordOptions;
                                const CommandText :WideString; var Params :TParams): IAstaIOData;

    function GetRecords(Count :Integer; var RecsOut :Integer; Options :TAstaIOGetRecordOptions;
                        const CommandText :WideString; Params :TParams): IAstaIOData; overload; virtual;
    function GetRecords(Count :Integer; var RecsOut :Integer; Options :TAstaIOGetRecordOptions): IAstaIOData; overload;
    procedure FetchNestedDetails;
    procedure TransferDataToPacket;
    function UpdatableField(Field: TField): Boolean;
    procedure SetNoSQLFields(Value: TStrings);
    function GetNoSQLFields: TStrings;

    {Events}
    property BeforeExec: TProviderEvent read FBeforeExecEvent write FBeforeExecEvent;
    property AfterExec: TProviderEvent read FAfterExecEvent write FAfterExecEvent;
    property BeforeGetParams: TGetParamsEvent read FBeforeGetParamsEvent write FBeforeGetParamsEvent;
    property AfterGetParams: TGetParamsEvent read FAfterGetParamsEvent write FAfterGetParamsEvent;
    property BeforeSetParams: TSetParamsEvent read FBeforeSetParamsEvent write FBeforeSetParamsEvent;
    property AfterSetParams: TSetParamsEvent read FAfterSetParamsEvent write FAfterSetParamsEvent;
    property BeforeEndTransaction: TEndTransactionEvent read FBeforeEndTransactionEvent write FBeforeEndTransactionEvent;
    property AfterEndTransaction: TEndTransactionEvent read FAfterEndTransactionEvent write FAfterEndTransactionEvent;
    property BeforeStartTransaction: TProviderEvent read FBeforeStartTransactionEvent write FBeforeStartTransactionEvent;
    property AfterStartTransaction: TProviderEvent read FAfterStartTransactionEvent write FAfterStartTransactionEvent;
    property BeforeRowRequest: TProviderEvent read FBeforeRowRequest write FBeforeRowRequest;
    property AfterRowRequest: TProviderEvent read FAfterRowRequest write FAfterRowRequest;
    property BeforeUpdateRecord: TBeforeUpdateRecordEvent read FBeforeUpdateRecordEvent write FBeforeUpdateRecordEvent;
    property AfterUpdateRecord: TAfterUpdateRecordEvent read FAfterUpdateRecordEvent write FAfterUpdateRecordEvent;
    property OnGetTableName :TGetTableNameEvent read FOnGetTableNameEvent write FOnGetTableNameEvent;
    property OnGetData: TOnGetDataEvent read FOnGetDataEvent write FOnGetDataEvent;
    property BeforeGetRecords :TProviderEvent read FBeforeGetRecords write FBeforeGetRecords;
    property AfterGetRecords :TProviderEvent read FAfterGetRecords write FAfterGetRecords;
    property ResolveToDataSet: Boolean read FResolveToDataSet write SetResolveToDataSet;
    property UpdateMode: TUpdateMode read FUpdateMode write FUpdateMode default upWhereKeyOnly;
    property DataSet: TDataSet read FDataSet write SetDataSet;
    property InfoDataSet: TDataSet read FInfoDataSet write FInfoDataSet;
    property Options: TAstaIProviderOptionSet read FOptions write FOptions;
    property Constraints: Boolean read FConstraints write FConstraints;
    property Resolver: TAstaIOCustomResolver read FResolver write FResolver;
    property Exported: Boolean read FExported write FExported;
    property DetailDataSets: TList read FDetailDataSets;
    property ClientDataSets: TList read FClientDataSets;
    property Active: Boolean read FActive write SetActive;
    property CommandText: WideString read FCommandText;
    property ReachedEof: Boolean read FReachedEof;
    property DisableStringTrim: Boolean read FDisableStringTrim write FDisableStringTrim;
    property NoSQLFields: TStrings read GetNoSQLFields write SetNoSQLFields;
  public
    property UseNULLSyntax :Boolean read FUseNULLSyntax write FUseNULLSyntax;
    constructor Create(AComponent: TComponent); override;
    destructor Destroy; override;
    procedure SetCommandText(const CommandText: WideString); virtual;

    procedure Open(ClientDataSet :TDataSet = nil); virtual;
    procedure Close;
    procedure Next;
    procedure Prior;
    procedure First;
    procedure Last;
    procedure Reset;
    procedure SetInfoDataSetPath(APath: TList);

    property DataPacket: TAstaIODataSet read FDataPacket;
  published
    property About: String read FAbout write FAbout;
  end;

  TAstaIODataSetProvider = class(TAstaIOCustomDataSetProvider)
  public
    constructor Create(AComponent: TComponent); override;
    destructor Destroy; override;
    procedure EndTransaction(Commit: Boolean); override;
    function LocateRecord(Source, Delta: TDataSet) :Boolean; override;
    procedure UpdateRecord(Source, Delta: TDataSet; BlobsOnly, KeyOnly: Boolean); override;
    procedure UpdateSourceRecord(Source, OldDelta, NewDelta: TDataSet; DeltaType :TDeltaType); override;
    //procedure FetchDetails(Source, Delta: TDataSet); override;
    procedure ApplyUpdates(ADataSet :TDataSet; MaxErrors: Integer; var ErrorCount :Integer); override;
    function ExecuteStatement(const ASQL: string; AParams: TParams; ResultSet: Pointer = nil): Integer; override;
    procedure Execute; override;
    function GetKeyFields: string; override;
    function PSGetIndexDefs(IndexTypes: TIndexOptions = [ixPrimary..ixNonMaintained]): TIndexDefs; override;
    function PSGetKeyFields: String; override;
    function GetParams: TParams; override;
    function GetQuoteChar: string; override;
    function GetTableName: string; override;
    function InTransaction: Boolean; override;
    procedure SetParams(AParams: TParams); override;
    procedure StartTransaction; override;
    function AddClientDataSet(ADataSet :TDataSet) :Integer; override;
    procedure RemoveClientDataSet(ADataSet :TDataSet); override;
    function GetRecords(Count :Integer; var RecsOut :Integer; Options :TAstaIOGetRecordOptions;
                        const CommandText :WideString; Params :TParams): IAstaIOData; overload; override;
    function GetRecords(Count :Integer; var RecsOut :Integer; Options :TAstaIOGetRecordOptions): IAstaIOData; overload;
    property Active;
    property DetailDataSets;
    property ClientDataSets;
    property ReachedEof;
    property DisableStringTrim;
  published
    property BeforeOpen :TOpenEvent read FBeforeOpenEvent write FBeforeOpenEvent;
    property AfterOpen :TOpenEvent read FAfterOpenEvent write FAfterOpenEvent;
    property AfterApplyUpdates: TAfterUpdateEvent read FAfterApplyUpdates write FAfterApplyUpdates;
    property BeforeApplyUpdates: TBeforeUpdateEvent read FBeforeApplyUpdates write FBeforeApplyUpdates;
    property BeforeUpdateRecord;
    property AfterUpdateRecord;
    property OnGetTableName;
    property OnGetData;
    property BeforeGetRecords;
    property AfterGetRecords;

    property BeforeExec;
    property AfterExec;
    property BeforeGetParams;
    property AfterGetParams;
    property BeforeSetParams;
    property AfterSetParams;
    property BeforeEndTransaction;
    property AfterEndTransaction;
    property BeforeStartTransaction;
    property AfterStartTransaction;
    property BeforeRowRequest;
    property AfterRowRequest;

    property ResolveToDataSet;
    property Options;
    property Exported;
    property DataSet;
    property UpdateMode;
  end;

implementation
uses AstaIOResources,
     AstaIOConst,
     AstaIOClientDataSet;

type
  TDataSetHack = class(TDataSet);
  TAstaCustomAuditDataSetHack = class(TAstaCustomAuditDataSet);

constructor TAstaIODataObj.Create(ADataSet: TAstaIOCustomDataSet;
  AOwned: Boolean);
begin
  inherited Create;
  FDataSet := ADataSet;
  FOwned := AOwned;
end;

destructor TAstaIODataObj.Destroy;
begin
  if FOwned then begin
    FDataSet.Free;
    FDataSet := nil;
  end;
  inherited Destroy;
end;

function TAstaIODataObj.GetDataSet: TAstaIOCustomDataSet;
begin
  Result := FDataSet;
end;

{ TAstaIODataSetProvider }

constructor TAstaIODataSetProvider.Create(AComponent: TComponent);
begin
  inherited Create(AComponent);
end;

destructor TAstaIODataSetProvider.Destroy;
begin
  if Assigned(FResolver) then
    FResolver.Free;
  inherited Destroy;
end;

function TAstaIODataSetProvider.LocateRecord(Source, Delta: TDataSet) :Boolean;
begin
  Result:=inherited LocateRecord(Source, Delta);
end;

procedure TAstaIODataSetProvider.UpdateSourceRecord(Source, OldDelta, NewDelta: TDataSet; DeltaType :TDeltaType);
begin
  inherited UpdateSourceRecord(Source, OldDelta, NewDelta, DeltaType);
end;

procedure TAstaIODataSetProvider.UpdateRecord(Source, Delta: TDataSet; BlobsOnly, KeyOnly: Boolean);
begin
  inherited UpdateRecord(Source, Delta, BlobsOnly, KeyOnly);
end;

procedure TAstaIODataSetProvider.ApplyUpdates(ADataSet: TDataSet; MaxErrors: Integer; var ErrorCount: Integer);
begin
  inherited ApplyUpdates(ADataSet, MaxErrors, ErrorCount);
end;

procedure TAstaIODataSetProvider.EndTransaction(Commit: Boolean);
begin
  if Assigned(FBeforeEndTransactionEvent) then FBeforeEndTransactionEvent(Self, Commit);
  inherited EndTransaction(Commit);
  if Assigned(FAfterEndTransactionEvent) then FAfterEndTransactionEvent(Self, Commit);
end;

{procedure TAstaIODataSetProvider.FetchDetails(Source, Delta: TDataSet);
begin
  inherited FetchDetails(Source, Delta);
end;}

function TAstaIODataSetProvider.ExecuteStatement(const ASQL: string; AParams: TParams; ResultSet: Pointer): Integer;
begin
  Result := inherited ExecuteStatement(ASQL, AParams, ResultSet);
end;

procedure TAstaIODataSetProvider.Execute;
begin
  inherited Execute;
  if Assigned(FAfterExecEvent) then FAfterExecEvent(Self);
end;

function TAstaIODataSetProvider.GetKeyFields: string;
begin
  Result:=inherited GetKeyFields;
end;

function TAstaIODataSetProvider.PSGetIndexDefs(
  IndexTypes: TIndexOptions): TIndexDefs;
begin
  Result:=inherited PSGetIndexDefs(IndexTypes);
end;

function TAstaIODataSetProvider.PSGetKeyFields: String;
begin
  Result:=inherited PSGetKeyFields;
end;

function TAstaIODataSetProvider.GetParams: TParams;
begin
  if Assigned(FBeforeGetParamsEvent) then FBeforeGetParamsEvent(Self);
  Result:=inherited GetParams;
  if Assigned(FAfterGetParamsEvent) then FAfterGetParamsEvent(Self);
end;

function TAstaIODataSetProvider.GetQuoteChar: string;
begin
  Result:=inherited GetQuoteChar;
end;

function TAstaIODataSetProvider.GetTableName: string;
begin
  Result:=inherited GetTableName;
end;

function TAstaIODataSetProvider.InTransaction: Boolean;
begin
  Result:=inherited InTransaction;
end;

procedure TAstaIODataSetProvider.SetParams(AParams: TParams);
begin
  if Assigned(FBeforeSetParamsEvent) then FBeforeSetParamsEvent(Self, AParams);
  inherited SetParams(AParams);
  if Assigned(FAfterSetParamsEvent) then FAfterSetParamsEvent(Self, AParams);
  FParams.Assign(AParams);
end;

procedure TAstaIODataSetProvider.StartTransaction;
begin
  if Assigned(FBeforeStartTransactionEvent) then FBeforeStartTransactionEvent(Self);
  inherited StartTransaction;
  if Assigned(FAfterStartTransactionEvent) then FAfterStartTransactionEvent(Self);
end;

function TAstaIODataSetProvider.AddClientDataSet(ADataSet :TDataSet) :Integer;
begin
  Result:=inherited AddClientDataSet(ADataSet);
end;

procedure TAstaIODataSetProvider.RemoveClientDataSet(ADataSet :TDataSet);
begin
  inherited RemoveClientDataSet(ADataSet);
end;

function TAstaIODataSetProvider.GetRecords(Count: Integer; var RecsOut: Integer; Options: TAstaIOGetRecordOptions;
                                           const CommandText: WideString; Params: TParams): IAstaIOData;
begin
  Result := inherited GetRecords(Count, RecsOut, Options, CommandText, Params);
end;


function TAstaIODataSetProvider.GetRecords(Count: Integer; var RecsOut: Integer; Options: TAstaIOGetRecordOptions): IAstaIOData;
begin
  Result := inherited GetRecords(Count, RecsOut, Options);
end;

{ TAstaIOCustomDataSetProvider }

procedure TAstaIOCustomDataSetProvider.ParseSQL(SQL: string);
begin
  FParser.SQL.Text:=SQL;
  FParser.Deconstruct;

  if (poReadOnly in FOptions) and (FParser.SQLStatementType in [stUpdate, stDelete, stInsert, stAlter, stCreate]) then
    DatabaseError(SPRovDataSetReadOnly);

  if (poDisableInserts in FOptions) and (FParser.SQLStatementType in [stInsert]) then
    DatabaseError(SPRovDataSetNoInsert);

  if (poDisableDeletes in FOptions) and (FParser.SQLStatementType in [stDelete]) then
    DatabaseError(SPRovDataSetNoDelete);

  if (poDisableEdits in FOptions) and (FParser.SQLStatementType in [stUpdate]) then
    DatabaseError(SPRovDataSetNoEdit);
end;


procedure TAstaIOCustomDataSetProvider.ApplyUpdates(ADataSet :TDataSet; MaxErrors: Integer; var ErrorCount :Integer);
begin
  if Assigned(FResolver) then FResolver.Free;
  if not Assigned(TAstaCustomAuditDataSetHack(ADataSet).OldValuesDataSet) then exit;
  if TAstaCustomAuditDataSetHack(ADataSet).ChangeCount = 0 then exit;

  if FResolveToDataSet then
  begin
    if not FDataSet.Active then FDataSet.Open;
    FResolver:=TAstaIODataSetResolver.Create(Self);
    FResolver.ApplyUpdates(ADataSet, MaxErrors, ErrorCount);
  end
  else
  begin
    FResolver:=TAstaIOSQLResolver.Create(Self);
    FResolver.ApplyUpdates(ADataSet, MaxErrors, ErrorCount);
  end;
end;

procedure TAstaIOCustomDataSetProvider.Close;
begin
  CheckDataSet;
  try
    FDataSet.Close;
    FDataPacket.Close;
  finally
    FActive:=FDataSet.Active;
  end;
  CloseDetailDataSets;
end;

constructor TAstaIOCustomDataSetProvider.Create(AComponent: TComponent);
begin
  inherited Create(AComponent);
  FExported:=True;
  FConstraints:=True;
  FResolveToDataSet:=False;
  FUpdateMode:=upWhereKeyOnly;
  FDetailDataSets:=TList.Create;
  FClientDataSets:=TList.Create;
  FParser:=TAstaIOSQLParser.Create(nil);
  FParams:=TParams.Create(Self);
  FDataPacket:=TAstaIODataSet.Create(nil);
  FDetailCloneDataSet:=TAstaIODataSet.Create(nil);
  FDetailCloneStream:=TMemoryStream.Create;
  FDisableStringTrim:=False;
  FNoSQLFields:=TStringList.Create;
  FUseNULLSyntax:=True;
end;

destructor TAstaIOCustomDataSetProvider.Destroy;
begin
  FDetailDataSets.Free;
  FClientDataSets.Free;
  FParser.Free;
  FParams.Free;
  FDataPacket.Free;
  FDetailCloneDataSet.Free;
  FDetailCloneStream.Free;
  FNoSQLFields.Free;
  inherited Destroy;
end;

{procedure TAstaIOCustomDataSetProvider.FetchDetails(Source, Delta: TDataSet);
var
  i: Integer;
  Field: TField;
begin
  Source.First;
  while not Source.EOF do
  begin
    Delta.Insert;
    for i:=0 to Delta.FieldCount - 1 do
    begin
      Field:=Source.FindField(Delta.Fields[i].FieldName);
      if Field <> nil then
        Delta.Fields[i].Assign(Field);
    end;
    Delta.Post;
    Source.Next;
  end;
end;}

procedure TAstaIOCustomDataSetProvider.First;
begin
  CheckDataSet;
  FDataSet.First;
end;

procedure TAstaIOCustomDataSetProvider.Last;
begin
  CheckDataSet;
  FDataSet.Last;
end;

function TAstaIOCustomDataSetProvider.LocateRecord(Source, Delta: TDataSet) :Boolean;
begin
  Result:=FindRecord(Source, Delta, FUpdateMode);
end;

procedure TAstaIOCustomDataSetProvider.Next;
begin
  CheckDataSet;
  FDataSet.Next;
end;

procedure TAstaIOCustomDataSetProvider.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = FDataSet) and (Operation = opRemove) then
    DataSet := nil;
end;

procedure TAstaIOCustomDataSetProvider.Open(ClientDataSet :TDataSet);
var
  i: Integer;
begin
  CheckDataSet;
  FReachedEof := False;
  if Assigned(FBeforeOpenEvent) then
    FBeforeOpenEvent(Self, FParams);
  try
    SetParams(FParams);
    FDataSet.Open;
    FDataPacket.Close;
    FDataPacket.FieldDefs.Assign(DataSet.FieldDefs);
    TDataSetHack(FDataSet).GetDetailDataSets(FDetailDataSets);
    for i := 0 to FDetailDataSets.Count - 1 do
      FDataPacket.FieldDefs.Add(TDataSet(FDetailDataSets[i]).Name, ftDataSet, 0, False);
    FDataPacket.Open;
    FReachedEof := FDataPacket.Eof;
  finally
    FActive := FDataSet.Active;
  end;
  OpenDetailDataSets(FDetailDataSets);
  if Assigned(FAfterOpenEvent) then
    FAfterOpenEvent(Self, FParams);
end;

procedure TAstaIOCustomDataSetProvider.Prior;
begin
  CheckDataSet;
  FDataSet.Prior;
end;

procedure TAstaIOCustomDataSetProvider.Reset;
begin
  CheckDataSet;
  IProviderSupport(DataSet).PSReset;
end;

procedure TAstaIOCustomDataSetProvider.CheckDataSet;
begin
  if not Assigned(FDataSet) then DatabaseError(SMissingDataSet);
end;

procedure TAstaIOCustomDataSetProvider.SetResolveToDataSet(Value: Boolean);
begin
  FResolveToDataSet := Value;
end;

procedure TAstaIOCustomDataSetProvider.UpdateRecord(Source, Delta: TDataSet; BlobsOnly, KeyOnly: Boolean);
var
  Field: TField;
  i: Integer;
  UseUpMode: TUpdateMode;
begin
  if KeyOnly then
    UseUpMode:=upWhereKeyOnly
  else
    UseUpMode:=UpdateMode;

  if not FindRecord(Source, Delta, UseUpMode) then
    DatabaseError(SRecordChanged);
  begin
    if not FindRecord(Source, Delta, upWhereKeyOnly) then
      DatabaseError(SRecordChanged);
    with Delta do
    begin
      Edit;
      for i:=0 to FieldCount - 1 do
      begin
        Field:=Source.FindField(Fields[i].FieldName);
        if (Field <> nil) and (not BlobsOnly or (Field.IsBlob and VarIsNull(Fields[i].NewValue))) then
          DBFieldsAssign(Fields[i], Field);
      end;
      Post;
    end;
  end;
end;

procedure TAstaIOCustomDataSetProvider.SetActive(Value: Boolean);
begin
  FActive := Value;
  if Assigned(FDataSet) and not (csLoading in ComponentState) then
    FDataSet.Active := Value;
end;

procedure TAstaIOCustomDataSetProvider.OpenDetailDataSets(DetailList :TList);
var i  :Integer;
begin
  //for i:=0 to FDetailDataSets.Count - 1 do
  for i:=0 to DetailList.Count - 1 do
  begin
    //TDataSet(FDetailDataSets[i]).Open;
    TDataSet(DetailList[i]).Open;
  end;
end;

procedure TAstaIOCustomDataSetProvider.CloseDetailDataSets;
var i  :Integer;
begin
  for i:=0 to FDetailDataSets.Count - 1 do
  begin
    TDataSet(FDetailDataSets[i]).Close;
  end;
end;


procedure TAstaIOCustomDataSetProvider.EndTransaction(Commit: Boolean);
begin
  CheckDataSet;
  IProviderSupport(DataSet).PSEndTransaction(Commit);
end;

procedure TAstaIOCustomDataSetProvider.Execute;
begin
  CheckDataSet;
  if Assigned(FBeforeExecEvent) then FBeforeExecEvent(Self);
  ParseSQL(FCommandText);
  IProviderSupport(DataSet).PSExecute;
end;

function TAstaIOCustomDataSetProvider.ExecuteStatement(const ASQL: string; AParams: TParams; ResultSet: Pointer): Integer;
var List   :TParams;
    FSQL   :String;
begin
  // PSExecuteStatement expects the already parsed sql that contains the ? for placeholders
  List:=TParams.Create(Self);
  try
    FSQL:=List.ParseSQL(ASQL, True);
  finally
    List.Free;
  end;
  CheckDataSet;
  Result:=IProviderSupport(DataSet).PSExecuteStatement(ASQL, AParams, ResultSet);
end;

function TAstaIOCustomDataSetProvider.FindRecord(Source, Delta: TDataSet; UpdateMode: TUpdateMode): Boolean;

  procedure GetFieldList(DataSet: TDataSet; UpdateMode: TUpdateMode; List: TList);
  var i  :Integer;
  begin
    for i:=0 to DataSet.FieldCount - 1 do
    begin
      if DataSet.Fields[i].FieldKind = fkData then
      with DataSet.Fields[i] do
      begin
        if (DataType in [ftBytes, ftVarBytes]) or IsBlob or
          (DataSet.Fields[i] is TObjectField) then continue;
        case UpdateMode of
          upWhereKeyOnly:
            if pfInKey in ProviderFlags then List.Add(DataSet.Fields[i]);
            //List.Add(DataSet.Fields[i]);
          upWhereAll:
            if pfInWhere in ProviderFlags then List.Add(DataSet.Fields[i]);
          upWhereChanged:
          begin
            if (pfInKey in ProviderFlags) or (not VarIsEmpty(NewValue)) then
              List.Add(DataSet.Fields[i]);
           end;
        end;
      end;
    end;
  end;

var
  i: Integer;
  KeyValues: Variant;
  Fields: string;
  FieldList: TList;
//  IsDelta: Boolean;
begin
  Result:=False;
//  IsDelta:=false;

  FieldList:=TList.Create;
  try
    GetFieldList(Source, UpdateMode, FieldList);
    if FieldList.Count > 1 then
    begin
      KeyValues:=VarArrayCreate([0, FieldList.Count - 1], varVariant);
      Fields:='';
      for i:=0 to FieldList.Count - 1 do
        with TField(FieldList[i]) do
        begin
          if FieldKind = fkData then
          begin
            KeyValues[i]:=Delta.FieldByName(FieldName).Value;
            if Fields <> '' then
              Fields:=Fields + ';';
            Fields:=Fields + FieldName;
          end;
        end;
      Result:=Source.Locate(Fields, KeyValues, []);
    end
    else
    if FieldList.Count = 1 then
    begin
      with TField(FieldList[0]) do
        Result:=Source.Locate(FieldName, Delta.FieldByName(FieldName).Value, []);
    end
    else
      DatabaseError(SNoPrimeFieldsGen);
  finally
    FieldList.Free;
  end;
end;

function TAstaIOCustomDataSetProvider.GetKeyFields: string;
begin
  CheckDataSet;
  Result:=IProviderSupport(FInfoDataSet).PSGetKeyFields;
end;

function TAstaIOCustomDataSetProvider.GetParams: TParams;
begin
  CheckDataSet;
  Result:=IProviderSupport(DataSet).PSGetParams;
  if Result = nil then
    FParams.Clear
  else
    FParams.Assign(Result);
end;

function TAstaIOCustomDataSetProvider.GetQuoteChar: string;
begin
  CheckDataSet;
  Result:=IProviderSupport(FInfoDataSet).PSGetQuoteChar;
end;

function TAstaIOCustomDataSetProvider.GetTableName: string;
begin
  CheckDataSet;
  Result:=IProviderSupport(FInfoDataSet).PSGetTableName;
  if Assigned(FOnGetTableNameEvent) then
    FOnGetTableNameEvent(Self, FInfoDataSet, Result);
end;

procedure TAstaIOCustomDataSetProvider.InternalExecute(const CommandText: WideString; var Params: OleVariant);
begin
  CheckDataSet;
  IProviderSupport(FDataSet).PSExecute;
end;

function TAstaIOCustomDataSetProvider.InTransaction: Boolean;
begin
  CheckDataSet;
  Result:=IProviderSupport(DataSet).PSInTransaction;
end;

function TAstaIOCustomDataSetProvider.PSGetIndexDefs(IndexTypes: TIndexOptions): TIndexDefs;
begin
  CheckDataSet;
  Result:=IProviderSupport(FInfoDataSet).PSGetIndexDefs(IndexTypes);
end;

function TAstaIOCustomDataSetProvider.PSGetKeyFields: String;
begin
  CheckDataSet;
  Result:=IProviderSupport(FInfoDataSet).PSGetKeyFields;
end;

procedure TAstaIOCustomDataSetProvider.SetParams(AParams: TParams);
begin
  CheckDataSet;
  IProviderSupport(DataSet).PSSetParams(AParams);
end;

procedure TAstaIOCustomDataSetProvider.StartTransaction;
begin
  CheckDataSet;
  if Assigned(FBeforeStartTransactionEvent) then FBeforeStartTransactionEvent(Self);
  IProviderSupport(DataSet).PSStartTransaction;
end;

procedure TAstaIOCustomDataSetProvider.SetCommandText(const CommandText: WideString);
begin
  if not (poAllowCommandText in FOptions) then
    DatabaseError(SProvCannotChangeCommandText);
  CheckDataSet;
  IProviderSupport(DataSet).PSSetCommandText(Trim(CommandText));
  FCommandText:=Trim(CommandText);
end;

procedure TAstaIOCustomDataSetProvider.UpdateSourceRecord(Source, OldDelta, NewDelta: TDataSet; DeltaType: TDeltaType);

  procedure AssignFields;
  var
    i: Integer;
    destField, srcField: TField;
  begin
    for i := 0 to Source.Fields.Count - 1 do begin
      destField := Source.Fields[i];
      srcField := NewDelta.FindField(destField.FieldName);
      if (srcField <> nil) and UpdatableField(destField) and
         DBFieldsAreNotEqual(destField, srcField) then
        DBFieldsAssign(destField, srcField);
    end;
  end;

begin
  if LocateRecord(Source, OldDelta) then
  begin
    case DeltaType of
      dtEdit:
      begin
        Source.Edit;
        AssignFields;
        Source.Post;
      end;
      dtDelete:
      begin
        Source.Delete;
      end;
      dtAppend:
      begin
        Source.Append;
        AssignFields;
        Source.Post;
      end;
    end;
  end;
end;

procedure TAstaIOCustomDataSetProvider.SetNoSQLFields(Value: TStrings);
begin
  FNoSQLFields.Assign(Value);
end;

function TAstaIOCustomDataSetProvider.GetNoSQLFields: TStrings;
begin
  Result:=FNoSQLFields;
end;
function TAstaIOCustomDataSetProvider.UpdatableField(Field: TField): Boolean;
begin
  Result := True;
  if Field.FieldKind <> fkData then Result := False
  else if Field.DataType = ftDataSet then Result := False
  else if not (pfInUpdate in Field.ProviderFlags) then Result := False
  else if (Field.DataSet is TAstaCustomAuditDataSet) and
          (TAstaCustomAuditDataSetHack(Field.DataSet).NoSQLFields.IndexOf(Field.FieldName) <> -1) then Result := False;
end;

function TAstaIOCustomDataSetProvider.AddClientDataSet(ADataSet :TDataSet) :Integer;
begin
  Result:=-1;
  if FClientDataSets.IndexOf(ADataSet) >=0 then exit;
  Result:=FClientDataSets.Add(ADataSet);
end;

procedure TAstaIOCustomDataSetProvider.RemoveClientDataSet(ADataSet :TDataSet);
begin
  FClientDataSets.Remove(ADataSet);
end;
procedure TAstaIOCustomDataSetProvider.TransferDataToPacket;
begin
  FDataPacket.DataTransfer(DataSet, True, False);
end;

procedure TAstaIOCustomDataSetProvider.FetchNestedDetails;
var i   :Integer;
begin
  for i:=0 to FDetailDataSets.Count - 1 do
  begin
    if FDataPacket.FindField(TDataSet(FDetailDataSets[i]).Name) <> nil then
    begin
      FDataPacket.Edit;
      FDetailCloneStream.Clear;
      FDetailCloneDataSet.CleanCloneFromDataSet(TDataSet(FDetailDataSets[i]), True);
      FDetailCloneDataSet.SaveToStream(FDetailCloneStream);
      FDetailCloneStream.Position:=0;
      FDataPacket.StoreMemoryStream(FDataPacket.FieldByName(TDataSet(FDetailDataSets[i]).Name), FDetailCloneStream);
      FDataPacket.Post;
    end;
  end;
end;

function TAstaIOCustomDataSetProvider.InternalGetRecords(Count: Integer;  var RecsOut: Integer; Options :TAstaIOGetRecordOptions;
                                                         const CommandText: WideString;  var Params: TParams): IAstaIOData;
var Cnt    :Integer;
begin
  Cnt:=0;
  {if not FDataSet.Active then
  begin
    if Assigned(Params) then
      SetParams(Params);
    if Length(CommandText) > 0 then
      SetCommandText(CommandText);
    Open;
  end;}

  FDataPacket.Empty;
  if (grReset in Options) then FDataSet.First;
  try
    while not FDataSet.Eof do
    begin
      FDataPacket.AppendSingleRecordFromSource(FDataSet);
      if (poFetchDetailsOnDemand in FOptions) then
        FetchNestedDetails;
      FDataSet.Next;
      if FDataSet.Eof then break;
      Inc(Cnt);
      if (Cnt >= Count) and (Count > 0) then break;
    end;
  finally
    RecsOut:=Cnt;
    FReachedEof:=FDataSet.Eof;
    if FDataSet.Eof and FDataSet.Active then FDataSet.First;
    FDataPacket.First;
  end;
  Result := TAstaIODataObj.Create(FDataPacket, False);
end;

function TAstaIOCustomDataSetProvider.GetRecords(Count: Integer; var RecsOut: Integer; Options: TAstaIOGetRecordOptions;
                                                 const CommandText: WideString; Params: TParams): IAstaIOData;
begin
  if Assigned(FBeforeGetRecords) then
    FBeforeGetRecords(Self);

  Result := InternalGetRecords(Count, RecsOut, Options, CommandText, Params);

  if Assigned(FAfterGetRecords) then
    FAfterGetRecords(Self);
end;

function TAstaIOCustomDataSetProvider.GetRecords(Count: Integer; var RecsOut: Integer; Options: TAstaIOGetRecordOptions): IAstaIOData;
begin
  Result := GetRecords(Count, RecsOut, Options, '', nil);
end;

procedure TAstaIOCustomDataSetProvider.SetInfoDataSetPath(APath: TList);
var
  i: Integer;
  detailDSs: TList;
begin
  FInfoDataSet := FDataSet;
  detailDSs := nil;
  try
    try
      if APath <> nil then begin
        detailDSs := FDetailDataSets;
        for i := 0 to APath.Count - 1 do begin
          FInfoDataSet := TDataSet(detailDSs[Integer(APath[i])]);
          if i < APath.Count - 1 then begin
            if i = 0 then
              detailDSs := TList.Create;
            TDataSetHack(FInfoDataSet).GetDetailDataSets(detailDSs);
          end;
        end;
      end;
    finally
      if (detailDSs <> nil) and (detailDSs <> FDetailDataSets) then
        detailDSs.Free;
    end;
  except
    FInfoDataSet := FDataSet;
    raise;
  end;
end;

procedure TAstaIOCustomDataSetProvider.SetDataSet(const Value: TDataSet);
begin
  if FDataSet <> Value then begin
    FDataSet := Value;
    FInfoDataSet := FDataSet;
    if FDataSet <> nil then
      FDataSet.FreeNotification(Self);
  end;
end;

{ TAstaIOCustomResolver }

procedure TAstaIOCustomResolver.ApplyUpdates(ADataSet: TDataSet; MaxErrors: Integer; var ErrorCount: Integer);
begin
  //
end;

procedure TAstaIOCustomResolver.BeginUpdate;
begin

end;

constructor TAstaIOCustomResolver.Create(AProvider :TAstaIOCustomDataSetProvider);
begin
  inherited Create;
  FProvider:=AProvider;
  FSQLGenerator:=TAstaIOSQLGenerator.Create(nil);
end;

destructor TAstaIOCustomResolver.Destroy;
begin
  FSQLGenerator.Free;
  inherited Destroy;
end;

procedure TAstaIOCustomResolver.EndUpdate;
begin

end;

procedure TAstaIOCustomResolver.GetTableName(DataSet: TDataSet; var TableName: String);
begin
  TableName:=FProvider.GetTableName;
end;

{ TAstaIOSQLResolver }

procedure TAstaIOSQLResolver.ApplyDetailUpdates(ASender: TObject; ADataSet: TDataSet; ASQL: TAstaParamList);
var
  sqlGen: TAstaIOSQLGenerator;
  CurrentValues: TAstaIODataSet;
  pars: TAstaParamList;
  infoProv: TAstaIODataSetProvider;
begin
  if (TAstaCustomAuditDataSetHack(ADataSet).PrimeFields.Count = 0) and
     (FProvider.UpdateMode = upWhereKeyOnly) then
    DatabaseError(SNoPrimeFieldsGen, FProvider);

  if ADataSet is TAstaIOCustomClientDataSet then begin // ?????
    infoProv := TAstaIOCustomClientDataSet(ADataSet).InitInfoProvider;
    if infoProv <> FProvider then
      DatabaseError(SInfoProvMismatch, FProvider);
  end;

  sqlGen := TAstaIOSQLGenerator.Create(nil);
  try
    CurrentValues:=TAstaCustomAuditDataSetHack(ADataSet).DeltaDataSetCurrentValueDataSet;
    try
      sqlGen.BaseDataSet:=ADataSet;
      sqlGen.OldValuesDataSet:=TAstaCustomAuditDataSetHack(ADataSet).OldValuesDataSet;
      sqlGen.PrimeFields.Assign(TAstaCustomAuditDataSetHack(ADataSet).PrimeFields);
      if TAstaCustomAuditDataSetHack(ADataSet).UpdateTableName = '' then
        sqlGen.UpdateTableName:=FProvider.GetTableName
      else
        sqlGen.UpdateTableName:=TAstaCustomAuditDataSetHack(ADataSet).UpdateTableName;
      sqlGen.NoSQLFields.Assign(TAstaCustomAuditDataSetHack(ADataSet).NoSQLFields);
      sqlGen.CurrentDataSet:=CurrentValues;
      sqlGen.UpdateMode:=FProvider.UpdateMode;
      sqlGen.TrimStringFields:=not FProvider.DisableStringTrim;
      sqlGen.OnProcessDetails:=ApplyDetailUpdates;
      pars := sqlGen.GenerateClientSideSQL(TAstaCustomAuditDataSetHack(ADataSet), False, True);
      pars.CopyParams(ASQL, False);
    finally
      CurrentValues.Free;
    end;
  finally
    sqlGen.Free;
  end;
end;

procedure TAstaIOSQLResolver.ApplyUpdates(ADataSet: TDataSet; MaxErrors: Integer; var ErrorCount: Integer);
var SQLCnt        :Integer;
    CurrentValues :TAstaIODataSet;
    Handled       :Boolean;
begin
  if (TAstaCustomAuditDataSetHack(ADataSet).PrimeFields.Count = 0) and (FProvider.UpdateMode = upWhereKeyOnly) then
    DatabaseError(SNoPrimeFieldsGen, FProvider);

  FSQLGenerator.BaseDataSet:=ADataSet;
  FSQLGenerator.OldValuesDataSet:=TAstaCustomAuditDataSetHack(ADataSet).OldValuesDataSet;
  CurrentValues:=TAstaCustomAuditDataSetHack(ADataSet).DeltaDataSetCurrentValueDataSet;
  try
    Handled:=False;
    if Assigned(FProvider.FBeforeApplyUpdates) then
      FProvider.FBeforeApplyUpdates(FProvider, TAstaCustomAuditDataSetHack(ADataSet).OldValuesDataSet, CurrentValues, Handled);

    if not Handled then begin
      FSQLGenerator.PrimeFields.Assign(TAstaCustomAuditDataSetHack(ADataSet).PrimeFields);
      if TAstaCustomAuditDataSetHack(ADataSet).UpdateTableName = '' then
        FSQLGenerator.UpdateTableName:=FProvider.GetTableName
      else
        FSQLGenerator.UpdateTableName:=TAstaCustomAuditDataSetHack(ADataSet).UpdateTableName;
      FSQLGenerator.NoSQLFields.Assign(TAstaCustomAuditDataSetHack(ADataSet).NoSQLFields);
      FSQLGenerator.CurrentDataSet:=CurrentValues;
      FSQLGenerator.UpdateMode:=FProvider.UpdateMode;
      FSQLGenerator.TrimStringFields:=not FProvider.DisableStringTrim;
      FSQLGenerator.OnProcessDetails:=ApplyDetailUpdates;
      FSQLList:=FSQLGenerator.GenerateClientSideSQL(TAstaCustomAuditDataSetHack(ADataSet), False, True);
      ErrorCount:=0;
      //if not FProvider.InTransaction then FProvider.StartTransaction;
      if FSQLList <> nil then
        for SQLCnt:=0 to FSQLList.Count - 1 do
        begin
          try
            FProvider.ExecuteStatement(FSQLList[SQLCnt].Name, AstaParamsToTParams(FSQLList[SQLCnt].AsParamList));
          except
            on E: Exception do begin
              Inc(ErrorCount);
              with FSQLGenerator, OldValuesDataSet do
                if Locate(sfld_BookMark, BookmarkList[SQLCnt].AsInteger, []) then begin
                  Edit;
                  FieldByName(sfld_ErrorCode).AsInteger := ErrorCount;
                  FieldByName(sfld_ErrorInfo).AsString := E.Message;
                  Post;
                end;
              if (ErrorCount >= MaxErrors) and (MaxErrors >= 0) then
              begin
                FProvider.EndTransaction(False);
                raise;
              end;
            end;
          end;
        end;
      //if FProvider.InTransaction then FProvider.EndTransaction(True);
    end;
    if Assigned(FProvider.FAfterApplyUpdates) then
      FProvider.FAfterApplyUpdates(FProvider, TAstaCustomAuditDataSetHack(ADataSet).OldValuesDataSet, CurrentValues);
  finally
    CurrentValues.Free;
  end;
end;

{ TAstaIODataSetResolver }
procedure TAstaIODataSetResolver.BaseApplyUpdates(ADataSet: TDataSet; MaxErrors: Integer; var ErrorCount: Integer);
var CurrentValues    :TAstaIODataSet;
    SingleOldDS,
    SingleCurrDS     :TAstaIODataSet;
    SourceBM, BM     :TBookmark;
    Hotspot          :Integer;
    Action           :TDeltaType;
    Handled          :Boolean;
    prevInfoDataSet  :TDataSet;

  procedure ProcessDetails;
  var
    i: Integer;
    infoProv: TAstaIODataSetProvider;
    ds: TAstaCustomAuditDataSet;
    prevInfoDataSet: TDataSet;
  begin
    prevInfoDataSet := FProvider.InfoDataSet;
    try
      for i := 0 to TDataSetHack(ADataSet).NestedDataSets.Count - 1 do begin
        ds := TDataSet(TDataSetHack(ADataSet).NestedDataSets[i]) as TAstaCustomAuditDataSet;
        if ds is TAstaIOCustomClientDataSet then begin
          infoProv := TAstaIOCustomClientDataSet(ds).InitInfoProvider;
          if infoProv <> FProvider then
            DatabaseError(SInfoProvMismatch, FProvider);
        end;
        if ds.ChangeCount <> 0 then
          BaseApplyUpdates(ds, MaxErrors, ErrorCount);
      end;
    finally
      FProvider.InfoDataSet := prevInfoDataSet;
    end;
  end;

begin
  CurrentValues:=TAstaCustomAuditDataSetHack(ADataSet).DeltaDataSetCurrentValueDataSet;
  SingleOldDS:=TAstaIODataSet.Create(nil);
  SingleCurrDS:=TAstaIODataSet.Create(nil);
  prevInfoDataSet:=FProvider.InfoDataSet;
  SourceBM:=FProvider.InfoDataSet.Bookmark;
  BM:=ADataSet.Bookmark;
  try

    Handled:=False;
    if Assigned(FProvider.FBeforeApplyUpdates) then
      FProvider.FBeforeApplyUpdates(FProvider, TAstaCustomAuditDataSetHack(ADataSet).OldValuesDataSet, CurrentValues, Handled);

    if not Handled then begin
      SingleOldDS.DisposeAstaList;
      SingleOldDS.FieldDefs.Assign(TAstaCustomAuditDataSetHack(ADataSet).FieldDefs);
      SingleOldDS.Open;

      SingleCurrDS.DisposeAstaList;
      SingleCurrDS.FieldDefs.Assign(TAstaCustomAuditDataSetHack(ADataSet).FieldDefs);
      SingleCurrDS.Open;

      CurrentValues.First;
      TAstaCustomAuditDataSetHack(ADataSet).OldValuesDataSet.First;

      //if not FProvider.InTransaction then FProvider.StartTransaction;
      while not TAstaCustomAuditDataSetHack(ADataSet).OldValuesDataSet.Eof do
      begin
        Action:=TDeltaType(TAstaCustomAuditDataSetHack(ADataSet).OldValuesDataSet.FieldByName(sfld_Delta).AsInteger);
        HotSpot:=TAstaCustomAuditDataSetHack(ADataSet).OldValuesDataSet.FieldByName(sfld_BookMark).AsInteger;

        SingleOldDS.CloneSingleRecordFromSource(TAstaCustomAuditDataSetHack(ADataSet).OldValuesDataSet, True);
        try
          if CurrentValues.Locate(sfld_BookMark, HotSpot, []) then
            SingleCurrDS.CloneSingleRecordFromSource(CurrentValues, True);

          Handled := False;
          if Assigned(Fprovider.FBeforeUpdateRecordEvent) then
            FProvider.FBeforeUpdateRecordEvent(FProvider, SingleOldDS, SingleCurrDS, Action, Handled);

          if not Handled then begin
            case Action of
            dtEdit:
              begin
                FProvider.UpdateSourceRecord(FProvider.InfoDataSet, SingleOldDS, SingleCurrDS, Action);
                ProcessDetails;
              end;
            dtDelete:
              begin
                ProcessDetails;
                FProvider.UpdateSourceRecord(FProvider.InfoDataSet, SingleOldDS, SingleCurrDS, Action);
              end;
            dtAppend:
              begin
                FProvider.UpdateSourceRecord(FProvider.InfoDataSet, SingleOldDS, SingleCurrDS, Action);
                ProcessDetails;
              end;
            end;
          end;

          if Assigned(FProvider.FAfterUpdateRecordEvent) then
            FProvider.FAfterUpdateRecordEvent(FProvider, SingleOldDS, SingleCurrDS, Action);
        except
          on E: Exception do begin
            Inc(ErrorCount);
            with TAstaCustomAuditDataSetHack(ADataSet).OldValuesDataSet do
            try
              Edit;
              FieldByName(sfld_ErrorCode).AsInteger := ErrorCount;
              FieldByName(sfld_ErrorInfo).AsString := E.Message;
              Post;
            except
              Cancel;
            end;
            if (ErrorCount >= MaxErrors) and (MaxErrors >= 0) then
            begin
              FProvider.EndTransaction(False);
              raise;
            end;
          end;
        end;
        TAstaCustomAuditDataSetHack(ADataSet).OldValuesDataSet.Next;
      end;
      //if FProvider.InTransaction then FProvider.EndTransaction(True);
    end;
    if Assigned(FProvider.FAfterApplyUpdates) then
      FProvider.FAfterApplyUpdates(FProvider, TAstaCustomAuditDataSetHack(ADataSet).OldValuesDataSet, CurrentValues);
  finally
    ADataSet.Bookmark:=BM;
    SingleOldDS.Free;
    SingleCurrDS.Free;
    CurrentValues.Free;
    FProvider.InfoDataSet:=prevInfoDataSet;
    FProvider.InfoDataSet.Bookmark:=SourceBM;
  end;
end;

procedure TAstaIODataSetResolver.ApplyUpdates(ADataSet: TDataSet; MaxErrors: Integer; var ErrorCount: Integer);
begin
  ErrorCount:=0;
  try
    BaseApplyUpdates(ADataSet, MaxErrors, ErrorCount);
  finally
    FProvider.SetInfoDataSetPath(nil);
  end;
end;

end.

