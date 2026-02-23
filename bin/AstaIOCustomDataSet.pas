{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10097: AstaIOCustomDataSet.pas 
{
{   Rev 1.1    4/19/2003 5:50:34 AM  SteveG
}
{
{   Rev 1.0    4/10/2003 6:30:24 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:36:52 PM  Steve    Version: 1.505
}

unit AstaIOCustomDataSet;
{*********************************************************}
{*   Copyright (c) 1997-2002 Asta Technology Group Inc.  *}
{*               All rights reserved.                    *}
{*               www.astatech.com                        *}
{*********************************************************}

{$I AstaIO.inc}

{$POINTERMATH ON}

interface

uses DB, System.AnsiStrings,
  FMTBcd,
  {$ifdef AstaIOXML}
  AstaIOXMLComponent,
  {$endif}
  Classes,
  SysUtils,
  AstaIODBList,
  AstaIOIndexes,
  AstaIOBlobList,
  AstaIOFilter,
  AstaIODBConst,
  AstaIOUtil;

type
  TAstaIOXMLDataSetFormat=(taxADO,TaxMidas);
  TDataSetProtected = class(TDataSet);
  TAstaFileName = string;
  TOffLineString = string[20];

  PRecInfo = ^TRecInfo;
  TRecInfo = packed record
    BookMark: Integer;
    BookmarkFlag: TBookmarkFlag;
  end;

  PInt = ^Integer;
  PLongInt = ^Integer;
  PIn64 = ^Int64;
  PWordBool = ^WordBool;
  PSmallInt = ^SmallInt;
  PWord = ^Word;
  PDouble = ^Double;

  TAstaIOKeyIndex = (kiLookup, kiRangeStart, kiRangeEnd, kiCurRangeStart,
    kiCurRangeEnd, kiSave);

  PAstaIOKeyBuffer = ^TAstaIOKeyBuffer;
  TAstaIOKeyBuffer = packed record
    Modified: Boolean;
    Exclusive: Boolean;
    FieldCount: Integer;
  end;

  TIntArr = array[0..1000] of Integer;
  PIntArr = ^TIntArr;

  TAstaIOStreamSaveOption=(ssZlibCompression,ssEncrypted,ssIndexes,
                           ssCachedUpdates,ssXMLADO,ssXMLMidas,ssAstaPack);

  TAstaIOStreamSaveOptions=set of TAstaIOStreamSaveOption;

  TAstaIOCustomDataset = class;
  TAutoCreateCalcFields = class;
  TAutoCreateCalcFieldType = (cfString, cfInteger, cfSmallInt, cfWord, cfFloat, cfCurrency, cfBCD, cfFMTBCD, cfBoolean,
                              cfDateTime, cfDate, cfTime, cfLargeInt, cfWideString);


  TAutoCreateCalcField = class(TCollectionItem)
  private
    FParent             :TAutoCreateCalcFields;
    FFieldName          :String;
    FActive             :Boolean;
    FDataType           :TAutoCreateCalcFieldType;
    FVisible            :Boolean;
    FSize               :Integer;
    procedure SetActive(Value :Boolean);
    procedure SetDataType(Value :TAutoCreateCalcFieldType);
  protected
    function GetDisplayName :String; override;
  public
    constructor Create(Collection: TCollection); override;
  published
    property FieldName :String read FFieldName write FFieldName;
    property Active :Boolean read FActive write SetActive;
    property DataType :TAutoCreateCalcFieldType read FDataType write SetDataType;
    property Visible :Boolean read FVisible write FVisible;
    property Size :Integer read FSize write FSize;
  end;

  TAutoCreateCalcFields = class(TCollection)
  private
    FOwner              :TComponent;
    FLastItemCount      :Integer;
  protected
    function GetOwner :TPersistent; override;
    procedure SetItem(Index :Integer; Value :TAutoCreateCalcField);
    function GetItem(Index :Integer) :TAutoCreateCalcField;
    function Insert(Index :Integer) :TAutoCreateCalcField;
  public
    constructor Create(Owner :TComponent);
    destructor Destroy; override;
    function Add :TAutoCreateCalcField;
    property LastItemCount :Integer read FLastItemCount write FLastItemCount;
    property Items[Index :Integer] :TAutoCreateCalcField read GetItem write SetItem; default;
  end;

  TAstaIODataLink = class(TDetailDataLink)
  private
    FOnMasterChange: TNotifyEvent;
    FOnMasterDisable: TNotifyEvent;
  protected
    FDataSet: TAstaIOCustomDataset;
    procedure ActiveChanged; override;
    procedure RecordChanged(Field: TField); override;
    procedure CheckBrowseMode; override;
    procedure LayoutChanged; override;
  public
    constructor Create(ADataSet: TAstaIOCustomDataset);
    destructor Destroy; override;
    property OnMasterChange: TNotifyEvent read FOnMasterChange write FOnMasterChange;
    property OnMasterDisable: TNotifyEvent read FOnMasterDisable write FOnMasterDisable;
  end;

  TAstaIOCustomDataset = class(TDataSet)
  private
    FStreamOptions:TAstaIOStreamSaveOptions;
    FSuspendEvents: Boolean;
    FFileName: TAstaFileName;
    FBlobRollBackList: TStringList;
    FReadOnly: Boolean;
    FAbout: String;
    FFldOffs: PInteger;
    FRecBufSize: Integer;
    FStartCalculated: Integer;
    FSaveChanges: Boolean;
    FFilterBuffer: Pointer;
    FFilterTreeReader: TAstaExpressionTreeReader;
    FExpressions: TStringList;
    FExpressionUsages: TAstaExpressionUsages;
    FCurValidate: TFieldNotifyEvent;
    FConstDisableCount: Integer;
    FInFindRecord: Boolean;
    FCloneList: TStringList;
    FEchoEditAppend: Boolean;
    FDirectRead: Boolean;
    FDirectWrite: Boolean;
    FDirectWriteRec: TAstaDBListItem;
    FBatchStates: TList;
    FKeyBuffers: array[TAstaIOKeyIndex] of PAstaIOKeyBuffer;
    FKeyBuffer: PAstaIOKeyBuffer;
    FRangeFrom, FRangeTo: TAstaDBListItem;
    FRangeFromExclusive, FRangeToExclusive: Boolean;
    FRangeFromFieldCount, FRangeToFieldCount: Integer;
    FKeyBuffersAllocated: Boolean;
    FAggregates: TAstaIOAggregates;
    procedure RemoveInvalidPersistentFields;
    //procedure AddFieldsFromString(S: string);
    procedure SetBufferSizeFromFieldList;
    function FieldSizeFromField(Field: Tfield): Integer;
    function GetActiveRecordBuffer: PAnsiChar;overload;
    {Procedure SetupFilteredRanges;}
    procedure MasterChildFilter(DataSet: TDataSet; var Accept: Boolean);
    function MasterDetailFilter: Boolean;

    function GetUniqueBookMark: Integer;
    function GetChangedFlag(Field: TField; Buffer: Pointer): Boolean;
    procedure SetChangedFlag(Field: TField; Changed: Boolean; Buffer: Pointer);
    //function AdjustForBCD(d: TFieldtype): TFieldType;
    procedure CloneDeleteUpdate;
    procedure RegisterClone(Source: TAstaIOCustomDataset);
    procedure CloneEditUpdate;
    procedure DoIndexChanging(AIndex: TAstaIOIndex; AReason: TAstaIOIndexReason);
    procedure SetIndexes(const AValue: TAstaIOIndexes);
    procedure SetIndexFieldNames(const AValue: String);
    procedure SetIndexName(const AValue: String);
    function GetIndexName: String;
    function GetIndexFieldNames: String;
    function GetKeyExclusive: Boolean;
    procedure SetKeyExclusive(AValue: Boolean);
    function GetKeyFieldCount: Integer;
    procedure SetKeyFieldCount(AValue: Integer);
    function GetIndexFieldCount: Integer;
    function GetIndexField(AIndex: Integer): TField;
    procedure SetAggregates(AValue: TAstaIOAggregates);
    procedure SetIndexDefs(const Value: TAstaIOIndexDefs);
  protected
    FDoCheckRanges: Boolean;
    FMasterLink: TMasterDataLink;
    FMasterFieldsList: TStringList;
    FDetailFieldsList: TStringList;
    FIsMasterActive: Boolean;
    FDetailFields: string;
    FIndexDefs: TAstaIOIndexDefs;
    FDataSource: TDataSource;
    FDataLink: TAstaIODataLink;
    FDoEvents: Boolean;

    FStoreDefs: Boolean;
    FLastBookmark: Integer;
    FBlobList: TAstaBlobList;
    FIsOpen: Boolean;
    FIndexes: TAstaIOIndexes;
    FRecInfoOfs: Integer;
    FRawBufSize: Integer;

    FReqActList: TList;
    FHasNestedDataSets: Boolean;
    FAutoCreateCalcFields :TAutoCreateCalcFields;
    procedure AddAutoCreateCalcFields(OwnerDataSet :TDataSet);

    procedure InternalLoadFromStreamWithFields(Stream: TStream);
    procedure ClearCalcFields(Buffer: TRecordBuffer); override;
    function CheckMasterFilter: Boolean; virtual;
    function CheckRecordStatusFilter: Boolean; virtual;
    procedure RecordStatusFilter(DataSet: TDataSet; var Accept: Boolean); virtual;
    //Before's
    procedure DoBeforeDelete; override;
    procedure DoBeforeCancel; override;
    procedure DoBeforePost; override;
    procedure DoBeforeEdit; override;
    procedure DoBeforeInsert; override;
    //After's
    procedure DoAfterInsert; override;
    procedure DoAfterDelete; override;
    procedure DoAfterCancel; override;
    procedure DoAfterPost; override;
    procedure DoAfterEdit; override;

    procedure BookMarkRenumber;
    //function BCDToCurr(BCD: Pointer; var Curr: Currency): Boolean;
    //function CurrToBCD(const Curr: Currency; BCD: Pointer; Precision, Decimals: Integer): Boolean;
    procedure Loaded; override;
    function IsDesignTime: Boolean; virtual;
    function GetDetailFieldByIndex(Index: Integer): string;
    function GetMasterFieldByIndex(Index: Integer): string;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure InternalOpen; override;
    procedure InternalInitFieldDefs; override;
    procedure InternalClose; override;
    function IsCursorOpen: Boolean; override;
    procedure GetBookmarkData(Buffer: TRecordBuffer; Data: Pointer); override;
    procedure SetBookmarkData(Buffer: TRecordBuffer; Data: Pointer); overload; override;
    //procedure SetBookmarkData(Buffer: TRecordBuffer; Bookmark: TBookmark); overload;
    function GetBookmarkFlag(Buffer: TRecordBuffer): TBookmarkFlag; override;
    procedure SetBookmarkFlag(Buffer: TRecordBuffer; Value: TBookmarkFlag); override;
    procedure InternalSetToRecord(Buffer: TRecordBuffer); override;
    procedure InternalGotoBookmark(Bookmark: Pointer); overload; override;
    //procedure InternalGotoBookmark(Bookmark: DB.TBookmark); overload;
    function GetRecNo: integer; override;
    procedure SetRecNo(Value: Integer); override;
    function GetRecordCount: Integer; override;
    function AllocRecordBuffer: TRecordBuffer; override;
    procedure FreeRecordBuffer(var Buffer: TRecordBuffer); override;
    function GetRecord(Buffer: TRecordBuffer; GetMode: TGetMode; DoCheck: Boolean): TGetResult; override;
    function ReadField(key: TAstaDBlistItem; AField: TField): Variant;
    procedure InternalInsert; override;
    procedure InternalEdit; override;
    procedure InternalCancel; override;
    function GetIsIndexField(Field: TField): Boolean; override;
    procedure CheckSetKeyMode;
    procedure AllocKeyBuffers;
    procedure FreeKeyBuffers;
    function GetKeyBuffer(KeyIndex: TAstaIOKeyIndex): PAstaIOKeyBuffer;
    function InitKeyBuffer(Buffer: PAstaIOKeyBuffer): PAstaIOKeyBuffer;
    procedure SetKeyBuffer(KeyIndex: TAstaIOKeyIndex; Clear: Boolean);
    procedure SetKeyFields(KeyIndex: TAstaIOKeyIndex; const Values: array of const);
    procedure PostKeyBuffer(Commit: Boolean);
    function SetCursorRange: Boolean;
    function ResetCursorRange: Boolean;
    function InternalGotoKey(ANearest: Boolean): Boolean;
    function CheckRanges(ARec: TAstaDBListItem; APart: Integer): Integer;
    procedure FirstUsingRanges;
    procedure LastUsingRanges;
    {$IFDEF indexdefs}
    procedure UpdateIndexDefs; override;
    {$ENDIF}
    procedure InternalInitRecord(Buffer: TRecordBuffer); override;
    procedure InternalAddRecord(Buffer: Pointer; Append: Boolean); override;
    procedure InternalPost; override;
    procedure InternalDelete; override;
    procedure SetFieldDataNoDataEvent(Field: TField; Buffer: Pointer);
    procedure SetFieldData(Field: TField; Buffer: Pointer); overload; override;
    procedure InternalFirst; override;
    procedure InternalLast; override;
    procedure InternalHandleException; override;
    procedure DoOnNewRecord; override;
    function GetCanModify: Boolean; override;
    function FilterRecord(Buffer: TRecordBuffer): Boolean;
    function FindRecord(Restart, GoForward: Boolean): Boolean; override;
    procedure EmptyDataSet;
    property SuspendEvents: Boolean read FSuspendEvents write FSuspendEvents;
    procedure DoAfterOpen; override;
    procedure DoAfterClose; override;
    procedure DataEvent(Event: TDataEvent; Info: Integer); override;
    procedure PostDataToParentDataSet; virtual;
    function IsMasterActive(DataSource: TDataSource): Boolean;
    // Master/Detail fields
    function GetMasterFields: string;
    procedure SetMasterFields(const Value: string);
    procedure SetDataSource(Value: TDataSource);
    function GetDataSource: TDataSource; override;
    procedure ExtractLinkFields;
    procedure ExtractFields;
    procedure SetDetailFields(const Value: string);
    procedure SourceChanged; virtual;
    procedure SourceDisabled; virtual;

    procedure DataConvert(Field: TField; Source,Dest: Pointer; ToNative: Boolean); override;

    function InternalFetchAll(AForce: Boolean): Boolean; virtual;
    function InternalFetchPacket: Boolean; virtual;

    procedure UnprepareFilter;
    procedure PrepareFilter(const Text: string; Options: TFilterOptions);
    procedure SetFiltered(Value: Boolean); override;
    procedure SetFilterOptions(Value: TFilterOptions); override;
    procedure SetFilterText(const Value: string); override;
    procedure SetOnFilterRecord(const Value: TFilterRecordEvent); override;
    procedure SetFilterData(const Text: string; Options: TFilterOptions);
    procedure DoOnCalcFields; override;
    procedure EvaluteDefaults(AFieldKinds: TFieldKinds);
    procedure EvaluteChecks(AField: TField);
    procedure DoValidate(ASender: TField);

    function DirectFilter(ARec: TAstaDBListItem): Boolean;
    procedure BeginDirectRead(ARec: TAstaDBListItem; ANeedCalcFields: Boolean);
    procedure EndDirectRead;
    procedure BeginDirectWrite(ARec: TAstaDBListItem);
    procedure BeginDirectAppend;
    procedure EndDirectWrite;
    function GetOldFieldData(Field: TField; Buffer: Pointer): Boolean; virtual;

    property DirectWrite: Boolean read FDirectWrite;
    property DirectRead: Boolean read FDirectRead;

    property MasterFields: string read GetMasterFields write SetMasterFields;
    property MasterSource: TDataSource read GetDataSource write SetDataSource;
    property DetailFields :String read FDetailFields write SetDetailFields;
    property IndexDefs: TAstaIOIndexDefs read FIndexDefs write SetIndexDefs;
  public
    FDisposeAstaList: Boolean;
    FAstaList: TAstaDBList;

    procedure RequestNestedActivation(ADataSet: TAstaIOCustomDataset);
    procedure RemoveNestedActivation(ADataSet: TAstaIOCustomDataset);
    procedure ClearNestedActivations;
    procedure ExecuteNestedActivations;

    //procedure AddIndexFields(const TheIndexName: String; const FieldNames: array of String; const Descending: array of Boolean);
    procedure AddIndexFields(Const TheIndexName: string;const FieldNames: array of string; const ADescending: array of Boolean);

    procedure AddIndex(const FieldName: String; Descending: Boolean);overload;
    procedure AddIndex(const AIndexName, AIndexFields: String; AOptions: TAstaIOIndexOptions;
      const ADescFields: String = ''; const ACaseInsFields: String = ''; AGroupingLevel: Integer = 0); overload;
    procedure DeleteIndex(const AIndexName: String);
    function CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream; override;
    property BlobList: TAstaBlobList read FBlobList write FBlobList;
    function GetFieldData(Field: TField; Buffer: Pointer): Boolean; overload; override;
    function GetFieldData(Field: TField; var Buffer: TValueBuffer): Boolean; overload; override;
    {$IF CompilerVersion >= 33.0}
    procedure SetFieldData(Field: TField; Buffer: TValueBuffer); overload; override;
    {$IFEND}
    function BookmarkValid(Bookmark: TBookmark): Boolean; override;
    procedure DisposeAstaList;
    function FilterCount: integer;
    function IsSequenced: Boolean; override;
    function GetBookmarkAsInteger: Integer;
    destructor Destroy; override;
    function CompareBookmarks(Bookmark1, Bookmark2: TBookmark): Integer; override;
    function IsBlobField(F: Tfield): Boolean;
    function Lookup(const KeyFields: string; const KeyValues: Variant;
      const ResultFields: string): Variant; override;
    function LocateRecord(const KeyFields: string; const KeyValues: Variant;
      Options: TLocateOptions): Boolean;
    function Locate(const KeyFields: string; const KeyValues: Variant;
      Options: TLocateOptions): Boolean; override;
    procedure UnRegisterClone(Source: TAstaIOCustomDataset);
    procedure RemoveCloneLinks;
    procedure CloneCursor(Source: TAstaIOCustomDataset; KeepFilter: Boolean = false);
    procedure InternalCleanCloneFromDataSet(D: TDataSet);
    procedure CleanCloneFromDataSet(D: TDataSet; AddData: Boolean = True;
      CopyBookMarks: Boolean = False; FieldsToSkip: TStrings = nil);
    procedure CloneSingleRecordFromSource(SourceDataSet :TDataSet; DoBlobs :Boolean = True); virtual;
    constructor Create(AOwner: TComponent); override;
    function AstaFieldCreate(ClearFieldDefs: Boolean): Boolean;
    procedure DataTransfer(D: TDataSet; IncludeBlobs: Boolean; IncludeBookmarks: Boolean = False);
    procedure NukeAllFieldInfo;
    procedure NukeUnMatchedFields(D: TDataSet);
    function CurrentBookMark: Integer;
    function GetRecordSize: Word; override;
    procedure SaveToStream(Stream: TStream); virtual;
    procedure LoadFromStream(Stream: TStream); virtual;
    procedure LoadFromStreamwithFields(Stream: TStream);
    procedure SaveToFile(const FileName: string = ''); virtual;
    procedure LoadFromFile(const FileName: string = ''); virtual;
    procedure LoadFromFileWithFields(const FileName: string = ''); virtual;
    {$ifdef AstaIOXML}
    procedure LoadFromXML(const FileName: string);overload;
    procedure LoadFromXML(Const Stream:Tstream);overload;
    procedure SaveToXML(const FileName: string;XMLFormat:TAstaIOXMLDataSetFormat=taxADO);overload;
    procedure SaveToXML(Stream:TStream;XMLFormat:TAstaIOXMLDataSetFormat);overload;
    function DataSetFactory(const dsName: string;const fldDefs: array of ThunkFieldDef): TDataSet;
    {$endif}
    function SaveToString: AnsiString;
    procedure LoadFromString(S: AnsiString);
    procedure LoadFromStringWithFields(S: AnsiString);
    procedure StoreMemoryStream(Field: TField; M: TMemoryStream);
    function GetMemoryStream(Field: TField): TMemoryStream;
    procedure DirectListTransfer(DS: TAstaIOCustomDataset);
    procedure CloneFieldsFromDataSetPreserveFields(D: TDataSet; AddData, IncludeBlobs: Boolean);
    procedure CloneFieldsFromDataSet(D: TDataSet; AddData, IncludeBlobs: Boolean);
    procedure SmartRetrieveFieldsFromDataSet(D: TDataSet);
    procedure RetrieveFieldsFromDataSet(D: TDataSet; ClearFields: Boolean; DoRequired :Boolean = True); // sm 9/8/2001 - Added DoRequired
    procedure AddField(FieldName: string; FType: TFieldType; FSize: Integer; FRequired: Boolean = False);
    procedure Empty; virtual;
    function BookMarkToRawListFetch(FieldName: string; Index: Integer): string;
    procedure AppendSingleRecordFromSource(SourceDataSet :TDataSet);
    procedure ChangesSaved;
    procedure FetchAll(AForce: Boolean);
    procedure ApplyRange;
    procedure CancelRange;
    procedure EditRangeEnd;
    procedure EditRangeStart;
    procedure SetRange(const StartValues, EndValues: array of const);virtual;
    procedure SetRangeEnd;
    procedure SetRangeStart;
    procedure SetKey;
    procedure EditKey;
    function FindKey(AnIndexName:String;const KeyValues: array of const): Boolean;overload;
    function FindKey(const KeyValues: array of const): Boolean;overload;virtual;
    procedure FindNearest(const KeyValues: array of const);
    function GotoKey: Boolean;
    procedure GotoNearest;
    procedure Cancel; override;
    procedure Post; override;
    procedure UnprepareExpressions;
    procedure PrepareExpressions;
    procedure DisableConstraints;
    procedure EnableConstraints;
    function ConstraintsDisabled: Boolean;
    procedure BeginBatch(AAllowActivate: Boolean = True);
    procedure EndBatch;
    function InBatch: Boolean;
    function CompareField(AField: TField; key1, key2: TAstaDBlistItem;
      APartialComp, ACaseInsensitive: Boolean): Integer;
    property IndexFieldCount: Integer read GetIndexFieldCount;
    property IndexFields[Index: Integer]: TField read GetIndexField;
    property KeyExclusive: Boolean read GetKeyExclusive write SetKeyExclusive;
    property KeyFieldCount: Integer read GetKeyFieldCount write SetKeyFieldCount;
    property SaveChanges: Boolean read FSaveChanges;
    property StreamOptions:TAstaIOStreamSaveOptions read FStreamOptions write FStreamOptions;
  published
    property AutoCreateCalcFields :TAutoCreateCalcFields read FAutoCreateCalcFields write FAutoCreateCalcFields;
    property FileName: TAstaFileName read FFileName write FFileName;
    property StoreDefs: Boolean read FStoreDefs write FStoreDefs default False;
    property AutoCalcFields;
    property ReadOnly: Boolean read FReadOnly write FReadOnly default false;
    property About: String read FAbout write FAbout;
    property Active;
    property Indexes: TAstaIOIndexes read FIndexes write SetIndexes;
    property IndexFieldNames: String read GetIndexFieldNames write SetIndexFieldNames;
    property IndexName: String read GetIndexName write SetIndexName;
    property Aggregates: TAstaIOAggregates read FAggregates write SetAggregates;
    property Filter;
    property FilterOptions;
    property Constraints;
    property Filtered;
    property ObjectView default True;
    property BeforeOpen;
    property AfterOpen;
    property BeforeClose;
    property AfterClose;
    property BeforeInsert;
    property AfterInsert;
    property BeforeEdit;
    property AfterEdit;
    property BeforePost;
    property AfterPost;
    property BeforeCancel;
    property AfterCancel;
    property BeforeDelete;
    property AfterDelete;
    property BeforeScroll;
    property AfterScroll;
    property OnCalcFields;
    property OnDeleteError;
    property OnEditError;
    property OnFilterRecord;
    property OnNewRecord;
    property OnPostError;
  end;

  TAstaIODataSet = class(TAstaIOCustomDataset)
  private
    function FieldDefsStored: Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property FieldDefs stored FieldDefsStored;
  end;

  TAstaCustomAuditDataSet = class(TAstaIOCustomDataset)
  private
    procedure SetStatusFilter(const Value: TUpdateStatusSet);
  protected
    FParams: TParams;
    FMultiTableDataset,FFieldMapDataSet,
    FOldValuesDataSet: TAstaIODataSet;
    FUpdateMethod: TAstaUpdateMethod;
    FUpdateMode: TUpdateMode;
    FSavePoint: Integer;
    FIsBusyCancel: Boolean;
    FOpening:Boolean;
    FBookMarkAccessed: Boolean;
    FNoSQLFields: TStrings;
    FRefetchFields: TStrings;
    FPrimeFields: TStrings;
    FUpdateTableName: string;
    FAutoIncrementField, FSequence: string;
    FOffLine: TOffLineString;
    FStatusFilter: TUpdateStatusSet;
    FBeforePostState: TDataSetState;

    function GetParams: TParams;
    procedure SetParams(Value: TParams);
    function GetParamsCount: Integer;

    procedure SetupFieldMapDataSet;virtual;
    procedure SetupMultiTableDataSet;virtual;
    procedure SetupDeltaDataSet;
    procedure PopOldValues;virtual; //sm 06/01/2003
    procedure FixOldValues(Value: TDeltaType);
    procedure PushRowData(Value: TDeltaType); virtual;
    function FieldDefsStored: Boolean;

    function DeleteFromUpdateCache: Boolean;
    function RowHasBeenChanged: Boolean;
    function GetChangedCount: Integer;
    function AlreadyAppended: Boolean;
    procedure SetUpdateMethod(Value: TAstaUpdateMethod);
    procedure SetSavePoint(Value: Integer);
    function GetSavePoint: Integer;
    function CurrentValueDataSetRecord(DataSet:TAstaIODataSet = nil): TAstaIODataSet;
    procedure RollBackCache(Value: Integer);
    procedure BookMarkPositionFromDeleted;
    procedure RestoreDeltaValues(UseAppend: Boolean);
    function InternalRevertRecord(DoDeltaBookMarkCheck :Boolean = True): Boolean;

    procedure SetPrimeFields(Value: TStrings);
    function GetPrimeFields: TStrings;
    procedure SetNoSQLFields(Value: TStrings);
    function GetNoSQLFields: TStrings;
    procedure SetRefetchFields(Value: TStrings);
    function GetRefetchFields: TStrings;

    procedure DoAfterCancel; override;
    procedure DoBeforeEdit; override;
    procedure DoBeforeDelete; override;
    procedure DoAfterInsert; override;
    procedure DoBeforePost; override;
    procedure DoAfterPost; override;
    procedure InternalClose; override;

    procedure InternalPost; override;
    procedure InternalDelete; override;

    procedure DoDBUpdates; virtual;
    function CheckRecordStatusFilter: Boolean; override;
    procedure RecordStatusFilter(DataSet: TDataSet; var Accept: Boolean); override;
    function GetOldFieldData(Field: TField; Buffer: Pointer): Boolean; override;

    property UpdateMethod: TAstaUpdateMethod read FUpdateMethod write SetUpdateMethod;
    property UpdateMode: TUpdateMode read FUpdateMode write FUpdateMode default upWhereKeyOnly;

    property RefetchFields: TStrings read GetRefetchFields write SetRefetchFields;
    property PrimeFields: TStrings read GetPrimeFields write SetPrimeFields;
    property NoSQLFields: TStrings read GetNoSQLFields write SetNoSQLFields;
    property Sequence: string read FSequence write FSequence;
    property AutoIncrementField: string read FAutoIncrementField write FAutoIncrementField;
    property UpdateTableName: string read FUpdateTableName write FUpdateTableName;
    property Params: TParams read GetParams write SetParams;
    property OffLine: TOffLineString read FOffLine write FOffLine;
  public
    procedure MapColumn(CurrentField,MapToField,MapTableName:String);
    procedure SetMultiFieldUpdateTableName(AFieldName,AnUpdateTableName:String;Primekey:Boolean);
    procedure SaveToStream(Stream: TStream); override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure EmptyCache;
    procedure Empty; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function UpdatesPending: Boolean;
    function TrackDeltas: Boolean;
    function MoveToBookMarkFromDelta: Boolean;
    function RevertRecord: Boolean;
    procedure CancelUpdates;
    function UndoLastChange(FollowChange: Boolean): Boolean;
    function UpdateStatus: TUpdateStatus; override;
    function DeltaDataSetCurrentValueDataSet: TAstaIODataSet;
    property MappingDataSet: TAstaIODataSet read FFieldMapDataSet;
    property MultiTableDataSet: TAstaIODataSet read FMultiTableDataSet;
    property OldValuesDataSet: TAstaIODataSet read FOldValuesDataSet;
    property ChangeCount: Integer read GetChangedCount;
    property SavePoint: Integer read GetSavePoint write SetSavePoint;
    property StatusFilter: TUpdateStatusSet read FStatusFilter write SetStatusFilter;
  published
    property FieldDefs stored FieldDefsStored;
  end;

  TAstaIOAuditDataSet = class(TAstaCustomAuditDataSet)
  private
  protected
  public
    constructor Create(AOwner: TComponent); override;
    property UpdateMode;
  published
    property UpdateMethod;
  end;

function IsABlobField(d: TdataSet; FieldName: string): Boolean;
function DataSetToStringwithFieldProperties(D: TAstaIODataSet): AnsiString;
function DataSetToString(D: TAstaIODataSet): AnsiString;
function StringToDataSet(S: AnsiString): TAstaIODataSet;
function StringToDataSetwithFieldProperties(S: AnsiString): TAstaIODataSet;
function CloneDataSetToString(D: TDataSet): AnsiString;overload;
function CloneDataSetToString(D: TDataSet;RowsToReturn:Integer): AnsiString;overload;

procedure PopulateDataSetFromStringWithFieldProperties(DS: TAstaIODataSet; const S: AnsiString);
function AstaISQLBenchmarkString(D: TAstaIOCustomDataset; STartTime, Endtime: TTimeStamp): string;
function DBFieldsAreNotEqual(F1, F2: TField): Boolean;
procedure DBFieldsAssign(ADestField, ASrcField: TField);

implementation



uses
  DBCommon, DBConsts, Variants, SqlTimSt, AstaIOBits,
  AstaIODataSetUtils, AstaIOResources, AstaIOClientDataSet, AstaIOConst;



const
  FilterRangesNotSet = -2;
  DefaultCursor = 0;
  HourGlassCursor = -11;
  AstaIOStreamSignature=93323134;


{ TAutoCreateCalcField }

constructor TAutoCreateCalcField.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FParent:=TAutoCreateCalcFields(Collection);
  FParent.LastItemCount:=FParent.LastItemCount + 1;
  FFieldName:='AutoCalc' + IntToStr(FParent.LastItemCount);
  FVisible:=True;
  FDataType:=cfInteger;
end;

procedure TAutoCreateCalcField.SetDataType(Value :TAutoCreateCalcFieldType);
begin
  if (FDataType <> cfString) and ((Value = cfString) or (Value = cfWideString)) then
    FSize:=20;
  FDataType:=Value;
end;

procedure TAutoCreateCalcField.SetActive(Value :Boolean);
begin
  if Value then
  begin
    if Trim(FFieldName) = '' then
      raise EDataBaseError.Create(FNoFieldName);
  end;
  FActive:=Value;
end;

function TAutoCreateCalcField.GetDisplayName :String;
begin
  Result:=FFieldName;
end;

{ TAutoCreateCalcFields }

constructor TAutoCreateCalcFields.Create(Owner: TComponent);
begin
  inherited Create(TAutoCreateCalcField);
  FOwner:=Owner;
  FLastItemCount:=0;
end;

destructor TAutoCreateCalcFields.Destroy;
begin
  inherited Destroy;
end;

function TAutoCreateCalcFields.GetOwner: TPersistent;
begin
  Result:=FOwner;
end;

procedure TAutoCreateCalcFields.SetItem(Index: Integer; Value: TAutoCreateCalcField);
begin
  inherited Items[Index].Assign(Value);
end;

function TAutoCreateCalcFields.GetItem(Index: Integer): TAutoCreateCalcField;
begin
  Result:=TAutoCreateCalcField(inherited Items[Index]);
end;

function TAutoCreateCalcFields.Insert(Index:Integer): TAutoCreateCalcField;
begin
  Result:=Add;
  Result.Index:=Index;
end;

function TAutoCreateCalcFields.Add: TAutoCreateCalcField;
begin
  Result:=TAutoCreateCalcField(inherited Add);
end;

procedure TAstaIOCustomDataset.AddAutoCreateCalcFields(OwnerDataSet :TDataSet);
var i  :Integer;

function GetFieldClass(FieldType :TAutoCreateCalcFieldType) :TFieldClass;
begin
  case FieldType of
    cfString     :Result:=TStringField;
    cfInteger    :Result:=TIntegerField;
    cfSmallInt   :Result:=TSmallIntField;
    cfWord       :Result:=TWordField;
    cfFloat      :Result:=TFloatField;
    cfCurrency   :Result:=TCurrencyField;
    cfBCD        :Result:=TBCDField;
    cfFMTBCD     :Result:=TFMTBCDField;
    cfBoolean    :Result:=TBooleanField;
    cfDateTime   :Result:=TDateTimeField;
    cfDate       :Result:=TDateField;
    cfTime       :Result:=TTimeField;
    cfLargeInt   :Result:=TLargeIntField;
    cfWideString :Result:=TWideStringField;
    else          Result:=nil;
  end;
end;

begin
  for i:=0 to FAutoCreateCalcFields.Count - 1 do
  begin
    if FAutoCreateCalcFields[i].Active then
    begin
      with GetFieldClass(FAutoCreateCalcFields[i].DataType).Create(OwnerDataSet) do
      begin
        FieldName:=FAutoCreateCalcFields[i].FieldName;
        Calculated:=True;
        DataSet:=OwnerDataSet;
        Visible:=FAutoCreateCalcFields[i].Visible;
        Name:=FieldName;
        Size:=FAutoCreateCalcFields[i].Size;
      end;
    end;
  end;
end;

function TAstaIOCustomDataset.Lookup(const KeyFields: string; const KeyValues: Variant;
  const ResultFields: string): Variant;
var
  bmk: Integer;
begin
  CheckBrowseMode;
  bmk := Indexes.Position;
  FDoEvents := False;
  try
    if LocateRecord(KeyFields, KeyValues, []) then
    begin
      SetTempState(dsCalcFields);
      try
        GetRecord(TempBuffer, gmCurrent, True);
        CalculateFields(TempBuffer);
        Result := FieldValues[ResultFields];
      finally
        RestoreState(dsBrowse);
      end;
    end
    else
      Result := Null;
  finally
    FDoEvents := True;
    Indexes.Position := bmk;
  end;
end;

function TAstaIOCustomDataset.Locate(const KeyFields: string; const
 KeyValues: Variant; Options: TLocateOptions): Boolean;
begin
  CheckBrowseMode;
  DoBeforeScroll;
  FDoEvents := False;
  try
    Result := LocateRecord(KeyFields, KeyValues, Options);
  finally
    FDoEvents := True;
  end;
  if Result then
  begin
    Resync([rmExact, rmCenter]);
    DoAfterScroll;
  end;
end;

function TAstaIOCustomDataset.LocateRecord(const KeyFields: string; const KeyValues: Variant;
  Options: TLocateOptions): Boolean;
var
  iPos: Integer;
  tmpItem: TAstaDBListItem;
  V: Variant;
  fld: TField;
begin
  tmpItem := TAstaDBListItem.CreateForLocate(FAstaList, 0);
  try
    BeginDirectWrite(tmpItem);
    try
      if Pos(';', KeyFields) = 0 then begin
        if VarIsArray(KeyValues) then
          V := KeyValues[0]
        else
          V := KeyValues;
        fld := FieldByName(KeyFields);
{$ifdef Delphi6AndUP}
        if fld.DataType = ftLargeInt then
          TLargeIntField(fld).Value := V
        else
{$endif}
          fld.Value := V;
      end
      else
        FieldValues[KeyFields] := KeyValues;
    finally
      EndDirectWrite;
    end;
    Result := (Indexes.LocateRecord(tmpItem, KeyFields, [loFiltered], Options, iPos) = 0);
    if Result then
      if iPos = -1 then
        Result := False
      else
      Indexes.Position := iPos;
  finally
    tmpItem.Free;
  end;
end;

function TAstaIOCustomDataset.GetIsIndexField(Field: TField): Boolean;
var
  i: Integer;
begin
  Result := False;
  if FIndexes.SelectedIndex <> nil then
    for i := 0 to FIndexes.SelectedIndex.FieldCount - 1 do
      if AnsiCompareText(FIndexes.SelectedIndex.FieldNames[i], Field.FieldName) = 0 then begin
        Result := True;
        Break;
      end;
end;

procedure TAstaIOCustomDataset.SetIndexDefs(const Value: TAstaIOIndexDefs);
begin
  FIndexDefs.Assign(Value);
end;
{$IFDEF indexdefs}
procedure TAstaIOCustomDataset.UpdateIndexDefs;
begin
  IndexDefs.Indexes2Defs;
end;
{$endif}

function TAstaIOCustomDataset.IsSequenced: Boolean;
begin
  Result := (FRangeFrom = nil) and (FRangeTo = nil);
end;

function TAstaIOCustomDataset.IsDesignTime: Boolean;
begin
  result := ((csloading in componentstate) or (csdesigning in componentstate));
end;

function TAstaIOCustomDataset.AstaFieldCreate(ClearFieldDefs: Boolean): Boolean;
var
  i: Integer;
begin
  FIsOpen := False;
  if FAstaList = nil then begin
    FAstaList := TAstaDBList.Create; {creates is own field list manager}
    FIndexes.DBListCreated;
    FAggregates.DBListCreated;
  end
  else if ClearFieldDefs then begin
    FAstaList.Clear;
    FAstaList.FFieldList.Clear;
  end;

  if ClearFieldDefs or (FAstaList.FFieldList.Count = 0) then begin
    for i := 0 to FieldDefs.Count - 1 do
      FAstaList.AddField(FieldDefs[i].Name, FieldDefs[i].DataType, FieldDefs[i].Size, FieldDefs[i].Precision);
    FAstaList.FieldsDefined;
  end;

  result := FieldDefs.Count > 0;
end;

procedure TAstaIOCustomDataset.StoreMemoryStream(Field: TField; M: TMemoryStream);
var
  BlobID: Integer;
  temp: TMemoryStream;
  destBuffer: Pointer;
begin
  if FBlobList = nil then
    FBlobList := TAstaBlobList.Create;
  GetFieldData(Field, @Blobid);
  if (blobid <= 0) or (State = dsInsert) then begin
    if ((M = nil) or (M.size = 0)) and (State in [dsinsert]) then
      Exit;
    Blobid := FBlobList.AddBlobAutoInc(M, Field.FieldNo);
    SetFieldDataNoDataEvent(Field, @Blobid);
  end
  else begin
    temp := FBlobList.GetBlob(Blobid, Field.FieldNo);
    if (temp = nil) then
      Exit;
    Temp.LoadFromSTream(M);
    temp.position := 0;
  end;
   //11/27/2000 for null memo/blob flag
  DestBuffer := GetActiveRecordbuffer;
  SetChangedFlag(Field, (M <> nil) and (M.size > 0), DestBuffer);
end;

function TAstaIOCustomDataset.GetMemoryStream(Field: TField): TMemoryStream;
var
  Blobspot: Integer;
begin
  result := nil;
  if not Active then
    Exit;
  if FBlobList = nil then
    FBlobList := TastaBlobList.Create;
  case State of
    dsedit:
      begin
//       Setmodified(True); //08/29/98 sg
        //Since Blob's are stored as Integers to a List ID number, they
        //don't change if the blob is edited with a dbmemo
        //SetModified toggles the modified flag if a memo has changed
        GetFieldData(Field, @BlobSpot);
        Result := FBlobList.GetBlob(BlobSpot, Field.FieldNo);
        if Result = nil then begin
          Exit;
          // 12/14/2000 - sm
          {Result := TMemoryStream.Create;
          BlobSpot := FBlobList.AddBlobAutoInc(Result, Field.FieldNo);
          SetFieldDataNoDataEvent(Field, @BlobSpot);}
        end;
      end;
    dsinsert:
      begin
        GetFieldData(Field, @BlobSpot);
        Result := FBlobList.GetBlob(BlobSpot, Field.FieldNo);
        if Result = nil then begin
          // AP???
          Exit;
          // 12/14/2000 - sm
          {Result := TMemoryStream.Create;
          BlobSpot := FBlobList.AddBlobAutoInc(Result, Field.FieldNo);
          SetFieldDataNoDataEvent(Field, @BlobSpot);}
        end;
      end;
  else
    begin
      if RecordCount = 0 then
        Exit;
      GetFieldData(Field, @BlobSpot);
      Result := FBlobList.GetBlob(BlobSpot, Field.FieldNo);
    end;
  end;
  if Result <> nil then
    Result.position := 0;
end;

function TAstaIOCustomDataset.GetCanModify: Boolean;
begin
  result := not FReadOnly;
end;



constructor TAstaIOCustomDataset.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FAstaList := nil;
  FFilterBuffer := nil;
  FBlobList := TAstaBlobList.Create; //changed from nil
  {Manages Blobs by BookMarks and FieldNumbers}
  FDisposeAstaList := False;
  FReadOnly := False;
  FBlobRollBackList := nil;
  FCloneList := nil;
  FEchoEditAppend := True;
  NestedDataSetClass := TAstaIOCustomClientDataSet;
  ObjectView := True;
  FReqActList := nil;
  FBatchStates := TList.Create;
  FIndexes := TAstaIOIndexes.Create(Self);
  FIndexes.OnChanging := DoIndexChanging;
  FAggregates := TAstaIOAggregates.Create(FIndexes, FAstaList);
  FSuspendEvents := False;
  FDoEvents := False;
  FDoCheckRanges := True;

  FExpressions := TStringList.Create;
  FExpressions.Sorted := True;
  FExpressions.Duplicates := dupError;
  FExpressionUsages := [];
  FConstDisableCount := 0;

  FIndexDefs := TAstaIOIndexDefs.Create(Self, FIndexes);
  FMasterFieldsList := TStringList.Create;
  FDetailFieldsList := TStringList.Create;
  FMasterLink := TMasterDataLink.Create(Self);

  FDataLink := TAstaIODataLink.Create(Self);
  FAutoCreateCalcFields:=TAutoCreateCalcFields.Create(Self);
end;


destructor TAstaIOCustomDataset.Destroy;
begin
  FBatchStates.Free;
  FBatchStates := nil;

  FIndexes.Active := False;
  FIndexes.Clear;
  FAggregates.Active := False;
  FAggregates.Clear;

  

  FIndexDefs.Free;
  FMasterFieldsList.Free;
  FDetailFieldsList.Free;
  FMasterLink.Free;
  FDataLink.Free;

  ClearNestedActivations;
  FBlobRollBackList.Free;
  //10/26/98 eek! av's at design time
  if not (csDesigning in ComponentState) or (csLoading in ComponentState) then
    FBlobList.Free;
  if (FAstaList <> nil) and not (csDesigning in ComponentState) and not Active  then
    DisposeAstaList
  else
    FDisposeAstaList := True;
  RemoveCloneLinks;
  FCloneList.Free;

  UnprepareFilter;
  UnprepareExpressions;
  FExpressions.Free;
  FExpressions := nil;

  inherited Destroy;
  FAutoCreateCalcFields.Free;
  // sm - 6/27/2003 - swithed the sequence to first free aggregates and then indexes
  FAggregates.Free;
  FAggregates := nil;
  FIndexes.Free;
  FIndexes := nil;
end;

procedure TAstaIOCustomDataset.BeginBatch(AAllowActivate: Boolean);
var
  prevInd, prevBmk, prevAgg: Boolean;
begin
  DisableControls;
  if not Active and AAllowActivate then
    Active := True;
  DisableConstraints;
  prevInd := FIndexes.Active;
  FIndexes.Active := False;
  prevBmk := FIndexes.BMKActive;
  FIndexes.BMKActive := True;
  prevAgg := FAggregates.Active;
  FAggregates.Active := False;
  FBatchStates.Add(Pointer(Integer(prevInd) * 4 + Integer(prevBmk) * 2 + Integer(prevAgg)));
end;

procedure TAstaIOCustomDataset.EndBatch;
var
  i: Integer;
begin
  i := Integer(FBatchStates.Last);
  FBatchStates.Delete(FBatchStates.Count - 1);
  FIndexes.Active := i and 4 <> 0;
  FIndexes.BMKActive := i and 2 <> 0;
  FAggregates.Active := i and 1 <> 0;
  EnableConstraints;
  EnableControls;
end;

function TAstaIOCustomDataset.InBatch: Boolean;
begin
  Result := (FBatchStates <> nil) and (FBatchStates.Count > 0);
end;

function TAstaIOCustomDataset.GetDetailFieldByIndex(Index: Integer): string;
begin
  result := '';
  result := TokenCount(FDetailFields + ';', Index, ';');
end;

function TAstaIOCustomDataset.IsMasterActive(DataSource: TDataSource): Boolean;
begin
  Result := Assigned(DataSource) and (DataSource.DataSet.Active);
end;

function TAstaIOCustomDataset.GetMasterFieldByIndex(Index: Integer): string;
begin
  result := '';
  result := TokenCount(MasterFields + ';', Index, ';');
end;

function TAstaIOCustomDataset.GetMasterFields: string;
begin
  Result := FMasterLink.FieldNames;
end;

procedure TAstaIOCustomDataset.SetMasterFields(const Value: string);
begin
  if FMasterLink.FieldNames <> Value then
    DataEvent(dePropertyChange, 0);
  FMasterLink.FieldNames := Value;
end;

procedure TAstaIOCustomDataset.SetDataSource(Value: TDataSource);
begin
  if IsLinkedTo(Value) then DatabaseError(SCircularDataLink, self);
  if FDataLink <> nil then
    FDataLink.DataSource := Value;
  {if FMasterLink.DataSource <> Value then
    DataEvent(dePropertyChange, 0);}
end;

function TAstaIOCustomDataset.GetDataSource: TDataSource;
begin
  if FDataLink = nil then
    Result:=nil
  else
    Result:=FDataLink.DataSource;
end;

procedure TAstaIOCustomDataset.ExtractLinkFields;
var
  i: Integer;
begin
  FMasterFieldsList.Clear;
  FDetailFieldsList.Clear;
  i := 1;
  while i <= Length(MasterFields) do
    FMasterFieldsList.Add(ExtractFieldName(MasterFields, i));
  i := 1;
  while i <= Length(FDetailFields) do
    FDetailFieldsList.Add(ExtractFieldName(FDetailFields, i));
end;

procedure TAstaIOCustomDataset.ExtractFields;
var
  i: Integer;
begin
  FMasterFieldsList.Clear;
  i := 1;
  while i <= Length(MasterFields) do
    FMasterFieldsList.Add(ExtractFieldName(MasterFields, i));
end;

procedure TAstaIOCustomDataset.SetDetailFields(const Value: string);
begin
  FDetailFields := Value;
end;

procedure TAstaIOCustomDataset.SourceChanged;
begin

end;

procedure TAstaIOCustomDataset.AppendSingleRecordFromSource(SourceDataSet :TDataSet);
var i      :Integer;
begin
  try
    DisableControls;
    if not Active then Active:=True;
    AppendRecord([]);
    Edit;
    for i:=0 to SourceDataSet.FieldCount - 1 do
      if (FindField(SourceDataSet.Fields[i].FieldName) <> nil)
        and (SourceDataSet.Fields[i].FieldKind in [fkData, fkInternalCalc]) then
        // and not (SourceDataSet.Fields[i].DataType in [ftFmtMemo, ftMemo, ftGraphic, ftBlob, ftVarBytes]) then
        FieldByName(SourceDataSet.Fields[i].Fieldname).Assign(SourceDataSet.Fields[i]);
    Post;
  finally
    EnableControls;
  end;
end;

procedure TAstaIOCustomDataset.SourceDisabled;
begin

end;

procedure TAstaIOCustomDataset.LoadFromFile(const FileName: string = '');
var
  Stream: TStream;
  AFileName: string;
begin
  if (FileName = '') and (Self.FileName = '') then
    DatabaseError(SNoFileName, Self);

  AFileName := FileName;
  if FileName = '' then
    AFileName := Self.FileName;

  try
    Stream := TFileStream.Create(AFileName, fmOpenRead);
  except
    raise EDataBaseError.Create(SysUtils.Format(SUnAbleToLoadFromFile, [AFileName]));
  end;
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TAstaIOCustomDataset.LoadFromFileWithFields(const FileName: string = '');
var
  Stream: TStream;
  AFileName: string;
begin
  if (FileName = '') and (Self.FileName = '') then
    DatabaseError(SNoFileName, Self);

  AFileName := FileName;
  if FileName = '' then
    AFileName := Self.FileName;

  try
    Stream := TFileStream.Create(AFileName, fmOpenRead);
  except
    raise EDataBaseError.Create(SysUtils.Format(SUnAbleToLoadFromFile, [AFileName]));
  end;
  try
    LoadFromStreamwithFields(Stream);
    Refresh;
  finally
    Stream.Free;
  end;
end;

procedure TAstaIOCustomDataset.SaveToFile(const FileName: string = '');
var
  Stream: TStream;
  AFileName: string;
begin
  if (FileName = '') and (Self.FileName = '') then
    DatabaseError(SNoFileName, Self);

  AFileName := FileName;
  if FileName = '' then
    AFileName := Self.FileName;

  Stream := TFileStream.Create(AFileName, fmCreate);
  try
    SaveToStream(Stream);
  finally
    Stream.Free;
  end;
end;

procedure TAstaIOCustomDataset.SetBufferSizeFromFieldList;
begin
  FRawBufSize := FAstaList.FBufferSize;
end;

function TAstaIOCustomDataSet.GetUniqueBookMark: Integer;
var
  BM: TBookmarkStr;
begin
  Result := FAstaList.GetLastBookMark + 1;
  while True do begin
    SetLength(BM, SizeOf(Integer));
    Move(Result, BM[1], SizeOf(Integer));
    if not BookmarkValid(TBookmark(BM)) then Break;
    Inc(Result);
  end;
end;

procedure TAstaIOCustomDataset.InternalOpen;
var
  i, j: Integer;
begin
  for i:=0 to FieldDefs.Count - 1 do
    if FieldDefs[i].DataType = ftBCD then
      FieldDefs[i].Size:=0;


  if (FAstaList = nil) or (FieldDefs.Count = 0) then
    AstaFieldCreate(True);

  if (Fields.Count = 0) and (FieldCount > 0) then
    AstaFieldCreate(False);

  FIndexes.Position := -1;
  FLastBookmark := FAstaList.Count;
  // AP???
  if FieldDefs.Count = 0 then
    InternalInitFieldDefs;
  SetBufferSizeFromFieldList;
// This is for RunTime removal of persistent fields
  if DefaultFields then
    CreateFields
  else if Fields.Count > 0 then
    RemoveInvalidPersistentFields;
  AddAutoCreateCalcFields(Self); // Only if no persistent fields. Need to find out when
  BindFields(true);
  GetMem(FFldOffs, sizeof(Integer) * FAstaList.FFieldList.Count);
  for i := 0 to FAstaList.FFieldList.Count - 1 do
  begin
    PIntegerArray(FFldOffs)^[i] := FAstaList.FFieldList.Items[i].FOffset;
  end;

  FRecInfoOfs := FRawBufSize;
  FStartCalculated := FRecInfoOfs + SizeOf(TRecInfo);
  FRecBufSize := FRecInfoOfs + SizeOf(TRecInfo) + CalcFieldsSize;
  BookmarkSize := SizeOf(Integer);
  FIsOpen := (FieldCount > 0);

  AllocKeyBuffers;
  PrepareExpressions;
  if Filter <> '' then
    PrepareFilter(Filter, FilterOptions);

  if FAstaList <> nil then
    FIndexes.Rebuild;
end;

procedure TAstaIOCustomDataset.SetChangedFlag(Field: TField; Changed: Boolean; Buffer: Pointer);
var
  Spot: Integer;
  FieldAddrInBuffer: Pointer;
begin
  if (FAstaList <> nil) and (Buffer <> nil) then
  begin
    Spot := ((Field.FieldNo - 1) div 32);
    FieldAddrInBuffer := FAstaList.GetNullBufferFromBuffer(Buffer);
    if Changed then
      SetBit(PIntegerArray(FieldAddrInBuffer)^[Spot], FAstaList.AdjustFieldNullOffset(Field.FieldNo - 1))
    else
      ClearBit(PIntegerArray(FieldAddrInBuffer)^[Spot], FAstaList.AdjustFieldNullOffset(Field.FieldNo - 1));
  end;
  if not FDirectWrite then
  begin
    FIndexes.RecordCheckUpdate(Field);
    FAggregates.RecordCheckUpdate(Field);
  end;
end;

function TAstaIOCustomDataset.GetChangedFlag(Field: TField; Buffer: Pointer): Boolean;
var
  Spot: Integer;
  FieldAddrInBuffer: Pointer;
begin
  Result := False;
  if (FAstaList <> nil) and (Buffer <> nil) then
  begin
    Spot := ((Field.FieldNo - 1) div 32);
    FieldAddrInBuffer := FAstaList.GetNullBufferFromBuffer(Buffer);
    Result := TestBit(PIntegerArray(FieldAddrInBuffer)^[Spot], FAstaList.AdjustFieldNullOffset(Field.FieldNo - 1));
  end;
end;

procedure TAstaIOCustomDataset.InternalInitFieldDefs;
var
  i: integer;
  fldsize: Integer;
begin
  if FAstaList = nil then
    Exit;
  if FAstaList.FFieldList.Count>0 then //sm 2/8/2003
   FieldDefs.Clear;
  for i := 0 to FAstaList.FFieldList.count - 1 do
    with FAstalist.FFieldList.items[i] do begin
      if (FFieldtype in [ftString, ftBytes, ftVarBytes, ftFixedChar,
         ftWideString, ftdataset ,ftguid]) then
        FldSize := FFieldSize
      else
        FldSize := 0;
      TFieldDef.Create(FieldDefs, FFieldName, FFieldType, FldSize, False, FieldDefs.Count + 1);
    end;
end;

//Thanks to TimYoung for the BCD routines
function PadLeft(Value: string; Padding: Byte; PadChar: Char): string;
var
  NumToPad: Byte;
  I: Byte;
begin
  Result := Value;
  if Length(Result) >= Padding then
    Exit;
  NumToPad := Padding - Length(Result);
  for I := 1 to NumToPad do
    Result := PadChar + Result;
end;

function FloorCurr(X: Currency): Currency;
begin
  if (X <> Int(X)) then
    Result := Int(X)
  else
    Result := X;
end;

function CompToFloat(Value: Comp): double;
var
  TempString: string;
  DecPos: Byte;
  Decimals: Byte;
  IsNegative: Boolean;
begin
  IsNegative := False;
  try
    TempString := FloatToStr(Value);
  except
    TempString := '04';
  end;
  if (Copy(TempString, 1, 1) = '-') then begin
    IsNegative := True;
    TempString := Copy(TempString, 2, Length(TempString) - 1);
  end;
  Decimals := Byte(StrToInt(Copy(TempString, Length(TempString), 1)));
  Delete(TempString, Length(TempString), 1);
  if (Length(TempString) < Decimals) then
    TempString := PadLeft(TempString, Decimals, '0');
  DecPos := (Length(TempString) - (Decimals - 1));
  Insert(FormatSettings.DecimalSeparator, TempString, DecPos);
  if (DecPos = 1) then
    TempString := PadLeft(TempString, Length(TempString) + 1, '0');
  if IsNegative then
    TempString := '-' + TempString;
  Result := StrToFloat(TempString);
end;

function PowerCurr(X: Currency; Y: Byte): Currency;
var
  Temp: Currency;
  I: Integer;
begin
  if (Y = 0) then
    Result := 1
  else begin
    if (X = 0) or (Y = 1) then
      Result := X
    else begin
      Temp := 1;
      for I := 1 to Abs(Y) do
        Temp := Temp * X;
      if (Y > 0) then
        Result := Temp
      else
        Result := (1 / Temp);
    end;
  end;
end;

function RoundCurr(Value: Currency; Decimals: Byte): Currency;
begin
  if (Value <> 0) then
  begin
    if (Decimals > 4) then
      Decimals := 4;
    if (Value > 0) then begin
      if (Decimals > 0) then
        Result := FloorCurr(((Value / PowerCurr(0.1, Decimals)) + 0.5)) *
          PowerCurr(0.1, Decimals)
      else
        Result := FloorCurr(Value + 0.5);
    end
    else begin
      if (Decimals > 0) then
        Result := FloorCurr(((Value / PowerCurr(0.1, Decimals)) - 0.5)) *
          PowerCurr(0.1, Decimals)
      else
        Result := FloorCurr(Value - 0.5);
    end;
  end
  else
    Result := 0;
end;

function PadRight(Value: string; Padding: Byte; PadChar: Char): string;
var
  NumToPad: Byte;
  I: Byte;
begin
  Result := Value;
  if Length(Result) >= Padding then
    Exit;
  NumToPad := Padding - Length(Result);
  for I := 1 to NumToPad do
    Result := Result + PadChar;
end;

function CurrToComp(Value: Currency; Decimals: Byte): Comp;
var
  TempString: string;
  DecPos: Byte;
  DigitsToPad: Integer;
begin
  try
    TempString := CurrToStr(RoundCurr(Value, Decimals));
  except
    TempString := '0';
  end;
  DecPos := Pos(FormatSettings.DecimalSeparator, TempString);
  if (DecPos > 0) then
  begin
    DigitsToPad := (Decimals - (Length(TempString) - DecPos));
    Delete(TempString, DecPos, 1);
    TempString := PadRight(TempString, Length(TempString) + DigitsToPad, '0');
  end
  else
  begin
    DigitsToPad := Decimals;
    TempString := PadRight(TempString, Length(TempString) + DigitsToPad, '0');
  end;
  TempString := TempString + IntToStr(Decimals);
  Result := StrToFloat(TempString);
end;

function CompToCurr(Value: Comp): Currency;
var
  TempString: string;
  DecPos: Byte;
  Decimals: Byte;
  IsNegative: Boolean;
begin
  IsNegative := False;
  try
    TempString := FloatToStr(Value);
  except
    TempString := '04';
  end;
  if (Copy(TempString, 1, 1) = '-') then
  begin
    IsNegative := True;
    TempString := Copy(TempString, 2, Length(TempString) - 1);
  end;
  Decimals := Byte(StrToInt(Copy(TempString, Length(TempString), 1)));
  Delete(TempString, Length(TempString), 1);
  if (Length(TempString) < Decimals) then
    TempString := PadLeft(TempString, Decimals, '0');
  DecPos := (Length(TempString) - (Decimals - 1));
  Insert(FormatSettings.DecimalSeparator, TempString, DecPos);
  if (DecPos = 1) then
    TempString := PadLeft(TempString, Length(TempString) + 1, '0');
  if IsNegative then
    TempString := '-' + TempString;
  Result := StrToCurr(TempString);
end;


procedure TAstaIOCustomDataset.Loaded;
begin
  inherited Loaded;
  if (csdesigning in componentstate) and (not active) and (FieldCount = 0)
    and (FastaList = nil) then
  begin
    AstaFieldCreate(True);
    FDisposeAstaList := True;
  end;
end;

procedure TAstaIOCustomDataset.DisposeAstaList;
begin
   Try                         // <<<<<<<<<<<<<<<<<<< Insert !!! jn - 09/06/2015
      ResetCursorRange;                               
      if Assigned(FAstaList) then
         FAstaList.Free;
      FAstaList := nil;
      FIndexes.DBListDestroyed;
      FDisposeAstaList := False;
      if FBlobList <> nil then
         FBlobList.Clear;
   Except                      // <<<<<<<<<<<<<<<<<<< Insert !!! jn - 09/06/2015
      // Silenciar A.V. quando ocorrer !
   End;                        // <<<<<<<<<<<<<<<<<<< Insert !!! jn - 09/06/2015
end;

procedure TAstaIOCustomDataset.InternalClose;
begin
Try
  //if there are no persistant fields then destroy calls InternalClose
  //if there are persistant fields then InternalClose fires before destroy
  UnprepareFilter;
  UnprepareExpressions;
  ResetCursorRange;
  FreeKeyBuffers;
  if (FastaList <> nil) and (FAstaList.FFieldList.Count > 0) and
     (FFldOffs <> nil) then begin
    FreeMem(FFldOffs, SizeOf(Integer) * FAstaList.FFieldList.Count);
    FFldOffs := nil;
  end;
  if FDisposeAstaList then
    DisposeAstaList;
  FLastBookmark := 0;
  FIndexes.Position := -1;
  BindFields(False);
  if DefaultFields then
    DestroyFields;
  FIsOpen := False;
Except
   // Quite !
End;
end;

function TAstaIOCustomDataset.IsCursorOpen: Boolean;
begin
  Result := FIsOpen;
end;

function TAstaIOCustomDataset.GetRecordCount: Longint;
begin
  Result := FIndexes.RecsCount;
end;

function TAstaIOCustomDataset.AllocRecordBuffer: TRecordBuffer;
begin
  GetMem(Result, FRecBufSize);
  Fillchar(result^, FRecbufSize, #0);
end;

procedure TAstaIOCustomDataset.FreeRecordBuffer(var Buffer: TRecordBuffer);
begin
  freemem(buffer, FRecBufSize);
end;

function TAstaIOCustomDataset.GetRecordSize: Word;
begin
  Result := FRawBufSize;
end;

procedure TAstaIOCustomDataset.InternalInitRecord(Buffer: TRecordBuffer);
begin
  FillChar(Buffer^, RecordSize, 0);
end;

procedure TAstaIOCustomDataset.InternalInsert;
begin
  if FIndexes.IsPositionValid then
    FIndexes.RecordInserting(FIndexes.Rec)
  else
    FIndexes.RecordCancel(nil);
end;

procedure TAstaIOCustomDataset.InternalEdit;
begin
  FIndexes.RecordUpdating(FIndexes.Rec);
  FAggregates.RecordUpdating(FIndexes.Rec);
end;

procedure TAstaIOCustomDataset.InternalCancel;
begin
  if FIndexes.IsPositionValid then begin
    FIndexes.RecordCancel(FIndexes.Rec);
    FAggregates.RecordCancel(FIndexes.Rec);
  end
  else begin
    FIndexes.RecordCancel(nil);
    FAggregates.RecordCancel(nil);
  end;
end;

procedure TAstaIOCustomDataset.InternalAddRecord(Buffer: Pointer; Append: Boolean);
var
  item: TAstaDBListItem;
begin
  InternalInsert;
  Inc(FLastBookmark);
  if Append or FIndexes.Ordered then begin
    InternalLast;
    item := FAstaList.AppendRow(FLastBookMark);
    //9 december fix for Insert Problem
  end
  else
    item := FAstaList.InsertRow(FIndexes.Position, FLastBookMark);
  item.GetFromBuffer(PwideChar(Buffer));
  try
    FIndexes.RecordInserted(item);
    FAggregates.RecordInserted(item);
  except
    item.Free;
    raise;
  end;
  FSaveChanges := True;
end;

procedure TAstaIOCustomDataset.InternalPost;
var
  f: TBookmarkFlag;
  item: TAstaDBListItem;
begin
  {$IFDEF Delphi6AndUP}
  inherited InternalPost;
  {$ENDIF}
  f := GetBookmarkFlag(activebuffer);
  if State <> dsEdit then begin
    Inc(FLastBookmark);
    if (f = bfInserted) and not FIndexes.Ordered then
      item := FastaList.InsertRow(FIndexes.Position, FLastBookMark)
    else
      item := FastaList.AppendRow(FLastBookMark);
  end
  else
    item := FIndexes.Rec;

  item.GetFromBuffer(PwideChar(ActiveBuffer));
  try
    if State <> dsEdit then begin
      FIndexes.RecordInserted(item);
      FAggregates.RecordInserted(item);
    end
    else begin
      FIndexes.RecordUpdated(item);
      FAggregates.RecordUpdated(item);
    end;
  except
    if State <> dsEdit then
      item.Free;
    raise;
  end;
  if FBlobRollBackList <> nil then
    FblobRollBackList.Clear;
  FSaveChanges := True;
end;

function TAstaIOCustomDataset.CompareBookmarks(Bookmark1, Bookmark2: TBookmark): Integer;
const
  RetCodes: array[Boolean, Boolean] of ShortInt = ((2, -1), (1, 0));
begin
  { Check for uninitialized bookmarks }
  Result := RetCodes[Bookmark1 = nil, Bookmark2 = nil];
  if Result = 2 then begin
    Result := PInteger(Pointer(BookMark1))^ - PInteger(Pointer(BookMark2))^;
    if Result < 0 then
      Result := -1
    else if Result > 0 then
      Result := 1
  end;
end;

function TAstaIOCustomDataset.CurrentBookMark: Integer;
begin
  Result := FIndexes.Bookmark;
end;

procedure TAstaIOCustomDataset.InternalDelete;
var
  blobid, i: Integer;
  r: TAstaDBListItem;
begin
  for i := 0 to fieldcount - 1 do
    case fields[i].datatype of
      ftDataSet,
        ftOraBlob,
        ftOraClob,
        ftmemo,
        ftblob,
        ftfmtmemo,
        fttypedbinary,
        ftgraphic:
        begin
          blobid := FIndexes.Rec.GetInteger(fields[i].fieldno - 1);
          FBlobList.BlobDelete(blobid, fields[i].fieldno);
        end;
    end;
  FSaveChanges := True;
  r := FIndexes.Rec;
  FAggregates.RecordDeleted(r);
  FIndexes.RecordDeleted(r);
  r.Destroy;
end;

function TAstaIOCustomDataset.GetRecord(Buffer: TRecordBuffer; GetMode: TGetMode; DoCheck: Boolean): TGetResult;
var
  Acceptable: Boolean;
  recCnt, priorPos: Integer;
  newRec: TAstaDBListItem;
begin
  Acceptable := False;
  recCnt := FIndexes.RecsCount;
  newRec := nil;
  if recCnt < 1 then
    Result := grEOF
  else begin
    Result := grOK;
    repeat
      case GetMode of
        gmNext:
          begin
            priorPos := -2;
            if (FIndexes.Position < recCnt - 1) and
               ((FIndexes.Position < 0) or (CheckRanges(FIndexes.Rec, 1) < 1)) then begin
              priorPos := FIndexes.Position;
              FIndexes.Position := FIndexes.Position + 1;
              newRec := FIndexes.Rec;
              if CheckRanges(newRec, 1) <> 0 then
                Result := grEOF
              else
                Result := grOK;
            end
            else
              Result := grEOF;
            if (Result = grEOF) and InternalFetchPacket then begin
                if priorPos <> -2 then
                  FIndexes.Position := priorPos;
                Result := GetRecord(Buffer, gmNext, DoCheck);
                Exit;
            end;
          end;
        gmPrior:
          if (FIndexes.Position > 0) and
             ((FIndexes.Position >= recCnt) or (CheckRanges(FIndexes.Rec, -1) > -1)) then begin
            FIndexes.Position := FIndexes.Position - 1;
            newRec := FIndexes.Rec;
            if CheckRanges(newRec, -1) <> 0 then
              Result := grBOF
            else
              Result := grOK;
          end
          else
            Result := grBOF;
        gmCurrent:
          if FIndexes.Position >= recCnt then
            Result := grEOF
          else if FIndexes.Position < 0 then
            Result := grBOF
          else begin
            newRec := FIndexes.Rec;
            case CheckRanges(newRec, 0) of
            -1: Result := grBOF;
             0: Result := grOk;
             1: Result := grEOF;
            end;
          end;
      end;
      if (Result = grOK) and (newRec <> nil) then begin
        newRec.PutToBuffer(PChar(Buffer));
        with PRecInfo(PAnsiChar(Buffer) + FRecInfoOfs)^ do begin
          BookmarkFlag := bfCurrent;
          Bookmark := FIndexes.BookMark;
        end;
        if CalcFieldsSize <> 0 then begin
          ClearCalcFields(Buffer);
          GetCalcFields(buffer);
        end;
        Acceptable := FilterRecord(Buffer);
        if (GetMode = gmCurrent) and not Acceptable then
          Result := grError;
      end
      else if (Result = grError) and DoCheck then
        DatabaseError(SNoRecords, Self);
    until (Result <> grOk) or Acceptable;
  end;
end;

procedure TAstaIOCustomDataset.GetBookmarkData(Buffer: TRecordBuffer;
  Data: Pointer);
begin
  PLongInt(Data)^ := PRecInfo(PAnsiChar(Buffer) + FRecInfoOfs).Bookmark;
end;

function TAstaIOCustomDataset.GetBookmarkAsInteger: Integer;
var
  S: string;
begin
  result:=0;
  if BookmarkAvailable then begin
    result := 0;
    if Length(BookMark) = 0 then
      Exit;
    //move(BookMark[0], result, sizeof(result));
    Result := PInteger(BookMark)^;
  end
  else if State = dsFilter then
    GetBookmarkData(TRecordBuffer(FFilterBuffer), @Result)
  else if state= dsInsert then result:=FlastBookMark
end;

procedure TAstaIOCustomDataset.SetBookmarkData(Buffer: TRecordBuffer; Data: Pointer);
begin
  PRecInfo(PAnsiChar(Buffer) + FRecInfoOfs).Bookmark := PLongInt(Data)^;
end;

{
procedure TAstaIOCustomDataset.SetBookmarkData(Buffer: TRecordBuffer; Bookmark: TBookmark);
var
  BmVal: Integer;
begin
  if Length(Bookmark) = SizeOf(Integer) then
  begin
    Move(Bookmark[0], BmVal, SizeOf(Integer));
    PRecInfo(PByte(Buffer) + FRecInfoOfs).Bookmark := BmVal;
  end;
end;
}

function TAstaIOCustomDataset.GetBookmarkFlag(Buffer: TRecordBuffer): TBookmarkFlag;
begin
  Result := PRecInfo(PAnsiChar(Buffer) + FRecInfoOfs).BookmarkFlag;
end;

procedure TAstaIOCustomDataset.SetBookmarkFlag(Buffer: TRecordBuffer; Value: TBookmarkFlag);
begin
  PRecInfo(PAnsiChar(Buffer) + FRecInfoOfs).BookmarkFlag := Value;
end;

procedure TAstaIOCustomDataset.InternalSetToRecord(Buffer: TRecordBuffer);
begin
  InternalGotoBookmark(@PRecInfo(PAnsiChar(Buffer) + FRecInfoOfs).Bookmark);
end;

procedure TAstaIOCustomDataset.InternalGotoBookmark(Bookmark: Pointer);
begin
  FIndexes.Bookmark := PLongInt(Bookmark)^;
  if FIndexes.RecsCount = 0 then exit;
  if FIndexes.Bookmark <> PLongInt(Bookmark)^ then
    DatabaseError('Bookmark not found');
end;

{
procedure TAstaIOCustomDataset.InternalGotoBookmark(Bookmark: TBookmark);
var
  BmVal: Integer;
begin
  if Length(Bookmark) = SizeOf(Integer) then
  begin
    Move(Bookmark[0], BmVal, SizeOf(Integer));
    InternalGotoBookmark(@BmVal);
  end
  else
    DatabaseError('Invalid bookmark size');
end;
}

function TAstaIOCustomDataset.GetFieldData(Field: TField; Buffer: Pointer): Boolean;
var
  FieldAddrInbuffer : Pointer;
  SourceBuffer :PAnsiChar;
  Pc :PAnsiChar;
begin
  Result := False;
  try
    if ((not Filtered) and (Recordcount = 0)) and (Field.Calculated) then exit;

    if Field.FieldKind in [fkCalculated, fkLookup] then begin
      SourceBuffer := GetActiveRecordBuffer;
      if Assigned(SourceBuffer) then begin
       SourceBuffer := PAnsiChar(PAnsiChar(SourceBuffer) + FStartCalculated + Field.Offset);
        if Buffer = nil then begin// IsNull calls GetFieldData with Buffer as nil
          Result := PBoolean(SourceBuffer)^;
          Exit;
        end;
        if not PBoolean(SourceBuffer)^ then
          Exit
        else begin
          Pc := PAnsiChar(PAnsiChar(SourceBuffer) + 1);
          Move(Pc^, Buffer^, Field.DataSize);
        end;
        Result := True;
        Exit;
      end;
    end;

    if (Field.FieldKind = fkData) and (State = dsOldValue) then begin
      Result := GetOldFieldData(Field, Buffer);
      Exit;
    end;
    if buffer = nil then begin
      SourceBuffer := GetActiveRecordBuffer;
      if not FIsOpen or (SourceBuffer = nil) then
        Exit;
      if (Field.FieldKind in [fkCalculated, fkLookup]) then
        Result := True
      else
        Result := GetChangedFlag(Field, SourceBuffer);
      Exit;
    end;

    SourceBuffer := GetActiveRecordBuffer;
    if not FIsOpen or (SourceBuffer = nil) then
      Exit;
    if (Field.FieldKind in [fkCalculated, fkLookup]) then begin
      FieldAddrInBuffer := OffsetPointer(SourceBuffer, FStartCalculated + Field.Offset);
      system.Move(FieldAddrInbuffer^, Buffer^, Field.DataSize);
      Result := GetChangedFlag(Field, SourceBuffer);
      Exit;
    end;
    FieldAddrInBuffer := OffsetPointer(SourceBuffer, PIntegerArray(FFldOffs)^[Field.FieldNo - 1]);
    case Field.DataType of
      ftString,
        ftFixedChar,
        ftguid      :
        System.AnsiStrings.StrLCopy(PAnsiChar(Buffer), PAnsiChar(FieldAddrInBuffer), FieldSizeFromField(Field));
      ftwidestring,
        ftvarbytes,
        ftbytes     : system.move(FieldAddrInbuffer^, Buffer^, FieldSizeFromField(Field));
      ftlargeInt    : system.move(FieldAddrInbuffer^, Buffer^, Sizeof(Int64));
      ftbcd         : system.move(FieldAddrInbuffer^, Buffer^, Sizeof(Currency));
      ftcurrency,
      ftFloat       : system.move(FieldAddrInbuffer^, Buffer^, Sizeof(Double));
      ftDate,
      ftDateTime    : system.move(FieldAddrInbuffer^, Buffer^, Sizeof(TDateTime));
      ftsmallint    : system.move(FieldAddrInbuffer^, Buffer^, Sizeof(SmallInt));
      ftboolean     : system.move(FieldAddrInbuffer^, Buffer^, Sizeof(WordBool));
      ftautoinc,
      ftInteger,
      ftTime        : system.move(FieldAddrInbuffer^, Buffer^, Sizeof(Integer));
      ftword        : system.move(FieldAddrInbuffer^, Buffer^, Sizeof(Word));
      ftDataSet,
      ftOraBlob,
      ftOraClob,
      ftmemo,
      ftblob,
      fttypedbinary,
      ftfmtmemo,
      ftgraphic     : system.move(FieldAddrInbuffer^, Buffer^, Sizeof(Integer));
{$ifdef Delphi6AndUP}
      fttimeStamp   :system.move(FieldAddrInbuffer^, Buffer^, Sizeof(TSQLTimeStamp));
      ftFmtBcd      :system.move(FieldAddrInbuffer^, Buffer^, Sizeof(TBcd));
{$endif}
    else
      Exit;
    end;
  except
    raise EDataBaseError.Create(Format(SFieldOffSet, [Field.FieldName]));
  end;
  Result := GetChangedFlag(Field, SourceBuffer);
end;

{$IF CompilerVersion >= 33.0}
function TAstaIOCustomDataset.GetFieldData(Field: TField; var Buffer: TValueBuffer): Boolean;
var
  Ptr: Pointer;
  ReqSize: Integer;
begin
  if not FIsOpen then
  begin
    Result := False;
    Exit;
  end;

  if Field.FieldKind in [fkCalculated, fkLookup] then
    ReqSize := Field.DataSize
  else
    ReqSize := FieldSizeFromField(Field);

  if Length(Buffer) < ReqSize then
    SetLength(Buffer, ReqSize);

  if Length(Buffer) > 0 then
    Ptr := @Buffer[0]
  else
    Ptr := nil;
  Result := GetFieldData(Field, Ptr);
end;

procedure TAstaIOCustomDataset.SetFieldData(Field: TField; Buffer: TValueBuffer);
var
  Ptr: Pointer;
begin
  if Length(Buffer) > 0 then
    Ptr := @Buffer[0]
  else
    Ptr := nil;
  SetFieldData(Field, Ptr);
end;
{$IFEND}

function TAstaIOCustomDataset.GetOldFieldData(Field: TField; Buffer: Pointer): Boolean;
begin
  Result := False;
end;

procedure TAstaIOCustomDataset.SetFieldData(Field: TField; Buffer: Pointer);
var
  LocalValidate:TFieldNotifyEvent;
  FieldAddrInbuffer :Pointer;
  DestinationBuffer :PAnsiChar;
  Pc :PAnsiChar;
  IsNull: Boolean;
begin
  if (buffer = nil) and (Field = nil) then
    exit;
  if not FDirectWrite and not (state in dswritemodes) then
    DatabaseError(SNotInEditMode);
  if (State = dsSetKey) and not Field.IsIndexField then
    DatabaseErrorFmt(SNotIndexField, [Field.DisplayName]);
  if not FDirectWrite and (State in [dsEdit, dsInsert]) and (Field.FieldKind = fkdata) then begin
    LocalValidate := Field.OnValidate;
    FCurValidate := Field.OnValidate;
    Field.OnValidate := DoValidate;
    try
      Field.Validate(Buffer);
    finally
      Field.OnValidate := LocalValidate;
    end;
  end;

  DestinationBuffer := GetActiveRecordBuffer;
  if Destinationbuffer = nil then
    exit;

  if Field.FieldKind in [fkCalculated, fkLookup] then
  begin
    DestinationBuffer := PAnsiChar(PAnsiChar(DestinationBuffer) + FStartCalculated + Field.Offset);
    PBoolean(DestinationBuffer)^ := (Buffer <> nil);
    if PBoolean(DestinationBuffer)^ then begin
      Pc := PAnsiChar(PAnsiChar(DestinationBuffer) + 1);
      Move(Buffer^, Pc^, Field.DataSize);
    end;
    if not FDirectWrite and not (State in [dsCalcFields, dsFilter, dsNewValue]) then
      DataEvent(deFieldChange, Longint(Field)); // Causes InternalPost
    Exit;
  end;

  if (Field.FieldKind in [fkCalculated, fkLookup]) then begin
    FieldAddrInBuffer := OffsetPointer(DestinationBuffer, FStartCalculated + Field.offset);
    if buffer <> nil then
      system.move(buffer^, fieldAddrInBuffer^, Field.DataSize);
    // DataEvent(deFieldChange, Longint(Field)); // sm - 9/12/2000 removed 06/29/01 to match Asta 2
    if field <> nil then
      SetChangedFlag(Field, Buffer<>nil,DestinationBuffer);
    Exit;
  end;
  FieldAddrInBuffer := OffsetPointer(DestinationBuffer, PIntegerArray(FFldOffs)^[Field.FieldNo - 1]);

  if buffer <> nil then
    system.move(buffer^, fieldAddrInBuffer^, FieldSizeFromField(Field));

  if buffer = nil then
    IsNull := True
  else begin
    IsNull := False;
    if (Field.DataType in [ftString, ftFixedChar]) and
       (System.AnsiStrings.StrLen(PAnsiChar(Buffer)) = 0) then
      IsNull := True
    else if (Field.DataType = ftWideString) and
            (WideStrLen(PWideChar(Buffer), Field.Size) = 0) then
      IsNull := True
  end;
  if field <> nil then
    SetChangedFlag(Field, not IsNull, DestinationBuffer);

  if not FDirectWrite and not (State in [dsCalcFields, dsFilter, dsNewValue]) then
    DataEvent(deFieldChange, Longint(Field));
end;

procedure TAstaIOCustomDataset.SetFieldDataNoDataEvent(Field: TField; Buffer: Pointer);
var
  FieldAddrInbuffer, DestinationBuffer: Pointer;
begin
  if buffer = nil then exit;
  DestinationBuffer := GetActiveRecordBuffer;
  FieldAddrInBuffer := OffsetPointer(DestinationBuffer, PIntegerArray(FFldOffs)^[Field.FieldNo - 1]);
  system.move(buffer^, fieldAddrInBuffer^, FieldSizeFromField(Field));
  SetChangedFlag(Field, True, DestinationBuffer);
end;

function WideStrLen(S: PWideChar; MaxLen: Integer): Integer;
var
  Count: Integer;
begin
  Result := 0;
  if S = nil then
    Exit;
  Count := 0;
  while (Count < MaxLen) and (S^ <> #0) do
  begin
    Inc(Result);
    Inc(Count);
    Inc(S);
  end;
end;

procedure TAstaIOCustomDataset.DataConvert(Field: TField; Source,
  Dest: Pointer; ToNative: Boolean);
var
  TimeStamp: TTimeStamp;
begin
  if Field.DataType = ftBCD then
    PCurrency(Dest)^ := PCurrency(Source)^
  else if Field.DataType = ftWideString then begin
     {$ifdef WideStrChange}
     if ToNative then
          WideStrLCopy(PWideString(Source)^, PWideChar(Dest), Field.Size)
        else
          WideStrLSet(PWideString(Dest)^, PWideChar(Source), Field.Size)
    {$else}
    if ToNative then begin
      PWord(Dest)^ := Length(PWideString(Source)^);
      Move(PWideString(Source)^[1], (PChar(Dest) + sizeof(Word))^, Length(PWideString(Source)^) * sizeof(WideChar));
    end
    else
      SetString(PWideString(Dest)^, PWideChar(PChar(Source) + sizeof(Word)), PWord(Source)^);
   {$endif}
  end
  else if (Field.DataType = ftDateTime) and not ToNative and (PDouble(Source)^ = 0) then begin
    TimeStamp.Time := 0;
    TimeStamp.Date := DateDelta;
    PDateTime(Dest)^ := TimeStampToDateTime(TimeStamp);
  end
  else
    inherited;
end;

procedure TAstaIOCustomDataset.InternalFirst;
begin
  FirstUsingRanges;
end;

procedure TAstaIOCustomDataset.InternalLast;
begin
  LastUsingRanges;
end;

procedure TAstaIOCustomDataset.AddField(FieldName: string; FType: TFieldType; FSize: Integer; FRequired: Boolean = False);
begin
  with FieldDefs do
    with AddFieldDef do
    begin
      Name := FieldName;
      DataType := FType;
      Size := FSize;
      Required := FRequired;
    end;
end;


function TAstaIOCustomDataset.FieldSizeFromField(Field: Tfield): Integer;
begin
  result := FastaList.FFieldList.items[Field.Fieldno - 1].FFieldSize;
end;


procedure TAstaIOCustomDataset.DoOnNewRecord;
var
  prevAutoCalc: Boolean;
begin
  prevAutoCalc := AutoCalcFields;
  AutoCalcFields := False;
  try
    EvaluteDefaults([fkData]);
  finally
    AutoCalcFields := prevAutoCalc;
  end;
  GetCalcFields(ActiveBuffer);
//xx  if MasterFields <> '' then GetDataFromMaster;
//xx  if (MasterSource <> nil) and (MasterFields <> '') then GetDataFromMaster;
  inherited DoOnNewRecord;
end;

function TAstaIOCustomDataset.CheckMasterFilter: Boolean;
begin
  result := (FDataLink.DataSet <> nil) and (MasterFields <> '')
    and (FDetailFields <> '');
end;

function TAstaIOCustomDataset.MasterDetailFilter: Boolean;
var
  i: integer;
  tray: string;
begin
  result := True;
  if MasterFields = '' then
    Exit;
  i := 0;
  tray := string(tokencount(AnsiString(MasterFields + ';'), i, ';'));
  while tray <> '' do begin
    Result := AnsiCompareText(FDataLink.DataSet.FieldByName(Tray).AsString,
      FieldbyName(string(tokencount(AnsiString(FDetailFields + ';'), i, ';'))).AsString) = 0;
    if not Result then
      Exit;
    inc(i);
    tray := string(tokencount(AnsiString(MasterFields + ';'), i, ';'))
  end;
end;

procedure TAstaIOCustomDataset.MasterChildFilter(DataSet: TDataSet; var Accept: Boolean);
begin
  Accept := MasterDetailFilter;
end;

function TAstaIOCustomDataset.CheckRecordStatusFilter: Boolean;
begin
  Result := False;
end;

procedure TAstaIOCustomDataset.RecordStatusFilter(DataSet: TDataSet; var Accept: Boolean);
begin
  Accept := True;
end;
function TAstaIOCustomDataset.FilterRecord(Buffer: TRecordBuffer): Boolean;
var
  SaveState: TDatasetState;
begin
  Result := True;
  if not CheckMasterFilter and
     not CheckRecordStatusFilter and
     not (Filtered or FInFindRecord) then
    Exit;
  SaveState := SetTempState(dsFilter);
  FFilterBuffer := PAnsiChar(Buffer);
  try
    if CheckMasterFilter then
      MasterChildFilter(Self, Result);
    if CheckRecordStatusFilter then
      RecordStatusFilter(Self, Result);
    if Filtered or FInFindRecord then begin
      if Result and (FFilterTreeReader <> nil) then
        try
          Result := FFilterTreeReader.Evaluate;
        except
          UnprepareFilter;
          Filtered := False;
          raise;
        end;
      if Result and Assigned(OnFilterRecord) then
        OnFilterRecord(Self, Result);
    end;
  finally
    RestoreState(SaveState);
    FFilterBuffer := nil;
  end;
end;

procedure TAstaIOCustomDataset.InternalHandleException;
begin
  {$IFDEF Delphi6AndUP}
  if Assigned(Classes.ApplicationHandleException) then
    Classes.ApplicationHandleException(Self);
  {$ELSE}
//  Application.HandleException(Self);
  {$ENDIF}
end;

procedure TAstaIOCustomDataset.DirectListTransfer(DS: TAstaIOCustomDataset);
begin
  FDisposeAstaList := False;
  ResetCursorRange;
  DS.FAstaList.Free;
  DS.FIndexes.DBListDestroyed;
  DS.FAstaList := FAstaList;
  DS.FIndexes.DBListCreated;
  DS.FAggregates.DBListCreated;
  DS.FieldDefs.Assign(FieldDefs);
  ds.Active := True;
  DS.Indexes.Rebuild;
  DS.Aggregates.Recalc;
end;

function TAstaIOCustomDataset.GetRecNo: integer;
begin
  updateCursorPos;
  checkActive;
  Result := FIndexes.Position + 1;
end;

procedure TAstaIOCustomDataset.ClearCalcFields(Buffer: TRecordBuffer);
begin
  fillchar(buffer[FStartCalculated], calcfieldssize, #0);
end;

function TAstaIOCustomDataset.GetActiveRecordBuffer: PAnsiChar;
begin
  if FDirectRead or FDirectWrite then
    Result := PAnsiChar(TempBuffer)
  else
    case State of
      dsBrowse, dsNewValue, dsCurValue:
        if IsEmpty then
          Result := nil
        else
          Result := PAnsiChar(ActiveBuffer);
      dsCalcFields:
        Result := PAnsiChar(CalcBuffer);
      dsFilter:
        Result := PAnsiChar(FFilterBuffer);
      dsEdit,
      dsInsert:
        Result := PAnsiChar(ActiveBuffer);
      dsSetKey:
        Result := PAnsiChar(PAnsiChar(FKeyBuffer) + SizeOf(TAstaIOKeyBuffer));
      else
        Result := nil;
    end;
end;

procedure TAstaIOCustomDataset.SetRecNo(Value: Integer);
begin
  CheckBrowseMode;
  if Value <> RecNo then begin
    DoBeforeScroll;
    FIndexes.Position := Value - 1;
    Resync([rmCenter]);
    DoAfterScroll;
  end;
end;

{ TAstaBlobStream }

type
  TAstaBlobStream = class(TMemoryStream)
  private
    FField: TBlobField;
    FDataSet: TAstaIOCustomDataset;
    FFieldNo: Integer;
    FModified: Boolean;
    procedure ReadBlobData;
  public
    constructor Create(Field: TBlobField; Mode: TBlobStreamMode);
    destructor Destroy; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    procedure Truncate;
  end;

{ TAstaBlobStream }

function StreamToString(TM: TMemoryStream): AnsiString;
var
  p: Pointer;
begin
  if TM = nil then begin
    Result := '';
    Exit;
  end;
  SetLength(result, TM.Size);
  TM.Position := 0;
  p := PAnsiChar(result);
  TM.ReadBuffer(p^, TM.Size);
end;

constructor TAstaBlobStream.Create(Field: TBlobField; Mode: TBlobStreamMode);
begin
  inherited Create;
  FField := Field;
  FFieldNo := FField.FieldNo;
  FModified := False;
  FDataSet := FField.DataSet as TAstaIOCustomDataset;
  if Mode = bmWrite then
    Truncate
  else
    ReadBlobData;
end;

destructor TAstaBlobStream.Destroy;
begin
  if FModified and (FDataset.FDirectWrite or (FDataset.State in [dsEdit, dsInsert])) then
  try
   { if FField.Transliterate then FDataSet.Translate(Memory, Memory, True);}
    FDataSet.StoreMemoryStream(FField, Self);
    FField.Modified := True;
    FModified := False;
    FDataSet.DataEvent(deFieldChange, Longint(FField));
  except
    // changed by AI, 24 Nov 2001
    {$IFDEF Delphi6AndUP}
    if Assigned(Classes.ApplicationHandleException) then
      ApplicationHandleException(Self);
    {$ELSE}
//    Application.HandleException(Self);
    {$ENDIF}
  end;
  inherited destroy;
end;

procedure TAstaBlobStream.ReadBlobData;
var
  M: TMemoryStream;
begin
  M := FDataSet.GetMemoryStream(FField);
  if M = nil then exit;
  if M.Size > 0 then
  begin
    Position := 0;
    m.savetostream(self);
    FModified := False;
     {if FField.Transliterate then FDataSet.Translate(Memory, Memory, False);}
    Position := 0;
  end;
end;

function TAstaBlobStream.Write(const Buffer; Count: Longint): Longint;
begin
  Result := inherited Write(Buffer, Count);
  FModified := True;
end;

procedure TAstaBlobStream.Truncate;
begin
  Clear;
  FModified := True;
end;

function TAstaIOCustomDataset.CreateBlobStream(Field: TField; Mode: TBlobStreamMode): TStream;
begin
  Result := TAstaBlobStream.Create(Field as TBlobField, Mode);
end;

procedure TAstaIOCustomDataset.EmptyDataSet;
begin
  if FAstaList <> nil then
    FAstaList.Clear;
  if FBlobList <> nil then
    FBlobList.Clear;
  FIndexes.Rebuild;
  FAggregates.Recalc;
  if FAstaList <> nil then
    FLastBookmark := FAstaList.Count
  else
    FLastBookmark := 0;
  ClearBuffers;
  Refresh;
end;

procedure TAstaIOCustomDataset.Empty;
{
var HoldDoEvents :Boolean;
begin
  if not Active then
    Exit;
  HoldDoEvents := FDoEvents;
  FDoEvents := False;
  // AP???
  BeginBatch;
  try
    Last;
    while not IsEmpty do
      Delete;
  finally
    FDoEvents:=HoldDoEvents;
    EndBatch;
    FLastBookMark := FAstaList.GetLastBookMark;
  end;
}
begin
  if not Active then
    Exit;
  CheckBrowseMode;
  DoBeforeScroll;
  if FAstaList <> nil then
    FAstaList.Clear;
  if FBlobList <> nil then
    FBlobList.Clear;
  if FBlobRollBackList <> nil then
    FBlobRollBackList.Clear;
  RemoveCloneLinks;
  if FIndexes <> nil then
    FIndexes.Rebuild;
  if FAggregates <> nil then
    FAggregates.Recalc;
{$IFDEF NestedDataSet}
  if FReqActList <> nil then
    FReqActList.Clear;
{$ENDIF}
  if FAstaList <> nil then
    FLastBookmark := FAstaList.Count
  else
    FLastBookmark := 0;
  UpdateBufferCount;
  Resync([]);
  DoAfterScroll;
end;

{function TAstaIOCustomDataset.AdjustForBCD(d: TFieldtype): TFieldType;
begin
  result := d;
  if d = ftBCD then result := ftFloat;
end;}

procedure TAstaIOCustomDataset.RetrieveFieldsFromDataSet(D: TDataSet; ClearFields: Boolean; DoRequired :Boolean = True);
var
  i: Integer;
  AllTheSame: Boolean;
begin
  AllTheSame := (d.FieldCount > 0) and (Fieldcount = d.fieldcount);
  if allTheSame then begin
    for i := 0 to FieldDefs.Count - 1 do begin
      AllTheSame := (fields[i].FieldName = d.fields[i].FieldName) and
        (fields[i].DataType = D.Fields[i].DataType) or
        (fields[i].Size = d.fields[i].Size);
      if not AllTheSame then
        break;
    end;
    if AlltheSame then
      Exit;
  end;
  if ClearFields then begin
    if AllTheSame then
      FieldDefs.Clear
    else
      NukeAllFieldInfo;
  end;
  // FieldDefs.Assign(D.FieldDefs);
  // We only need to do the Fields, not FieldDefs. When we create sql from Oldvalues, and we have persistent fields.
  // sm - 12/12/2000
  // sm 9/8/2001 - Added DoRequired
  // AP ???
  for i := 0 to D.Fields.Count - 1 do
    if D.Fields[i].FieldKind = fkData then
      TFieldDef.Create(FieldDefs, D.Fields[i].FieldName, D.Fields[i].DataType,
        D.Fields[i].Size, (DoRequired and D.Fields[i].Required), i);
end;

procedure TAstaIOCustomDataset.RemoveInvalidPersistentFields;
var
  i, j: Integer;
  FieldOK: Boolean;
  F: TField;
begin
  if FieldDefs.Count > 0 then begin
    for i := Fields.Count - 1 downto 0 do begin
      FieldOK := FALSE;
      if Fields[i].FieldKind in [fkCalculated, fkLookup] then
        FieldOK := TRUE
      else
        for j := 0 to FieldDefs.Count - 1 do begin
          if AnsiUpperCase(Fields[i].FieldName) = AnsiUpperCase(FieldDefs.Items[j].Name) then begin
            if Fields[i].DataType = FieldDefs.Items[j].DataType then
              FieldOK := TRUE;
            break;
          end;
        end;
      if not FieldOK then begin
        Close;
        raise EDatabaseError.Create(Name + ':Field ''' + Fields[i].FieldName +
          ''' not found');
      end;
    end;
  end
  else begin
    for i := Fields.Count - 1 downto 0 do begin
      F := Fields[i];
      Fields.Remove(F);
      F.Free;
    end;
  end;
end;

procedure TAstaIOCustomDataset.InternalCleanCloneFromDataSet(D: TDataSet);
var
  i, j: integer;
  F: TField;
  DestroyField: boolean;
begin
  if Active then
    Active := False;
  if Fields.Count > 0 then
    for i := Fields.Count - 1 downto 0 do begin
      F := Fields[i];
      DestroyField := TRUE;
      if Fields[i].FieldKind in [fkCalculated, fkLookup] {fklookup added 11-24-99} then
        DestroyField := FALSE
      else if D.FieldDefs.Count > 0 then
          for j := 0 to D.FieldDefs.Count - 1 do begin
            if (AnsiUpperCase(Fields[i].FieldName) = AnsiUpperCase(D.FieldDefs.Items[j].Name))
              and (Fields[i].DataType = D.FieldDefs.Items[j].DataType) then begin
              DestroyField := FALSE;
              break;
            end;
          end;
      if DestroyField then begin
        Fields.Remove(F);
        F.DataSet := nil;
        F.Free;
        if Self.designer <> nil then
          Self.Designer.DataEvent(deFieldListChange, 0);
      end;
    end;
  Indexes.Clear;
  FieldDefs.Clear;
  FDisposeAstaList := True;
  DisposeAstaList;

  FieldDefs.Assign(D.FieldDefs);
  AstaFieldCreate(True);

  if D is TAstaIOCustomDataset then begin
    Indexes.Assign(TAstaIOCustomDataset(D).Indexes);
    Aggregates.Assign(TAstaIOCustomDataset(D).Aggregates);
    Constraints.Assign(TAstaIOCustomDataset(D).Constraints);
  end;
end;

procedure TAstaIOCustomDataset.CloneSingleRecordFromSource(SourceDataSet :TDataSet; DoBlobs :Boolean = True);
var
  i: Integer;
begin
  FSuspendEvents:=True;
  try
    Edit;
    for i:=0 to SourceDataSet.FieldCount - 1 do
      if (FindField(SourceDataSet.Fields[i].FieldName) <> nil)
        and (SourceDataSet.Fields[i].FieldKind = fkData) //then
        and not (SourceDataSet.Fields[i].DataType in [ftFmtMemo, ftMemo, ftGraphic, ftBlob, ftVarBytes, ftBytes]) then
        FieldByName(SourceDataSet.Fields[i].Fieldname).Assign(SourceDataSet.Fields[i]);

    if DoBlobs then
    begin
      for i:=0 to SourceDataSet.FieldCount - 1 do
        if (FindField(SourceDataSet.Fields[i].FieldName) <> nil)
          and (SourceDataSet.Fields[i].FieldKind = fkData)
          and (SourceDataSet.Fields[i].DataType in [ftFmtMemo, ftMemo, ftGraphic, ftBlob, ftVarBytes, ftBytes]) then
          FieldByName(SourceDataSet.Fields[i].Fieldname).Assign(SourceDataSet.Fields[i]);
    end;
    Post;
  finally
    FSuspendEvents:=False;
  end;
end;
procedure TAstaIOCustomDataset.CleanCloneFromDataSet(D: TDataSet;
  AddData: Boolean = True; CopyBookMarks: Boolean = False; FieldsToSkip: TStrings = nil);
var
  i: Integer;
begin
  NukeAllFieldInfo;
  FieldDefs.Assign(D.FieldDefs);
  i := 0;
  while i < FieldDefs.Count do
    if (FieldsToSkip <> nil) and (FieldsToSkip.IndexOf(FieldDefs[i].Name) <> -1) then
      FieldDefs.Delete(i)
    else begin
     {$ifndef WideStrChange}
      if (FieldDefs[i].DataType = ftfixedchar) or (FieldDefs[i].DataType = ftwidestring) then
        FieldDefs[i].DataType := ftString;
     {$endif}   
      Inc(i);
    end;

  //for i:=0 to FieldDefs.Count -1 do
  //begin
  //  if FieldDefs[i].DataType in [ftBCD{$ifdef Delphi6AndUp},ftFMTBCD{$endif}] then
  //    FieldDefs[i].DataType:=ftFloat;
  //end;

  AstaFieldCreate(True);
  Open;
  if D is TAstaIOCustomDataset then begin
    Indexes.Assign(TAstaIOCustomDataset(D).Indexes);
    Aggregates.Assign(TAstaIOCustomDataset(D).Aggregates);
    Constraints.Assign(TAstaIOCustomDataset(D).Constraints);
  end;
  if AddData then
    DataTransfer(D, True, CopyBookMarks);
end;

procedure TAstaIOCustomDataset.SmartRetrieveFieldsFromDataSet(D: TDataSet);
var
  i: Integer;
  AllTheSame: Boolean;
begin
  AllTheSame := (d.FieldCount > 0) and (Fieldcount = d.fieldcount);
  if allTheSame then
    for i := 0 to FieldCount - 1 do
    begin
      AllTheSame := (fields[i].FieldName = d.fields[i].FieldName) and
        (fields[i].DataType = D.Fields[i].DataType)
        or (fields[i].Size = d.fields[i].Size);
      if not AllTheSame then break;
    end;
  if AllTheSame then exit;
  NukeAllFieldInfo;
  FieldDefs.Assign(D.FieldDefs);
  AstaFieldCreate(true);
end;

function IsABlobField(d: TdataSet; FieldName: string): Boolean;
begin
  case d.fieldbyname(Fieldname).DataType of
    //was ftautoinc duh! 01/13/98
    ftblob..ftcursor: result := true;
    ftbytes, ftvarbytes: result := true;
    ftAdt..ftdataset: result := True;
    ftLargeInt: result := False;
  else
    result := False;
  end;
end;

// changed by EM, 22 May 2001
procedure TAstaIOCustomDataset.DataTransfer(D: TDataSet; IncludeBlobs: Boolean; IncludeBookmarks: Boolean = False);
var
  i, N: Integer;
  bm: TBookmark;
  Fld: TField;
begin
  if (d.eof and d.bof) then exit;
  try
    BeginBatch;
    //bm := d.BookMark;
    D.disableControls;
    D.First;
    N := D.FieldCount - 1;
    while not d.eof do
    begin
      BeginDirectAppend;
      try
        for I := 0 to N do
        begin
          Fld := D.fields[i];
          if (findfield(Fld.FieldName) <> nil) and
             (IncludeBlobs or not isAblobfield(self, Fld.fieldname))
             then
            DBFieldsAssign(fieldbyname(Fld.Fieldname), Fld);
        end;
      finally
        EndDirectWrite;
      end;
      if IncludeBookmarks then
        //FAstaList.Items[FAstaList.Count-1].FBookMark := PLongInt(D.BookMark)^;
      D.Next;
    end;
  finally
    First;
    EndBatch;
    //D.Bookmark := bm;
    D.EnableControls;
  end;
end;

procedure TAstaIOCustomDataset.CloneFieldsFromDataSet(D: TDataSet; AddData, IncludeBlobs: Boolean);
begin
  RetrieveFieldsFromDataSet(D, True);
  if not adddata then
  begin
    open;
    exit;
  end;
  DataTransfer(D, IncludeBlobs);
end;

procedure TAstaIOCustomDataset.SaveToStream(Stream: TStream);
//This Expects the DataSet to exist on a form and to
//save just the Data. The fields are saved in the dfm file
begin
  if FAstaList <> nil then begin
    FAstaList.SaveToStream(Stream);
    if FBlobList = nil then
      FBlobList := TAstaBlobList.Create;
    FBlobList.SaveToStream(Stream);
    FIndexes.SaveToStream(Stream);
    FAggregates.SaveToStream(Stream);
  end;
end;

function TAstaIOCustomDataset.SaveToString: AnsiString;
// Save just the data, not the fielddefs to a string
var
  TempStream: TMemoryStream;
begin
  TempStream := TMemoryStream.Create;
  try
    SaveToStream(TempStream);
    Result := StreamToString(TempStream);
  finally
    TempStream.Free;
  end;
end;

procedure TAstaIOCustomDataset.LoadFromString(S: AnsiString);
// This loads JUST DATA, the field defs are not included in this
// string.  The DataSet should already exist with matching fielddefs
var
  TempStream: TMemoryStream;
begin
  TempStream := TMemoryStream.Create;
  try
    StringToStream(S, TempStream);
    S := ''; //free memory
    LoadFromStream(TempStream);
  finally
    TempStream.Free;
  end;
end;

procedure TAstaIOCustomDataset.LoadFromStringWithFields(S: AnsiString);
var
  TempStream: TMemoryStream;
begin
  TempStream := TMemoryStream.Create;
  try
    StringToStream(S, TempStream);
    S := ''; //free memory
    LoadFromStreamwithFields(TempStream);
  finally
    TempStream.Free;
  end;
end;

procedure TAstaIOCustomDataset.LoadFromStreamwithFields(Stream: TStream);
begin
  DisableControls;
  try
    Close;
    InternalLoadFromStreamWithFields(Stream);
    Open;
  finally
    EnableControls;
  end;
end;

procedure TAstaIOCustomDataset.InternalLoadFromStreamWithFields(Stream: TStream);
begin
  ResetCursorRange;
  FieldDefs.Clear;
  FAstaList.Free;
  FIndexes.DBListDestroyed;
  FAstaList := TAstaDBlist.Create;
  FIndexes.DBListCreated;
  FAggregates.DBListCreated;
  { sm - 10/09/2000. Removed for TFieldDefs. Add fields in AstaFieldCreate}
  {for i := 0 to FAstaList.FFieldList.Count - 1 do
  begin
    with FAstaList.FFieldList.items[i] do
      FieldDefs.Add(FFieldName, FfieldType, FFieldSize);
  end;}
  LoadFromStream(Stream);
  //AstaFieldCreate(False);
end;

procedure TAstaIOCustomDataset.LoadFromStream(Stream: TStream);
begin
  //expects the same fields to be defined!!!
  if FAstaList <> nil then
  begin
    if assigned(DataSetField) then Empty;//06/01/2003 
    FLastBookMark := 0;
    FAstaList.clear;
    FAstaList.LoadFromStream(Stream);
    if FBlobList = nil then
      FBlobList := TAstaBlobList.Create
    else
      FBlobList.Clear;
    FBlobList.LoadFromSTream(Stream);
    FIndexes.LoadFromStream(Stream);
    FIndexes.Position:=0;//06/02/2003
    //FAggregates.Clear;
    FAggregates.LoadFromStream(Stream);
    FLastBookMark := GetUniqueBookMark;
    if Active then
      Refresh;
  end;
end;

procedure TAstaIOCustomDataset.CloneFieldsFromDataSetPreserveFields(D: TDataSet; AddData, IncludeBlobs: Boolean);
begin
  SmartRetrieveFieldsFromDataSet(D);
  if not adddata then
  begin
    open;
    exit;
  end;
  DataTransfer(D, IncludeBlobs);
end;

procedure TAstaIOCustomDataset.NukeAllFieldInfo;
begin
  //d3:if the dataset is closed the Fields don't get destroyed!!!
  //Who would have thought that. It must be open and then the
  //Destroy fields will nuke the fields. Then it must close
  //and be able to open to handle the Default Fields properly
  if not Active then
    Active := True;
  Indexes.Clear;
  Aggregates.Clear;
  Constraints.Clear;
  FDisposeAstaList := True;
  Close;
  if FieldCount > 0 then
    DestroyFields;
  FieldDefs.Clear;
  DisposeAstaList;
end;

procedure TAstaIOCustomDataset.NukeUnMatchedFields(D: TDataSet);
var
  i: Integer;
begin
  close;
  DestroyFields;
  i := 0;
  while i < FieldDefs.Count do
    if d.FindField(FieldDefs[i].Name) <> nil then
      inc(i)
    else
      FieldDefs[i].Free;
  FieldDefs.Clear;
  DisposeAstaList;
end;

procedure TAstaIOCustomDataset.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
end;

procedure TAstaIOCustomDataset.BookMarkRenumber;
var
  i: Integer;
begin
  for i := 1 to FAstaList.Count do
    FAstaList.Items[i - 1].FBookMark := i;
  for i := 1 to FIndexes.Count - 1 do
    if FIndexes[i].ByBookmark then begin
      FIndexes[i].Rebuild;
      if FIndexes[i] = FIndexes.SelectedIndex then
        First;
    end;
end;

function TAstaIOCustomDataset.BookMarkToRawListFetch(FieldName: string; Index: Integer): string;
begin
  InternalGotoBookmark(@Index);
  Result := FIndexes.Rec.FieldNameGetAsString(FieldName);
end;

procedure TAstaIOCustomDataset.DoBeforeCancel;
var
  i: Integer;
  m: TMemoryStream;
  fld: TField;
begin
  inherited DoBeforeCancel;
  if FBlobRollBackList = nil then
    Exit;
  for i := 0 to FBlobRollBackList.count - 1 do begin
    fld := TField(FBlobRollBackList.objects[i]);
    if fld.datatype = ftDataSet then begin
      m := TMemoryStream.Create;
      try
        StringToStream(FBlobRollBackList[i], m);
        StoreMemoryStream(fld, m);
      finally
        m.Free;
      end;
    end
    else
      fld.AsString := FBlobRollBackList[i];
  end;
  FBlobRollBackList.Clear;
end;

procedure TAstaIOCustomDataset.DoBeforeInsert;
begin
  if not FSuspendEvents then inherited DoBeforeInsert;
end;

procedure TAstaIOCustomDataset.DoBeforeEdit;
var
  i: Integer;
  v: string;
begin
  if FSuspendEvents then
    Exit;
  inherited DoBeforeEdit;
  if FBlobRollBackList = nil then
    FBlobRollBackList := TStringList.Create
  else
    FBlobRollBackLIst.Clear;
  for i := 0 to fieldcount - 1 do
    if isBlobField(fields[i]) and not fields[i].readonly then begin
      if fields[i].datatype = ftDataSet then
        v := StreamToString(GetMemoryStream(fields[i]))
      else
        v := fields[i].AsString;
      FBlobRollBackList.AddObject(v, Fields[i]);
    end;
end;

function TAstaIOCustomDataset.FilterCount: Integer;
begin
  Result := 0;
  if RecordCount = 0 then exit;

  if not Filtered and (MasterSource = nil) and
    (FRangeFrom = nil) and (FRangeTo = nil) then begin
    Result := RecordCount;
    Exit;
  end;
  InternalFirst;
  while GetRecord(TempBuffer, gmNext, False) = grOK do
    Inc(Result);
  CursorPosChanged;
  UpdateCursorPos;
end;

function TAstaIOCustomDataset.BookmarkValid(Bookmark: TBookmark): Boolean;
begin
  if BookMark = nil then
    result := False
  else //06.09.2000 the quickpen boys
    Result := FAstaList.GetIndexFromBookMark(PLongInt(Bookmark)^) <> -1;
end;

function TAstaIOCustomDataset.IsBlobField(F: Tfield): Boolean;
begin
  case f.DataType of
    //was ftautoinc duh! 01/13/98
    ftLargeInt: result := False;
    ftblob..ftcursor: result := true;
    ftbytes, ftvarbytes: result := true;
    ftAdt..ftdataset: result := True;
  else
    result := False;
  end;
end;

function DataSetToString(D: TAstaIODataSet): AnsiString;
var
  m: TMemorySTream;
begin
  result := '';
//  m:=nil;
  if d = nil then exit;
  m := TMemoryStream.Create;
  try
    D.SaveToSTream(m);
    result := StreamToString(m);
  finally
    m.free;
  end;
end;

function DataSetToStringwithFieldProperties(D: TAstaIODataSet): AnsiString;
var
  m: TMemorySTream;
  i: Integer;
begin
  result := '';
  if d = nil then exit;
  m := TMemoryStream.Create;
  try
    D.SaveToSTream(m);
    for i := 0 to d.fieldcount - 1 do
      m.writecomponent(d.fields[i]);
    result := StreamToString(m);
  finally
    m.free;
  end;
end;

function StringToDataSet(S: AnsiString): TAstaIODataSet;
var
  m: TMemorySTream;
begin
  result := nil;
  if s = '' then exit;
  result := TAstaIODataSet.Create(nil);
  m := NewStringToStream(s);
  try
   result.LoadFromStreamWithFields(m);
  finally
   m.free;
  end;
end;

function StringToDataSetwithFieldProperties(S: AnsiString): TAstaIODataSet;
var
  m: TMemorySTream;
begin
  result := nil;
  if s = '' then exit;
  result := TAstaIODataSet.Create(nil);
  m := NewStringToStream(s);
  try
   result.LoadFromStreamWithFields(m);
  {$IFDEF FIELDANDSTREAM}
   SetDataSetFieldsFromAFieldStream(result, M, False);
 {$ENDIF}
  finally
    m.free;
 end;
end;

function TAstaIOCustomDataset.FindRecord(Restart, GoForward: Boolean): Boolean;
begin
  CheckBrowseMode;
  DoBeforeScroll;
  SetFound(False);
  UpdateCursorPos;
  CursorPosChanged;
  FInFindRecord := True;
  try
    if GoForward then begin
      if Restart then
        InternalFirst;
      Result := (GetRecord(ActiveBuffer, gmNext, True) = grOK);
    end
    else begin
      if Restart then
        InternalLast;
      Result := (GetRecord(ActiveBuffer, gmPrior, True) = grOK);
    end;
  finally
    FInFindRecord := False;
  end;
  if Result then begin
    Resync([rmExact, rmCenter]);
    SetFound(True);
    Result := True;
    DoAfterScroll;
  end;
end;

procedure TAstaIOCustomDataset.RemoveCloneLinks;
var
  i, spot: Integer;

begin
  if FCloneList = nil then exit;
  for i := 0 to FCloneList.count - 1 do
  begin
    spot := TAstaIOCustomDataset(FCloneList.objects[i]).FCloneList.IndexofObject(self);
    if spot >= 0 then TAstaIOCustomDataset(FCloneList.objects[i]).FCloneList.delete(spot);
  end;
  FCloneList.Clear;
end;

procedure TAstaIOCustomDataset.RegisterClone(Source: TAstaIOCustomDataset);
begin
  if FCloneList = nil then FCloneList := TStringList.Create;
  FCloneList.AddObject(Source.Name, source);
end;

procedure TAstaIOCustomDataset.UnRegisterClone(Source: TAstaIOCustomDataset);
var
  Spot: Integer;
begin
  if FCloneList = nil then exit;
  Spot := FCloneList.IndexofObject(Source);
  if spot >= 0 then FCloneList.Delete(Spot);
end;

procedure TAstaIOCustomDataset.CloneCursor(Source: TAstaIOCustomDataset; KeepFilter: Boolean);
var
  i: Integer;
  d: TAstaIOCustomDataset;
begin
//  CleanCloneFromDataSet(Source);
  FEchoEditAppend := False;
  Empty;
  if Source.DefaultFields then
    CleanCloneFromDataSet(Source)
  else
  begin
    Close;
{$IFDEF FIELDANDSTREAM}
    for i := 0 to Source.FieldCount - 1 do
      MimicFieldCreate(source.fields[i], self);
{$ENDIF}
    FieldDefs.Update;
    if Source.FilterCount > 0 then
      DataTransfer(Source, True)
    else
      Open;
  end; //moved per dn 1-12-00
  FEchoEditAppend := True;
  Source.RegisterClone(Self);
  Self.RegisterClone(Source);
  if source.FCloneList <> nil then
  begin
    for i := 0 to source.FCloneList.Count - 1 do
    begin
      if TAstaIOCustomDataset(source.FCloneList.objects[i]).FCloneList.IndexofObject(self) = -1 then
      begin
        d := Source.FCloneList.objects[i] as TAstaIOCustomDataset;
        if (d <> self) and (d <> source) then
           // Maybe you can skip this test, it was in before the above test
        begin
          d.RegisterClone(Self);
          Self.RegisterClone(d);
        end;
      end;
    end;
  end;
  Self.OnCalcFields := Source.OnCalcFields;
  if KeepFilter then
  begin
    Filter := Source.Filter;
    OnFilterRecord := source.OnfilterRecord;
    FilterOptions := source.FilterOptions;
    Filtered := Source.Filtered;
  end;
end;

procedure TAstaIOCustomDataset.CloneEditUpdate;
var
  i, j: Integer;
  bm: TBookmark;
  d: TAstaIOCustomDataset;
  isFiltered, isCheckRanges: Boolean;
begin
  if FCloneList = nil then exit;
  for i := 0 to FCloneList.count - 1 do
  begin
    d := FCloneList.objects[i] as TAstaIOCustomDataset;
    d.FechoEditAppend := False;
    d.disableControls;
    isFiltered := d.Filtered;
    isCheckRanges := d.FDoCheckRanges;
    d.FDoCheckRanges := false;
    d.filtered := false;
    bm := d.bookmark;
    try
      d.bookMark := BookMark;
      if state = dsedit then
        d.edit
      else
      begin
        if eof then
          d.append
        else
          d.insert;
      end;
      for j := 0 to d.fieldcount - 1 do
        DBFieldsAssign(d.fields[j], fields[j]);
      d.post;
      d.bookmark := bm;
    except
    end;
    d.FDoCheckRanges := isCheckRanges;
    d.filtered := isFiltered;
    d.enableControls;
    d.FEchoEditAppend := True;
  end;
end;

procedure TAstaIOCustomDataset.CloneDeleteUpdate;
var
  i: Integer;
  bm: TBookmark;
  isFiltered, isCheckRanges: Boolean;
  d: TAstaIOCustomDataset;
begin
  if FCloneList = nil then exit;
  for i := 0 to FCloneList.count - 1 do
  begin
    d := FCloneList.objects[i] as TAstaIOCustomDataset;
    d.FechoEditAppend := False;
    d.disableControls;
    bm := d.bookmark;
    isFiltered := d.Filtered;
    isCheckRanges := d.FDoCheckRanges;
    d.FDoCheckRanges := False;
    d.Filtered := False;
    try
      d.bookMark := BookMark;
      d.delete;
      if bm <> nil then d.bookmark := bm;
    except
    end;
    d.FDoCheckRanges := isCheckRanges;
    d.Filtered := isFiltered;
    d.enableControls;
    d.FEchoEditAppend := True;
  end;
end;

procedure TAstaIOCustomDataset.DoBeforePost;
begin
  if FSuspendEvents then exit;
  inherited DoBeforePost;
  EvaluteChecks(nil);
  if FEchoEditAppend then CloneEditUpdate;
end;

procedure TAstaIOCustomDataset.DoBeforeDelete;
begin
  if FSuspendEvents then exit;
  inherited DoBeforeDelete;
  if FEchoEditAppend then CloneDeleteUpdate;
end;

procedure TAstaIOCustomDataset.DoAfterDelete;
begin
  if FSuspendEvents then exit;
  inherited DoAfterDelete;
end;

procedure TAstaIOCustomDataset.DoAfterCancel;
begin
  inherited DoAfterCancel;
end;

procedure TAstaIOCustomDataset.DoAfterPost;
begin
  if FSuspendEvents then exit;
  inherited DoAfterPost;
end;

procedure TAstaIOCustomDataset.DoAfterEdit;
begin
  if FSuspendEvents then exit;
  inherited DoAfterEdit;
end;

procedure TAstaIOCustomDataset.DoAfterInsert;
begin
  if FSuspendEvents then exit;
  inherited DoAfterInsert;
end;

function CloneDataSetToString(D: TDataSet): AnsiString;
var
  m: TMemorySTream;
  Ads: TAstaIODataSet;
begin
  ads := nil;
  result := '';
  m := nil;
  if d = nil then exit;
  try
    Ads := TAstaIODataSet.Create(nil);
    ads.CleanCloneFromDataSet(d);
    m := TMemoryStream.Create;
    Ads.SaveToStream(m);
    result := StreamToString(m);
  finally
    m.free;
    ads.free;
  end;
end;

function CloneDataSetToString(D: TDataSet;RowsToReturn:Integer): AnsiString;
var
  m: TMemorySTream;
  Ads: TAstaIODataSet;
begin
  ads := nil;
  result := '';
  m := nil;
  if d = nil then exit;
  try
    Ads := TAstaIODataSet.Create(nil);
    ads.CleanCloneFromDataSet(d);
    m := TMemoryStream.Create;
    Ads.SaveToStream(m);
    result := StreamToString(m);
  finally
    m.free;
    ads.free;
  end;
end;

procedure TAstaIOCustomDataSet.AddIndex(const FieldName: String; Descending: Boolean);
begin
  if FIndexes.FindIndex(FieldName) = nil then
    with Indexes.Add do begin
      Name := FieldName;
      Fields := FieldName;
      if Descending then
        Options := [ioDescending]
      else
        Options := [ioCaseInsensitive];
    end;
  IndexName := FieldName;
end;

procedure TAstaIOCustomDataSet.AddIndex(const AIndexName, AIndexFields: String; AOptions: TAstaIOIndexOptions;
  const ADescFields: String = ''; const ACaseInsFields: String = ''; AGroupingLevel: Integer = 0);

  function InFields(const AFields, AField: String): Boolean;
  var
    j: Integer;
  begin
    j := Pos(AField, AFields);
    Result := (j <> 0) and (
        ((j = 1) or (AFields[j - 1] = ';')) and
        ((j = Length(AFields) - Length(AField)) or (AFields[j + Length(AField)] = ';'))
      );
  end;

  function FieldsToMarks(const AAllFields, ASubFields: String): String;
  var
    i: Integer;
    ucAllFields: String;
  begin
    Result := '';
    if ASubFields <> '' then begin
      ucAllFields := UpperCase(AAllFields);
      i := 1;
      while i <= Length(ASubFields) do
        if InFields(ucAllFields, UpperCase(ExtractFieldName(ASubFields, i))) then
          Result := Result + 'X'
        else
          Result := Result + ' ';
    end;
  end;

begin
  with FIndexes.IndexSure(AIndexName) do begin
    Active := False;
    Fields := AIndexFields;
    Options := AOptions;
    Descendings := FieldsToMarks(AIndexFields, ADescFields);
    CaseInsensitives := FieldsToMarks(AIndexFields, ACaseInsFields);
    Active := True;
  end;
end;

procedure TAstaIOCustomDataSet.AddIndexFields(Const TheIndexName: string;
const FieldNames: array of string; const ADescending: array of Boolean);
var
  i: Integer;
  TheFields, s: string;
begin
  if FIndexes.FindIndex(TheIndexName) = nil then begin
    TheFields := '';
    for i := low(FieldNames) to high(FieldNames) do
      TheFields := TheFields + FieldNames[i] + ';';
    SetLength(TheFields, Length(TheFields) - 1);
    for i := Low(ADescending) to High(ADescending) do
      if ADescending[i] then
        s := s + 'X'
      else
        s := s + ' ';
    with Indexes.Add do begin
      Name := TheIndexName;
      Fields := TheFields;
      Descendings := S;
      if ADescending[0] then
        Options := [ioDescending, ioCaseinsensitive]
      else
        Options := [ioCaseinsensitive];
    end;
  end;
  IndexName := TheIndexName;
end;
(*
procedure TAstaIOCustomDataSet.AddIndexFields(const TheIndexName: String; const FieldNames: array of String; const Descending: array of Boolean);
var
  i: Integer;
  TheFields: String;
begin

  if FIndexes.FindIndex(TheIndexName) = nil then begin
    TheFields:='';
    for i := low(FieldNames) to high(FieldNames) do
      TheFields := TheFields + FieldNames[i] + ';';
    SetLength(TheFields, Length(TheFields) - 1);
    with Indexes.Add do begin
      Name := TheIndexName;
      Fields := TheFields;
      if Descending[0] then
        Options := [ioDescending]
      else
        Options := [];
    end;
  end;
  IndexName := TheIndexName;
end;
*)
procedure TAstaIOCustomDataSet.DeleteIndex(const AIndexName: String);
var
  ind: TAstaIOIndex;
begin
  ind := Indexes.FindIndex(AIndexName);
  if ind <> nil then
    ind.Free;
end;

function TAstaIOCustomDataSet.ReadField(key: TAstaDBlistItem; AField: TField): Variant;
begin
  BeginDirectRead(key, AField.FieldKind <> fkData);
  try
    Result := AField.Value;
  finally
    EndDirectRead;
  end;
end;

{$WARNINGS OFF}
function TAstaIOCustomDataset.CompareField(AField: TField; key1, key2: TAstaDBlistItem;
  APartialComp, ACaseInsensitive: Boolean): Integer;
const
  TimeStampPrecision: Double = 1.0 / (24 * 60 * 60 * 1000);
var
  si1, si2: SmallInt;
  b1, b2: WordBool;
  i1, i2: Integer;
  w1, w2: Word;
  li1, li2: LargeInt;
  d1, d2: Double;
  c1, c2: Currency;
  s1, s2: String;
  ws1, ws2: WideString;
{$ifdef Delphi6AndUp}
  bcd1, bcd2: TBcd;
  ts1, ts2: TSQLTimeStamp;
{$endif}
  isNull1, isNull2: Boolean;

  procedure GetFieldOrdVal(AKey: TAstaDBlistItem; var AIsNull: Boolean; var AValue);
  var
    buff: Pointer;
  begin
    if AField.FieldKind <> fkData then begin
      BeginDirectRead(AKey, True);
      try
        AIsNull := AField.IsNull;
        if not AIsNull then
          case AField.DataType of
          ftSmallInt: PSmallInt(@AValue)^ := SmallInt(AField.AsInteger);
          ftWord:     PWord(@AValue)^ := Word(AField.AsInteger);
          ftboolean:  PWordBool(@AValue)^ := AField.AsBoolean;
          ftInteger:  PInt(@AValue)^ := AField.AsInteger;
          ftAutoinc:  PInt(@AValue)^ := AField.AsInteger;
          ftfloat:    PDouble(@AValue)^ := AField.AsFloat;
          ftcurrency: PDouble(@AValue)^ := AField.AsFloat;
          ftbcd:      PCurrency(@AValue)^ := AField.AsCurrency;
          ftdatetime: PDouble(@AValue)^ := AField.AsFloat;
          ftTime:     PDouble(@AValue)^ := AField.AsFloat;
          ftDate:     PDouble(@AValue)^ := AField.AsFloat;
{$ifdef Delphi6AndUp}
          ftFmtBcd:   PBcd(@AValue)^ := (AField as TFMTBCDField).Value;
          ftTimeStamp:PSqlTimeStamp(@AValue)^ := (AField as TSQLTimeStampField).Value;
{$endif}
          ftlargeInt: PIn64(@AValue)^ := (AField as TLargeintField).Value;
          end;
      finally
        EndDirectRead;
      end;
    end
    else begin
      AIsNull := not GetChangedFlag(AField, PAnsiChar(AKey.FData));
      if not AIsNull then begin
        buff := OffsetPointer(PAnsiChar(AKey.FData), PIntegerArray(FFldOffs)^[AField.FieldNo - 1]);
        case AField.DataType of
        ftSmallInt: Move(buff^, AValue, SizeOf(SmallInt));
        ftWord:     Move(buff^, AValue, SizeOf(Word));
        ftboolean:  Move(buff^, AValue, SizeOf(WordBool));
        ftInteger:  Move(buff^, AValue, SizeOf(Integer));
        ftAutoinc:  Move(buff^, AValue, SizeOf(Integer));
        ftfloat:    Move(buff^, AValue, SizeOf(Double));
        ftcurrency: Move(buff^, AValue, SizeOf(Double));
        ftbcd:      DataConvert(AField, buff, @AValue, False);
        ftdatetime: DataConvert(AField, buff, @AValue, False);
        ftTime:     DataConvert(AField, buff, @AValue, False);
        ftDate:     DataConvert(AField, buff, @AValue, False);
{$ifdef Delphi6AndUp}
        ftFmtBcd:   DataConvert(AField, buff, @AValue, False);
        ftTimeStamp:DataConvert(AField, buff, @AValue, False);
{$endif}
        ftlargeInt: Move(buff^, AValue, SizeOf(Int64));
        end;
      end;
    end;
  end;

  procedure GetFieldStringVal(AKey: TAstaDBlistItem; var AIsNull: Boolean; var AValue: String);
  var
    buff: Pointer;
    sz, sz2: Integer;
  begin
    if AField.FieldKind <> fkData then begin
      BeginDirectRead(AKey, True);
      try
        AIsNull := AField.IsNull;
        if not AIsNull then
          AValue := AField.AsString;
      finally
        EndDirectRead;
      end;
    end
    else begin
      AIsNull := not GetChangedFlag(AField, PAnsiChar(AKey.FData));
      if not AIsNull then begin
        buff := OffsetPointer(PAnsiChar(AKey.FData), PIntegerArray(FFldOffs)^[AField.FieldNo - 1]);
        sz := FieldSizeFromField(AField);
        sz2 := System.AnsiStrings.StrLen(PAnsiChar(buff));
        if sz2 < sz then
          sz := sz2;
        SetString(AValue, PAnsiChar(buff), sz);
      end;
    end;
  end;

  procedure GetFieldWideStringVal(AKey: TAstaDBlistItem; var AIsNull: Boolean;
    var AValue: WideString);
  var
    buff: Pointer;
  begin
    if AField.FieldKind <> fkData then begin
      BeginDirectRead(AKey, True);
      try
        AIsNull := AField.IsNull;
        if not AIsNull then
          AValue := (AField as TWideStringField).Value;
      finally
        EndDirectRead;
      end;
    end
    else begin
      AIsNull := not GetChangedFlag(AField, PAnsiChar(AKey.FData));
      if not AIsNull then begin
        buff := OffsetPointer(PAnsiChar(AKey.FData), PIntegerArray(FFldOffs)^[AField.FieldNo - 1]);
        WideStrLSet(AValue, PWideChar(buff), TWideStringField(AField).Size);
      end;
    end;
  end;
{$ifdef ver130}
  function WideCompare(const S1, S2: WideString; ANoCase: Boolean): Integer;
  var
    flag: DWORD;
    a1, a2: AnsiString;
  begin
    SetLastError(0);
    if ANoCase then
      flag := NORM_IGNORECASE
    else
      flag := 0;
    Result := CompareStringW(LOCALE_USER_DEFAULT, flag, PWideChar(S1),
      Length(S1), PWideChar(S2), Length(S2)) - 2;
    case GetLastError of
      0: ;
      ERROR_CALL_NOT_IMPLEMENTED:
        begin
          a1 := s1;
          a2 := s2;
          Result := CompareStringA(LOCALE_USER_DEFAULT, flag, PAnsiChar(a1), Length(a1),
            PAnsiChar(a2), Length(a2)) - 2;
        end;
    else
      Result := -1;
    end;
  end;

  function WideCompareText(const S1, S2: WideString): Integer;
  begin
    Result := WideCompare(S1, S2, True);
  end;

  function WideCompareStr(const S1, S2: WideString): Integer;
  begin
    Result := WideCompare(S1, S2, False);
  end;
{$endif}
begin
  Result := 0;
  with FAstaList do
    case AField.DataType of
    ftSmallInt:
      begin
        GetFieldOrdVal(key1, isNull1, si1);
        GetFieldOrdVal(key2, isNull2, si2);
        if isNull1 and not isNull2 then
          Result := -1
        else if not isNull1 and isNull2 then
          Result := 1
        else if not isNull1 and not isNull2 then
          if si1 < si2 then
            Result := -1
          else if si1 > si2 then
            Result := 1;
      end;
    ftWord:
      begin
        GetFieldOrdVal(key1, isNull1, w1);
        GetFieldOrdVal(key2, isNull2, w2);
        if isNull1 and not isNull2 then
          Result := -1
        else if not isNull1 and isNull2 then
          Result := 1
        else if not isNull1 and not isNull2 then
          if w1 < w2 then
            Result := -1
          else if w1 > w2 then
            Result := 1;
      end;
    ftboolean:
      begin
        GetFieldOrdVal(key1, isNull1, b1);
        GetFieldOrdVal(key2, isNull2, b2);
        if isNull1 and not isNull2 then
          Result := -1
        else if not isNull1 and isNull2 then
          Result := 1
        else if not isNull1 and not isNull2 then
          if b1 < b2 then
            Result := -1
          else if b1 > b2 then
            Result := 1;
      end;
    ftLargeInt:
      begin
        GetFieldOrdVal(key1, isNull1, li1);
        GetFieldOrdVal(key2, isNull2, li2);
        if isNull1 and not isNull2 then
          Result := -1
        else if not isNull1 and isNull2 then
          Result := 1
        else if not isNull1 and not isNull2 then
          if li1 < li2 then
            Result := -1
          else if li1 > li2 then
            Result := 1;
      end;
    ftInteger,
    ftAutoinc:
      begin
        GetFieldOrdVal(key1, isNull1, i1);
        GetFieldOrdVal(key2, isNull2, i2);
        if isNull1 and not isNull2 then
          Result := -1
        else if not isNull1 and isNull2 then
          Result := 1
        else if not isNull1 and not isNull2 then
          if i1 < i2 then
            Result := -1
          else if i1 > i2 then
            Result := 1;
      end;
    ftbcd:
      begin
        GetFieldOrdVal(key1, isNull1, c1);
        GetFieldOrdVal(key2, isNull2, c2);
        if isNull1 and not isNull2 then
          Result := -1
        else if not isNull1 and isNull2 then
          Result := 1
        else if not isNull1 and not isNull2 then
          if c1 < c2 then
            Result := -1
          else if c1 > c2 then
            Result := 1;
      end;
    ftfloat,
    ftcurrency,
    ftdatetime,
    ftTime,
    ftDate:
      begin
        GetFieldOrdVal(key1, isNull1, d1);
        GetFieldOrdVal(key2, isNull2, d2);
        if isNull1 and not isNull2 then
          Result := -1
        else if not isNull1 and isNull2 then
          Result := 1
        else if not isNull1 and not isNull2 then
          if d2 - d1 > 0.0000000000001 then
            Result := -1
          else if d1 - d2 > 0.0000000000001 then
            Result := 1;
      end;
{$ifdef Delphi6AndUp}
    ftFmtBcd:
      begin
        GetFieldOrdVal(key1, isNull1, bcd1);
        GetFieldOrdVal(key2, isNull2, bcd2);
        if isNull1 and not isNull2 then
          Result := -1
        else if not isNull1 and isNull2 then
          Result := 1
        else if not isNull1 and not isNull2 then
          Result := BcdCompare(bcd1, bcd2);
      end;
    ftTimeStamp:
      begin
        GetFieldOrdVal(key1, isNull1, ts1);
        GetFieldOrdVal(key2, isNull2, ts2);
        if isNull1 and not isNull2 then
          Result := -1
        else if not isNull1 and isNull2 then
          Result := 1
        else if not isNull1 and not isNull2 then begin
          d1 := SQLTimeStampToDateTime(ts1);
          d2 := SQLTimeStampToDateTime(ts2);
          if d2 - d1 > TimeStampPrecision then
            Result := -1
          else if d1 - d2 > TimeStampPrecision then
            Result := 1;
        end;
      end;
{$endif}
    ftstring, ftfixedchar, ftguid:
      begin
        GetFieldStringVal(key1, isNull1, s1);
        GetFieldStringVal(key2, isNull2, s2);
        if isNull1 and not isNull2 then
          Result := -1
        else if not isNull1 and isNull2 then
          Result := 1
        else if not isNull1 and not isNull2 then
          if ACaseInsensitive then
            if APartialComp then
              Result := CompareText(s1, Copy(s2, 1, Length(s1)))
            else
              Result := CompareText(s1, s2)
          else
            if APartialComp then
              Result := CompareStr(s1, Copy(s2, 1, Length(s1)))
            else
              Result := CompareStr(s1, s2);
      end;
    {$ifdef Delphi6AndUp}
    ftwidestring:
      begin
        GetFieldWideStringVal(key1, isNull1, ws1);
        GetFieldWideStringVal(key2, isNull2, ws2);
        if isNull1 and not isNull2 then
          Result := -1
        else if not isNull1 and isNull2 then
          Result := 1
        else if not isNull1 and not isNull2 then
          if ACaseInsensitive then
            if APartialComp then
              Result := WideCompareText(ws1, Copy(ws2, 1, Length(ws1)))
            else
              Result := WideCompareText(ws1, ws2)
          else
            if APartialComp then
              Result := WideCompareStr(ws1, Copy(ws2, 1, Length(ws1)))
            else
              Result := WideCompareStr(ws1, ws2);
      end;
    {$endif}
    ftBytes, ftVarBytes:
      begin
        GetFieldStringVal(key1, isNull1, s1);
        GetFieldStringVal(key2, isNull2, s2);
        if isNull1 and not isNull2 then
          Result := -1
        else if not isNull1 and isNull2 then
          Result := 1
        else if not isNull1 and not isNull2 then
          if s1 < s2 then
            Result := -1
          else if s1 > s2 then
            Result := 1;
      end;
    else
      raise Exception.CreateFmt('Comparison is not supported for field ''%s''',
        [AField.FieldName]);
    end;
end;
{$WARNINGS ON}

function TAstaIOCustomDataSet.DirectFilter(ARec: TAstaDBListItem): Boolean;
begin
  if not CheckMasterFilter and not Filtered then
    Result := True
  else begin
    BeginDirectRead(ARec, True);
    try
      Result := FilterRecord(TRecordBuffer(TempBuffer));
    finally
      EndDirectRead;
    end;
  end;
  Result := Result and (CheckRanges(ARec, 0) = 0);
end;

procedure TAstaIOCustomDataSet.BeginDirectRead(ARec: TAstaDBListItem; ANeedCalcFields: Boolean);
begin
  FDirectRead := True;
  if Length(ARec.FData) = FAstaList.FBufferSize then begin
    Move(PAnsiChar(ARec.FData)^, PByte(TempBuffer)^, FAstaList.FBufferSize);
    if (CalcFieldsSize > 0) and ANeedCalcFields then begin
      ClearCalcFields(TempBuffer);
      GetCalcFields(TempBuffer);
    end;
  end
  else if Length(ARec.FData) = FRecBufSize then
    Move(ARec.FData[1], PByte(TempBuffer)^, FRecBufSize)
  else begin
    FDirectRead := False;
    raise Exception.Create('Direct read buffer length mismatch');
  end;
end;

procedure TAstaIOCustomDataSet.EndDirectRead;
begin
  FDirectRead := False;
end;

procedure TAstaIOCustomDataSet.BeginDirectWrite(ARec: TAstaDBListItem);
begin
  FDirectWrite := True;
  FDirectWriteRec := ARec;
  InternalInitRecord(TempBuffer);
  ClearCalcFields(TempBuffer);
end;

procedure TAstaIOCustomDataset.BeginDirectAppend;
begin
  Inc(FLastBookmark);
  BeginDirectWrite(FastaList.AppendRow(FLastBookMark));
end;
procedure TAstaIOCustomDataset.EndDirectWrite;
begin
  FDirectWrite := False;
  if FDirectWriteRec.IsFake then
    FDirectWriteRec.GetFromBufferExt(PChar(TempBuffer), FRecBufSize)
  else
    FDirectWriteRec.GetFromBuffer(PChar(TempBuffer));
end;

procedure TAstaIOCustomDataset.DoIndexChanging(AIndex: TAstaIOIndex; AReason: TAstaIOIndexReason);
begin
  if (AIndex <> nil) and AIndex.Selected then
    if AReason in [irDefinition, irSelected, irDeleted] then
      ResetCursorRange;
  FIndexDefs.Updated := False;
end;

procedure TAstaIOCustomDataset.SetIndexes(const AValue: TAstaIOIndexes);
begin
  Active := False;
  FIndexes.Assign(AValue);
end;

procedure TAstaIOCustomDataset.SetAggregates(AValue: TAstaIOAggregates);
begin
  FAggregates.Assign(AValue);
end;

procedure TAstaIOCustomDataset.SetIndexFieldNames(const AValue: String);
begin
  if IndexFieldNames <> AValue then
    FIndexes.SelectedIndexFields := AValue;
end;

procedure TAstaIOCustomDataset.SetIndexName(const AValue: String);
begin
  if IndexName <> AValue then
    FIndexes.SelectedIndexName := AValue;
end;

function TAstaIOCustomDataset.GetIndexName: String;
begin
  if (FIndexes.SelectedIndex <> nil) and
     (AnsiCompareText(FIndexes.SelectedIndex.Name, SASTADefIndexName) <> 0) then
    Result := FIndexes.SelectedIndex.Name
  else
    Result := '';
end;

function TAstaIOCustomDataset.GetIndexFieldNames: String;
begin
  if (FIndexes.SelectedIndex <> nil) and
     (AnsiCompareText(FIndexes.SelectedIndex.Name, SASTADefIndexName) = 0) then
    Result := FIndexes.SelectedIndex.Fields
  else
    Result := '';
end;

function TAstaIOCustomDataset.GetIndexFieldCount: Integer;
begin
  Result := 0;
  if FIndexes.SelectedIndex <> nil then
    Result := FIndexes.SelectedIndex.FieldCount;
end;

function TAstaIOCustomDataset.GetIndexField(AIndex: Integer): TField;
begin
  Result := nil;
  if FIndexes.SelectedIndex <> nil then
    Result := FIndexes.SelectedIndex.FieldObj[AIndex];
end;

procedure TAstaIOCustomDataset.CheckSetKeyMode;
begin
  if State <> dsSetKey then
    DatabaseError(SNotEditing, Self);
end;

procedure TAstaIOCustomDataset.AllocKeyBuffers;
var
  i: TAstaIOKeyIndex;
begin
  try
    for i := Low(TAstaIOKeyIndex) to High(TAstaIOKeyIndex) do
      FKeyBuffers[i] := InitKeyBuffer(
        AllocMem(SizeOf(TAstaIOKeyBuffer) + FRecBufSize));
    FKeyBuffersAllocated := True;
  except
    FreeKeyBuffers;
    raise;
  end;
end;

procedure TAstaIOCustomDataset.FreeKeyBuffers;
var
  i: TAstaIOKeyIndex;
begin
  FKeyBuffersAllocated := False;
  for i := Low(TAstaIOKeyIndex) to High(TAstaIOKeyIndex) do begin
    DisposeMem(FKeyBuffers[i], SizeOf(TAstaIOKeyBuffer) + FRecBufSize);
    FKeyBuffers[i] := nil;
  end;
end;

function TAstaIOCustomDataset.GetKeyBuffer(KeyIndex: TAstaIOKeyIndex): PAstaIOKeyBuffer;
begin
  Result := FKeyBuffers[KeyIndex];
end;

function TAstaIOCustomDataset.InitKeyBuffer(Buffer: PAstaIOKeyBuffer): PAstaIOKeyBuffer;
begin
  FillChar(Buffer^, SizeOf(TAstaIOKeyBuffer) + FRecBufSize, 0);
  Result := Buffer;
end;

function TAstaIOCustomDataset.GetKeyExclusive: Boolean;
begin
  CheckSetKeyMode;
  Result := FKeyBuffer^.Exclusive;
end;

procedure TAstaIOCustomDataset.SetKeyExclusive(AValue: Boolean);
begin
  CheckSetKeyMode;
  FKeyBuffer^.Exclusive := AValue;
end;

function TAstaIOCustomDataset.GetKeyFieldCount: Integer;
begin
  CheckSetKeyMode;
  Result := FKeyBuffer^.FieldCount;
end;

procedure TAstaIOCustomDataset.SetKeyFieldCount(AValue: Integer);
begin
  CheckSetKeyMode;
  FKeyBuffer^.FieldCount := AValue;
end;

procedure TAstaIOCustomDataset.SetKeyBuffer(KeyIndex: TAstaIOKeyIndex; Clear: Boolean);
begin
  CheckBrowseMode;
  FKeyBuffer := FKeyBuffers[KeyIndex];
  Move(FKeyBuffer^, FKeyBuffers[kiSave]^, SizeOf(TAstaIOKeyBuffer) + FRecBufSize);
  if Clear then
    InitKeyBuffer(FKeyBuffer);
  SetState(dsSetKey);
  SetModified(FKeyBuffer^.Modified);
  DataEvent(deDataSetChange, 0);
end;

procedure TAstaIOCustomDataset.SetKeyFields(KeyIndex: TAstaIOKeyIndex; const Values: array of const);
var
  I: Integer;
  SaveState: TDataSetState;
begin
  if IndexFieldCount = 0 then
    DatabaseError(SNoFieldIndexes, Self);
  SaveState := SetTempState(dsSetKey);
  try
    FKeyBuffer := InitKeyBuffer(FKeyBuffers[KeyIndex]);
    for I := 0 to High(Values) do
      GetIndexField(I).AssignValue(Values[I]);
    FKeyBuffer^.FieldCount := High(Values) + 1;
    FKeyBuffer^.Modified := Modified;
  finally
    RestoreState(SaveState);
  end;
end;

procedure TAstaIOCustomDataset.PostKeyBuffer(Commit: Boolean);
begin
  DataEvent(deCheckBrowseMode, 0);
  if Commit then
    FKeyBuffer^.Modified := Modified
  else
    Move(FKeyBuffers[kiSave]^, FKeyBuffer^, SizeOf(TAstaIOKeyBuffer) + FRecBufSize);
  SetState(dsBrowse);
  DataEvent(deDataSetChange, 0);
end;

procedure TAstaIOCustomDataset.SetKey;
begin
  SetKeyBuffer(kiLookup, True);
end;

procedure TAstaIOCustomDataset.EditKey;
begin
  SetKeyBuffer(kiLookup, False);
end;

function TAstaIOCustomDataset.FindKey(const KeyValues: array of const): Boolean;
begin
  CheckBrowseMode;
  SetKeyFields(kiLookup, KeyValues);
  Result := GotoKey;
end;

function TAstaIOCustomDataset.FindKey(AnIndexName:String;const KeyValues: array of const): Boolean;
begin
 if (AnIndexName<>'') and (IndexName<>AnIndexName) then IndexName:=AnIndexName;
 CheckBrowseMode;
 SetKeyFields(kiLookup, KeyValues);
 Result := GotoKey;
end;

procedure TAstaIOCustomDataset.FindNearest(const KeyValues: array of const);
begin
  CheckBrowseMode;
  SetKeyFields(kiLookup, KeyValues);
  GotoNearest;
end;

function TAstaIOCustomDataset.InternalGotoKey(ANearest: Boolean): Boolean;
var
  iPos: Integer;
  tmpItem: TAstaDBListItem;
  keyBuff: PAstaIOKeyBuffer;
  locOpts: TAstaIOLocateOptions;
  fldCnt: Integer;
begin
  CheckBrowseMode;
  DoBeforeScroll;
  CursorPosChanged;
  keyBuff := GetKeyBuffer(kiLookup);
  tmpItem := TAstaDBListItem.CreateForLocate(FAstaList, 0);
  try
    tmpItem.GetFromBufferExt(PChar(keyBuff) + SizeOf(TAstaIOKeyBuffer), FRecBufSize);
    if FKeyBuffer^.FieldCount <= 0 then
      fldCnt := FIndexes.SelectedIndex.FieldCount
    else
      fldCnt := FKeyBuffer^.FieldCount;
    if ANearest then
      locOpts := [loFiltered, loNearest]
    else
      locOpts := [loFiltered];
    Result := (Indexes.SelectedIndex.LocateRecord(tmpItem, fldCnt, locOpts, [], iPos) = 0);
  finally
    tmpItem.Free;
  end;
  if Result or (lonearest in Locopts) then begin
    Indexes.Position := iPos;
    Resync([rmExact, rmCenter]);
    DoAfterScroll;
  end;
end;

function TAstaIOCustomDataset.GotoKey: Boolean;
begin
  Result := InternalGotoKey(False);
end;

procedure TAstaIOCustomDataset.GotoNearest;
begin
  InternalGotoKey(True);
end;

procedure TAstaIOCustomDataset.ApplyRange;
begin
  CheckBrowseMode;
  if SetCursorRange then
    First;
end;

procedure TAstaIOCustomDataset.CancelRange;
begin
  CheckBrowseMode;
  UpdateCursorPos;
  if ResetCursorRange then
    Resync([]);
end;

procedure TAstaIOCustomDataset.EditRangeStart;
begin
  SetKeyBuffer(kiRangeStart, False);
end;

procedure TAstaIOCustomDataset.EditRangeEnd;
begin
  SetKeyBuffer(kiRangeEnd, False);
end;

procedure TAstaIOCustomDataset.SetRangeStart;
begin
  SetKeyBuffer(kiRangeStart, True);
end;

procedure TAstaIOCustomDataset.SetRangeEnd;
begin
  SetKeyBuffer(kiRangeEnd, True);
end;

procedure TAstaIOCustomDataset.SetRange(const StartValues, EndValues: array of const);
begin
  CheckBrowseMode;
  SetKeyFields(kiRangeStart, StartValues);
  SetKeyFields(kiRangeEnd, EndValues);
  ApplyRange;
end;

function TAstaIOCustomDataset.SetCursorRange: Boolean;
var
  keyBuff: PAstaIOKeyBuffer;
begin
  Result := False;
  if not (
    BuffersEqual(GetKeyBuffer(kiRangeStart), GetKeyBuffer(kiCurRangeStart),
                 SizeOf(TAstaIOKeyBuffer) + FRecBufSize) and
    BuffersEqual(GetKeyBuffer(kiRangeEnd), GetKeyBuffer(kiCurRangeEnd),
                 SizeOf(TAstaIOKeyBuffer) + FRecBufSize)) then
  begin
    keyBuff := GetKeyBuffer(kiRangeStart);
    if keyBuff^.Modified then begin
      FRangeFrom := TAstaDBListItem.CreateForLocate(FAstaList, 0);
      FRangeFrom.GetFromBufferExt(PChar(keyBuff) + SizeOf(TAstaIOKeyBuffer), FRecBufSize);
      FRangeFromExclusive := keyBuff^.Exclusive;
      FRangeFromFieldCount := keyBuff^.FieldCount;
      if FRangeFromFieldCount = 0 then
        FRangeFromFieldCount := -1;
    end
    else if FRangeFrom <> nil then begin
      FRangeFrom.Free;
      FRangeFrom := nil;
    end;
    keyBuff := FKeyBuffers[kiRangeEnd];
    if keyBuff^.Modified then begin
      FRangeTo := TAstaDBListItem.CreateForLocate(FAstaList, 0);
      FRangeTo.GetFromBufferExt(PChar(keyBuff) + SizeOf(TAstaIOKeyBuffer), FRecBufSize);
      FRangeToExclusive := keyBuff^.Exclusive;
      FRangeToFieldCount := keyBuff^.FieldCount;
      if FRangeToFieldCount = 0 then
        FRangeToFieldCount := -1;
    end
    else if FRangeTo <> nil then begin
      FRangeTo.Free;
      FRangeTo := nil;
    end;
    Move(GetKeyBuffer(kiRangeStart)^, GetKeyBuffer(kiCurRangeStart)^, SizeOf(TAstaIOKeyBuffer) + FRecBufSize);
    Move(GetKeyBuffer(kiRangeEnd)^, GetKeyBuffer(kiCurRangeEnd)^, SizeOf(TAstaIOKeyBuffer) + FRecBufSize);
    FAggregates.Recalc;
    Result := True;
  end;
end;

function TAstaIOCustomDataset.ResetCursorRange: Boolean;
begin
  Result := False;
  if FKeyBuffersAllocated and (
     GetKeyBuffer(kiCurRangeStart)^.Modified or
     GetKeyBuffer(kiCurRangeEnd)^.Modified
    ) then
  begin
    if FRangeFrom <> nil then begin
      FRangeFrom.Free;
      FRangeFrom := nil;
    end;
    if FRangeTo <> nil then begin
      FRangeTo.Free;
      FRangeTo := nil;
    end;
    InitKeyBuffer(GetKeyBuffer(kiCurRangeStart));
    InitKeyBuffer(GetKeyBuffer(kiCurRangeEnd));
    FAggregates.Recalc;
    Result := True;
  end;
end;

function TAstaIOCustomDataset.CheckRanges(ARec: TAstaDBListItem; APart: Integer): Integer;
var
  crOpt: TAstaIOIndexCompareRecOptions;

  function CheckFrom: Integer;
  begin
    case FIndexes.SelectedIndex.CompareRecs(FRangeFrom, ARec, FRangeFromFieldCount, crOpt) of
     1:   Result := -1;
     0:   if FRangeFromExclusive then Result := -1 else Result := 0;
     else Result := 0;
    end;
  end;

  function CheckTo: Integer;
  begin
    case FIndexes.SelectedIndex.CompareRecs(FRangeTo, ARec, FRangeToFieldCount, crOpt) of
    -1:  Result := 1;
     0:  if FRangeToExclusive then Result := 1 else Result := 0;
    else Result := 0;
    end;
  end;

  procedure BuildOpts;
  begin
    crOpt := [crPartial, crNearest];
    if ioDescending in FIndexes.SelectedIndex.Options then
      Include(crOpt, crDescending);
    if ioCaseInsensitive in FIndexes.SelectedIndex.Options then
      Include(crOpt, crCaseInsensitive);
  end;

begin
  Result := 0;
  if ((FRangeFrom <> nil) or (FRangeTo <> nil)) and
     FDoCheckRanges and (FIndexes.SelectedIndex <> nil) then begin
    BuildOpts;
    if APart <= 0 then begin
      if FRangeFrom <> nil then
        Result := CheckFrom;
      if (Result = 0) and (FRangeTo <> nil) then
        Result := CheckTo;
    end
    else begin
      if FRangeTo <> nil then
        Result := CheckTo;
      if (Result = 0) and (FRangeFrom <> nil) then
        Result := CheckFrom;
    end;
  end;
end;

procedure TAstaIOCustomDataset.FirstUsingRanges;
var
  i: Integer;
begin
  if FDoCheckRanges and (FIndexes.SelectedIndex <> nil) and (FRangeFrom <> nil) then begin
    FIndexes.SelectedIndex.LocateRecord(FRangeFrom, FRangeFromFieldCount,
      [loNearest], [loPartialKey], i);
    FIndexes.Position := i - 1;
  end
  else
    FIndexes.Position := -1;
end;

procedure TAstaIOCustomDataset.LastUsingRanges;
var
  i, res: Integer;
begin
  if FDoCheckRanges and (FIndexes.SelectedIndex <> nil) and (FRangeTo <> nil) then begin
    res := FIndexes.SelectedIndex.LocateRecord(FRangeTo, FRangeToFieldCount,
      [loNearest, loEnd], [loPartialKey], i);
    if res = 1 then
      FIndexes.Position := i
    else if res = 0 then
      FIndexes.Position := i + 1;
//    1 -> i
//    0 -> i + 1
  end
  else
    FIndexes.Position := FIndexes.RecsCount;
end;

procedure TAstaIOCustomDataset.Cancel;
begin
  inherited Cancel;
  if State = dsSetKey then
    PostKeyBuffer(False);
end;

procedure TAstaIOCustomDataset.Post;
begin
  inherited Post;
  if State = dsSetKey then
    PostKeyBuffer(True);
end;

type
  THacker = class(TCustomMemoryStream);

procedure PopulateDataSetFromStringWithFieldProperties(DS: TAstaIODataSet; const S: AnsiString);
var
  TempStream: TMemoryStream;
begin
  TempStream := TMemoryStream.Create;
  try
    THacker(TempStream).SetPointer(Pointer(S), Length(S));
    DS.LoadFromStreamWithFields(TempStream);
{$IFDEF FIELDANDSTREAM}
    SetDataSetFieldsFromAFieldStream(DS, TempStream, False);
{$ENDIF}
  finally
    TempStream.Free;
  end;
end;

procedure TAstaIOCustomDataset.DoAfterOpen;
var
  prevAggAct: Boolean;
begin
  prevAggAct := Aggregates.Active;
  Aggregates.Active := False;
  try
      Indexes.FieldsDefined;
      Aggregates.FieldsDefined;
      Indexes.UpToDate;
  finally
    Aggregates.Active := prevAggAct;
  end;
  if Active then begin
    ExecuteNestedActivations;
    ClearNestedActivations;
  end;
  if FSuspendEvents then exit;
  inherited DoAfterOpen;
end;

procedure TAstaIOCustomDataSet.DoAfterClose;
begin
  Indexes.FieldsUnDefined;
  Aggregates.FieldsUnDefined;
  if FSuspendEvents then Exit;
  inherited DoAfterClose;
end;

function TAstaIOCustomDataset.InternalFetchPacket: Boolean;
begin
  Result := False;
end;
function TAstaIOCustomDataset.InternalFetchAll(AForce: Boolean): Boolean;
begin
  Result := False;
end;

procedure TAstaIOCustomDataset.FetchAll(AForce: Boolean);
begin
  CheckBrowseMode;
  if InternalFetchAll(AForce) then
    DataEvent(deDataSetChange, 0);
end;

procedure TAstaIOCustomDataset.SetFilterOptions(Value: TFilterOptions);
begin
  if Value <> FilterOptions then
    SetFilterData(Filter, Value);
end;

procedure TAstaIOCustomDataset.SetFilterText(const Value: string);
begin
  if Value <> Filter then
    SetFilterData(Value, FilterOptions);
end;

procedure TAstaIOCustomDataset.SetOnFilterRecord(const Value: TFilterRecordEvent);
begin
  inherited SetOnFilterRecord(Value);
  if Filtered and Active then
    FAggregates.Recalc;
end;

procedure TAstaIOCustomDataset.SetFiltered(Value: Boolean);
begin
  if Value <> Filtered then begin
    inherited SetFiltered(Value);
    if Active then begin
      FAggregates.Recalc;
      if State = dsBrowse then begin
        UpdateCursorPos;
        Resync([]);
      end;
    end;
  end;
end;

procedure TAstaIOCustomDataset.UnprepareFilter;
begin
  if FFilterTreeReader <> nil then begin
    TAstaExpressionTreeReader(FFilterTreeReader).Free;
    FFilterTreeReader := nil;
  end;
end;

procedure TAstaIOCustomDataset.PrepareFilter(const Text: string; Options: TFilterOptions);
begin
  UnprepareFilter;
  try
    FFilterTreeReader := TAstaExpressionTreeReader.Create(Self, Text, Options, fuFilter, '');
    FFilterTreeReader.Parse;
  except
    if FFilterTreeReader <> nil then
      TAstaExpressionTreeReader(FFilterTreeReader).Free;
    FFilterTreeReader := nil;
    raise;
  end;
end;

procedure TAstaIOCustomDataset.SetFilterData(const Text: string; Options: TFilterOptions);
begin
  if Active then begin
    CheckBrowseMode;
    if Text <> '' then
      PrepareFilter(Text, Options)
    else
      UnprepareFilter;
    if Filtered then begin
      FAggregates.Recalc;
      UpdateCursorPos;
      Resync([]);
    end;
  end;
  inherited SetFilterText(Text);
  inherited SetFilterOptions(Options);
end;

procedure TAstaIOCustomDataset.UnprepareExpressions;
var
  i: Integer;
begin
  if FExpressions <> nil then begin
    for i := 0 to FExpressions.Count - 1 do
      FExpressions.Objects[i].Free;
    FExpressions.Clear;
    FExpressionUsages := [];
  end;
end;

procedure TAstaIOCustomDataset.PrepareExpressions;
var
  i: Integer;

  procedure CompileExpression(const AName, AExpression, AFieldName: String;
    AUsage: TAstaExpressionUsage; const AErrMsg: String);
  var
    exp: TAstaExpressionTreeReader;
  begin
    exp := TAstaExpressionTreeReader.Create(Self, AExpression, [], AUsage, AFieldName);
    try
      exp.Parse;
      FExpressions.AddObject(AName, exp);
      Include(FExpressionUsages, AUsage);
    except
      on E: Exception do begin
        exp.Free;
        raise Exception.CreateFmt(AErrMsg, [E.Message]);
      end;
    end;
  end;

begin
  if FExpressions.Count > 0 then
    UnprepareExpressions;
  FExpressionUsages := [];
  for i := 0 to Constraints.Count - 1 do
    with Constraints[i] do begin
      if CustomConstraint <> '' then
        CompileExpression(#255'CC' + IntToStr(i), CustomConstraint, '', fuRecordCheck,
          SFailedCompileCustRecConstr);
      if ImportedConstraint <> '' then
        try
          CompileExpression(#255'CI' + IntToStr(i), ImportedConstraint, '', fuRecordCheck,
            SFailedCompileImpRecConstr);
        except
          // If we get exception during compile, then probably
          // we does not have all fields included into dataset
          // In this case - simply avoid compilation
          if not FromDictionary then
            raise;
        end;
    end;
  for i := 0 to FieldCount - 1 do
    with Fields[i] do begin
      if CustomConstraint <> '' then
        CompileExpression(FieldName + #255'CC', CustomConstraint, FieldName,
          fuFieldCheck, SFailedCompileCustFldConstr1 + FieldName + SFailedCompileCustFldConstr2);
      if ImportedConstraint <> '' then
        CompileExpression(FieldName + #255'CI', ImportedConstraint, FieldName,
          fuFieldCheck, SFailedCompileImpFldConstr1 + FieldName + SFailedCompileImpFldConstr2);
      if DefaultExpression <> '' then
        CompileExpression(FieldName + #255'D', DefaultExpression, FieldName,
          fuFieldDefault, SFailedCompileDefFldExpr1 + FieldName + SFailedCompileDefFldExpr2);
    end;
end;

procedure TAstaIOCustomDataset.EvaluteDefaults(AFieldKinds: TFieldKinds);
var
  i, j: Integer;
  exp: TAstaExpressionTreeReader;
begin
  if fuFieldDefault in FExpressionUsages then
    for i := 0 to FieldCount - 1 do
      with Fields[i] do
        if (FieldKind in AFieldKinds) and (DefaultExpression <> '') then begin
          if not FExpressions.Find(FieldName + #255'D', j) then
            raise EDatabaseError.CreateFmt(SDefExprNotFound, [FieldName]);
          exp := TAstaExpressionTreeReader(FExpressions.Objects[j]);
          try
            exp.Evaluate;
          except
            on E: Exception do begin
              raise EDatabaseError.CreateFmt(SDefExprEvalFailed, [FieldName, E.Message]);
            end;
          end;
        end;
end;

procedure TAstaIOCustomDataset.EvaluteChecks(AField: TField);

  procedure EvaluteSingle(const AExpName, AMessage, AErrMsg: String; AImported: Boolean);
  var
    i: Integer;
    V: Variant;
  begin
    if not FExpressions.Find(AExpName, i) then
      if AImported then
        Exit
      else
        raise EDatabaseError.Create(SCheckNotFound);
    V := TAstaExpressionTreeReader(FExpressions.Objects[i]).Evaluate;
    if VarIsEmpty(V) or VarIsNull(V) or not V then
      raise EDatabaseError.CreateFmt(AErrMsg, [AMessage]);
  end;

  procedure EvaluteField(AField: TField);
  begin
    with AField do begin
      if CustomConstraint <> '' then
        EvaluteSingle(FieldName + #255'CC', ConstraintErrorMessage,
          SFailedFieldCustConstr1 + AField.DisplayLabel + SFailedFieldCustConstr2, False);
      if ImportedConstraint <> '' then
        EvaluteSingle(FieldName + #255'CI', ConstraintErrorMessage,
          SFailedFieldImpConstr1 + AField.DisplayLabel + SFailedFieldImpConstr2, True);
    end;
  end;

var
  i: Integer;
begin
  if ConstraintsDisabled then
    Exit;
  if AField = nil then begin
    if fuRecordCheck in FExpressionUsages then
      for i := 0 to Constraints.Count - 1 do
        with Constraints[i] do begin
          if CustomConstraint <> '' then
            EvaluteSingle(#255'CC' + IntToStr(i), ErrorMessage,
              SFailedRecCustConstr, False);
          if ImportedConstraint <> '' then
            EvaluteSingle(#255'CI' + IntToStr(i), ErrorMessage,
              SFailedRecImpConstr, True);
        end;
    if fuFieldCheck in FExpressionUsages then
      for i := 0 to FieldCount - 1 do
        EvaluteField(Fields[i]);
  end
  else if fuFieldCheck in FExpressionUsages then
    EvaluteField(AField);
end;

procedure TAstaIOCustomDataset.DoOnCalcFields;
begin
  EvaluteDefaults([fkCalculated]);
  inherited DoOnCalcFields;
end;

procedure TAstaIOCustomDataset.DoValidate(ASender: TField);
begin
  EvaluteChecks(ASender);
  if Assigned(FCurValidate) then
    FCurValidate(ASender);
end;

procedure TAstaIOCustomDataset.DisableConstraints;
begin
  Inc(FConstDisableCount);
end;

procedure TAstaIOCustomDataset.EnableConstraints;
begin
  if FConstDisableCount > 0 then
    Dec(FConstDisableCount);
end;

function TAstaIOCustomDataset.ConstraintsDisabled: Boolean;
begin
  Result := FConstDisableCount > 0;
end;

procedure TAstaIOCustomDataset.DataEvent(Event: TDataEvent; Info: Integer);
var
  i: Integer;
begin
  if Event = deFieldChange then
    if (TField(Info).FieldKind = fkCalculated) and TField(Info).IsIndexField and
       (State = dsSetKey) then
      SetModified(True);
  inherited DataEvent(Event, Info);
  if Event = deCheckBrowseMode then
    for i := 0 to NestedDataSets.Count - 1 do
      (TDataSet(NestedDataSets[i]) as TAstaIOCustomDataset).PostDataToParentDataSet;
end;

procedure TAstaIOCustomDataset.PostDataToParentDataSet;
begin
end;

procedure TAstaIOCustomDataset.ChangesSaved;
begin
  FSaveChanges := False;
end;

procedure TAstaIOCustomDataset.RequestNestedActivation(ADataSet: TAstaIOCustomDataset);
begin
  if FReqActList = nil then
    FReqActList := TList.Create;
  if FReqActList.IndexOf(ADataSet) = -1 then
    FReqActList.Add(ADataSet);
end;

procedure TAstaIOCustomDataset.RemoveNestedActivation(ADataSet: TAstaIOCustomDataset);
begin
  if FReqActList <> nil then
    FReqActList.Remove(ADataSet);
end;

procedure TAstaIOCustomDataset.ClearNestedActivations;
begin
  if FReqActList <> nil then
  begin
    FReqActList.Clear;
    FReqActList.Free;
    FReqActList := nil;
  end;
end;

procedure TAstaIOCustomDataset.ExecuteNestedActivations;
var
  i: Integer;
begin
  if FReqActList <> nil then
    for i := 0 to FReqActList.Count - 1 do
      TAstaIOCustomDataset(FReqActList[i]).Active := True;
end;
{$ifdef AstaIOXML}
function TAstaIOCustomDataset.DataSetFactory(const dsName: string;
  const fldDefs: array of ThunkFieldDef): TDataSet;
var
  i: integer;
begin
  if Active then Empty;
  Close;
  NukeAllFieldInfo;
  for i := Low(fldDefs) to High(fldDefs) do
    with fldDefs[i] do
      AddField(mname,mtype,msize);
  Result := self;
end;

procedure TAstaIOCustomDataset.LoadFromXML(const Stream: TStream);
begin
  LoadFromXMLgeneric(Stream, DataSetFactory);
end;

procedure TAstaIOCustomDataset.LoadFromXML(const FileName: string);
begin
  LoadFromXMLgeneric(FileName, DataSetFactory);
end;

procedure TAstaIOCustomDataset.SaveToXML(const FileName: string;XMLFormat:TAstaIOXMLDataSetFormat=taxADO);
begin
Case XMLFormat of
  taxADO   :AstaSaveMS_XML(self,FileName);
  taxMidas :AstaSaveMidas_XML(self,FileName);
end;
end;

procedure TAstaIOCustomDataset.SaveToXML(Stream:TStream;XMLFormat:TAstaIOXMLDataSetFormat);
begin
Case XMLFormat of
  taxADO   :AstaSaveMS_XML(self,Stream);
  taxMidas :AstaSaveMidas_XML(self,Stream);
end;
end;
{$endif}

function AstaISQLBenchmarkString(D: TAstaIOCustomDataset; STartTime, Endtime: TTimeStamp): string;
var
  TotalTime,
    TotalRows: string;
  tbInt: Integer;
  bps: double;
  TotalBytes, bytespersecond: string;
  BytesPerRow: string;
  Elapsedtime: double;
begin
  ElapsedTime := (EndTime.time - StartTime.time) * 0.001;
  TotalTime := FloatToStrF(ElapsedTime, ffNumber, 12, 4);
  TotalRows := IntToStr(D.RecordCount);
  BytesPerRow := IntToStr(d.GetRecordSize);
  TBInt := d.RecordCount * d.GetRecordSize;
  TotalBytes := IntToStr(TBInt);
  if (ElapsedTime < 1) then
    bps := (tbint + (1 - elapsedtime) / elapsedtime)
  else
    BPS := TBInt / ElapsedTime;
  BytesPerSecond := FloatToStrF(BPS, ffNumber, 12, 0);

  result := TotalTime + ' Rows=' + TotalRows + ' ' +
    'Bytes per Row=' + BytesPerRow + ' Total Bytes Fetched=' + TotalBytes +
    ' Bytes Per Second =' + BytesPerSecond;
end;

function TAstaIODataSet.FieldDefsStored: Boolean;
begin
  Result := StoreDefs and (FieldDefs.Count > 0);
end;

constructor TAstaIODataSet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FStoreDefs := True;
end;

destructor TAstaIODataSet.Destroy;
begin
  // Sometimes it gives av with line below ???
  // if assigned(FOldValuesDataSet) then FOldValuesDataSet.Free;
  inherited Destroy;
end;
{ TAstaIOAuditDataSet }

constructor TAstaIOAuditDataSet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FStoreDefs := True;
end;


{ TAstaCustomAuditDataSet }




constructor TAstaCustomAuditDataSet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOldValuesDataSet := nil;
  FUpdateMethod := umCached;
  FUpdateMode := upWhereKeyOnly;
  FPrimeFields := TStringList.Create;
  FNOSQLFields := TStringList.create;
  FRefetchFields := TStringList.create;
  FParams := TParams.Create(Self);
  FStreamOptions:=[];
  FMultiTableDataSet:=nil;
  FFieldMapDataSet:=nil;
end;

destructor TAstaCustomAuditDataSet.Destroy;
begin
  FMultiTableDataSet.Free;
  FFieldMapDataSet.Free;
  FPrimeFields.Free;
  FParams.Free;
  FNOSQLFields.Free;
  FRefetchFields.Free;
  inherited Destroy;
end;

procedure TAstaCustomAuditDataSet.SaveToStream(Stream: TStream);
begin
  inherited SaveToStream(Stream);
  if not (ssCachedUpdates in FStreamOptions) then Exit;
  if FOldValuesDataSet = nil then SetupDeltaDataSet;
  FOldValuesDataSet.SaveToStream(Stream);

end;

procedure TAstaCustomAuditDataSet.LoadFromStream(Stream: TStream);
begin
  inherited LoadFromStream(Stream);
  if not (ssCachedUpdates in FStreamOptions) then Exit;
  if FOldValuesDataSet<> nil then FreeAndNil(FOldValuesDataSet);
  SetupDeltaDataSet;
  FOldValuesDataSet.LoadFromStream(Stream);
end;

function TAstaCustomAuditDataSet.FieldDefsStored: Boolean;
begin
  Result := StoreDefs and (FieldDefs.Count > 0);
end;

function TAstaCustomAuditDataSet.GetPrimeFields: TStrings;
begin
  result := FPrimeFields;
end;

procedure TAstaCustomAuditDataSet.SetPrimeFields(Value: TStrings);
begin
  FPrimeFields.Assign(Value);
end;

function TAstaCustomAuditDataSet.GetRefetchFields: TStrings;
begin
  result := FRefetchFields;
end;

procedure TAstaCustomAuditDataSet.SetRefetchFields(Value: TStrings);
begin
  FRefetchFields.Assign(Value);
end;

function TAstaCustomAuditDataSet.GetNoSQLFields: TStrings;
begin
  result := FNoSQLFields;
end;

procedure TAstaCustomAuditDataSet.SetNOSQLFields(Value: TStrings);
begin
  FNoSQLFields.Assign(Value);
end;

procedure TAstaCustomAuditDataSet.SetParams(Value: TParams);
begin
  FParams.Assign(Value);
end;

function TAstaCustomAuditDataSet.GetParams: TParams;
begin
  Result := FParams;
end;

function TAstaCustomAuditDataSet.GetParamsCount: Integer;
begin
  Result := FParams.Count;
end;

procedure TAstaCustomAuditDataSet.SetUpdateMethod(Value: TAstaUpdateMethod);
begin
  FUpDateMethod := Value;
end;

// hooks for change tracking
procedure TAstaCustomAuditDataSet.DoAfterCancel;
begin
  if FSuspendEvents then exit;
  inherited DoAfterCancel;
  if TrackDeltas and not FBookMarkAccessed then PopOldValues;
  FBookMarkAccessed := False;
end;

procedure TAstaCustomAuditDataSet.DoAfterInsert;
begin
  if not FSuspendEvents then inherited DoAfterInsert;
  FBookMarkAccessed := False;
  if (csdesigning in componentstate) then exit;
  if TrackDeltas then PushRowData(dtAppend);
end;

procedure TAstaCustomAuditDataSet.DoBeforeDelete;
begin
  if not FSuspendEvents then inherited DobeforeDelete;
  if TrackDeltas then PushRowData(dtDelete);
end;

procedure TAstaCustomAuditDataSet.InternalDelete;
begin
  if FUpdateMethod = umAfterPost then DoDBUpdates;
  inherited InternalDelete;
end;

procedure TAstaCustomAuditDataSet.DoBeforeEdit;
begin
  if not FSuspendEvents then inherited DoBeforeEdit;
  if (csdesigning in componentstate) then exit;
  //if RowHasBeenChanged then FBookMarkAccessed := True; // sm - 2/9/2003 replaced with line below. 
  FBookMarkAccessed := RowHasBeenChanged;
  if TrackDeltas then PushRowData(dtEdit)
end;

procedure TAstaCustomAuditDataSet.DoBeforePost;
begin
  inherited DoBeforePost;
  FBeforePostState := State;
end;

procedure TAstaCustomAuditDataSet.DoAfterPost;
begin
  if not (csdesigning in componentstate) and TrackDeltas and
     (FOldValuesDataSet <> nil) and (FBeforePostState in [dsInsert, dsEdit]) then
    if FBeforePostState = dsEdit then
      FixOldValues(dtEdit)
    else if FBeforePostState = dsInsert then
      FixOldValues(dtAppend);
  inherited DoAfterPost;
end;

procedure TAstaCustomAuditDataSet.InternalPost;
begin
  if FUpdateMethod = umAfterPost then DoDBUpdates;
  inherited InternalPost;
end;

procedure TAstaCustomAuditDataSet.InternalClose;
begin
  inherited InternalClose;
  FSavePoint := 0;
  if assigned(FOldValuesDataSet) then
  begin
    FOldValuesDataSet.Empty;
    FreeAndNil(FOldValuesDataSet);
  end;
end;

procedure TAstaCustomAuditDataSet.DoDBUpdates;
begin
  //
end;

// delta management
function TAstaCustomAuditDataSet.TrackDeltas: Boolean;
begin
  Result := FUpdateMethod <> umManual;
end;

procedure TAstaCustomAuditDataSet.SetupDeltaDataSet;
begin
  FOldValuesDataSet.Free;
  FOldValuesDataSet := TAstaIODataSet.Create(nil);
  FOldValuesDataSet.FDisposeAstaList := True;
  FOldValuesDataSet.RetrieveFieldsFromDataSet(Self, False, False); // sm 9/8/2001
  FOldValuesDataSet.AddField(sfld_BookMark, ftinteger, 0);
  FOldValuesDataSet.AddField(sfld_Delta, ftinteger, 0);
  FOldValuesDataSet.AddField(sfld_SavePoint, ftInteger, 0);
  FOldValuesDataSet.AddField(sfld_ErrorCode, ftInteger, 0);
  FOldValuesDataSet.AddField(sfld_ErrorInfo, ftMemo, 0);
  FOldValuesDataSet.AstaFieldCreate(False);

  {
  with FOldValuesDataSet.Indexes.Add do begin
   Name := 'Prime';
   Fields := sfld_BookMark;
   Active := True;
   FOldValuesDataSet.IndexName := 'Prime';
  end;
  }
  FOldValuesDataSet.Open;
end;

procedure TAstaCustomAuditDataSet.SetMultiFieldUpdateTableName(AFieldName,AnUpdateTableName:String;Primekey:Boolean);
begin
 if FMultiTableDataset=nil then SetupMultiTabledataSet;
 if not FMultiTableDataset.Locate('FieldName',AFieldName,[loCaseInsensitive]) then
  raise Exception.Create(AFieldName+' not found. ');
 with FMultiTabledataSet do begin
  Edit;
  FieldByName('UpdateTableName').AsString:=AnUpdateTableName;
  FieldByname('PrimeKey').AsBoolean:=PrimeKey;
  Post;
 end;
end;

procedure TAstaCustomAuditDataSet.MapColumn(CurrentField,MapToField,MapTableName:String);
begin
 if FFieldMapDataSet=nil then SetupFieldMapDataSet
{ if not FMultiTableDataset.Locate('FieldName',AFieldName,[loCaseInsensitive]) then
  raise Exception.Create(AFieldName+' not found. ');
 with FMultiTabledataSet do begin
  Edit;
  FieldByName('UpdateTableName').AsString:=AnUpdateTableName;
  FieldByname('PrimeKey').AsBoolean:=PrimeKey;
  Post;
 end;}
end;

procedure TAstaCustomAuditDataSet.SetupMultiTableDataSet;
var
i:integer;
begin
  FMultiTableDataSet.Free;
  FMultiTableDataSet:= TAstaIODataSet.Create(nil);
  FMultiTableDataSet.AddField('FieldName',ftString, 100);
  FMultiTableDataSet.AddField('UpdateTableName',ftString, 100);
  FMultiTableDataSet.AddField('PrimeKey',ftboolean,0);
  FMultiTableDataSet.Open;
  FMultiTableDataSet.AddIndex('FieldName',False);
  for i:=0 to FieldCount-1 do
   FMultiTableDataSet.AppendRecord([Fields[i].FieldName,FUpdateTableName,False]);
end;

procedure TAstaCustomAuditDataSet.SetupFieldMapDataSet;
begin
  FFieldMapDataSet.Free;
  FFieldMapDataSet:= TAstaIODataSet.Create(nil);
  FFieldMapDataSet.AddField('FieldName',ftString, 100);
  FFieldMapDataSet.AddField('MapName',ftString, 100);
  FFieldMapDataSet.Open;
  FFieldMapDataSet.AddIndex('FieldName',False);
  FFieldMapDataSet.AddIndex('MapName',False);
end;

procedure TAstaCustomAuditDataSet.PopOldValues;
begin
  if (FOldValuesDataSet <> nil) and (not FOldValuesDataSet.Eof) then
  begin
    FOldValuesDataSet.delete;
  end;
end;

procedure TAstaCustomAuditDataSet.FixOldValues(Value: TDeltaType);
begin
  if not RowHasBeenChanged then exit;
  if Value = dtEdit then begin
    if UpdateStatus = usUnmodified then
      FOldValuesDataSet.Delete;
  end
  else if Value = dtAppend then begin
    if FOldValuesDataSet.FieldbyName(sfld_BookMark).AsInteger = 0 then begin
      FOldValuesDataSet.Edit;
      FOldValuesDataSet.FieldbyName(sfld_BookMark).AsInteger := GetBookMarkAsInteger;
      FOldValuesDataSet.Post;
    end;
  end;
end;

procedure TAstaCustomAuditDataSet.PushRowData(Value: TDeltaType);
var
  i: Integer;
begin
  if FOldValuesDataSet = nil then SetupDeltaDataSet;
  if (Value in [dtEdit, dtDelete]) and AlreadyAppended then
  begin
    if (Value <> dtDelete) then exit;
    if FoldValuesDataSet.FieldbyName(sfld_Delta).AsInteger in [ord(dtEdit), ord(dtDelete)] then
    begin
      FOldValuesDataSet.Edit;
      FOldValuesDataSet.FieldbyName(sfld_Delta).asInteger := Ord(dtDelete);
      FOldValuesDataSet.Post;
      exit;
    end;
    {if it's been appended and now deleted, kiil it!}
    FOldValuesDataSet.Edit;
    FOldValuesDataSet.FieldbyName(sfld_Delta).asInteger := Ord(dtAppendAndDelete);
    FOldValuesDataSet.Post;
    exit;
  end;

  FOldValuesDataSet.Append;
  if Eof and (Value = dtAppend) {(RecordCount = 0)} then
    FOldValuesDataSet.FieldByName(sfld_BookMark).AsInteger := 0
  else
    FOldValuesDataSet.FieldByName(sfld_BookMark).AsInteger := GetBookMarkAsInteger;

  if (value = dtappend) then FOldValuesDataSet.FieldByName(sfld_BookMark).AsInteger := FLastBookMark + 1;
  FOldValuesDataSet.FieldbyName(sfld_Delta).asInteger := Ord(Value);
  Inc(FSavePoint);
  FOldValuesDataSet.FieldbyName(sfld_SavePoint).asInteger := FSavePoint;

  for i := 0 to FieldCount - 1 do
    if Fields[i].FieldKind = fkData then
      DBFieldsAssign(FOldValuesDataSet.FieldByName(Fields[i].FieldName), Fields[i]);
  FOldValuesDataSet.Post;
end;

function TAstaCustomAuditDataSet.RowHasBeenChanged: Boolean;
begin
  Result := False;
  if FOldValuesDataSet = nil then exit;
  Result := FOldValuesDataSet.Locate(sfld_BookMark, GetBookMarkAsInteger, []);
end;

function TAstaCustomAuditDataSet.AlreadyAppended: Boolean;
begin
  Result := RowHasBeenChanged;
end;

// update status & filtering
function TAstaCustomAuditDataSet.GetChangedCount: Integer;
begin
  Result := 0;
  if FOldValuesDataSet = nil then exit;
  Result := FOldValuesDataSet.RecordCount;
end;

function TAstaCustomAuditDataSet.UpdatesPending: Boolean;
{ This is a smarter version of the original. Its logic depends
  on the fact that unmodified records are not saved in the update
  cache (see DoBeforePost). }
var
  i: integer;
begin
  Result := FALSE;
  // If FOldValuesDataSet is not empty, then we must have an update
  // pending. The *only* time this may not be the case is if we are
  // editing a record and there is only one record in the update
  // cache. In this case, the record in the update cache holds the
  // deltas for the record we are currently editing. To be sure that
  // an update is pending, we need to check that at least one field
  // has been modified.
  if Assigned(FOldValuesDataSet) and (not OldValuesDataSet.IsEmpty)
    then
    if (State <> dsEdit) or
      (FOldValuesDataSet.FieldByName(sfld_Delta).AsInteger <>
      Ord(dtEdit)) or (FOldValuesDataSet.RecordCount > 1)
      then
      Result := TRUE
    else
      // If we get to here, then FOldValuesDataSet.RecordCount = 1,
      // delta type = dtEdit, and State = dsEdit. This means the
      // record in FOldValuesDataSet corresponds to the current
      // record being edited and does not necessarily mean that an
      // update is pending. However, an update will be pending if
      // at least one field has been modified.
      if Modified then
        // Because Modified may return true when there are no updates
        // to be applied (for example, if a user edits a field and
        // then edits it back to the starting value), we need to do a
        // brute force search for a modified field. However, if
        // Modified returns false, we'll save time by avoiding the
        // brute force search.
        for i := 0 to Fields.Count - 1 do
          if (FoldValuesDataSet.FindField(Fields[i].FieldName)<>Nil) and DBFieldsAreNotEqual(Fields[i],
            FOldValuesDataSet.FieldByName(Fields[i].FieldName))
            then
          begin
            Result := TRUE;
            Break;
          end;
end;

function TAstaCustomAuditDataSet.UpdateStatus: TUpdateStatus;
{ Returns the update status of the current record. TUpdatesStatus
  is defined in the DB unit. Possible return values are
  usUnmodified, usModified, and usInserted. Because of how the
  TAstaClientDataSet is implemented, usDeleted will never be
  returned by this routine. (Unapplied deletes are saved in
  FOldValuesDataSet as records with deltas of type dtDelete.) }
var
  i: integer;
begin
  Result := usUnmodified;
  if State = dsInsert then
    Result := usInserted
  else
    if RowHasBeenChanged then
      case FOldValuesDataSet.FieldbyName(sfld_Delta).AsInteger of
        Ord(dtAppend): Result := usInserted;
        Ord(dtEdit): begin
                       if State <> dsEdit then Result := usUnModified;
                       for i := 0 to Fields.Count - 1 do
                         begin
                           if (Fields[i].FieldKind = fkdata)  and (FoldValuesDataSet.FindField(fields[i].FieldName)<>nil)
                           and  DBFieldsAreNotEqual(Fields[i], FOldValuesDataSet.FieldByName(Fields[i].FieldName)) then
                           begin
                            Result := usModified;
                            exit;
                           end;
                          end;
                      end;
       end;
end;

function TAstaCustomAuditDataSet.CheckRecordStatusFilter: Boolean;
begin
  Result := (FStatusFilter <> []);
end;

procedure TAstaCustomAuditDataSet.RecordStatusFilter(DataSet: TDataSet; var Accept: Boolean);
begin
  Accept := (FStatusFilter = []) or (UpdateStatus in FStatusFilter);
end;

procedure TAstaCustomAuditDataSet.SetStatusFilter(const Value: TUpdateStatusSet);
begin
  if FStatusFilter <> Value then begin
    FStatusFilter := Value;
    UpdateCursorPos;
    Resync([]);
  end;
end;

function TAstaCustomAuditDataSet.GetOldFieldData(Field: TField;
  Buffer: Pointer): Boolean;
var
  OldField: TField;
begin
  if FOldValuesDataSet <> nil then begin
    OldField := FOldValuesDataSet.FindField(Field.FieldName);
    if OldField <> nil then
      Result := TAstaIOCustomDataset(FOldValuesDataSet).GetFieldData(OldField, Buffer)
    else
      Result := False;
  end
  else
    Result := False;
end;

// savepoints
procedure TAstaCustomAuditDataSet.RestoreDeltaValues(UseAppend: Boolean);
var
  i,FFieldIndex:Integer;
  TempUM: TAstaUpdateMethod;
  TempEvent: TDataSetNotifyEvent;
begin
  TempUM := FUpdateMethod;
  FUpdateMethod := umManual;
  if useAppend then
    Insert
  else
    if State <> dsEdit then Edit;

  FFieldIndex := 0;  // dj - 2003/07/15
  for i := 0 to Fieldcount - 1 do
    if Fields[i].FieldKind = fkData then  // skip calculate and lookup field
    begin
      DBFieldsAssign(Fields[i], FOldValuesDataSet.Fields[FFieldIndex]);
      Inc(FFieldIndex);
    end;


  TempEvent := nil;
  if (State = dsEdit) or (State = dsInsert) then
  begin
    if Assigned(BeforePost) then
    begin
      TempEvent := BeforePost;
      BeforePost := nil;
    end;
    Post;
  end;
  if Assigned(TempEvent) then BeforePost := TempEvent;
  FUpdateMethod := TempUM;
end;

function TAstaCustomAuditDataSet.MoveToBookMarkFromDelta: Boolean;
var
  BmInt: Integer;
  bm: TBytes;
begin
  result := False;
  BMInt := FOldValuesDataSet.FieldByName(sfld_BookMark).AsInteger;
  SetLength(bm, SizeOf(Integer));
  Move(BmInt, bm[0], SizeOf(Integer));
  if not BookmarkValid(bm) then Exit;
  if UpdateMethod <> umAfterPost then
    BookMark := BM;
  result := True;
end;

procedure TAstaCustomAuditDataSet.BookMarkPositionFromDeleted;
var
  DeltaBM: Integer;
begin
  First;
  DeltaBM := FOldValuesDataSet.FieldByName(sfld_BookMark).AsInteger;
  while not Eof and (DeltaBM > GetBookmarkAsInteger) do
    Next;
end;

procedure TAstaCustomAuditDataSet.RollBackCache(Value: Integer);
var
  Bm: TBookmark;
  OkToPopBookMark: Boolean;
  TempEvent: TDataSetNotifyEvent;
  isfiltered, isCheckRanges: Boolean;
  fld_SavePoint, fld_Delta: TField;
  TempUM: TAstaUpdateMethod;
begin
  if not Assigned(FOldValuesDataSet) then exit;
  TempEvent := nil;
  FIsBusyCancel := True;
  TempUM := FUpdateMethod;
  FUpdateMethod := umManual;

  if (State = dsEdit) or (State = dsInsert) then
  begin
    if Assigned(BeforePost) then
    begin
      TempEvent := BeforePost; // so that before post doesnt' fire when updates are canceled
      BeforePost := nil;
    end;
    Post;
  end;
  Bm := Bookmark;
  isFiltered := Filtered;
  isCheckRanges := FDoCheckRanges;
  try
    OkToPopBookMark := True;
    DisableControls;
    Filtered := False;
    FDoCheckRanges := False;
    fld_SavePoint := FOldValuesDataSet.FieldByName(sfld_SavePoint);
    fld_Delta := FOldValuesDataSet.FieldbyName(sfld_Delta);
    FOldValuesDataSet.Last;
    while not FOldValuesDataSet.Bof do
    begin
      if fld_SavePoint.AsInteger >= Value then
      begin
        case fld_Delta.AsInteger of
          Ord(dtEdit):
            begin
              MoveToBookMarkFromDelta;
              RestoreDeltaValues(False);
            end;

          Ord(dtDelete):
            begin
              BookMarkPositionFromDeleted;
              RestoreDeltaValues(True);
            end;

          Ord(dtAppend):
            begin
              if MoveToBookMarkFromDelta then
              begin
                if okTopopBookMark then
                  if bm = BookMark then
                    OkToPopBookMark := False;
                Delete;
              end;
            end;
        end;
        FOldValuesDataSet.Delete;
      end
      else
        FOldValuesDataSet.Prior;
    end;
    if OkToPopBookMark then BookMark := Bm;
  finally
    if Assigned(TempEvent) then BeforePost := TempEvent;
    Filtered := isFiltered;
    FDoCheckRanges := isCheckRanges;
    FIsBusyCancel := False;
    FUpdateMethod := TempUM;
    EnableControls;
  end;
end;

function TAstaCustomAuditDataSet.GetSavePoint: Integer;
begin
  Result := FSavePoint;
end;

procedure TAstaCustomAuditDataSet.SetSavePoint(Value: Integer);
begin
  CheckActive;
  BeginBatch;
  FsuspendEvents := True;
  try
    if Value > FSavePoint then exit;
    Cancel;
    CheckBrowseMode;
    UpdateCursorPos;
    RollBackCache(Value);
    CursorPosChanged;
  finally
    FSavePoint := Value;
    FSuspendEvents := False;
    EndBatch;
  end;
end;

function TAstaCustomAuditDataSet.CurrentValueDataSetRecord(DataSet:TAstaIODataSet = nil): TAstaIODataSet;
begin
 if not Assigned(DataSet) then
    result := TAstaIODataSet.Create(nil)
  else
  begin
    Result:=DataSet;
    Result.Empty;
    Result.Close;
    Result.FieldDefs.Clear;
    Result.Fields.Clear;
  end;
  result.RetrieveFieldsFromDataSet(Self, False);
  result.AddField(sfld_BookMark, ftInteger, 0);
  result.Open;
  result.FieldbyName(sfld_BookMark).Visible := False;

  result.AppendSingleRecordFromSource(Self);

  result.Edit;
  result.FieldByName(sfld_BookMark).AsInteger := self.GetBookMarkAsInteger;
  result.Post;



end;

procedure TAstaCustomAuditDataSet.CancelUpdates;
begin
  SetSavePoint(0);
end;

// undo changes routines
function TAstaCustomAuditDataSet.InternalRevertRecord(DoDeltaBookMarkCheck :Boolean = True): Boolean;
var
  TempUM: TAstaUpdateMethod;
begin
  Result := False;
  if not Assigned(FOldValuesdataSet) then exit;
  DisableControls;
  FIsBusyCancel := True;

  FSuspendEvents := True;
  TempUM := FUpdateMethod;
  FUpdateMethod := umManual;
  try
    if not RowHasBeenChanged then exit;

    if OldValuesdataSet.FieldbyName(sfld_Delta).AsInteger = Ord(dtAppend) then
      Delete
    else
      RestoreDeltaValues(OldValuesdataSet.FieldbyName(sfld_Delta).AsInteger = Ord(dtDelete));
    OldValuesdataSet.Delete;
    Result := True;
  finally
    FSuspendEvents := False;
    FUpdateMethod := TempUM;
    EnableControls;
  end;
end;

function TAstaCustomAuditDataSet.RevertRecord: Boolean;
begin
  CheckActive;
  Result := InternalRevertRecord;
end;

function TAstaCustomAuditDataSet.UndoLastChange(FollowChange: Boolean): Boolean;
var
  Bm: TBookmark;
begin
  Bm := Bookmark;
  Result := False;
  if ChangeCount = 0 then exit;
  Cancel;
  CheckBrowseMode;
  UpdateCursorPos;

  OldValuesdataSet.Last;
  if OldValuesdataSet.FieldbyName(sfld_Delta).AsInteger = Ord(dtDelete) then
  begin
    BookMarkPositionFromDeleted;
    RestoreDeltaValues(True);
    OldValuesdataSet.Delete;
  end
  else
    if MoveToBookMarkFromDelta then
      Result := RevertRecord;

  if Result then
  begin
    if not FollowChange then
    begin
      Bookmark := Bm;
      CursorPosChanged;
    end;
  end;
end;



function TAstaCustomAuditDataSet.DeleteFromUpdateCache: Boolean;
begin
  if FUpdateMethod = umAfterPost then
    Result := True
  else
    Result := RowHasBeenChanged;
  if Result then FOldValuesDataSet.Delete;
end;

procedure TAstaCustomAuditDataSet.EmptyCache;
begin
  if FOldValuesDataSet <> nil then FOldValuesDataSet.Empty;
end;

procedure TAstaCustomAuditDataSet.Empty;
var
  t: TAstaUpdateMethod;
begin
  t := FUpdateMethod;
  FUpdateMethod := ummanual;
  inherited Empty;
  EmptyCache;
  FUpdateMethod := t;
  EmptyCache;
end;

function TAstaCustomAuditDataSet.DeltaDataSetCurrentValueDataSet: TAstaIODataSet;
var
  bm: TBookmark;
  i: Integer;
  res_BookMark, ods_Delta: TField;
begin
  result := nil;
  if OldValuesDataSet = nil then exit;
  result := TAstaIODataSet.Create(nil);
  result.RetrieveFieldsFromDataSet(Self, False);
  result.AddField(sfld_BookMark, ftInteger, 0);
  result.Open;
  SyncDataSetToPersistantFields(self, result);
  res_BookMark := result.FieldbyName(sfld_BookMark);
  res_BookMark.Visible := FAlse;
  bm := self.bookmark; // AfterPost moved to BEFORE internalpost, and this causes recursive calls
  self.disablecontrols;
  try
  OldValuesDataSet.First;
  ods_Delta := OldValuesDataSet.FieldByName(sfld_Delta);

  while not OldValuesDataSet.Eof do
  begin
    if (ods_Delta.AsInteger = ord(dtdelete)) or MoveToBookmarkFromDelta then
    begin
      result.Append;
      if (ods_Delta.AsInteger <> ord(dtdelete)) then
      begin //don't need values for deletes in the fcuurentvalues dataset
        for i := 0 to fieldcount - 1 do
         if (result.findfield(self.fields[i].FieldName)<>nil) and
          (result.fieldbyname(self.fields[i].fieldname).fieldkind = fkdata) then //sg 03.10.2002
            DBFieldsAssign(result.fieldbyname(self.fields[i].fieldname), self.Fields[i]);
        res_BookMark.AsInteger := self.GetBookMarkAsInteger; //this is the current book mark
      end
      else
      begin
        res_BookMark.AsInteger := 0;
        //No bookMark for Deletes on the current value dataset!!!
      end;
      //on deletes we still need the delta and the bookmark!
      result.Post;
    end;
    self.OldValuesDataSet.Next;
  end;
  if UpdateMethod <> umAfterPost then
    self.bookmark := bm; // AfterPost moved to BEFORE internalpost, and this causes recursive calls
  finally
   self.EnableControls;
  end;
end;

function DBFieldsAreNotEqual(F1, F2: TField): Boolean;
begin
  result := (f1.IsNull xor f2.IsNull) or
    (CompareStr(f1.AsString, f2.AsString) <> 0);
end;

procedure DBFieldsAssign(ADestField, ASrcField: TField);
begin
  if (ADestField.DataSet is TAstaIOCustomDataset) and (ASrcField.DataSet is TAstaIOCustomDataset) then begin
    case ASrcField.DataType of
      ftLargeInt:
        TLargeintField(ADestField).AsLargeInt := TLargeintField(ASrcField).AsLargeInt;
      ftDataSet:
        TAstaIOCustomDataset(ADestField.DataSet).StoreMemoryStream(ADestField,
          TAstaIOCustomDataset(ASrcField.DataSet).GetMemoryStream(ASrcField));
    else
      ADestField.Assign(ASrcField);
    end;
  end
  else
    ADestField.Assign(ASrcField);
end;

// TAstaIODataLink

constructor TAstaIODataLink.Create(ADataSet: TAstaIOCustomDataset);
begin
  inherited Create;
  FDataSet := ADataSet;
end;

destructor TAstaIODataLink.Destroy;
begin
  FDataSet.FDataLink := nil;
  inherited;
end;

procedure TAstaIODataLink.ActiveChanged;
begin
  if FDataSet.IsDesignTime then exit;
  if Active then
  begin
    FDataSet.SourceChanged;
  end
  else
    if not Active then
    begin
      FDataSet.SourceDisabled;
    end;
end;

procedure TAstaIODataLink.CheckBrowseMode;
begin
  if FDataSet.Active then
    FDataSet.CheckBrowseMode;
end;

procedure TAstaIODataLink.RecordChanged(Field: TField);
begin
  if FDataSet.IsDesignTime then exit;
  if (Field = nil) and FDataSet.Active then
    FDataSet.SourceChanged;
end;

procedure TAstaIODataLink.LayoutChanged;
begin
  ActiveChanged;
end;


end.



