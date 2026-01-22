{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10387: daASTAIO.pas 
{
{   Rev 1.0    4/10/2003 6:32:48 AM  Steve
}
{******************************************************************************}
{               Report Builder (c) DADE plugin support for ASTAIO (c)            }
{******************************************************************************}

unit daASTAIO;

interface

{$I ppIfDef.pas}

uses Classes, SysUtils, Forms, ExtCtrls, DB, scktcomp,
     ppClass, ppComm, ppDBPipe, ppDB, ppClasUt, ppTypes,
     daDB, daQueryDataView, daDataView, daPreviewDataDlg,
     daQuery, daDataVw,
     AstaIOClientRemoteDataSet, AstaIOClientWire, AstaIODBConst;

{
  ASTAIO DataView Classes:

     1.  ASTAIO TDataSet descendants
           - TDataSets that can be children of a DataView.
           - Override the HasParent method of TComponent to return True
           - Must be registerd with the Delphi IDE using the RegisterNoIcon procedure

       a. TdaChildAstaIOQuery - TAstaIOClientQuery descendant that can be a child of a DataView
       b. TdaChildAstaIOTable - TAstaIOClientQuery descendant that can be a child of a DataView
       c. TdaChildAstaIOStoredProc - TADOStoredProc descendant that can be a child of a DataView

     2.  TdaAstaIOSession
           - descendant of TdaSession
           - implements GetDatabaseNames, GetTableNamess, etc.

     3.  TdaAstaIODataSet
          - descendant of TdaDataSet
          - implements GetFieldNames for SQL

     4.  TdaAstaIOQueryDataView
          - descendant of TdaQueryDataView
          - uses the above classes to create the required Query -> DataSource -> Pipeline -> Report connection
          - uses the SQL built by the QueryWizard to assign SQL to the TQuery etc.
}

const

  SECS_PER_DAY         = 11.5741e-6;
  SOCKET_OPEN_TIMEOUT  = 5;  // Time outs (in seconds), use larger numbers for networks with more latency
  DATASET_OPEN_TIMEOUT = 5;

type

  EAstaDadeError = Exception;

  { TdaChildAstaIOQuery }

  TdaChildAstaIOQuery = class(TAstaIOClientQuery)
  public
    function HasParent: Boolean; override;
  end;

  { TdaChildAstaIOTable }

  TdaChildAstaIOTable = class(TAstaIOClientTable)
  public
    function HasParent: Boolean; override;
  end;

  { TdaChildAstaIOStoredProc }

  TdaChildAstaIOStoredProc = class(TAstaIOClientStoredProc)
  public
    function HasParent: Boolean; override;
  end;

  { TdaAstaIOSession }

  TdaAstaIOSession = class(TdaSession)
  private
    procedure AddDatabase(aDatabase: TComponent);
  protected
    procedure SetDataOwner(aDataOwner: TComponent); override;
  public
    function GetAliasDriverName(const Alias: string): string;
    procedure GetDatabaseNames(aList: TStrings); override;
    function GetDatabaseType(const aDatabaseName: String): TppDatabaseType; override;
    procedure GetTableNames(const aDatabaseName: string; List: TStrings); override;
    function ValidDatabaseTypes: TppDatabaseTypes; override;

    class function ClassDescription: string; override;
    class function DataSetClass: TdaDataSetClass; override;
    class function DatabaseClass: TComponentClass; override;
  end;

  { TdaAstaIODataSet }

  TdaAstaIODataSet = class(TdaDataSet)
  private
    FQuery: TAstaIOClientQuery;
    function GetQuery: TAstaIOClientQuery;
  protected
    procedure BuildFieldList; override;
    function  GetActive: Boolean; override;
    procedure SetActive(Value: Boolean); override;
    procedure SetDatabase(aDatabase: TComponent); Override;
    procedure SetDataName(const aDataName: string); override;
    property Query: TAstaIOClientQuery read GetQuery;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    class function ClassDescription: string; override;
    procedure GetFieldNamesForSQL(aList: TStrings; aSQL: TStrings); override;
    procedure GetFieldsForSQL(aList: TList; aSQL: TStrings); override;
  end;

  TdaAstaIOQueryDataView = class(TdaQueryDataView)
  private
    FDataSource: TppChildDataSource;
    FQuery: TdaChildAstaIOQuery;
  protected
    procedure SQLChanged; override;

    procedure Loaded; override;
    class procedure DefaultClientWireDisconnectHandler(Sender: TObject); //Socket: TAstaIOClientWire);
    class procedure DefaultClientWireConnectHandler(Sender: TObject); // Socket: TAstaIOClientWire);
    class procedure DefaultClientWireErrorHandler(Sender: TObject; ErrorMsg: string; var ErrorCode:Integer); // (Sender: TObject; Socket: TAstaIOClientWire; ErrorEvent: TErrorEvent; var ErrorCode: Integer);
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;

    class function PreviewFormClass: TFormClass; override;
    class function SessionClass: TClass; override;

    procedure Init; override;
    procedure ConnectPipelinesToData; override;
  published
    property DataSource: TppChildDataSource read FDataSource;
  end;

procedure CloseDataset(const Dataset: TAstaClientRemoteDataSet);
procedure OpenDataset(const Dataset: TAstaClientRemoteDataSet);
procedure OpenSocket(const Socket: TAstaIOClientWire);

function daGetDefaultClientWire: TAstaIOClientWire;
function daGetClientWireByName(const aDatabaseName: string; const DesignTime: boolean): TAstaIOClientWire;
function daGetClientWiresList: TppComponentList;
function daGetTokenByName(Source: string; const Index: integer; const seperator: char):string;

procedure Register;

implementation

type

  TClientWireState = (ssClosed, ssOpen, ssError);

var

  FClientWiresList: TppComponentList;
  FClientWire:      TAstaIOClientWire;
  FClientWireState: TClientWireState;

const

  cDefaultDatabase  = 'DefaultAstaIODatabase';

procedure Register;
begin
  RegisterNoIcon([TdaChildAstaIOQuery]);
  RegisterNoIcon([TdaChildAstaIOTable]);
  RegisterNoIcon([TdaAstaIOQueryDataView]);
end;

function TdaChildAstaIOQuery.HasParent: Boolean;
begin
  Result := True;
end;

function TdaChildAstaIOTable.HasParent: Boolean;
begin
  Result := True;
end;

{ TdaChildAstaIOStoredProc }

function TdaChildAstaIOStoredProc.HasParent: Boolean;
begin
  Result := True;
end;

{******************************************************************************}
{*                                                                            *}
{*                            TdaAstaIOSession                                  *}
{*                                                                            *}
{******************************************************************************}

procedure TdaAstaIOSession.AddDatabase(aDatabase: TComponent);
begin
  if daGetClientWiresList.IndexOf(aDatabase) < 0 then
    FClientWiresList.Add(aDatabase);
end;

procedure TdaAstaIOSession.SetDataOwner(aDataOwner: TComponent);
var lList: TStringList;
begin
  inherited SetDataOwner(aDataOwner);
  lList := TStringList.Create;
  GetDatabaseNames(lList);
  lList.Free;
end;

function TdaAstaIOSession.GetAliasDriverName(const Alias: string): string;
begin
  Result := '';
end;

procedure TdaAstaIOSession.GetDatabaseNames(aList: TStrings);
var liIndex: Integer;
begin
//  daGetADOConnectionNames(aList);
  daGetDatabaseObjectsFromOwner(TdaSessionClass(Self.ClassType), aList, DataOwner);
  for liIndex := 0 to aList.Count-1 do
    if aList.Objects[liIndex] <> nil then
      AddDatabase(TComponent(aList.Objects[liIndex]));
end;

function TdaAstaIOSession.GetDatabaseType(const aDatabaseName: String): TppDatabaseType;
begin
  Result := dtOther;
end;

procedure TdaAstaIOSession.GetTableNames(const aDatabaseName: string; List: TStrings);
var MetaData:   TAstaIOMetaDataDataSet;
    ClientWire: TAstaIOClientWire;
begin
  ClientWire := daGetClientWireByName(aDatabaseName, ((csDesigning in ComponentState) or (csLoading in ComponentState)));
  MetaData   := TAstaIOMetaDataDataSet.Create(nil);
  try
    MetaData.AstaClientWire  := ClientWire;
    MetaData.DataBase        := ClientWire.Name;
    MetaData.MetaDataRequest := mdTables;
    OpenSocket(ClientWire);
    OpenDataset(MetaData);

    while not MetaData.EOF do begin
      List.Add(MetaData.Fields[0].AsString);
      MetaData.Next;
    end;
  finally
//    if Assigned(FClientWire) then
//      FClientWire.Active := False;
    MetaData.Free;
  end;
end;

function TdaAstaIOSession.ValidDatabaseTypes: TppDatabaseTypes;
begin
  Result := [dtOther];
end;

class function TdaAstaIOSession.ClassDescription: string;
begin
  Result := 'AstaIOSession';
end;

class function TdaAstaIOSession.DataSetClass: TdaDataSetClass;
begin
  Result := TdaAstaIODataSet;
end;

class function TdaAstaIOSession.DatabaseClass: TComponentClass;
begin
  Result := TAstaIOClientWire;
end;

{******************************************************************************}
{*                                                                            *}
{*                            TdaAstaIODataSet                                  *}
{*                                                                            *}
{******************************************************************************}

constructor TdaAstaIODataSet.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FQuery := nil;
end;

destructor TdaAstaIODataSet.Destroy;
begin
  if Assigned(FQuery) then FQuery.Free;
  inherited Destroy;
end;

class function TdaAstaIODataSet.ClassDescription: string;
begin
  Result := 'AstaIODataSet';
end;

function TdaAstaIODataSet.GetActive: Boolean;
begin
  Result := GetQuery.Active
end;

procedure TdaAstaIODataSet.SetActive(Value: Boolean);
var pQuery: TAstaIOClientQuery;
begin
  pQuery := GetQuery;
  try
    if Value then
      begin
        if not pQuery.AstaClientWire.Active then
          OpenSocket(pQuery.AstaClientWire);
        OpenDataset(pQuery);
      end
    else
      pQuery.Close;
  except
    raise EAstaDadeError.Create('Failure when opening metadata table '+Query.UpdateTableName+'.');
  end;
end;

function TdaAstaIODataSet.GetQuery: TAstaIOClientQuery;
begin
  if not Assigned(FQuery) then
    begin
      FQuery := TAstaIOClientQuery.Create(Self);
    end;
  Result := FQuery;
end;

procedure TdaAstaIODataSet.SetDatabase(aDatabase: TComponent);
begin
  inherited SetDatabase(aDatabase);
  {table cannot be active to set database property}
  if GetQuery.Active then
    FQuery.Active := False;

  FQuery.AstaClientWire := daGetClientWireByName(aDatabase.Name, ((csDesigning in ComponentState) or (csLoading in ComponentState)));
end;

procedure TdaAstaIODataSet.SetDataName(const aDataName: string);
begin
  inherited SetDataName(aDataName);
  if GetQuery.Active then
    FQuery.Active := False;
  FQuery.SQL.Clear;
  FQuery.SQL.Text := 'SELECT * FROM '+aDataName+' Where ''v''<>''v''';
end;

procedure TdaAstaIODataSet.BuildFieldList;
var lField: TppField;
    liIndex: Integer;
    lQueryField: TField;
begin
  inherited BuildFieldList;
  for liIndex := 0 to FQuery.FieldCount - 1 do
    begin
      lQueryField      := FQuery.Fields[liIndex];
      lField           := TppField.Create(nil);
      lField.TableName := DataName;
      lField.FieldName := lQueryField.FieldName;
      lField.DataType  := ppConvertFieldType(lQueryField.DataType);
      AddField(lField);
    end;
end;

procedure TdaAstaIODataSet.GetFieldNamesForSQL(aList: TStrings; aSQL: TStrings);
var pQuery: TAstaIOClientQuery;
begin
  aList.Clear;
  pQuery := TAstaIOClientQuery.Create(Self);
  try
    pQuery.AstaClientWire := daGetClientWireByName(Database.Name, ((csDesigning in ComponentState) or (csLoading in ComponentState)));
    OpenSocket(Query.AstaClientWire);
    pQuery.SQL := aSQL;
    pQuery.Active := True;
    pQuery.GetFieldNames(aList);
  finally
    pQuery.Free;
  end;
end;

procedure TdaAstaIODataSet.GetFieldsForSQL(aList: TList; aSQL: TStrings);
var pQuery:  TAstaIOClientQuery;
    TheField: TField;
    ppField:  TppField;
    i:        integer;
begin
  // create a temporary dataset which will be used to retrieve a result set
  pQuery := TAstaIOClientQuery.Create(Self);
  try
    pQuery.AstaClientWire := daGetClientWireByName(Database.Name, ((csDesigning in ComponentState) or (csLoading in ComponentState)));
    pQuery.SQL := aSQL;
    if not pQuery.AstaClientWire.Active then
      OpenSocket(pQuery.AstaClientWire);

    // we got a connection so load List with the fields returned in the result set
    if pQuery.AstaClientWire.Active and pQuery.Active then
      for i := 0 to pQuery.FieldCount - 1 do
        begin
          TheField          := pQuery.Fields[i];
          ppField           := TppField.Create(nil);
          ppField.FieldName := TheField.FieldName;
          ppField.DataType  := ppConvertFieldType(TheField.DataType);
          aList.Add(ppField);
        end;
  finally
    pQuery.Free;
  end;
end;


{******************************************************************************}
{*                                                                            *}
{*                            TdaAstaIOQueryDataView                            *}
{*                                                                            *}
{*   notes: 1. must use ChildQuery, ChildDataSource, ChildPipeline etc.       *}
{*          2. use Self as owner for Query, DataSource etc.                   *}
{*          3. do NOT assign a Name                                           *}
{*                                                                            *}
{******************************************************************************}

constructor TdaAstaIOQueryDataView.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FQuery              := TdaChildAstaIOQuery.Create(Self);
  FQuery.Options      := [soFetchMemos, soFetchBlobs];
  FDataSource         := TppChildDataSource.Create(Self);
  FDataSource.DataSet := FQuery;
end;

destructor TdaAstaIOQueryDataView.Destroy;
begin
  if Assigned(FDataSource) then FDataSource.Free;
  if Assigned(FQuery) then FQuery.Free;
  inherited Destroy;
end;

class procedure TdaAstaIOQueryDataView.DefaultClientWireConnectHandler(Sender: TObject); // Socket: TAstaIOClientWire);
begin
  FClientWireState := ssOpen;
end;

class procedure TdaAstaIOQueryDataView.DefaultClientWireDisconnectHandler(Sender: TObject); // Socket: TAstaIOClientWire);
begin
  FClientWireState := ssClosed;
end;

procedure TdaAstaIOQueryDataView.Loaded;
begin
  inherited Loaded;
  SQLChanged;
end;

procedure TdaAstaIOQueryDataView.ConnectPipelinesToData;
begin
  if DataPipelineCount > 0 then
    TppDBPipeline(DataPipelines[0]).DataSource := FDataSource;
end;

class function TdaAstaIOQueryDataView.PreviewFormClass: TFormClass;
begin
  Result := TFormClass(GetClass('TdaPreviewDataDialog'));
end;

class function TdaAstaIOQueryDataView.SessionClass: TClass;
begin
  Result := TdaAstaIOSession;
end;

procedure TdaAstaIOQueryDataView.Init;
var lDataPipeline: TppChildDBPipeline;
begin
  inherited Init;
  if DataPipelineCount = 0 then
    begin
      lDataPipeline                  := TppChildDBPipeline(ppComponentCreate(Self, TppChildDBPipeline));
      lDataPipeline.DataSource       := FDataSource;
      lDataPipeline.AutoCreateFields := False;
      lDataPipeline.DataView         := Self;
    end;
end;

procedure TdaAstaIOQueryDataView.SQLChanged;
begin
  if FQuery.Active then CloseDataset(FQuery);
  FQuery.AstaClientWire := daGetClientWireByName(SQL.DatabaseName, ((csDesigning in ComponentState) or (csLoading in ComponentState)));
  if not FQuery.AstaClientWire.Active then OpenSocket(FQuery.AstaClientWire);
  if not FQuery.Active then
    begin
      FQuery.SQL := SQL.MagicSQLText;
      OpenDataset(FQuery);
    end;
//  FQuery.AstaClientWire.Active := False;
end;

{******************************************************************************}
{*                                                                            *}
{*                      Global procedures and functions                       *}
{*                                                                            *}
{******************************************************************************}

procedure CloseDataset(const Dataset: TAstaClientRemoteDataSet);
var Start : TDateTime;
begin
  Start          := Now;
  Dataset.Active := False;
  while (Dataset.Active) and ((Now-Start) <= DATASET_OPEN_TIMEOUT*SECS_PER_DAY) do
    Application.ProcessMessages;
end;

procedure OpenDataset(const Dataset: TAstaClientRemoteDataSet);
var Start : TDateTime;
begin
  Start          := Now;
  Dataset.Active := True;
  while (not Dataset.Active) and ((Now-Start) <= DATASET_OPEN_TIMEOUT*SECS_PER_DAY) do
    Application.ProcessMessages;
end;

procedure OpenSocket(const Socket: TAstaIOClientWire);
var Start : TDateTime;
begin
  Start := Now;
  if not Socket.Active then Socket.Active := True;
  while (not Socket.Active) and ((Now-Start) <= SOCKET_OPEN_TIMEOUT*SECS_PER_DAY) do
    Application.ProcessMessages;
end;

function daGetClientWireByName(const aDatabaseName: string; const DesignTime: boolean): TAstaIOClientWire;
var SocketName: string;
    liIndex:    integer;
    Addr:       string;
    PortStr:    string;
    PortNum:    integer;
    PortCode:   integer;
begin
  Result     := nil;
  liIndex    := 0;
  SocketName := Trim(daGetTokenByName(aDatabaseName,1,':'));
  Addr       := Trim(daGetTokenByName(aDatabaseName,2,':'));
  PortStr    := Trim(daGetTokenByName(aDatabaseName,3,':'));
  Val(PortStr, PortNum, PortCode);

  // check for a socket object whose name matches SocketName.
  // if SocketName is empty then take the first object in the list
  // (unless the list is empty, or course)
  while (Result = nil) and (liIndex < daGetClientWiresList.Count) do
    begin
      if (SocketName = '') or(AnsiCompareStr(FClientWiresList[liIndex].Name, SocketName) = 0) then
        Result :=  TAstaIOClientWire(FClientWiresList[liIndex]);
      Inc(liIndex);
    end;

  // here's the tricky bit:
  // if we're doing this at design time we can't use any socket
  // which was found on a form or what ever. In this case we must
  // create one from scratch and then fix it up with various properties
  // taken from the one which was found on the form or data module

  if not Assigned(Result) then
    Result := daGetDefaultClientWire;

  if not Assigned(Result.OnDisconnect) then
    Result.OnDisconnect := TdaAstaIOQueryDataView.DefaultClientWireDisconnectHandler;

  if Addr <> '' then
    Result.Address := Addr;

  if PortCode = 0 then
    Result.Port := PortNum;
end;

function daGetDefaultClientWire: TAstaIOClientWire;
begin
  if not Assigned(FClientWire) then
    begin
      FClientWire              := TAstaIOClientWire.Create(nil);
      FClientWire.OnDisconnect := TdaAstaIOQueryDataView.DefaultClientWireDisconnectHandler;
      FClientWire.OnConnect    := TdaAstaIOQueryDataView.DefaultClientWireConnectHandler;
      FClientWire.OnError      := TdaAstaIOQueryDataView.DefaultClientWireErrorHandler;
    end;
  Result := FClientWire;
end;

function daGetTokenByName(Source: string; const Index: integer; const seperator: char):string;
var p1: integer;
    p2: integer;
    i:  integer;
begin
  Result := '';
  if index < 1 then Exit;
  if Length(Source) < index then Exit;
  // the requested token lies between ith-1 and the ith seperators.
  // find the ith-1 seperator
  // unless i is 1 in which case we start at the 1st character of source
  if index > 1 then
    begin
      i := 0;
      while (i < index-1) and (Source <> '') do
        begin
          p1 := Pos(Seperator, Source);
          if p1 = 0 then p1 := Length(Source);
          // found one, or ran off the end looking.
          Delete(Source, 1, p1);
          Inc(i);
        end;
    end;

  // now find the next one and we're done
  p2 := Pos(Seperator, Source);
  if p2 > 1 then
    Result := Copy(Source,1,p2-1)
  else if p2 = 0 then
    Result := Source
  else
    Result := '';
end;

function daGetClientWiresList: TppComponentList;
begin
  if (FClientWiresList = nil) then
    FClientWiresList := TppComponentList.Create(nil);
  Result := FClientWiresList;
end;

class procedure TdaAstaIOQueryDataView.DefaultClientWireErrorHandler(Sender: TObject; ErrorMsg: string; var ErrorCode:Integer); //(Sender: TObject; Socket: TAstaIOClientWire; ErrorEvent: TErrorEvent; var ErrorCode: Integer);
begin
  FClientWireState := ssError;
end;

{******************************************************************************}
{*                                                                            *}
{*                      Initialization/Finalization                           *}
{*                                                                            *}
{******************************************************************************}

initialization
  RegisterClasses([TdaChildAstaIOQuery, TdaChildAstaIOTable]);
  daRegisterSession(TdaAstaIOSession);
  daRegisterDataSet(TdaAstaIODataSet);
  daRegisterDataView(TdaAstaIOQueryDataView);

  FClientWire    := nil;
  FClientWireState := ssClosed;

finalization
  if Assigned(FClientWire) then FClientWire.Free;
  UnRegisterClasses([TdaChildAstaIOQuery, TdaChildAstaIOTable]);
  daUnRegisterSession(TdaAstaIOSession);
  daUnRegisterDataSet(TdaAstaIODataSet);
  daUnRegisterDataView(TdaAstaIOQueryDataView);


{$IFDEF COMPILE_THIS}
procedure daGetAstaDatabaseNames(const List: TStrings; const Owner:TComponent);
var ClientSocket: TAstaIOClientWire;
    Dataset:      TAstaIOClientQuery;
begin

  // let's get the small details out of the way before we start in
  // with the sockets and datasets.
  GetAddress(Owner);

  // create a socket...
  ClientSocket := TAstaIOClientWire.Create(Owner);
  try
    // and get it set up.
    ClientSocket.Address := AstaAddress;
    ClientSocket.Port := AstaPort;
    OpenSocket(ClientSocket);

    // if that succeeded then
    // create a dataset...

    Dataset := TAstaIOClientQuery.Create(Owner);
    try
      // and get it set up.
      DataSet.AstaClientSocket := ClientSocket;
      Dataset.MetaDataRequest := mdDBMSName;
      OpenDataset(Dataset);

      DataSet.First;
      while not DataSet.EOF do
      begin
        if length(trim(DataSet.FieldByName('Database').AsString)) > 0 then
        begin
          List.Add(DataSet.FieldByName('Database').AsString);
        end;
        DataSet.Next;
      end;
    finally
      Dataset.Free;
    end;
  finally
    ClientSocket.Free;
  end;
end;

{$ENDIF}

end.
