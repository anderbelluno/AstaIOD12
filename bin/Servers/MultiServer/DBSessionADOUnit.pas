unit DBSessionADOUnit;

interface

{$I Compiler.inc}

uses
  SysUtils, Classes, IniFiles, DB, AnyCommon, DBAnySessionUnit,
  {$IFDEF D6ANDUP}Variants, {$ELSE} Forms, {$ENDIF}
  AstaIOUserList,AstaIOCustomDataSet,AstaIODBConst, AstaIOProvider,
  AstaIOMetaData, AstaIOServerMethod, AstaIOIBInfo, AstaIOIProvider,
  AstaIODBInfo, AstaIODataSetProvider, AstaIODataBasePlugin,
  ADODB;

type
  TDBPluginADO = class(TDBAnyPlugin)
  private
    { Private declarations }
    procedure DataBasePluginAutoIncrementFetch(Sender: TObject;
      U: TUserRecord; SQLDataSet: TDataSet; DataBaseStr, TableName,
      AutoIncFieldName: String; var AutoIncrementValue: Integer);
    procedure DataBasePluginCreateProviderParamsEvent(Sender: TObject;
      var Params: TParams; DataSet: TDataSet);
    procedure DataBasePluginExecSQL(Sender: TObject; U: TUserRecord;
      SQLDataSet: TComponent; DataBaseStr, SQLString: string;
      ClientParams: TParams; var RowsAffected: Integer);
    procedure DataBasePluginFetchMetaData(Sender: TObject; U: TUserRecord;
      var MetaDataDataSet: TDataSet; DataBaseStr, ObjectName: string;
      MetaDataRequest: TAstaMetaData);
    procedure DataBasePluginSetProviderParams(Sender: TObject;
      U: TUserRecord; DataSet: TDataSet; DataBaseStr, ProviderName: String;
      ClientParams: TParams);
    procedure DataBasePluginSubmitSQL(Sender: TObject; U: TUserRecord;
      SQLDataSet: TDataSet; DataBaseStr, SQLString: string;
      ClientParams: TParams; RowsToReturn: Integer);
    procedure DataBasePluginSupplyDBComponent(Sender: TObject;
      U: TUserRecord; DatabaseString: String; var ADBComponent: TComponent;
      ADBAction: TDbAction; SQLOPtions: TAstaDataSetOptionSet);
    procedure DataBasePluginTransactionBegin(Sender: TObject;
      U: TUserRecord; Transaction: TComponent; DatabaseStr: string);
    procedure DataBasePluginTransactionEnd(Sender: TObject; U: TUserRecord;
      Transaction: TComponent; Success: Boolean; DatabaseStr: string);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
  end;

  TDBSessionADO = class(TDBAnySession)
    StoredProc: TADOStoredProc;
    ExecQuery: TADOQuery;
    Connection: TADOConnection;
    Query: TADOQuery;
    procedure DBInfoSetSQL(Sender: TObject; U: TUserRecord;
      Query: TDataSet; SQLString: String);
    procedure MetaDataTables(Sender: TObject; U: TUserRecord;
      MetaDataRequest: TAstaMetaData; MetaDataSet: TAstaIODataSet;
      DataBaseName, TableName: String);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure ReadSettings; override;

    procedure OpenConnection; override;
    procedure CloseConnection; override;
  end;

implementation

uses AstaIOSessionCollection, AstaIOADOUtils;

{$R *.dfm}

{=== TDBPluginADO =============================================================}

constructor TDBPluginADO.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  { Set the session class }
  FDBSessionClass := TDBSessionADO;
  { Map the events }
  Self.OnAutoIncrementFetch        := DataBasePluginAutoIncrementFetch;
  Self.OnSetProviderParams         := DataBasePluginSetProviderParams;
  Self.OnSubmitSQL                 := DataBasePluginSubmitSQL;
  Self.OnCreateProviderParamsEvent := DataBasePluginCreateProviderParamsEvent;
  Self.OnExecSQL                   := DataBasePluginExecSQL;
  Self.OnSupplyDBComponent         := DataBasePluginSupplyDBComponent;
  Self.OnFetchMetaData             := DataBasePluginFetchMetaData;
  Self.OnTransactionBegin          := DataBasePluginTransactionBegin;
  Self.OnTransactionEnd            := DataBasePluginTransactionEnd;

  with Sessions.Add do
  begin
    SessionName := 'Session1';
    Default     := True;
  end;
end;

procedure TDBPluginADO.DataBasePluginAutoIncrementFetch(Sender: TObject;
  U: TUserRecord; SQLDataSet: TDataSet; DataBaseStr, TableName,
  AutoIncFieldName: String; var AutoIncrementValue: Integer);
begin
  with TADOQuery(SQLDataSet) do
  begin
    Active := False;
{$IFDEF ADO_SQLServer}
    SQL.Text := Format('select @@IDENTITY from %s', [TableName]);
{$ELSE}
    SQL.Text := Format('select Max(%s) from %s', [AutoIncFieldName, TableName]);
{$ENDIF}
    Open;
    if not Eof then AutoIncrementValue := Fields[0].AsInteger;
  end;
end;

type
  TMyADODataSet = class(TCustomADODataSet); // Get the protected Parameters property

procedure TDBPluginADO.DataBasePluginCreateProviderParamsEvent(
  Sender: TObject; var Params: TParams; DataSet: TDataSet);
var
  i: integer;
begin
  Params.Clear;
  if not ((DataSet is TADOQuery) or (DataSet is TADOStoredProc) or (DataSet is TADODataSet)) then Exit;

  with DataSet as TMyADODataSet do
  begin
    for i := 0 to Parameters.Count - 1 do
      Params.CreateParam(Parameters[i].DataType, Parameters[i].Name, AstaADOParamTranslate(Parameters[i]));
  end;
end;

procedure TDBPluginADO.DataBasePluginExecSQL(Sender: TObject;
  U: TUserRecord; SQLDataSet: TComponent; DataBaseStr, SQLString: string;
  ClientParams: TParams; var RowsAffected: Integer);
begin
  LogMessage(SQLString);
  ServerWire.LogParams(ClientParams);
  with TADOQuery(SQLDataSet) do
  begin
    Active := False;
    Parameters.Clear;
    SQL.Text := SQLString;
    Parameters.Assign(ClientParams);
    LogMessage('PARAMS.COUNT=' + IntToStr(Parameters.Count));
    ExecSQL;
  end;
  RowsAffected := TADOQuery(SQLDataSet).RowsAffected;
end;

procedure TDBPluginADO.DataBasePluginFetchMetaData(Sender: TObject;
  U: TUserRecord; var MetaDataDataSet: TDataSet; DataBaseStr,
  ObjectName: string; MetaDataRequest: TAstaMetaData);
begin
  MetaDataDataSet := TDBSessionADO(U.DatabaseSession).MetaData.GetMetaData(Sender, U,
                        MetaDataRequest,
                        DatabaseStr,
                        ObjectName);
end;

procedure TDBPluginADO.DataBasePluginSetProviderParams(Sender: TObject;
  U: TUserRecord; DataSet: TDataSet; DataBaseStr, ProviderName: String;
  ClientParams: TParams);
var
  i: integer;
begin
  with DataSet as TMyADODataSet do
  begin
    for i := 0 to ClientParams.Count - 1 do
      Parameters[i].value := ClientParams[i].Value;
  end;
end;

procedure TDBPluginADO.DataBasePluginSubmitSQL(Sender: TObject;
  U: TUserRecord; SQLDataSet: TDataSet; DataBaseStr, SQLString: string;
  ClientParams: TParams; RowsToReturn: Integer);
var
  i: integer;
begin
  LogMessage(SQLString);
  with TADOQuery(SQLDataSet) do
  begin
    Active := False;
    SQL.Text := SQLString;
    for i := 0 to ClientParams.Count - 1 do
    begin
      Parameters[i].DataType := ClientParams[i].DataType;
      Parameters[i].Value := ClientParams[i].value;
    end;
    Open;
  end;
end;

procedure TDBPluginADO.DataBasePluginSupplyDBComponent(Sender: TObject;
  U: TUserRecord; DatabaseString: String; var ADBComponent: TComponent;
  ADBAction: TDbAction; SQLOPtions: TAstaDataSetOptionSet);
begin
  if (soPackets in SQLOptions) then
  begin
    case AdbAction of
      tdbSelect:
        begin
          ADBComponent:=TAdoQuery.Create(nil);
          TAdoQuery(ADBComponent).Connection := TDBSessionADO(U.DatabaseSession).Connection;
        end;
    end;
  end
  else begin
    case ADBAction of
        tdbSelect, tdbMetaData, tdbServerMethod, tdbCustom, tdbExecSQL:
          ADBComponent := TDBSessionADO(U.DatabaseSession).ExecQuery;
        tdbTransaction:
          ADBComponent := TDBSessionADO(U.DatabaseSession).Connection;
        tdbStoredProc, tdbExecProc:
          ADBComponent := TDBSessionADO(U.DatabaseSession).StoredProc;
        tdbDataModule:
          ADBComponent := U.DatabaseSession;
    end;
  end;
end;

procedure TDBPluginADO.DataBasePluginTransactionBegin(Sender: TObject;
  U: TUserRecord; Transaction: TComponent; DatabaseStr: string);
begin
  LogMessage('Begin Transaction');
  with TADOConnection(Transaction) do
    if not InTransaction then BeginTrans;
end;

procedure TDBPluginADO.DataBasePluginTransactionEnd(Sender: TObject;
  U: TUserRecord; Transaction: TComponent; Success: Boolean;
  DatabaseStr: string);
begin
  LogMessage('End Transaction');
  with TADOConnection(Transaction) do
  begin
    if InTransaction then
    begin
      if Success then
        CommitTrans
      else
        RollBackTrans;
    end;
  end;
end;

{=== TDBSessionADO ============================================================}

procedure TDBSessionADO.ReadSettings;
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
  try
    with Connection do
    begin
      ConnectionString := Ini.ReadString('ADODatabase', 'ConnectionString', '');
      //TODO: Maybey some more settings to read ?
      LoginPrompt := False;
    end;
  finally
    Ini.Free;
  end;
end;

procedure TDBSessionADO.CloseConnection;
begin
  Connection.Connected := False;
end;

procedure TDBSessionADO.OpenConnection;
begin
  Connection.Connected := True;
end;

procedure TDBSessionADO.DBInfoSetSQL(Sender: TObject; U: TUserRecord;
  Query: TDataSet; SQLString: String);
begin
  // This is used to assign the sql to the query that does the select from the system tables for metadata
  TADODataSet(Query).CommandText:=SQLString;
end;

procedure TDBSessionADO.MetaDataTables(Sender: TObject; U: TUserRecord;
  MetaDataRequest: TAstaMetaData; MetaDataSet: TAstaIODataSet;
  DataBaseName, TableName: String);
var
  SL: TStringList;
  i:integer;
begin
  SL := TStringList.Create;
  try
    Connection.GetTableNames(SL,False);
    for i:=0 to SL.Count-1 do
      MetaDataSet.AppendRecord([SL[i]]);
  finally
   SL.Free;
 end;
end;

end.
