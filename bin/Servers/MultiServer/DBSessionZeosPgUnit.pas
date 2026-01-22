unit DBSessionZeosPgUnit;

interface

uses
  SysUtils, Classes, IniFiles, DB, AnyCommon, AnyXMLParser,
  {$IFDEF D6ANDUP}Variants, {$ELSE} Forms, {$ENDIF}
  AstaIOUserList,AstaIOCustomDataSet,AstaIODBConst, AstaIOProvider,
  AstaIOMetaData, AstaIOServerMethod, AstaIOIBInfo, AstaIOIProvider,
  AstaIODBInfo, AstaIODataSetProvider,
  ZQuery, ZPgSqlQuery, ZTransact, ZPgSqlTr, ZConnect, ZPgSqlCon,
  DBAnySessionUnit;

type
  TDBPluginZeosPg = class(TDBAnyPlugin)
  private
    { Private declarations }
    procedure DatabasePluginAutoIncrementFetch(Sender: TObject;
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
      U: TUserRecord; DataSet: TDataSet; DataBaseStr, ProviderName: string;
      ClientParams: TParams);
    procedure DataBasePluginSubmitSQL(Sender: TObject; U: TUserRecord;
      SQLDataSet: TDataSet; DataBaseStr, SQLString: String;
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

  TDBSessionZeosPg = class(TDBAnySession)
    Database: TZPgSqlDatabase;
    Transaction: TZPgSqlTransact;
    Query: TZPgSqlQuery;
    procedure MetaDataDBMSName(Sender: TObject; U: TUserRecord;
      MetaDataSet: TAstaIODataSet);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure ReadSettings; override;

    procedure OpenConnection; override;
    procedure CloseConnection; override;
  end;

implementation

{$R *.dfm}

{=== TDBPluginZeosPg ==========================================================}

constructor TDBPluginZeosPg.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  { Set the session class }
  FDBSessionClass := TDBSessionZeosPg;
  { Map the events }
  Self.OnAutoIncrementFetch := DatabasePluginAutoIncrementFetch;
  Self.OnSetProviderParams := DataBasePluginSetProviderParams;
  Self.OnSubmitSQL := DataBasePluginSubmitSQL;
  Self.OnCreateProviderParamsEvent := DataBasePluginCreateProviderParamsEvent;
  Self.OnExecSQL := DataBasePluginExecSQL;
  Self.OnSupplyDBComponent := DataBasePluginSupplyDBComponent;
  Self.OnFetchMetaData := DataBasePluginFetchMetaData;
  Self.OnTransactionBegin := DataBasePluginTransactionBegin;
  Self.OnTransactionEnd := DataBasePluginTransactionEnd;

  with Sessions.Add do
  begin
    SessionName := 'Session1';
    Default     := True;
  end;
end;

procedure TDBPluginZeosPg.DatabasePluginAutoIncrementFetch(Sender: TObject;
  U: TUserRecord; SQLDataSet: TDataSet; DataBaseStr, TableName,
  AutoIncFieldName: String; var AutoIncrementValue: Integer);
begin
  with TZPgSqlQuery(SQLDataSet) do
  begin
    Active := False;
    SQL.Text := Format('SELECT MAX(%s) FROM "%s";', [AutoIncFieldName, TableName]);
    Open;
    if not EOF then AutoIncrementValue := Fields[0].AsInteger;
  end;
end;

procedure TDBPluginZeosPg.DataBasePluginCreateProviderParamsEvent(
  Sender: TObject; var Params: TParams; DataSet: TDataSet);
begin
  Params.Assign(TZPgSQLQuery(DataSet).Params);
end;

procedure TDBPluginZeosPg.DataBasePluginExecSQL(Sender: TObject;
  U: TUserRecord; SQLDataSet: TComponent; DataBaseStr, SQLString: string;
  ClientParams: TParams; var RowsAffected: Integer);
var
  i: Integer;
begin
  LogMessage(SQLString);
  ServerWire.LogParams(ClientParams);
  with TZPgSQLQuery(SQLDataSet) do
  begin
    Active := False;
    Params.Clear;
    SQL.Text := SQLString;
    for i := 0 to Params.Count - 1 do
    begin
      Params[i].DataType := ClientParams[i].DataType;
      Params[i].Value := ClientParams[i].value;
    end;
    LogMessage('PARAMS.COUNT=' + IntToStr(Params.Count));
    ExecSQL;
  end;
  RowsAffected := TZPgSQLQuery(SQLDataSet).RowsAffected;
end;

procedure TDBPluginZeosPg.DataBasePluginFetchMetaData(Sender: TObject;
  U: TUserRecord; var MetaDataDataSet: TDataSet; DataBaseStr,
  ObjectName: string; MetaDataRequest: TAstaMetaData);
begin
  MetaDataDataSet := TDBSessionZeosPg(u.DatabaseSession).MetaData.GetMetaData(Sender, U,
    MetaDataRequest,
    DatabaseStr,
    ObjectName);
end;

procedure TDBPluginZeosPg.DataBasePluginSetProviderParams(Sender: TObject;
  U: TUserRecord; DataSet: TDataSet; DataBaseStr, ProviderName: string;
  ClientParams: TParams);
begin
  TZPgSQLQuery(DataSet).Params.Assign(ClientParams);
end;

procedure TDBPluginZeosPg.DataBasePluginSubmitSQL(Sender: TObject;
  U: TUserRecord; SQLDataSet: TDataSet; DataBaseStr, SQLString: String;
  ClientParams: TParams; RowsToReturn: Integer);
var
  i: Integer;
begin
  LogMessage(SQLString);
  with TZPgSQLQuery(SQLDataSet) do
  begin
    Active := False;
    SQL.Text := SQLString;
    for i := 0 to Params.Count - 1 do
    begin
      Params[i].DataType := ClientParams[i].DataType;
      Params[i].Value := ClientParams[i].value;
    end;
    Open;
  end;
end;

procedure TDBPluginZeosPg.DataBasePluginSupplyDBComponent(Sender: TObject;
  U: TUserRecord; DatabaseString: String; var ADBComponent: TComponent;
  ADBAction: TDbAction; SQLOPtions: TAstaDataSetOptionSet);
begin
  if (soPackets in SQLOptions) then
  begin
    case AdbAction of
      tdbSelect:
        begin
          ADBComponent := TZPgSQLQuery.Create(nil);
          TZPgSQLquery(ADBComponent).DataBase := TDBSessionZeosPg(U.DatabaseSession).Database;
        end;
    end
  end
  else begin
    case ADBAction of
      tdbSelect, tdbMetaData, tdbServerMethod, tdbCustom, tdbExecSQL:
        ADBComponent := TDBSessionZeosPg(U.DatabaseSession).Query;
      tdbTransaction:
        ADBComponent := TDBSessionZeosPg(U.DatabaseSession).Transaction;
      //tdbStoredProc, tdbExecProc:
      //  ADBComponent := TDBSessionZeosMySQL(U.DatabaseSession).StoredProc;
      tdbDataModule:
        ADBComponent := U.DatabaseSession;
    end;
  end;
end;

procedure TDBPluginZeosPg.DataBasePluginTransactionBegin(Sender: TObject;
  U: TUserRecord; Transaction: TComponent; DatabaseStr: string);
begin
  LogMessage('Begin Transaction');
  try
    TZPgSQLTransact(Transaction).StartTransaction;
  finally
  end;
end;

procedure TDBPluginZeosPg.DataBasePluginTransactionEnd(Sender: TObject;
  U: TUserRecord; Transaction: TComponent; Success: Boolean;
  DatabaseStr: string);
begin
  //LogMessage('End Transaction');
  try
    if Success then
    begin
      LogMessage('Committing Transaction');
      TZPgSQLTransact(Transaction).Commit;
    end
    else begin
      LogMessage('Rolling back Transaction');
      TZPgSQLTransact(Transaction).Rollback;
    end;
  finally
  end;
end;

{=== TDBSessionZeosPg =========================================================}

procedure TDBSessionZeosPg.ReadSettings;
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
  try
    with Database do
    begin
      Port     := Ini.ReadString('PostgreSQL', 'Port', '5432');
      Host     := Ini.ReadString('PostgreSQL', 'Host', 'localhost');
      Database := Ini.ReadString('PostgreSQL', 'Database', 'test');
      Login    := Ini.ReadString('PostgreSQL', 'Username', 'test');
      Password := Ini.ReadString('PostgreSQL', 'Password', '');
    end;
  finally
    Ini.Free;
  end;
end;

procedure TDBSessionZeosPg.OpenConnection;
begin
  Database.Connected := True;
end;

procedure TDBSessionZeosPg.CloseConnection;
begin
  Database.Connected := False;
end;

procedure TDBSessionZeosPg.MetaDataDBMSName(Sender: TObject;
  U: TUserRecord; MetaDataSet: TAstaIODataSet);
begin
  MetaDataSet.AppendRecord([ExtractFileName(ParamStr(0)),
                            ExtractFileName(Database.Database),
                            'Some info',
                            ExtractFileName(ParamStr(0))])
end;


end.
