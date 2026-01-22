unit DBSessionZeosMySQLUnit;

interface

uses
  SysUtils, Classes, IniFiles, DB, AnyCommon, DBAnySessionUnit,
  {$IFDEF D6ANDUP}Variants, {$ELSE} Forms, {$ENDIF}
  AstaIOUserList,AstaIOCustomDataSet,AstaIODBConst, AstaIOProvider,
  AstaIOMetaData, AstaIOServerMethod, AstaIOIBInfo, AstaIOIProvider,
  AstaIODBInfo, AstaIODataSetProvider, AstaIODataBasePlugin,
  ZQuery, ZMySqlQuery, ZTransact, ZMySqlTr, ZConnect, ZMySqlCon;

type
  TDBPluginZeosMySQL = class(TDBAnyPlugin)
  private
    { Private declarations }
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

  TDBSessionZeosMySQL = class(TDBAnySession)
    Database: TZMySqlDatabase;
    Transact: TZMySqlTransact;
    Query: TZMySqlQuery;
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

{=== TDBPluginIBX =============================================================}

constructor TDBPluginZeosMySQL.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  { Set the session class }
  FDBSessionClass := TDBSessionZeosMySQL;
  { Map the events }
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

procedure TDBPluginZeosMySQL.DataBasePluginCreateProviderParamsEvent(
  Sender: TObject; var Params: TParams; DataSet: TDataSet);
begin
  Params.Assign(TZMySQLQuery(DataSet).Params);
end;

procedure TDBPluginZeosMySQL.DataBasePluginExecSQL(Sender: TObject;
  U: TUserRecord; SQLDataSet: TComponent; DataBaseStr, SQLString: string;
  ClientParams: TParams; var RowsAffected: Integer);
var
  i: Integer;
begin
  LogMessage(SQLString);
  ServerWire.LogParams(ClientParams);
  with TZMySQLQuery(SQLDataSet) do
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
  RowsAffected := TZMySQLQuery(SQLDataSet).RowsAffected;
end;

procedure TDBPluginZeosMySQL.DataBasePluginFetchMetaData(Sender: TObject;
  U: TUserRecord; var MetaDataDataSet: TDataSet; DataBaseStr,
  ObjectName: string; MetaDataRequest: TAstaMetaData);
begin
  MetaDataDataSet := TDBSessionZeosMySQL(u.DatabaseSession).MetaData.GetMetaData(Sender, U,
    MetaDataRequest,
    DatabaseStr,
    ObjectName);
end;

procedure TDBPluginZeosMySQL.DataBasePluginSetProviderParams(
  Sender: TObject; U: TUserRecord; DataSet: TDataSet; DataBaseStr,
  ProviderName: string; ClientParams: TParams);
begin
  TZMySQLQuery(DataSet).Params.Assign(ClientParams);
end;

procedure TDBPluginZeosMySQL.DataBasePluginSubmitSQL(Sender: TObject;
  U: TUserRecord; SQLDataSet: TDataSet; DataBaseStr, SQLString: String;
  ClientParams: TParams; RowsToReturn: Integer);
var
  i: Integer;
begin
  LogMessage(SQLString);
  with TZMySQLQuery(SQLDataSet) do
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

procedure TDBPluginZeosMySQL.DataBasePluginSupplyDBComponent(
  Sender: TObject; U: TUserRecord; DatabaseString: String;
  var ADBComponent: TComponent; ADBAction: TDbAction;
  SQLOPtions: TAstaDataSetOptionSet);
begin
  if (soPackets in SQLOptions) then
  begin
    case AdbAction of
      tdbSelect:
        begin
          ADBComponent:=TZMySQLQuery.Create(nil);
          TZMySQLquery(ADBComponent).DataBase := TDBSessionZeosMySQL(U.DatabaseSession).Database;
        end;
    end
  end
  else begin
    case ADBAction of
      tdbSelect, tdbMetaData, tdbServerMethod, tdbCustom, tdbExecSQL:
        ADBComponent := TDBSessionZeosMySQL(U.DatabaseSession).Query;
      tdbTransaction:
        ADBComponent := TDBSessionZeosMySQL(U.DatabaseSession).Transact; //Database; {Transaction ????}
      //tdbStoredProc, tdbExecProc:
      //  ADBComponent := TDBSessionZeosMySQL(U.DatabaseSession).StoredProc;
      tdbDataModule:
        ADBComponent := U.DatabaseSession;
    end;
  end;
end;

procedure TDBPluginZeosMySQL.DataBasePluginTransactionBegin(
  Sender: TObject; U: TUserRecord; Transaction: TComponent;
  DatabaseStr: string);
begin
  LogMessage('Begin Transaction');
  try
    TZMySQLTransact(Transaction).StartTransaction;
  finally
  end;
{
  with TDataBaseection(Transaction) do
    if InTransaction then
    begin
      TD.TransactionID := 1;
      TD.IsolationLevel := xilREADCOMMITTED;
      StartTransaction(TD);
     end;
}
end;

procedure TDBPluginZeosMySQL.DataBasePluginTransactionEnd(Sender: TObject;
  U: TUserRecord; Transaction: TComponent; Success: Boolean;
  DatabaseStr: string);
begin
  //LogMessage('End Transaction');
  try
    if Success then
    begin
      LogMessage('Committing Transaction');
      TZMySQLTransact(Transaction).Commit;
    end
    else begin
      LogMessage('Rolling back Transaction');
      TZMySQLTransact(Transaction).Rollback;
    end;
  finally
  end;
(*
  with TDataBaseection(Transaction) do
    if InTransaction then
    begin
      TD.TransactionID := 1;
      TD.IsolationLevel := xilREADCOMMITTED;
      if Success then
        Commit(TD)
      else
        RollBack(TD);
  end;
*)
end;

{=== TDBSessionZeosMySQL ======================================================}

procedure TDBSessionZeosMySQL.ReadSettings;
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
  try
    with Database do
    begin
      Port     := Ini.ReadString('MySQL', 'Port', '3306');
      Host     := Ini.ReadString('MySQL', 'Host', 'localhost');
      Database := Ini.ReadString('MySQL', 'Database', 'test');
      Login    := Ini.ReadString('MySQL', 'Username', 'test');
      Password := Ini.ReadString('MySQL', 'Password', '');
    end;
  finally
    Ini.Free;
  end;
end;

procedure TDBSessionZeosMySQL.MetaDataDBMSName(Sender: TObject;
  U: TUserRecord; MetaDataSet: TAstaIODataSet);
begin
  MetaDataSet.AppendRecord([ExtractFileName(ParamStr(0)),
                            ExtractFileName(Database.Database),
                            'Some info',
                            ExtractFileName(ParamStr(0))])
end;

procedure TDBSessionZeosMySQL.CloseConnection;
begin
  Database.Connected := False;
end;

procedure TDBSessionZeosMySQL.OpenConnection;
begin
  Database.Connected := True;
end;


end.
