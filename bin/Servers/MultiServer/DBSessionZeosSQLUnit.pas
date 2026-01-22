unit DBSessionZeosSQLUnit;

interface
{$I Compiler.inc}

uses
  SysUtils, Classes, IniFiles, DB, AnyCommon, AnyXMLParser,
  {$IFDEF D6ANDUP}Variants, {$ELSE} Forms, {$ENDIF}
  AstaIOUserList,AstaIOCustomDataSet,AstaIODBConst, AstaIOProvider,
  AstaIOMetaData, AstaIOServerMethod, AstaIOIBInfo, AstaIOIProvider,
  AstaIODBInfo, AstaIODataSetProvider,
  ZStoredProc, ZMsSqlStoredProc, ZQuery, ZMsSqlQuery, ZTransact, ZMsSqlTr,
  ZConnect, ZMsSqlCon, DBAnySessionUnit;

type
  TDBPluginZeosSQL = class(TDBAnyPlugin)
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
    procedure DatabasePluginSetSQLParamsEvent(Sender: TObject;
      U: TUserRecord; SQLString: String; Query: TComponent;
      ADBAction: TDbAction; ParamList: TParams);
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

  TDBSessionZeosSQL = class(TDBAnySession)
    Database: TZMsSqlDatabase;
    Transaction: TZMsSqlTransact;
    Query: TZMsSqlQuery;
    ExecQuery: TZMsSqlQuery;
    StoredProc: TZMsSqlStoredProc;
    procedure DBInfoSetSQL(Sender: TObject; U: TUserRecord;
      Query: TDataSet; SQLString: String);
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

{=== TDBPluginZeosSQL =========================================================}

constructor TDBPluginZeosSQL.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  { Set the session class }
  FDBSessionClass := TDBSessionZeosSQL;
  { Map the events }
  Self.OnAutoIncrementFetch        := DataBasePluginAutoIncrementFetch;
  Self.OnSetProviderParams         := DataBasePluginSetProviderParams;
//  Self.OnSubmitSQL                 := DataBasePluginSubmitSQL;
  Self.OnSetSQLParamsEvent         := DatabasePluginSetSQLParamsEvent;
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

procedure TDBPluginZeosSQL.DataBasePluginAutoIncrementFetch(
  Sender: TObject; U: TUserRecord; SQLDataSet: TDataSet; DataBaseStr,
  TableName, AutoIncFieldName: String; var AutoIncrementValue: Integer);
begin
  with TZMsSqlQuery(SQLDataSet) do
  begin
    Active := False;
    SQL.Text := Format('select @@IDENTITY from %s', [TableName]);
    Open;
    if not Eof then AutoIncrementValue := Fields[0].AsInteger;
  end;
end;

procedure TDBPluginZeosSQL.DataBasePluginCreateProviderParamsEvent(
  Sender: TObject; var Params: TParams; DataSet: TDataSet);
begin
  if DataSet is TZCustomMsSqlDataset then
    Params.Assign(TZCustomMsSqlDataset(DataSet).Params)
  else if DataSet is TZCustomMsSqlStoredProc then
    Params.Assign(TZCustomMsSqlStoredProc(DataSet).Params);
end;

procedure TDBPluginZeosSQL.DataBasePluginExecSQL(Sender: TObject;
  U: TUserRecord; SQLDataSet: TComponent; DataBaseStr, SQLString: string;
  ClientParams: TParams; var RowsAffected: Integer);
begin
  LogMessage(SQLString);
  ServerWire.LogParams(ClientParams);
  TZMsSqlQuery(SQLDataSet).ExecSQL;
  RowsAffected := TZMsSqlQuery(SQLDataSet).RowsAffected;
end;

procedure TDBPluginZeosSQL.DataBasePluginFetchMetaData(Sender: TObject;
  U: TUserRecord; var MetaDataDataSet: TDataSet; DataBaseStr,
  ObjectName: string; MetaDataRequest: TAstaMetaData);
begin
  MetaDataDataSet := TDBSessionZeosSQL(U.DatabaseSession).MetaData.GetMetaData(Sender, U,
                        MetaDataRequest,
                        DatabaseStr,
                        ObjectName);
end;

procedure TDBPluginZeosSQL.DataBasePluginSetProviderParams(Sender: TObject;
  U: TUserRecord; DataSet: TDataSet; DataBaseStr, ProviderName: String;
  ClientParams: TParams);
begin
  TZMsSqlQuery(DataSet).Params.Assign(ClientParams);
end;

procedure TDBPluginZeosSQL.DatabasePluginSetSQLParamsEvent(Sender: TObject;
  U: TUserRecord; SQLString: String; Query: TComponent;
  ADBAction: TDbAction; ParamList: TParams);
var
  i:integer;
begin
  with TZMsSqlQuery(Query) do
  begin
    Active := False;
    SQL.Text := SQLString;
    //Prepare;
    for i := 0 to ParamList.Count - 1 do
    begin
      if ParamList[i].IsNull then
      begin
        Params[i].DataType := ParamList[i].DataType;
        Params[i].Clear;
      end
      else begin
        case ParamList[i].DataType of
          ftString:
            Params[i].AsString := ParamList[i].AsString;
          ftInteger, ftSmallInt, ftword:
            Params[i].AsInteger := ParamList[i].AsInteger;
          ftLargeInt:
            Params[i].AsFloat := ParamList[i].AsFloat;
          ftFloat,ftCurrency:
            Params[i].AsFloat := ParamList[i].AsFloat;
          ftBoolean:
            Params[i].AsBoolean := ParamList[i].AsBoolean;
          ftTime, ftDate, ftDateTime:
            Params[i].AsDateTime := ParamList[i].AsDateTime;
          ftMemo, ftBlob:
            Params[i].AsString := ParamList[i].AsString;
          ftBCD:
            Params[i].AsBCD := ParamList[i].AsBCD;
          else
            Params[i].Value := ParamList[i].Value;
        end;
      end;
    end;
  end;
end;


procedure TDBPluginZeosSQL.DataBasePluginSupplyDBComponent(Sender: TObject;
  U: TUserRecord; DatabaseString: String; var ADBComponent: TComponent;
  ADBAction: TDbAction; SQLOPtions: TAstaDataSetOptionSet);
begin
  if (soPackets in SQLOptions) then
  begin
    case AdbAction of
      tdbSelect:
        begin
          ADBComponent:=TZMsSQLQuery.Create(nil);
          TZMsSQLQuery(ADBComponent).Database := TDBSessionZeosSQL(U.DatabaseSession).Database;
        end;
    end;
  end
  else begin
    case ADBAction of
        tdbSelect, tdbMetaData, tdbServerMethod, tdbCustom, tdbExecSQL:
          ADBComponent := TDBSessionZeosSQL(U.DatabaseSession).Query;
        tdbTransaction:
          ADBComponent := TDBSessionZeosSQL(U.DatabaseSession).Transaction;
        tdbStoredProc, tdbExecProc:
          ADBComponent := TDBSessionZeosSQL(U.DatabaseSession).StoredProc;
        tdbDataModule:
          ADBComponent := U.DatabaseSession;
    end;
  end;
end;

procedure TDBPluginZeosSQL.DataBasePluginTransactionBegin(Sender: TObject;
  U: TUserRecord; Transaction: TComponent; DatabaseStr: string);
begin
  LogMessage('Begin Transaction');
  with TZMsSqlTransact(Transaction) do
    StartTransaction;
end;

procedure TDBPluginZeosSQL.DataBasePluginTransactionEnd(Sender: TObject;
  U: TUserRecord; Transaction: TComponent; Success: Boolean;
  DatabaseStr: string);
begin
  //LogMessage('Ending Transaction...');
  with TZMsSqlTransact(Transaction) do
  begin
    if Success then
    begin
      LogMessage('Committing Transaction');
      Commit;
    end
    else begin
      LogMessage('Rolling back Transaction');
      RollBack;
    end;
  end;
end;

{=== TDBSessionZeosSQL ========================================================}

procedure TDBSessionZeosSQL.ReadSettings;
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
  try
    with Database do
    begin
      Host := Ini.ReadString('SQLServer', 'Host', '127.0.0.1');
      Database := Ini.ReadString('SQLServer', 'Database', '');
      Login := Ini.ReadString('SQLServer', 'Username', 'SA');
      Password := Ini.ReadString('SQLServer', 'Password', '');
      LoginPrompt := False;
    end;
  finally
    Ini.Free;
  end;
end;

procedure TDBSessionZeosSQL.CloseConnection;
begin
  Database.Connected := False;
end;

procedure TDBSessionZeosSQL.OpenConnection;
begin
  Database.Connected := True;
end;


procedure TDBSessionZeosSQL.DBInfoSetSQL(Sender: TObject; U: TUserRecord;
  Query: TDataSet; SQLString: String);
begin
  // This is used to assign the sql to the query that does the select from the system tables for metadata
  TZMsSqlQuery(Query).SQL.Text := SQLString;
end;

procedure TDBSessionZeosSQL.MetaDataDBMSName(Sender: TObject;
  U: TUserRecord; MetaDataSet: TAstaIODataSet);
begin
  MetaDataSet.AppendRecord([ExtractFileName(ParamStr(0)),
                            ExtractFileName(Database.Database),
                            'Some info',
                            ExtractFileName(ParamStr(0))])
end;

end.
