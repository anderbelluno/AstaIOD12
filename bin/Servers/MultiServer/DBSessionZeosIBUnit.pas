unit DBSessionZeosIBUnit;

interface

{$I Compiler.inc}

uses
  SysUtils, Classes, IniFiles, DB, AnyCommon, AnyXMLParser,
  {$IFDEF D6ANDUP}Variants, {$ELSE} Forms, {$ENDIF}
  AstaIOUserList,AstaIOCustomDataSet,AstaIODBConst, AstaIOProvider,
  AstaIOMetaData, AstaIOServerMethod, AstaIOIBInfo, AstaIOIProvider,
  AstaIODBInfo, AstaIODataSetProvider,
  ZIbSqlQuery, ZQuery, ZTransact,ZIbSqlTr, ZConnect, ZIbSqlCon,
  DBAnySessionUnit;

type
  TDBPluginZeosIB = class(TDBAnyPlugin)
  private
    { Private declarations }
    procedure DatabasePluginAutoIncrementFetch(Sender: TObject;
      U: TUserRecord; SQLDataSet: TDataSet; DataBaseStr, TableName,
      AutoIncFieldName: String; var AutoIncrementValue: Integer);
    procedure DatabasePluginCreateProviderParamsEvent(Sender: TObject;
      var Params: TParams; DataSet: TDataSet);
    procedure DatabasePluginExecSQL(Sender: TObject; U: TUserRecord;
      SQLDataSet: TComponent; DataBaseStr, SQLString: String;
      ClientParams: TParams; var RowsAffected: Integer);
    procedure DatabasePluginFetchMetaData(Sender: TObject; U: TUserRecord;
      var MetaDataDataSet: TDataSet; DataBaseStr, ObjectName: String;
      MetaDataRequest: TAstaMetaData);
    procedure DatabasePluginSetProviderParams(Sender: TObject;
      U: TUserRecord; DataSet: TDataSet; DataBaseStr, ProviderName: String;
      ClientParams: TParams);
    procedure DatabasePluginSetSQLParamsEvent(Sender: TObject;
      U: TUserRecord; SQLString: String; Query: TComponent;
      ADBAction: TDbAction; ParamList: TParams);
    procedure DatabasePluginSupplyDBComponent(Sender: TObject;
      U: TUserRecord; DatabaseString: String; var ADBComponent: TComponent;
      ADBAction: TDbAction; SQLOPtions: TAstaDataSetOptionSet);
    procedure DatabasePluginTransactionBegin(Sender: TObject;
      U: TUserRecord; Transaction: TComponent; DatabaseStr: String);
    procedure DatabasePluginTransactionEnd(Sender: TObject; U: TUserRecord;
      Transaction: TComponent; Success: Boolean; DatabaseStr: String);
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
  end;

  TDBSessionZeosIB = class(TDBAnySession)
    Database: TZIbSqlDatabase;
    Transaction: TZIbSqlTransact;
    Query: TZIbSqlQuery;
    StoredProc: TZIbSqlStoredProc;
    procedure DBInfoSetSQL(Sender: TObject; U: TUserRecord;
      Query: TDataSet; SQLString: String);
    procedure MetaDataDBMSName(Sender: TObject; U: TUserRecord;
      MetaDataSet: TAstaIODataSet);
  private
    { Private declarations }
  protected
    { Protected declarations }
  public
    { Public declarations }
    procedure ReadSettings; override;

    procedure OpenConnection; override;
    procedure CloseConnection; override;
  end;

implementation

uses AstaIOUtil;

{$R *.dfm}

{=== TDBPlugin code ===========================================================}

constructor TDBPluginZeosIB.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  { Set the session class }
  FDBSessionClass := TDBSessionZeosIB;
  { Map the events }
  Self.OnSetProviderParams := DatabasePluginSetProviderParams;
  Self.OnAutoIncrementFetch := DatabasePluginAutoIncrementFetch;
  Self.OnCreateProviderParamsEvent := DatabasePluginCreateProviderParamsEvent;
  Self.OnExecSQL := DatabasePluginExecSQL;
  Self.OnSupplyDBComponent := DatabasePluginSupplyDBComponent;
  Self.OnFetchMetaData := DatabasePluginFetchMetaData;
  Self.OnTransactionBegin := DatabasePluginTransactionBegin;
  Self.OnTransactionEnd := DatabasePluginTransactionEnd;
  Self.OnSetSQLParamsEvent := DatabasePluginSetSQLParamsEvent;

  with Sessions.Add do
  begin
    SessionName := 'Session1';
    Default     := True;
  end;
end;

procedure TDBPluginZeosIB.DatabasePluginAutoIncrementFetch(Sender: TObject;
  U: TUserRecord; SQLDataSet: TDataSet; DataBaseStr, TableName,
  AutoIncFieldName: String; var AutoIncrementValue: Integer);
begin
  with TZIbSqlQuery(SQLDataSet) do
  begin
    Active := False;
    SQL.Text := Format('SELECT MAX(%s) FROM "%s";', [AutoIncFieldName, TableName]);
    Open;
    if not EOF then AutoIncrementValue := Fields[0].AsInteger;
  end;
end;

procedure TDBPluginZeosIB.DatabasePluginCreateProviderParamsEvent(
  Sender: TObject; var Params: TParams; DataSet: TDataSet);
begin
  Params.Assign(TZIbSqlQuery(DataSet).Params);
end;

procedure TDBPluginZeosIB.DatabasePluginExecSQL(Sender: TObject;
  U: TUserRecord; SQLDataSet: TComponent; DataBaseStr, SQLString: String;
  ClientParams: TParams; var RowsAffected: Integer);
begin
  LogMessage(SQLString);
  ServerWire.LogParams(ClientParams);
  TZIbSqlQuery(SQLDataSet).ExecSQL;
  RowsAffected := TZIbSqlQuery(SQLDataSet).RowsAffected;
  //returning -1 ????
end;

procedure TDBPluginZeosIB.DatabasePluginFetchMetaData(Sender: TObject;
  U: TUserRecord; var MetaDataDataSet: TDataSet; DataBaseStr,
  ObjectName: String; MetaDataRequest: TAstaMetaData);
begin
  MetaDataDataSet := TDBSessionZeosIB(U.DatabaseSession).MetaData.GetMetaData(Sender, U,
                        MetaDataRequest,
                        DatabaseStr,
                        ObjectName);
end;

procedure TDBPluginZeosIB.DatabasePluginSetProviderParams(Sender: TObject;
  U: TUserRecord; DataSet: TDataSet; DataBaseStr, ProviderName: String;
  ClientParams: TParams);
begin
  TZIbSqlQuery(DataSet).Params.Assign(ClientParams);
end;

procedure TDBPluginZeosIB.DatabasePluginSetSQLParamsEvent(Sender: TObject;
  U: TUserRecord; SQLString: String; Query: TComponent;
  ADBAction: TDbAction; ParamList: TParams);
var
  i:integer;
begin
  with TZIbSqlQuery(Query) do
  begin
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

procedure TDBPluginZeosIB.DatabasePluginSupplyDBComponent(Sender: TObject;
  U: TUserRecord; DatabaseString: String; var ADBComponent: TComponent;
  ADBAction: TDbAction; SQLOPtions: TAstaDataSetOptionSet);
begin
  if (soPackets in SQLOptions) then
  begin
    case AdbAction of
      tdbSelect:
        begin
          ADBComponent := TZIbSqlQuery.Create(nil);
          TZIbSqlQuery(ADBComponent).DataBase := TDBSessionZeosIB(U.DatabaseSession).Database;
          TZIbSqlQuery(ADBComponent).Transaction := TDBSessionZeosIB(U.DatabaseSession).Transaction;
        end;
    end;
  end
  else begin
    case ADBAction of
        tdbSelect, tdbMetaData, tdbServerMethod, tdbCustom, tdbExecSQL:
          ADBComponent := TDBSessionZeosIB(U.DatabaseSession).Query;
        tdbTransaction:
          ADBComponent := TDBSessionZeosIB(U.DatabaseSession).Transaction;
        tdbStoredProc, tdbExecProc:
          ADBComponent := TDBSessionZeosIB(U.DatabaseSession).StoredProc;
        tdbDataModule:
          ADBComponent := U.DatabaseSession;
    end;
  end;
end;

procedure TDBPluginZeosIB.DatabasePluginTransactionBegin(Sender: TObject;
  U: TUserRecord; Transaction: TComponent; DatabaseStr: String);
begin
  LogMessage('Begin Transaction');
  with TZIbSqlTransact(Transaction) do
    StartTransaction;
end;

procedure TDBPluginZeosIB.DatabasePluginTransactionEnd(Sender: TObject;
  U: TUserRecord; Transaction: TComponent; Success: Boolean;
  DatabaseStr: String);
begin
  //LogMessage('Ending Transaction...');
  with TZIbSqlTransact(Transaction) do
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
  end
end;

{=== TDBSession code ==========================================================}

procedure TDBSessionZeosIB.ReadSettings;
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
  try
    with Database do
    begin
      Host := Ini.ReadString('Interbase', 'Host', '127.0.0.1');
      Database := Ini.ReadString('Interbase', 'DataFile', '');
      Login := Ini.ReadString('Interbase', 'Username', 'SYSDBA');
      Password := Ini.ReadString('Interbase', 'Password', 'masterkey');
      SQLDialect := Ini.ReadInteger('Interbase', 'SQLDialect', 3);
      LoginPrompt := False;
    end;
  finally
    Ini.Free;
  end;
end;

procedure TDBSessionZeosIB.DBInfoSetSQL(Sender: TObject; U: TUserRecord;
  Query: TDataSet; SQLString: String);
begin
  // This is used to assign the sql to the query that does the select from the system tables for metadata
  TZIbSqlQuery(Query).SQL.Text:=SQLString;
end;

procedure TDBSessionZeosIB.MetaDataDBMSName(Sender: TObject; U: TUserRecord;
  MetaDataSet: TAstaIODataSet);
begin
  MetaDataSet.AppendRecord([ExtractFileName(ParamStr(0)),
                            ExtractFileName(Database.Database),
                            'Some info',
                            ExtractFileName(ParamStr(0))])
end;


procedure TDBSessionZeosIB.CloseConnection;
begin
  Database.Connected := True;
end;

procedure TDBSessionZeosIB.OpenConnection;
begin
  Database.Connected := False;
end;

end.
