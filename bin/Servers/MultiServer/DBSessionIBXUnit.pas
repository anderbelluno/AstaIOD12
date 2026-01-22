unit DBSessionIBXUnit;

interface

{$I Compiler.inc}

uses
  SysUtils, Classes, IniFiles, DB, AnyCommon, DBAnySessionUnit,
  {$IFDEF D6ANDUP}Variants, {$ELSE} Forms, {$ENDIF}
  AstaIOUserList,AstaIOCustomDataSet,AstaIODBConst, AstaIOProvider,
  AstaIOMetaData, AstaIOServerMethod, AstaIOIBInfo, AstaIOIProvider,
  AstaIODBInfo, AstaIODataSetProvider, AstaIODataBasePlugin,
  IBDatabase, IBStoredProc, IBCustomDataSet, IBQuery, IBTable;

type
  TDBPluginIBX = class(TDBAnyPlugin)
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

  TDBSessionIBX = class(TDBAnySession)
    Query: TIBQuery;
    StoredProc: TIBStoredProc;
    Database: TIBDatabase;
    Transaction: TIBTransaction;
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

constructor TDBPluginIBX.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  { Set the session class }
  FDBSessionClass := TDBSessionIBX;
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

procedure TDBPluginIBX.DatabasePluginAutoIncrementFetch(Sender: TObject;
  U: TUserRecord; SQLDataSet: TDataSet; DataBaseStr, TableName,
  AutoIncFieldName: String; var AutoIncrementValue: Integer);
begin
  with TIBQuery(SQLDataSet) do
  begin
    Active := False;
    SQL.Text := Format('SELECT MAX(%s) FROM "%s";', [AutoIncFieldName, TableName]);
    Open;
    if not EOF then AutoIncrementValue := Fields[0].AsInteger;
  end;
end;

procedure TDBPluginIBX.DatabasePluginCreateProviderParamsEvent(
  Sender: TObject; var Params: TParams; DataSet: TDataSet);
begin
  Params.Assign(TIBQuery(DataSet).Params);
end;

procedure TDBPluginIBX.DatabasePluginExecSQL(Sender: TObject;
  U: TUserRecord; SQLDataSet: TComponent; DataBaseStr, SQLString: String;
  ClientParams: TParams; var RowsAffected: Integer);
begin
  LogMessage(SQLString);
  ServerWire.LogParams(ClientParams);
  TIBQuery(SQLDataSet).ExecSQL;
  RowsAffected := TIBQuery(SQLDataSet).RowsAffected;
  //returning -1 ????
end;

procedure TDBPluginIBX.DatabasePluginFetchMetaData(Sender: TObject;
  U: TUserRecord; var MetaDataDataSet: TDataSet; DataBaseStr,
  ObjectName: String; MetaDataRequest: TAstaMetaData);
begin
  MetaDataDataSet := TDBSessionIBX(U.DatabaseSession).MetaData.GetMetaData(Sender, U,
                        MetaDataRequest,
                        DatabaseStr,
                        ObjectName);
end;

procedure TDBPluginIBX.DatabasePluginSetProviderParams(Sender: TObject;
  U: TUserRecord; DataSet: TDataSet; DataBaseStr, ProviderName: String;
  ClientParams: TParams);
begin
  TIBQuery(DataSet).Params.Assign(ClientParams);
end;

procedure TDBPluginIBX.DatabasePluginSetSQLParamsEvent(Sender: TObject;
  U: TUserRecord; SQLString: String; Query: TComponent;
  ADBAction: TDbAction; ParamList: TParams);
var
  i:integer;
begin
  with TIBQuery(Query) do
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

procedure TDBPluginIBX.DatabasePluginSupplyDBComponent(Sender: TObject;
  U: TUserRecord; DatabaseString: String; var ADBComponent: TComponent;
  ADBAction: TDbAction; SQLOPtions: TAstaDataSetOptionSet);
begin
  if (soPackets in SQLOptions) then
  begin
    case AdbAction of
      tdbSelect:
        begin
          ADBComponent := TIBQuery.Create(nil);
          TIBQuery(ADBComponent).DataBase := TDBSessionIBX(U.DatabaseSession).Database;
          TIBQuery(ADBComponent).Transaction := TDBSessionIBX(U.DatabaseSession).Transaction;
        end;
    end;
  end
  else begin
    case ADBAction of
        tdbSelect, tdbMetaData, tdbServerMethod, tdbCustom, tdbExecSQL:
          ADBComponent := TDBSessionIBX(U.DatabaseSession).Query;
        tdbTransaction:
          ADBComponent := TDBSessionIBX(U.DatabaseSession).Transaction;
        tdbStoredProc, tdbExecProc:
          ADBComponent := TDBSessionIBX(U.DatabaseSession).StoredProc;
        tdbDataModule:
          ADBComponent := U.DatabaseSession;
    end;
  end;
end;

procedure TDBPluginIBX.DatabasePluginTransactionBegin(Sender: TObject;
  U: TUserRecord; Transaction: TComponent; DatabaseStr: String);
begin
  LogMessage('Begin Transaction');
  with TIBTransaction(Transaction) do
    if not InTransaction then StartTransaction;
end;

procedure TDBPluginIBX.DatabasePluginTransactionEnd(Sender: TObject;
  U: TUserRecord; Transaction: TComponent; Success: Boolean;
  DatabaseStr: String);
begin
  LogMessage('Ending Transaction...');
  with TIBTransaction(Transaction) do
  begin
    if not InTransaction then
    begin
      LogMessage('No Active Transaction !!');
      exit;
    end;

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


procedure TDBSessionIBX.ReadSettings;
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
  try
    with Database do
    begin
      DatabaseName := Ini.ReadString('Interbase', 'Host', '127.0.0.1') + ':' +
                      Ini.ReadString('Interbase', 'DataFile', '');
      Params.Add(Format('user_name=%s', [Ini.ReadString('Interbase', 'Username', 'SYSDBA')]));
      Params.Add(Format('password=%s', [Ini.ReadString('Interbase', 'Password', 'masterkey')]));
      SQLDialect := Ini.ReadInteger('Interbase', 'SQLDialect', 3);
      LoginPrompt := False;
    end;
  finally
    Ini.Free;
  end;
end;

procedure TDBSessionIBX.CloseConnection;
begin
  Database.Connected := True;
end;

procedure TDBSessionIBX.OpenConnection;
begin
  Database.Connected := False;
end;

procedure TDBSessionIBX.DBInfoSetSQL(Sender: TObject; U: TUserRecord;
  Query: TDataSet; SQLString: String);
begin
  // This is used to assign the sql to the query that does the select from the system tables for metadata
  TIBQuery(Query).SQL.Text:=SQLString;
end;

procedure TDBSessionIBX.MetaDataDBMSName(Sender: TObject; U: TUserRecord;
  MetaDataSet: TAstaIODataSet);
begin
  MetaDataSet.AppendRecord([ExtractFileName(ParamStr(0)),
                            ExtractFileName(Database.DatabaseName),
                            'Some info',
                            ExtractFileName(ParamStr(0))])
end;



end.
