unit DBSessionDbIsamUnit;

interface

{$I Compiler.inc}

uses
  SysUtils, Classes, IniFiles, DB, AnyCommon, DBAnySessionUnit,
  {$IFDEF D6ANDUP}Variants, {$ELSE} Forms, {$ENDIF}
  AstaIOUserList,AstaIOCustomDataSet,AstaIODBConst, AstaIOProvider,
  AstaIOMetaData, AstaIOServerMethod, AstaIOIBInfo, AstaIOIProvider,
  AstaIODBInfo, AstaIODataSetProvider, AstaIODataBasePlugin, DBISAMTb;

type
  TDBPluginDbIsam = class(TDBAnyPlugin)
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
    procedure AstaIODataBasePluginSubmitSQL(Sender: TObject;
      U: TUserRecord; SQLDataSet: TDataSet; DataBaseStr, SQLString: String;
      ClientParams: TParams; RowsToReturn: Integer);
    procedure AstaIODataBasePluginSupplyDBComponent(Sender: TObject;
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

  TDBSessionDbIsam = class(TDBAnySession)
    Database: TDBISAMDatabase;
    Session1: TDBISAMSession;
    Query: TDBISAMQuery;
    procedure MetaDataDBMSName(Sender: TObject; U: TUserRecord;
      MetaDataSet: TAstaIODataSet);
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

uses AstaIOUtil;

{$R *.dfm}

{=== TDBPlugin code ===========================================================}

constructor TDBPluginDbIsam.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  { Set the session class }
  FDBSessionClass := TDBSessionDbIsam;
  { Map the events }
  Self.OnSetProviderParams := DataBasePluginSetProviderParams;
  Self.OnSetIProviderParams := DataBasePluginSetProviderParams;
  Self.OnSetServerMethodParams := DataBasePluginSetProviderParams;
  Self.OnSubmitSQL := AstaIODataBasePluginSubmitSQL;
  Self.OnCreateProviderParamsEvent := DataBasePluginCreateProviderParamsEvent;
  Self.OnExecSQL := DataBasePluginExecSQL;
  Self.OnSupplyDBComponent := AstaIODataBasePluginSupplyDBComponent;
  Self.OnFetchMetaData := DataBasePluginFetchMetaData;
  Self.OnTransactionBegin := DataBasePluginTransactionBegin;
  Self.OnTransactionEnd := DataBasePluginTransactionEnd;

  with Sessions.Add do
  begin
    SessionName := 'Session1';
    Default     := True;
  end;
end;

procedure TDBPluginDbIsam.DataBasePluginCreateProviderParamsEvent(
  Sender: TObject; var Params: TParams; DataSet: TDataSet);
var
  i: integer;
begin
{$IFDEF D6ANDUP}
  for i := 0 to TDBISAMQuery(DataSet).Params.Count-1 do
    Params.CreateParam(TDBISAMQuery(DataSet).Params[i].DataType, TDBISAMQuery(DataSet).Params[i].Name, TParamtype(TDBISAMQuery(DataSet).Params[i].ParamType));
{$ENDIF}
end;

procedure TDBPluginDbIsam.DataBasePluginExecSQL(Sender: TObject;
  U: TUserRecord; SQLDataSet: TComponent; DataBaseStr, SQLString: string;
  ClientParams: TParams; var RowsAffected: Integer);
var
  i: Integer;
begin
  LogMessage(SQLString);
  ServerWire.LogParams(ClientParams);
  with TDBISAMQuery(SQLDataSet) do
  begin
    Active := False;
    Params.Clear;
    SQL.Text := SQLString;
    LogMessage('PARAMS.COUNT=' + IntToStr(Params.Count));
    for i := 0 to ClientParams.Count - 1 do
    begin
      if ClientParams[i].IsNull then
      begin
        Params[i].DataType := ClientParams[i].DataType;
        Params[i].Clear;
        Params[i].Bound := True;
      end
      else begin
        case ClientParams[i].DataType of
          ftString:
            Params[i].AsString := ClientParams[i].AsString;
          ftInteger, ftSmallint, ftWord:
            Params[i].AsInteger := ClientParams[i].AsInteger;
          ftFloat, ftCurrency:
            Params[i].AsFloat := ClientParams[i].AsFloat;
          ftBoolean:
            Params[i].AsBoolean := ClientParams[i].AsBoolean;
          ftTime:
            Params[i].AsTime := ClientParams[i].AsTime;
          ftDate, ftDateTime:
            Params[i].AsDateTime:= ClientParams[i].AsDateTime;
          ftBlob, ftGraphic, ftTypedBinary:
            Params[i].AsBlob := ClientParams[i].AsString;
          ftMemo, ftFmtMemo:
            Params[i].AsMemo := ClientParams[i].AsString;
        end;
      end;
    end;
    ExecSQL;
  end;
  RowsAffected := TDBISAMQuery(SQLDataSet).RowsAffected;
end;

procedure TDBPluginDbIsam.DataBasePluginFetchMetaData(Sender: TObject;
  U: TUserRecord; var MetaDataDataSet: TDataSet; DataBaseStr,
  ObjectName: string; MetaDataRequest: TAstaMetaData);
begin
  MetaDataDataSet := TDBSessionDbIsam(u.DatabaseSession).MetaData.GetMetaData(Sender, U,
    MetaDataRequest,
    DatabaseStr,
    ObjectName);
end;

procedure TDBPluginDbIsam.DataBasePluginSetProviderParams(Sender: TObject;
  U: TUserRecord; DataSet: TDataSet; DataBaseStr, ProviderName: string;
  ClientParams: TParams);
var
  i: integer;
begin
{$IFDEF D6ANDUP}
  for i := 0 to ClientParams.Count-1 do
    TDBISAMQuery(DataSet).Params.CreateParam(ClientParams[i].DataType, ClientParams[i].Name, ClientParams[i].Paramtype);
{$ENDIF}
end;

procedure TDBPluginDbIsam.AstaIODataBasePluginSubmitSQL(Sender: TObject;
  U: TUserRecord; SQLDataSet: TDataSet; DataBaseStr, SQLString: String;
  ClientParams: TParams; RowsToReturn: Integer);
var
  i:Integer;
begin
  LogMessage(SQLString);
  with TDBisamQuery(SQLDataSet) do
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

procedure TDBPluginDbIsam.AstaIODataBasePluginSupplyDBComponent(
  Sender: TObject; U: TUserRecord; DatabaseString: String;
  var ADBComponent: TComponent; ADBAction: TDbAction;
  SQLOPtions: TAstaDataSetOptionSet);
begin
  case ADBAction of
    tdbSelect, tdbMetaData, tdbServerMethod, tdbCustom, tdbExecSQL:
      ADBComponent := TDBSessionDbIsam(U.DatabaseSession).Query;
    tdbTransaction:
      ADBComponent := TDBSessionDbIsam(U.DatabaseSession).Database;
    tdbDataModule:
      ADBComponent := U.DatabaseSession;
  end;
end;

procedure TDBPluginDbIsam.DataBasePluginTransactionBegin(Sender: TObject;
  U: TUserRecord; Transaction: TComponent; DatabaseStr: string);
begin
  with TDBISAMDatabase(Transaction) do
    if not InTransaction then StartTransaction;
end;

procedure TDBPluginDbIsam.DataBasePluginTransactionEnd(Sender: TObject;
  U: TUserRecord; Transaction: TComponent; Success: Boolean;
  DatabaseStr: string);
begin
  LogMessage('End Transaction');
  with TDBISAMDatabase(Transaction) do
  begin
    if not InTransaction then Exit;
    if Success then
      Commit
    else
      RollBack;
  end;
end;

{=== TDBSession code ==========================================================}

procedure TDBSessionDbIsam.ReadSettings;
var
  i: integer;
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(ChangeFileExt(ParamStr(0), '.ini'));
  try
    with Database do
    begin
      DatabaseName := Ini.ReadString('DBIsam', 'DatabaseName', 'MyDatabase');
      Directory    := Ini.ReadString('DBIsam', 'Directory', ExtractFilePath(ParamStr(0)));
      if Ini.ReadString('DBIsam', 'DBPassword', '') <> '' then
        Session1.AddPassword(Ini.ReadString('DBIsam', 'DBPassword', ''));

      for i := 0 to ComponentCount - 1 do
      begin
        if Components[i] is TDBISAMQuery then
          TDBISAMQuery(Components[i]).DatabaseName := Database.DatabaseName
        else if Components[i] is TDBISAMTable then
          TDBISAMTable(Components[i]).DatabaseName := Database.DatabaseName;

      end;
      //TODO: Some more settings ?
    end;
  finally
    Ini.Free;
  end;
end;

procedure TDBSessionDbIsam.CloseConnection;
begin
  Database.Connected := False;
end;

procedure TDBSessionDbIsam.OpenConnection;
begin
  Database.Connected := True;
end;


procedure TDBSessionDbIsam.MetaDataDBMSName(Sender: TObject;
  U: TUserRecord; MetaDataSet: TAstaIODataSet);
begin
  MetaDataSet.AppendRecord(['AstaIODbisamServer',
                            'Database',
                            ExtractFileName(ParamStr(0)),
                            'Test'
                            ]);
end;

procedure TDBSessionDbIsam.MetaDataTables(Sender: TObject; U: TUserRecord;
  MetaDataRequest: TAstaMetaData; MetaDataSet: TAstaIODataSet;
  DataBaseName, TableName: String);
var
  i: integer;
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    Session1.GetTableNames(Database.DataBaseName, SL);
    for i := 0 to SL.count-1 do
    begin
      if Pos('.', SL[i]) > 0 then
        MetaDataSet.Appendrecord([StringBeforeToken(SL[i],'.')])
      else
        MetaDataSet.Appendrecord([SL[i]]);
    end;
  finally
    SL.Free;
  end;
end;

end.
