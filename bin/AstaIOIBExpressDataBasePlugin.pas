{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10165: AstaIOIBExpressDataBasePlugin.pas 
{
{   Rev 1.0    4/10/2003 6:31:02 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:37:22 PM  Steve    Version: 1.505
}
unit AstaIOIBExpressDataBasePlugin;

interface
uses Classes,db,
     AstaIODataBasePlugin,
     AstaIODBConst,
     AstaIOUserList,
     AstaIOMetaData,
     AstaIOParamList,
     IBDatabase, IBStoredProc, IBCustomDataSet, IBQuery;

Type
 TAstaIOIBExpressDataBasePlugin=Class(TAstaIODataBasePlugin)

 public
  procedure DataBasePluginSupplyDBComponent(Sender: TObject;
   U: TUserRecord; var ADBComponent: TComponent; ADBAction: TDbAction;
   SQLOptions: TAstaDataSetOptionSet);
  procedure DataBasePluginFetchMetaData(Sender: TObject;
  U: TUserRecord; var MetaDataDataSet: TDataSet; DataBaseStr,
  ObjectName: string; MetaDataRequest: TAstaMetaData);
  procedure DataBasePluginExecSQL(Sender: TObject; U: TUserRecord;
  SQLDataSet: TComponent; DataBaseStr, SQLString: string;
  ClientParams: TParams; var RowsAffected: Integer);
  procedure DataBasePluginTransactionBegin(Sender: TObject;
  U: TUserRecord; Transaction: TComponent; DatabaseStr: string);
  procedure DataBasePluginTransactionEnd(Sender: TObject;
  U: TUserRecord; Transaction: TComponent; Success: Boolean;
  DatabaseStr: string);
  procedure DataBasePluginSetProviderParams(Sender: TObject;
  U: TUserRecord; DataSet: TDataSet; DataBaseStr, ProviderName: string;
  ClientParams: TParams);
  procedure DataBasePluginCreateProviderParamsEvent(Sender: TObject;
  var Params: TParams; DataSet: TDataSet);
  procedure DataBasePluginSubmitSQL(Sender: TObject; U: TUserRecord;
  SQLDataSet: TDataSet; DataBaseStr, SQLString: String;
  ClientParams: TParams; RowsToReturn: Integer);
 end;
implementation

procedure TAstaIOIBExpressDataBasePlugin.DataBasePluginSupplyDBComponent(Sender: TObject;
  U: TUserRecord; var ADBComponent: TComponent; ADBAction: TDbAction;
  SQLOptions: TAstaDataSetOptionSet);
begin
{  if (sopackets in SQLOptions) then begin
   case AdbAction of
    tdbSelect:begin
                ADBComponent:=TIBQuery.Create(nil);
                TIBquery(ADBComponent).DataBase:=TAstaDataModule(U.DatabaseSession).Database as TIBDataBase;
                TIbquery(ADBComponent).Transaction:=TAstaDataModule(U.DatabaseSession).Transaction as TIBTransaction;
               end;

  end

  end else
  case ADBAction of
      tdbSelect,
      tdbMetaData,
      tdbServerMethod,
      tdbCustom,
      tdbExecSQL: AdbComponent := TAstaDataModule(U.DatabaseSession).Query;
      tdbTransaction: AdbComponent := TAstaDataModule(U.DatabaseSession).Transaction;
      tdbStoredProc,
      tdbExecProc: AdbComponent := TAstaDataModule(U.DatabaseSession).StoredProc;
      tdbDataModule: AdbComponent := U.DatabaseSession;
  end;
 }
end;


procedure TAstaIOIBExpressDataBasePlugin.DataBasePluginFetchMetaData(Sender: TObject;
  U: TUserRecord; var MetaDataDataSet: TDataSet; DataBaseStr,
  ObjectName: string; MetaDataRequest: TAstaMetaData);
begin
{  MetaDataDataSet := TAstaDataModule(u.DatabaseSession).MetaData.GetMetaData(Sender, U,
    MetaDataRequest,
    DatabaseStr,
    ObjectName); }

end;

procedure TAstaIOIBExpressDataBasePlugin.DataBasePluginExecSQL(Sender: TObject; U: TUserRecord;
  SQLDataSet: TComponent; DataBaseStr, SQLString: string;
  ClientParams: TParams; var RowsAffected: Integer);
var
  i: Integer;
begin
  with TIBQuery(SQLDataSet) do
  begin
    Close;
    Params.Clear;
    SQL.Text := SQLString;
    for i := 0 to Params.Count - 1 do
    begin
      Params[i].DataType := ClientParams[i].DataType;
      Params[i].Value := ClientParams[i].value;
    end;
    Prepare;
    ExecSQL;
    UnPrepare;
  end;
  RowsAffected := TIBQuery(SQLDataSet).RowsAffected;
  //returning -1 ????
end;



procedure TAstaIOIBExpressDataBasePlugin.DataBasePluginTransactionBegin(Sender: TObject;
  U: TUserRecord; Transaction: TComponent; DatabaseStr: string);
begin
  with TIBTransaction(Transaction) do
    if not InTransaction then StartTransaction;
end;

procedure TAstaIOIBExpressDataBasePlugin.DataBasePluginTransactionEnd(Sender: TObject;
  U: TUserRecord; Transaction: TComponent; Success: Boolean;
  DatabaseStr: string);
begin
  with TIBTransaction(Transaction) do
  begin
    if not InTransaction then exit;
    if Success then Commit
    else RollBack;
  end;
end;

procedure TAstaIOIBExpressDataBasePlugin.DataBasePluginSetProviderParams(Sender: TObject;
  U: TUserRecord; DataSet: TDataSet; DataBaseStr, ProviderName: string;
  ClientParams: TParams);
begin
  TIBQuery(DataSet).Params.Assign(ClientParams);
end;

procedure TAstaIOIBExpressDataBasePlugin.DataBasePluginCreateProviderParamsEvent(Sender: TObject;
  var Params: TParams; DataSet: TDataSet);
begin
  Params.Assign(TIBQuery(DataSet).Params);
end;

procedure TAstaIOIBExpressDataBasePlugin.DataBasePluginSubmitSQL(Sender: TObject; U: TUserRecord;
  SQLDataSet: TDataSet; DataBaseStr, SQLString: String;
  ClientParams: TParams; RowsToReturn: Integer);
var i: Integer;
begin
  with TIBQuery(SQLDataSet) do
  begin
    Close;
    SQL.Text := SQLString;
    for i := 0 to Params.Count - 1 do
    begin
      Params[i].DataType := ClientParams[i].DataType;
      Params[i].Value := ClientParams[i].value;
    end;
    open;
  end;
end;


end.
