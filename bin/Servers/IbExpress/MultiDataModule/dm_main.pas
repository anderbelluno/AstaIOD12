unit dm_main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  IBDatabase, Db, IBStoredProc, IBCustomDataSet, IBQuery, 
  AstaIOUserList,AstaIOCustomDataSet,AstaIODBConst,
  AstaIOProvider,
  AstaIOMetaData, AstaIOServerMethod, AstaIOIBInfo, AstaIOIProvider,
  IBTable, AstaIODBInfo;

type
  TAstaDataModule = class(TDataModule)
    Query: TIBQuery;
    StoredProc: TIBStoredProc;
    Database: TIBDatabase;
    Transaction: TIBTransaction;
    Customerquery: TIBQuery;
    ProviderQuery: TIBQuery;
    ServerMethodQuery: TIBQuery;
    prov: TAstaIOProvider;
    servermethod1: TAstaIOServerMethodResultSet;
    servermethod2: TAstaIOServerMethodExec;
    MetaData: TAstaIOMetaData;
    iprov1: TAstaIOIProvider;
    iprov_query1: TIBQuery;
    iprov2: TAstaIOIProvider;
    iprov2_table: TIBTable;
    iprov_query2: TIBQuery;
    iprov3: TAstaIOIProvider;
    DBInfo: TAstaIODBInfo;
    ServerMethodProviderParams: TAstaIOServerMethodResultSet;
    ServerMethodParams: TAstaIOServerMethodResultSet;
    procedure MetaDataDBMSName(Sender: TObject; U: TUserRecord;
      MetaDataSet: TAstaIODataSet);
    procedure DBInfoSetSQL(Sender: TObject; U: TUserRecord;
      Query: TDataSet; SQLString: String);
    procedure provAfterDelete(Sender: TObject; U: TUserRecord;
      ExecQuery: TComponent; OriginalValueDataSet,
      ServerValueDataSet: TDataSet);
    procedure provAfterInsert(Sender: TObject; U: TUserRecord;
      ExecQuery: TComponent; OriginalValueDataSet, CurrentValueDataSet,
      ServerValueDataSet: TDataSet);
    procedure provAfterUpdate(Sender: TObject; U: TUserRecord;
      ExecQuery: TComponent; OriginalValueDataSet, CurrentValueDataSet,
      ServerValueDataSet: TDataSet);
    procedure provBeforeDelete(Sender: TObject; U: TUserRecord;
      ExecQuery: TComponent; OriginalValueDataSet,
      ServerValueDataSet: TDataSet; var Handled: Boolean);
    procedure provBeforeInsert(Sender: TObject; U: TUserRecord;
      ExecQuery: TComponent; OriginalValueDataSet, CurrentValueDataSet,
      ServerValueDataSet: TDataSet; var Handled: Boolean);
    procedure provBeforeUpdate(Sender: TObject; U: TUserRecord;
      ExecQuery: TComponent; OriginalValueDataSet, CurrentValueDataSet,
      ServerValueDataSet: TDataSet; var Handled: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AstaDataModule: TAstaDataModule;

implementation
uses u_main;
{$R *.DFM}

procedure TAstaDataModule.MetaDataDBMSName(Sender: TObject; U: TUserRecord;
  MetaDataSet: TAstaIODataSet);
begin
  MetaDataSet.AppendRecord([ExtractFileName(Application.EXEName),
                            ExtractFileName(Database.DatabaseName),
                            'Some info',
                            ExtractFileName(Application.EXEName)])
end;

procedure TAstaDataModule.DBInfoSetSQL(Sender: TObject; U: TUserRecord;
  Query: TDataSet; SQLString: String);
begin
  // This is used to assign the sql to the query that does the select from the system tables for metadata
  TIBQuery(Query).SQL.Text:=SQLString;
end;

procedure TAstaDataModule.provAfterDelete(Sender: TObject; U: TUserRecord;
  ExecQuery: TComponent; OriginalValueDataSet,
  ServerValueDataSet: TDataSet);
begin
  f_main.LogIt('***** provAfterDelete *****');
end;

procedure TAstaDataModule.provAfterInsert(Sender: TObject; U: TUserRecord;
  ExecQuery: TComponent; OriginalValueDataSet, CurrentValueDataSet,
  ServerValueDataSet: TDataSet);
begin
  f_main.LogIt('***** provAfterInsert *****');
end;

procedure TAstaDataModule.provAfterUpdate(Sender: TObject; U: TUserRecord;
  ExecQuery: TComponent; OriginalValueDataSet, CurrentValueDataSet,
  ServerValueDataSet: TDataSet);
begin
  f_main.LogIt('***** provAfterUpdate *****');
end;

procedure TAstaDataModule.provBeforeDelete(Sender: TObject; U: TUserRecord;
  ExecQuery: TComponent; OriginalValueDataSet,
  ServerValueDataSet: TDataSet; var Handled: Boolean);
begin
  f_main.LogIt('***** provBeforeDelete *****');
end;

procedure TAstaDataModule.provBeforeInsert(Sender: TObject; U: TUserRecord;
  ExecQuery: TComponent; OriginalValueDataSet, CurrentValueDataSet,
  ServerValueDataSet: TDataSet; var Handled: Boolean);
begin
  f_main.LogIt('***** provBeforeInsert *****');
end;

procedure TAstaDataModule.provBeforeUpdate(Sender: TObject; U: TUserRecord;
  ExecQuery: TComponent; OriginalValueDataSet, CurrentValueDataSet,
  ServerValueDataSet: TDataSet; var Handled: Boolean);
begin
  f_main.LogIt('***** provBeforeUpdate *****');
  if OriginalValueDataSet.FieldByName('CONTACT_LAST').AsString = 'Brown' then
    Handled:=True
  else
    Handled:=False;

  if Handled then exit;
  f_main.LogIt('Old = ' + OriginalValueDataSet.FieldByName('CONTACT_LAST').AsString);
  f_main.LogIt('Current = ' + CurrentValueDataSet.FieldByName('CONTACT_LAST').AsString);
end;

end.
