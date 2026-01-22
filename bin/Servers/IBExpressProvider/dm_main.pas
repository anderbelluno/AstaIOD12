unit dm_main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  IBDatabase, Db, IBStoredProc, IBCustomDataSet, IBQuery, 
  AstaIOUserList,AstaIOCustomDataSet,AstaIODBConst,
  AstaIOProvider,
  AstaIOMetaData, AstaIOServerMethod, AstaIOIBInfo, AstaIOIProvider,
  IBTable, AstaIODBInfo, AstaIODataSetProvider;

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
    procedure servermethod2Action(Sender: TObject);
    procedure MetaDataDBMSName(Sender: TObject; U: TUserRecord;
      MetaDataSet: TAstaIODataSet);
    procedure provBeforeDelete(Sender: TObject; U: TUserRecord;
      ExecQuery: TComponent; OriginalValueDataSet,
      ServerValueDataSet: TDataSet; var Handled: Boolean);
    procedure provBeforeUpdate(Sender: TObject; U: TUserRecord;
      ExecQuery: TComponent; OriginalValueDataSet, CurrentValueDataSet,
      ServerValueDataSet: TDataSet; var Handled: Boolean);
    procedure provAfterDelete(Sender: TObject; U: TUserRecord;
      ExecQuery: TComponent; OriginalValueDataSet,
      ServerValueDataSet: TDataSet);
    procedure provAfterUpdate(Sender: TObject; U: TUserRecord;
      ExecQuery: TComponent; OriginalValueDataSet, CurrentValueDataSet,
      ServerValueDataSet: TDataSet);
    procedure DBInfoSetSQL(Sender: TObject; U: TUserRecord;
      Query: TDataSet; SQLString: String);
    procedure provAfterInsert(Sender: TObject; U: TUserRecord;
      ExecQuery: TComponent; CurrentValueDataSet: TDataSet);
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

procedure TAstaDataModule.servermethod2Action(Sender: TObject);
begin
  servermethod2.Params[0].AsInteger:=servermethod2.Params[0].AsInteger * 10;
  servermethod2.Params[1].AsString:=DateTimeToStr(now);
end;

procedure TAstaDataModule.MetaDataDBMSName(Sender: TObject; U: TUserRecord;
  MetaDataSet: TAstaIODataSet);
begin
  MetaDataSet.AppendRecord([ExtractFileName(Application.EXEName),
                            ExtractFileName(Database.DatabaseName),
                            'Some info',
                            ExtractFileName(Application.EXEName)])
end;

procedure TAstaDataModule.provBeforeDelete(Sender: TObject; U: TUserRecord;
  ExecQuery: TComponent; OriginalValueDataSet,
  ServerValueDataSet: TDataSet; var Handled: Boolean);
begin
  f_main.LogIt('***** provBeforeDelete *****');

end;

procedure TAstaDataModule.provBeforeUpdate(Sender: TObject; U: TUserRecord;
  ExecQuery: TComponent; OriginalValueDataSet, CurrentValueDataSet,
  ServerValueDataSet: TDataSet; var Handled: Boolean);
begin
//  Raise Exception.Create('some exception');
  f_main.LogIt('***** provBeforeUpdate *****');
  if OriginalValueDataSet.FieldByName('CONTACT_LAST').AsString = 'Brown' then
    Handled:=True
  else
    Handled:=False;

  if Handled then exit; 
  f_main.LogIt('Old = ' + OriginalValueDataSet.FieldByName('CONTACT_LAST').AsString);
  f_main.LogIt('Current = ' + CurrentValueDataSet.FieldByName('CONTACT_LAST').AsString);
end;

procedure TAstaDataModule.provAfterDelete(Sender: TObject; U: TUserRecord;
  ExecQuery: TComponent; OriginalValueDataSet,
  ServerValueDataSet: TDataSet);
begin
  f_main.LogIt('***** provAfterDelete *****');

end;

procedure TAstaDataModule.provAfterUpdate(Sender: TObject; U: TUserRecord;
  ExecQuery: TComponent; OriginalValueDataSet, CurrentValueDataSet,
  ServerValueDataSet: TDataSet);
begin
  f_main.LogIt('***** provAfterUpdate *****');

end;

procedure TAstaDataModule.DBInfoSetSQL(Sender: TObject; U: TUserRecord;
  Query: TDataSet; SQLString: String);
begin
  // This is used to assign the sql to the query that does the select from the system tables for metadata
  TIBQuery(Query).SQL.Text:=SQLString;

end;

procedure TAstaDataModule.provAfterInsert(Sender: TObject; U: TUserRecord;
  ExecQuery: TComponent; CurrentValueDataSet: TDataSet);
begin
  f_main.LogIt('***** provAfterInsert *****');

end;

end.
