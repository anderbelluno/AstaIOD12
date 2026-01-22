unit dm_main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  IBDatabase, Db, IBStoredProc, IBCustomDataSet, IBQuery, 
  AstaIOUserList,AstaIOCustomDataSet,AstaIODBConst,
  AstaIOProvider,
  AstaIOMetaData, AstaIOServerMethod, AstaIOIBInfo, AstaIOIProvider,
  IBTable, AstaIODBInfo, AstaIOClientDataSet, AstaIODataSetProvider;

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
    CustParamQuery: TIBQuery;
    ProviderCustParam: TAstaIOProvider;
    asta_provider3: TAstaIODataSetProvider;
    mas_asta_dataset3: TAstaIOClientDataSet;
    ds_asta_mas_clientdataset3: TDataSource;
    mas_asta_table3: TIBTable;
    ds_asta_mas_query3: TDataSource;
    det_asta_query3: TIBQuery;
    ds_asta_det_clientdataset3: TDataSource;
    det_asta_dataset3: TAstaIOClientDataSet;
    prov_nds3: TAstaIOProvider;
    det_det_asta_dataset3: TAstaIOClientDataSet;
    ds_asta_det_det_clientdataset3: TDataSource;
    det_det_asta_query3: TIBQuery;
    det_det_det_asta_query3: TIBQuery;
    ds_asta_det_det_det_clientdataset3: TDataSource;
    det_det_det_asta_dataset3: TAstaIOClientDataSet;
    asta_provider2: TAstaIODataSetProvider;
    mas_asta_dataset2: TAstaIOClientDataSet;
    det_asta_dataset2: TAstaIOClientDataSet;
    ds_asta_det_clientdataset2: TDataSource;
    det_asta_query2: TIBQuery;
    mas_asta_table2: TIBTable;
    ds_asta_mas_query2: TDataSource;
    ds_asta_mas_clientdataset2: TDataSource;
    det_det_asta_query2: TIBQuery;
    ds_asta_det_det_clientdataset2: TDataSource;
    det_det_asta_dataset2: TAstaIOClientDataSet;
    prov_nds2: TAstaIOProvider;
    asta_provider1: TAstaIODataSetProvider;
    mas_asta_dataset1: TAstaIOClientDataSet;
    det_asta_dataset1: TAstaIOClientDataSet;
    ds_asta_det_clientdataset1: TDataSource;
    det_asta_query1: TIBQuery;
    mas_asta_table1: TIBTable;
    ds_asta_mas_query1: TDataSource;
    ds_asta_mas_clientdataset1: TDataSource;
    prov_nds1: TAstaIOProvider;
    mas_asta_dataset1EMP_NO: TSmallintField;
    mas_asta_dataset1FIRST_NAME: TStringField;
    mas_asta_dataset1LAST_NAME: TStringField;
    mas_asta_dataset1PHONE_EXT: TStringField;
    mas_asta_dataset1HIRE_DATE: TDateTimeField;
    mas_asta_dataset1DEPT_NO: TStringField;
    mas_asta_dataset1JOB_CODE: TStringField;
    mas_asta_dataset1JOB_GRADE: TSmallintField;
    mas_asta_dataset1JOB_COUNTRY: TStringField;
    mas_asta_dataset1SALARY: TFloatField;
    mas_asta_dataset1FULL_NAME: TStringField;
    mas_asta_dataset1det_asta_query1: TDataSetField;
    mas_asta_dataset2EMP_NO: TSmallintField;
    mas_asta_dataset2FIRST_NAME: TStringField;
    mas_asta_dataset2LAST_NAME: TStringField;
    mas_asta_dataset2PHONE_EXT: TStringField;
    mas_asta_dataset2HIRE_DATE: TDateTimeField;
    mas_asta_dataset2DEPT_NO: TStringField;
    mas_asta_dataset2JOB_CODE: TStringField;
    mas_asta_dataset2JOB_GRADE: TSmallintField;
    mas_asta_dataset2JOB_COUNTRY: TStringField;
    mas_asta_dataset2SALARY: TFloatField;
    mas_asta_dataset2FULL_NAME: TStringField;
    mas_asta_dataset2det_det_asta_query2: TDataSetField;
    mas_asta_dataset2det_asta_query2: TDataSetField;
    mas_asta_dataset3EMP_NO: TSmallintField;
    mas_asta_dataset3FIRST_NAME: TStringField;
    mas_asta_dataset3LAST_NAME: TStringField;
    mas_asta_dataset3PHONE_EXT: TStringField;
    mas_asta_dataset3HIRE_DATE: TDateTimeField;
    mas_asta_dataset3DEPT_NO: TStringField;
    mas_asta_dataset3JOB_CODE: TStringField;
    mas_asta_dataset3JOB_GRADE: TSmallintField;
    mas_asta_dataset3JOB_COUNTRY: TStringField;
    mas_asta_dataset3SALARY: TFloatField;
    mas_asta_dataset3FULL_NAME: TStringField;
    mas_asta_dataset3det_det_det_asta_query3: TDataSetField;
    mas_asta_dataset3det_det_asta_query3: TDataSetField;
    mas_asta_dataset3det_asta_query3: TDataSetField;
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
