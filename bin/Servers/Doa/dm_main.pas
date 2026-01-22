unit dm_main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Db,   AstaIOUserList,AstaIOCustomDataSet,AstaIODBConst,
  AstaIOProvider,
  AstaIOMetaData, AstaIOServerMethod, AstaIOIProvider,
  AstaIODBInfo, Oracle, OracleData;

type
  TAstaDataModule = class(TDataModule)
    MainLogon: TOracleLogon;
    Database: TOracleSession;
    SpColumnDataSet: TOracleDataSet;
    SpNameDataSet: TOracleDataSet;
    Query: TOracleDataSet;
    FielddataSet: TOracleDataSet;
    BlobDataSet: TOracleDataSet;
    ExecQuery: TOracleQuery;
    MetaQuery: TOracleDataSet;
    MetaData: TAstaIOMetaData;
    AstaIODBInfo1: TAstaIODBInfo;
    procedure MetaDataDBMSName(Sender: TObject; U: TUserRecord;
      MetaDataSet: TAstaIODataSet);
    procedure DBInfoSetSQL(Sender: TObject; U: TUserRecord;
      Query: TDataSet; SQLString: String);
    procedure MetaDataTables(Sender: TObject; U: TUserRecord;
      MetaDataRequest: TAstaMetaData; MetaDataSet: TAstaIODataSet;
      DataBaseName, TableName: String);
    procedure MetaDataFields(Sender: TObject; U: TUserRecord;
      MetaDataRequest: TAstaMetaData; MetaDataSet: TAstaIODataSet;
      DataBaseName, TableName: String);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AstaDataModule: TAstaDataModule;

implementation
uses u_main,AstaIOUtil;
{$R *.DFM}

procedure TAstaDataModule.MetaDataDBMSName(Sender: TObject; U: TUserRecord;
  MetaDataSet: TAstaIODataSet);
begin
  MetaDataSet.AppendRecord([ExtractFileName(Application.EXEName),
                            ExtractFileName(Database.LogOnDatabase),
                            'Some info',
                            ExtractFileName(Application.EXEName)])
end;

procedure TAstaDataModule.DBInfoSetSQL(Sender: TObject; U: TUserRecord;
  Query: TDataSet; SQLString: String);
begin
  // This is used to assign the sql to the query that does the select from the system tables for metadata
  if Query is TOracleDataSet then
    TOracleDataSet(Query).SQL.Text:=SQLString;
end;

procedure TAstaDataModule.MetaDataTables(Sender: TObject; U: TUserRecord;
  MetaDataRequest: TAstaMetaData; MetaDataSet: TAstaIODataSet;
  DataBaseName, TableName: String);
begin
  MetaQuery.Close;
  //if AddOwnerName then
//  MetaDataSet.SQL.Add('Select Owner,Table_Name from User_Tables order by Owner,Table_Name')
//  else
 MetaQuery.SQL.text:='Select Table_Name from User_Tables order by Table_Name';
 MetaQuery.Open;
 while not MetaQuery.Eof do begin
  MetaDataSet.Appendrecord([MetaQuery.Fields[0].Asstring]);
  MetaQuery.Next;
 end;
end;

procedure TAstaDataModule.MetaDataFields(Sender: TObject; U: TUserRecord;
  MetaDataRequest: TAstaMetaData; MetaDataSet: TAstaIODataSet;
  DataBaseName, TableName: String);
begin
   if pos('.',tableName)>0 then //for ownername
   TableName:=StringAfterToken(tablename,'.');
	 FieldDataSet.Close;
   FieldDataSet.SQL.Text:='Select column_name,data_type,data_length'+
  ' data_precision,data_scale,nullable from all_tab_columns '+
  'where table_name= :TableName';
	FieldDataSet.DeclareVariable('TableName',otString);
	FieldDataSet.SetVariable('TableName', tablename);
	FieldDataSet.Open;
  while not FieldDataSet.Eof do begin
  MetaDataSet.Appendrecord([FieldDataSet.Fields[0].Asstring]);
  FieldDataSet.Next;
 end;

end;

end.
