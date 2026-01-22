unit dm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Db, 
  AstaIOUserList,AstaIOCustomDataSet,AstaIODBConst,
  AstaIOProvider,
  AstaIOMetaData, AstaIOServerMethod, AstaIOIBInfo, AstaIOIProvider,
  AstaIODBInfo, AstaIODataSetProvider, adstable, adsdata,
  adsfunc, adscnnct;

type
  TAstaDataModule = class(TDataModule)
    MetaData: TAstaIOMetaData;
    AdsConnection: TAdsConnection;
    IndexTable: TAdsTable;
    ColumnTable: TAdsTable;
    AdsQuery: TAdsQuery;
    ExecQuery: TAdsQuery;
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
     procedure PopulateColumns(Databasename,TableName: string);
  public
    { Public declarations }
  end;

var
  AstaDataModule: TAstaDataModule;

implementation
uses mainunit,AstaIOAdvantageSupplementDM,AstaIOUtil;
{$R *.DFM}

procedure TAstaDataModule.DBInfoSetSQL(Sender: TObject; U: TUserRecord;
  Query: TDataSet; SQLString: String);
begin
  // This is used to assign the sql to the query that does the select from the system tables for metadata
  TADSQuery(Query).SQL.Text:=SQLString;
end;

procedure TAstaDataModule.MetaDataTables(Sender: TObject; U: TUserRecord;
  MetaDataRequest: TAstaMetaData; MetaDataSet: TAstaIODataSet;
  DataBaseName, TableName: String);
var
List:TStringlist;
i:integer;
begin
List:=TStringList.Create;
try
 ColumnTable.GetFiles(List);
for i:=0 to List.count-1 do
 if pos('.',List[i])>0 then  MetaDataSet.AppendRecord([TokenCount(list[i],0,'.')]) else
 MetaDataSet.AppendRecord([list[i]]);
finally
 List.free;
end;
end;


procedure TAstaDataModule.MetaDataFields(Sender: TObject; U: TUserRecord;
  MetaDataRequest: TAstaMetaData; MetaDataSet: TAstaIODataSet;
  DataBaseName, TableName: String);
var
i:integer;
begin
  PopulateColumns(Databasename, Tablename);
   for i:=0 to ColumnTable.FieldCount-1  do
    MetaDataSet.AppendRecord([ColumnTable.Fields[i].FieldName,
                              ord(ColumnTable.Fields[i].DataType)
                             ,ColumnTable.Fields[i].Size]);
end;

Procedure TAstaDataModule.PopulateColumns(DatabaseName, TableName:String);
var
i:Integer;
begin
with dm.AstaDataModule do begin
 ColumnTable.Close;
//ColumnTable.DatabaseName:=LookupDatabaseName(DatabaseName);
 ColumnTable.Tablename:=TableName;
 ColumnTable.Open;
end;
end;

end.
