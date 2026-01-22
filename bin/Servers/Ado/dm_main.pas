unit dm_main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Db, AstaIOUserList,AstaIOCustomDataSet,AstaIODBConst,
  AstaIOProvider,
  AstaIOMetaData, AstaIOServerMethod, AstaIOIBInfo, AstaIOIProvider,
  AstaIODBInfo, ADODB, AstaIODataSetProvider;

type
  TAstaDataModule = class(TDataModule)
    MetaData: TAstaIOMetaData;
    DBInfo: TAstaIODBInfo;
    StoredProc: TADOStoredProc;
    ExecQuery: TADOQuery;
    Query: TADOQuery;
    empprovider: TAstaIOProvider;
    empquery: TADOQuery;
    Connection: TADOConnection;
    procedure MetaDataTables(Sender: TObject; U: TUserRecord;
      MetaDataRequest: TAstaMetaData; MetaDataSet: TAstaIODataSet;
      DataBaseName, TableName: String);
    procedure DBInfoSetSQL(Sender: TObject; U: TUserRecord;
      Query: TDataSet; SQLString: String);
    procedure DataModuleCreate(Sender: TObject);
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

procedure TAstaDataModule.MetaDataTables(Sender: TObject; U: TUserRecord;
  MetaDataRequest: TAstaMetaData; MetaDataSet: TAstaIODataSet;
  DataBaseName, TableName: String);
Var
List:TStringList;
i:Integer;
begin
 List:=TStringList.Create;
 try
  Connection.GetTableNames(list,False);
  for i:=0 to List.Count-1 do
   MetaDataSet.AppendRecord([List[i]]);
  finally
   List.Free;
 end;
end;

procedure TAstaDataModule.DBInfoSetSQL(Sender: TObject; U: TUserRecord;
  Query: TDataSet; SQLString: String);
begin
  // This is used to assign the sql to the query that does the select from the system tables for metadata
  TADODataSet(Query).CommandText:=SQLString;
end;

procedure TAstaDataModule.DataModuleCreate(Sender: TObject);
begin
//empprovider.sqlgenerator.nosqlfields.add('test');
end;

end.
