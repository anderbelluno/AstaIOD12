unit dm_main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Db,
  AstaIOUserList,AstaIOCustomDataSet,AstaIODBConst,
  AstaIOProvider,
  AstaIOMetaData, AstaIOServerMethod, AstaIOIProvider,
  AstaIODBInfo, FMTBcd, DBXpress, SqlExpr;

type
  TAstaDataModule = class(TDataModule)
    MetaData: TAstaIOMetaData;
    Query: TSQLQuery;
    SQLConn: TSQLConnection;
    StoredProc: TSQLStoredProc;
    DbInfo: TAstaIODBInfo;
    procedure MetaDataDBMSName(Sender: TObject; U: TUserRecord;
      MetaDataSet: TAstaIODataSet);
    procedure MetaDataTables(Sender: TObject; U: TUserRecord;
      MetaDataRequest: TAstaMetaData; MetaDataSet: TAstaIODataSet;
      DataBaseName, TableName: String);
    procedure MetaDataFields(Sender: TObject; U: TUserRecord;
      MetaDataRequest: TAstaMetaData; MetaDataSet: TAstaIODataSet;
      DataBaseName, TableName: String);
    procedure MetaDataStoredProcs(Sender: TObject; U: TUserRecord;
      MetaDataRequest: TAstaMetaData; MetaDataSet: TAstaIODataSet;
      DataBaseName, TableName: String);
    procedure MetaDataIndexes(Sender: TObject; U: TUserRecord;
      MetaDataRequest: TAstaMetaData; MetaDataSet: TAstaIODataSet;
      DataBaseName, TableName: String);
    procedure DbInfoSetSQL(Sender: TObject; U: TUserRecord;
      Query: TDataSet; SQLString: String);  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AstaDataModule: TAstaDataModule;

implementation
uses mainunit;
{$R *.DFM}


procedure TAstaDataModule.MetaDataDBMSName(Sender: TObject; U: TUserRecord;
  MetaDataSet: TAstaIODataSet);
begin
  MetaDataSet.AppendRecord(['ServerName',
                            'Database',
                            ExtractFileName(Application.EXEName),
                            'Test'
                            ])

end;

procedure TAstaDataModule.MetaDataTables(Sender: TObject; U: TUserRecord;
  MetaDataRequest: TAstaMetaData; MetaDataSet: TAstaIODataSet; DataBaseName,
  TableName: String);
var
List:TStringList;
i:integer;
begin
    List:=TStringList.Create;
    try
     SQLConn.GetTableNames(List,False);
     for i:=0 to List.Count-1 do
      MetaDataSet.AppendRecord([List[i]]);
    finally
     List.Free;
    end;
end;

procedure TAstaDataModule.MetaDataFields(Sender: TObject; U: TUserRecord;
  MetaDataRequest: TAstaMetaData; MetaDataSet: TAstaIODataSet;
  DataBaseName, TableName: String);
var
List:TStringList;
i:integer;
begin
    List:=TStringList.Create;
    try
     SQLConn.GetFieldNames(TableName,List);
     for i:=0 to List.Count-1 do
      MetaDataSet.AppendRecord([List[i]]);
    finally
     List.Free;
    end;
end;

procedure TAstaDataModule.MetaDataStoredProcs(Sender: TObject;
  U: TUserRecord; MetaDataRequest: TAstaMetaData;
  MetaDataSet: TAstaIODataSet; DataBaseName, TableName: String);
var
List:TStringList;
i:integer;
begin
    List:=TStringList.Create;
    try
     SQLConn.GetProcedureNames(List);
     for i:=0 to List.Count-1 do
      MetaDataSet.AppendRecord([List[i]]);
    finally
     List.Free;
    end;
end;

procedure TAstaDataModule.MetaDataIndexes(Sender: TObject; U: TUserRecord;
  MetaDataRequest: TAstaMetaData; MetaDataSet: TAstaIODataSet;
  DataBaseName, TableName: String);
var
List:TStringList;
i:integer;
begin
    List:=TStringList.Create;
    try
     SQLConn.GetIndexNames(TableName,List);
     for i:=0 to List.Count-1 do
      MetaDataSet.AppendRecord([List[i]]);
    finally
     List.Free;
    end;
end;

procedure TAstaDataModule.DbInfoSetSQL(Sender: TObject; U: TUserRecord;
  Query: TDataSet; SQLString: String);
begin
  TSQLQuery(Query).SQL.Text:=SQLString;

end;

end.
