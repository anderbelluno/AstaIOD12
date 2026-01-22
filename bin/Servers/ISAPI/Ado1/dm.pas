unit dm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  IBDatabase, Db, IBStoredProc, IBCustomDataSet, IBQuery,
  AstaIOUserList,AstaIOCustomDataSet,AstaIODBConst,
  AstaIOProvider,
  AstaIOMetaData, AstaIOServerMethod, AstaIOIBInfo, AstaIOIProvider,
  IBTable, AstaIODBInfo, ADODB, AstaIODataSetProvider;

type
  TAstaDataModule = class(TDataModule)
    MetaData: TAstaIOMetaData;
    DBInfo: TAstaIODBInfo;
    StoredProc: TADOStoredProc;
    ExecQuery: TADOQuery;
    Connection: TADOConnection;
    Query: TADOQuery;
    CustomerQuery: TADOQuery;
    CustProvider: TAstaIOProvider;
    Increment: TAstaIOServerMethodExec;
    Employeequery: TADOQuery;
    empprovider: TAstaIOProvider;
    procedure MetaDataTables(Sender: TObject; U: TUserRecord;
      MetaDataRequest: TAstaMetaData; MetaDataSet: TAstaIODataSet;
      DataBaseName, TableName: String);
    procedure DBInfoSetSQL(Sender: TObject; U: TUserRecord;
      Query: TDataSet; SQLString: String);
    procedure IncrementAction(Sender: TObject; U: TUserRecord;
      Params: TParams);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure CloseConnection;
  end;

var
  AstaDataModule: TAstaDataModule;

implementation

{$R *.DFM}
procedure TAstaDataModule.CloseConnection;
begin
 Connection.Connected:=False;
end;

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

procedure TAstaDataModule.IncrementAction(Sender: TObject; U: TUserRecord;
  Params: TParams);
begin
 params.ParamByName('outPut').AsInteger:=Params.ParambyName('Intput').Asinteger+1;
end;

end.
