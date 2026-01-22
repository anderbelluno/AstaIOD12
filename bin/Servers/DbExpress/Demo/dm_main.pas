unit dm_main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Db,
  AstaIOUserList,AstaIOCustomDataSet,AstaIODBConst,
  AstaIOProvider,
  AstaIOMetaData, AstaIOServerMethod, AstaIOIProvider,
  AstaIODBInfo, FMTBcd, DBXpress, SqlExpr, AstaIODataSetProvider;

type
  TAstaDataModule = class(TDataModule)
    MetaData: TAstaIOMetaData;
    Query: TSQLQuery;
    SQLConn: TSQLConnection;
    StoredProc: TSQLStoredProc;
    DbInfo: TAstaIODBInfo;
    SQLQuery1: TSQLQuery;
    AstaIOProvider1: TAstaIOProvider;
    AstaIOServerMethodExec1: TAstaIOServerMethodExec;
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
      Query: TDataSet; SQLString: String);
    procedure SQLConnBeforeConnect(Sender: TObject);
    procedure AstaIOServerMethodExec1Action(Sender: TObject;
      U: TUserRecord; Params: TParams);  private
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

procedure TAstaDataModule.SQLConnBeforeConnect(Sender: TObject);
begin
   With SQLConn Do
   Case f_main.RadioGroupDB.ItemIndex Of
      0 : Begin
             ConnectionName := 'Oracle';
             DriverName     := 'Oracle';
             GetDriverFunc  := 'getSQLDriverORACLE';
             LibraryName    := 'dbexpora.dll';
             Params.Values['DataBase'] := 'ORACLE8';
             Params.Values['User_Name'] := 'SISTEF';
             Params.Values['Password'] := '199192056';
             VendorLib      := 'OCI.DLL';
          End;
      1 : Begin
             ConnectionName := 'SQLServerConnection';
             DriverName     := 'SQLServer';
             GetDriverFunc  := 'getSQLDriverSQLServer';
             LibraryName    := 'dbexpsda.dll';
             Params.Values['DataBase'] := '192.168.0.4/EMPLOYEE';
             Params.Values['User_Name'] := 'SISTEF';
             Params.Values['Password'] := '199192056';
             VendorLib      := 'SQLOLEDB.DLL';
          End;
      2 : Begin
             ConnectionName := 'IBLocal';
             DriverName     := 'Interbase';
             GetDriverFunc  := 'getSQLDriverINTERBASE';
             LibraryName    := 'dbexpint.dll';
             Params.Values['DataBase'] := '192.168.0.26:c:\interbase\EMPLOYEE.gdb';
             Params.Values['User_Name'] := 'SISTEF';
             Params.Values['Password'] := '199192056';
             VendorLib      := 'GDS32.DLL';             
          End;
   End;
end;

procedure TAstaDataModule.AstaIOServerMethodExec1Action(Sender: TObject;
  U: TUserRecord; Params: TParams);
begin
   SQLQuery1.Close;
   SQLQuery1.SQL.Clear;
   SQLQuery1.SQL.Add('SELECT * FROM EMPLOYEE');
   SQLQuery1.Open;
end;

end.
