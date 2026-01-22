unit dm_main;

interface
           
uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  AstaIOUserList,AstaIOCustomDataSet,AstaIODBConst,
  AstaIOProvider,
  AstaIOMetaData, AstaIOServerMethod, AstaIOIBInfo, AstaIOIProvider,
  db,DBISAMTb, AstaIODataSetProvider;

type
  TAstaDataModule = class(TDataModule)
    Query: TDBISAMQuery;
    Customerquery: TDBISAMQuery;
    ProviderQuery: TDBISAMQuery;
    ServerMethodQuery: TDBISAMQuery;
    prov: TAstaIOProvider;
    servermethod1: TAstaIOServerMethodResultSet;
    servermethod2: TAstaIOServerMethodExec;
    MetaData: TAstaIOMetaData;
    iprov1: TAstaIOIProvider;
    iprov_query1: TDBISAMQuery;
    iprov2: TAstaIOIProvider;
    iprov_query2: TDBISAMQuery;
    iprov3: TAstaIOIProvider;
    Database: TDBISAMDatabase;
    Session1: TDBISAMSession;
    ColumnTable: TDBISAMTable;
    procedure IBInfoSetSQL(Sender: TObject; Query: TDataSet;
      SQLString: String);
    procedure MetaDataDBMSName(Sender: TObject; U: TUserRecord;
      MetaDataSet: TAstaIODataSet);
    procedure MetaDataTables(Sender: TObject; U: TUserRecord;
      MetaDataRequest: TAstaMetaData; MetaDataSet: TAstaIODataSet;
      DataBaseName, TableName: String);
    procedure servermethod2Action(Sender: TObject);
    procedure provAfterDelete(Sender: TObject; U: TUserRecord;
      ExecQuery: TComponent; OriginalValueDataSet,
      ServerValueDataSet: TDataSet);
    procedure DataModuleCreate(Sender: TObject);
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

procedure TAstaDataModule.IBInfoSetSQL(Sender: TObject; Query: TDataSet;
  SQLString: String);
begin
  TDbisamQuery(Query).SQL.Text:=SQLString;
end;

procedure TAstaDataModule.MetaDataDBMSName(Sender: TObject; U: TUserRecord;
  MetaDataSet: TAstaIODataSet);
begin
  MetaDataSet.AppendRecord(['AstaIODbisamServer',
                            'Database',
                            ExtractFileName(Application.EXEName),
                            'Test'
                            ])

end;

procedure TAstaDataModule.MetaDataTables(Sender: TObject; U: TUserRecord;
  MetaDataRequest: TAstaMetaData; MetaDataSet: TAstaIODataSet; DataBaseName,
  TableName: String);
Var
i:Integer;
List:TStringList;
begin
  List:=TStringList.Create;
  try
  Session1.GetTableNames(Database.DataBaseName, List);
   for i:=0 to List.count-1 do
    if Pos('.',list[i])>0 then
      MetaDataSet.Appendrecord([StringBeforeToken(list[i],'.')])
    else       MetaDataSet.Appendrecord([list[i]]);
  finally
    List.Free;
  end;
end;

procedure TAstaDataModule.servermethod2Action(Sender: TObject);
begin
  servermethod2.Params[0].AsInteger:=servermethod2.Params[0].AsInteger * 10;
  servermethod2.Params[1].AsString:=DateTimeToStr(now);
end;

procedure TAstaDataModule.provAfterDelete(Sender: TObject; U: TUserRecord;
  ExecQuery: TComponent; OriginalValueDataSet,
  ServerValueDataSet: TDataSet);
begin
  f_main.m.Lines.Add('After Provider Delete Event');
end;

procedure TAstaDataModule.DataModuleCreate(Sender: TObject);
Var
 i:integer;
begin
if ParamBool('Path') then Database.Directory:=ExtractFilePath(Application.ExeName);
DataBase.DataBaseName:=DataBase.DataBaseName+floatToStr(now);
for i:=0 to componentcount-1 do
 if components[i] is TDbisamQuery then
  TDbisamQuery(Components[i]).DatabaseName:=Database.DatabaseName
else if components[i] is TDbisamTable then
  TDbisamTable(Components[i]).DatabaseName:=Database.DatabaseName

end;

procedure TAstaDataModule.MetaDataFields(Sender: TObject; U: TUserRecord;
  MetaDataRequest: TAstaMetaData; MetaDataSet: TAstaIODataSet;
  DataBaseName, TableName: String);
var
i:integer;
begin
    //ColumnTable.DatabaseName:=LookupDatabaseName(DatabaseName);
    ColumnTable.Close;
    ColumnTable.Tablename := TableName;
    ColumnTable.Open;
    for i := 0 to ColumnTable.FieldCount - 1 do
      MetaDataSet.AppendRecord([ColumnTable.Fields[i].FieldName,
        ord(ColumnTable.Fields[i].DataType)
          , ColumnTable.Fields[i].Size]);
end;

end.
