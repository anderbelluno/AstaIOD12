unit dm_main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  AstaIOUserList,AstaIOCustomDataSet,AstaIODBConst,
  AstaIOProvider,
  AstaIOMetaData, AstaIOServerMethod, AstaIOIBInfo, AstaIOIProvider,
  db,DBTables, AstaIODataSetProvider;

type
  TAstaDataModule = class(TDataModule)
    Query: TQuery;
    Customerquery: TQuery;
    ProviderQuery: TQuery;
    ServerMethodQuery: TQuery;
    prov: TAstaIOProvider;
    servermethod1: TAstaIOServerMethodResultSet;
    servermethod2: TAstaIOServerMethodExec;
    MetaData: TAstaIOMetaData;
    iprov1: TAstaIOIProvider;
    iprov_query1: TQuery;
    iprov2: TAstaIOIProvider;
    iprov_query2: TQuery;
    iprov3: TAstaIOIProvider;
    Database: TDatabase;
    Session1: TSession;
    ServerMethodProviderParams: TAstaIOServerMethodResultSet;
    procedure IBInfoSetSQL(Sender: TObject; Query: TDataSet;
      SQLString: String);
    procedure MetaDataDBMSName(Sender: TObject; U: TUserRecord;
      MetaDataSet: TAstaIODataSet);
    procedure MetaDataTables(Sender: TObject; U: TUserRecord;
      MetaDataRequest: TAstaMetaData; MetaDataSet: TAstaIODataSet;
      DataBaseName, TableName: String);
    procedure servermethod2Action(Sender: TObject);
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

procedure TAstaDataModule.IBInfoSetSQL(Sender: TObject; Query: TDataSet;
  SQLString: String);
begin
  TQuery(Query).SQL.Text:=SQLString;
end;

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
Var
i:Integer;
List:TStringList;
begin
  List:=TStringList.Create;
  try
  Session1.GetTableNames(Database.DataBaseName, '',False, False, List);
   for i:=0 to List.count-1 do
      MetaDataSet.Appendrecord([list[i]]);
  finally
    List.Free;
  end;
end;

procedure TAstaDataModule.servermethod2Action(Sender: TObject);
begin
  servermethod2.Params[0].AsInteger:=servermethod2.Params[0].AsInteger * 10;
  servermethod2.Params[1].AsString:=DateTimeToStr(now);
end;

end.
