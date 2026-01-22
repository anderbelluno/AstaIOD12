unit dm_main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  AstaIOUserList,AstaIOCustomDataSet,AstaIODBConst,
  AstaIOProvider,
  AstaIOMetaData, AstaIOServerMethod, AstaIOIBInfo, AstaIOIProvider,
  db,DBTables;

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
    procedure IBInfoSetSQL(Sender: TObject; Query: TDataSet;
      SQLString: String);
    procedure MetaDataDBMSName(Sender: TObject; U: TUserRecord;
      MetaDataSet: TAstaIODataSet);
    procedure MetaDataFields(Sender: TObject; U: TUserRecord;
      MetaDataRequest: TAstaMetaData; MetaDataSet: TAstaIODataSet;
      DataBaseName, TableName: String);
    procedure MetaDataTables(Sender: TObject; U: TUserRecord;
      MetaDataRequest: TAstaMetaData; MetaDataSet: TAstaIODataSet;
      DataBaseName, TableName: String);
    procedure servermethod2Action(Sender: TObject);
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
    procedure provBeforeOpen(Sender: TObject; U: TUserRecord;
      Params: TParams);
    procedure provBeforeUpdate(Sender: TObject; U: TUserRecord;
      ExecQuery: TComponent; OriginalValueDataSet, CurrentValueDataSet,
      ServerValueDataSet: TDataSet; var Handled: Boolean);
    procedure provAfterTransaction(Sender: TObject; U: TUserRecord;
      CurrentValueDataSet: TDataSet; TransactionFailed: Boolean);
    procedure provBeforeTransaction(Sender: TObject; U: TUserRecord;
      OriginalValueDataSet, CurrentValueDataSet: TDataSet;
      var Handled: Boolean);
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

procedure TAstaDataModule.MetaDataFields(Sender: TObject; U: TUserRecord;
  MetaDataRequest: TAstaMetaData; MetaDataSet: TAstaIODataSet; DataBaseName,
  TableName: String);
begin
 // m.lines.add('Fields Info for ' + TableName);
(*   IbInfo.GetMetaData(U,MetaDataRequest, DatabaseName, TableName);
   IbInfo.DataSet.First;
   while not IbInfo.DataSet.Eof do
   begin
    MetaDataSet.AppendRecord([IbInfo.DataSet.Fields[0].AsString,
                              Ord(IbInfo.IBTypeToVCLFieldType(IbInfo.DataSet.Fields[1].AsInteger)),
                              IbInfo.DataSet.Fields[2].AsInteger]);
    IbInfo.DataSet.Next;
  end; *)
end;

procedure TAstaDataModule.MetaDataTables(Sender: TObject; U: TUserRecord;
  MetaDataRequest: TAstaMetaData; MetaDataSet: TAstaIODataSet; DataBaseName,
  TableName: String);
Var
i:Integer;
List:TStringList;
begin
  //m.lines.add('Tables Info');
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

procedure TAstaDataModule.provAfterDelete(Sender: TObject; U: TUserRecord;
  ExecQuery: TComponent; OriginalValueDataSet,
  ServerValueDataSet: TDataSet);
begin
  f_main.m.Lines.Add('After Provider Delete Event');
end;

procedure TAstaDataModule.provAfterInsert(Sender: TObject; U: TUserRecord;
  ExecQuery: TComponent; OriginalValueDataSet, CurrentValueDataSet,
  ServerValueDataSet: TDataSet);
begin
  f_main.m.Lines.Add('After Provider Insert Event');
end;

procedure TAstaDataModule.provAfterUpdate(Sender: TObject; U: TUserRecord;
  ExecQuery: TComponent; OriginalValueDataSet, CurrentValueDataSet,
  ServerValueDataSet: TDataSet);
begin
  f_main.m.Lines.Add('After Provider Update Event');
end;

procedure TAstaDataModule.provBeforeDelete(Sender: TObject; U: TUserRecord;
  ExecQuery: TComponent; OriginalValueDataSet,
  ServerValueDataSet: TDataSet; var Handled: Boolean);
begin
  f_main.m.Lines.Add('Before Provider Delete Event');
end;

procedure TAstaDataModule.provBeforeInsert(Sender: TObject; U: TUserRecord;
  ExecQuery: TComponent; OriginalValueDataSet, CurrentValueDataSet,
  ServerValueDataSet: TDataSet; var Handled: Boolean);
begin
  f_main.m.Lines.Add('Before Provider Insert Event');
end;

procedure TAstaDataModule.provBeforeOpen(Sender: TObject; U: TUserRecord;
  Params: TParams);
begin
  f_main.m.Lines.Add('Before Provider Open Event');
end;

procedure TAstaDataModule.provBeforeUpdate(Sender: TObject; U: TUserRecord;
  ExecQuery: TComponent; OriginalValueDataSet, CurrentValueDataSet,
  ServerValueDataSet: TDataSet; var Handled: Boolean);
begin
  f_main.m.Lines.Add('Before Provider Updat Event');
end;

procedure TAstaDataModule.provAfterTransaction(Sender: TObject;
  U: TUserRecord; CurrentValueDataSet: TDataSet;
  TransactionFailed: Boolean);
begin
  f_main.m.Lines.Add('After Provider Transaction Event');
end;

procedure TAstaDataModule.provBeforeTransaction(Sender: TObject;
  U: TUserRecord; OriginalValueDataSet, CurrentValueDataSet: TDataSet;
  var Handled: Boolean);
begin
  f_main.m.Lines.Add('Before Provider Transaction Event');
end;

procedure TAstaDataModule.DataModuleCreate(Sender: TObject);
begin
query.DatabaseName:=database.DatabaseName;
end;

end.
