unit dm_main;

interface

uses
  SysUtils, Classes, QControls, QForms, QDialogs,
  DB, AstaIOIBInfo,
  AstaIOMetaData,AstaIOUserList,AstaIOCustomDataSet,AstaIODBConst,
  AstaIODataSetProvider, FMTBcd, SqlExpr, DBXpress, AstaIOBaseRdbmsInfo;

type
  TAstaDataModule = class(TDataModule)
    Query: TSQLQuery;
    SQLConn: TSQLConnection;
    MetaData: TAstaIOMetaData;
    IBInfo: TAstaIOIBInfo;
    procedure MetaDataDBMSName(Sender: TObject; U: TUserRecord;
      MetaDataSet: TAstaIODataSet);
    procedure MetaDataFields(Sender: TObject; U: TUserRecord;
      MetaDataRequest: TAstaMetaData; MetaDataSet: TAstaIODataSet;
      DataBaseName, TableName: String);
    procedure MetaDataTables(Sender: TObject; U: TUserRecord;
      MetaDataRequest: TAstaMetaData; MetaDataSet: TAstaIODataSet;
      DataBaseName, TableName: String);
    procedure MetaDataSystemTables(Sender: TObject; U: TUserRecord;
      MetaDataRequest: TAstaMetaData; MetaDataSet: TAstaIODataSet;
      DataBaseName, TableName: String);
    procedure MetaDataStoredProcs(Sender: TObject; U: TUserRecord;
      MetaDataRequest: TAstaMetaData; MetaDataSet: TAstaIODataSet;
      DataBaseName, TableName: String);
    procedure MetaDataViews(Sender: TObject; U: TUserRecord;
      MetaDataRequest: TAstaMetaData; MetaDataSet: TAstaIODataSet;
      DataBaseName, TableName: String);
    procedure MetaDataForeignKeys(Sender: TObject; U: TUserRecord;
      MetaDataRequest: TAstaMetaData; MetaDataSet: TAstaIODataSet;
      DataBaseName, TableName: String);
    procedure MetaDataIndexes(Sender: TObject; U: TUserRecord;
      MetaDataRequest: TAstaMetaData; MetaDataSet: TAstaIODataSet;
      DataBaseName, TableName: String);
    procedure MetaDataPrimeKeys(Sender: TObject; U: TUserRecord;
      MetaDataRequest: TAstaMetaData; MetaDataSet: TAstaIODataSet;
      DataBaseName, TableName: String);
    procedure MetaDataStoredProcColumns(Sender: TObject; U: TUserRecord;
      MetaDataRequest: TAstaMetaData; MetaDataSet: TAstaIODataSet;
      DataBaseName, TableName: String);
    procedure MetaDataVCLFields(Sender: TObject; U: TUserRecord;
      MetaDataRequest: TAstaMetaData; MetaDataSet: TAstaIODataSet;
      DataBaseName, TableName: String);
    procedure IBInfoSetSQL(Sender: TObject; U: TUserRecord;
      Query: TDataSet; SQLString: String);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AstaDataModule: TAstaDataModule;

implementation
uses u_main;
{$R *.dfm}

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
   IbInfo.GetMetaData(U,MetaDataRequest, DatabaseName, TableName);
   IbInfo.DataSet.First;
   while not IbInfo.DataSet.Eof do
   begin
    MetaDataSet.AppendRecord([IbInfo.DataSet.Fields[0].AsString,
                              Ord(IbInfo.IBTypeToVCLFieldType(IbInfo.DataSet.Fields[1].AsInteger)),
                              IbInfo.DataSet.Fields[2].AsInteger]);
    IbInfo.DataSet.Next;
  end;
end;

procedure TAstaDataModule.MetaDataTables(Sender: TObject; U: TUserRecord;
  MetaDataRequest: TAstaMetaData; MetaDataSet: TAstaIODataSet; DataBaseName,
  TableName: String);
begin
  //m.lines.add('Tables Info');
    IbInfo.GetMetaData(U,MetaDataRequest, DatabaseName, TableName);
    IbInfo.DataSet.First;
    while not IbInfo.DataSet.Eof do
    begin
      MetaDataSet.AppendRecord([IbInfo.DataSet.Fields[0].AsString]);
      IbInfo.DataSet.Next;
    end;

end;

procedure TAstaDataModule.MetaDataSystemTables(Sender: TObject; U: TUserRecord;
  MetaDataRequest: TAstaMetaData; MetaDataSet: TAstaIODataSet; DataBaseName,
  TableName: String);
begin
  //m.Lines.Add('System Tables Info');
  IbInfo.GetMetaData(U,MetaDataRequest, DatabaseName, TableName);
  IbInfo.DataSet.First;
  while not IbInfo.DataSet.Eof do
  begin
    MetaDataSet.AppendRecord([IbInfo.DataSet.Fields[0].AsString]);
    IbInfo.DataSet.Next;
  end;
end;

procedure TAstaDataModule.MetaDataStoredProcs(Sender: TObject; U: TUserRecord;
  MetaDataRequest: TAstaMetaData; MetaDataSet: TAstaIODataSet; DataBaseName,
  TableName: String);
begin
  //m.Lines.Add('Stored Procs Info');
  IbInfo.GetMetaData(U,MetaDataRequest, DatabaseName, TableName);
  IbInfo.DataSet.First;
  while not IbInfo.DataSet.Eof do
  begin
    MetaDataSet.AppendRecord([IbInfo.DataSet.Fields[0].AsString]);
    IbInfo.DataSet.Next;
  end;
end;


procedure TAstaDataModule.MetaDataViews(Sender: TObject; U: TUserRecord;
  MetaDataRequest: TAstaMetaData; MetaDataSet: TAstaIODataSet; DataBaseName,
  TableName: String);
begin
  //m.lines.add('View Info');
  IbInfo.GetMetaData(U,MetaDataRequest, DatabaseName, TableName);
  IbInfo.DataSet.First;
  while not IbInfo.DataSet.Eof do
  begin
    MetaDataSet.AppendRecord([IbInfo.DataSet.Fields[0].AsString]);
    IbInfo.DataSet.Next;
  end;
end;


procedure TAstaDataModule.MetaDataForeignKeys(Sender: TObject; U: TUserRecord;
  MetaDataRequest: TAstaMetaData; MetaDataSet: TAstaIODataSet; DataBaseName,
  TableName: String);
begin
  //m.lines.add('Foreign Keys Info for ' + TableName);
  IbInfo.GetMetaData(U,MetaDataRequest, DatabaseName, TableName);
  IbInfo.DataSet.First;
  while not IbInfo.DataSet.Eof do
  begin
    MetaDataSet.AppendRecord([IbInfo.DataSet.Fields[0].AsString,
                              IbInfo.DataSet.Fields[1].AsString,
                              IbInfo.DataSet.Fields[2].AsString]);
    IbInfo.DataSet.Next;
  end;
end;

procedure TAstaDataModule.MetaDataIndexes(Sender: TObject; U: TUserRecord;
  MetaDataRequest: TAstaMetaData; MetaDataSet: TAstaIODataSet; DataBaseName,
  TableName: String);
begin
  //m.lines.add('Index Info for ' + TableName);
  IbInfo.GetMetaData(U,MetaDataRequest, DatabaseName, TableName);
  IbInfo.DataSet.First;
  while not IbInfo.DataSet.Eof do
  begin
    MetaDataSet.AppendRecord([IbInfo.DataSet.Fields[0].AsString]);
    IbInfo.DataSet.Next;
  end;
end;

procedure TAstaDataModule.MetaDataPrimeKeys(Sender: TObject; U: TUserRecord;
  MetaDataRequest: TAstaMetaData; MetaDataSet: TAstaIODataSet; DataBaseName,
  TableName: String);
begin
  //m.lines.add('Prime Keys Info for ' + TableName);
  IbInfo.GetMetaData(U,MetaDataRequest, DatabaseName, TableName);
  IbInfo.DataSet.First;
  while not IbInfo.DataSet.Eof do
  begin
    MetaDataSet.AppendRecord([IbInfo.DataSet.Fields[0].AsString]);
    IbInfo.DataSet.Next;
  end;
end;

procedure TAstaDataModule.MetaDataStoredProcColumns(Sender: TObject;
  U: TUserRecord; MetaDataRequest: TAstaMetaData;
  MetaDataSet: TAstaIODataSet; DataBaseName, TableName: String);
begin
  IbInfo.GetMetaData(U,MetaDataRequest, DatabaseName, TableName);
  IbInfo.DataSet.First;
  while not IbInfo.DataSet.Eof do
  begin
    MetaDataSet.AppendRecord([IbInfo.DataSet.Fields[0].AsString,
                              Ord(IbInfo.IBTypeToVCLFieldType(IbInfo.DataSet.Fields[1].AsInteger)),
                              IbInfo.DataSet.Fields[2].AsInteger]);
    IbInfo.DataSet.Next;
  end;
end;

procedure TAstaDataModule.MetaDataVCLFields(Sender: TObject; U: TUserRecord;
  MetaDataRequest: TAstaMetaData; MetaDataSet: TAstaIODataSet; DataBaseName,
  TableName: String);
begin
  //m.lines.add('Fields Info for ' + TableName);
  IbInfo.GetMetaData(U,MetaDataRequest, DatabaseName, TableName);
  IbInfo.DataSet.First;
  while not IbInfo.DataSet.Eof do
  begin
    MetaDataSet.AppendRecord([IbInfo.DataSet.Fields[0].AsString,
                              Ord(IbInfo.IBTypeToVCLFieldType(IbInfo.DataSet.Fields[1].AsInteger)),
                              IbInfo.DataSet.Fields[2].AsInteger]);
    IbInfo.DataSet.Next;
  end;
end;

procedure TAstaDataModule.IBInfoSetSQL(Sender: TObject; U: TUserRecord;
  Query: TDataSet; SQLString: String);
begin
  TSQLQuery(Query).SQL.Text:=SQLString;
end;

end.
