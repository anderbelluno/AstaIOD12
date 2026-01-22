unit dm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Db,
  AstaIOUserList, AstaIOCustomDataSet, AstaIODBConst,
  AstaIOProvider,
  AstaIOMetaData, AstaIOServerMethod, AstaIOIBInfo, AstaIOIProvider,
  AstaIODBInfo, IBODataset, IB_Session, IB_Components,
  AstaIODataSetProvider;


type
  TIBDataModule = class(TDataModule)
    MetaData: TAstaIOMetaData;
    DBInfo: TAstaIODBInfo;
    Transaction: TIB_Transaction;
    IBConnection: TIB_Connection;
    Session: TIB_Session;
    Query: TIBOQuery;
    ExecQuery: TIBOQuery;
    StoredProc: TIBOStoredProc;
    empquery: TIBOQuery;
    emprovider: TAstaIOProvider;
    procedure DBInfoSetSQL(Sender: TObject; U: TUserRecord;
      Query: TDataSet; SQLString: string);
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
      FDatabaseList: TStringList;
  end;

var
  IBDataModule: TIBDataModule;

implementation
uses AstaIOIBOSupplementDM;
{$R *.DFM}

procedure TIBDataModule.DBInfoSetSQL(Sender: TObject; U: TUserRecord;
  Query: TDataSet; SQLString: string);
begin
  // This is used to assign the sql to the query that does the select from the system tables for metadata
  TIBOQuery(Query).SQL.Text := SQLString;
end;

procedure TIBDataModule.DataModuleCreate(Sender: TObject);
begin
  FDatabaseList := TStringList.Create;
end;

procedure TIBDataModule.DataModuleDestroy(Sender: TObject);
var i: Integer;
begin
  for i := 0 to FDatabaseList.Count - 1 do
    TDBSet(FDatabaseList.Objects[i]).Free;
  FDatabaseList.Free;
  if IBConnection.connected then
    IBConnection.connected := false;
end;

initialization
  IBDataModule := nil;

end.

//Session component must be the first created!!!!

