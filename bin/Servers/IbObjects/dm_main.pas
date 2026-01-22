unit dm_main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
   Db,
  AstaIOUserList,AstaIOCustomDataSet,AstaIODBConst,
  AstaIOProvider,
  AstaIOMetaData, AstaIOServerMethod, AstaIOIBInfo, AstaIOIProvider,
  AstaIODBInfo, IBODataset, IB_Session, IB_Components;

type
  TAstaDataModule = class(TDataModule)
    MetaData: TAstaIOMetaData;
    DBInfo: TAstaIODBInfo;
    Transaction: TIB_Transaction;
    Database: TIB_Connection;
    Session: TIB_Session;
    Query: TIBOQuery;
    ExecQuery: TIBOQuery;
    StoredProc: TIBOStoredProc;
    procedure MetaDataDBMSName(Sender: TObject; U: TUserRecord;
      MetaDataSet: TAstaIODataSet);
    procedure DBInfoSetSQL(Sender: TObject; U: TUserRecord;
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
{$R *.DFM}

procedure TAstaDataModule.MetaDataDBMSName(Sender: TObject; U: TUserRecord;
  MetaDataSet: TAstaIODataSet);
begin
  MetaDataSet.AppendRecord([ExtractFileName(Application.EXEName),
                            ExtractFileName(Database.DatabaseName),
                            'Some info',
                            ExtractFileName(Application.EXEName)])
end;

procedure TAstaDataModule.DBInfoSetSQL(Sender: TObject; U: TUserRecord;
  Query: TDataSet; SQLString: String);
begin
  // This is used to assign the sql to the query that does the select from the system tables for metadata
  TIBOQuery(Query).SQL.Text:=SQLString;
end;

end.

Session component must be the first created!!!!
