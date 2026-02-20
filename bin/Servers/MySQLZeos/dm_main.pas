unit dm_main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Db,
  AstaIOUserList,AstaIOCustomDataSet,AstaIODBConst,
  AstaIOProvider,
  AstaIOMetaData, AstaIOServerMethod, AstaIOIProvider,
  AstaIODBInfo, FMTBcd, SqlExpr, ZQuery, ZMySqlQuery, ZTransact,
  ZMySqlTr, ZConnect, ZMySqlCon;

type
  TAstaDataModule = class(TDataModule)
    MetaData: TAstaIOMetaData;
    Database: TZMySqlDatabase;
    Transaction: TZMySqlTransact;
    Query: TZMySqlQuery;
    dbInfo: TAstaIODBInfo;
    procedure MetaDataDBMSName(Sender: TObject; U: TUserRecord;
      MetaDataSet: TAstaIODataSet);
    procedure DbInfoSetSQL(Sender: TObject; U: TUserRecord;
      Query: TDataSet; SQLString: String);  private
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
  MetaDataSet.AppendRecord(['MySQLZeos',
                            'Database',
                            ExtractFileName(Application.EXEName),
                            'Test'
                            ])

end;


procedure TAstaDataModule.DbInfoSetSQL(Sender: TObject; U: TUserRecord;
  Query: TDataSet; SQLString: String);
begin
  TZMySQLQuery(Query).SQL.Text:=SQLString;

end;

end.
