unit dm_main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  DB,
  AstaIOUserList,AstaIOCustomDataSet,AstaIODBConst,
  AstaIOMetaData, AstaIOIBInfo,
  AstaIODBInfo, FIBQuery, pFIBQuery, FIBDatabase, pFIBDatabase,
  FIBDataSet, pFIBDataSet, pFIBStoredProc;

type
  TAstaDataModule = class(TDataModule)
    MetaData: TAstaIOMetaData;
    DBInfo: TAstaIODBInfo;
    Database: TpFIBDatabase;
    Query: TpFIBDataSet;
    Transaction: TpFIBTransaction;
    StoredProc: TpFIBStoredProc;
    ExecQuery: TpFIBQuery;
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
  TpFIBDataSet(Query).SelectSQL.Text:=SQLString;
end;

end.


