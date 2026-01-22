unit dm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  IBDatabase, Db, IBStoredProc, IBCustomDataSet, IBQuery,
  AstaIOUserList,AstaIOCustomDataSet,AstaIODBConst,
  AstaIOProvider,
  AstaIOMetaData, AstaIOServerMethod, AstaIOIBInfo, AstaIOIProvider,
  IBTable, AstaIODBInfo, AstaIODataSetProvider;


type
  TIBXDataModule = class(TDataModule)
    Query: TIBQuery;
    StoredProc: TIBStoredProc;
    IBConnection: TIBDatabase;
    Transaction: TIBTransaction;
    MetaData: TAstaIOMetaData;
    DBInfo: TAstaIODBInfo;
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
  IBXDataModule: TIBXDataModule;

implementation
uses AstaIOIBXSupplementDM;
{$R *.DFM}

procedure TIBXDataModule.MetaDataDBMSName(Sender: TObject; U: TUserRecord;
  MetaDataSet: TAstaIODataSet);
begin
  MetaDataSet.AppendRecord([ExtractFileName(Application.EXEName),
                            ExtractFileName(IBConnection.DatabaseName),
                            'Some info',
                            ExtractFileName(Application.EXEName)])
end;

procedure TIBXDataModule.DBInfoSetSQL(Sender: TObject; U: TUserRecord;
  Query: TDataSet; SQLString: String);
begin
  // This is used to assign the sql to the query that does the select from the system tables for metadata
  TIBQuery(Query).SQL.Text:=SQLString;
end;


end.
