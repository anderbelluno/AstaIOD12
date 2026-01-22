unit dm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Db,AstaIOUserList,AstaIOCustomDataSet,AstaIODBConst,
  AstaIOProvider,  AstaIOMetaData, AstaIOServerMethod, AstaIOIProvider,
  AstaIODBInfo, ZAbstractRODataset, ZAbstractDataset, ZDataset,
  ZConnection, AstaIODataSetProvider;


type
  TAstaDataModule = class(TDataModule)
    DBInfo: TAstaIODBInfo;
    MetaData: TAstaIOMetaData;
    Database: TZConnection;
    TableQuery: TZQuery;
    Query: TZQuery;
    ColumnQuery: TZQuery;
    procedure DBInfoSetSQL(Sender: TObject; U: TUserRecord;
      Query: TDataSet; SQLString: String);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure CloseConnection;
  end;

var
  AstaDataModule: TAstaDataModule;

implementation
uses mainunit,AstaIOZeos6SupplementDM;
{$R *.DFM}
procedure TAstaDataModule.CloseConnection;
begin
 Database.Connected:=False;
end;

procedure TAstaDataModule.DBInfoSetSQL(Sender: TObject; U: TUserRecord;
  Query: TDataSet; SQLString: String);
begin
  // This is used to assign the sql to the query that does the select from the system tables for metadata
  TZquery(Query).SQL.Text:=SQLString;
end;

end.
