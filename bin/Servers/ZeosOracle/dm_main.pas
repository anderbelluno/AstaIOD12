unit dm_main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Db, AstaIOUserList,AstaIOCustomDataSet,AstaIODBConst, AstaIOProvider,
  AstaIOMetaData, AstaIOServerMethod, AstaIOOracleInfo, AstaIOIProvider,
  AstaIODBInfo, AstaIODataSetProvider, ZConnect, ZOraSqlCon, ZTransact,
  ZOraSqlTr, ZQuery, ZOraSqlQuery;

type
  TAstaDataModule = class(TDataModule)
    MetaData: TAstaIOMetaData;
    DBInfo: TAstaIODBInfo;
    DataBase: TZOraSqlDatabase;
    Transaction: TZOraSqlTransact;
    Query: TZOraSqlQuery;
    procedure MetaDataDBMSName(Sender: TObject; U: TUserRecord;
      MetaDataSet: TAstaIODataSet);
    procedure DBInfoSetSQL(Sender: TObject; U: TUserRecord;
      Query: TDataSet; SQLString: String);
    procedure provBeforeInsert(Sender: TObject; U: TUserRecord;
      ExecQuery: TComponent; CurrentValueDataSet: TDataSet;
      var Handled: Boolean);
    procedure provBeforeUpdate(Sender: TObject; U: TUserRecord;
      ExecQuery: TComponent; OriginalValueDataSet, CurrentValueDataSet,
      ServerValueDataSet: TDataSet; var Handled: Boolean);

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
                            ExtractFileName(Database.Database),
                            'Some info',
                            ExtractFileName(Application.EXEName)])
end;

procedure TAstaDataModule.DBInfoSetSQL(Sender: TObject; U: TUserRecord;
  Query: TDataSet; SQLString: String);
begin
  TZOraSqlQuery(Query).SQL.Text:=SQLString;
end;

procedure TAstaDataModule.provBeforeInsert(Sender: TObject; U: TUserRecord;
  ExecQuery: TComponent; CurrentValueDataSet: TDataSet;
  var Handled: Boolean);
begin
  handled:=True;
end;

procedure TAstaDataModule.provBeforeUpdate(Sender: TObject; U: TUserRecord;
  ExecQuery: TComponent; OriginalValueDataSet, CurrentValueDataSet,
  ServerValueDataSet: TDataSet; var Handled: Boolean);
begin
  if OriginalValueDataSet.FieldByName('CONTACT_LAST').AsString = 'Brown' then
    Handled:=True
  else
    Handled:=False;

  if Handled then exit;
  f_main.LogIt('Old = ' + OriginalValueDataSet.FieldByName('CONTACT_LAST').AsString);
  f_main.LogIt('Current = ' + CurrentValueDataSet.FieldByName('CONTACT_LAST').AsString);
end;

end.
