{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10402: dbisam_main.pas 
{
{   Rev 1.0    4/10/2003 6:34:02 AM  Steve
}
unit dbisam_main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  AstaIOUserList,AstaIOCustomDataSet,AstaIODBConst,
  AstaIOProvider,AstaIOMetaData, AstaIOServerMethod,  AstaIOIProvider,
  db,DBISAMTb, AstaIODataSetProvider;

type
  TAstaDbisamDataModule = class(TDataModule)
    Query: TDBISAMQuery;
    Customerquery: TDBISAMQuery;
    ProviderQuery: TDBISAMQuery;
    ServerMethodQuery: TDBISAMQuery;
    prov: TAstaIOProvider;
    servermethod1: TAstaIOServerMethodResultSet;
    servermethod2: TAstaIOServerMethodExec;
    MetaData: TAstaIOMetaData;
    iprov1: TAstaIOIProvider;
    iprov_query1: TDBISAMQuery;
    iprov2: TAstaIOIProvider;
    iprov_query2: TDBISAMQuery;
    iprov3: TAstaIOIProvider;
    Database: TDBISAMDatabase;
    Session1: TDBISAMSession;
    procedure IBInfoSetSQL(Sender: TObject; Query: TDataSet;
      SQLString: String);
    procedure MetaDataDBMSName(Sender: TObject; U: TUserRecord;
      MetaDataSet: TAstaIODataSet);
    procedure MetaDataTables(Sender: TObject; U: TUserRecord;
      MetaDataRequest: TAstaMetaData; MetaDataSet: TAstaIODataSet;
      DataBaseName, TableName: String);
    procedure servermethod2Action(Sender: TObject);
    procedure DataModuleCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AstaDbisamDataModule: TAstaDbisamDataModule;

implementation
uses AstaIOUtil;
{$R *.DFM}

procedure TAstaDbisamDataModule.IBInfoSetSQL(Sender: TObject; Query: TDataSet;
  SQLString: String);
begin
  TDbisamQuery(Query).SQL.Text:=SQLString;
end;

procedure TAstaDbisamDataModule.MetaDataDBMSName(Sender: TObject; U: TUserRecord;
  MetaDataSet: TAstaIODataSet);
begin
  MetaDataSet.AppendRecord(['ServerName',
                            'Dbisam',
                            ExtractFileName(Application.EXEName),
                            'Test'
                            ])

end;

procedure TAstaDbisamDataModule.MetaDataTables(Sender: TObject; U: TUserRecord;
  MetaDataRequest: TAstaMetaData; MetaDataSet: TAstaIODataSet; DataBaseName,
  TableName: String);
Var
i:Integer;
List:TStringList;
begin
  List:=TStringList.Create;
  try
  Session1.GetTableNames(Database.DataBaseName, List);
   for i:=0 to List.count-1 do
      if pos('.',List[i])>0 then
      MetaDataSet.Appendrecord([StringBeforeToken(list[i],'.')]) else
      MetaDataSet.Appendrecord([list[i]]);
  finally
    List.Free;
  end;
end;

procedure TAstaDbisamDataModule.servermethod2Action(Sender: TObject);
begin
  servermethod2.Params[0].AsInteger:=servermethod2.Params[0].AsInteger * 10;
  servermethod2.Params[1].AsString:=DateTimeToStr(now);
end;

procedure TAstaDbisamDataModule.DataModuleCreate(Sender: TObject);
Var
 i:integer;
begin
DataBase.DataBaseName:=DataBase.DataBaseName+floatToStr(now);
for i:=0 to componentcount-1 do
 if components[i] is TDbisamQuery then
  TDbisamQuery(Components[i]).DatabaseName:=Database.DatabaseName;
end;

end.
