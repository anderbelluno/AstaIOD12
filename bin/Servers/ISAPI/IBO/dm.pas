unit dm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  IBDatabase, Db, IBStoredProc, IBCustomDataSet, IBQuery,
  AstaIOUserList,AstaIOCustomDataSet,AstaIODBConst,
  AstaIOProvider,AstaIOSQLGenerator, AstaIOMetaData, IB_Components,
  IBODataset, AstaIOServerMethod, AstaIODataSetProvider,
  AstaIOBaseRdbmsInfo, AstaIOIBInfo, AstaIOIBOSupplementDM, AstaIODBInfo;

type
  TAstaDataModule = class(TDataModule)
    CustProvider: TAstaIOProvider;
    Increment: TAstaIOServerMethodExec;
    empprovider: TAstaIOProvider;
    Connection: TIB_Connection;
    Query: TIBOQuery;
    ExecQuery: TIBOQuery;
    StoredProc: TIBOQuery;
    IBOTransaction: TIBOTransaction;
    MetaData: TAstaIOMetaData;
    DBinfo: TAstaIODBInfo;

    procedure IncrementAction(Sender: TObject; U: TUserRecord;
      Params: TParams);
    procedure DataModuleCreate(Sender: TObject);
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

{$R *.DFM}
procedure TAstaDataModule.CloseConnection;
begin
 Connection.Connected:=False;
end;



procedure TAstaDataModule.IncrementAction(Sender: TObject; U: TUserRecord;
  Params: TParams);
begin
 params.ParamByName('outPut').AsInteger:=Params.ParambyName('Intput').Asinteger+1;
end;

procedure TAstaDataModule.DataModuleCreate(Sender: TObject);
begin
    Connection.Connected := False;
    Connection.Username := 'sysdba';
    Connection.Password := 'masterkey';
    Connection.Path := 'e:\herbdata\merlinsqldat.gdb';
    Connection.Connected := True;
end;

procedure TAstaDataModule.DBInfoSetSQL(Sender: TObject; U: TUserRecord;
  Query: TDataSet; SQLString: String);
begin
  // This is used to assign the sql to the query that does the select from the system tables for metadata
  TIBOQuery(Query).SQL.Text := SQLString;
end;

end.
