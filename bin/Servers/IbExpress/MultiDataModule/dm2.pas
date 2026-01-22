unit dm2;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  IBDatabase, Db, IBStoredProc, IBCustomDataSet, IBQuery, 
  AstaIOUserList,AstaIOCustomDataSet,AstaIODBConst,
  AstaIOProvider,
  AstaIOMetaData, AstaIOServerMethod, AstaIOIBInfo, AstaIOIProvider,
  IBTable, AstaIODBInfo;

type
  TAstaExtraDataModule = class(TDataModule)
    AlienProviderQuery: TIBQuery;
    AlienProvider: TAstaIOProvider;
    AlienServerMethod: TAstaIOServerMethodResultSet;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AstaExtraDataModule: TAstaExtraDataModule;

implementation
{$R *.DFM}


end.
