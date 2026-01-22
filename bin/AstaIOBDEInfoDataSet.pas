{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10059: AstaIOBDEInfoDataSet.pas 
{
{   Rev 1.0    4/10/2003 6:30:06 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:36:38 PM  Steve    Version: 1.505
}
{*********************************************************}
{*     Copyright (c) 1997-2002 Asta Technology Group Inc *}
{*                 All rights reserved.                  *}
{*                 www.astatech.com                      *}
{*********************************************************}

unit AstaIOBDEInfoDataSet;
{.$D+,L+}
interface
uses sysutils, classes, db, BDE, dbtables, AstaIOCustomDataSet, dbcommon;

type

  BDEInfoType = (BdeNOInfo, BDEUserInfo, BDEFieldInfo, BDEIndexInfo, BDELockList, BDEOpenTAbles,
 {BDEForeignKeys,} BDEStoredProcParams, BDEStoredProcedures, BDESystemStoredProcedures);
  TIdapiUtil = class
    FDisposeHandle: Boolean;
    DBName: pchar;
    hdb: hDBIDb; {Handle to the database.}
  public
    constructor Create(DataBaseName, PassWord: string);
    constructor CreateWithHandle(AHandle: HdbIdb);
    destructor destroy; override;
    function OpenTablesCursor(Mask: PChar): HDBICur;
    function FieldAsCursor(Tablename: string): HDbiCur;
    function IndexListAsCursor(Tablename: string): HDbiCur;
    function StoredProcedures(SystemTables: Boolean): HDBiCur;
    function StoredProcedureParamInfo(TableName: string): HDbiCur;
    function ForeignKeys(TableName: string): HDbiCur;
    function GetFileServerType: pchar;
  end;

  TAstaBDEInfoTable = class(TTable)
  private
    FIdapiUtil: TIdapiUtil;
    FPassWord: string;
    FBDEInfo: BDEInfoType;
    function DoIdapiUtilOpen: HDBICur;
  protected
    function CreateHandle: HDBICur; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property BDEInfo: BDEInfoType read FBDEInfo write FBDEInfo default BDENoInfo;
    property PassWord: string read FPassWord write fPassword;
  end;
procedure Register;

procedure BDEInfoToStoredProcAstaInfoTable(BDEInfo: TDataSet; AstaInfo: TAstaIOCustomDataSet);

function BDEFieldtypeIntegerToVCLType(FType, SubType: Integer): TFieldtype;
procedure IndexInfoToDataSet(IndexDS, TargetDS: TDataSet);
implementation

uses dbiprocs, wintypes, dialogs;

function CheckBDEError(rslt: Integer; ShowError: Boolean): Boolean;
var
  szErrMsg: array[0..255] of char;
begin
//try
  result := rslt = 0;
  if result then exit;
  dbiGetErrorString(rslt, szErrMsg);
// except
  raise EDataBaseError.Create(szerrmsg);
//end;
end;

constructor TIdapiUtil.Create(DataBaseName, PassWord: string);
begin
  inherited create;
  FdisposeHandle := True;
  getmem(dbname, length(databasename) + 1);
  strpcopy(dbname, databasename);
  CheckBDEError(DbiOpenDatabase(dbName, nil, dbiREADONLY, dbiOPENSHARED, pchar(password), 0, nil, nil, hDb), True);
end;

constructor TIdapiUtil.CreateWithHandle(AHandle: Hdbidb);
begin
  inherited create;
  hdb := AHandle;
  dbName := nil;
  FDisposeHandle := FAlse;
end;

destructor TIdapiUtil.destroy;
begin
  if dbname < nil then freemem(dbName, strlen(dbname) + 1);
  if FDisposeHandle then DbiCloseDatabase(hDb);
  inherited destroy;
end;

function TIdapiUtil.OpenTablesCursor(Mask: Pchar): HDBICur;
begin
  CheckBDEError(DbiOpenTableList(hDb, True, False, MASK, result), True);
end;

function TIdapiUtil.StoredProcedureParamInfo(TableName: string): HDbiCur;
begin
  CheckBDEError(DbiOpenSPParamList(hdb, pchar(TableName), False, 0, result), True);
end;

function TIdapiUtil.StoredProcedures(SystemTables: Boolean): HDBICur;
begin
  CheckBDEError(DbiOpenSPList(hdb, False, SystemTables, nil, result), True);
end;

function TIdapiUtil.IndexListAsCursor(Tablename: string): HDbiCur;
var
  tname: array[0..60] of char;
begin
{strpcopy(tname,tablename);}
  AnsiToNative(Session.Locale, tablename, tname, SizeOf(tname) - 1);
  checkBdeError(DbiOpenIndexList(hDb, tname, GetFileServerType, Result), True);
end;

function TIdapiUtil.ForeignKeys(Tablename: string): HDbiCur;
begin
  result := nil;
  CheckBDEError(DbiOpenRintList(hDb, pchar(TableName), GetFileServerType, Result), True)
end;


function TIdapiUtil.FieldAsCursor(Tablename: string): HDbiCur;
var
  tname: array[0..60] of char;
begin
  result := nil;
{strpcopy(tname,tablename);}
  AnsiToNative(Session.Locale, tablename, tname, SizeOf(tname) - 1);
  CheckBDEError(DbiOpenFieldList(hDb, tname, GetFileServerType, False, result), True);
end;

function TAstaBDeInfoTable.DoIdapiUtilOpen: HDBICur;
begin
  Result := nil;
  FIdapiUtil.Free;
  FIdapiUtil := TIdapiUtil.CreateWithHandle(DBHandle);
  case FBDEInfo of
    BDEFieldInfo: Result := FIdapiUtil.FieldAsCursor(Tablename);
    BDEIndexInfo: Result := FIdapiUtil.IndexListAsCursor(TableName);
    BdeOpenTables: Result := FIdapiUtil.OpenTablesCursor(nil);
{  BDEForeignKeys:result:=FIdapiUtil.ForeignKeys(TableName);}
    BDEStoredProcParams: result := FIdapiUtil.StoredProcedureParamInfo(TableName);
    BDEStoredProcedures: result := FIdapiUtil.StoredProcedures(False);
    BDESystemStoredProcedures: result := FIdapiUtil.StoredProcedures(True);
  {  BdeLockList  :Result:=Idapi.LockListCursor(Tablename);}
  end;
  if result = nil then
    raise EDataBaseError.Create('Unable To Open BDE Cursor Handle');

end;

function TIdapiUtil.GetFileServerType: pchar;
begin
  result := szparadox;
end;

function UsersListAsCursor: HDBICur;
begin
  CheckBDEError(DbiOpenUserList(result), True);
end;

constructor TAstaBDeInfoTable.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FIdapiutil := nil;
end;

destructor TAstaBDeInfoTable.Destroy;
begin
  FIdapiUtil.Free;
  inherited destroy;
end;


function TAstaBDeInfoTable.CreateHandle: HDBICur;
begin
// result:=nil;
  case FBDEInfo of
    BDEUserInfo: Result := UsersListAsCursor;
    BDEFieldInfo..
      BDEStoredProcedures: REsult := DoIdapiUtilOpen;
  else Result := inherited CreateHandle;
  end;
end;

procedure BDEInfoToStoredProcAstaInfoTable(BDEInfo: TDataSet; AstaInfo: TAstaIOCustomDataSet);
var
  fType: Integer;
  ParamDataType: TFieldType;
begin
  AstaInfo.Open;
  AstaInfo.Empty;
  BDEInfo.First;
  while not BDEInfo.EOF do begin
    AstaInfo.Append;
    AstaInfo.FieldByName('Name').AsString := BdeInfo.FieldByName('Name').AsString;
    AstaInfo.FieldbyName('InputType').AsInteger := BDEInfo.FieldByName('Type').AsInteger;
    FType := BDEInfo.FieldByName('DataType').AsInteger;
  //changed 01/13/99 to stream back param types correctly
    if Ftype < MAXLOGFLDTYPES then ParamDataType := DataTypeMap[FType]
    else ParamDataType := ftUnknown;
    if (FType = fldFLOAT) and (BDEInfo.FieldByName('DataSubType').AsInteger = fldstMONEY) then
      ParamDataType := ftCurrency;
    AstaInfo.FieldbyName('DataType').AsInteger := ord(ParamDataType);
    AstaInfo.Post;
    BDEInfo.Next;
  end;
end;

function BDEFieldtypeIntegerToVCLType(FType, SubType: Integer): TFieldtype;
begin
  result := ftunknown;
  if Ftype < MAXLOGFLDTYPES then result := DataTypeMap[FType];
  if (FType = fldFLOAT) and (Subtype = fldstMONEY) then
    result := ftCurrency;
end;

procedure IndexInfoToDataSet(IndexDS, TargetDS: TDataSet);
begin
  with TargetDS do begin
    close;
    Open;
    TAstaIOCustomDataSet(TargetDS).Empty;
    while not IndexDS.Eof do begin
      Append;
      FieldbyName('Table').AsString := IndexDs.Fields[0].AsString;
      FieldByName('Link Column').AsString := IndexDS.Fields[1].AsString;
      FieldByName('Local Column').AsString := IndexDS.Fields[2].AsString;
      FieldByName('Local Index').AsString := IndexDS.Fields[3].AsString;
      Post;
      IndexDS.Next;
    end;
  end;
end;

procedure Register;
begin
  RegisterComponents('AstaIO', [TAstaIOBDEInfoDataSet]);
end;

end.

