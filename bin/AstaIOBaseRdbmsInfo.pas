{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10057: AstaIOBaseRdbmsInfo.pas 
{
{   Rev 1.0    4/10/2003 6:30:04 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:36:38 PM  Steve    Version: 1.505
}
unit AstaIOBaseRdbmsInfo;
{*********************************************************}
{*   Copyright (c) 1997-2002 Asta Technology Group Inc.  *}
{*               All rights reserved.                    *}
{*               www.astatech.com                        *}
{*********************************************************}

interface

uses
  Classes,
  DB,
  SysUtils,
  AstaIODataBasePlugin,
  AstaIOCustomDataSet,
  AstaIOSQLUtils,
  AstaIOUserList,
  AstaIODBConst;

type
  TOnSetSQLEvent = procedure(Sender: TObject; U: TUserRecord; Query: TDataSet; SQLString: string) of object;
  TRdbmInfoType=(rdbmOwnerInTableNames);
  TRdbmsInfoTypes= set of TRdbmInfoType;
  TBlobType = (btInternal, btExternal); // Roberto Amorim
type
  TAstaIOBaseRdbmsInfo = class(TComponent)
  private
  protected
    FAbout: string;
    FOnSetSQL: TOnSetSQLEvent;
    FSQL: TStringList;
    FDataSet: TAstaIODataSet;
    FServerDataSet: TComponent;
    FDataBasePlugin: TAstaIODataBasePlugin;
    FCaseSensitive: Boolean;
    FOptions:TRdbmsInfoTypes;
    procedure GetTables; virtual; abstract;
    procedure GetTriggers(ObjectName: string = ''); virtual; abstract;
    procedure GetIndexes(ObjectName: string); virtual; abstract;
    procedure GetFields(ObjectName: string); virtual; abstract;
    procedure GetViews; virtual; abstract;
    procedure GetStoredProcs; virtual; abstract;
    procedure GetForeignKeys(ObjectName: string); virtual; abstract;
    procedure GetSystemTables; virtual; abstract;
    procedure GetPrimeKeys(ObjectName: string); virtual; abstract;
    procedure GetStoredProcColumns(ObjectName: string); virtual; abstract;
    procedure SetDataBasePlugin(Value: TAstaIODataBasePlugin);
    function GetDataBasePlugin: TAstaIODataBasePlugin;
  public
    function IBTypeToVCLFieldType(IBFieldType: Integer): TFieldType; overload;
    function IBTypeToVCLFieldType(IBFieldType: string): TFieldType; overload;

    function PostgreSQLTypeToVCLFieldType(PostgreFieldType: string; var Size: Integer): TFieldType;
    function ASATypeToVCLFieldType(ASAFieldType: string): TFieldType; overload;

    function OracleTypeToVCLFieldType(OracleFieldType: string; Scale, Precision :SmallInt): TFieldType;

    property DataSet: TAstaIODataSet read FDataSet write FDataSet;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetMetaData(U: TUserRecord; MetaDataRequest: TAstaMetaData;
      DataBaseName: string;
      ObjectName: string = ''): TAstaIODataSet;
  published
    property About: string read FAbout write FAbout;
    property OnSetSQL: TOnSetSQLEvent read FOnSetSQL write FOnSetSQL;
    property DataBasePlugin: TAstaIODataBasePlugin read GetDataBasePlugin write SetDataBasePlugin;
    property CaseSensitive: Boolean read FCaseSensitive write FCaseSensitive;
    property Options:TRdbmsInfoTypes read FOptions write FOptions default [];
  end;

implementation
uses AstaIOResources;

function TAstaIOBaseRdbmsInfo.IBTypeToVCLFieldType(IBFieldType: Integer): TFieldType;
begin
  Result := ftUnknown;

  if IBFieldType = 14 then // CHAR
    Result := ftString
  else
  if IBFieldType = 40 then //CSTRING
    Result := ftString
  else
  if IBFieldType = 37 then // VARCHAR
    Result := ftString
  else
  if IBFieldType = 7 then // SMALLINT
    Result := ftSmallInt
  else
  if IBFieldType = 8 then // INTEGER
    Result := ftInteger
  else
  if IBFieldType = 16 then // INT64
    Result := ftLargeInt
  else
  if IBFieldType = 9 then // QUAD
    Result := ftCurrency // ?????
  else
  if IBFieldType = 10 then // FLOAT
    Result := ftFloat
  else
  if IBFieldType = 11 then // D_FLOAT
    Result := ftFloat
  else
  if IBFieldType = 27 then // DOUBLE
    Result := ftFloat
  else
  if IBFieldType = 12 then // DATE
    Result := ftDate
  else
  if IBFieldType = 13 then // TIME
    Result := ftTime
  else
  if IBFieldType = 35 then // TIMESTAMP
    Result := ftDateTime //varbytes
  else
  if IBFieldType = 261 then // BLOB
    Result := ftBlob
end;

function TAstaIOBaseRdbmsInfo.IBTypeToVCLFieldType(IBFieldType: string): TFieldType;
begin
  Result := ftUnknown;

  if IBFieldType = 'TEXT' then
    Result := ftString
  else
  if IBFieldType = 'CHAR' then
    Result := ftString
  else
  if IBFieldType = 'VARCHAR' then
    Result := ftString
  else
  if IBFieldType = 'SHORT' then
    Result := ftSmallInt
  else
  if IBFieldType = 'SMALLINT' then
    Result := ftSmallInt
  else
  if IBFieldType = 'LONG' then
    Result := ftInteger
  else
  if IBFieldType = 'INTEGER' then
    Result := ftInteger
  else
  if IBFieldType = 'INT64' then
    Result := ftLargeInt
  else
  if IBFieldType = 'QUAD' then
    Result := ftCurrency // ?????
  else
  if IBFieldType = 'FLOAT' then
    Result := ftFloat
  else
  if IBFieldType = 'D_FLOAT' then
    Result := ftFloat
  else
  if IBFieldType = 'DOUBLE' then
    Result := ftWord
  else
  if IBFieldType = 'DATE' then
    Result := ftDate
  else
  if IBFieldType = 'TIME' then
    Result := ftTime
  else
  if IBFieldType = 'TIMESTAMP' then
    Result := ftDateTime //from varbytes
  else
  if IBFieldType = 'VARYING' then
    Result := ftString
  else
  if IBFieldType = 'BLOB' then
    Result := ftBlob
  else
  if IBFieldType = 'CSTRING' then
    Result := ftString
  else
  if IBFieldType = 'BLOB_ID' then
    Result := ftInteger
end;

{ TAstaIOBaseRdbmsInfo }

function TAstaIOBaseRdbmsInfo.PostgreSQLTypeToVCLFieldType(PostgreFieldType: string; var Size: Integer): TFieldType;
var
  IsArray: Boolean;
begin
  IsArray := False;

  { If the field name starts with '_', should an array }
  if PostgreFieldType[1] = '_' then
  begin
    IsArray := True;
    Delete(PostgreFieldType, 1, 1);
  end;
  if Size < 0 then Size := 0;
  if (PostgreFieldType = 'interval') or (PostgreFieldType = 'char')
    or (PostgreFieldType = 'varchar') or ((PostgreFieldType = 'text') and (Size > 0)) then
    Result := ftString
  else if PostgreFieldType = 'text' then
    Result := ftMemo
  else if PostgreFieldType = 'oid' then
    Result := ftBlob
  else if PostgreFieldType = 'int2' then
    Result := ftInteger
  else if PostgreFieldType = 'int4' then
    Result := ftInteger
  else if PostgreFieldType = 'int8' then
    Result := ftInteger
  else if (PostgreFieldType = 'float4') or (PostgreFieldType = 'float8')
    or (PostgreFieldType = 'decimal') or (PostgreFieldType = 'numeric') then
    Result := ftFloat
  else if PostgreFieldType = 'money' then
    Result := ftCurrency
  else if PostgreFieldType = 'bool' then
    Result := ftBoolean
  else if (PostgreFieldType = 'datetime') or (PostgreFieldType = 'timestamp') or (PostgreFieldType = 'abstime') then
    Result := ftDateTime
  else if PostgreFieldType = 'date' then
    Result := ftDate
  else if (PostgreFieldType = 'time') then
    Result := ftTime
  else if PostgreFieldType = 'name' then
  begin
    Result := ftString;
    Size := 32;
  end else if PostgreFieldType = 'regproc' then
  begin
    Result := ftString;
  end
  else
  begin
    Result := ftString;
    if Size <= 0 then
      Size := 50;
  end;
  { Fixing array type and subtype }
//  if IsArray then
//  begin
//    ArraySubType := Result;
//  end;
  if (Result = ftString) and (Size = 0) then
    Size := 50;
  if Result <> ftString then Size := 0;
end;

constructor TAstaIOBaseRdbmsInfo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSQL := TStringList.Create;
  FDataSet := TAstaIODataSet.Create(Self);
  FDataBasePlugin := nil;
  FCaseSensitive := True;
  FOptions:=[];
end;

destructor TAstaIOBaseRdbmsInfo.Destroy;
begin
  if FServerDataSet <> nil then TDataSet(FServerDataSet).Close;
  FSQL.Free;
  FDataSet.Close;
  FDataSet.Free;
  inherited;
end;

function TAstaIOBaseRdbmsInfo.GetMetaData(U: TUserRecord; MetaDataRequest: TAstaMetaData;
  DataBaseName, ObjectName: string): TAstaIODataSet;
var
  Valid: Boolean;
  ADataSet: TDataSet;
begin
  Result := nil;
  if not assigned(FDataBasePlugin) then
    DataBaseError(SNoDBPlugin);

  if not FCaseSensitive then
    ObjectName:=UpperCase(ObjectName);
  Valid := True;
  case MetaDataRequest of
    mdTables:
      begin
        GetTables;
      end;

    mdTriggers:
      begin
        GetTriggers(ObjectName);
      end;

    mdIndexes:
      begin
        GetIndexes(ObjectName);
      end;

    mdFields:
      begin
        GetFields(ObjectName);
      end;

    mdViews:
      begin
        GetViews;
      end;

    mdStoredProcs:
      begin
        GetStoredProcs;
      end;

    mdForeignKeys:
      begin
        GetForeignKeys(ObjectName);
      end;

    mdSystemTables:
      begin
        GetSystemTables;
      end;

    mdPrimeKeys:
      begin
        GetPrimeKeys(ObjectName);
      end;

    mdStoredProcColumns:
      begin
        GetStoredProcColumns(ObjectName);
      end;
  else
    Valid := False;
  end;
  if not Valid then exit;

  if not assigned(FOnSetSQL) then
    DataBaseError('OnSetSQL is not assigned');

  ADataSet := nil;
  try
    if Assigned(FDataBasePlugin.OnSupplyDBComponent) then
      FDataBasePlugin.OnSupplyDBComponent(Self, U, DatabaseName,TComponent(ADataSet), tdbSelect, []);

    ADatASet.Close;
    FOnSetSQL(Self, U, ADataSet, FSQL.Text);
    ADatASet.Open;
    FDataSet.Empty;
    FDataSet.CleanCloneFromDataSet(ADataSet);
  finally
    if not Assigned(FDataBasePlugin.OnSupplyDBComponent) then ADataSet.free;
  end;

  Result := FDataSet;
end;

procedure TAstaIOBaseRdbmsInfo.SetDataBasePlugin(Value: TAstaIODataBasePlugin);
begin
  FDataBasePlugin := Value;
end;

function TAstaIOBaseRdbmsInfo.GetDataBasePlugin: TAstaIODataBasePlugin;
begin
  Result := FDataBasePlugin;
end;

function TAstaIOBaseRdbmsInfo.ASATypeToVCLFieldType(
  ASAFieldType: string): TFieldType;
begin
  Result:=ftUnknown;
  if ASAFieldType = 'char' then
    Result:=ftString
  else
  if ASAFieldType = 'binary' then
    Result:=ftVarBytes
  else
  if ASAFieldType = 'text' then
    Result:=ftMemo
  else
  if ASAFieldType = 'tinyint' then
    Result:=ftSmallInt
  else
  if ASAFieldType = 'smallint' then
    Result:=ftSmallInt
  else
  if ASAFieldType = 'int' then
    Result:=ftInteger
  else
  if ASAFieldType = 'bigint' then
    Result:=ftInteger
  else
  if ASAFieldType = 'real' then
    Result:=ftFloat
  else
  if ASAFieldType = 'float' then
    Result:=ftFloat
  else
  if ASAFieldType = 'double' then
    Result:=ftFloat
  else
  if ASAFieldType = 'bit' then
    Result:=ftBoolean
  else
  if ASAFieldType = 'datetime' then
    Result:=ftDateTime
  else
  if ASAFieldType = 'smalldatetime' then
    Result:=ftDateTime
  else
  if ASAFieldType = 'money' then
    Result:=ftCurrency
  else
  if ASAFieldType = 'smallmoney' then
    Result:=ftCurrency
  else
  if ASAFieldType = 'numeric' then
    Result:=ftFloat
  else
  if ASAFieldType = 'decimal' then
    Result:=ftFloat
  else
  if ASAFieldType = 'varchar' then
    Result:=ftString
  else
  if ASAFieldType = 'integer' then
    Result:=ftInteger
end;

function TAstaIOBaseRdbmsInfo.OracleTypeToVCLFieldType(OracleFieldType: string; Scale, Precision :SmallInt): TFieldType;
begin
  OracleFieldType:=UpperCase(OracleFieldType);
  if OracleFieldType = 'CHAR' then
    Result:=ftString
  else
  if OracleFieldType = 'VARCHAR2' then
    Result:=ftString
  else
  if OracleFieldType = 'VARCHAR' then
    Result:=ftString
  else
  if OracleFieldType = 'LONG' then
    Result:=ftMemo
  else
  if OracleFieldType = 'BLOB' then
    Result:=ftBlob
  else
  if OracleFieldType = 'CLOB' then
    Result:=ftBlob
  else
  if OracleFieldType = 'NUMBER' then
  begin
    if Scale > 0 then
      Result:=ftFloat
    else
      Result:=ftInteger
  end
  else
  if OracleFieldType = 'ROWID' then
    Result:=ftMemo
  else
  if OracleFieldType = 'DATE' then
    Result:=ftDateTime
  else
    Result:=ftUnknown;
end;

end.

