{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10129: AstaIODBInfo.pas 
{
{   Rev 1.0    4/10/2003 6:30:44 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:37:06 PM  Steve    Version: 1.505
}
unit AstaIODBInfo;
{*********************************************************}
{*   Copyright (c) 1997-2002 Asta Technology Group Inc.  *}
{*               All rights reserved.                    *}
{*               www.astatech.com                        *}
{*********************************************************}

interface

uses Classes, Sysutils, DB,
     AstaIOCustomDataSet,
     AstaIOUserList,
     AstaIOBaseRdbmsInfo,
     AstaIODataBasePlugin,
     AstaIODBConst;

type
  TOnSetSQLEvent = procedure(Sender: TObject; U :TUserRecord; Query: TDataSet; SQLString: string) of object;

type
  TAstaIODBInfo = class(TComponent)
  private
    FAbout: String;
    FDataBasePlugin: TAstaIODataBasePlugin;
    FRdbmsInfoKind: TAstaRdbmsInfoKind;
    FOnSetSQL: TOnSetSQLEvent;
    FInfo :TAstaIOBaseRdbmsInfo;
    FDBInfoSupply: TDBInfoSupplySet;
    FCaseSensitive: Boolean;

    function GetDataBasePlugin: TAstaIODataBasePlugin;
    procedure SetDataBasePlugin(Value: TAstaIODataBasePlugin);
  protected
    procedure GetDatabaseInfo(U: TUserRecord; MetaDataRequest: TAstaMetaData;
                              MetaDataSet: TAstaIODataSet; DataBaseName, TableName: string);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure GetDBInfo(Sender: TObject; U: TUserRecord; MetaDataRequest: TAstaMetaData;
                        MetaDataSet: TAstaIODataSet; DataBaseName, TableName: string);
  published
    property About: String read FAbout write FAbout;
    property DataBasePlugin: TAstaIODataBasePlugin read GetDataBasePlugin write SetDataBasePlugin;
    property RdbmsInfoKind: TAstaRdbmsInfoKind read FRdbmsInfoKind write FRdbmsInfoKind;
    property DBInfoSupply: TDBInfoSupplySet read FDBInfoSupply write FDBInfoSupply;
    property CaseSensitive: Boolean read FCaseSensitive write FCaseSensitive;
    property OnSetSQL: TOnSetSQLEvent read FOnSetSQL write FOnSetSQL;
  end;

implementation
uses AstaIOResources,
     AstaIOIBInfo,
     AstaIOMSSqlInfo,
     AstaIOASEInfo,
     AstaIOMySQLInfo,
     AstaIOOracleInfo,
     AstaIOPostgreSQLInfo,
     AstaIOASAInfo,
     AstaIODB2Info;

{ TAstaIODBInfo }

constructor TAstaIODBInfo.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRdbmsInfoKind := rdbInterbase;
  FDataBasePlugin := nil;
  FDBInfoSupply := [isTables, isSysTables, isFields, isFkeys, isPKeys, isTriggers, isIndexes, isViews, isSProcs, isSProcCols];
  FCaseSensitive := False;
end;

destructor TAstaIODBInfo.Destroy;
begin
  inherited;
end;

procedure TAstaIODBInfo.SetDataBasePlugin(Value: TAstaIODataBasePlugin);
begin
  FDataBasePlugin := Value;
end;

function TAstaIODBInfo.GetDataBasePlugin: TAstaIODataBasePlugin;
begin
  Result := FDataBasePlugin;
end;

procedure TAstaIODBInfo.GetDBInfo(Sender: TObject; U: TUserRecord; MetaDataRequest: TAstaMetaData;
                                  MetaDataSet: TAstaIODataSet; DataBaseName, TableName: string);
begin
  FInfo:=nil;
  try
    case RdbmsInfoKind of
      rdbInterbase   :FInfo:=TAstaIOIBInfo.Create(Self);
      rdbASE         :FInfo:=TAstaIOASEInfo.Create(Self);
      rdbASA         :FInfo:=TAstaIOASAInfo.Create(Self);
      rdbMSSql       :FInfo:=TAstaIOMSSqlInfo.Create(Self);
      rdbMySql       :FInfo:=TAstaIOMySQLInfo.Create(Self);
      rdbOracle      :FInfo:=TAstaIOOracleInfo.Create(Self);
      rdbPostgreSQL  :FInfo:=TAstaIOPostgreSQLInfo.Create(Self);
    else
      DatabaseError(SDbInfoRdbmsNotSupported+' but API calls may be supported like in Dbexpress or ADO');
    end;
    GetDatabaseInfo(U, MetaDataRequest, MetaDataSet, DataBaseName, TableName);
  finally
    if Assigned(FInfo) then FInfo.Free;
  end;
end;

procedure TAstaIODBInfo.GetDatabaseInfo(U: TUserRecord; MetaDataRequest: TAstaMetaData;
                                        MetaDataSet: TAstaIODataSet; DataBaseName, TableName: string);
var TmpInt   :Integer;
    TmpFieldType: TFieldType;
    TmpSize: Integer;
begin
  FInfo.CaseSensitive:=FCaseSensitive;
  FInfo.DataBasePlugin:=FDataBasePlugin;
  FInfo.OnSetSQL:=FOnSetSQL;
  FInfo.GetMetaData(U, MetaDataRequest, DatabaseName, TableName);
  FInfo.DataSet.First;

  case MetaDataRequest of
    mdTables:
      begin
        while not FInfo.DataSet.Eof do
        begin
          if FRdbmsInfoKind in [rdbOracle, rdbInterbase] then
            MetaDataSet.AppendRecord([Trim(FInfo.DataSet.Fields[0].AsString),
                                      Trim(FInfo.DataSet.Fields[1].AsString),
                                      Trim(FInfo.DataSet.Fields[2].AsString)])
          else
            MetaDataSet.AppendRecord([Trim(FInfo.DataSet.Fields[0].AsString)]);
          FInfo.DataSet.Next;
        end;
      end;

    mdTriggers:
      begin
        while not FInfo.DataSet.Eof do
        begin
          MetaDataSet.AppendRecord([Trim(FInfo.DataSet.Fields[0].AsString),
                                   Trim(FInfo.DataSet.Fields[1].AsString),
                                   Trim(FInfo.DataSet.Fields[2].AsString),
                                   Trim(FInfo.DataSet.Fields[3].AsString)]);
          FInfo.DataSet.Next;
        end;
      end;

    mdIndexes:
      begin
        while not FInfo.DataSet.Eof do
        begin
          MetaDataSet.AppendRecord([Trim(FInfo.DataSet.Fields[0].AsString)]);
          FInfo.DataSet.Next;
        end;
      end;

    mdFields:
      begin
        while not FInfo.DataSet.Eof do
        begin
          if FRdbmsInfoKind = rdbInterbase then
            MetaDataSet.AppendRecord([Trim(FInfo.DataSet.Fields[0].AsString),
                                     Ord(FInfo.IBTypeToVCLFieldType(FInfo.DataSet.Fields[1].AsInteger)),
                                     FInfo.DataSet.Fields[2].AsInteger,
                                     FInfo.DataSet.Fields[5].AsInteger])
          else
          if FRdbmsInfoKind = rdbASA then
            MetaDataSet.AppendRecord([Trim(FInfo.DataSet.Fields[0].AsString),
                                     Ord(FInfo.ASATypeToVCLFieldType(FInfo.DataSet.Fields[1].AsString)),
                                     FInfo.DataSet.Fields[2].AsInteger])
          else
          if FRdbmsInfoKind = rdbASE then
            MetaDataSet.AppendRecord([Trim(FInfo.DataSet.Fields[0].AsString),
                                     Ord(FInfo.ASATypeToVCLFieldType(FInfo.DataSet.Fields[1].AsString)),
                                     FInfo.DataSet.Fields[2].AsInteger,
                                     FInfo.DataSet.Fields[3].AsInteger])
          else
          if FRdbmsInfoKind = rdbMSSQL then
            MetaDataSet.AppendRecord([Trim(FInfo.DataSet.Fields[0].AsString),
                                     Ord(FInfo.ASATypeToVCLFieldType(FInfo.DataSet.Fields[1].AsString)),
                                     FInfo.DataSet.Fields[2].AsInteger])
          else
          if FRdbmsInfoKind = rdbOracle then
            MetaDataSet.AppendRecord([Trim(FInfo.DataSet.Fields[0].AsString),
                                     Ord(FInfo.OracleTypeToVCLFieldType(FInfo.DataSet.Fields[1].AsString,
                                                                        FInfo.DataSet.Fields[3].AsInteger,
                                                                        FInfo.DataSet.Fields[4].AsInteger)),
                                     FInfo.DataSet.Fields[2].AsInteger]);

          if FRdbmsInfoKind = rdbPostgreSQL then
          begin
            tmpSize := FInfo.DataSet.Fields[2].AsInteger;
            TmpFieldType:= FInfo.PostgreSQLTypeToVCLFieldType(FInfo.DataSet.Fields[3].AsString, TmpSize);

            MetaDataSet.AppendRecord([Trim(FInfo.DataSet.Fields[0].AsString),
                                     ord(TmpFieldType),
                                     TmpSize,
                                     FInfo.DataSet.Fields[5].AsInteger]);

          end;
          FInfo.DataSet.Next;
        end;
      end;

    mdViews:
      begin
        while not FInfo.DataSet.Eof do
        begin
          if FRdbmsInfoKind in [rdbOracle, rdbInterbase] then
            MetaDataSet.AppendRecord([Trim(FInfo.DataSet.Fields[0].AsString),
                                      Trim(FInfo.DataSet.Fields[1].AsString),
                                      Trim(FInfo.DataSet.Fields[2].AsString)])
          else
            MetaDataSet.AppendRecord([Trim(FInfo.DataSet.Fields[0].AsString)]);
          FInfo.DataSet.Next;
        end;
      end;

    mdStoredProcs:
      begin
        while not FInfo.DataSet.Eof do
        begin
          if FRdbmsInfoKind in [rdbOracle, rdbInterbase] then
            MetaDataSet.AppendRecord([Trim(FInfo.DataSet.Fields[0].AsString),
                                      Trim(FInfo.DataSet.Fields[1].AsString),
                                      Trim(FInfo.DataSet.Fields[2].AsString)])
          else
            MetaDataSet.AppendRecord([Trim(FInfo.DataSet.Fields[0].AsString)]);
          FInfo.DataSet.Next;
        end;
      end;

    mdForeignKeys:
      begin
        while not FInfo.DataSet.Eof do
        begin
          MetaDataSet.AppendRecord([Trim(FInfo.DataSet.Fields[0].AsString),
                                   Trim(FInfo.DataSet.Fields[1].AsString),
                                   Trim(FInfo.DataSet.Fields[2].AsString)]);
          FInfo.DataSet.Next;
        end;
      end;

    mdSystemTables:
      begin
        while not FInfo.DataSet.Eof do
        begin
          if FRdbmsInfoKind in [rdbOracle, rdbInterbase] then
            MetaDataSet.AppendRecord([Trim(FInfo.DataSet.Fields[0].AsString),
                                      Trim(FInfo.DataSet.Fields[1].AsString),
                                      Trim(FInfo.DataSet.Fields[2].AsString)])
          else
            MetaDataSet.AppendRecord([Trim(FInfo.DataSet.Fields[0].AsString)]);
          FInfo.DataSet.Next;
        end;
      end;

    mdPrimeKeys:
      begin
        while not FInfo.DataSet.Eof do
        begin
          MetaDataSet.AppendRecord([Trim(FInfo.DataSet.Fields[0].AsString)]);
          FInfo.DataSet.Next;
        end;
      end;

    mdStoredProcColumns:
      begin
        while not FInfo.DataSet.Eof do
        begin
          if FRdbmsInfoKind = rdbInterbase then
            MetaDataSet.AppendRecord([Trim(FInfo.DataSet.Fields[0].AsString),
                                      Ord(FInfo.IBTypeToVCLFieldType(FInfo.DataSet.Fields[1].AsInteger)),
                                      FInfo.DataSet.Fields[2].AsInteger,
                                      FInfo.DataSet.Fields[3].AsInteger])
          else
          if FRdbmsInfoKind = rdbASA then
            MetaDataSet.AppendRecord([Trim(FInfo.DataSet.Fields[0].AsString),
                                      Ord(FInfo.ASATypeToVCLFieldType(FInfo.DataSet.Fields[1].AsString)),
                                      FInfo.DataSet.Fields[2].AsInteger,
                                      FInfo.DataSet.Fields[3].AsInteger])
          else
          if FRdbmsInfoKind = rdbASE then
            MetaDataSet.AppendRecord([Trim(FInfo.DataSet.Fields[0].AsString),
                                      Ord(FInfo.ASATypeToVCLFieldType(FInfo.DataSet.Fields[1].AsString)),
                                      FInfo.DataSet.Fields[2].AsInteger,
                                      FInfo.DataSet.Fields[3].AsInteger,
                                      FInfo.DataSet.Fields[4].AsInteger])
          else
          if FRdbmsInfoKind = rdbMSSQL then
            MetaDataSet.AppendRecord([Trim(FInfo.DataSet.Fields[0].AsString),
                                      Ord(FInfo.ASATypeToVCLFieldType(FInfo.DataSet.Fields[1].AsString)),
                                      FInfo.DataSet.Fields[2].AsInteger,
                                      FInfo.DataSet.Fields[3].AsInteger])
          else
          if FRdbmsInfoKind = rdbOracle then
          begin
            TmpInt:=0;
            if UpperCase(FInfo.DataSet.Fields[3].AsString) = 'IN' then
              TmpInt:=Ord(ptInput)
            else
            if UpperCase(FInfo.DataSet.Fields[3].AsString) = 'OUT' then
              TmpInt:=Ord(ptOutput)
            else
            if UpperCase(FInfo.DataSet.Fields[3].AsString) = 'IN/OUT' then
              TmpInt:=Ord(ptInputOutput);

            MetaDataSet.AppendRecord([Trim(FInfo.DataSet.Fields[0].AsString),
                                      Ord(FInfo.OracleTypeToVCLFieldType(FInfo.DataSet.Fields[1].AsString,
                                                                         FInfo.DataSet.Fields[4].AsInteger,
                                                                         FInfo.DataSet.Fields[5].AsInteger)),
                                      FInfo.DataSet.Fields[2].AsInteger,
                                      TmpInt]);
          end;
          FInfo.DataSet.Next;
       end;
      end;
  end;
end;

end.
