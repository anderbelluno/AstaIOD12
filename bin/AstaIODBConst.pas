{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10125: AstaIODBConst.pas 
{
{   Rev 1.0    4/10/2003 6:30:42 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:37:04 PM  Steve    Version: 1.505
}
unit AstaIODBConst;
{*********************************************************}
{*   Copyright (c) 1997-2002 Asta Technology Group Inc.  *}
{*               All rights reserved.                    *}
{*               www.astatech.com                        *}
{*********************************************************}

interface
uses Classes, DB;
const
  AstaRTRFetchAll = -1;
  ASTARTRFieldDefs = 0;
  CrazyBrakes = 2500000;
  AllRecords=-1;
  
type
  TRemoteDataSetOpenOrigin = (trServer,trFile,trNoFetch);
  TThreadDbAction = (ttSelect, ttExec, ttStoredProc, ttMetaData, ttBlobFetch,
    ttTransaction, ttTransactionStart, ttCustom, ttDataModule, ttServerMethod); // no longer needed 05-14-99 ttPacketSelect);
  TUpdateSQLMethod = (usmNoTransactions, usmUseSQL, usmServerTransaction, usmCommitAnySuccess);
  TAstaUpdateEvent = procedure(Sender: TObject; Client: TObject; ExecQuery: TComponent; OriginalValueDataSet, CurrentValueDataSet, ServerValueDataSet: TDataset;
    var Handled: Boolean) of object;
  TAstaInsertEvent = procedure(Sender: TObject; Client: TObject; ExecQuery: TComponent; CurrentValueDataSet: TDataset; var Handled: Boolean) of object;
  TAstaDeleteEvent = procedure(Sender: TObject; Client: TObject; ExecQuery: TComponent; OriginalValueDataSet, ServerValueDataSet: TDataset;
    var Handled: Boolean) of object;
  TAstaUpdateMethod = (umAfterPost, umCached, umManual);
  TServerParamType = (spBusinessObject, spProvider);
  TDeltaType = (dtEdit, dtDelete, dtAppend, dtAppendAndDelete);
  TDeltaOptions = set of TDeltaType;
  TBroadCastOptionType=(bcUpdate,bcInsert,bcDelete,bcStoredProc,bcCreateTable);
  TBroadCastOptionTypes= set of TBroadCastOptionType;
  TSQLValueFormat = (tsInsert, tsCompare, tsAssign);
  TAstaUpdateMode = (upmWhereAll, upmWhereChanged, upmWhereKeyOnly);
  TRefetchStatus = (rfNone, rfAutoIncPrimeKey, ftNeedsPrimeFields);
  TDbAction = (tdbSelect, tdbExecSQL, tdbExecProc, tdbStoredProc, tdbMetaData,
    tdbTransaction, tdbDataModule, tdbServerMethod, tdbCustom);
  TAstaMetaData = (mdNormalQuery, mdTables, mdIndexes, mdFields, mdViews,
    mdStoredProcs, mdForeignKeys, mdSystemTables, mdDBMSName, mdPrimeKeys,
    mdStoredProcColumns, mdServerMethods, mdServerMethodParams,
    mdProviders, mdProviderParams, mdIProviders, mdIProviderParams,
    mdOtherMetaData, mdUserList, mdAliasedServerMethods,
    mdSessionInfo, mdTriggers, mdServerMethodsAndProviders, mdServerMethodsExec, mdServerMethodExecParams,
    mdDirectory, mdSystemInfo, mdAll,mdSoapServiceParams,mdSoapServices,mdStatelessUserList);
  TAstaDataSetOptions = (soFetchMemos, soFetchBlobs, soPackets, soFieldProperties,
    soCyclopsSQL, soCompress, soIgnoreAutoIncrement, soFetchBytes, soPackOnServer, soFetchPrimeFields);
  TAstaDataSetOptionSet = set of TAstaDataSetOptions;
  TAstaSelectNotifyEvent = procedure(Sender: TObject; Client: TObject; AQuery: TDataSet; Started: Boolean) of object;
  TAstaThreadModel = (tmSingleSession, tmPooledSessions, tmPersistentSessions, tmMixed);
  TAstaIProviderOptions = (poReadOnly, poDisableInserts, poDisableEdits, poDisableDeletes, poAllowCommandText, poFetchBlobsOnDemand, poFetchDetailsOnDemand);
  TAstaIProviderOptionSet = set of TAstaIProviderOptions;

  TAstaIOGetRecordOption = (grMetaData, grReset, grXML);
  TAstaIOGetRecordOptions = set of TAstaIOGetRecordOption;

  TAstaRdbmsInfoKind = (rdbInterbase, rdbASE, rdbASA, rdbMSSQL, rdbOracle,rdbMySQL,rdbPostgreSQL,rdbDB2,rdbAccess);

  TDBInfoSupply = (isTables, isSysTables, isFields, isFkeys, isPKeys, isTriggers, isIndexes, isViews, isSProcs, isSProcCols);
  TDBInfoSupplySet = set of TDBInfoSupply;

function DataSetOptionsToInteger(Options: TAstaDataSetOptionSet): Integer;
function IntegerToDataSetOptions(I: Integer): TAstaDataSetOptionSet;
function ByteStringToInteger(S: string): Integer;
function IntegerToByteString(I: Integer): string;

function IProviderOptionsToInteger(Options: TAstaIProviderOptionSet): Integer;
function IntegerToIProviderOptions(I: Integer): TAstaIProviderOptionSet;

implementation

function IProviderOptionsToInteger(Options: TAstaIProviderOptionSet): Integer;
begin
  result := 0;
  move(options, result, sizeof(options));
end;

function IntegerToIProviderOptions(I: Integer): TAstaIProviderOptionSet;
begin
  result := [];
  move(i, result, sizeof(result));
end;

function DataSetOptionsToInteger(Options: TAstaDataSetOptionSet): Integer;
begin
  result := 0;
  move(options, result, sizeof(options));
end;

function IntegerToDataSetOptions(I: Integer): TAstaDataSetOptionSet;
begin
  result := [];
  move(i, result, sizeof(result));
end;

function IntegerToByteString(I: Integer): string;
begin
  SetLength(result, sizeof(Integer));
  move(i, result[1], sizeof(Integer));
end;

function ByteStringToInteger(S: string): Integer;
begin
  result := 0;
  move(s[1], result, sizeof(result));
end;

end.




