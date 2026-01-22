{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10181: AstaIOIProvider.pas 
{
{   Rev 1.0    4/10/2003 6:31:08 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:37:26 PM  Steve    Version: 1.505
}
unit AstaIOIProvider;
{$I AstaIO.inc}

{*********************************************************}
{*   Copyright (c) 1997-2002 Asta Technology Group Inc.  *}
{*               All rights reserved.                    *}
{*               www.astatech.com                        *}
{*********************************************************}

{
This is what we try to support:

  IProviderSupport = interface
    procedure PSEndTransaction(Commit: Boolean);
    procedure PSExecute;
    function PSExecuteStatement(const ASQL: string; AParams: TParams;
      ResultSet: Pointer = nil): Integer;
    procedure PSGetAttributes(List: TList);
    function PSGetDefaultOrder: TIndexDef;
    function PSGetKeyFields: string;
    function PSGetParams: TParams;
    function PSGetQuoteChar: string;
    function PSGetTableName: string;
    function PSGetIndexDefs(IndexTypes: TIndexOptions = [ixPrimary..ixNonMaintained]): TIndexDefs;
    function PSGetUpdateException(E: Exception; Prev: EUpdateError): EUpdateError;
    function PSInTransaction: Boolean;
    function PSIsSQLBased: Boolean;
    function PSIsSQLSupported: Boolean;
    procedure PSReset;
    procedure PSSetParams(AParams: TParams);
    procedure PSSetCommandText(const CommandText: string);
    procedure PSStartTransaction;
    function PSUpdateRecord(UpdateKind: TUpdateKind; Delta: TDataSet): Boolean;
  end;
}

interface

uses
 {$IFDEF Delphi6AndUp}
  Variants,
 {$ENDIF}
  DB, SysUtils, Classes,
  AstaIOStrUtil,
  AstaIOUserList, AstaIOParamList,
  AstaIODataBasePlugin, AstaIODBConst, AstaIOProvider,
  AstaIOCustomDataSet, AstaIOSQLParser;

type

  TProviderEvent = procedure(Sender: TObject; U :TUserRecord) of object;
  TGetParamsEvent = procedure(Sender: TObject; U :TUserRecord) of object;
  TSetParamsEvent = procedure(Sender: TObject; U :TUserRecord; Params: TParams) of object;
  TEndTransactionEvent = procedure(Sender: TObject; U :TUserRecord; Commit: Boolean) of object;
  TApplyUpdatesEvent = procedure(Sender: TObject; U :TUserRecord; var OwnerData: OleVariant) of object;

type
  TAstaIOIProvider = class(TAstaIOCustomProvider)
  private
    FBeforeExecEvent: TProviderEvent;
    FAfterExecEvent: TProviderEvent;

    FAfterApplyUpdates: TApplyUpdatesEvent;
    FBeforeApplyUpdates: TApplyUpdatesEvent;

    FBeforeGetParamsEvent: TGetParamsEvent;
    FAfterGetParamsEvent: TGetParamsEvent;

    FBeforeSetParamsEvent: TSetParamsEvent;
    FAfterSetParamsEvent: TSetParamsEvent;

    FBeforeEndTransactionEvent: TEndTransactionEvent;
    FAfterEndTransactionEvent: TEndTransactionEvent;

    FBeforeStartTransactionEvent: TProviderEvent;
    FAfterStartTransactionEvent: TProviderEvent;

    FBeforeRowRequest: TProviderEvent;
    FAfterRowRequest: TProviderEvent;

    // FOnGetData: TProviderEvent;
    // FOnUpdateData: TProviderEvent;
  protected
    FRecordsSent: Integer;

    //procedure FetchDetails(Source, Delta: TDataSet); override;

    function ExecuteStatement(const ASQL: string; AParams: TParams; ResultSet: Pointer = nil): Integer; override;
    Function InternalExecSQL(SQLString: string; ExecParams: TParams; Action: Byte; ExecQuery: TDataSet = nil):Boolean; override;
  public
    constructor Create(AComponent: TComponent); override;
    destructor Destroy; override;
    procedure EndTransaction(Commit: Boolean); override;
    procedure Execute; override;
    function GetKeyFields: string; override;
    function PSGetIndexDefs(IndexTypes: TIndexOptions = [ixPrimary..ixNonMaintained]): TIndexDefs; override;

    function GetParams: TParams; override;
    function GetQuoteChar: string; override;
    function InTransaction: Boolean; override;
    procedure SetParams(AParams: TParams); override;
    procedure SetCommandText(const CommandText: WideString); override;
    procedure StartTransaction; override;

    procedure DoBeforeApplyUpdates;
    procedure DoAfterApplyUpdates;

    procedure Update; override;

    property Active;
    property CommandText;
    property UpdateTableName: string read GetTableName;
  published
    {Events}
    property BeforeExec: TProviderEvent read FBeforeExecEvent write FBeforeExecEvent;
    property AfterExec: TProviderEvent read FAfterExecEvent write FAfterExecEvent;
    property BeforeGetParams: TGetParamsEvent read FBeforeGetParamsEvent write FBeforeGetParamsEvent;
    property AfterGetParams: TGetParamsEvent read FAfterGetParamsEvent write FAfterGetParamsEvent;
    property BeforeSetParams: TSetParamsEvent read FBeforeSetParamsEvent write FBeforeSetParamsEvent;
    property AfterSetParams: TSetParamsEvent read FAfterSetParamsEvent write FAfterSetParamsEvent;
    property BeforeEndTransaction: TEndTransactionEvent read FBeforeEndTransactionEvent write FBeforeEndTransactionEvent;
    property AfterEndTransaction: TEndTransactionEvent read FAfterEndTransactionEvent write FAfterEndTransactionEvent;
    property BeforeStartTransaction: TProviderEvent read FBeforeStartTransactionEvent write FBeforeStartTransactionEvent;
    property AfterStartTransaction: TProviderEvent read FAfterStartTransactionEvent write FAfterStartTransactionEvent;
    property BeforeRowRequest: TProviderEvent read FBeforeRowRequest write FBeforeRowRequest;
    property AfterRowRequest: TProviderEvent read FAfterRowRequest write FAfterRowRequest;
    property AfterApplyUpdates: TApplyUpdatesEvent read FAfterApplyUpdates write FAfterApplyUpdates;
    property BeforeApplyUpdates: TApplyUpdatesEvent read FBeforeApplyUpdates write FBeforeApplyUpdates;
    property Options;
  end;

implementation
uses AstaIOResources;

{ TAstaIOIProvider }

constructor TAstaIOIProvider.Create(AComponent: TComponent);
begin
  inherited Create(AComponent);
  FDataBasePlugin := nil;
end;

destructor TAstaIOIProvider.Destroy;
begin
  inherited Destroy;
end;

procedure TAstaIOIProvider.DoAfterApplyUpdates;
begin
  if Assigned(FAfterApplyUpdates) then
//    FAfterApplyUpdates();
end;

procedure TAstaIOIProvider.DoBeforeApplyUpdates;
begin
  if Assigned(FBeforeApplyUpdates) then
//    FBeforeApplyUpdates();
end;

procedure TAstaIOIProvider.EndTransaction(Commit: Boolean);
begin
  if Assigned(FBeforeEndTransactionEvent) then FBeforeEndTransactionEvent(Self, FUserRecord, Commit);
  inherited EndTransaction(Commit);
  if Assigned(FAfterEndTransactionEvent) then FAfterEndTransactionEvent(Self, FUserRecord, Commit);
end;

procedure TAstaIOIProvider.Execute;
begin
  inherited Execute;
  FUserRecord.LogActivity('IProvider CommandText:' + FCommandText, [slfIProviderCommandtext]);
  if Assigned(FAfterExecEvent) then FAfterExecEvent(Self, FUserRecord);
end;

function TAstaIOIProvider.ExecuteStatement(const ASQL: string; AParams: TParams; ResultSet: Pointer): Integer;
var i  :Integer;
begin
  inherited ExecuteStatement(ASQL, AParams, ResultSet);

  FUserRecord.LogActivity('IProvider Execute Statement '+ASQL,[slfIProviderCommandtext]);
  for i:=0 to Aparams.Count-1 do
   FUserRecord.LogActivity('IProvider Params '+AParams[i].Name+':'+Aparams[i].AsString,[slfIProviderCommandtext]);
end;

{procedure TAstaIOIProvider.FetchDetails(Source, Delta: TDataSet);
begin
  inherited FetchDetails(Source, Delta);
end;}

function TAstaIOIProvider.GetKeyFields: string;
begin
  Result:=inherited GetKeyFields;
end;

function TAstaIOIProvider.GetParams: TParams;
begin
  if Assigned(FBeforeGetParamsEvent) then FBeforeGetParamsEvent(Self, FUserRecord);
  Result:=FParams;
  Result:=inherited GetParams;
  if Result = nil then Result:=FParams;
  if Assigned(FAfterGetParamsEvent) then FAfterGetParamsEvent(Self, FUserRecord);
end;

function TAstaIOIProvider.GetQuoteChar: string;
begin
  Result:=inherited GetQuoteChar;
end;

Function TAstaIOIProvider.InternalExecSQL(SQLString: string;
  ExecParams: TParams; Action: Byte; ExecQuery: TDataSet = nil):Boolean;
begin
 result:=False;
 try
  CheckDataSet;
  ExecuteStatement(SQLString, ExecParams);
  result:=True;
  except
  end;
end;

function TAstaIOIProvider.InTransaction: Boolean;
begin
  CheckDataSet;
  Result := IProviderSupport(DataSet).PSInTransaction;
end;

function TAstaIOIProvider.PSGetIndexDefs(IndexTypes: TIndexOptions): TIndexDefs;
begin
  Result:=inherited PSGetIndexDefs(IndexTypes);
end;

procedure TAstaIOIProvider.SetCommandText(const CommandText: WideString);
begin
  inherited SetCommandText(CommandText);
end;

procedure TAstaIOIProvider.SetParams(AParams: TParams);
begin
  if Assigned(FBeforeSetParamsEvent) then FBeforeSetParamsEvent(Self, FUserRecord, AParams);
  inherited SetParams(AParams);
  if Assigned(FAfterSetParamsEvent) then FAfterSetParamsEvent(Self, FUserRecord, AParams);
end;

procedure TAstaIOIProvider.StartTransaction;
begin
  if Assigned(FBeforeStartTransactionEvent) then FBeforeStartTransactionEvent(Self, FUserRecord);
  inherited StartTransaction;
  if Assigned(FAfterStartTransactionEvent) then FAfterStartTransactionEvent(Self, FUserRecord);
end;

procedure TAstaIOIProvider.Update;
begin
  ExtractItemsFromString(GetKeyFields, ';', FPrimeFields);
  FUpdateTableName:=GetTableName;

  {if FResolveToDataSet then
  begin
    CurrentDataSet.First;
    while not CurrentDataSet.Eof do
    begin
      UpdateRecord(CurrentDataSet, OldValuesDataSet, False, False);
      CurrentDataSet.Next;
      OldValuesDataSet.Next;
    end;
  end
  else
    inherited Update;}

  inherited Update;
end;
end.

