{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10073: AstaIOClientIProvider.pas 
{
{   Rev 1.0    4/10/2003 6:30:12 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:36:44 PM  Steve    Version: 1.505
}
unit AstaIOClientIProvider;
{*********************************************************}
{*   Copyright (c) 1997-2002 Asta Technology Group Inc.  *}
{*               All rights reserved.                    *}
{*               www.astatech.com                        *}
{*********************************************************}

{$I AstaIO.inc}

interface

uses
  SysUtils,
  DB,
  Classes,
  AstaIODBConst,
  AstaIOParamList, AstaIOResources,
  AstaIOClientRemoteDataSet;

type

  TAfterExecuteEvent = procedure(Sender: TObject; var OwnerData: OleVariant) of object;
  TBeforeExecuteEvent = procedure(Sender: TObject; var OwnerData: OleVariant) of object;
  TAfterGetParamsEvent = procedure(Sender: TObject; var OwnerData: OleVariant) of object;
  TBeforeGetParamsEvent = procedure(Sender: TObject; var OwnerData: OleVariant) of object;

  TAstaIOCustomClientIProvider = class(TAstaParamsDataSet)
  private
    FAfterExecute: TAfterExecuteEvent;
    FBeforeExecute: TBeforeExecuteEvent;
    FAfterGetParams: TAfterGetParamsEvent;
    FBeforeGetParams: TBeforeGetParamsEvent;

    FCommandText: TStrings;
    FServerProviderOptions: TAstaIProviderOptionSet;
    FParamCheck: Boolean;
    procedure QueryChanged(Sender: TObject);
    procedure SetCommandText(Value: TStrings);
    procedure CreateParamsFromProvider(ParamsString: AnsiString);
    procedure SetIProviderName(Value: string);
  protected
    procedure InternalOpen; override;
    procedure InternalClose; override;
    function MessageFetchDataSetFromServer(PackOnServer: Boolean): AnsiString; override;
    function MessageFetchIProvExecFromServer: AnsiString;
    function MessageFetchIProvParamsFromServer: AnsiString;

    procedure SetIProviderOptionsFromServer(IntOptions: Integer); override;
    function CheckMasterFilter: Boolean; override;

    procedure DoBeforeInsert; override;
    procedure DoBeforeDelete; override;
    procedure DoBeforeEdit; override;
    procedure DoAfterOpen; override;

    property AfterExecute: TAfterExecuteEvent read FAfterExecute write FAfterExecute;
    property BeforeExecute: TBeforeExecuteEvent read FBeforeExecute write FBeforeExecute;
    property AfterGetParams: TAfterGetParamsEvent read FAfterGetParams write FAfterGetParams;
    property BeforeGetParams: TBeforeGetParamsEvent read FBeforeGetParams write FBeforeGetParams;

    property CommandText: TStrings read FCommandText write SetCommandText;
    property IProviderName: string read FProviderName write SetIProviderName;
    property ParamCheck: Boolean read FParamCheck write FParamCheck;

    property Params;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure FetchBlobs;
    procedure FetchParams;
    procedure Reconcile;
    procedure Execute;

    function ApplyUpdates(TransactionMethod: TUpdateSQLMethod = usmServerTransaction): string; override;
    property ServerProviderOptions: TAstaIProviderOptionSet read FServerProviderOptions;
  published
  end;

  TAstaIOClientIProvider = class(TAstaIOCustomClientIProvider)
  private
  protected
  public
  published
    property AfterExecute;
    property BeforeExecute;
    property AfterGetParams;
    property BeforeGetParams;

    property MasterFields;
    property MasterSource;
    property DetailFields;
    property IndexDefs;

    property CommandText;
    property IProviderName;
    property ParamCheck;
    property Params;
  end;

implementation
uses AstaIOConst, AstaIOMessagePacker, AstaIOCustomDataSet, AstaIODataSetUtils;

{ TAstaIOCustomClientIProvider }

constructor TAstaIOCustomClientIProvider.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCommandText:=TStringList.Create;
  TStringList(FCommandText).OnChange := QueryChanged;
  FParamCheck:=True;
end;

destructor TAstaIOCustomClientIProvider.Destroy;
begin
  FCommandText.Free;

  inherited Destroy;
end;

function TAstaIOCustomClientIProvider.MessageFetchIProvExecFromServer: AnsiString;
var
  AParamList: TAstaParamList;
begin
  AParamList := TParamsToAstaParams(Params);
  try
    Result := FClientWire.MessageToString(Self, ATDBIProviderExec, [FDataBase,
      FProviderName,
        FCommandText.Text,
        AParamList.AsTokenizedString(False),
        AdjustedOptions(True)]);
  finally
    AParamList.Free;
  end;
end;

procedure TAstaIOCustomClientIProvider.Execute;
var
  r: TAstaMessageReader;
begin
  // if Assigned(FBeforeExecute) then FBeforeExecute( );

  r := FClientWire.SendStringGetReader(MessageFetchIProvExecFromServer, False);
  try
    if r.Token = ATDBException then
    begin
      FInternalErrorString := r.ReadString(0);
      raise EAstaDataSetException.Create(r.ReadString(0));
    end;
    GetParamsBack(r.ReadString(0));
  finally
    r.free;
  end;
  // if Assigned(FAfterExecute) then FAfterExecute( );
end;

procedure TAstaIOCustomClientIProvider.FetchBlobs;
begin

end;

function TAstaIOCustomClientIProvider.MessageFetchIProvParamsFromServer: AnsiString;
begin
  Result := FClientWire.MessageToString(Self, ATDBIProviderFetchParams, [FDataBase,
    FProviderName]);
end;

procedure TAstaIOCustomClientIProvider.FetchParams;
var
  r: TAstaMessageReader;
begin
  // if Assigned(FBeforeGetParams) then FBeforeGetParams( );

  r := FClientWire.SendStringGetReader(MessageFetchIProvParamsFromServer, False);
  try
    if r.Token = ATDBException then
    begin
      FInternalErrorString := r.ReadString(0);
      raise EAstaDataSetException.Create(r.ReadString(0));
    end;
    CreateParamsFromProvider(r.ReadString(0));
  finally
    r.free;
  end;
  // if Assigned(FAfterGetParams) then FAfterGetParams( );
end;

procedure TAstaIOCustomClientIProvider.SetIProviderName(Value: string);
begin
  if Value = FProviderName then exit;
  FProviderName := Value;
  if not (csReading in ComponentState) and (Assigned(FClientWire)) and FClientWire.Active then
  begin
    MetaDataFetchParams(mdIProviderParams, FProviderName, 'IProviderName');
  end;
end;

procedure TAstaIOCustomClientIProvider.InternalClose;
begin
  inherited InternalClose;
end;

procedure TAstaIOCustomClientIProvider.InternalOpen;
begin
  if Trim(FProviderName) = '' then
    DatabaseError(SNoProviderName, Self);
  EmptyCache;
  inherited InternalOpen;
end;

function TAstaIOCustomClientIProvider.MessageFetchDataSetFromServer(PackOnServer: Boolean): AnsiString;
var
  AParamList: TAstaParamList;
begin
  AParamList := TParamsToAstaParams(Params);
  try
    Result := FClientWire.MessageToString(Self, ATDBIProvider, [FDataBase,
      FProviderName,
        FCommandText.Text,
        AParamList.AsTokenizedString(False),
        FRowsToReturn,
        AdjustedOptions(PackOnServer)]);
  finally
    AParamList.Free;
  end;
end;

procedure TAstaIOCustomClientIProvider.Reconcile;
begin

end;

procedure TAstaIOCustomClientIProvider.SetCommandText(Value: TStrings);
begin
  FCommandText.Assign(Value);
end;

procedure TAstaIOCustomClientIProvider.CreateParamsFromProvider(ParamsString: AnsiString);
var
  TheAstaParamList: TAstaParamList;
begin
  Params.Clear;
  TheAstaParamList := TAstaParamList.CreateFromTokenizedString(ParamsString);
  try
    Params.Assign(AstaParamsToTParams(TheAstaParamList));
  finally
    TheAstaParamList.Free;
  end;
end;

function TAstaIOCustomClientIProvider.ApplyUpdates(TransactionMethod: TUpdateSQLMethod = usmServerTransaction): string;
var
  d: TDataSet;
  r: TAstaMessageReader;
begin
  CheckActive;
  if state in [dsinsert,dsedit] then Post;
  if (ChangeCount <> 0) and UpdatesPending then
  begin
    r := nil;
    d := DataSetforServerSideTransaport(ATDBIProviderModify);
    try
      AddToTransPortDataSet(ATDBIProviderModify, D,0);
    r := FClientWire.SendStringGetReader(FClientWire.MessageToString(Self,
      ATDBIProviderModify, [DataSetToString(TAstaIODataSet(d))]));
    if r.Token = ATDBException then //if there is a failure we want to bring back the bookmark of the row that failed
      raise EAstaDataSetException.Create(r.ReadString(0))
    else
    begin
      EmptyCache;
    end;
  finally
    d.free;
    r.free;
  end;
 end;
end;

procedure TAstaIOCustomClientIProvider.SetIProviderOptionsFromServer(IntOptions: Integer);
begin
  FServerProviderOptions:=IntegerToIProviderOptions(IntOptions);

  if poReadOnly in FServerProviderOptions then
    ReadOnly := True
  else
    ReadOnly := False;
end;

procedure TAstaIOCustomClientIProvider.DoBeforeInsert;
begin
  if poDisableInserts in FServerProviderOptions then
    DatabaseError(SPRovDataSetNoInsert, Self);

  inherited DoBeforeInsert;
end;

procedure TAstaIOCustomClientIProvider.DoBeforeDelete;
begin
  if poDisableDeletes in FServerProviderOptions then
    DatabaseError(SPRovDataSetNoDelete, Self);

  inherited DoBeforeDelete;
end;

procedure TAstaIOCustomClientIProvider.DoBeforeEdit;
begin
  if poDisableEdits in FServerProviderOptions then
    DatabaseError(SPRovDataSetNoEdit, Self);

  inherited DoBeforeEdit;
end;

procedure TAstaIOCustomClientIProvider.QueryChanged(Sender: TObject);
var
  List: TParams;
  i: Integer;
  //FText :String;
begin
  if not (csReading in ComponentState) then
  begin
    if FParamCheck or (csDesigning in ComponentState) then
    begin
      List := TParams.Create(Self);
      try
        List.ParseSQL(FCommandText.Text, True);
        List.AssignValues(Params);
        Params.Clear;
        Params.Assign(List);
      finally
        List.Free;
      end;
    end;
    DataEvent(dePropertyChange, 0);
  end
  else
    Params.ParseSQL(FCommandText.Text, False);

  if FParamCheck then
    for i := 0 to Params.Count - 1 do
      Params[i].ParamType := DB.ptInput;
end;

function TAstaIOCustomClientIProvider.CheckMasterFilter: Boolean;
begin
  Result:=False;
end;

procedure TAstaIOCustomClientIProvider.DoAfterOpen;
begin
  inherited DoAfterOpen;
  // if FParams.Count > 0 then Reload; // sm 7/20/2002. Do not know what this is here.
  Refresh;
end;

end.


