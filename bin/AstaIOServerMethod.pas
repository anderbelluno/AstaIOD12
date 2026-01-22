{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10297: AstaIOServerMethod.pas 
{
{   Rev 1.0    4/10/2003 6:32:04 AM  Steve
}
{
{   Rev 1.0    11/7/2002 1:13:48 PM  Steve
}
{
{   Rev 1.0    10/30/2002 8:38:10 PM  Steve    Version: 1.505
}
unit AstaIOServerMethod;

{*********************************************************}
{*   Copyright (c) 1997-2002 Asta Technology Group Inc.  *}
{*               All rights reserved.                    *}
{*               www.astatech.com                        *}
{*********************************************************}
{$I AstaIO.inc}

interface

uses
  DB,
  SysUtils,
  Classes,
  AstaIOUserList,
  AstaIOParamList,
  AstaIOProvider;

type
  {$ifdef ServerMethodDatabase}
  TResultActionEvent = procedure(Sender: TObject; U: TUserRecord; DataSet :TDataSet; DatabaseString :String; Params :TParams) of object;
  TExecActionEvent = procedure(Sender: TObject; U: TUserRecord; DatabaseString :String; Params :TParams) of object;
  {$else}
  TResultActionEvent = procedure(Sender: TObject; U: TUserRecord; DataSet :TDataSet; Params :TParams) of object;
  TExecActionEvent = procedure(Sender: TObject; U: TUserRecord; Params :TParams) of object;
  {$endif}
type
  TAstaIOBaseServerMethod = class(TComponent)
  private
    FAbout: String;
    FDataSet: TDataSet;
    FGetParamsFromProvider:Boolean;
    //FUseNoDataSet: Boolean;
    FProvider: TAstaIOProvider;
    procedure GetProviderParams;
    procedure SetParams(AParams: TParams);
  protected
    FParams: TParams;
    FUserRecord: TUserRecord;
    FDatabase :String;
    procedure DoEventAction; virtual; abstract;
    procedure SetClientInputParams(ClientParams:TAstaParamList);
    Procedure SetClientOutputParams(ClientParams:TAstaParamList);
    property Provider: TAstaIOProvider read FProvider write FProvider;
    property GetParamsFromProvider: Boolean read FGetParamsFromProvider write FGetParamsFromProvider;
    property DataSet: TDataSet read FDataSet write FDataSet;
    property Database :String read FDatabase write FDatabase;
  public
    Procedure FreshenParams;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    Procedure DoAction(ClientParams:TAstaParamList);
    procedure EndAction;
    property UserRecord: TUserRecord read FUserRecord write FUserRecord;
  published
    property About: String read FAbout write FAbout;
    property Params: TParams read FParams write SetParams;
  end;

  TAstaIOServerMethodResultSet = class(TAstaIOBaseServerMethod)
  private
    FOnAction: TResultActionEvent;
  protected
    procedure DoEventAction;override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    Function GetDataSet:TDataSet;
    property Database;
  published
    property OnAction: TResultActionEvent read FOnAction write FOnAction;
    property Provider;
    property GetParamsFromProvider;
    property DataSet;
  end;

  TAstaIOServerMethodExec = class(TAstaIOBaseServerMethod)
  private
    FOnAction: TExecActionEvent;
    FSoapService:Boolean;
  protected
    procedure DoEventAction;override;
  public
    Constructor Create(AOwner:TComponent);override;
    property Database;
  published
    property OnAction: TExecActionEvent read FOnAction write FOnAction;
    property SoapService:Boolean read FSoapService write FSoapService default false;
  end;

implementation

{ TAstaIOServerMethod }

constructor TAstaIOBaseServerMethod.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FParams := TParams.Create(Self);
end;

destructor TAstaIOBaseServerMethod.Destroy;
begin
  FParams.Free;

  inherited Destroy;
end;

procedure TAstaIOBaseServerMethod.GetProviderParams;
begin
  if not Assigned(FProvider) then exit;
  FParams.Assign(FProvider.Params);
end;

procedure TAstaIOBaseServerMethod.EndAction;
begin
  if Assigned(FDataSet) and FDataSet.Active then FDataSet.Close;

  if Assigned(FProvider) then
  begin
    if FProvider.Active then FProvider.Close;
  end;
end;


Procedure TAstaIOBaseServerMethod.DoAction(ClientParams:TAstaParamList);
//creates a ParamList of Output Params Only;
begin
  if Assigned(FDataSet) and FDataSet.Active then FDataSet.Close
   else if Assigned(FProvider) and FProvider.Active then FProvider.Close;

  FreshenParams; //if the params came from the provider get them

  SetClientInputParams(ClientParams);
  DoEventAction;
  //we only want to execute the query once and if there is a provider let him do it.
  if Assigned(FDataSet)  then
  begin
    // if FDataSet.Active then FDataSet.Close;//moved 03/14/2002
    //could have been oopened in the DoEventAction
    if not FDataSet.Active then FDataSet.Open;
  end
  else if Assigned(FProvider) then
  begin
    //if FProvider.Active then FProvider.Close;
    //Provider could have been opened in the DoEventaction
    if not FProvider.Active then FProvider.Active:=True
    //todo: need to set ClientParams from the provider
  end;
  SetClientOutputParams(ClientParams);
end;

procedure TAstaIOBaseServerMethod.SetParams(AParams: TParams);
begin
  FParams.Assign(AParams);
end;

Procedure TAstaIOBaseServerMethod.FreshenParams;
begin
  if FGetParamsFromProvider and Assigned(FProvider) then
    GetProviderParams;

end;

procedure TAstaIOBaseServerMethod.SetClientInputParams(ClientParams:TAstaParamList);
var
i:Integer;
begin
 //on pda's not all params maybe sent to the server so we want to null the FParams here
 for i:=0 to FParams.Count-1 do
  FParams[i].Clear;
 ClientParams.AssignParamValues(FParams,[ptUnKnown,ptInput,PtInputOutput]);
end;

Procedure TAstaIOBaseServerMethod.SetClientOutputParams(ClientParams:TAstaParamList);
var
i:integer;
begin
 ClientParams.UpdateParamValues(FParams,[ptunknown,ptresult,ptOutput,ptInPutOutput]);
 i:=0;
 //only send back output params
 while i<ClientParams.Count do
  if not ClientParams[i].IsOutput then ClientParams[i].Free
   else inc(i);
end;

{ TAstaIOServerMethodResultSet }


procedure TAstaIOServerMethodResultSet.DoEventAction;
var
 i:integer;
begin
  if (FProvider<>Nil) and FGetParamsFromProvider then
  begin
  //set the provider params automatically
    for i:=0 to FParams.Count-1 do
     if (FParams[i].ParamType in [ptInput,ptInputOutput])
     and (FProvider.Params.FindParam(FParams[i].Name)<>nil) then
      FProvider.Params.ParamByname(FParams[i].Name).Assign(FParams[i]);
     FProvider.SetDataSetParams;
  end;
  {$ifdef ServerMethodDatabase}
  if Assigned(FOnAction) then
   FOnAction(Self, FUserRecord, FDataSet, FDatabase, FParams);
  {$else}
  if Assigned(FOnAction) then
   FOnAction(Self, FUserRecord, FDataSet, FParams);
  {$endif}
end;

Function TAstaIOServerMethodResultSet.GetDataSet:TDataSet;
begin
 result:= FDataSet;
 if Assigned(Result) then exit;
 if Assigned(FProvider) then result:=FProvider.DataSet;
end;

procedure TAstaIOServerMethodResultSet.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = FDataSet) and (Operation = opRemove) then
  begin
    FDataSet := nil;
  end;
end;

{ TAstaIOServerMethodExec }

procedure TAstaIOServerMethodExec.DoEventAction;
begin
  if Assigned(FOnAction) then
  {$ifdef ServerMethodDatabase}
    FOnAction(Self, FUserRecord, FDatabase, FParams);
  {$else}
    FOnAction(Self, FUserRecord, FParams);
  {$endif}  
end;
Constructor TAstaIOServerMethodExec.Create(AOwner:TComponent);
begin
   inherited Create(AOwner);
   FSoapService:=False;
end;

end.




