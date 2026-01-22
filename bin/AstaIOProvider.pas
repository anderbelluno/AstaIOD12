{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10279: AstaIOProvider.pas 
{
{   Rev 1.0    4/10/2003 6:31:56 AM  Steve
}
unit AstaIOProvider;

{*********************************************************}
{*   Copyright (c) 1997-2002 Asta Technology Group Inc.  *}
{*               All rights reserved.                    *}
{*               www.astatech.com                        *}
{*********************************************************}

interface

uses
  DB,
  SysUtils,
  Classes,
{$IFDEF LINUX}
  Variants,
{$ENDIF}
  AstaIOParamList,
  AstaIOUserList,
  AstaIODataBasePlugin,
  AstaIOSQLGenerator,
  AstaIOCustomDataSet,
  AstaIODBConst,
  AstaIODataSetProvider;

type
  TProviderEvent = procedure(Sender: TObject) of object;
  TAfterExecEvent = procedure(Sender: TObject) of object;
//  TSetParamsEvent = procedure(Sender: TObject; Params: TParams) of object;

  TBeforeOpenEvent = procedure(Sender: TObject; U: TUserRecord; DatabaseString :String; Params: TParams) of object; // sm - 8/28/2002

  TBeforeDeleteEvent = procedure(Sender: TObject; U: TUserRecord; ExecQuery: TComponent;
    OriginalValueDataSet, ServerValueDataSet: TDataSet; var Handled: Boolean) of object;
  TBeforeUpdateEvent = procedure(Sender: TObject; U: TUserRecord; ExecQuery: TComponent;
    OriginalValueDataSet, CurrentValueDataSet, ServerValueDataSet: TDataSet;
    var Handled: Boolean) of object;
  //TBeforeInsertEvent = procedure(Sender: TObject; U: TUserRecord; ExecQuery: TComponent; OriginalValueDataSet, CurrentValueDataSet, ServerValueDataSet: TDataSet;
  //  var Handled: Boolean) of object;
  TBeforeInsertEvent = procedure(Sender: TObject; U: TUserRecord; ExecQuery: TComponent;  CurrentValueDataSet : TDataSet; var Handled: Boolean) of object;
  TAfterDeleteEvent = procedure(Sender: TObject; U: TUserRecord; ExecQuery: TComponent;
    OriginalValueDataSet, ServerValueDataSet: TDataSet) of object;
  TAfterUpdateEvent = procedure(Sender: TObject; U: TUserRecord; ExecQuery: TComponent;
    OriginalValueDataSet, CurrentValueDataSet, ServerValueDataSet: TDataSet) of object;
  //TAfterInsertEvent = procedure(Sender: TObject; U: TUserRecord; ExecQuery: TComponent;
  //  OriginalValueDataSet, CurrentValueDataSet, ServerValueDataSet: TDataSet) of object;
  TAfterInsertEvent = procedure(Sender: TObject; U: TUserRecord; ExecQuery: TComponent; CurrentValueDataSet : TDataSet) of object;
  TBeforeTransactionEvent = procedure(Sender: TObject; U: TUserRecord; OriginalValueDataSet, CurrentValueDataSet: TDataSet;
    var Handled: Boolean) of object;
  TAfterTransactionEvent = procedure(Sender: TObject; U: TUserRecord; CurrentValueDataSet: TDataSet; TransactionFailed: Boolean) of object;
  TBeforeBroadcastEvent= procedure (Sender :TObject; U:TUserRecord;BroadCastList:TAstaParamList;CurrentValueDatSet:TDataSet) of object;
  TClientBroadcastEvent= procedure (Sender :TObject; U:TUserRecord;CurrentValueDatSet:TDataSet;Var BroadcastOK:Boolean;ExtraParams:TAstaParamList) of object;
type
  TAstaIOCustomProvider = class(TAstaIOCustomDataSetProvider) //TComponent)
  private
//    FAbout: String;
  protected
    FBroadcastToSelf:Boolean;
    FClientBroadcastEvent:TClientBroadcastEvent;
    FBeforeBroadcastEvent:TBeforeBroadcastEvent;
    FBroadcastList:TAstaParamList;
    FBeforeOpenEvent: TBeforeOpenEvent;
    FBeforeDeleteEvent: TBeforeDeleteEvent;
    FBeforeInsertEvent: TBeforeInsertEvent;
    FBeforeUpdateEvent: TBeforeUpdateEvent;
    FAfterDeleteEvent: TAfterDeleteEvent;
    FAfterInsertEvent: TAfterInsertEvent;
    FAfterUpdateEvent: TAfterUpdateEvent;
    FBeforeTransactionEvent: TBeforeTransactionEvent;
    FAfterTransactionEvent: TAfterTransactionEvent;

    FCurrentDataSet: TAstaIODataSet;
    FOldValuesDataSet,FOldValueBroadcastDataSet: TAstaIODataSet;
    FRefetchPackage: TAstaParamList;

    FDataBasePlugin: TAstaIODataBasePlugin;
    FExtraClientParams:TParams;
    FActive: Boolean;
    FDatabase: string;
    FSQLGenerator: TAstaIOSQLGenerator;
    FUpdateMode: TUpdateMode;
    FUpdateTableName: string;
    FPrimeFields: TStringList;
    FUserRecord: TUserRecord;
    FReturnPackageString: STring;

    Function CurrentDataSetForBroadcastList:String;
    procedure CheckValueDataSets;
    procedure CheckDataSet;
    procedure CheckGenerator;
    procedure SetDataBasePlugin(Value: TAstaIODataBasePlugin);
    function GetDataBasePlugin: TAstaIODataBasePlugin;
    procedure SetUpdateMode(Value: TUpdateMode);
    function GenerateSQL: TAstaParamList;
    Function InternalExecSQL(SQLString: string; ExecParams: TParams; Action: Byte; ExecQuery: TDataSet = nil):Boolean; virtual;
    Function  ClientsRegisteredForBroadcast:Boolean;

    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    procedure DoBeforeOpen(Sender: TObject; U: TUserRecord; DatabaseString :String; Params: TParams); virtual; // sm - 8/28/2002
    procedure DoBeforeDelete(Sender: TObject; U: TUserRecord; ExecQuery: TComponent;
      OriginalValueDataSet, ServerValueDataSet: TDataSet; var Handled: Boolean); virtual;
    procedure DoBeforeInsert(Sender: TObject; U: TUserRecord; ExecQuery: TComponent;  CurrentValueDataSet : TDataSet; var Handled: Boolean); virtual;
    procedure DoBeforeUpdate(Sender: TObject; U: TUserRecord; ExecQuery: TComponent;
      OriginalValueDataSet, CurrentValueDataSet, ServerValueDataSet: TDataSet; var Handled: Boolean); virtual;
    procedure DoAfterDelete(Sender: TObject; U: TUserRecord; ExecQuery: TComponent;
      OriginalValueDataSet, ServerValueDataSet: TDataSet); virtual;
    procedure DoAfterInsert(Sender: TObject; U: TUserRecord; ExecQuery: TComponent;
      OriginalValueDataSet, CurrentValueDataSet, ServerValueDataSet: TDataSet); virtual;
    procedure DoAfterUpdate(Sender: TObject; U: TUserRecord; ExecQuery: TComponent;
      OriginalValueDataSet, CurrentValueDataSet, ServerValueDataSet: TDataSet); virtual;
    procedure DoBeforeBroadCast(Sender: TObject; U: TUserRecord; BroadCastList:TAstaParamList;
      CurrentValueDataSet: TDataSet);virtual;
    procedure ProviderBroadcast;
    Function BroadCastRequested:Boolean;
    property ReturnPackageString: String read FReturnPackageString;
    property RefetchPackage: TAstaParamList read FRefetchPackage write FRefetchPackage;
    procedure BroadcastOldValuesCheck;
  public
    constructor Create(AComponent: TComponent); override;
    destructor Destroy; override;
    procedure Update; virtual;
    procedure Open(ClientDataSet :TDataSet = nil); override;

    property OldValuesDataSet: TAstaIODataSet read FOldValuesDataSet write FOldValuesDataSet;
    property CurrentDataSet: TAstaIODataSet read FCurrentDataSet write FCurrentDataSet;
    property DataBasePlugin: TAstaIODataBasePlugin read GetDataBasePlugin write SetDataBasePlugin;
    property UserRecord: TUserRecord read FUserRecord write FUserRecord;
    property UseNULLSyntax;
  published
//    property About: String read FAbout write FAbout;
    property BroadcastToOriginator:Boolean read FBroadcastToSelf write FBroadcastToSelf;
    property DataSet;
    property UpdateMode: TUpdateMode read FUpdateMode write SetUpdateMode; // default upWhereKeyOnly;
    property ResolveToDataSet;
    property Options;
    property Exported;
    property NoSQLFields;
  end;

  TAstaIOProvider = class(TAstaIOCustomProvider)
  private
    procedure SetParams(Value: TParams);override;
    function GetParams: TParams;
    procedure SetActive(Value: Boolean);
    function GetActive: Boolean;
    function GetPrimeFields: TStrings;
    procedure SetPrimeFields(Value: TStrings);
  protected
    Function InternalExecSQL(SQLString: string; ExecParams: TParams; Action: Byte; ExecQuery: TDataSet = nil):Boolean; override;
  public
    procedure SetDataSetParams;
    procedure DoBeforeTransaction(var Handled: Boolean);
    procedure DoAfterTransaction(Success: Boolean);
    procedure DoBeforeBroadCastPrepare;

    procedure GetParamsFromDataSet;
    property Active: Boolean read GetActive write SetActive;
    property ClientParams:TParams read FExtraClientParams write FExtraClientParams;
    property Params: TParams read GetParams write SetParams;
    property SQLGenerator: TAstaIOSQLGenerator read FSQLGenerator;
    property Database: string read FDatabase write FDatabase;
    property ReturnPackageString;
    property RefetchPackage;
  published
    property BeforeOpen: TBeforeOpenEvent read FBeforeOpenEvent write FBeforeOpenEvent;
    property BeforeDelete: TBeforeDeleteEvent read FBeforeDeleteEvent write FBeforeDeleteEvent;
    property BeforeInsert: TBeforeInsertEvent read FBeforeInsertEvent write FBeforeInsertEvent;
    property BeforeUpdate: TBeforeUpdateEvent read FBeforeUpdateEvent write FBeforeUpdateEvent;
    property AfterDelete: TAfterDeleteEvent read FAfterDeleteEvent write FAfterDeleteEvent;
    property AfterInsert: TAfterInsertEvent read FAfterInsertEvent write FAfterInsertEvent;
    property AfterUpdate: TAfterUpdateEvent read FAfterUpdateEvent write FAfterUpdateEvent;
    property BeforeTransaction: TBeforeTransactionEvent read FBeforeTransactionEvent write FBeforeTransactionEvent;
    property AfterTransaction: TAfterTransactionEvent read FAfterTransactionEvent write FAfterTransactionEvent;
    property BeforeBroadcast:TBeforeBroadcastEvent read FBeforeBroadCastEvent write FBeforeBroadCastEvent;
    property PrimeFields: TStrings read GetPrimeFields write SetPrimeFields;
    property UpdateTableName: string read FUpdateTableName write FUpdateTableName;
    property OnClientBroadcast:TClientBroadcastEvent read FClientBroadcastEvent write FClientBroadcastEvent;
  end;

implementation
uses AstaIOResources,
     AstaIOServerWire,
     AstaIOConst,
     AstaIOBroadCast;

{ TAstaIOCustomProvider }

procedure TAstaIOCustomProvider.CheckDataSet;
begin
  if not Assigned(DataSet) then DatabaseError(SMissingDataSet);
end;

procedure TAstaIOCustomProvider.CheckGenerator;
begin
  if not Assigned(FSQLGenerator) then
    FSQLGenerator := TAstaIOSQLGenerator.Create(Self);
  if Assigned(FSQLGenerator) and Assigned(FDatabasePlugin) then
  FSQLGenerator.SQLOptions:=FDatabasePlugin.SQLGenerateOptions;
end;

function TAstaIOCustomProvider.GetDataBasePlugin: TAstaIODataBasePlugin;
begin
  result := FDataBasePlugin;
end;

procedure TAstaIOCustomProvider.SetDataBasePlugin(Value: TAstaIODataBasePlugin);
begin
  FDataBasePlugin := Value;
end;

procedure TAstaIOCustomProvider.SetUpdateMode(Value: TUpdateMode);
begin
  FUpdateMode := Value;
end;

function TAstaIOCustomProvider.GenerateSQL: TAstaParamList;
begin
  result:=nil;
  if (FUpdateTableName='') and (FPrimeFields.Count=0) then exit;
  if Trim(FUpdateTableName) = '' then   DatabaseError(SNoUpdateTable, Self);

  if (FPrimeFields.Count = 0) and (FUpdateMode = upWhereKeyOnly) then  DatabaseError(Format(SNoPrimeFields, [FUpdateTableName]), Self);

  {FSQLGenerator.OldValuesDataSet := FOldValuesDataSet;
  FSQLGenerator.PrimeFields.Assign(FPrimeFields);
  FSQLGenerator.UpdateTableName := FUpdateTableName;
  FSQLGenerator.UpdateMode := FUpdateMode;
  FSQLGenerator.CurrentDataSet := FCurrentDataSet;}

  FSQLGenerator.CreateSQL;
  result := nil;
  if FSQLGenerator.SQLList.Count = 0 then exit;
  result := FSQLGenerator.SQLList;
end;

procedure TAstaIOCustomProvider.CheckValueDataSets;
begin
  if not Assigned(FCurrentDataSet) then DatabaseError(SMissingCurrentDataSet);
  if not Assigned(FOldValuesDataSet) then DatabaseError(SMissingOldValuesDataSet);
end;

constructor TAstaIOCustomProvider.Create(AComponent: TComponent);
begin
  inherited Create(AComponent);
  FDataBasePlugin := nil;
  FSQLGenerator := nil;
  FPrimeFields := TStringList.Create;
  FExtraClientParams := TParams.Create(Self);
  FBroadCastList:=nil;
end;

destructor TAstaIOCustomProvider.Destroy;
begin
  FreeAndNil(FOldValueBroadcastDataSet);
  FreeAndNil(FBroadCastList);
  FPrimeFields.Free;
  if Assigned(FSQLGenerator) then FSQLGenerator.Free;
  FExtraClientParams.Free;
  inherited Destroy;
end;

Function  TAstaIOCustomProvider.InternalExecSQL(SQLString: string; ExecParams: TParams; Action: Byte; ExecQuery: TDataSet = nil):Boolean;
begin
 result:=False;
end;

procedure TAstaIOCustomProvider.Open(ClientDataSet :TDataSet = nil);
var RecsOut  :Integer;
begin
  DoBeforeOpen(Self, UserRecord, FDatabase, FParams); // sm - 8/28/2002
  inherited Open(ClientDataSet);
  GetRecords(-1, RecsOut, [], '', FParams);
end;

procedure TAstaIOCustomProvider.BroadcastOldValuesCheck;
begin
if FOldValueBroadcastDataSet<>nil then FreeAndNil(FOldValueBroadcastDataSet);
if not ClientsRegisteredForBroadcast then exit;
FOldValueBroadcastDataSet:=TAstaIODataSet.Create(nil);
FOldValueBroadcastDataSet.CleanCloneFromDataSet(FOldValuesDataSet,True,True);
end;


procedure TAstaIOCustomProvider.Update;
var
  i:Integer;
  List: TAstaParamList;
  SQLCount: Integer;
  HotSpot: Integer;
  Action: Integer;
  FTmpCurrentDataSet: TAstaIODataSet;
  FTmpOldValuesDataSet: TAstaIODataSet;
  Handled,
  CurrFound: Boolean;
  ADataSet: TDataSet;
  ReturnPackage: TAstaParamList;
  ReFetchCnt: Integer;
  RefetchPackageItem: TAstaParamList;
  AutoIncrementField: String;
  BookMark: String;
  EditAction: Integer;
  PreparedString: String;
  AutoIncrementFields: TAstaParamList;
begin
  Handled:=False;
  CheckGenerator;
  CheckDataSet;
  CheckValueDataSets;
  FSQLGenerator.SQLList.Clear;
  AutoIncrementFields:=TAstaParamList.Create;
  ReturnPackage:=TAstaParamList.Create;

  ADataSet := nil;
  try
    if Assigned(FDataBasePlugin.OnSupplyDBComponent) then
      FDataBasePlugin.OnSupplyDBComponent(Self, FUserRecord, FDatabase,TComponent(ADataSet), tdbExecSQL, []);

    FTmpCurrentDataSet:=TAstaIODataSet.Create(nil);
    FTmpOldValuesDataSet:=TAstaIODataSet.Create(nil);
    try
      FTmpCurrentDataSet.InternalCleanCloneFromDataSet(FCurrentDataSet);
      FTmpOldValuesDataSet.InternalCleanCloneFromDataSet(FOldValuesDataSet);
      FTmpOldValuesDataSet.Open;
      FTmpCurrentDataSet.Open;

      FSQLGenerator.PrimeFields.Assign(FPrimeFields);
      FSQLGenerator.UpdateTableName := FUpdateTableName;
      FSQLGenerator.UpdateMode := FUpdateMode;
      FSQLGenerator.NoSQLFields.Assign(NoSQLFields);
      FSQLGenerator.PrimeFieldsValues.Clear;
      FSQLGenerator.UseNULLSyntax:=UseNULLSyntax;

      ReFetchCnt:=0;
      BroadcastOldValuesCheck;
      while FOldValuesDataSet.RecordCount > 0 do
      begin
        FTmpOldValuesDataSet.Empty;
        FTmpOldValuesDataSet.AppendSingleRecordFromSource(FOldValuesDataSet);

        HotSpot := FOldValuesDataSet.FieldByName('BookMark').AsInteger;
        Action:=FOldValuesDataSet.FieldByName('Delta').AsInteger;

        CurrFound:=False;
        if TDeltaType(Action) in [dtEdit, dtAppend] then
          if not FCurrentDataSet.Locate('BookMark', HotSpot, []) then
          begin
            // Delete does not have a currentdataset record
            CurrFound:=True;
          end;

        FTmpCurrentDataSet.Empty;
        FTmpCurrentDataSet.AppendSingleRecordFromSource(FCurrentDataSet);
        Handled:=False;
        try
          case TDeltaType(Action) of
            dtEdit:
              DoBeforeUpdate(Self, UserRecord, ADataSet,
                  FTmpOldValuesDataSet, FTmpCurrentDataSet, nil, Handled); // For now just send nil for srever dataset
            dtAppend:
              DoBeforeInsert(Self, UserRecord, ADataSet,  FTmpCurrentDataSet, Handled);
            dtDelete:
              DoBeforeDelete(Self, UserRecord, ADataSet,  FTmpOldValuesDataSet, nil, Handled); // For now just send nil for srever dataset
          end;

          if not Handled then
          begin
            FSQLGenerator.OldValuesDataSet := FTmpOldValuesDataSet;
            FSQLGenerator.CurrentDataSet := FTmpCurrentDataSet;
            List := GenerateSQL;
            if List = nil then exit;
            SQLCount := 0;
            repeat
              if not InternalExecSQL(List[SQLCount].Name,
                AstaParamsToTParams(List[SQLCount].AsParamList),
                FSQLGenerator.DeltaTypeList[SQLCount].AsInteger,
                ADataSet) then Break;
              inc(SQLCount);
            until SQLCount = List.Count;
            //FSQLGenerator.ClearSQLList; //dj 11/20/2002
          end
         else if (FSQLGenerator.PrimeFieldsValues.Count - 1) < ReFetchCnt then
        // check if valuesfor current index exist, if not create primefieldsvalues(ReFetchCnt)
         with TAstaParamList.Create do begin
              try
                 for i := 0 to FPrimeFields.Count - 1 do
                   with Add do begin
                     Name := FPrimeFields[i];
                     DataType := FTmpOldValuesDataSet.FieldbyName(FPrimeFields[i]).DataType;
                     IsNull := FTmpOldValuesDataSet.FieldbyName(FPrimeFields[i]).IsNull;
                     Value := FTmpOldValuesDataSet.FieldbyName(FPrimeFields[i]).Value;
                   end;
                 FSQLGenerator.PrimeFieldsValues.Add(AsTokenizedString);
              finally
              Free;
              end;
            end;
          FSQLGenerator.ClearSQLList;
          //07/11/02 sg added handled check

          if Assigned(RefetchPackage) and (RefetchPackage.Count > 0) {and not Handled} then // sm - 4/4/2003 - added (RefetchPackage.Count > 0) and check for TDeltaType
          begin
            try
              RefetchPackageItem:=TAstaParamList.CreateFromTokenizedString(RefetchPackage[ReFetchCnt].AsString);
              RefetchPackageItem.ParamByName('PrimeFieldsParams').AsString:=FSQLGenerator.PrimeFieldsValues[ReFetchCnt];

              if (RefetchPackageItem.Count > 0) then
              begin
              RefetchPackageItem.ParamByName('PrimeFieldsParams').AsString:=FSQLGenerator.PrimeFieldsValues[ReFetchCnt];
                AutoIncrementField:=RefetchPackageItem[3].AsString;
                BookMark:=RefetchPackageItem[4].AsString;
                EditAction:=RefetchPackageItem[5].AsInteger;

                if FDataBasePlugin.MustFetchAutoInc(EditAction, AutoIncrementField) then
                  FDataBasePlugin.FetchAutoInc(FUserRecord,
                                               EditAction,
                                               AutoIncrementField,
                                               0, //SQLOptions,
                                               FDatabase,
                                               FUpdateTableName,
                                               AutoIncrementFields);

                // Now fetch the refetches
                if FDataBasePlugin.MustFetchRefetches(EditAction) then
                  PreparedString:=FDataBasePlugin.PrepareRefetchesForTransport(FUserRecord, RefetchPackageItem, AutoIncrementFields, FUpdateTableName,
                                                                                FDatabase, 0, EditAction)
                else
                  PreparedString:='';

                with ReturnPackage.Add do
                begin
                  Name:=BookMark;
                  AsString:=PreparedString;
                end;
              end;
            finally
              RefetchPackageItem.Free;
            end;
            Inc(ReFetchCnt); // sm - 4/5/2003 - Moved 1 line up, into the block
          end;

          // Refetch stuff must go in here
          case TDeltaType(Action) of
            dtEdit:
              DoAfterUpdate(Self, UserRecord, ADataSet,
                  FTmpOldValuesDataSet, FTmpCurrentDataSet, nil);
            dtAppend:
              DoAfterInsert(Self, UserRecord, ADataSet,
                  FTmpOldValuesDataSet, FTmpCurrentDataSet, nil);
            dtDelete:
              //DoAfterDelete(Self, UserRecord, ADataSet, FTmpCurrentDataSet, nil); // Original Asta
              DoAfterDelete(Self, UserRecord, ADataSet, FTmpOldValuesDataSet, nil); // ef - 1/14/2009 - To do AfterDelete work !
          end;
        finally
          FOldValuesDataSet.Delete;
          if CurrFound and (FCurrentDataSet.RecordCount > 0) then
            FCurrentDataSet.Delete;
        end;
      end;
    finally
      // This should be 2 seperate try/finally blocks, but for now....
      FTmpCurrentDataSet.Free;
      FTmpOldValuesDataSet.Free;
    end;
  finally
    if not Assigned(FDataBasePlugin.OnSupplyDBComponent) then ADataSet.free;
    FReturnPackageString:=ReturnPackage.AsTokenizedString(False);
    AutoIncrementFields.Free;
    if Assigned(RefetchPackage) then
      ReturnPackage.Free;
  end;
end;

procedure TAstaIOCustomProvider.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = FDataSet) and (Operation = opRemove) then
  begin
    FDataSet := nil;
  end;
end;

{ TAstaIOProvider }

procedure TAstaIOProvider.SetParams(Value: TParams);
begin
  FParams.Assign(Value);
end;

procedure TAstaIOProvider.SetActive(Value: Boolean);
begin
  if Value then
    Open
  else
    Close;
end;

function TAstaIOProvider.GetActive: Boolean;
begin
  CheckDataSet;
  Result := FDataSet.Active;
end;

Function TAstaIOProvider.InternalExecSQL(SQLString: string; ExecParams: TParams; Action: Byte; ExecQuery: TDataSet = nil):Boolean;
var
  RowsAffected: Integer;
begin
  if Assigned(FDataBasePlugin.OnExecSQL) then
  begin
  result:=FDAtabasePlugin.DoExecSQLEvent(FuserRecord,ExecQuery,FDatabase,SQLString,ExecParams,RowsAffected,False);

(*    FDataBasePlugin.ExecSQLEvent(Self,
      FUserRecord,
      TComponent(ExecQuery),
      FDatabase,
      SQLString,
      ExecParams,
      RowsAffected); // Do we need to send it back? *)
  end;
end;

function TAstaIOProvider.GetPrimeFields: TStrings;
begin
  result := FPrimeFields;
end;

procedure TAstaIOProvider.SetPrimeFields(Value: TStrings);
begin
  FPrimeFields.Assign(Value);
end;

procedure TAstaIOProvider.GetParamsFromDataSet;
begin
  if Assigned(DataSet) then
  begin
    FParams.Clear;
    FDatabasePlugIn.CreateProviderParams(Self, FParams, DataSet);
  end;
end;

procedure TAstaIOProvider.SetDataSetParams;
begin
  if Assigned(DataSet) then
  begin
    FDatabasePlugIn.SetProviderDataSetParams(FUserRecord,Self,FParams);
  end;
end;

function TAstaIOProvider.GetParams: TParams;
begin
  Result := FParams;
end;

procedure TAstaIOProvider.DoBeforeBroadCastPrepare;
begin
  if BroadCastRequested then FBroadCastList:=TAstaParamList.Create
end;

Function TAstaIOCustomProvider.CurrentDataSetForBroadcastList:String;
var
d:TAstaIODataSet;
i:Integer;
begin
 d:=TAstaIODataSet.create(nil);
 try
  d.RetrieveFieldsFromDataSet(CurrentDataSet,False);
  d.AddField('Delta',ftInteger,0);
  d.DataTransfer(CurrentDataSet,True, False);
  d.First;
  while not d.eof do begin
   if FOldValueBroadcastDataSet.Locate('Bookmark',d.FieldbyName('BookMark').AsInteger,[]) then begin
    d.edit;
    d.FieldByName('delta').Asinteger:=FOldValueBroadcastDataSet.Fieldbyname('Delta').AsInteger;
    d.post;
   end;
   d.next;
  end;
  d.first;
  //for some reason currentvalueDataset has a row with NULL values
  //removed by using the bookmark above
  (*while not d.eof do
   if d.fieldbyname('Delta').IsNull then d.delete
    else d.next; *)

  FOldValueBroadcastDataSet.First;
  while not FOldValueBroadcastDataSet.eof do begin
    if FOldValueBroadcastDataSet.fieldByName('Delta').AsInteger=ord(dtDelete) then begin
     d.append;
     for i:=0 to d.fieldcount-1 do
       if FOldValueBroadCastDataSet.findField(d.fields[i].FieldName)<>nil then
       d.fields[i].Assign(FOldValueBroadcastDataSet.FieldByName(D.fields[i].FieldName));
     d.post;
     end;
     FOldValueBroadcastDataSet.Next;
  end;
  result:=DataSetToString(D);
  finally
   d.free;
 end;

end;


procedure TAstaIOProvider.DoAfterTransaction(Success: Boolean);
begin
  if Assigned(FAfterTransactionEvent) then
    FAfterTransactionEvent(Self, UserRecord, CurrentDataSet, Success);
  // broadcast requested
//  if not TAstaIOServerWire(FDatabasePlugin.ServerWire).BroadcastList.ProviderRegisteredForBroadcast(Self.Name)
//   then exit;
  if not success then begin
   if assigned(FOldValueBroadcastDataSet) then freeAndNil(FOldValueBroadcastDataSet);
   exit;
  end;
  DoBeforeBroadcastPrepare;
  if (FBroadCastList<>nil) then begin
   DoBeforeBroadCast(Self,UserRecord,FBroadCastList,CurrentDataSet);
   FBroadCastList.FastAdd('CurrentDataSet',CurrentDataSetForBroadcastList);
   ProviderBroadCast;
  end;
end;

procedure TAstaIOProvider.DoBeforeTransaction(var Handled: Boolean);
begin
  //if BroadCastRequested then FBroadCastList:=TAstaParamList.Create
  //else FreeAndNil(FBroadCastList);
  if Assigned(FBeforeTransactionEvent) then
    FBeforeTransactionEvent(Self, UserRecord, OldValuesDataSet, CurrentDataSet, Handled);
end;

procedure TAstaIOCustomProvider.DoBeforeOpen(Sender: TObject; U: TUserRecord;
  DatabaseString :String; Params: TParams);
begin
  if Assigned(FBeforeOpenEvent) then
    FBeforeOpenEvent(Sender, U, DatabaseString, Params); // sm - 8/28/2002
end;

procedure TAstaIOCustomProvider.DoBeforeDelete(Sender: TObject; U: TUserRecord;
  ExecQuery: TComponent; OriginalValueDataSet,
  ServerValueDataSet: TDataSet; var Handled: Boolean);
begin
  if Assigned(FBeforeDeleteEvent) then
    FBeforeDeleteEvent(Sender, U, ExecQuery, OriginalValueDataSet, ServerValueDataSet, Handled);
end;

procedure TAstaIOCustomProvider.DoBeforeInsert(Sender: TObject; U: TUserRecord;
  ExecQuery: TComponent; CurrentValueDataSet : TDataSet; var Handled: Boolean);
begin
  if Assigned(FBeforeInsertEvent) then
    FBeforeInsertEvent(Sender, U, ExecQuery, CurrentValueDataSet, Handled);
end;

procedure TAstaIOCustomProvider.DoBeforeBroadCast(Sender: TObject; U: TUserRecord;BroadCastList:TAstaParamList;
  CurrentValueDataSet: TDataSet);
begin
 if Assigned(FBeforeBroadcastEvent) then
    FBeforeBroadCastEvent(Sender, UserRecord, BroadCastList,CurrentValueDataSet);
end;

procedure TAstaIOCustomProvider.DoBeforeUpdate(Sender: TObject; U: TUserRecord;
  ExecQuery: TComponent; OriginalValueDataSet, CurrentValueDataSet,
  ServerValueDataSet: TDataSet; var Handled: Boolean);
begin
  if Assigned(FBeforeUpdateEvent) then
    FBeforeUpdateEvent(Sender, U, ExecQuery, OriginalValueDataSet, CurrentValueDataSet, ServerValueDataSet, Handled);
end;

procedure TAstaIOCustomProvider.DoAfterDelete(Sender: TObject; U: TUserRecord;
  ExecQuery: TComponent; OriginalValueDataSet,
  ServerValueDataSet: TDataSet);
begin
  if Assigned(FAfterDeleteEvent) then
    FAfterDeleteEvent(Sender, U, ExecQuery, OriginalValueDataSet, ServerValueDataSet);
end;

procedure TAstaIOCustomProvider.DoAfterInsert(Sender: TObject; U: TUserRecord;
  ExecQuery: TComponent; OriginalValueDataSet, CurrentValueDataSet,
  ServerValueDataSet: TDataSet);
begin
  if Assigned(FAfterInsertEvent) then
    FAfterInsertEvent(Sender, U, ExecQuery, CurrentValueDataSet);
end;

procedure TAstaIOCustomProvider.DoAfterUpdate(Sender: TObject; U: TUserRecord;
  ExecQuery: TComponent; OriginalValueDataSet, CurrentValueDataSet,
  ServerValueDataSet: TDataSet);
begin
  if Assigned(FAfterUpdateEvent) then
    FAfterUpdateEvent(Sender, U, ExecQuery, OriginalValueDataSet, CurrentValueDataSet, ServerValueDataSet);
end;

Function TAstaIOCustomProvider.BroadCastRequested:Boolean;
begin
 result:=TAstaIOServerWire(FDatabasePlugin.ServerWire).BroadCastList.ProviderBroadcastRequested(FUserRecord,Self);

end;

Function TAstaIOCustomProvider.ClientsRegisteredForBroadcast:Boolean;
begin
result:= TAstaIOServerWire(FDatabasePlugin.ServerWire).ClientsRegisteredForBroadcast(self);
end;

procedure TAstaIOCustomProvider.ProviderBroadcast;
begin
  if (FBroadCastList<>Nil) then begin
   TAstaIOServerWire(FDatabasePlugin.ServerWire).DoProviderBroadcast(FUserRecord,FBroadCastList.AsTokenizedString(False),Self);
   FreeAndNil(FBroadCastList);
  end;
end;

end.

