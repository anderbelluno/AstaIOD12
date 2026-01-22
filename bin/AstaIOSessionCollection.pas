{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10307: AstaIOSessionCollection.pas 
{
{   Rev 1.0    4/10/2003 6:32:10 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:38:12 PM  Steve    Version: 1.505
}
unit AstaIOSessionCollection;
{*********************************************************}
{*   Copyright (c) 1997-2002 Asta Technology Group Inc.  *}
{*               All rights reserved.                    *}
{*               www.astatech.com                        *}
{*********************************************************}
{$I AstaIO.inc}

interface
uses Classes,
  DB,
  AstaIODBConst,
  AstaIOCustomDataSet;

const
  ServerComponentsDataSetName = 'ServerComponents';
  ServerProvidersDataSetName = 'Providers';
  ServerIProvidersDataSetName = 'IProviders';
  ServerMethodsDataSetName = 'ServerMethods';
  ServerMethodsExecDataSetName = 'ServerMethodsExec';
  ServerSoapServicesDataSetName = 'SoapServices';
  sfld_ServerMethod = 'ServerMethod';
  sfld_Params = 'Params';
  sfld_DataModule = 'DataModule';
  sfld_DataSetName = 'DataSetName';
  sfld_DataSet = 'DataSet';
  sfld_IProviderName = 'IProviderName';
  sfld_ProviderName = 'ProviderName';
  sfld_UpdateTableName='UpdateTablename';
  sfld_PrimekeyFields='PrimeKeyFields';
type
  TAstaInfoDataSetList = class;
  TAstaDataModuleInventoryEvent = procedure(Sender: TObject; ASession: TObject; ASessionName: string;
    DataModuleList: TList; List: TAstaInfoDataSetList) of object;
  TAstaInfoDataSetList = class(TStringList)
   // String is Name eg Providers Object is TAstaIOCustomDataSet
    procedure AddConsolidatedInventoryDataSet;
    destructor destroy; override;
    procedure AddDataSet(DataSetName: string; D: TDataset);
    function DataSetByName(DataSetName: string): TDataSet;
    procedure TakeInventory(DataModuleList: TList; Plugin: TComponent);
    function CreateServerInventoryDataset(MetaDataRequest: TAstaMetaData): TAstaIODataSet;
    function GetServerInventoryDataset(MetaDataRequest: TAstaMetaData): TDataSet;
  end;

  TAstaIOServerDMInventoryList = class(TStringList)
   private
    //FCriticalSection:TCriticalSection;
   public
    function GetInfoDataSetList(SessionName: string): TAstaInfoDataSetList;
    function DataSetByName(SessionName, DataSetName: string): TDataSet;
    destructor Destroy; override;
    procedure AddSession(SessionName: string; L: TAstaInfoDataSetList);
  end;

  TSessionDataModuleList = class(TStringList)
  private
    FMainDataModule: TComponent;
  public
    property DataModule: TComponent read FMainDataModule write FMainDataModule;
    constructor Create(DM: TComponent);
    destructor destroy; override;
    procedure AddDataModule(DM: TComponent);
    function FindComponent(ComponentName: string): TComponent;
  end;

  TSessionDefineItem = class(TCollectionItem)
  private
    FDefault:Boolean;
    FSessionName: string;
    FMaxSessions: integer;
    FMaxAsyncSessions: Integer;
    FInitialSessions: Integer;
    FAliasList: TStrings;
    function GetAliasList: TStrings;
    procedure SetAliasList(Value: TStrings);
    procedure SetDefault(Value:Boolean);
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure AssignTo(Dest: TPersistent); override;
  published
    property SessionName: string read FSessionName write FSessionName;
    property Default:Boolean read FDefault write SetDefault;
    property MaxSessions: Integer read FMaxSessions write FMaxSessions default -1;
    property MaxAsyncSessions: Integer read FMaxAsyncSessions write FMaxAsyncSessions default -1;
    property InitialSessions: Integer read FInitialSessions write FInitialSessions default 1;
    property AliasList: TStrings read GetAliasList write SetAliasList;
  end;

  TSessionDefineItems = class(TCollection)
  private
    FDatabasePlugin: TComponent;
    function GetSessionItem(Index: Integer): TSessionDefineItem;
    procedure SetSessionItem(Index: Integer; Value: TSessionDefineItem);
  protected
    function GetAttrCount: Integer; override;
    function GetAttr(Index: Integer): string; override;
    function GetItemAttr(Index, ItemIndex: Integer): string; override;
    function GetOwner: TPersistent; override;
    procedure SetItemName(Item: TCollectionItem); override;
  ////  procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AnOwner: TComponent; ItemClass: TCollectionItemClass);
    function Add: TSessionDefineItem;
    property Items[Index: Integer]: TSessionDefineItem read GetSessionItem
    write SetSessionItem; default;
  end;

implementation
uses SysUtils,
  AstaIOProvider,
  AstaIOIProvider,
  AstaIOServerMethod,
  AstaIOParamList,
  AstaIODatabasePlugin,
  AstaIOUtil;
procedure ErrorMsg(const Msg: string);
begin
  raise Exception.Create(Msg);
end;

constructor TSessionDefineItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FSessionName := 'Session' + IntToStr(Collection.Count);
  FMaxSessions := -1;
  FMaxAsyncSessions := -1;
  FInitialSessions := 1;
  FAliasList := TStringList.Create;
  FDefault:=False;
  if Collection.Count=1 then FDefault:=True;
end;

function TSessionDefineItem.GetAliasList: TStrings;
begin
  result := FAliasList;
end;

procedure TSessionDefineItem.SetDefault(Value:Boolean);
var
i:Integer;
begin
 FDefault:=Value;
 if Value then begin
   for i:=0 to Collection.Count-1 do
    if (Collection.items[i]<>self) and (TSessionDefineItem(Collection.items[i]).FDefault) then
     TSessionDefineItem(Collection.items[i]).FDefault:=False;
 end;
end;

procedure TSessionDefineItem.SetAliasList(Value: TStrings);
begin
  FAliasList.Assign(value);
end;

procedure TSessionDefineItem.AssignTo(Dest: TPersistent);
begin
  if Dest is TSessionDefineItem then
  begin
    if Assigned(Collection) then Collection.BeginUpdate;
    try
      with TSessionDefineItem(Dest) do
      begin
        self.FAliasList.Assign(FAliasList);
        FSessionname := Self.SessionName;
        FMaxSessions := Self.MaxSessions;
        FMaxAsyncSessions := self.MaxAsyncSessions;
        FInitialSessions := Self.InitialSessions;
        FDefault := Self.Default;
      end;
    finally
      if Assigned(Collection) then Collection.EndUpdate;
    end;
  end
  else
    inherited AssignTo(Dest);
end;

destructor TSessionDefineItem.Destroy;
begin
  FAliasList.Free;
  inherited Destroy;
end;

constructor TSessionDefineItems.Create(AnOwner: TComponent; ItemClass: TCollectionItemClass);
begin
  inherited Create(ItemClass);
  FDatabasePlugin := AnOwner;
end;

function TSessionDefineItems.Add: TSessionDefineItem;
begin
  Result := TSessionDefineItem(inherited Add);
end;

function TSessionDefineItems.GetSessionItem(Index: Integer): TSessionDefineItem;
begin
  Result := TSessionDefineItem(inherited Items[Index]);
end;

function TSessionDefineItems.GetAttrCount: Integer;
begin
  Result := 6;
end;

function TSessionDefineItems.GetAttr(Index: Integer): string;
begin
  result := '';
  case Index of
    0: Result := 'Session Name';
    1: Result := 'Maximum Sessions';
    2: Result := 'MaximumAsyncSessions';
    3: Result := 'IntialSessions';
    4: result := 'Aliases';
    5: result := 'Default';
  end;
end;

function TSessionDefineItems.GetItemAttr(Index, ItemIndex: Integer): string;
begin
  result := '';
  case Index of
    0: Result := Items[ItemIndex].FSessionName;
    1: Result := IntToStr(Items[ItemIndex].FMaxSessions);
    2: result := IntToStr(Items[ItemIndex].FMaxAsyncSessions);
    3: result := IntToStr(Items[ItemIndex].FInitialSessions);
    4:
      case Items[ItemIndex].FAliasList.Count of
        0: result := 'No Aliases';
        1: result := 'Single Alias';
      else
        result := intToStr(Items[ItemIndex].FAliasList.Count);
      end;
    5: if Items[ItemIndex].Default then result:='True' else result:='False';
  end;
end;

function TSessionDefineItems.GetOwner: TPersistent;
begin
  Result := FDatabasePlugin;
end;

procedure TSessionDefineItems.SetSessionItem(Index: Integer; Value: TSessionDefineItem);
begin
  Items[Index].Assign(Value);
end;

procedure TSessionDefineItems.SetItemName(Item: TCollectionItem);
var
  I, J: Integer;
  ItemName: string;
  CurItem: TSessionDefineItem;
begin
  J := 1;
  while True do
  begin
    ItemName := 'Session' + IntToStr(j);
    I := 0;
    while I < Count do
    begin
      CurItem := Items[I] as TSessionDefineItem;
      if (CurItem <> Item) and (CompareText(CurItem.SessionName, ItemName) = 0) then
      begin
        Inc(J);
        Break;
      end;
      Inc(I);
    end;
    if I >= Count then
    begin
      (Item as TSessionDefineItem).SessionName := ItemName;
      Break;
    end;
  end;
end;

constructor TSessionDataModuleList.Create(DM: TComponent);
begin
  inherited Create;
  FMainDataModule := DM;
end;

destructor TSessionDataModuleList.destroy;
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
    TComponent(Objects[i]).Free;
  FMainDataModule.Free; //??
  inherited Destroy;
end;

procedure TSessionDataModuleList.AddDataModule(DM: TComponent);
begin
  AddObject(DM.Name, DM);
end;

function TSessionDataModuleList.FindComponent(ComponentName: string): TComponent;
var
  i: Integer;
begin
  result := FMainDataModule.findComponent(ComponentName);
  if Result <> nil then exit;
  for i := 0 to Count - 1 do
  begin
    result := TComponent(objects[i]).FindComponent(ComponentName);
    if result <> nil then exit;
  end;
end;

procedure TAstaInfoDataSetList.TakeInventory(DataModuleList: TList; Plugin: TComponent);
var
  i, j: Integer;
  procedure addProvider(P: TAstaIOProvider);
  begin
    P.DatabasePlugin := Plugin as TAstaIODataBasePlugin;
    P.GetParamsFromDataSet;
    with DataSetbyName(ServerProvidersDataSetname) do
    begin
      Append;
      FieldByName(sfld_ProviderName).AsString := P.Name;
      Fieldbyname(sfld_DataModule).AsString := P.Owner.Name;
      FieldByname(sfld_Params).AsString := TParamsToAstaParamsString(P.Params);
      FieldByname(sfld_UpdateTableName).AsString := P.UpdateTableName;
      FieldByname(sfld_PrimekeyFields).AsString:=P.PrimeFields.text;
      Post;
    end;
  end;

  procedure addIProvider(P: TAstaIOIProvider);
  var Params :TParams;
  begin
    P.DatabasePlugin := Plugin as TAstaIODataBasePlugin;
    with DataSetbyName(ServerIProvidersDataSetname) do
    begin
      Append;
      FieldByName(sfld_IProviderName).AsString := P.Name;
      Fieldbyname(sfld_DataModule).AsString := P.Owner.Name;
      Params:=TParams.Create(nil);
      try
        Params.Assign(P.GetParams);
        if Params <> nil then
          FieldByname(sfld_Params).AsString := TParamsToAstaParamsString(Params);
      finally
        Params.Free;
      end;
      Post;
    end;
  end;

  procedure addServerMethod(S: TAstaIOServerMethodResultSet);
  begin
    with DataSetbyName(ServerMethodsDataSetName) do
    begin
      S.FreshenParams;
      Append;
      FieldByName(sfld_ServerMethod).AsString := S.Name;
      FieldByName(sfld_DataModule).AsString := S.Owner.Name;
      FieldByname(sfld_Params).AsString := TParamsToAstaParamsString(S.Params);
      if s.provider<>nil then
      FieldByName(sfld_ProviderName).AsString:=S.Provider.Name;
      Post;
    end;
  end;

  procedure addServerMethodExec(S: TAstaIOServerMethodExec);
  begin
    with DataSetbyName(ServerMethodsExecDataSetName) do
    begin
      Append;
      FieldByName(sfld_ServerMethod).AsString := S.Name;
      FieldByName(sfld_DataModule).AsString := S.Owner.Name;
      FieldByname(sfld_Params).AsString := TParamsToAstaParamsString(S.Params);
      Post;
    end;
  end;
  procedure addSoapService(S: TAstaIOServerMethodExec);
  begin
    with DataSetbyName(ServerSoapServicesDataSetName) do
    begin
      Append;
      FieldByName(sfld_ServerMethod).AsString := S.Name;
      FieldByName(sfld_DataModule).AsString := S.Owner.Name;
      FieldByname(sfld_Params).AsString := TParamsToAstaParamsString(S.Params);
      Post;
    end;

  end;
  var
    DM, C: TComponent;
begin
  AddDataSet(ServerProvidersDataSetName, CreateServerInventoryDataset(mdProviders));
  AddDataSet(ServerIProvidersDataSetName, CreateServerInventoryDataset(mdIProviders));
  AddDataSet(ServerMethodsDataSetName, CreateServerInventoryDataset(mdServerMethods));
  AddDataSet(ServerMethodsExecDataSetName, CreateServerInventoryDataset(mdServerMethodsExec));
  AddDataSet(ServerSoapServicesDataSetName, CreateServerInventoryDataset(mdSoapServices));
  for i := 0 to DataModuleList.Count - 1 do
  begin
    DM := DataModuleList[i];

    if  DM <> nil then
      for j := 0 to TComponent(DataModuleList[i]).ComponentCount - 1 do
      begin
        C := DM.Components[j];

        if CheckClass(C, TAstaIOProvider) then
          AddProvider(TAstaIOProvider(C))
        else
        //if C is TAstaIOIProvider then
        if CheckClass(C, TAstaIOIProvider) then
          AddIProvider(TAstaIOIProvider(C))
        else
        //if C is TAstaIOServerMethodResultSet then
        if CheckClass(C, TAstaIOServerMethodResultSet) then
          AddServerMethod(TAstaIOServerMethodResultSet(C))
        else
        //if C is TAstaIOServerMethodExec then begin
        if CheckClass(C, TAstaIOServerMethodExec) then begin
          AddServerMethodExec(TAstaIOServerMethodExec(C));
          if TAstaIOServerMethodExec(c).SoapService then
          AddSoapService(TAstaIOServerMethodExec(C));
        end;
      end;
  end;

  AddConsolidatedInventoryDataSet;
end;

procedure TAstaInfoDataSetList.AddConsolidatedInventoryDataSet;
  var
    DS: TDataSet;

  procedure AddRec(const Name: string);
  begin
    DS.AppendRecord([Name,
                  DataSetToString(TAstaIODataSet(DataSetByName(Name)))]);
  end;

begin
  DS := CreateServerInventoryDataSet(mdServerMethodsAndProviders);
  AddDataSet(ServerComponentsDataSetName, DS);
  AddRec(ServerProvidersDataSetName);
  AddRec(ServerIProvidersDataSetName);
  AddRec(ServerMethodsDataSetName);
  AddRec(ServerMethodsExecDataSetName);
  AddRec(ServerSoapServicesDataSetName);
end;

function TAstaInfoDatASetList.GetServerInventoryDataset(MetaDataRequest: TAstaMetaData): TDataSet;
begin
  //this all needs to come back in one cached call to the client in a combined dataset
  case MetaDataRequest of
    mdproviders: result := DataSetByName(ServerProvidersDataSetName);
    mdproviderParams: result := DataSetByName(ServerProvidersDataSetName);
    mdIProviders: result := DataSetByName(ServerIProvidersDataSetName);
    mdIProviderParams: result := DataSetByName(ServerIProvidersDataSetName);
    mdServerMethods: result := DataSetByName(ServerMethodsDataSetName);
    mdServerMethodParams: result := DataSetByName(ServerMethodsDataSetName);
    mdServerMethodExecParams: result := DataSetByName(ServerMethodsExecDataSetName);
    mdServerMethodsExec: result := DataSetByName(ServerMethodsExecDataSetName);
    mdServerMethodsandProviders: result := DataSetByName(ServerComponentsDataSetName);
    mdSoapServices: result := DataSetByName(ServerSoapServicesDataSetName);
    mdSoapServiceParams: result := DataSetByName(ServerSoapServicesDataSetName);
  else
    result := nil;
  end;
end;

function TAstaInfoDataSetList.CreateServerInventoryDataset(MetaDataRequest: TAstaMetaData): TAstaIODataSet;
begin
  result := nil;
  case MetaDataRequest of
    mdProviders:
      begin
        Result := TAstaIODataSet.Create(nil);
        Result.AddField(sfld_ProviderName, ftString, 60, True);
        Result.AddField(sfld_DataModule, ftString, 60, True);
        Result.AddField(sfld_Params, ftMemo, 0, False);
        result.AddField(sfld_UpdateTAblename, ftString, 60, True);
        result.addfield(sfld_primekeyFields, ftmemo, 0, True);
        Result.Open;
      end;
    mdIProviders:
      begin
        Result := TAstaIODataSet.Create(nil);
        Result.AddField(sfld_IProviderName, ftString, 60, True);
        Result.AddField(sfld_DataModule, ftString, 60, True);
        Result.AddField(sfld_Params, ftMemo, 0, False);
        Result.Open;
      end;
    mdServerMethods:
      begin
        Result := TAstaIODataSet.Create(nil);
        Result.AddField(sfld_ServerMethod, ftString, 60, True);
        Result.AddField(sfld_DataModule, ftString, 60, True);
        Result.AddField(sfld_Params, ftMemo, 0, False);
        result.addfield(sfld_ProviderName,ftstring,60,false);
        Result.Open;
      end;
    mdServerMethodsExec,mdSoapServices:
      begin
        Result := TAstaIODataSet.Create(nil);
        Result.AddField(sfld_ServerMethod, ftString, 60, True);
        Result.AddField(sfld_DataModule, ftString, 60, True);
        Result.AddField(sfld_Params, ftMemo, 0, False);
        Result.Open;
      end;
    mdServerMethodsAndProviders:
      begin
        Result := TAstaIODataSet.Create(nil);
        Result.AddField(sfld_DataSetName, ftString, 60, True);
        Result.AddField(sfld_DataSet, ftblob, 0, True);
        Result.Open;
      end;
  end;
end;

destructor TAstaInfoDataSetList.destroy;
var
  i: Integer;
begin
  for i := 0 to count - 1 do
    objects[i].Free;
  inherited Destroy;
end;

procedure TAstaInfoDataSetList.AddDataSet(DataSetName: string; D: TDataset);
begin
  AddObject(DataSetName, D);
end;

function TAstaInfoDataSetList.DataSetByName(DataSetName: string): TDataSet;
var
  spot: Integer;
begin
  Result := nil;
  spot := Indexof(DataSetName);
  if spot < 0 then
    ErrorMsg(DataSetname + ' not found ')
  else
    Result := TDataSet(Objects[spot]);
end;

function TAstaIOServerDMInventoryList.GetInfoDataSetList(SessionName: string): TAstaInfoDataSetList;
var
  spot: Integer;
begin
  result := nil;
  if SessionName='' then spot:=0 else
  spot := Indexof(SessionName);
  if spot < 0 then
    ErrorMsg(SessionName + ' not found')
  else
    result := Objects[Spot] as TAstaInfoDatasetList;
end;

procedure TAstaIOServerDMInventoryList.AddSession(SessionName: string; L: TAstaInfoDataSetList);
begin
  AddObject(SessionName, L);
end;

function TAstaIOServerDMInventoryList.DataSetByName(SessionName, DataSetName: string): TDataSet;
var
  L: TAstaInfoDataSetList;
begin
  L := GetInfoDataSetList(SessionName);
  if  L = nil then
    Result := nil
  else
    Result := L.DataSetByName(DataSetName);
end;

destructor TAstaIOServerDMInventoryList.Destroy;
var
  i: integer;
begin
  for i := 0 to count - 1 do
    TAstaInfoDatasetList(objects[i]).Free;
  inherited Destroy;
end;

end.

