{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10237: AstaIOMetaData.pas 
{
{   Rev 1.0    4/10/2003 6:31:34 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:37:46 PM  Steve    Version: 1.505
}
unit AstaIOMetaData;
{*********************************************************}
{*   Copyright (c) 1997-2002 Asta Technology Group Inc.  *}
{*               All rights reserved.                    *}
{*               www.astatech.com                        *}
{*********************************************************}

interface

uses Classes,
  DB,
  {$IFDEF LINUX}
  Libc,
  {$ELSE}
  Windows,
  {$IFDEF FRAMEWORK_FMX }
    FMX.Dialogs,
  {$ELSE}
    VCL.Dialogs,
  {$ENDIF}
  {$ENDIF}
  Sysutils,
  AstaIOCustomDataSet,
  AstaIODBInfo,
  AstaIOSQLUtils,
  AstaIOUserList,
  AstaIODBConst;

type
  TBeforeMetaDataEvent = procedure(Sender: TObject;
    U: TUserRecord;
    MetaDataRequest: TAstaMetaData;
    DataBaseName: string;
    TableName: string) of object;
  TAfterMetaDataEvent = procedure(Sender: TObject;
    U: TUserRecord;
    MetaDataRequest: TAstaMetaData;
    DataBaseName: string;
    TableName: string) of object;
  TOnOtherMetaDataEvent = procedure(Sender: TObject;
    U: TUserRecord;
    MetaDataRequest: TAstaMetaData;
    MetaDataSet: TAstaIODataSet;
    DataBaseName: string;
    TableName: string) of object;
  TOnDBMSNameEvent = procedure(Sender: TObject;
    U: TUserRecord;
    MetaDataSet: TAstaIODataSet) of object;
  TOnTablesEvent = procedure(Sender: TObject;
    U: TUserRecord;
    MetaDataRequest: TAstaMetaData;
    MetaDataSet: TAstaIODataSet;
    DataBaseName: string;
    TableName: string) of object;
  TOnIndexesEvent = procedure(Sender: TObject;
    U: TUserRecord;
    MetaDataRequest: TAstaMetaData;
    MetaDataSet: TAstaIODataSet;
    DataBaseName: string;
    TableName: string) of object;
  TOnFieldsEvent = procedure(Sender: TObject;
    U: TUserRecord;
    MetaDataRequest: TAstaMetaData;
    MetaDataSet: TAstaIODataSet;
    DataBaseName: string;
    TableName: string) of object;
  TOnViewsEvent = procedure(Sender: TObject;
    U: TUserRecord;
    MetaDataRequest: TAstaMetaData;
    MetaDataSet: TAstaIODataSet;
    DataBaseName: string;
    TableName: string) of object;
  TOnStoredProcsEvent = procedure(Sender: TObject;
    U: TUserRecord;
    MetaDataRequest: TAstaMetaData;
    MetaDataSet: TAstaIODataSet;
    DataBaseName: string;
    TableName: string) of object;
  TOnForeignKeysEvent = procedure(Sender: TObject;
    U: TUserRecord;
    MetaDataRequest: TAstaMetaData;
    MetaDataSet: TAstaIODataSet;
    DataBaseName: string;
    TableName: string) of object;
  TOnSystemTablesEvent = procedure(Sender: TObject;
    U: TUserRecord;
    MetaDataRequest: TAstaMetaData;
    MetaDataSet: TAstaIODataSet;
    DataBaseName: string;
    TableName: string) of object;
  TOnTriggersEvent = procedure(Sender: TObject;
    U: TUserRecord;
    MetaDataRequest: TAstaMetaData;
    MetaDataSet: TAstaIODataSet;
    DataBaseName: string;
    TableName: string) of object;
  TOnPrimeKeysEvent = procedure(Sender: TObject;
    U: TUserRecord;
    MetaDataRequest: TAstaMetaData;
    MetaDataSet: TAstaIODataSet;
    DataBaseName: string;
    TableName: string) of object;
  TOnStoredProcColumnsEvent = procedure(Sender: TObject;
    U: TUserRecord;
    MetaDataRequest: TAstaMetaData;
    MetaDataSet: TAstaIODataSet;
    DataBaseName: string;
    TableName: string) of object;
  TOnDirectoryEvent = procedure(Sender: TObject;
    U: TUserRecord;
    MetaDataRequest: TAstaMetaData;
    MetaDataSet: TAstaIODataSet;
    DataBaseName: string;
    Path: string) of object;
  TOnSystemInfoEvent = procedure(Sender: TObject;
    U: TUserRecord;
    MetaDataRequest: TAstaMetaData;
    MetaDataSet: TAstaIODataSet) of object;
  TOnAllEvent = procedure(Sender: TObject;
    U: TUserRecord;
    MetaDataRequest: TAstaMetaData;
    MetaDataSet: TAstaIODataSet;
    DataBaseName: string) of object;

type
  TMetaData = class
    MetaDataRequest: TAstaMetaData;
    DataSet: TAstaIODataSet;
  end;

type
  TAstaIOMetaData = class(TComponent)
  private
    FAbout: String;

    FBeforeMetaData: TBeforeMetaDataEvent;
    FAfterMetaData: TAfterMetaDataEvent;
    FOnOtherMetaData: TOnOtherMetaDataEvent;
    FOnDBMSName: TOnDBMSNameEvent;
    FOnTables: TOnTablesEvent;
    FOnAll: TOnAllEvent;
    FOnIndexes: TOnIndexesEvent;
    FOnFields: TOnFieldsEvent;
    FOnViews: TOnViewsEvent;
    FOnStoredProcs: TOnStoredProcsEvent;
    FOnForeignKeys: TOnForeignKeysEvent;
    FOnSystemTables: TOnSystemTablesEvent;
    FOnPrimeKeys: TOnPrimeKeysEvent;
    FOnStoredProcColumns: TOnStoredProcColumnsEvent;
    FOnTriggers: TOnTriggersEvent;
    FOnDirectory: TOnDirectoryEvent;
    FOnSystemInfo: TOnSystemInfoEvent;

    FDefault: TAstaIODataSet;
    FOtherMetaData: TAstaIODataSet;
    FSessionINfo: TAstaIODataSet;
    FDBMSName: TAstaIODataSet;
    FTables: TAstaIODataSet;
    FTriggers: TAstaIODataSet;
    FIndexes: TAstaIODataSet;
    FFields: TAstaIODataSet;
    FViews: TAstaIODataSet;
    FStoredProcs: TAstaIODataSet;
    FForeignKeys: TAstaIODataSet;
    FSystemTables: TAstaIODataSet;
    FPrimeKeys: TAstaIODataSet;
    FStoredProcColumns: TAstaIODataSet;
    // FDataModule: TDataModule;
    FMetaDataList: TList;
    FDirectory: TAstaIODataSet;
    FSystemInfo: TAstaIODataSet;
    FAll: TAstaIODataSet;
    FDBInfo: TAstaIODBInfo;
    function GetMetaDataItem(MetaDataRequest: TAstaMetaData): TMetaData;
    procedure GetDirectory(const Path: string);
    procedure GetSysInfo;
    procedure GetAll(U: TUserRecord; DataBaseName: string);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    property MetaDataList: TList read FMetaDataList;
    property MetaData[MetaDataRequest: TAstaMetaData]: TMetaData read GetMetaDataItem;
    // property DataModule: TDataModule read FDataModule write FDataModule;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetMetaData(Sender: TObject;
      U: TUserRecord;
      MetaDataRequest: TAstaMetaData;
      DataBaseName: string;
      TableName: string): TAstaIODataSet;
    procedure InitDataSet(MetaDataRequest: TAstaMetaData);
  published
    property About: String read FAbout write FAbout;
    property BeforeMetaData: TBeforeMetaDataEvent read FBeforeMetaData write FBeforeMetaData;
    property AfterMetaData: TAfterMetaDataEvent read FAfterMetaData write FAfterMetaData;
    property OnOtherMetaData: TOnOtherMetaDataEvent read FOnOtherMetaData write FOnOtherMetaData;
    property OnDBMSName: TOnDBMSNameEvent read FOnDBMSName write FOnDBMSName;
    property OnTables: TOnTablesEvent read FOnTables write FOnTables;
    property OnAll: TOnAllEvent read FOnAll write FOnAll;
    property OnTriggers: TOnTriggersEvent read FOnTriggers write FOnTriggers;
    property OnIndexes: TOnIndexesEvent read FOnIndexes write FOnIndexes;
    property OnFields: TOnFieldsEvent read FOnFields write FOnFields;
    property OnViews: TOnViewsEvent read FOnViews write FOnViews;
    property OnStoredProcs: TOnStoredProcsEvent read FOnStoredProcs write FOnStoredProcs;
    property OnForeignKeys: TOnForeignKeysEvent read FOnForeignKeys write FOnForeignKeys;
    property OnSystemTables: TOnSystemTablesEvent read FOnSystemTables write FOnSystemTables;
    property OnPrimeKeys: TOnPrimeKeysEvent read FOnPrimeKeys write FOnPrimeKeys;
    property OnStoredProcColumns: TOnStoredProcColumnsEvent read FOnStoredProcColumns write FOnStoredProcColumns;
    property OnDirectory: TOnDirectoryEvent read FOnDirectory write FOnDirectory;
    property OnSystemInfo: TOnSystemInfoEvent read FOnSystemInfo write FOnSystemInfo;
    property DBInfo: TAstaIODBInfo read FDBInfo write FDBInfo;
  end;

implementation

{ TAstaIOMetaData }

constructor TAstaIOMetaData.Create(AOwner: TComponent);
var
  AMetaData: TMetaData;
begin
  inherited Create(AOwner);
  FOtherMetaData := TAstaIODataSet.Create(Self);
  FTables := TAstaIODataSet.Create(Self);
  FTriggers := TAstaIODataSet.Create(Self);
  FSessionInfo := TAstaIODataSet.Create(Self);
  FDBMSName := TAstaIODataSet.Create(Self);
  FIndexes := TAstaIODataSet.Create(Self);
  FFields := TAstaIODataSet.Create(Self);
  FViews := TAstaIODataSet.Create(Self);
  FStoredProcs := TAstaIODataSet.Create(Self);
  FForeignKeys := TAstaIODataSet.Create(Self);
  FSystemTables := TAstaIODataSet.Create(Self);
  FPrimeKeys := TAstaIODataSet.Create(Self);
  FStoredProcColumns := TAstaIODataSet.Create(Self);
  FDefault := TAstaIODataSet.Create(Self);
  FDirectory := TAstaIODataSet.Create(Self);
  FSystemInfo := TAstaIODataSet.Create(Self);
  FAll := TAstaIODataSet.Create(Self);

  FMetaDataList := TList.Create;

  AMetaData := TMetaData.Create;
  AMetaData.DataSet := FDefault;
  FMetaDataList.Add(AMetaData);

  AMetaData := TMetaData.Create;
  AMetaData.MetaDataRequest := mdDBMSName;
  AMetaData.DataSet := FDBMSName;
  FMetaDataList.Add(AMetaData);

  AMetaData := TMetaData.Create;
  AMetaData.MetaDataRequest := mdOtherMetaData;
  AMetaData.DataSet := FOtherMetaData;
  FMetaDataList.Add(AMetaData);

  AMetaData := TMetaData.Create;
  AMetaData.MetaDataRequest := mdTables;
  AMetaData.DataSet := FTables;
  FMetaDataList.Add(AMetaData);

  AMetaData := TMetaData.Create;
  AMetaData.MetaDataRequest := mdIndexes;
  AMetaData.DataSet := FIndexes;
  FMetaDataList.Add(AMetaData);

  AMetaData := TMetaData.Create;
  AMetaData.MetaDataRequest := mdFields;
  AMetaData.DataSet := FFields;
  FMetaDataList.Add(AMetaData);

  AMetaData := TMetaData.Create;
  AMetaData.MetaDataRequest := mdViews;
  AMetaData.DataSet := FViews;
  FMetaDataList.Add(AMetaData);

  AMetaData := TMetaData.Create;
  AMetaData.MetaDataRequest := mdStoredProcs;
  AMetaData.DataSet := FStoredProcs;
  FMetaDataList.Add(AMetaData);

  AMetaData := TMetaData.Create;
  AMetaData.MetaDataRequest := mdForeignKeys;
  AMetaData.DataSet := FForeignKeys;
  FMetaDataList.Add(AMetaData);

  AMetaData := TMetaData.Create;
  AMetaData.MetaDataRequest := mdSystemTables;
  AMetaData.DataSet := FSystemTables;
  FMetaDataList.Add(AMetaData);

  AMetaData := TMetaData.Create;
  AMetaData.MetaDataRequest := mdPrimeKeys;
  AMetaData.DataSet := FPrimeKeys;
  FMetaDataList.Add(AMetaData);

  AMetaData := TMetaData.Create;
  AMetaData.MetaDataRequest := mdStoredProcColumns;
  AMetaData.DataSet := FStoredProcColumns;
  FMetaDataList.Add(AMetaData);

  AMetaData := TMetaData.Create;
  AMetaData.MetaDataRequest := mdTriggers;
  AMetaData.DataSet := FTriggers;
  FMetaDataList.Add(AMetaData);

  AMetaData := TMetaData.Create;
  AMetaData.MetaDataRequest := mdSessionInfo;
  AMetaData.DataSet := FSessionInfo;
  FMetaDataList.Add(AMetaData);

  AMetaData := TMetaData.Create;
  AMetaData.MetaDataRequest := mdDirectory;
  AMetaData.DataSet := FDirectory;
  FMetaDataList.Add(AMetaData);

  AMetaData := TMetaData.Create;
  AMetaData.MetaDataRequest := mdSystemInfo;
  AMetaData.DataSet := FSystemInfo;
  FMetaDataList.Add(AMetaData);

  AMetaData := TMetaData.Create;
  AMetaData.MetaDataRequest := mdAll;
  AMetaData.DataSet := FAll;
  FMetaDataList.Add(AMetaData);
end;

destructor TAstaIOMetaData.Destroy;
var
  i: Integer;
begin
  FSessionInfo.Free;
  FOtherMetaData.Free;
  FDBMSName.Free;
  FTriggers.Free;
  FTables.Free;
  FIndexes.Free;
  FFields.Free;
  FViews.Free;
  FStoredProcs.Free;
  FForeignKeys.Free;
  FSystemTables.Free;
  FPrimeKeys.Free;
  FStoredProcColumns.Free;
  FDefault.Free;
  FDirectory.Free;
  FSystemInfo.Free;
  FAll.Free;

  for i := 0 to FMetaDataList.Count - 1 do
    TMetaData(FMetaDatalist[i]).Free;

  FMetaDataList.Free;
  inherited;
end;

procedure TAstaIOMetaData.GetDirectory(const Path: string);
var
  FSearchRec: TSearchRec;
begin
  FDirectory.Empty;
  if FindFirst(Path, faAnyFile, FSearchRec) = 0 then
    repeat
      FDirectory.AppendRecord([ExtractFileName(FSearchRec.Name),
        FSearchRec.Size,
          FileDatetoDateTime(FSearchRec.Time),
          FSearchRec.Attr,
          ExtractFilePath(Path)]);
    until Findnext(FSearchRec) <> 0;
end;

procedure TAstaIOMetaData.GetSysInfo;
var OSType, OSDescription :String;
    Processors,
    TotalRAM, FreeRAM,
    TotalSwap, FreeSwap :Integer;
    Disks, Environment :TStringList;

    {$IFDEF WIN32}
    AMemInfo :TMemoryStatus;
    AVerInfo :TOSVersionInfo;  // holds version information
    ASysInfo :TSystemInfo;   // holds the system information

    PEnvironment :PChar;
    {$ENDIF}

    {$IFDEF LINUX}
    ASysInfo :TSysInfo;
    {$ENDIF}

begin
  Disks:=TStringList.Create;
  Environment:=TStringList.Create;
  try
    FSystemInfo.Empty;

    // ********************************* Windows ******************************************
    {$IFDEF WIN32}
    OSType:='WIN32';

    AMemInfo.dwLength := Sizeof (AMemInfo);
    GlobalMemoryStatus (AMemInfo);

    TotalRAM:=AMemInfo.dwTotalPhys div 1024;
    FreeRAM:=AMemInfo.dwAvailPhys div 1024;
    TotalSwap:=AMemInfo.dwTotalPageFile div 1024;
    FreeSwap:=AMemInfo.dwAvailPageFile div 1024;

    {set the size member of the TOSVersionInfo structure}
    AVerInfo.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
    {retrieve the operating system version information}
    GetVersionEx(AVerInfo);

    case AVerInfo.dwPlatformId of
      VER_PLATFORM_WIN32s        :OSDescription:='Win 32s under Windows 3.1';
      VER_PLATFORM_WIN32_WINDOWS :OSDescription:='Windows 95';
      VER_PLATFORM_WIN32_NT      :OSDescription:='Windows NT'
      else
        OSDescription:='Windows 98';
    end;

    {retrieve information about the system}
    GetSystemInfo(ASysInfo);
    Processors:=ASysInfo.dwNumberOfProcessors;

    PEnvironment:=GetEnvironmentStrings;
    if PEnvironment <> nil then
      while PEnvironment <> nil do
      begin
        Environment.Add(StrPas(PEnvironment));
        Inc(PEnvironment, StrLen(PEnvironment) + 1);
        if (Byte(PEnvironment[0]) = 0) then PEnvironment:=nil;
      end;
    {$ENDIF}

    // ********************************* Linux ******************************************
    {$IFDEF LINUX}
    Processors:=-1;

    OSType:='LINUX';

    sysinfo(ASysInfo);
    TotalRAM:=ASysInfo.totalram;
    FreeRAM:=ASysInfo.freeram;
    TotalSwap:=ASysInfo.totalswap;
    FreeSwap:=ASysInfo.freeswap;
    {$ENDIF}

    FSystemInfo.AppendRecord([OSType, OSDescription, Processors,
                              TotalRAM, FreeRAM, TotalSwap, FreeSwap,
                              Environment.Text, Disks.Text]);
  finally
    Disks.Free;
    Environment.Free;
  end;
end;

function TAstaIOMetaData.GetMetaData(Sender: TObject;
  U: TUserRecord;
  MetaDataRequest: TAstaMetaData;
  DataBaseName, TableName: string): TAstaIODataSet;
var
  Valid: Boolean;
begin
  if assigned(FBeforeMetaData) then
    FBeforeMetaData(Sender, U, MetaDataRequest, DataBaseName, TableName);
  try
    InitDataSet(MetaDataRequest);
    Valid := True;
    case MetaDataRequest of
      mdDBMSName:
        begin
          FDBMSName.Open;
          if Assigned(FOnDBMSName) then
            FOnDBMSName(Sender, U, FDBMSName)
        end;

      mdOtherMetaData:
        begin
          FOtherMetaData.Open;
          if Assigned(FOnOtherMetaData) then
            FOnOtherMetaData(Sender, U, MetaDataRequest, FOtherMetaData, DataBaseName, TableName)
        end;

      mdTriggers:
        begin
          FTriggers.Open;
          if Assigned(FOnTriggers) then
            FOnTriggers(Sender, U, MetaDataRequest, FTriggers, DataBaseName, TableName)
          else
          if Assigned(FDBInfo) and (isTriggers in FDBInfo.DBInfoSupply) then
            FDBInfo.GetDBInfo(Sender, U, MetaDataRequest, FTriggers, DataBaseName, TableName)
        end;

      mdTables:
        begin
          FTables.Open;
          if Assigned(FOnTables) then
            FOnTables(Sender, U, MetaDataRequest, FTables, DataBaseName, TableName)
          else
          if Assigned(FDBInfo) and (isTables in FDBInfo.DBInfoSupply) then
            FDBInfo.GetDBInfo(Sender, U, MetaDataRequest, FTables, DataBaseName, TableName)
        end;

      mdIndexes:
        begin
          FIndexes.Open;
          if Assigned(FOnIndexes) then
            FOnIndexes(Sender, U, MetaDataRequest, FIndexes, DataBaseName, TableName)
          else
          if Assigned(FDBInfo) and (isIndexes in FDBInfo.DBInfoSupply) then
            FDBInfo.GetDBInfo(Sender, U, MetaDataRequest, FIndexes, DataBaseName, TableName)
        end;

      mdFields:
        begin
          FFields.Open;
          if Assigned(FOnFields) then
            FOnFields(Sender, U, MetaDataRequest, FFields, DataBaseName, TableName)
          else
          if Assigned(FDBInfo) and (isFields in FDBInfo.DBInfoSupply) then
            FDBInfo.GetDBInfo(Sender, U, MetaDataRequest, FFields, DataBaseName, TableName)
        end;

      mdViews:
        begin
          FViews.Open;
          if Assigned(FOnViews) then
            FOnViews(Sender, U, MetaDataRequest, FViews, DataBaseName, TableName)
          else
          if Assigned(FDBInfo) and (isViews in FDBInfo.DBInfoSupply) then
            FDBInfo.GetDBInfo(Sender, U, MetaDataRequest, FViews, DataBaseName, TableName)
        end;

      mdStoredProcs:
        begin
          FStoredProcs.Open;
          if Assigned(FOnStoredProcs) then
            FOnStoredProcs(Sender, U, MetaDataRequest, FStoredProcs, DataBaseName, TableName)
          else
          if Assigned(FDBInfo) and (isSProcs in FDBInfo.DBInfoSupply) then
            FDBInfo.GetDBInfo(Sender, U, MetaDataRequest, FStoredProcs, DataBaseName, TableName)
        end;

      mdForeignKeys:
        begin
          FForeignKeys.Open;
          if Assigned(FOnForeignKeys) then
            FOnForeignKeys(Sender, U, MetaDataRequest, FForeignKeys, DataBaseName, TableName)
          else
          if Assigned(FDBInfo) and (isFKeys in FDBInfo.DBInfoSupply) then
            FDBInfo.GetDBInfo(Sender, U, MetaDataRequest, FForeignKeys, DataBaseName, TableName)
        end;

      mdSystemTables:
        begin
          FSystemTables.Open;
          if Assigned(FOnSystemTables) then
            FOnSystemTables(Sender, U, MetaDataRequest, FSystemTables, DataBaseName, TableName)
          else
          if Assigned(FDBInfo) and (isSysTables in FDBInfo.DBInfoSupply) then
            FDBInfo.GetDBInfo(Sender, U, MetaDataRequest, FSystemTables, DataBaseName, TableName)
        end;

      mdPrimeKeys:
        begin
          FPrimeKeys.Open;
          if Assigned(FOnPrimeKeys) then
            FOnPrimeKeys(Sender, U, MetaDataRequest, FPrimeKeys, DataBaseName, TableName)
          else
          if Assigned(FDBInfo) and (isPKeys in FDBInfo.DBInfoSupply) then
            FDBInfo.GetDBInfo(Sender, U, MetaDataRequest, FPrimeKeys, DataBaseName, TableName)
        end;

      mdStoredProcColumns:
        begin
          FStoredProcColumns.Open;
          if Assigned(FOnStoredProcColumns) then
            FOnStoredProcColumns(Sender, U, MetaDataRequest, FStoredProcColumns, DataBaseName, TableName)
          else
          if Assigned(FDBInfo) and (isSProcCols in FDBInfo.DBInfoSupply) then
            FDBInfo.GetDBInfo(Sender, U, MetaDataRequest, FStoredProcColumns, DataBaseName, TableName)
        end;

      mdDirectory:
        begin
          FDirectory.Open;
          if Assigned(FOnDirectory) then
            FOnDirectory(Sender, U, MetaDataRequest, FDirectory, DataBaseName, TableName)
          else
            GetDirectory(TableName);
        end;

      mdSystemInfo:
        begin
          FSystemInfo.Open;
          if Assigned(FOnSystemInfo) then
            FOnSystemInfo(Sender, U, MetaDataRequest, FSystemInfo)
          else
            GetSysInfo;
        end;

      mdAll:
        begin
          FAll.Open;
          if Assigned(FOnAll) then
            FOnAll(Sender, U, MetaDataRequest, FAll, DataBaseName)
          else
            GetAll(U, DatabaseName);
        end;

    else
      Valid := False;

    end;
    if Valid then
      Result := MetaData[MetaDataRequest].DataSet
    else
      Result := FDefault;

  finally
    if assigned(FAfterMetaData) then
      FAfterMetaData(Sender, U, MetaDataRequest, DataBaseName, TableName);
  end;
end;

function TAstaIOMetaData.GetMetaDataItem(MetaDataRequest: TAstaMetaData): TMetaData;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to MetaDataList.Count - 1 do
    if TMetaData(MetaDataList[i]).MetaDataRequest = MetaDataRequest then
    begin
      Result := TMetaData(MetaDataList[i]);
      exit;
    end;
end;

procedure TAstaIOMetaData.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (AComponent = FDBInfo) and (Operation = opRemove) then
  begin
    FDBInfo := nil;
  end;
end;

procedure TAstaIOMetaData.InitDataSet(MetaDataRequest: TAstaMetaData);
begin
  case MetaDataRequest of
    mdOtherMetaData:
      begin
        FOtherMetaData.NukeAllFieldInfo;
        FOtherMetaData.AddField('Memo', ftMemo, 0);
      end;

    mdTriggers:
      begin
        FTriggers.NukeAllFieldInfo;
        FTriggers.AddField('TriggerName', ftString, 30);
        FTriggers.AddField('RelationName', ftString, 30);
        FTriggers.AddField('TriggerType', ftString, 16);
        FTriggers.AddField('Description', ftString, 80);
      end;

    mdTables:
      begin
        FTables.NukeAllFieldInfo;
        FTables.AddField('TableName', ftString, 40);
        FTables.AddField('OwnerName', ftString, 40);
        FTables.AddField('Description', ftString, 40);
      end;

    mdIndexes:
      begin
        FIndexes.NukeAllFieldInfo;
        FIndexes.AddField('IndexName', ftString, 40);
      end;

    mdFields:
      begin
        FFields.NukeAllFieldInfo;
        FFields.AddField('FieldName', ftString, 40);
        FFields.AddField('FieldType', ftInteger, 0);
        FFields.AddField('FieldSize', ftInteger, 0);
        FFields.AddField('Required', ftInteger, 0);
      end;

    mdViews:
      begin
        FViews.NukeAllFieldInfo;
        FViews.AddField('ViewName', ftString, 40);
        FViews.AddField('OwnerName', ftString, 40);
        FViews.AddField('Description', ftString, 40);
      end;

    mdStoredProcs:
      begin
        FStoredProcs.NukeAllFieldInfo;
        FStoredProcs.AddField('SProcName', ftString, 40);
        FStoredProcs.AddField('OwnerName', ftString, 40);
        FStoredProcs.AddField('Description', ftString, 40);
      end;

    mdSystemTables:
      begin
        FSystemTables.NukeAllFieldInfo;
        FSystemTables.AddField('TableName', ftString, 40);
        FSystemTables.AddField('OwnerName', ftString, 40);
        FSystemTables.AddField('Description', ftString, 40);
      end;

    mdPrimeKeys:
      begin
        // There can be only one primary key, which can have multiple fields
        FPrimeKeys.NukeAllFieldInfo;
        FPrimeKeys.AddField('FieldName', ftString, 40);
      end;

    mdForeignKeys:
      begin
        // There can be multiple foreign keys, which each can have multiple fields
        FForeignKeys.NukeAllFieldInfo;
        FForeignKeys.AddField('FieldNames', ftString, 255);
        FForeignKeys.AddField('TableName', ftString, 40);
        FForeignKeys.AddField('IndexName', ftString, 40);
      end;

    mdStoredProcColumns:
      begin
        FStoredProcColumns.NukeAllFieldInfo;
        FStoredProcColumns.AddField('ColumnName', ftString, 40);
        FStoredProcColumns.AddField('ColumnType', ftInteger, 0);
        FStoredProcColumns.AddField('ColumnSize', ftInteger, 0);
        FStoredProcColumns.AddField('ParamType', ftInteger, 0);
        FStoredProcColumns.AddField('Required', ftInteger, 0);
      end;

    mdDBMSName:
      begin
        FDBMSName.NukeAllFieldInfo;
        FDBMSName.AddField('AstaServer', ftString, 25);
        FDBMSName.AddField('DataBase', ftString, 40);
        FDBMSName.AddField('Info', ftString, 40);
        FDBMSName.AddField('Server EXE Name', ftString, 40);
      end;

    mdSessionInfo:
      begin
        FSessionInfo.NukeAllFieldInfo;
        FSessionInfo.AddField('SessionName', ftString, 25);
        FSessionInfo.AddField('MaxSessions', ftInteger, 0);
        FSessionInfo.AddField('MaxAsyncSessions', ftInteger, 0);
        FSessionInfo.AddField('InitialSessions', ftInteger, 0);
        FSessionInfo.AddField('Aliases', ftmemo, 0);
      end;

    mdDirectory:
      begin
        FDirectory.NukeAllFieldInfo;
        FDirectory.AddField('Name', ftString, 50);
        FDirectory.AddField('Size', ftInteger, 0);
        FDirectory.AddField('Modified', ftDateTime, 0);
        FDirectory.AddField('Attributes', ftInteger, 0);
        FDirectory.AddField('Directory', ftString, 300);
      end;

    mdSystemInfo:
      begin
        FSystemInfo.NukeAllFieldInfo;
        FSystemInfo.AddField('OSType', ftString, 15);
        FSystemInfo.AddField('OSDescription', ftString, 30);
        FSystemInfo.AddField('Processors', ftInteger, 0);
        FSystemInfo.AddField('TotalRAM', ftInteger, 0);
        FSystemInfo.AddField('FreeRAM', ftInteger, 0);
        FSystemInfo.AddField('TotalSwap', ftInteger, 0);
        FSystemInfo.AddField('FreeSwap', ftInteger, 0);
        FSystemInfo.AddField('Environment', ftMemo, 0);
        FSystemInfo.AddField('Disks', ftMemo, 0);
      end;

    mdAll:
      begin
        FAll.NukeAllFieldInfo;
        FAll.AddField('ObjectType', ftInteger, 0);
        FAll.AddField('ObjectName', ftString, 40);
        FAll.AddField('ObjectFields', ftMemo, 0);
        FAll.AddField('ObjectPrimeKeys', ftMemo, 0);
        FAll.AddField('ObjectForeignKeys', ftMemo, 0);
        FAll.AddField('ObjectTriggers', ftMemo, 0);
        FAll.AddField('ObjectIndexes', ftMemo, 0);
      end;

  else
    begin
      FDefault.NukeAllFieldInfo;
      FDefault.AddField('Default', ftString, 40);
    end
  end;
end;

procedure TAstaIOMetaData.GetAll(U: TUserRecord; DataBaseName: string);
var ADataSet: TAstaIODataSet;
    DetailDataSet: TAstaIODataSet;
    ObjectFieldsStr: String;
    ObjectPrimeKeysStr: String;
    ObjectForeignKeysStr :String;
    ObjectTriggersStr :String;
    ObjectIndexesStr: String;
begin
  FAll.Empty;

  // Tables
  ADataSet:=GetMetaData(Self, U, mdTables, DatabaseName, '');
  ADataSet.First;
  while not ADataSet.Eof do
  begin
    ObjectFieldsStr:='';
    ObjectPrimeKeysStr:='';
    ObjectForeignKeysStr:='';
    ObjectTriggersStr:='';
    ObjectIndexesStr:='';

    DetailDataSet:=GetMetaData(Self, U, mdFields, DatabaseName, ADataSet.FieldByName('TableName').AsString);
    ObjectFieldsStr:=DetailDataSet.SaveToString;

    DetailDataSet:=GetMetaData(Self, U, mdPrimeKeys, DatabaseName, ADataSet.FieldByName('TableName').AsString);
    ObjectPrimeKeysStr:=DetailDataSet.SaveToString;

    DetailDataSet:=GetMetaData(Self, U, mdForeignKeys, DatabaseName, ADataSet.FieldByName('TableName').AsString);
    ObjectForeignKeysStr:=DetailDataSet.SaveToString;

    DetailDataSet:=GetMetaData(Self, U, mdTriggers, DatabaseName, ADataSet.FieldByName('TableName').AsString);
    ObjectTriggersStr:=DetailDataSet.SaveToString;

    DetailDataSet:=GetMetaData(Self, U, mdIndexes, DatabaseName, ADataSet.FieldByName('TableName').AsString);
    ObjectIndexesStr:=DetailDataSet.SaveToString;

    FAll.AppendRecord([Ord(mdTables),
                       ADataSet.FieldByName('TableName').AsString,
                       ObjectFieldsStr,
                       ObjectPrimeKeysStr,
                       ObjectForeignKeysStr,
                       ObjectTriggersStr,
                       ObjectIndexesStr]);
    ADataSet.Next;
  end;

  // SystemTables
  ADataSet:=GetMetaData(Self, U, mdSystemTables, DatabaseName, '');
  ADataSet.First;
  while not ADataSet.Eof do
  begin
    ObjectFieldsStr:='';
    ObjectPrimeKeysStr:='';
    ObjectForeignKeysStr:='';
    ObjectTriggersStr:='';
    ObjectIndexesStr:='';

    DetailDataSet:=GetMetaData(Self, U, mdFields, DatabaseName, ADataSet.FieldByName('TableName').AsString);
    ObjectFieldsStr:=DetailDataSet.SaveToString;

    DetailDataSet:=GetMetaData(Self, U, mdPrimeKeys, DatabaseName, ADataSet.FieldByName('TableName').AsString);
    ObjectPrimeKeysStr:=DetailDataSet.SaveToString;

    DetailDataSet:=GetMetaData(Self, U, mdForeignKeys, DatabaseName, ADataSet.FieldByName('TableName').AsString);
    ObjectForeignKeysStr:=DetailDataSet.SaveToString;

    DetailDataSet:=GetMetaData(Self, U, mdTriggers, DatabaseName, ADataSet.FieldByName('TableName').AsString);
    ObjectTriggersStr:=DetailDataSet.SaveToString;

    DetailDataSet:=GetMetaData(Self, U, mdIndexes, DatabaseName, ADataSet.FieldByName('TableName').AsString);
    ObjectIndexesStr:=DetailDataSet.SaveToString;

    FAll.AppendRecord([Ord(mdSystemTables),
                       ADataSet.FieldByName('TableName').AsString,
                       ObjectFieldsStr,
                       ObjectPrimeKeysStr,
                       ObjectForeignKeysStr,
                       ObjectTriggersStr,
                       ObjectIndexesStr]);
    ADataSet.Next;
  end;

  // Views
  ADataSet:=GetMetaData(Self, U, mdViews, DatabaseName, '');
  ADataSet.First;
  while not ADataSet.Eof do
  begin
    ObjectFieldsStr:='';
    ObjectPrimeKeysStr:='';
    ObjectForeignKeysStr:='';
    ObjectTriggersStr:='';
    ObjectIndexesStr:='';

    DetailDataSet:=GetMetaData(Self, U, mdFields, DatabaseName, ADataSet.FieldByName('ViewName').AsString);
    ObjectFieldsStr:=DetailDataSet.SaveToString;

    FAll.AppendRecord([Ord(mdViews),
                       ADataSet.FieldByName('ViewName').AsString,
                       ObjectFieldsStr,
                       ObjectPrimeKeysStr,
                       ObjectForeignKeysStr,
                       ObjectTriggersStr,
                       ObjectIndexesStr]);
    ADataSet.Next;
  end;

  // Stored Procedures
  ADataSet:=GetMetaData(Self, U, mdStoredProcs, DatabaseName, '');
  ADataSet.First;
  while not ADataSet.Eof do
  begin
    ObjectFieldsStr:='';
    ObjectPrimeKeysStr:='';
    ObjectForeignKeysStr:='';
    ObjectTriggersStr:='';
    ObjectIndexesStr:='';

    DetailDataSet:=GetMetaData(Self, U, mdStoredProcColumns, DatabaseName, ADataSet.FieldByName('SProcName').AsString);
    ObjectFieldsStr:=DetailDataSet.SaveToString;

    FAll.AppendRecord([Ord(mdStoredProcs),
                       ADataSet.FieldByName('SProcName').AsString,
                       ObjectFieldsStr,
                       ObjectPrimeKeysStr,
                       ObjectForeignKeysStr,
                       ObjectTriggersStr,
                       ObjectIndexesStr]);
    ADataSet.Next;
  end;

  // Triggers
  ADataSet:=GetMetaData(Self, U, mdTriggers, DatabaseName, '');
  ADataSet.First;
  while not ADataSet.Eof do
  begin
    ObjectFieldsStr:='';
    ObjectPrimeKeysStr:='';
    ObjectForeignKeysStr:='';
    ObjectTriggersStr:='';
    ObjectIndexesStr:='';

    FAll.AppendRecord([Ord(mdTriggers),
                       ADataSet.FieldByName('TriggerName').AsString,
                       ObjectFieldsStr,
                       ObjectPrimeKeysStr,
                       ObjectForeignKeysStr,
                       ObjectTriggersStr,
                       ObjectIndexesStr]);
    ADataSet.Next;
  end;

end;

end.

