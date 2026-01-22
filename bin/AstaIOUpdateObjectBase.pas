{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10347: AstaIOUpdateObjectBase.pas 
{
{   Rev 1.0    4/10/2003 6:32:28 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:38:24 PM  Steve    Version: 1.505
}
unit AstaIOUpdateObjectBase;
{*********************************************************}
{*   Copyright (c) 1997-2002 Asta Technology Group Inc.  *}
{*               All rights reserved.                    *}
{*               www.astatech.com                        *}
{*********************************************************}
{$I AstaIO.inc}

interface

uses SysUtils, Classes, DB,
  AstaIOCustomDataSet,
  AstaIOParamList,
  AstaIODBConst,
  AstaIOSQLUtils;

type
  TBeforeSQLItemInsertEvent = procedure(Sender: TObject; var SQL: String; var Params: TParams; DeltaType :TDeltaType;
                                        var AddSQL: String; var AddParams: TParams) of object;
  TAfterSQLItemInsertEvent = procedure(Sender: TObject; SQL: String; Params: TParams; DeltaType :TDeltaType;
                                        var AddSQL: String; var AddParams: TParams) of object;
  TBeforeSQLBatchInsertEvent = procedure(Sender: TObject; var AddSQL: String; var AddParams: TParams) of object;
  TAfterSQLBatchInsertEvent = procedure(Sender: TObject; var AddSQL: String; var AddParams: TParams) of object;
  TProcessDetailsEvent = procedure(Sender: TObject; AParentDataSet: TDataSet; ASQL: TAstaParamList)  of object;

type
  TAstaIOUpdateObjectBase = class(TComponent)
  private
    FSQLOptions:TAstaIOSQLOptions;
    FAbout: String;

    function GetPrimeFields: TStrings;
    procedure SetPrimeFields(Value: TStrings);
    function GetRefetchFields: TStrings;
    procedure SetRefetchFields(Value: TStrings);
    function GetNoSQLFields: TStrings;
    procedure SetNoSQLFields(Value: TStrings);
  protected
    FBeforeSQLItemInsert: TBeforeSQLItemInsertEvent;
    FAfterSQLItemInsert: TAfterSQLItemInsertEvent;
    FBeforeSQLBatchInsert: TBeforeSQLBatchInsertEvent;
    FAfterSQLBatchInsert: TAfterSQLBatchInsertEvent;
    FAddParams: TParams;
    FAddSQL: String;
    FUpdateTableName: string;
    FUpdateMode: TUpdateMode;

    FNoSQLFields,
    FRefetchFields,
    FPrimeFields: TStringList;
    FPrimeFieldsValues: TStringList;
    FAutoIncrementField,
    FSequence: string;
    FMultiTableDataSet:TAstaIODataSet;
    FCurrentDataSet: TAstaIODataSet;
    FOldValuesDataSet: TAstaIODataSet;
    FBaseDataSet: TDataSet;
    FBookmarkList, FSQLList,
    FDeltaTypeList, FRefetchPackageList: TAstaParamList;
    FTrimStringFields: Boolean;
    FOnProcessDetails: TProcessDetailsEvent;

    function GetSQLItem(Index: Integer): TAstaParamItem;
    procedure SetSQLItem(Index: Integer; const Value: TAstaParamItem);

    procedure InternalAddSQL(SQLString: string; Params: TAstaParamList; EditAction: TDeltaType);
    procedure CreateSQL; virtual;
    procedure AddDummyToRefetchPackage;
    function PackupRefreshList(EditAction: TDeltaType; BookMark: Integer) :String;
    Function FormatParamName(AParamName:String):String;
    Function FormatTableName(ATableName:String):String;
    Function FormatFieldName(AFieldName:String):String;
  public
    constructor Create(AOwner:TComponent); override;
    destructor Destroy; override;

    procedure AddSQL(SQLString: string; Params: TParams);
    function UpdateInfoAsTransportList(TheSQLAndParams: TAstaParamList; Database: string): TAstaParamList;

    procedure UpdateFromClientDataSet(DataSet: TAstaCustomAuditDataSet); virtual;
    function GenerateTransPortDataForServerSQL(Dataset: TAstaCustomAuditDataSet): TAstaParamList;
    function GenerateClientSideSQL(Dataset: TAstaCustomAuditDataSet; UpdateFromClient :Boolean; ClearList :Boolean): TAstaParamList;
    procedure ClearSQLList;

    property SQLList: TAstaParamList read FSQLList write FSQLList;
    property BookMarkList: TAstaParamList read FBookMarkList write FBookMarkList;
    property DeltaTypeList: TAstaParamList read FDeltaTypeList write FDeltaTypeList;
    property SQLItems[Index: Integer]: TAstaParamItem read GetSQLItem write SetSQLItem;
    property RefetchPackageList: TAstaParamList read FRefetchPackageList write FRefetchPackageList;

    property OldValuesDataSet: TAstaIODataSet read FOldValuesDataSet write FOldValuesDataSet;
    property CurrentDataSet: TAstaIODataSet read FCurrentDataSet write FCurrentDataSet;
    property BaseDataSet: TDataSet read FBaseDataSet write FBaseDataSet;
    property MultiTableDataSet:TAstaIODataSet read FMultiTableDataSet write FMultiTableDataSet;
    property PrimeFields: TStrings read GetPrimeFields write SetPrimeFields;
    property RefetchFields: TStrings read GetRefetchFields write SetRefetchFields;
    property NoSQLFields: TStrings read GetNoSQLFields write SetNoSQLFields;
    property UpdateTableName: string read FUpdateTableName write FUpdateTableName;
    property Sequence: string read FSequence write FSequence;
    property AutoIncrementField: string read FAutoIncrementField write FAutoIncrementField;

    property BeforeSQLItemInsert: TBeforeSQLItemInsertEvent read FBeforeSQLItemInsert write FBeforeSQLItemInsert;
    property AfterSQLItemInsert: TAfterSQLItemInsertEvent read FAfterSQLItemInsert write FAfterSQLItemInsert;
    property BeforeSQLBatchInsert: TBeforeSQLBatchInsertEvent read FBeforeSQLBatchInsert write FBeforeSQLBatchInsert;
    property AfterSQLBatchInsert: TAfterSQLBatchInsertEvent read FAfterSQLBatchInsert write FAfterSQLBatchInsert;
    property PrimeFieldsValues: TStringList read FPrimeFieldsValues write FPrimeFieldsValues;
    property OnProcessDetails: TProcessDetailsEvent read FOnProcessDetails write FOnProcessDetails;
  published
    property About: String read FAbout write FAbout;
    property TrimStringFields: Boolean read FTrimStringFields write FTrimStringFields;
    property SQLOptions:TAstaIOSQLOptions read FSQLOptions write FSQLOptions default [];
  end;

implementation
uses AstaIOResources,AstaIOSQLDataSet,AstaIOConst
;//,AstaIOClientRemoteDataSet;

type
TAstaCustomClientSQLDataSetHackHack = class(TAstaCustomAuditDataSet);


{ TAstaIOUpdateObjectBase }

constructor TAstaIOUpdateObjectBase.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FPrimeFieldsValues:=TStringList.Create;
  FBookMarkList := TAstaParamList.Create;
  FDeltaTypeList := TAstaParamList.Create;
  FSQLList := TAstaParamList.Create;
  FRefetchPackageList := TAstaParamList.Create;
  FAddParams := TParams.Create;

  FPrimeFields := TStringList.Create;
  FRefetchFields := TStringList.Create;
  FNoSQLFields := TStringList.Create;
  FCurrentDataSet:=Nil;
  FMultiTableDataSet:=nil;
  FOldValuesDataSet:=nil;
  FSQLOptions:=[];

end;

destructor TAstaIOUpdateObjectBase.Destroy;
begin
  FBookMarkList.Free;
  FDeltaTypeList.Free;
  FSQLList.Free;
  FRefetchPackageList.Free;
  FAddParams.Free;
  FPrimeFieldsValues.Free;
  FPrimeFields.Free;
  FRefetchFields.Free;
  FNoSQLFields.Free;
  try
  if assigned(fCurrentDataSet) then
   FreeandNil(FCurrentDataset);
   except
   end;
  inherited Destroy;
end;

procedure TAstaIOUpdateObjectBase.AddSQL(SQLString: string; Params: TParams);
var AstaParams: TAstaParamList;
begin
  if Assigned(Params) then
  begin
    AstaParams := TParamsToAstaParams(Params);
    //FSQLList.FastAdd(SQLString, AstaParams.AsTokenizedString(False)) // sm - 1/5/2004. FastAdd ignores dup names
    with FSQLList.Add do
    begin
      Name:=SQLString;
      AsString:=AstaParams.AsTokenizedString(False);
    end;
  end
  else
  begin
    //FSQLList.FastAdd(SQLString, '')  // sm - 1/5/2004. FastAdd ignores dup names
    with FSQLList.Add do
    begin
      Name:=SQLString;
      AsString:='';
    end;
  end;
end;

procedure TAstaIOUpdateObjectBase.InternalAddSQL(SQLString: string; Params: TAstaParamList; EditAction: TDeltaType);
var AParams :TParams;
    AstaParams: TAstaParamList;
     Function ParamNameForRefetch:String;
     begin
      if EditAction=dtDelete then
       result:='~'+IntToStr(FRefetchPackageList.Count+1)
       else result:=  IntToStr(CurrentDataSet.FieldByName(sfld_BookMark).AsInteger)
     end;
begin
  try
    AParams := AstaParamsToTParams(Params);
    if Assigned(FBeforeSQLItemInsert) then
    begin
      FAddParams.Clear;
      FAddSQL:='';

      FBeforeSQLItemInsert(Self, SQLString, AParams, EditAction, FAddSQL, FAddParams);
      if Length(Trim(FAddSQL)) > 0 then
      begin
        AstaParams := TParamsToAstaParams(FAddParams);
        //Params := TParamsToAstaParams(AParams);
        try
          //FSQLList.FastAdd(FAddSQL, AstaParams.AsTokenizedString(False)); // sm - 1/5/2004. FastAdd ignores dup names
          with FSQLList.Add do
          begin
            Name:=FAddSQL;
            AsString:=AstaParams.AsTokenizedString(False);
          end;
          with FRefetchPackageList.Add do   // sm - 1/5/2004 - We need to send an empty param
          begin
            Name:='0';
            AsString:='';
          end;

        finally
          AstaParams.Free;
          AddDummyToRefetchPackage;
        end;
      end;
    end;

    with FSQLList.Add do
    begin
      Name:=SQLString;
      AsString:=Params.AsTokenizedString(False);
    end;

    FDeltaTypeList.FastAdd(Ord(EditAction));
    case EditAction of
      dtEdit,
      dtappend: FBookMarkList.FastAdd(CurrentDataSet.FieldByName(sfld_BookMark).AsInteger);
      dtdelete: FBookMarkList.FastAdd(FOldValuesDataSet.FieldByName(sfld_BookMark).AsInteger);
    end;

    // sm - 10/1/2002. When using TP Sleuth QA, the "with ...Add causes AV is CreateSQL is changed/fixed
    FRefetchPackageList.FastAdd(ParamNameForRefetch,
                           PackupRefreshList(EditAction, CurrentDataSet.FieldByName(sfld_BookMark).AsInteger));

    // sm - 10/1/2002 See above
    {with FRefetchPackageList.Add do
    begin
      Name:=IntToStr(CurrentDataSet.FieldByName(sfld_BookMark).AsInteger);
      AsString:=PackupRefreshList(EditAction, CurrentDataSet.FieldByName(sfld_BookMark).AsInteger);
    end;}

    if Assigned(FAfterSQLItemInsert) then
    begin
      FAddParams.Clear;
      FAddSQL:='';
      FAfterSQLItemInsert(Self, SQLString, AParams, EditAction, FAddSQL, FAddParams);

      if Length(Trim(FAddSQL)) > 0 then
      begin
        AstaParams := TParamsToAstaParams(FAddParams);
        try
          //FSQLList.FastAdd(FAddSQL, AstaParams.AsTokenizedString(False));  // sm - 1/5/2004. FastAdd ignores dup names
          with FSQLList.Add do
          begin
            Name:=FAddSQL;
            AsString:=AstaParams.AsTokenizedString(False);
          end;
          with FRefetchPackageList.Add do   // sm - 1/5/2004 - We need to send an empty param
          begin
            Name:='0';
            AsString:='';
          end;
        finally
          AstaParams.Free;
          AddDummyToRefetchPackage;
        end;
      end;
    end;

  finally
    if Assigned(AParams) then
      AParams.Free;
  end;
end;

procedure TAstaIOUpdateObjectBase.ClearSQLList;
var i: Integer;
begin
  for i := SQLList.Count - 1 downto 0 do
    FSQLList.Delete(i);
  FSQLList.Clear;
  FBookMarkList.Clear;
  FDeltaTypeList.Clear;
  FRefetchPackageList.Clear;
end;

procedure TAstaIOUpdateObjectBase.CreateSQL;
begin
  //
end;

procedure TAstaIOUpdateObjectBase.AddDummyToRefetchPackage;
begin
  FDeltaTypeList.FastAdd(-1);
  FRefetchPackageList.FastAdd('0', '');
end;

function TAstaIOUpdateObjectBase.GenerateClientSideSQL(Dataset: TAstaCustomAuditDataSet;
  UpdateFromClient :Boolean; ClearList :Boolean): TAstaParamList;
var AstaParams: TAstaParamList;
begin
  result := nil;
  if UpdateFromClient then
    UpdateFromClientDataSet(Dataset);
  if ClearList then
    ClearSQLList;
  if not Assigned(FOldValuesDataSet) then exit;
  if not Assigned(FCurrentDataSet) then exit;

  if Assigned(FBeforeSQLBatchInsert) then
  begin
    FAddParams.Clear;
    FAddSQL:='';
    FBeforeSQLBatchInsert(Self, FAddSQL, FAddParams);
    if Length(Trim(FAddSQL)) > 0 then
    begin
      AstaParams := TParamsToAstaParams(FAddParams);
      try
        //FSQLList.FastAdd(FAddSQL, AstaParams.AsTokenizedString(False)); // sm - 1/5/2004. FastAdd ignores dup names
        with FSQLList.Add do
        begin
          Name:=FAddSQL;
          AsString:=AstaParams.AsTokenizedString(False);
        end;
        with FRefetchPackageList.Add do // sm - 1/5/2004 - We need to send an empty param
        begin
          Name:='0';
          AsString:='';
        end;
      finally
        AstaParams.Free;
        AddDummyToRefetchPackage;
      end;
    end;
  end;

  CreateSQL;

  if Assigned(FAfterSQLBatchInsert) then
  begin
    FAddParams.Clear;
    FAddSQL:='';
    FAfterSQLBatchInsert(Self, FAddSQL, FAddParams);
    if Length(Trim(FAddSQL)) > 0 then
    begin
      AstaParams := TParamsToAstaParams(FAddParams);
      try
        //FSQLList.FastAdd(FAddSQL, AstaParams.AsTokenizedString(False)); // sm - 1/5/2004. FastAdd ignores dup names
        with FSQLList.Add do
        begin
          Name:=FAddSQL;
          AsString:=AstaParams.AsTokenizedString(False);
        end;
        with FRefetchPackageList.Add do // sm - 1/5/2004 - We need to send an empty param
        begin 
          Name:='0';
          AsString:='';
        end;
      finally
        AstaParams.Free;
        AddDummyToRefetchPackage;
      end;
    end;
  end;

  if SQLList.Count = 0 then exit;
  result := SQLList;
end;

Function TAstaIOUpdateObjectBase.FormatTableName(ATableName:String):String;
begin
 result:=ATableName;
 if (soQuotesinTablenames in FSQLOptions) then
  result:='"'+result+'"';

end;

Function TAstaIOUpdateObjectBase.FormatParamName(AParamName:String):String;
begin
 result:='w_'+AParamName;
 result:=StringReplace(result,chr(32),'_',[rfReplaceAll, rfIgnoreCase]);
end;

Function TAstaIOUpdateObjectBase.FormatFieldName(AFieldName:String):String;
begin
 result:=AFieldName;
 if soQuotesinFieldNames in FSQLOptions then
  result:='"'+result+'"'
end;

function TAstaIOUpdateObjectBase.GenerateTransPortDataForServerSQL(Dataset: TAstaCustomAuditDataSet): TAstaParamList;
begin
  OldValuesDataSet := Dataset.OldValuesDataSet;
  CurrentDataSet := TAstaCustomClientSQLDataSetHackHack(Dataset).DeltaDataSetCurrentValueDataSet;
  FMultiTableDataSet := TAstaCustomClientSQLDataSetHackHack(Dataset).MultiTableDataSet;
  result := TAstaParamList.Create;
  result.Fastadd('OldValuesDataSet', DataSetToString(OldValuesDataSet));
  result.Fastadd('CurrentValuesDataSet', DataSetToString(CurrentDataSet));
  //we need to add the multi dataset if we want to support this
end;

function TAstaIOUpdateObjectBase.GetSQLItem(Index: Integer): TAstaParamItem;
begin
  Result := FSQLList[Index];
end;

procedure TAstaIOUpdateObjectBase.SetSQLItem(Index: Integer; const Value: TAstaParamItem);
begin
  FSQLList[Index] := Value;
end;

procedure TAstaIOUpdateObjectBase.UpdateFromClientDataSet(DataSet: TAstaCustomAuditDataSet);
begin
  FOldValuesDataSet := DataSet.OldValuesDataSet;
  FPrimeFields.Assign(TAstaCustomClientSQLDataSetHackHack(DataSet).PrimeFields);
  FRefetchFields.Assign(TAstaCustomClientSQLDataSetHackHack(DataSet).RefetchFields);
  FUpdateTableName := TAstaCustomClientSQLDataSetHackHack(DataSet).UpdateTableName;
  FAutoIncrementField := TAstaCustomClientSQLDataSetHackHack(DataSet).AutoIncrementField;
  FNoSQLFields.Assign(TAstaCustomClientSQLDataSetHackHack(DataSet).NoSQLFields);
  FSequence := TAstaCustomClientSQLDataSetHackHack(DataSet).Sequence;

  if Assigned(FCurrentDataSet) then // sm - 7/5/2003
    FreeAndNil(FCurrentDataSet);
  if TAstaCustomClientSQLDataSetHackHack(DataSet).UpdateMethod = umAfterPost then
  begin
    FCurrentDataSet:=TAstaCustomClientSQLDataSetHackHack(DataSet).CurrentValueDataSetRecord(FCurrentDataSet);
    FCurrentDataSet.Edit;
    FCurrentDataSet.FieldByname(sfld_BookMark).AsInteger:=OldValuesDataSet.FieldByname(sfld_BookMark).AsInteger;
    FCurrentDataSet.Post;
  end
  else
    FCurrentDataSet := TAstaCustomClientSQLDataSetHackHack(DataSet).DeltaDataSetCurrentValueDataSet;
end;

function TAstaIOUpdateObjectBase.GetPrimeFields: TStrings;
begin
  result := FPrimeFields;
end;

function TAstaIOUpdateObjectBase.GetRefetchFields: TStrings;
begin
  result := FRefetchFields;
end;

procedure TAstaIOUpdateObjectBase.SetPrimeFields(Value: TStrings);
begin
  FPrimeFields.Assign(Value);
end;

procedure TAstaIOUpdateObjectBase.SetRefetchFields(Value: TStrings);
begin
  FRefetchFields.Assign(Value);
end;

function TAstaIOUpdateObjectBase.GetNoSQLFields: TStrings;
begin
  result := FNOSQLFields;
end;

procedure TAstaIOUpdateObjectBase.SetNoSQLFields(Value: TStrings);
begin
  FNoSQLFields.Assign(Value);
end;

function TAstaIOUpdateObjectBase.PackupRefreshList(EditAction: TDeltaType; BookMark: Integer) :String;
var i :Integer;
    RefetchFieldsParams: TAstaParamList;
    PrimeFieldsParams: TAstaParamList;
    ExtraItemsList: TAstaParamList;
begin
  RefetchFieldsParams:=TAstaParamList.Create;
  PrimeFieldsParams:=TAstaParamList.Create;
  ExtraItemsList:=TAstaParamList.Create;

  try
    if EditAction in [dtEdit, dtAppend] then
    begin
      for i:=0 to FRefetchFields.Count - 1 do
        with RefetchFieldsParams.Add do
        begin
          Name:=FRefetchFields[i];
          DataType:=FOldValuesDataSet.FieldByName(FRefetchFields[i]).DataType;
          Clear;
        end;

      for i:=0 to FPrimeFields.Count - 1 do
        with PrimeFieldsParams.Add do
        begin
          Name:=FPrimeFields[i];
          DataType:=FOldValuesDataSet.FieldByName(FPrimeFields[i]).DataType;
          Value:=FCurrentDataSet.FieldByName(FPrimeFields[i]).Value;
        end;
    end;
    with ExtraItemsList.Add do
      AsString:=RefetchFieldsParams.AsTokenizedString(False);
    with ExtraItemsList.Add do
      AsString:=PrimeFieldsParams.AsTokenizedString(False);;
    with ExtraItemsList.Add do
      AsString:=FUpdateTableName;
    with ExtraItemsList.Add do
      AsString:=FAutoIncrementField;
    with ExtraItemsList.Add do
      AsInteger:=BookMark;
    with ExtraItemsList.Add do
      AsInteger:=Ord(EditAction);

    Result:=ExtraItemsList.AsTokenizedString(False)
  finally
    RefetchFieldsParams.Free;
    PrimeFieldsParams.Free;
    ExtraItemsList.Free;
  end;
end;

function TAstaIOUpdateObjectBase.UpdateInfoAsTransportList(TheSQLAndParams: TAstaParamList; Database: string): TAstaParamList;
begin
  result := TAstaParamList.Create;
  Result.FastAdd(FCurrentDataSet.Name, Integer(CurrentDataset));
  result.FastAdd('Database', Database);
  result.FastAdd('SQL', TheSQLAndParams.AsTokenizedString(False));
  result.fastadd('RefetchFields', FRefetchFields.Text);
  //Params contains the SQL in the Name property and the Params as a TAstaParamList AsString
  result.FastAdd('BookMarks', FBookMarkList.AstokenizedString(False));
  result.FastAdd('DeltaTypes', FDeltaTypeList.AstokenizedString(False));
  result.FastAdd('RefetchPackage', FRefetchPackageList.AstokenizedString(False));
  //FBookMarkList contains the SQL in the name and the BookMark for each row generated
  result.FastAdd('UpdateTableName', FUpdateTableName);
  result.FastAdd('AutoIncrementField', FAutoIncrementField);
  result.FastAdd('Sequence', FSequence);
  result.FastAdd('UpdateMode', FUpdateMode);
  result.FastAdd('NoSQLFields', FNoSQLFields.Text);
end;

end.
