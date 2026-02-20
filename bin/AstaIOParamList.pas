{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10261: AstaIOParamList.pas 
{
{   Rev 1.0    4/10/2003 6:31:46 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:37:54 PM  Steve    Version: 1.505
}
unit AstaIOParamList;
{*********************************************************}
{*   Copyright (c) 1997-2002 Asta Technology Group Inc.  *}
{*               All rights reserved.                    *}
{*               www.astatech.com                        *}
{*********************************************************}

{$I AstaIO.inc}

interface

uses Classes, DB, SysUtils,AstaIOUtil,
  Variants,
  FmtBcd,
  SqlTimSt,
  Windows, ActiveX, ComObj,
  {$IFNDEF SQLDataSetOnly}
  AstaIOPdaBase,
  AstaIOJavaUtils,
  {$ENDIF}
  AstaIOCustomDataSet,
  AstaIOSQLParams,
  AstaIOReaderWriter;

type

  TAstaParamItem = class;
  TAstaParamList = class;
  TAstaParamListCreateToken = (tcToken, tcServer, tcJava, tcPalm, tcWinCE, tcLinuxPDA, tcPdaJavaServer);

  //  TParamType = (ptUnknown, ptInput, ptOutput, ptInputOutput, ptResult);
  //  TAstaParamTypes = set of TParamType;
  EAstaParamException = class(Exception); //sg 06/16/98
  TSQLCustomParamsEvent = procedure(Sender: TObject; Item: TAstaParamItem; var TheResult: string; var Handled: Boolean) of object;

  TAstaParamItem = class(TCollectionItem)
  private
    FName: string;
    FData: AnsiString;
    FParamType: TParamType;
    FDataType: TFieldType;
    FNull: Boolean;
    FSize: Integer;
    {$IFDEF AstaPluginIntf}
    FRefCount: integer;
    {$ENDIF}
    FVariant : variant;
    procedure SetName(AName: string);
    procedure SetParamType(AParamType: TParamType);
    procedure SetDataType(ADataType: TFieldType);
    procedure SetNull(ANull: Boolean);
    procedure SetSize(ASize: Integer);
    function GetAsDataSet: TDataSet;
    procedure SetAsDataSet(Value: TDataSet);
    function GetAsString: AnsiString;
    function GetAsLargeInt: LargeInt;
    procedure SetAsLargeInt(Value: LargeInt);
    function GetAsInteger: Integer; stdcall;
    function GetAsSmallint: Smallint; stdcall;
    function GetAsWord: Word; stdcall;
    function GetAsVariant: Variant;
    //These are not Streamable!!! I needed it for Server Side Method threading sg 2.22.99
    procedure SetObject(Source: TObject);
    function GetAsObject: TObject;
    function GetAsBoolean: Boolean; stdcall;
    function GetAsFloat: Double; stdcall;
    function GetAsDateTime: TDateTime; stdcall;
    //// but they could be <G>
    function GetAsGUID: TGUID;
    function GetAsFmtBcd: TBcd;
    function GetAsTimeStamp: TSQLTimeStamp;
    function GetAsAnsiString: AnsiString;
    procedure SetAsAnsiString(Value: AnsiString);
    procedure SetAsString(Value: AnsiString);
    procedure SetAsMemo(Value: AnsiString);
    procedure SetAsBlob(Value: AnsiString);
    procedure SetAsStream(Value: TStream);
    function GetAsDispatch: Pointer;
    procedure SetAsDispatch(Value: Pointer);
    function GetAsStream: TStream;
    function GetAsParamList: TAstaParamList;
    procedure SetAsParamList(Value: TAstaParamList);
    procedure SetAsBoolean(Value: Boolean); stdcall;
    procedure SetAsFloat(Value: Double); stdcall;
    procedure SetAsSmallint(Value: Smallint); stdcall;
    procedure SetAsWord(Value: Word); stdcall;
    procedure SetAsInteger(Value: Integer); stdcall;
    procedure SetAsDate(Value: TDateTime); stdcall;
    procedure SetAsTime(Value: TDateTime); stdcall;
    procedure SetAsDateTime(Value: TDateTime); stdcall;
    procedure SetAsVariant(Value: Variant);
    procedure SetAsText(Value: AnsiString);
    function GetBcd: Currency;
    procedure SetBcd(const Value: Currency);
    function GetCurrency: Currency;
    procedure SetCurrency(const Value: Currency);
{$IFDEF Windows}
    procedure SetAsGuid(const Value: TGUID);
{$ENDIF}
{$ifdef Delphi6AndUp}
    procedure SetAsFmtBcd(const Value: TBcd);
    procedure SetAsTimeStamp(const Value: TSQLTimeStamp);
{$endif}
    function DateBookEnds(UseAccess, DatesInQuotes: Boolean): string;
    function SQLFormat(UseAccess, DatesInquotes, ForceUSDates,
      DoubleQuotesInStrings: Boolean; const DateMask, DateTimeMask: string): string;
    function SQLFormatNull(UseAccess, DatesInQuotes, ForceUSDates,
      DoubleQuotesInStrings: Boolean; const DateMask, DateTimeMask: string): string;
    function StrToVariant(ST : string):Variant;
    {$ifndef SQLDATASETONLY}
    function JavaTransportFormat: String;
    {$ENDIF}
    procedure ConversionError;
  protected
    function GetDisplayName: string; override;
    procedure SetDisplayName(const Value: string); override;
  public
    constructor Create(Collection: Tcollection); override;
    Destructor Destroy;override;
    function IsOutput: Boolean;
    function IsInput: Boolean;
    function GetDataSize: Integer;
    procedure SetBlobData(Buffer: Pointer; Size: Integer);
    procedure LoadFromFile(FileName: string);
    procedure LoadFromStream(Stream: TStream);
    procedure AssignField(Field: TField);
    procedure WriteProperties(Writer: TAstaIOWriter);
    procedure ReadProperties(Reader: TAstaIOReader);
    procedure AssignParam(Dest: TAstaParamItem);
    procedure AssignToStdParam(Dest: TParam);
    procedure AssignStdParam(Src: TParam);
    procedure Clear;
    procedure Assign(Source: TPersistent); override;
    procedure AssignTo(Dest: TPersistent); override;
    property AsParamList: TAstaParamList read GetAsParamList write SetAsParamList;
    property AsDataSet: TDataSet read GetAsDataSet write SetAsDataSet;
    property AsAnsiString: AnsiString read GetAsAnsiString write SetAsAnsiString;
    property AsString: AnsiString read GetAsString write SetAsString;
    property AsMemo: AnsiString read GetAsString write SetAsMemo;
    property AsDispatch: pointer read GetAsDispatch write SetAsDispatch;
    property AsStream: TStream read GetAsStream write SetAsStream;
    property AsBlob: AnsiString read GetAsString write SetAsBlob;
    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
    property AsFloat: Double read GetAsFloat write SetAsFloat;
    property AsSmallint: Smallint read GetAsSmallint write SetAsSmallInt;
    property AsWord: Word read GetAsWord write SetAsWord;
    property AsInteger: Integer read GetAsInteger write SetAsInteger;
    property AsDate: TDateTime read GetAsDateTime write SetAsDate;
    property AsTime: TDateTime read GetAsDateTime write SetAsTime;
    property AsDateTime: TDateTime read GetAsDateTime write SetAsDateTime;
    property AsCurrency: Currency read GetCurrency write SetCurrency;
    property AsBcd: Currency read GetBcd write SetBcd;
    property Text: AnsiString read GetAsString write SetAsText;
    property AsObject: TObject read GetAsObject write SetObject;
{$IFDEF Windows}
    property AsGUID: TGUID read GetAsGUID write SetAsGUID;
{$ENDIF}
    property AsLargeInt: LargeInt read GetAsLargeInt write SetAsLargeInt;
{$ifdef Delphi6AndUp}
    property AsFmtBcd: TBcd read GetAsFmtBcd write SetAsFmtBcd;
    property AsTimeStamp: TSQLTimeStamp read GetAsTimeStamp write SetAsTimeStamp;
{$endif}
  published
    property Name: string read FName write SetName;
    property ParamType: TParamType read FParamType write SetParamType;
    property DataType: TFieldType read FDataType write SetDataType;
    property IsNull: Boolean read FNull write SetNull;
    property Size: Integer read FSize write SetSize;
    property Value: Variant read GetAsVariant write SetAsVariant;
  end;

  TAstaParamList = class(TCollection)
  private
    FUseAccessSyntax: Boolean;
    FUSDates: Boolean;
    FQuotesInDates: Boolean;
    FUsePrepare: Boolean;
    FUseDoubleQuotesInStrings: Boolean;
    FDateMask, FDateTimeMask: string;
    // added by EM, 16 Jan 2001
    function ParamTokenExtract(S: AnsiString; Num: Integer): string;
    procedure AddFromTransPortStringP(pCh, pEndCh: PAnsiChar);
    procedure AddFromTransPortString(const S: AnsiString);
  protected
    procedure SetParamItem(Index: Integer; Value: TAstaParamItem); virtual;
    function GetParamItem(Index: Integer): TAstaParamItem; virtual;
    procedure WriteItems(Writer: TAstaIOWriter);
    procedure ReadItems(Reader: TAstaIOReader);
  public
    Procedure AddSQLParam(P:TAstaIOSQLParamQuery);
    function CopyList: TAstaParamList;
    function CreateParam(FldType: TFieldType; const ParamName: string; AParamType: TParamType): TAstaParamItem;
    Function AsDisplayText:String;
    procedure ConstantAdd(const Values: array of const);
    procedure AssignValues(Values: TAstaParamList);
    Procedure UpdateParamValues(Origin:TParams;TargetTypes:TParamTypes = []; AddMissingParams:Boolean = True);
    Function DeleteParam(ParamName:String):Boolean;
    procedure AssignParamValues(Dest: TParams;TargetTypes:TParamTypes = []);
    procedure CopyParams(Dest: TParams; Clearit: Boolean); overload;
    procedure CopyParams(Dest: TAstaParamList; ClearIt: Boolean); overload;
    procedure LoadFromFile(FileName: string);
    procedure SaveToFile(FileName: string);
    procedure SaveToStream(Stream: TStream);
   procedure LoadFromStream(Stream: TStream);
    function ValuesToSql(S: string; OnCustomParamEvent: TSQLCustomParamsEvent): string;
    procedure PrepareFromDesignList(L: TStrings);
    function MergeValues(L: TStrings; AccessSyntax, UseQuotesInDates, ForceUSDates, DoubleQuotesInStrings: Boolean; OnCustomParamEvent: TSQLCustomParamsEvent;
      DateMask, DateTimeMask: string): string;
    function NullValuesToSql(S: string): string;
    function NullValueToListForDesignTime(L: TStrings; DateMask, DateTimeMask: string): TStringList;
    function AsTokenizedString(InputsOnly: Boolean = False): AnsiString;
    class function CreateFromTokenizedString(S: AnsiString): TAstaParamList;
    class function CreateFromServerString(S: AnsiString): TAstaParamList;
    constructor Create; virtual;
    function Add(AName: string = ''): TAstaParamItem;
    Function GetAsStringAndFree(ParamName:String):string;
    procedure FastAdd(Value: Variant); overload;
    procedure FastAdd(AName: string; Value: Variant); overload;
    procedure FastAdd(AName: string; DataSet:TDataSet); overload;
    procedure FastAdd(const AName: string; ADataType: TFieldtype;
      AValue: Variant); overload;
    function FindParam(ParamName: string): TAstaParamItem;
    function ParamByName(ParamName: string): TAstaParamItem;
    procedure SQLPrepare(SQL: TStrings);
    {$ifndef SQLDATASETONLY}
    procedure AddFromPDATransPortString(const S: AnsiString);
    function AsPDATransportString: AnsiString;
    class function CreateFromPDATokenizedString(const S: String): TAstaParamList;
    procedure AddFromJavaTransPortString(const S: AnsiString);
    function AsJavaTransportString: String;
    class function CreateFromJavaTokenizedString(const S: String): TAstaParamList;
    {$endif}
    property Items[Index: Integer]: TAstaParamItem read GetParamItem write SetParamItem; default;
    property AccessSQLSyntax: Boolean read FUseAccessSyntax write FUseAccessSyntax default false;
    property QuotesInDates: Boolean read FQuotesInDates write FQuotesInDates default True;
    property UseDoubleQuotesInStrings: Boolean read FUseDoubleQuotesInStrings write FUseDoubleQuotesInStrings default false;
  end;

function ParamListHasData(P: TAstaParamList): Boolean;
procedure AddParamsToListFromString(S: string; L: TStrings);
function ParamListToStringFastAdd(const ParamNames:Array of String;Data: array of Variant): AnsiString;overload;
function ParamListToStringFastAdd(const Data: array of Variant): AnsiString;overload;
function ParamsFromList(L: TStrings): TStringList;
function AstaParamsToTParams(Params: TAstaParamList): TParams;
function TParamsToAstaParams(Params: TParams;ParamTypes:TParamTypes): TAstaParamList;overload;

function TParamsToAstaParams(Params: TParams): TAstaParamList;overload;
function TParamsToAstaParamsString(Params: TParams): AnsiString;
function ParamsAsDataSet(P: TAstaParamList): TAstaIODataSet;overload;
function ParamsAsDataSet(TheParams:AnsiString): TAstaIODataSet;overload;
Procedure StringToAstaParamListToTParams(Const Data:AnsiString;Params:TParams);
procedure ParamItemAssignField(Dest:TParam;Source: TField);

const
  TokenForParams = chr(6); //'~';

implementation

uses
  AstaIOSQLUtils,
  AstaIOResources;

// -----------------------------------------------------------------------
// TAstaParamItem
Destructor TAstaParamItem.Destroy;
begin
  FData:='';
  inherited Destroy;
end;

constructor TAstaParamItem.Create(Collection: Tcollection);
begin
  inherited create(collection);
  FName := 'Param' + IntToStr(TAstaParamList(Collection).NextID);
  FData := '';
  FVariant := Null;
  FParamType := ptUnknown;
  FDataType := ftunknown;
  FNull := True;
  FSize := 0;
end;

{$IFNDEF SQLDATASETONLY}
function TAstaParamItem.JavaTransportFormat: string;
begin
  result := FData;
  case FDataType of
    ftdate    : result := formatDateTime('YYYY-MM-DD', AsDateTime);
    ftdatetime: result := FormatDateTime('YYYY-MM-DD hh:mm:ss', AsDateTime);
    fttime    : result := FormatDateTime('hh:mm:ss', AsTime);
    ftfloat,
    ftCurrency: result:=FormatFloat('###############.###############',AsFloat);
  end;
end;
{$ENDIF}

function TAstaParamItem.IsInput: Boolean;
begin
  result := (FParamType in [ptinput, ptinputoutput]);
end;

function TAstaParamItem.IsOutput: Boolean;
begin
  result := (FParamType in [ptinputoutput, ptoutput, ptresult]);
end;

procedure TAstaParamItem.SetName(AName: string);
begin
  FName := AName;
end;

function ParamListHasData(P: TAstaParamList): Boolean;
begin
  result := (p <> nil) and (p.count > 0) and (p[0].AsString <> '');
end;

function TAstaParamItem.GetDisplayName: string;
begin
  Result := Name;
end;

procedure TAstaParamItem.SetDisplayName(const Value: string);
var
  I: Integer;
  Param: TAstaParamItem;
begin
  if CompareText(Value, Name) <> 0 then
  begin
    if Collection <> nil then
      for I := 0 to Collection.Count - 1 do
      begin
        Param := TAstaParamItem(Collection.items[i]);
        if (Param <> Self) and
        {$ifndef AstaIOHarData}
        (Param is TAstaParamItem)
         {$else}
         CheckClass(Param, TAstaParamItem)
        {$endif}
         and (CompareText(Value, Param.Name) = 0) then
          raise Exception.Create(SDuplicateParamName);
      end;
    Name := Value;
    Changed(False);
  end;
end;

procedure TAstaParamItem.SetParamType(AParamType: TParamType);
begin
  FParamType := AParamType;
end;

procedure TAstaParamItem.SetDataType(ADataType: TFieldType);
begin
  FDataType := ADataType;
end;

procedure TAstaParamItem.SetNull(ANull: Boolean);
begin
  FNull := ANull;
  if FNull then
    Clear;
end;

procedure TAstaParamItem.SetSize(ASize: Integer);
begin
  if (ASize >= 0) and (ASize <> FSize) and
    (DataType in [ftString, ftBytes, ftVarBytes, ftBlob, ftMemo, ftgraphic,
{$ifdef mswindows}
                  ftguid,
{$endif}
                  ftFixedChar, ftWideString, ftOraCLOB, ftOraBLOB]) then
    FSize := ASize;
end;

function TAstaParamItem.GetDataSize: Integer;
begin
  result := Length(FData);
end;

procedure TAstaParamItem.Clear;
begin
  FNull := True;
  FData := '';
  FVariant := Null;
end;

procedure TAstaParamItem.AssignParam(Dest: TAstaParamItem);
begin
  Dest.FName := FName;
  Dest.FData := FData;
  Dest.FParamType := FParamType;
  Dest.FDataType := FDataType;
  Dest.FNull := FNull;
  Dest.FSize := FSize;
  Dest.FVariant := FVariant;
end;

procedure TAstaParamItem.AssignToStdParam(Dest: TParam);
begin
  Dest.Name := Name;
  Dest.DataType := DataType;
  Dest.ParamType := ParamType;
  if IsNull then
    Dest.Clear
  else
    Dest.Value := Value;
{$ifdef Delphi6AndUp}
  Dest.Size := Size;
{$endif}
end;

procedure TAstaParamItem.AssignStdParam(Src: TParam);
begin
  FName := Src.Name;
  FParamType := Src.ParamType;
  if Src.IsNull then
    Clear
  else
    Value := Src.Value;
  FDataType := Src.DataType;
{$ifdef Delphi6AndUp}
  FSize := Src.Size;
{$endif}
end;

procedure TAstaParamItem.AssignField(Field: TField);
 var Bytes: TArray<Byte>;
begin
  if Field <> nil then
  begin
    Name := Field.FieldName;
    DataType := Field.DataType;
    Size := Field.DataSize;
    if Field.IsNull then
      Clear
    else
      case Field.DataType of
        ftString,
// Unicode support - dj 2003/07/10
{$ifndef WideStrChange}
          ftWideString,
{$endif}
          ftFixedChar:
          begin
            AsString := Field.AsString;
            DataType := Field.DataType;
          end;
{$ifdef WideStrChange}
        ftWideString:
          begin
            Value := Field.Value;
            DataType := Field.DataType;
          end;
{$endif}

        ftMemo,
          ftOraCLOB:
          begin
            AsMemo := Field.AsString;
            DataType := Field.DataType;
          end;
        ftBlob,
          ftOraBLOB:
          begin
            // Fixed for D12 Binary compatibility using AsBytes
            if Field.IsNull then
              AsBlob := ''
            else
            begin
              Bytes := Field.AsBytes;
              SetString(FData, PAnsiChar(Bytes), Length(Bytes));
              AsBlob := FData;
            end;
            DataType := Field.DataType;
          end;
        ftBytes,
          ftVarBytes:
          begin
            Value := Field.AsVariant;
            DataType := Field.DataType;
          end;
        {$ifdef mswindows}
        ftGUID:
          AsGUID := (Field as TGuidField).AsGuid;
        {$endif}
        ftBoolean:
          AsBoolean := Field.AsBoolean;
        ftFloat:
          AsFloat := Field.AsFloat;
        ftCurrency:
          AsCurrency := Field.AsCurrency;
        ftBCD:
          AsBCD := Field.AsCurrency;
        ftSmallInt:
          AsSmallint := Field.AsInteger;
        ftWord:
          AsWord := Field.AsInteger;
        ftInteger:
          AsInteger := Field.AsInteger;
        ftDate:
          AsDate := Field.AsDateTime;
        ftTime:
          AsTime := Field.AsDateTime;
        ftDateTime:
          AsDateTime := Field.AsDateTime;
        ftLargeint:
          AsLargeInt := (Field as TLargeintField).AsLargeInt;
{$ifdef Delphi6AndUp}
        ftFmtBCD:
          AsFmtBcd := (Field as TFMTBCDField).AsBCD;
        ftTimeStamp:
          AsTimeStamp := (Field as TSQLTimeStampField).AsSQLTimeStamp;
{$ENDIF}
      else
        Value := Field.Value;
      end;
  end;
end;

procedure TAstaParamItem.AssignTo(Dest: TPersistent);
begin
  // Hardata, Lucio 01-08-2002
  // In order to use classes on other instance, we can't use the is/as operator
  //{$ifndef AstaIOHarData}
  //if Dest is TField then
  if CheckClass(Dest, TField) then
    TField(Dest).Value := Value
  //else if Dest is TAstaParamItem then
  else if CheckClass(Dest, TAstaParamItem) then
    AssignParam(TAstaParamItem(Dest))
  //else if Dest is TParam then
  else if CheckClass(Dest, TParam) then
    AssignToStdParam(TParam(Dest));
end;

procedure TAstaParamItem.Assign(Source: TPersistent);
begin
  // Hardata, Lucio 01-08-2002
  // In order to use classes on other instance, we can't use the is/as operator
  //if Source is TastaParamItem then
  if CheckClass(Source, TastaParamItem) then
    TastaParamItem(Source).AssignParam(Self)
  //else if Source is TField then
  else if CheckClass(Source, TField) then
    AssignField(TField(Source))
  //else if Source is TParam then
  else if CheckClass(Source, TParam) then
    AssignStdParam(TParam(Source))
  else
    inherited Assign(Source);
end;

procedure TAstaParamItem.ConversionError;
begin
  raise Exception.Create('Error converting parameter value');
end;

function TAstaParamItem.GetAsAnsiString: AnsiString;
begin
  if IsNull then begin
    result := '';
{$IFDEF Windows}
    if FDataType = ftGuid then
      Result := AnsiString(GUIDToString(GUID_NULL));
{$ENDIF}
  end
  else if FDataType in [ftString, ftMemo, ftBlob, ftFixedChar, ftWideString,ftDataSet,
{$IFDEF Windows}
                        ftGUID,
{$ENDIF}
                        ftBytes, ftVarBytes, ftOraCLOB, ftOraBLOB] then
    Result := FData
  else
    Result := AnsiString(VarToStr(Value));
end;

procedure TAstaParamItem.SetAsAnsiString(Value: AnsiString);
begin
  FData := Value;
  DataType := ftString;
  FNull := False;
  if Value = '' then
    Clear;
end;

function TAstaParamItem.GetAsString: AnsiString;
begin
  if IsNull then begin
    result := '';
{$IFDEF Windows}
    if FDataType = ftGuid then
      Result := GUIDToString(GUID_NULL);
{$ENDIF}
  end
  else if FDataType in [ftString, ftMemo, ftBlob, ftFixedChar, ftWideString,ftDataSet,
{$IFDEF Windows}
                        ftGUID,
{$ENDIF}
                        ftBytes, ftVarBytes, ftOraCLOB, ftOraBLOB] then
    Result := FData
  else
    Result := VarToStr(Value);// Stephan 6/6/2002
end;

procedure TAstaParamItem.SetAsString(Value: AnsiString);
begin
  FData := Value;
  DataType := ftString;
  FNull := False;
  if Value = EmptyStr then
    Clear;
end;

{$IFDEF Windows}
procedure TAstaParamItem.SetAsGuid(const Value: TGUID);
begin
  FData := GuidToString(Value);
  FNULL := Length(FData) <> 38;
  DataType := ftGUID;
end;

function TAstaParamItem.GetAsGuid: TGUID;
begin
  if FData <> '' then
    Result := StringToGuid(FData)
  else
    Result := GUID_NULL;
end;
{$ENDIF}

procedure TAstaParamItem.SetAsMemo(Value: AnsiString);
begin
  FData := AnsiString(Value);
  DataType := FtMemo;
  FNull := False;
  if Value = EmptyStr then
    Clear;
end;

procedure TAstaParamItem.SetAsBlob(Value: AnsiString);
begin
  FData := AnsiString(Value);
  DataType := ftblob;
  FNull := False;
  if Value = EmptyStr then
    Clear;
end;

function TAstaParamItem.GetAsBoolean: Boolean;
begin
  if IsNull then
    Result := False
  else if DataType = ftBoolean then
    Result := CompareText(FData, 'True') = 0
  else
    Result := Value;
end;

procedure TAstaParamItem.SetAsBoolean(Value: Boolean);
begin
  if Value then
    FData := 'True'
  else
    FData := 'False';
  DataType := ftBoolean;
  FNull := False;
end;

function TAstaParamItem.GetAsFloat: Double;
begin
  if IsNull then
    result := 0.0
  else if DataType = ftFloat then
    result := AstaStringFloat(FData)
  else
    result := Value;
end;

procedure TAstaParamItem.SetAsFloat(Value: Double);
begin
  FData := AstaFloatString(Value);
  DataType := ftFloat;
  FNull := False;
end;

function TAstaParamItem.GetBcd: Currency;
begin
  if IsNull then
    result := 0.0
  else if DataType = ftBcd then
    result := AstaStringCurrency(FData)
  else
    result := Value;
end;

procedure TAstaParamItem.SetBcd(const Value: Currency);
begin
  FData := AstaCurrencyString(Value);
  DataType := ftBCD;
  FNull := False;
end;

function TAstaParamItem.GetCurrency: Currency;
begin
  if IsNull then
    result := 0.0
  else if DataType = ftCurrency then
    result := AstaStringCurrency(FData)
  else
    result := Value;
end;

procedure TAstaParamItem.SetCurrency(const Value: Currency);
begin
  FData := AstaCurrencyString(Value);
  DataType := ftCurrency;
  FNull := False;
end;

function TAstaParamItem.GetAsSmallint: Smallint;
begin
  if IsNull then
    result := 0
  else if DataType = ftSmallInt then
    result := StringToInteger(FData)
  else
    result := Value;
end;

procedure TAstaParamItem.SetAsSmallInt(Value: Smallint);
begin
  FDataType := ftSmallint;
  FData := InTToStr(Value);
  FNull := False;
end;

function TAstaParamItem.GetAsWord: Word;
begin
  if IsNull then
    Result := 0
  else if DataType = ftWord then
    result := StringToInteger(FData)
  else
    result := Value;
end;

procedure TAstaParamItem.SetAsWord(Value: Word);
begin
  DataType := ftWord;
  FData := IntToStr(Value);
  FNull := False;
end;

function TAstaParamItem.GetAsInteger: Integer;
begin
  if IsNull then
    Result := 0
  else if DataType in [ftInteger, ftAutoInc, ftLargeInt] then
    Result := StringToInteger(FData)
  else
    Result := Value;
end;

procedure TAstaParamItem.SetAsInteger(Value: Integer);
begin
  DataType := ftInteger;
  FData := IntToStr(Value);
  FNull := False;
end;

function TAstaParamItem.GetAsLargeInt: LargeInt;
var
  E: integer;
begin
  if IsNull then
    result := 0
  else if DataType = ftLargeInt then begin
    Val(FData, result, E);
    if E <> 0 then
      Result := 0;
  end
  else
{$ifdef Delphi6AndUp}
    result := Value;
{$else}
    result:= AsInteger;
{$endif}
end;

procedure TAstaParamItem.SetAsLargeInt(Value: LargeInt);
begin
  DataType := ftLargeInt;
  Str(Value, FData);
  FNull := False;
end;

function TAstaParamItem.GetAsDateTime: TDateTime;
begin
  if IsNull then
    result := 0.0
  else if DataType in [ftTime, ftDate, ftDateTime] then
    result := AstaStringFloat(FData)
  else
    result := Value;
end;

procedure TAstaParamItem.SetAsDate(Value: TDateTime);
begin
  DataType := ftDate;
  FData := AstaFloatString(Value);
  FNull := False;
end;

procedure TAstaParamItem.SetAsTime(Value: TDateTime);
begin
  DataType := ftTime;
  SetAsDateTime(Value);
  DataType := ftTime;
  FNull := False;
end;

procedure TAstaParamItem.SetAsDateTime(Value: TDateTime);
begin
  DataType := ftDateTime;
  FData := AstaFloatString(Value);
  FNull := False;
end;

{$ifdef Delphi6AndUp}
function TAstaParamItem.GetAsFmtBcd: TBcd;
begin
  if IsNull then
    result := NullBcd
  else if DataType = ftFmtBcd then
    result := AstaStringFmtBCD(FData)
  else
    result := VarToBcd(Value);
end;

procedure TAstaParamItem.SetAsFmtBcd(const Value: TBcd);
begin
  FData := AstaFmtBCDString(Value);
  DataType := ftFmtBcd;
  FNull := False;
end;

function TAstaParamItem.GetAsTimeStamp: TSQLTimeStamp;
begin
  if IsNull then
    result := NullSQLTimeStamp
  else if DataType = ftTimeStamp then
    result := AstaStringTimeStamp(FData)
  else
    result := VarToSQLTimeStamp(Value);
end;

procedure TAstaParamItem.SetAsTimeStamp(const Value: TSQLTimeStamp);
begin
  FData := AstaTimeStampString(Value);
  DataType := ftTimeStamp;
  FNull := False;
end;
{$endif}

function TAstaParamItem.GetAsStream: TStream;
begin
  result := TMemoryStream.Create;
  if not IsNull then
    if true or (DataType in [ftBlob, ftMemo, ftOraCLOB, ftOraBLOB]) then
      StringToStream(FData, TMemoryStream(result))
    else
      ConversionError;
end;

procedure TAstaParamItem.SetAsStream(Value: TStream);
begin
  DataType := ftBlob;
  FData := StreamToString(TMemoryStream(Value));
  FNull := False;
end;

procedure TAstaParamItem.LoadFromStream(Stream: TStream);
begin
  AsStream := Stream;
end;

procedure TAstaParamItem.LoadFromFile(FileName: string);
var
  F: TFileStream;
begin
  f := TFileStream.Create(FileName, fmopenread);
  try
    f.position := 0;
    AsStream := f;
  finally
    f.Free;
  end;
end;

procedure TAstaParamItem.SetBlobData(Buffer: Pointer; Size: Integer);
begin
  FDataType := ftBlob;
  FData := '';
  if Size > 0 then begin
    SetLength(FData, Size);
    Move(pchar(buffer)^, FData[1], Size);
    FNull := False;
  end
  else
    FNull := True;
end;

procedure TAstaParamItem.SetObject(Source: TObject);
var
  P: Pointer;
begin
  FDataType := ftBlob;
  p := Source;
  SetLength(FData, sizeof(TObject));
  move(Integer(p), FData[1], Sizeof(Pointer));
  FNull := False;
end;

function TAstaParamItem.GetAsObject: Tobject;
var
  P: Pointer;
begin
  move(fdata[1], p, sizeof(pointer));
  result := TObject(p);
end;

function TAstaParamItem.GetAsDataSet: TDataSet;
begin
  result := nil;
  if FData <> '' then result := StringToDataSet(FData);
end;

function TAstaParamItem.GetAsDispatch: Pointer;
begin
  Result := Pointer(Self.AsInteger);
end;

procedure TAstaParamItem.SetAsDispatch(Value: Pointer);
begin
  Self.AsInteger := Integer(Value);
{$IFDEF Delphi5AndUp}
  FDataType := ftIDispatch;
{$ENDIF}
end;

procedure TAstaParamItem.SetAsDataSet(Value: TDataSet);
begin
  FDataType := ftDataSet;
  FData := CloneDataSetToString(Value);
  FNull := False;
end;

function TAstaParamItem.GetAsParamList: TAstaParamList;
begin
  result := TAstaParamList.CreateFromTokenizedString(FData);
end;

procedure TAstaParamItem.SetAsParamList(Value: TAstaParamList);
begin
  FData := Value.AsTokenizedString(False);
  FNull := False;
end;

function TAstaParamItem.GetAsVariant: Variant;
var
  S: string;
  P: Pointer;
begin
  if FNull then
    Result := Null
  else
    case DataType of
      ftString,
{$ifdef mswindows}
        ftGUID,
{$endif}
// Unicode support - dj 2003/07/10
{$ifndef WideStrChange}
        ftWideString,
{$endif}
        ftFixedChar:  Result := AsString;
{$ifdef WideStrChange}
      ftWideString: Result := AstaStringWideStr(AsString);
{$endif}
      ftBytes,
        ftVarBytes:
        begin
          S := AsString;
          Result := VarArrayCreate([0, Length(S) - 1], varByte);
          P := VarArrayLock(Result);
          try
            Move(Pointer(S)^, P^, Length(S));
          finally
            VarArrayUnlock(Result);
          end;
        end;
      ftMemo,
        ftOraClob:     Result := AsMemo;
      ftBlob,
        ftOraBlob:     Result := AsBlob;
      ftFloat:         Result := AsFloat;
      ftBoolean:       Result := AsBoolean;
      ftSmallint:      Result := AsSmallint;
      ftWord:          Result := AsWord;
      ftInteger,
        ftAutoInc:     Result := AsInteger;
      ftDate:          Result := AsDate;
      ftTime:          Result := AsTime;
      ftDateTime:      Result := AsDateTime;
      ftcurrency:      Result := AsCurrency;
      ftBcd:           Result := AsBCD;
{$ifdef Delphi6AndUp}
      ftFmtBcd:        Result := VarFMTBcdCreate(AsFmtBcd);
      ftTimeStamp:     Result := VarSQLTimeStampCreate(AsTimeStamp);
      ftLargeInt:      Result := AsLargeInt;
{$else}
      ftLargeInt:      Result := AsInteger;
{$endif}
      {Variant parameter support - Danilo}
      ftVariant:       Result := FVariant;
    else
      Result := Null;
    end;
end;

procedure TAstaParamItem.SetAsVariant(Value: Variant);
var
  vt: Word;
  N: Integer;
  S: string;
  P: Pointer;
begin
  vt := VarType(Value);
  case vt of
{$IFDEF Delphi4data}
    varDispatch:
      AsDispatch := Pointer(IDispatch(Value));
{$ENDIF}
// Unicode support - dj 2003/07/10
{$ifndef WideStrChange}
    varOleStr,
{$endif}
    varString:    AsString := Value;
{$ifdef WideStrChange}
    varOleStr:    AsString := AstaWideStrString(Value, False);
{$endif}
    varBoolean:     AsBoolean := Value;
    varSingle,
      varDouble:    AsFloat := Value;
    varCurrency:    AsCurrency := Value;
    varByte,
      varSmallInt
{$ifdef Delphi6AndUp}
    , varShortInt
{$endif}:
      if DataType = ftInteger then
        AsInteger := Value
      else
        AsSmallInt := Value;
    varInteger:     AsInteger := Value;
    varDate:        AsDateTime := Value;
{$ifdef Delphi6AndUp}
    varWord:        AsWord := Value;
    varLongWord,
      varInt64:     AsLargeInt := Value;
{$endif}
    varNull:        Clear;
    varVariant:
      begin
        FNull := VarIsNull(Value);
        FVariant := Value;
      end;
  else
{$ifdef Delphi6AndUp}
    if vt = VarFMTBcd then
      AsFmtBcd := VarToBcd(Value)
    else if vt = VarSQLTimeStamp then
      AsTimeStamp := VarToSQLTimeStamp(Value)
    else
{$endif}
    if VarIsArray(Value) then begin
      N := VarArrayHighBound(Value, 1) + 1;
      SetLength(S, N);
      P := VarArrayLock(Value);
      try
        Move(P^, Pointer(S)^, N);
      finally
        VarArrayUnlock(Value);
      end;
      FData := S;
      if not (DataType in [ftBytes, ftVarBytes]) then
        FDataType := ftBytes;
      FNull := False;
    end
    else begin
      DataType := ftUnknown;
      Clear;
    end;
  end;
end;

procedure TAstaParamItem.SetAsText(Value: AnsiString);
begin
  try
    if DataType = ftUnknown then
      DataType := ftString;
    Clear;
    if Value <> '' then
      case DataType of
        ftString,
          ftBytes,
          ftVarBytes,
          ftfixedchar,
          ftwidestring
{$ifdef mswindows}
        , ftguid
{$endif}:                AsString := Value;
        ftMemo:          AsMemo := AnsiString(Value);
        ftBlob:          AsBlob := AnsiString(Value);
        ftOraCLOB:
          begin
            AsMemo := Value;
            DataType := ftOraCLOB;
          end;
        ftOraBLOB:
          begin
            AsBlob := Value;
            DataType := ftOraBLOB;
          end;
        ftBoolean:       AsBoolean := UpperCase(Trim(Value)) = 'TRUE';
        ftFloat:         AsFloat := StrToFloat(Value);
        ftCurrency:      AsCurrency := StrToCurr(Value);
        ftBcd:           AsBCD := StrToCurr(Value);
        ftSmallint:      AsSmallint := StrToInt(Value);
        ftWord:          AsWord := StrToInt(Value);
        ftlargeint:      AsLargeInt := StrToInt64(Value);
        ftInteger,
          ftautoInc:     AsInteger := StrToInt(Value);
{$ifdef Delphi6AndUp}
        ftFmtBcd:        AsFmtBcd := StrToBcd(Value);
        ftTimeStamp:     AsTimeStamp := StrToSQLTimeStamp(Value);
{$endif}
        ftDate:          AsDate := StrToDate(Value);
        ftTime:          AsTime := StrToTime(Value);
        ftDateTime:      AsDateTime := StrToDateTime(Value);
      end;
  except
    raise Exception.Create('Unable to call SetAsText for ' + Fname);
  end;
end;

function TAstaParamItem.StrToVariant(ST: string): Variant;
var
  VType : TVarType;
begin
  VType:=ord(ST[1])*256+ord(ST[2]);
  delete(ST,1,2);
  case VType of
    varSmallint, varInteger, {$ifdef Delphi6AndUp}varShortInt,varWord,{$endif} varByte  :
      begin
        try
          Result:=StrToInt(ST);
        except
          on Exception do Result:=Null;
        end;
      end;
    varSingle, varDouble, varCurrency  :
      begin
        try
          Result:=StrToFloat(ST);
        except
          on Exception do Result:=Null;
        end;
      end;
    varDate  :
      begin
        try
          Result:=StrToDateTime(ST);
        except
          on Exception do Result:=Null;
        end;
      end;
    varString, varOleStr :
      Result:=ST;
    else Result := Null;
  end;
end;

function TAstaParamItem.DateBookEnds(UseAccess, DatesInQuotes: Boolean): string;
begin
  result := '';
  if UseAccess then
    result := '#'
  else
    if DatesInQuotes then
      result := SingleQuote;
end;

function TAstaParamItem.SQLFormat(UseAccess, DatesInquotes, ForceUSDates,
  DoubleQuotesInStrings: Boolean; const DateMask, DateTimeMask: string): string;
begin
  Result := AsString;
  case FDataType of
    ftstring
{$IFDEF Windows}
    , ftguid
{$ENDIF}:
      if DoubleQuotesInStrings then
        Result := '"' + SQLsingleQuotecheck(Result) + '"'
      else
        Result := SingleQuote + SQLsingleQuotecheck(Result) + SingleQuote;
    fttime,
      ftdate,
      ftdatetime
{$IFDEF Delphi6AndUp}
    , ftTimeStamp
{$ENDIF}:
      begin
        if (FDataType = ftDate) and (DateMask <> '') then
          Result := FormatDateTime(DateMask, AsDate)
        else if (FDatatype = ftDateTime) and (DateTimeMask <> '') then
          Result := FormatDateTime(DateTimeMask, AsTime)
{$IFDEF Delphi6AndUp}
        else if (FDatatype = ftTimeStamp) and (DateTimeMask <> '') then
          Result := SQLTimeStampToStr(DateTimeMask, AsTimeStamp)
{$ENDIF}
        else if ForceUSDates then begin {to force sql select params to use us dates if need be}
          if fDataType = ftDate then
            Result := FormatDateTime('MM/DD/YYYY', AsDate)
          else if FDataType = ftDateTime then
            Result := FormatDateTime('MM/DD/YYYY hh:mm:ss', AsDateTime)
{$IFDEF Delphi6AndUp}
          else if FDatatype = ftTimeStamp then
            Result := SQLTimeStampToStr('MM/DD/YYYY hh:mm:ss', AsTimeStamp)
{$ENDIF}
          ;
        end;
        Result := DateBookEnds(UseAccess, DatesInQuotes) + Result +
          DatebookEnds(UseAccess, DatesInQuotes)
      end;
  end;
end;

function TAstaParamItem.SQLFormatNull(UseAccess, DatesInQuotes, ForceUSDates,
  DoubleQuotesInStrings: Boolean; const DateMask, DateTimeMask: string): string;
begin
  case FDataType of
{$IFDEF Windows}
    ftGuid:     AsGuid := GUID_NULL;
{$ENDIF}
    ftString:   Asstring := '';
    ftDate:     AsDate := Sysutils.date;
    ftDateTime: AsDateTime := Now;
    ftLargeInt: AsLargeInt := 0;
  else
    AsInteger := 0;
  end;
  Result := SQLFormat(UseAccess, DatesInQuotes, ForceUSDates,
    DoubleQuotesInStrings, DateMask, DateTimeMask);
end;

procedure TAstaParamItem.ReadProperties(Reader: TAstaIOReader);
begin
  FName := Reader.ReadString;
  Fdata := Reader.ReadString;
  FParamType := TParamType(Reader.ReadInteger);
  FDataType := TFieldType(Reader.ReadInteger);
  FNull := Reader.ReadBoolean;
  FSize := Reader.ReadInteger;
end;

procedure TAstaParamItem.WriteProperties(Writer: TAstaIOWriter);
begin
  Writer.WriteString(FName);
  Writer.WriteString(FData);
  Writer.WriteInteger(Ord(FParamType));
  Writer.WriteInteger(Ord(FDataType));
  Writer.WriteBoolean(FNull);
  Writer.WriteInteger(FSize);
end;


{$IFDEF AstaPluginIntf}

function TAstaParamItem.QueryInterface(const IID: TGUID; out Obj): HRESULT;
const
  E_NOINTERFACE = HResult($80004002);
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

function TAstaParamItem._AddRef: integer;
begin
  inc(FRefCount);
  result := FRefCount;
end;

function TAstaParamItem._Release: integer;
begin
  Dec(FRefCount);
  result := FRefCount;
end;

procedure TAstaParamItem.SetIsNullX(Value: boolean); stdcall;
begin
  SetNull(Value);
end;

function TAstaParamItem.GetIsNullX: boolean; stdcall;
begin
  result := fNull;
end;

function TAstaParamItem.GetAsDate: TDateTime; stdcall;
begin
  result := GetAsDateTime;
end;

function TAstaParamItem.GetAsTime: TDateTime; stdcall;
begin
  result := GetAsDateTime;
end;

function TAstaParamItem.GetAsOLEVariant: OLEVariant; stdcall;
begin
  result := GetAsVariant;
end;

function TAstaParamItem.GetAsStringX: PWideChar; stdcall;
begin
  result := StringToOleStr(AsString);
end;

function TAstaParamItem.GetAsMemoX: PWideChar; stdcall;
begin
  result := StringToOleStr(AsMemo);
end;

function TAstaParamItem.GetDataTypeX: Integer; stdcall;
begin
  result := Ord(DataType);
end;

function TAstaParamItem.GetNameX: PWideChar; stdcall;
begin
  result := StringToOleStr(Name);
end;

function TAstaParamItem.GetParamTypeX: Integer; stdcall;
begin
  result := Ord(ParamType);
end;

procedure TAstaParamItem.SetAsOLEVariant(Value: OLEVariant); stdcall;
begin
  SetAsVariant(Value);
end;

procedure TAstaParamItem.SetAsMemoX(Value: PWideChar); stdcall;
begin
  AsMemo := OleStrToString(Value);
end;

procedure TAstaParamItem.SetAsStringX(Value: PWideChar); stdcall;
begin
  AsString := OleStrToString(Value);
end;

procedure TAstaParamItem.SetDataTypeX(Value: Integer); stdcall;
begin
  DataType := TFieldType(value);
end;

procedure TAstaParamItem.SetNameX(Value: PWideChar); stdcall;
begin
  Name := OleStrToString(Value);
end;

procedure TAstaParamItem.SetParamTypeX(Value: Integer); stdcall;
begin
  ParamType := TParamType(Value);
end;

procedure TAstaParamItem.SetText(AText: PWideChar); stdcall;
begin
  Text := OleStrToString(AText);
end;

function TAstaParamItem.GetBlob(Buffer: Pointer; var BufferLength: Integer): HRESULT; stdcall;
var
  S: string;
begin
  S := GetAsString;
  if BufferLength < Length(S) then
  begin
    result := E_INVALIDARG;
  end
  else
  begin
    MoveMemory(Buffer, PChar(S), BufferLength);
    result := S_OK;
  end;
  BufferLength := Length(S);
end;

function TAstaParamItem.SetBlob(Buffer: Pointer; BufferLength: Integer): HRESULT; stdcall;
var
  S: string;
begin
  if BufferLength > 0 then
  begin
    SetLength(S, BufferLength);
    MoveMemory(PChar(S), Buffer, BufferLength);
    AsBlob := S;
    result := S_OK;
  end
  else
    result := E_INVALIDARG;
end;

{$ENDIF}

// -----------------------------------------------------------------------
// TAstaParamList

constructor TAstaParamList.Create;
begin
  inherited Create(TAstaParamItem);
  FUsePrepare := False;
  FUseAccessSyntax := False;
  FQuotesInDates := true;
  FUSDates := False;
  FUseDoubleQuotesInStrings := False;
  FDateMask := '';
  FDateTimeMask := '';
end;

procedure TAstaParamList.AddSQLParam(P:TAstaIOSQLParamQuery);
var
  t: TAstaParamItem;
begin
  //sql maybe the same so we can't use Fastadd
  t := Add;
  T.Name := P.SQL.Text;
  T.AsString:=TParamsToAstaParamsString(P);
  T.ParamType := ptinput;
end;

function TAstaParamList.CopyList: TAstaParamList;
begin
  result := TAstaParamList.Create;
  CopyParams(Result, False);
end;

procedure TAstaParamList.CopyParams(Dest: TAstaParamList; Clearit: Boolean);
var
  i: Integer;
begin
  Dest.FUseAccessSyntax := FUseAccessSyntax;
  Dest.FQuotesInDates := FquotesInDates;
  if Clearit then
    Dest.Clear;
  for i := 0 to Count - 1 do
    dest.Add.Assign(items[i]);
end;

Procedure TAstaParamList.UpdateParamValues(Origin: TParams; TargetTypes: TParamTypes = []; AddMissingParams: Boolean = True);
var
  i: Integer;
  AParam: TAstaParamItem;
begin
  for i := 0 to Origin.Count - 1 do
    if (TargetTypes = []) or (Origin[i].ParamType in TargetTypes) then begin
      AParam := FindParam(Origin[i].Name);
      if (AParam = nil) and AddMissingParams then
        AParam := Add;
      if AParam <> nil then
        AParam.Assign(Origin[i]);
  end;
end;

Function TAstaParamList.DeleteParam(ParamName: String):Boolean;
begin
  result := FindParam(ParamName) <> nil;
  if result then
    ParamByName(ParamName).Free;
end;

procedure TAstaParamList.AssignParamValues(Dest: TParams; TargetTypes: TParamTypes = []);
var
  i: Integer;
  AParam: TParam;
begin
  for i := 0 to Count - 1 do
    if (TargetTypes = []) or (items[i].ParamType in TargetTypes) then begin
      AParam := Dest.FindParam(Items[i].Name);
      if AParam <> nil then
        AParam.Assign(Items[i]);
    end;
end;

procedure TAstaParamList.CopyParams(Dest: TParams; Clearit: Boolean);
var
  i: Integer;
begin
  if Clearit then
    Dest.Clear;
  for i := 0 to Count - 1 do
    Dest.Add.Assign(items[i]);
end;

procedure TAstaParamList.AssignValues(Values: TAstaParamList);
var
  i: Integer;
  t: TAstaParamItem;
begin
  for i := 0 to values.Count - 1 do begin
    t := FindParam(Values[i].Name);
    if t <> nil then
      t.Assign(Values[i]);
  end;
end;

procedure TAstaParamList.SQLPrepare(SQL: TStrings);
var
  l: TStringList;
  i: Integer;
  t: TAstaParamItem;
begin
  FUsePrepare := True;
  Clear;
  l := ParamsFromList(SQL);
  for i := 0 to L.count - 1 do begin
    t := Add;
    with t do begin
      FName := l[i];
      FData := '';
      FParamType := ptInput;
      FDataType := ftString;
      FNull := False;
      FSize := 50;
    end;
  end;
end;

procedure TAstaParamList.PrepareFromDesignList(L: TStrings);
var
  i: Integer;
  t: TAstaParamItem;
begin
  try
    FUsePrepare := True;
    if (l = nil) then raise EDataBaseError.Create(SNilListToPrepare);
    Clear;
    for i := 0 to L.count - 1 do
    begin
      if true then
      begin
        t := Add;
        with t do begin
          FName := ParamTokenExtract(l[i], 0);
          FParamType := TParamType(StringToInteger(ParamTokenExtract(l[i], 1)));
          FDataType := Ftstring;
          if (StringToInteger(ParamTokenExtract(l[i], 2)) > ord(ftunknown)) and
            (StringToInteger(ParamTokenExtract(l[i], 2)) <= ord(high(TFieldType))) then
            // StringToInteger(ParamTokenExtract(l[i], 2)) <= ord(ftmemo))
            FDataType := TFieldType(StringToInteger(ParamTokenExtract(l[i], 2)));
          FNull := False;
          if FDatatype = ftstring then
            FSize := 50;
          SetAsText(''); //This doesn't set the DataType but DOES allocate storage for the float/date fields!!
        end;
      end;
    end;
  except
    raise EDataBaseError.Create(Format(SProblemCreatedParams, [l.Text]));
  end;
end;

function TAstaParamList.AsTokenizedString(InputsOnly: Boolean): AnsiString;
const
  sFmt: AnsiString = '%s' +
    TokenForParams + '%d' +
    TokenForParams + '%d' +
    TokenForParams + '%d' +
    TokenForParams + '%d' +
    TokenForParams + '%s';
var
  i: Integer;
  S, S1, NewData: AnsiString;
  iLen: Integer;
  iGrow: Integer;
  w: word;
begin
  iLen := 0;
  iGrow := 256;
  SetLength(Result, iGrow);
  for i := 0 to count - 1 do
    with items[i] do
      if (Not InputsOnly) or (InputsOnly and IsInput) then begin
        {Variant parameter support - Danilo}
        if DataType=ftVariant then
        begin
          W:=VarType(FVariant);
          NewData:=chr(W div 256)+chr(W mod 256)+FData;
        end
        else
          NewData:=FData;
        
        S := AnsiString(FName) + AnsiChar(TokenForParams) +
             AnsiString(IntToStr(ord(FDataType))) + AnsiChar(TokenForParams) +
             AnsiString(IntToStr(ord(FParamType))) + AnsiChar(TokenForParams) +
             AnsiString(IntToStr(ord(FNull))) + AnsiChar(TokenForParams) +
             AnsiString(IntToStr(Length(NewData))) + AnsiChar(TokenForParams) +
             NewData;

        S1 := IntegerToByteString(Length(S));
        while Length(S) + Length(S1) + iLen > Length(Result) do begin
          if iGrow < 32768 then
            iGrow := 2 * iGrow;
          SetLength(Result, Length(Result) + iGrow);
        end;
        Move(S1[1], Result[iLen + 1], Length(S1));
        Move(S[1], Result[iLen + 1 + Length(S1)], Length(S));
        Inc(iLen, Length(S1) + Length(S));
      end;
  SetLength(Result, iLen);
end;

function TAstaParamList.ParamTokenExtract(S: AnsiString; Num: Integer): string;
begin
  result := TokenCount(S, Num, TokenForParams);
//result := CommaCount(S, Num);
end;

procedure TAstaParamList.AddFromTransPortStringP(pCh, pEndCh: PAnsiChar);
var
  DataSize: Integer;
  pStCh: PAnsiChar;
  i: Integer;
  s: AnsiString;
begin
  with Add do begin
    if (pCh = nil) then
      Exit;
    for i := 1 to 5 do begin
      pStCh := pCh;
      while (pCh <> pEndCh) and (pCh^ <> AnsiChar(TokenForParams)) do
        Inc(pCh);
      SetString(s, pStCh, pCh - pStCh);
      case i of
      1: FName := string(s);
      2: FDataType := TFieldType(StringToInteger(string(s)));
      3: FParamType := TParamType(StringToInteger(string(s)));
      4: FNull := Boolean(StringToInteger(string(s)));
      5:
        begin
          DataSize := StringToInteger(string(s));
          Inc(pCh);
          SetString(FData, pCh, DataSize);
          {Variant parameter support - Danilo}
          if FDataType = ftVariant then begin
            FVariant := StrToVariant(FData);
            FData := copy(FData, 3, Length(FData) - 2);
          end;
        end;
      end;
      if pCh <> pEndCh then
        Inc(pCh);
    end;
  end;
end;

procedure TAstaParamList.AddFromTransPortString(const S: AnsiString);
var
  pCh: PAnsiChar;
begin
  pCh := PAnsiChar(S);
  AddFromTransPortStringP(pCh, pCh + Length(S));
end;


class function TAstaParamList.CreateFromServerString(S: AnsiString): TAstaParamList;
var
  i: Integer;
  l: TStringList;
begin
  Result := TAstaParamList.Create;
  ;
  l := TStringList.Create;
  l.Text := string(S);
  for i := 0 to l.count - 1 do
    Result.AddFromTransportString(AnsiString(l[i]));
  l.free;
end;

class function TAstaParamList.CreateFromTokenizedString(S: AnsiString): TAstaParamList;
var
  L: Integer;
  pCh, pEndCh: PAnsiChar;
begin
  Result := TAstaParamList.Create;
  if s = '' then
    Exit;
  pCh := PAnsiChar(S);
  pEndCh := pCh + Length(S);
  repeat
    L := PInteger(pCh)^;
    Inc(pCh, SizeOf(L));
    Result.AddFromTransportStringP(pCh, pCh + L);
    Inc(pCh, L);
  until pCh >= pEndCh;
end;

function ByteStringSpotToInteger(S: string; Spot: Integer): Integer;
begin
  result := 0;
  move(s[Spot], result, sizeof(result));
end;

function TAstaParamList.ParamByName(ParamName: string): TAstaParamItem;
begin
  result := FindParam(ParamName);
  if result = nil then raise EAstaParamException.Create(Format(SParameterNotFound, [ParamName]));
end;

function TAstaParamList.FindParam(ParamName: string): TAstaParamItem;
var
  i: Integer;
begin
  result := nil;
  for i := 0 to Count - 1 do
    if comparetext(ParamName, items[i].Name) = 0 then
    begin
      result := items[i];
      Exit;
    end;
end;

procedure TAstaParamList.SetParamItem(Index: Integer; Value: TAstaParamItem);
begin
  inherited Items[Index].Assign(Value);
end;

function TAstaParamList.Add(AName:String ='') : TAstaParamItem;
begin
  Result := TAstaParamItem(inherited Add);
  if AName<>'' then items[count-1].Name:=AName;
  result.ParamType := ptinput;

end;

function TAstaParamList.GetParamItem(Index: Integer): TAstaParamItem;
begin
  if (index < 0) or (Index > count - 1) then raise EAstaParamException.Create('Index ' + IntToSTr(Index) + ' Out of bounds');
  Result := TAstaParamItem(inherited Items[Index]);
end;

function TAstaParamList.CreateParam(FldType: TFieldType; const ParamName: string; AParamType: TParamType): TAstaParamItem;
begin
  result := Add;
  result.Name := ParamName;
  result.DataType := FldType;
  result.ParamType := AParamType;
end;

procedure TAstaParamList.ReadItems(Reader: TAstaIOReader);
begin
  Reader.ReadListBegin;
  Clear;
  while not Reader.EndofList do
    with add do
      readproperties(Reader);
  Reader.ReadListend;
end;

procedure TAstaParamList.LoadFromFile(FileName: string);
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(fs);
  finally
    fs.Free;
  end;
end;

procedure TAstaParamList.SaveToFile(FileName: string);
var
  fs: TFileStream;
begin
  fs := TFileStream.Create(FileName, fmCreate);
  try
    SaveToStream(fs);
  finally
    fs.Free;
  end;
end;

procedure TAstaParamList.SaveToStream(Stream: TStream);
var
  Writer: TAstaIOWriter;
  i: LongInt;
begin
  Writer := TAstaIOWriter.Create(Stream, 1024);
  try
    with Writer do
    begin
      WriteString(Self.ClassName);
      WriteListBegin;
          //for descendents to write other varied data structures
      for i := 0 to Count - 1 do
      begin
        items[i].WriteProperties(Writer);
      end;
      WriteListEnd;
    end;
  finally
    Writer.Free;
  end;
end;

function TAstaParamList.ValuesToSql(S: string; OnCustomParamEvent: TSQLCustomParamsEvent): string;
var
  i: Integer;
  Handled: Boolean;
  temp: string;
  AnsiResult: AnsiString;
begin
  AnsiResult := AnsiString(S);
  for i := 0 to count - 1 do
    if Assigned(OnCustomParamEvent) then
    begin
      Handled := False;
      temp := '';
      OnCustomParamEvent(self, items[i], Temp, Handled);
      if Handled then
        ReplaceS(AnsiResult, AnsiString(':' + items[i].Name), AnsiString(' ' + Temp))
      else
        ReplaceS(AnsiResult, AnsiString(':' + items[i].Name), AnsiString(' ' + items[i].SQLFormat(FUseAccessSyntax, FQuotesInDates, FUSDates, FUseDoublequotesInStrings, FDateMask, FDateTimeMask) + ' '));
    end
    else
      ReplaceS(AnsiResult, AnsiString(':' + items[i].Name), AnsiString(' ' + items[i].SQLFormat(FUseAccessSyntax, FQuotesInDates, FUSDates, FuseDoubleQuotesInStrings, FDateMask, FDateTimeMask) + ' '));
    //added space 07/23/98
  Result := String(AnsiResult);
end;

function TAstaParamList.NullValuesToSql(S: string): string;
var
  i: Integer;
  AnsiResult: AnsiString;
begin
  AnsiResult := AnsiString(S);
  for i := 0 to count - 1 do
    ReplaceS(AnsiResult, AnsiString(':' + items[i].Name), AnsiString(items[i].SQLFormatNull(FUseAccessSyntax, FQuotesInDates, FUSDates, FUseDoubleQuotesInStrings, FDateMask, FDateTimeMask)));
  Result := String(AnsiResult);
end;

function TAstaParamList.MergeValues(L: TStrings; AccessSyntax, UseQuotesInDates, ForceUSDates, DoubleQuotesInStrings: Boolean; OnCustomParamEvent: TSQLCustomParamsEvent; DateMask, DateTimeMask: string): string;

var
  i: Integer;
begin
  FUseAccessSyntax := AccessSyntax;
  FQuotesInDates := UseQuotesInDates;
  FUsDates := ForceUSDates;
  FuseDoubleQuotesInStrings := DoubleQuotesInStrings;
  FDateMask := DateMask;
  FDateTimeMask := DateTimeMask;
  result := '';
  for i := 0 to L.count - 1 do
    result := result + ValuesToSQL(l[i], OnCustomParamEvent) + ' ';
  FUsDates := False; //only for sql selects
end;

function TAstaParamList.NullValueToListForDesignTime(L: TStrings; DateMask, DateTimeMask: string): TStringList;
var
  i: Integer;
  tray: string;
begin
  result := TStringList.Create;
  FDateMask := DateMask;
  FDateTimeMask := DateTimeMask;
  for i := 0 to L.count - 1 do
  begin
    tray := NullValuesToSQL(l[i]);
    if tray <> '' then result.add(tray);
  end;
end;

procedure TAstaParamList.WriteItems(Writer: TAstaIOWriter);
var
  i: Integer;
begin
  Writer.WriteListBegin;
  for i := 0 to count - 1 do
    items[i].WriteProperties(Writer);
  Writer.WriteListEnd;
end;

procedure TAstaParamList.FastAdd(Value: Variant);
var
  t: TAstaParamItem;
begin
  t := Add;
  T.Name := 'FastAdd' + IntToStr(Count - 1);
  T.Value := Value;
  T.ParamType := ptinput;
end;

procedure TAstaParamList.FastAdd(Aname: string; DataSet:TDataSet);
begin
  FastAdd(AName,'');
  items[count-1].AsDataSet:=DataSet;
end;

procedure TAstaParamList.FastAdd(const AName: string; ADataType: TFieldtype;
      AValue: Variant);
var
  t: TAstaParamItem;
begin
  t := Add;
  t.Name := AName;
  t.Value := AValue;
  t.Paramtype := ptinput;
  t.DataType := ADataType;
end;

procedure TAstaParamList.FastAdd(Aname: string; Value: Variant);
var
  t: TAstaParamItem;
begin
  t:=nil;
  if AName<>'' then  t:=FindParam(AName);
  if t=nil then begin
   t := Add;
   T.Name := AName;
   T.Value := Value;
   T.ParamType := ptinput;
   end else t.value:=Value;
end;

Function TAstaParamList.GetAsStringAndFree(ParamName:String):string;
var
  t: TAstaParamItem;
begin
  result:='';
  t:=FindParam(ParamName);
  if t=nil then exit;
  result:=T.AsString;
  t.Free;
end;

Function TAstaParamList.AsDisplayText:String;
var
i:integer;
begin
 result:='';
 for i:=0 to count-1 do
  if items[i].DataType in [ftmemo,ftblob]  then
   result:=result+items[i].Name+': +[Size '+IntToStr(Length(items[i].AsString))+']'+#13 else
  result:=result+items[i].Name+':'+items[i].AsString+#13;
end;

procedure TAstaParamList.ConstantAdd(const Values: array of const);
var
  t: TAstaParamItem;
  i: Integer;
begin
  for i := low(Values) to High(Values) do
  begin
    t := Add;
    T.Name := 'FastAdd' + IntToStr(NextID);
    T.ParamType := ptinput;
    with Values[I] do
      case VType of
        vtInteger: T.AsInteger := VInteger;
        vtBoolean: T.AsBoolean := VBoolean;
        vtExtended: T.AsFloat := VExtended^;
        vtString: T.AsString := VString^;
        vtAnsiString: T.AsString := string(VAnsiString);
      end;
  end;
end;

{$ifndef SQLDATASETONLY}
function TAstaParamList.AsPDATransportString: AnsiString;

type PInteger = ^Integer;

var
  i, j , k, l: Integer;
  s : AnsiString;
  ft: byte;
  pc: PAnsiChar;
  b : boolean;
  d : double;
  dt: TPDATimeRecord;
  NameAnsi: AnsiString;
begin
  result := '';
  k:=0;
  for i := 0 to Count - 1 do
  begin
    with Items[i] do
    begin
      NameAnsi := AnsiString(FName);
      j := sizeof(Integer) + (Length(NameAnsi) + 1) + sizeof(byte) * 3 + sizeof(integer);
      ft := VCLFieldTypeToPDAType(FDataType);
      case TAstaPDAFieldType(ft) of
        pftUnknown  : k := 0;
        pftByte,
        pftSmallint,
        pftLongint,
        pftBoolean  : k := 4;
        pftSingle,
        pftDouble   : k := sizeof(double);
        pftDateTime : k := sizeof(dt);
        pftDate,
        pftTime     : k := sizeof(integer);
        pftString   : k := Length(FData) + 1;
        pftBlob     : k := Length(FData);
      end;
      inc(j, k);
      SetLength(S, j);
      pc := @S[1];
      PInteger(pc)^ := j - sizeof(Integer);
      inc(pc, sizeof(Integer));
      Move(NameAnsi[1], PC^, Length(NameAnsi) + 1);
      Inc(pc, Length(NameAnsi) + 1);
      pc^ := AnsiChar(ft); inc(pc);
      pc^ := AnsiChar(FParamType); inc(pc);
      pc^ := AnsiChar(FNull); inc(pc);
      PInteger(pc)^ := k;
      inc(pc, sizeof(Integer));
      case TAstaPDAFieldType(ft) of
        pftByte,
        pftSmallint,
        pftLongint  : begin
                        l := AsInteger;
                        Move(l, PC^, SizeOf(l));
                      end;
        pftBoolean  : begin
                        b := AsBoolean;
                        l := Integer(b);
                        Move(l, PC^, SizeOf(l));
                      end;
        pftString   : begin
                        if k = 1 then
                          PC^ := #0
                        else
                          Move(FData[1], PC^, k);
                      end;
        pftBlob     : Move(FData[1], PC^, k);
        pftSingle,
        pftDouble   : begin
                        d := AsFloat;
                        Move(d, PC^, SizeOf(d));
                      end;
        pftDateTime : begin
                        dt := DateTimeToPDA(AsDateTime);
                        Move(dt, PC^, SizeOf(dt));
                      end;
        pftDate     : begin
                        l := DateToPDA(AsDateTime);
                        Move(l, PC^, SizeOf(l));
                      end;
        pftTime     : begin
                        l := TimeToPDA(AsDateTime);
                        Move(l, PC^, SizeOf(l));
                      end;
      end;
    end;
    result := result + S;
  end;
end;

procedure TAstaParamList.AddFromPDATransPortString(const S: AnsiString);
type PDouble = ^double;
     PInteger= ^Integer;
var
  RawData : AnsiString;
  DataSize : Integer;
  item     : TAstaParamItem;
  curp     : PAnsiChar;
  S1       : AnsiString;
  NameLen  : Integer;

begin
  item:=Add;
  with item do
  begin
    if s = '' then exit;
    S1 := S;
    curp := @s1[1];
    while curp^ <> #0 do inc(curp);
    NameLen := curp - @s1[1];
    FName := string(copy(S1, 1, NameLen));
    System.Delete(S1, 1, NameLen + 1);
    curp := @s1[1];

    FDataType := PDATypeToVCLFieldType(Byte(CurP^));
    Inc(CurP);
    FParamType := TParamType(Byte(CurP^));
    Inc(CurP);
    FNull := CurP^ <> #0;
    FData:='';
    Inc(CurP);
    DataSize := PInteger(CurP)^;
//    Inc(CurP, 4);
    System.Delete(S1, 1, 7);
    curp := @s1[1];
    if (not FNull) and (DataSize > 0) then
    begin
     RawData := S1;
     Case FDataType of
      ftBlob:    FData := RawData;
      ftDate:    SetAsDate(PDAAdjustDate(PInteger(CurP)^));
      ftTime:    SetAsTime(PDAAdjustTime(PInteger(CurP)^));
      ftDateTime:SetAsDateTime(PDAAdjustDateTime(PPDATimeRecord(CurP)^));
      ftFloat:   AsFloat := PDouble(@S1[1])^;
      ftBoolean: AsBoolean := Byte(S1[1]) <> 0;
      ftlargeint,
      ftSmallint,
      ftWord,
      ftInteger,
      ftAutoInc: AsInteger := PInteger(@S1[1])^;
      ftString : FData := RawData;
      else SetAsText(string(RawData));
     end;
    end;
  end;
end;

class function TAstaParamList.CreateFromPDATokenizedString(const S: string): TAstaParamList;
var
  SPtr, Sizer: Integer;
  S1: string;
begin
  Result := TAstaParamList.Create;

  if s = '' then exit;
  SPtr := 1;

  repeat
    Sizer := ByteStringspotToInteger(S, Sptr);
    s1 := Copy(S, Sptr + sizeof(Sizer), Sizer);
    Inc(SPtr, Sizer + 4);
    Result.AddFromPDATransportString(s1);
  until SPtr >= Length(S);
end;
procedure TAstaParamList.LoadFromStream(Stream: TStream);
var
  Reader: TAstaIOReader;
begin
  Reader := TAstaIOReader.Create(Stream, 1024);
  try
    with Reader do begin
      if not AnsiCompareText(ReadString, 'TAstaParamList') = 0 then
        raise EAstaParamException.Create('Stream does not appear to be a valid AstaParamList');
      ReadListBegin;
      while not EndOfList do
      begin
        with Self.Add do
          ReadProperties(Reader);
      end;
      ReadListEnd;
    end;
  finally
    Reader.Free;
  end;
end;

class function TAstaParamList.CreateFromJavaTokenizedString(const S: string): TAstaParamList;
var
  SPtr, Sizer: Integer;
  S1: string;
begin
  Result := TAstaParamList.Create;

  if s = '' then exit;
  SPtr := 1;

  repeat
    Sizer := ByteStringspotToInteger(S, Sptr);
    s1 := Copy(S, Sptr + sizeof(Sizer), Sizer);
    Inc(SPtr, Sizer + 4);
    Result.AddFromJavaTransportString(s1);
  until SPtr >= Length(S);
end;

procedure TAstaParamList.AddFromJavaTransPortString(const S: AnsiString);
var
  JavaData: AnsiString;
  DataSize: Integer;
  item: TAstaParamItem;
begin
  item := Add;
  with item do
  begin
    if s = '' then exit;
    FName := ParamTokenExtract(s, 0);
    FDataType := JdbcTypeToVCLFieldType(StringToInteger(ParamTokenExtract(s, 1)));
    FParamType := TParamType(StringToInteger(ParamTokenExtract(s, 2)));
    FNull := ParamTokenExtract(s, 3) = '1';
    FData := '';
    DataSize := StringToInteger(ParamTokenExtract(s, 4));
    if not FNull then begin
      JavaData := copy(s, Length(S) - DataSize + 1, DataSize);
      case FDatAType of
        ftblob:
          begin
            FData := JavaData;
            DataType := ftBlob;
            FNull := False;
          end;
        ftDate: SetAsDate(JavaAdjustDate(string(JavaData)));
        ftTime: SetAsTime(JavaAdjustTime(string(JavaData)));
        ftDateTime: SetasDateTime(JavaAdjustDateTime(string(JavaData)));
      else SetasText(string(javaData));
      end;
    end;
  end;
end;

{$IFNDEF SQLDATASETONLY}
function TAstaParamList.AsJavaTransportString: string;
var
  i: Integer;
  s: string;
begin
  result := '';
  for i := 0 to count - 1 do begin
    s := '';
    with items[i] do
      s := Fname +
        TokenForParams + IntToStr(ord(VCLFieldtypeToJdbc(FDataType))) +
        TokenForParams + IntToStr(ord(FParamType)) +
        TokenForParams + IntToStr(ord(FNull)) +
        TokenForParams + IntToStr(Length(Fdata)) +
        TokenForParams + JavaTransportFormat;
    result := result + IntegerToByteString(Length(s)) + s;
  end;
end;
{$ENDIF}
{$endif}

// -----------------------------------------------------------------------
// utils

procedure AddParamstoListFromString(S: string; L: TStrings);
const
  Quote = '''';
var
  Spot: Integer;
  Tray: string;
  AddToList: Boolean;
  DupList: TSTringList;
begin
  AddToList := False;
  L.Clear; // EE
  if Pos(':', S) = 0 then exit; // if no params, exit
  Spot := 0;
  DupList := TStringList.Create;
  repeat
    Inc(Spot);
//    SetLength(Tray,Length(S));
    Tray := '';
    // don't grab anything between a quoted string, just walk through it
    if S[Spot] = Quote then
      repeat
        Inc(Spot);
        if Spot = Length(S) then
        begin // they didn't close the quote
          DatabaseError(SOpenQuote);
          Break;
        end;
      until S[Spot] = Quote;

    if S[Spot] = ':' then
    begin
      AddToList := True;
      repeat
        Inc(Spot);
         // check for stupid typos -- it returns the bad string, but warned them.
        if S[Spot] = ':' then
        begin
          raise Exception.Create(SCheckSyntax);
        end;
//        if (S[Spot] <> Quote) and ((S[Spot] in ['0'..'9','a'..'z','A'..'Z','_'])) then
        if (S[Spot] <> Quote) and (not (S[Spot] in [',', #13, #10, ')', ' ', ';', '-', '+', '*'])) then
          Tray := Tray + S[Spot]; // picks up ':MyParam' properly
      until (S[Spot] in [' ', #13, '*', '+', '-', ',']) or (Spot = Length(S));
    end;
    if AddToList and (DupList.Indexof(Tray) < 0) then
    begin
      L.Add(Tray);
      DupList.Add(Tray);
    end;
    AddToList := False;
  until Spot >= Length(S);
  DupList.Free;
end;

function ParamsFromList(L: TStrings): TStringList;
var
  I: Integer;
begin
  Result := TStringList.Create;
  for I := 0 to L.Count - 1 do
    AddParamsToListFromString(L.Text, Result);
end;

procedure StringToAstaParamListToTParams(const Data: AnsiString; Params: TParams);
var
  p:  TAstaParamList;
begin
  p := TAstaParamList.CreateFromTokenizedString(Data);
  try
    p.CopyParams(Params, True);
  finally
    P.Free;
  end;
end;

function TParamsToAstaParams(Params: TParams): TAstaParamList;
begin
  result := TParamsToAstaParams(Params, []);
end;

function TParamsToAstaParams(Params: TParams; ParamTypes: TParamTypes): TAstaParamList;
var
  i: Integer;
begin
  Result := TAstaParamList.Create;
  for i := 0 to Params.Count - 1 do
    if (ParamTypes = []) or (Params[i].ParamType in ParamTypes) then
      Result.Add.Assign(Params[i]);
end;

function ParamsAsDataSet(TheParams:AnsiString): TAstaIODataSet;
var
  P: TAstaParamList;
begin
  P := TAstaParamList.CreateFromTokenizedString(TheParams);
  try
    result := ParamsAsDataSet(P);
  finally
    P.Free;
  end;
end;

function ParamsAsDataSet(P: TAstaParamList): TAstaIODataSet;
var
  i: Integer;
begin
  result := nil;
  if (p = nil) or (p.count = 0) then exit;
  result := TAstaIODataSet.Create(nil);
  with result do begin
    AddField('ParamName', ftstring, 100);
    AddField('ParamType', ftinteger, 0);
    AddField('DataType', ftinteger, 0);
    Open;
  end;
  for i := 0 to p.count - 1 do
    result.AppendRecord([p[i].name, ord(p[i].Paramtype), ord(p[i].DataType)]);
  result.First;
end;

function TParamsToAstaParamsString(Params: TParams): AnsiString;
var
  p: TAstaParamList;
begin
  p := TParamsToAstaParams(Params);
  try
    result := P.AsTokenizedString(False);
  finally
    P.Free;
  end;
end;

function AstaParamsToTParams(Params: TAstaParamList): TParams;
begin
  Result := TParams.Create;
  Params.CopyParams(result, True);
end;

function ParamListToStringFastAdd(const Data: array of Variant): AnsiString;
var
  p: TAstaParamList;
  i: Integer;
begin
  result := '';
  p := TAstaParamList.Create;
  try
    for i := 0 to high(data) do
      p.fastadd(data[i]);
    result := p.AstokenizedString(False);
  finally
    p.free;
  end;
end;

function ParamListToStringFastAdd(const ParamNames:Array of String;Data: array of Variant): AnsiString;
var
  p: TAstaParamList;
  i: Integer;
begin
  result := '';
  if high(paramNames)<>high(data) then raise Exception.Create('ParamNames and Data count must be the same!');
  p := TAstaParamList.Create;
  try
    for i := 0 to high(data) do
      p.fastadd(ParamNames[i],data[i]);
    result := p.AstokenizedString(False);
  finally
    p.free;
  end;
end;

procedure ParamItemAssignField(Dest:TParam;Source: TField);
begin
  if Source <> nil then
  begin
    if Source.IsNull then
      Dest.Clear
    else
      with Dest do
      case Source.DataType of
        ftString,
          ftFixedChar,
          ftWideString:
          begin
            DataType := Source.DataType;
            AsString := Source.AsString;
          end;
          ftMemo,
          ftOraCLOB:
          begin
            DataType := Source.DataType;
            AsMemo := Source.AsString;
          end;
        ftBlob,
          ftOraBLOB:
          begin
            DataType := Source.DataType;
            {$IFDEF Delphi2009AndUp}
            AsBlob := Source.AsBytes;
            {$ELSE}
            AsBlob := Source.AsString;
            {$ENDIF}
          end;
        ftBytes,
          ftVarBytes:
          begin
            DataType := Source.DataType;
            Value := Source.AsVariant;
          end;
        {$ifdef mswindows}
        ftGUID:
          AsString := Source.AsString;
        {$endif}
        ftBoolean:
          AsBoolean := Source.AsBoolean;
        ftFloat:
          AsFloat := Source.AsFloat;
        ftCurrency:
          AsCurrency := Source.AsCurrency;
        ftBCD:
          AsBCD := Source.AsCurrency;
        ftSmallInt:
          AsSmallint := Source.AsInteger;
        ftWord:
          AsWord := Source.AsInteger;
        ftInteger:
          AsInteger := Source.AsInteger;
        ftDate:
          AsDate := Source.AsDateTime;
        ftTime:
          AsTime := Source.AsDateTime;
        ftDateTime:
          AsDateTime := Source.AsDateTime;
{$ifdef Delphi6AndUp}
        ftLargeint:
          AsInteger := (Source as TLargeIntField).AsLargeInt;
        ftFmtBCD:
          AsFmtBcd := (Source as TFMTBCDField).AsBCD;
        ftTimeStamp:
          AsDateTime := Source.AsDateTime;
{$ENDIF}
      else
        Value := Source.Value;
      end;
    end;
end;

end.
