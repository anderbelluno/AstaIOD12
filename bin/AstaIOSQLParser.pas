{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10321: AstaIOSQLParser.pas 
{
{   Rev 1.0    4/10/2003 6:32:18 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:38:16 PM  Steve    Version: 1.505
}
unit AstaIOSQLParser;
{*********************************************************}
{*   Copyright (c) 1997-2002 Asta Technology Group Inc.  *}
{*               All rights reserved.                    *}
{*               www.astatech.com                        *}
{*********************************************************}

interface

uses Classes, DB;

const
  SUpdate = 'update';
  SSet = 'set';

  SDelete = 'delete';
  SSelect = 'select';
  SFrom = 'from';
  SWhere = 'where';
  SHaving = 'having';
  SGroup = 'group';
  SOrder = 'order';
  SBy = 'by';
  SInsert = 'insert';
  SInto = 'into';
  SAlter = 'alter';
  SCreate = 'create';
  SExecProc = 'exec';
  SExecuteProc = 'execute';
  MaxGet = 1000;

type
  TParseSQLOption = (psoUseFROMInDelete, psoUseINTOInInsert);
  TParseSQLOptions = set of TParseSQLOption;

  TSQLStatementType = (stUnknown, stSelect, stUpdate, stDelete, stInsert, stAlter, stCreate, stExecProc);

  TAboutString = string[20];

type
  TAstaIOSQLParser = class(TComponent)
  private
    FUpdateTable         :String;
    FDeleteTable         :String;
    FInsertTable         :String;
    FTables              :TStrings;
    FCorrelations        :TStrings;
    FGroup               :TStrings;
    FHaving              :String;
    FOrder               :TStrings;
    FFields              :TStrings;
    FSQL                 :TStrings;
    FWhere, FTheSQL      :String;
    FTableIsReadOnly     :Boolean;
    FSQLOptions          :TParseSQLOptions;
    FParams              :TParams;
    FAbout               :TAboutString;

    FLast,
    FMaxGet,
    IExecProc,
    IUpdate,
    IInsert,
    IInto,
    ISet,
    ISelect,
    IDelete,
    IDeleteFrom,
    IFrom,
    IWhere,
    IHaving,
    IGroup,
    IGroupBy,
    IOrder,
    IOrderBy             :Integer;
  protected
    FPlaces              :Array[0..12] of Integer;
    procedure ClearLists;
    procedure Init;
    function GetInsertTable(TheString :String) :String;
    function GetPart(Part :String; TheString :String) :Integer;
    function NextPos(StartIndex :Integer) :Integer;
    procedure Separate(TheString :String; var TheList :TStrings);
    function StripCrLf(const S :String) :String;
    procedure QueryChanged(Sender: TObject);

  public
    function SQLStatementType: TSQLStatementType;
    constructor Create(Owner :TComponent); override;
    destructor Destroy; override;
    procedure Construct;
    procedure Deconstruct;
    property TableIsReadOnly :Boolean read FTableIsReadOnly;
    property Params :TParams read FParams;
  published
    property About :TAboutString read FAbout write FAbout;
    property UpdateTable :String read FUpdateTable write FUpdateTable;
    property DeleteTable :String read FDeleteTable write FDeleteTable;
    property InsertTable :String read FInsertTable write FInsertTable;
    property Tables :TStrings read FTables;
    property Correlations: TStrings read FCorrelations;
    property Group :TStrings read FGroup;
    property Having :String read FHaving write FHaving;
    property Order :TStrings read FOrder;
    property Fields :TStrings read FFields;
    property Where: String read FWhere write FWhere;
    property SQL :TStrings read FSQL;
    property SQLOptions :TParseSQLOptions read FSQLOptions write FSQLOptions;
  end;

implementation

uses SysUtils,
     AstaIOResources;

function IsTrimEmpty(const TheString :String) :Boolean;
var i     :Integer;
begin
  Result:=False;
  for i:=1 to Length(TheString) do
    if TheString[i] > ' ' then Exit;
  Result:=True;
end;
     
function StringToPChar(const S :String) :PChar;
var l :Integer;
begin
  l:=Length(S);
  if l > 0 then
  begin
    Result:=StrAlloc(Length(S) + 1);
    StrPCopy(Result, S);
  end else Result:=nil;
end;

function PCharToString(P  :PChar) :String;
begin
  if Assigned(P) then
    Result:=P
  else Result:='';
end;

constructor TAstaIOSQLParser.Create(Owner :TComponent);
begin
  inherited;
  FFields:=TStringList.Create;
  FTables:=TStringList.Create;
  FOrder:=TStringList.Create;
  FCorrelations:=TStringList.Create;
  FGroup:=TStringList.Create;
  FSQLOptions:=[];
  FSQL:=TStringList.Create;
  TStringList(FSQL).OnChange:=QueryChanged;

  FParams:=TParams.Create;
end;

destructor TAstaIOSQLParser.Destroy;
begin
  FFields.Free;
  FTables.Free;
  FCorrelations.Free;
  FOrder.Free;
  FGroup.Free;
  FSQL.Free;
  FParams.Free;
  inherited;
end;

function TAstaIOSQLParser.StripCrLf(const S :String) :String;
var i, j   :Integer;
begin
  Result:=S;
  { replace the #13 and #10 }
  for i:=1 to Length(Result) do if Result[i] < #32 then Result[i]:=#32;
  { remove contigous white spaces }
  for i:=1 to Length(Result) do
    if Result[i] = #32 then
    begin
      j:=i;
      while (j < Length(Result)) and (Result[j] = #32) do Inc(j);
      Delete(Result, i, j - i - 1);
    end;
end;

procedure TAstaIOSQLParser.Deconstruct;
var i, SpcIdx, ThePos   :Integer;
    TheUpdateTable,
    TheDeleteTable,
    TheInsertTable,
    TheSelect,
    TheFrom,
    TheDeleteFrom,
    TheWhere,
    TheHaving,
    TheGroup,
    //TheInsertPart,
    TheOrder            :String;
    TheReference        :String;
    
begin
  ClearLists;

  FTheSQL:=StripCrLf(Trim(FSQL.Text));

  FMaxGet:=0;
  FLast:=0;
  IExecProc:=GetPart(SExecProc, FTheSQL);
  FPlaces[0]:=IExecProc;

  if IExecProc = 0 then
  begin
    FMaxGet:=0;
    FLast:=0;
    IExecProc:=GetPart(SExecuteProc, FTheSQL);
    FPlaces[0]:=IExecProc;
  end;  

  FMaxGet:=0;
  FLast:=0;
  ISelect:=GetPart(SSelect, FTheSQL);
  FPlaces[0]:=ISelect;

  FMaxGet:=0;
  FLast:=0;
  IDelete:=GetPart(SDelete, FTheSQL);
  FPlaces[1]:=IDelete;

  FMaxGet:=0;
  FLast:=0;
  IUpdate:=GetPart(SUpdate, FTheSQL);
  FPlaces[2]:=IUpdate;

  FMaxGet:=0;
  FLast:=0;
  IInsert:=GetPart(SInsert, FTheSQL);
  FPlaces[3]:=IInsert;

  FMaxGet:=0;
  FLast:=0;
  IInto:=GetPart(SInto, FTheSQL);
  if IInto > 0 then
    FPlaces[3]:=IInto;

  FMaxGet:=0;
  FLast:=0;
  ISet:=GetPart(SSet, FTheSQL);
  FPlaces[4]:=ISet;

  FMaxGet:=0;
  FLast:=0;
  IDeleteFrom:=GetPart(SFrom, FTheSQL);
  FPlaces[5]:=IDeleteFrom;

  if (IDelete > 0) and (IDeleteFrom > 0) then
  begin
    FMaxGet:=0;
    FLast:=0;
    IFrom:=GetPart(SFrom, Copy(FTheSQL, IDeleteFrom + 1, Length(FTheSQL) - IDeleteFrom)) + IDeleteFrom;
    FPlaces[6]:=IFrom;
  end
  else
  begin
    FMaxGet:=0;
    FLast:=0;
    IFrom:=GetPart(SFrom, FTheSQL);
    FPlaces[6]:=IFrom;
  end;

  FMaxGet:=0;
  FLast:=0;
  IWhere:=GetPart(SWhere, FTheSQL);
  FPlaces[7]:=IWhere;

  FMaxGet:=0;
  FLast:=0;
  IGroup:=GetPart(SGroup, FTheSQL);
  FPlaces[8]:=IGroup;

  FMaxGet:=0;
  FLast:=0;
  IGroupBy:=GetPart(SBy, FTheSQL);
  FPlaces[9]:=IGroupBy;

  FMaxGet:=0;
  FLast:=0;
  IHaving:=GetPart(SHaving, FTheSQL);
  FPlaces[10]:=IHaving;

  FMaxGet:=0;
  FLast:=0;
  IOrder:=GetPart(SOrder, FTheSQL);
  FPlaces[11]:=IOrder;

  FMaxGet:=0;
  FLast:=0;
  if IOrder > 0 then
  begin
    IOrderBy:=IOrder + GetPart(SBy, Copy(FTheSQL, IOrder, Length(FTheSQL)-IOrder)) - 1;
    FPlaces[12]:=IOrder;
  end;

  if ISelect > 0 then
  begin
    if IInsert > 0 then
      ThePos:=NextPos(5)
    else
      ThePos:=NextPos(1);
    TheSelect:=Copy(FTheSQL,
                    ISelect + Length(SSelect),
                    ThePos - (ISelect + Length(SSelect)));
  end;

  if IDelete > 0 then
  begin
    TheDeleteTable:=Copy(FTheSQL,
                         IDelete + Length(SDelete),
                         NextPos(2) - (IDelete + Length(SDelete)));
    FDeleteTable:=Trim(TheDeleteTable);
  end;

  if IUpdate > 0 then
    TheUpdateTable:=Copy(FTheSQL,
                         IUpdate + Length(SUpdate),
                         NextPos(3) - (IUpdate + Length(SUpdate)));
  FUpdateTable:=Trim(TheUpdateTable);

  if IInsert > 0 then
  begin
    if IInto > 0 then
      TheInsertTable:=Copy(FTheSQL,
                           IInto + Length(SInto),
                           NextPos(4) - (IInto + Length(SInto)))
    else
    TheInsertTable:=Copy(FTheSQL,
                         IInsert + Length(SInsert),
                         NextPos(4) - (IInsert + Length(SInsert)));
    FInsertTable:=GetInsertTable(Trim(TheInsertTable));
    for i:=0 to FParams.Count - 1 do
      FFields.Add(FParams[i].Name);
  end;


  if ISet > 0 then
    TheSelect:=Copy(FTheSQL,
                    ISet + Length(SSet),
                    NextPos(5) - (ISet + Length(SSet)));

  if (IDeleteFrom > 0) and (IsTrimEmpty(FDeleteTable)) then
  begin
    TheDeleteFrom:=Copy(FTheSQL,
                        IDeleteFrom + Length(SFrom),
                        NextPos(6) - (IDeleteFrom + Length(SFrom)));
    FDeleteTable:=Trim(TheDeleteFrom);
  end;

  if IFrom > 0 then
    TheFrom:=Copy(FTheSQL,
                  IFrom + Length(SFrom),
                  NextPos(7) - (IFrom + Length(SFrom)));

  if IWhere > 0 then
  begin
    if IOrderBy > IWhere then
      TheWhere:=Copy(FTheSQL,
                     IWhere + Length(SWhere),
                     FPlaces[11] - (IWhere + Length(SWhere)))
    else
      TheWhere:=Copy(FTheSQL,
                     IWhere + Length(SWhere),
                     NextPos(8) - (IWhere + Length(SWhere)));
  end;
  FWhere:=Trim(TheWhere);

  if IGroupBy > 0 then
    TheGroup:=Copy(FTheSQL,
                   IGroupBy + Length(SBy),
                   NextPos(10) - (IGroupBy + Length(SBy)));

  if IHaving > 0 then
    TheHaving:=Copy(FTheSQL,
                    IHaving + Length(SHaving),
                    NextPos(11) - (IHaving + Length(SHaving)));
  FHaving:=Trim(TheHaving);

  if IOrderBy > 0 then
    TheOrder:=Copy(FTheSQL,
                   IOrderBy + Length(SBy),
                   Length(FTheSQL) - (IOrderBy + Length(SBy)-1));

  Separate(Trim(TheSelect), FFields);
  if (IDelete > 0) and (IsTrimEmpty(FDeleteTable)) then
    FDeleteTable:=Trim(TheFrom)
  else
    Separate(Trim(TheFrom), FTables);
  Separate(Trim(TheGroup), FGroup);
  Separate(Trim(TheOrder), FOrder);

  FCorrelations.Clear;
  for i := 0 to FTables.Count - 1 do
  begin
    // check for the space indicating a correlation name
    SpcIdx := AnsiPos(#32, Trim(FTables[i]));
    // if it's there then separate

    TheReference:=Trim(Copy(FTables[i], SpcIdx, MaxInt));
    if SpcIdx > 0 then
      FTables[i] := Copy(FTables[i], 1, SpcIdx - 1)
    else
      TheReference:='';
    FCorrelations.Add(TheReference);
  end;

  if Tables.Count > 0 then
  begin
    if (AnsiPos('=', TheSelect) > 0)
      or (AnsiPos('+', TheSelect) > 0)
      or (AnsiPos('-', TheSelect) > 0)
      or ((AnsiPos('*', TheSelect) > 0) and (Trim(Fields.Text) <> '*'))
      or (AnsiPos('/', TheSelect) > 0)
      or (AnsiPos('(', TheSelect) > 0)
      or (AnsiPos(')', TheSelect) > 0) then
        FTableIsReadOnly:=True;
  end;
end;

procedure TAstaIOSQLParser.Construct;

  function AddList(List: TStrings; CorrList :TStrings = nil): string;
  var i: Integer;
  begin
    Result := '';
    for i := 0 to List.Count - 1 do
    begin
      if Assigned(CorrList) then
        Result := Result + Trim(List[i] + ' ' + CorrList[i]) + ','
      else
        Result := Result + Trim(List[i]) + ',';
        // Result := Result + Trim(List[i] + ' ' + PCharToString(Pointer(List.Objects[i]))) + ',';
    end;
    if Length(Result) > 0 then Result[Length(Result)] := ' ';
  end;

var TheSelect,
    TheFrom,
    TheInsert,
    TheWhere,
    TheOrder,
    TheHaving,
    TheGroup,
    TheUpdate,
    TheDelete,
    TheSet      :String;
    i           :Integer;
    TmpSQL      :TStrings;
    TheFields   :String;
begin
//  if Length(Trim(InsertTable)) > 0 then
//    NotSupportError(SInsertConstructNotSupported, Self);

  TmpSQL:=TStringList.Create;
  try
    if IsTrimEmpty(UpdateTable)
      and IsTrimEmpty(DeleteTable)
      and IsTrimEmpty(InsertTable) then
    begin
      // select is mandatory
      TheSelect:=SSelect + ' ' + AddList(Fields);
      // so is from
      TheFrom:=SFrom + ' ' + AddList(Tables, Correlations);
      // all the rest are optional so check first }

      if Length(FWhere) > 0 then
        TheWhere:=SWhere + ' ' + FWhere;

      if Length(FHaving) > 0 then
        TheHaving:=SHaving +  ' ' + Having;

      if (FGroup.Count > 0) and (Trim(Group.Text) <>'') then
        TheGroup:=SGroup + ' ' + SBy + ' ' + AddList(Group);

      if (Order.Count > 0) and (Length(Trim(Order.Text)) > 0) then
        TheOrder:=SOrder + ' ' + SBy + ' ' + AddList(Order);

      TmpSQL.Add(TheSelect);
      if Trim(TheFrom) <> '' then
        TmpSQL.Add(TheFrom);
      if Trim(TheWhere) <> '' then
        TmpSQL.Add(TheWhere);
      if Trim(TheGroup) <> '' then
        TmpSQL.Add(TheGroup);
      if Trim(TheHaving) <> '' then
        TmpSQL.Add(TheHaving);
      if Trim(TheOrder) <> '' then
        TmpSQL.Add(TheOrder);
    end
    else
    if not IsTrimEmpty(UpdateTable) then
    begin
      // update is mandatory
      TheUpdate:=SUpdate + ' ' + UpdateTable;

      // set is mandatory
      TheSet:=SSet + ' ' + AddList(Fields);

      if not IsTrimEmpty(Tables.Text) then
        TheFrom:=SFrom + ' ' + AddList(Tables);
      // all the rest are optional so check first }

      if Length(FWhere) > 0 then
        TheWhere:=SWhere + ' ' + FWhere;

      if Length(FHaving) > 0 then
        TheHaving:=SHaving +  ' ' + Having;

      TmpSQL.Add(TheUpdate);
      TmpSQL.Add(TheSet);

      if Trim(TheFrom) <> '' then
        TmpSQL.Add(TheFrom);
      if Trim(TheWhere) <> '' then
        TmpSQL.Add(TheWhere);
      if Trim(TheHaving) <> '' then
        TmpSQL.Add(TheHaving);
    end
    else
    if not IsTrimEmpty(DeleteTable) then
    begin
      // delete is mandatory

      // some db's need a from
      if psoUseFROMInDelete in FSQLOptions then
        TheDelete:=SDelete + ' from ' + DeleteTable
      else
        TheDelete:=SDelete + ' ' + DeleteTable;

      if not IsTrimEmpty(Tables.Text) then
        TheFrom:=SFrom + ' ' + AddList(Tables);
      // all the rest are optional so check first }

      if Length(FWhere) > 0 then
        TheWhere:=SWhere + ' ' + FWhere;

      if Length(FHaving) > 0 then
        TheHaving:=SHaving +  ' ' + Having;

      TmpSQL.Add(TheDelete);

      if Trim(TheFrom) <> '' then
        TmpSQL.Add(TheFrom);
      if Trim(TheWhere) <> '' then
        TmpSQL.Add(TheWhere);
      if Trim(TheHaving) <> '' then
        TmpSQL.Add(TheHaving);
    end
    else
    if not IsTrimEmpty(InsertTable) then
    begin
      // insert is mandatory

      // some db's need a from
      if psoUseINTOInInsert in FSQLOptions then
        TheInsert:=SInsert + ' into ' + InsertTable
      else
        TheInsert:=SInsert + ' ' + InsertTable;

      if not IsTrimEmpty(Tables.Text) then
        TheFrom:=SFrom + ' ' + AddList(Tables);

      TheFields:='(';
      for i:=0 to FFields.Count - 1 do
      begin
        TheFields:=TheFields + FFields[i];
        if i < FFields.Count - 1 then
          TheFields:=TheFields + ', ';
      end;
      TheFields:=TheFields + ')';
      TmpSQL.Add(TheInsert);
      TmpSQL.Add(TheFields);
      
      TmpSQL.Add('values');

      TheFields:='(';
      for i:=0 to FParams.Count - 1 do
      begin
        if FParams[i].ParamType = ptInput then
          TheFields:=TheFields + ':' + FParams[i].Name
        else
          TheFields:=TheFields + FParams[i].Name;
        if i < FParams.Count - 1 then
          TheFields:=TheFields + ', ';
      end;
      TheFields:=TheFields + ')';
      TmpSQL.Add(TheFields);
    end;
  finally
    FSQL.Assign(TmpSQL);
    TmpSQL.Free;
  end;

end;

procedure TAstaIOSQLParser.ClearLists;
var i    :Integer;
begin
  for i:=0 to High(FPlaces) do
    FPlaces[i]:=0;

  FTables.Clear;
  FGroup.Clear;
  FOrder.Clear;
  FFields.Clear;
  FHaving:='';
  FWhere:='';
  FUpdateTable:='';
  FDeleteTable:='';
  FInsertTable:='';
  FUpdateTable:='';
  FDeleteTable:='';

  ISelect:=0;
  IDelete:=0;
  IDeleteFrom:=0;
  IFrom:=0;
  IWhere:=0;
  IGroup:=0;
  IHaving:=0;
  IOrder:=0;
  IGroupBy:=0;
  IOrderBy:=0;
  IInsert:=0;
  IInto:=0;
end;

{$HINTS OFF}
function TAstaIOSQLParser.GetPart(Part, TheString: String): Integer;
var s1       :String;
    idx      :Integer;
    Continue :Boolean;
begin
  Inc(FMaxGet);
  Result:=FLast;
  if FMaxGet >= MaxGet then exit;

  Idx:=AnsiPos(UpperCase(Part), UpperCase(TheString));
  if (Idx > 0)  then
  begin
    FLast:=FLast + Idx;
    if ((Ord(TheString[Idx + Length(Part)]) = 32)
      or (Ord(TheString[Idx + Length(Part)]) = 0)
      or (TheString[Idx + Length(Part)] = #13)
      or (TheString[Idx + Length(Part)] = #10))
      and
      ((Ord(TheString[Idx - 1]) = 32)
      or (Ord(TheString[Idx - 1]) = 0)
      or (TheString[Idx - 1] = #13)
      or (TheString[Idx - 1] = #10))

      then Continue:=False
    else Continue:=True;

    if not Continue then
    begin
      Result:=FLast;
      exit;
    end;
    s1:=Copy(TheString, Idx+1, Length(TheString)-Idx);
    Idx := Idx + GetPart(Part, s1);
  end
  else
    FLast:=0;
  Result:=FLast;
end;
{$HINTS ON}
procedure TAstaIOSQLParser.Init;
begin
  FTables:=TStringList.Create;
  FGroup:=TStringList.Create;
  FOrder:=TStringList.Create;
  FFields:=TStringList.Create;
  FSQL:=TStringList.Create;
  FHaving:='';
  FWhere:='';
  FUpdateTable:='';
  FDeleteTable:='';
  FInsertTable:='';
end;

function TAstaIOSQLParser.NextPos(StartIndex: Integer): Integer;
var i       :Integer;
begin
  Result:=Length(FSQL.Text);
  for i:=StartIndex to High(FPlaces) do
  begin
    if (FPlaces[i] > 0) and (FPlaces[i] < Result) then
    begin
      Result:=FPlaces[i];
      exit;
    end;
  end;
end;

procedure TAstaIOSQLParser.Separate(TheString: String;
  var TheList: TStrings);
var i   :Integer;
    s   :String;
begin
  s:='';
  for i:=1 to Length(TheString) do
  begin
    if TheString[i] <> ',' then
      s:=s + TheString[i]
    else
    begin
      TheList.Add(Trim(s));
      s:='';
    end;
  end;
  // 30 Dec 2000. When no text, we do not want a list with one empty string
  if Trim(s) <> '' then
    TheList.Add(Trim(s));
end;

function TAstaIOSQLParser.SQLStatementType: TSQLStatementType;
 function Matches(S :String) :Boolean;
 begin
   Result:=CompareText(Copy(FSQL.Text, 1, Length(s)), S)=0;
 end;

begin
  Result:=stUnknown;
  if IExecProc > 0 then Result:=stExecProc
  else if Matches(SInsert) then Result:=stInsert
  else if Matches(SSelect) then Result:=stSelect
  else if Matches(SUpdate) then Result:=stUpdate
  else if Matches(SDelete) then Result:=stDelete
  else if Matches(SAlter) then Result:=stAlter
  else if Matches(SCreate) then Result:=StCreate
end;

function TAstaIOSQLParser.GetInsertTable(TheString: String): String;
var i    :Integer;
begin
  Result:='';
  for i:=1 to Length(TheString) do
  begin
    if (Ord(TheString[i]) = 32)
      or (Ord(TheString[i]) = 0)
      or (TheString[i] = #10)
      or (TheString[i] = #13)
      or (TheString[i] = '(') then exit;
    Result:=Result + TheString[i];
  end;
end;

procedure TAstaIOSQLParser.QueryChanged(Sender: TObject);
var List   :TParams;
    i      :Integer;
    FText  :String;
begin
  List:=TParams.Create(Self);
  try
    FText:=List.ParseSQL(FSQL.Text, True);
    List.AssignValues(FParams);
    FParams.Clear;
    FParams.Assign(List);
  finally
    List.Free;
  end;

  for i:=0 to FParams.Count-1 do
    FParams[i].ParamType:=ptInput;
end;

end.
