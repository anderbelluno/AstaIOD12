{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10315: AstaIOSQLDataSet.pas 
{
{   Rev 1.0    4/10/2003 6:32:16 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:38:14 PM  Steve    Version: 1.505
}
unit AstaIOSQLDataSet;
{*********************************************************}
{*   Copyright (c) 1997-2002 Asta Technology Group Inc.  *}
{*               All rights reserved.                    *}
{*               www.astatech.com                        *}
{*********************************************************}

{$I AstaIO.inc}
interface

uses DB,
  Classes,
  SysUtils,
  AstaIOCustomDataSet, AstaIODBConst;

type
  TGenerated = class;

  TGeneratedItem = class(TCollectionItem)
  private
    FParams        :TParams;
    FSQL           :String;
    FSQLText       :String;
    FOwner         :TGenerated;
    FEditAction    :TDeltaType;
  protected
    function GetParams :TParams;
    procedure SetParams(Value :TParams);
  public
    constructor Create(Collection :TCollection); override;
    destructor Destroy; override;

    property Params :TParams read GetParams write SetParams;
    property SQL :String read FSQL write FSQL;
    property SQLText :String read FSQLText write FSQLText;
    property EditAction :TDeltaType read FEditAction write FEditAction;
  end;

  TGenerated = class(TCollection)
  private
  protected
    procedure SetItem(Index: Integer; Value: TGeneratedItem);
    function GetItem(Index: Integer): TGeneratedItem;
    function Insert(Index:Integer): TGeneratedItem;
  public
    constructor Create;
    destructor Destroy; override;
    function Add: TGeneratedItem;
    property Items[Index: Integer]: TGeneratedItem read GetItem write SetItem;
  end;

  TAstaIOSQLDataSet = class(TAstaCustomAuditDataSet)
  private
    FGenerated        :TGenerated;
    FTrimStringFields :Boolean;
    FUseNULLSyntax    :Boolean;
    function GetGenerated(Index :Integer) :TGeneratedItem;
    function GetGeneratedCount :Integer;
  protected
    function KosherForSQL(Field :TField) :Boolean;
  public
    constructor Create(AOwner :TComponent); override;
    destructor Destroy; override;

    procedure CreateSQL(ClearChanges :Boolean = True);
    property Generated[Index :Integer]:TGeneratedItem read GetGenerated;
    property GeneratedCount :Integer read GetGeneratedCount;
  published
    property TrimStringFields :Boolean read FTrimStringFields write FTrimStringFields;
    property UseNULLSyntax :Boolean read FUseNULLSyntax write FUseNULLSyntax;
    property PrimeFields;
    property NoSQLFields;
    property UpdateTableName;
    property UpdateMode;
  end;

implementation
uses AstaIOResources, AstaIOConst;

{ TAstaIOSQLDataSet }

constructor TAstaIOSQLDataSet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FStoreDefs:=True;
  FUseNULLSyntax:=True;

  FGenerated:=TGenerated.Create;
end;

destructor TAstaIOSQLDataSet.Destroy;
begin
  FGenerated.Free;
  inherited Destroy;
end;

procedure TAstaIOSQLDataSet.CreateSQL(ClearChanges :Boolean = True);
var SQLString       :String;
    FWhereSQL       :String;
    InsertSQL       :String;                        
    FInsertSQL      :String;
    FCurrentDataSet :TAstaIODataSet;
    FSQLTextString  :String;

    i, modcnt       :Integer;
    Action          :Integer;
    HotSpot         :Integer;
    TmpParams       :TParams;

  function Quote(Field: TField): string;
  begin
    Result:='';
    if Field.DataType in [ftString, ftDateTime, ftDate, ftTime, ftMemo] then Result:='''';
  end;

  function WhereSQL(TheUpdateMode: TUpdateMode): string;
  var
    i: Integer;
  begin
    Result := '';
    Result := ' WHERE ';
    FWhereSQL := Result;
    case TheUpdateMode of
      upWhereKeyOnly:
        begin
          for i := 0 to FPrimeFields.Count - 1 do
          begin
            if FUseNULLSyntax and FOldValuesDataSet.FieldbyName(FPrimeFields[i]).IsNull then
              Result := Result + FPrimeFields[i] + ' IS NULL'
            else
            begin
              Result := Result + FPrimeFields[i] + '=:' + 'w_' + FPrimeFields[i];

              TmpParams.CreateParam(FOldValuesDataSet.FieldbyName(FPrimeFields[i]).DataType, 'w_' + FPrimeFields[i], ptInput);
              TmpParams[TmpParams.Count - 1].Value:=FOldValuesDataSet.FieldbyName(FPrimeFields[i]).Value;
              if FOldValuesDataSet.FieldbyName(FPrimeFields[i]).IsNull then
                TmpParams[TmpParams.Count - 1].Clear;
            end;

            FWhereSQL := FWhereSQL + FPrimeFields[i] + '=';
            if FOldValuesDataSet.FieldbyName(FPrimeFields[i]).IsNull then
              FWhereSQL := FWhereSQL + 'NULL' 
            else
              FWhereSQL := FWhereSQL + Quote(FOldValuesDataSet.FieldbyName(FPrimeFields[i]))
                                     + FOldValuesDataSet.FieldbyName(FPrimeFields[i]).AsString
                                     + Quote(FOldValuesDataSet.FieldbyName(FPrimeFields[i]));
            if i < FPrimeFields.Count - 1 then
            begin
              Result := Result + ' AND ';
              FWhereSQL := FWhereSQL + ' AND ';
            end;
          end;
        end;

      upWhereAll:
        begin
          for i := 0 to FOldValuesDataSet.FieldCount - OldValsSpecFields - 1 do
          begin
            if FUseNULLSyntax and FOldValuesDataSet.Fields[i].IsNull then
              Result := Result + FOldValuesDataSet.Fields[i].FullName + ' IS NULL'
            else
            begin
              Result := Result + FOldValuesDataSet.Fields[i].FullName + '=:' + 'w_' + FOldValuesDataSet.Fields[i].FullName;

              TmpParams.CreateParam(FOldValuesDataSet.Fields[i].DataType, 'w_' + FOldValuesDataSet.Fields[i].FullName, ptInput);
              TmpParams[TmpParams.Count - 1].Value:=FOldValuesDataSet.Fields[i].Value;
              if FOldValuesDataSet.Fields[i].IsNull then
                TmpParams[TmpParams.Count - 1].Clear;
            end;
            FWhereSQL := FWhereSQL + FOldValuesDataSet.Fields[i].FullName + '=';
            if FOldValuesDataSet.Fields[i].IsNull then
              FWhereSQL := FWhereSQL + 'NULL'
            else
              FWhereSQL := FWhereSQL + Quote(FOldValuesDataSet.Fields[i])
                                     + FOldValuesDataSet.Fields[i].AsString
                                     + Quote(FOldValuesDataSet.Fields[i]);

            if i < FOldValuesDataSet.FieldCount - OldValsSpecFields - 1 then
            begin
              Result := Result + ' AND ';
              FWhereSQL := FWhereSQL + ' AND ';
            end;
          end;
        end;

      upWhereChanged:
        begin
          for i := 0 to FCurrentDataSet.FieldCount - OldValsSpecFields - 1 do
          begin
            if (FCurrentDataSet.Fields[i].AsString <> FOldValuesDataSet.FieldbyName(FCurrentDataSet.Fields[i].FieldName).AsString) then
            begin
              if FUseNULLSyntax and FCurrentDataSet.Fields[i].IsNull then
                Result:=Result + FCurrentDataSet.Fields[i].FullName + ' IS NULL AND '
              else
              begin
                Result:=Result + FCurrentDataSet.Fields[i].FullName + '=:' + 'w_' + FCurrentDataSet.Fields[i].FullName + ' AND ';

                TmpParams.CreateParam(FCurrentDataSet.Fields[i].DataType, 'w_' + FCurrentDataSet.Fields[i].FullName, ptInput);
                TmpParams[TmpParams.Count - 1].Value:=FOldValuesDataSet.FieldbyName(FCurrentDataSet.Fields[i].FieldName).Value;
                if FCurrentDataSet.Fields[i].IsNull then
                  TmpParams[TmpParams.Count - 1].Clear;
              end;
              FWhereSQL:=FWhereSQL + FCurrentDataSet.Fields[i].FullName + '=';
              if FCurrentDataSet.Fields[i].IsNull then
                FWhereSQL:=FWhereSQL + 'NULL AND '
              else
                FWhereSQL:=FWhereSQL + Quote(FCurrentDataSet.Fields[i])
                                     + FCurrentDataSet.Fields[i].AsString
                                     + Quote(FCurrentDataSet.Fields[i])
                                     + ' AND ';
            end;
          end;
          Result:=Copy(Result, 1, Length(Result) - 5); // Remove the last AND
          FWhereSQL := Copy(FWhereSQL, 1, Length(FWhereSQL) - 5);
        end;
    end;
  end;

begin
  if Length(Trim(FUpdateTableName)) = 0 then
    DatabaseError(SNoUpdateTable);

  if (FUpdateMode = upWhereKeyOnly) and (FPrimeFields.Count = 0) then
    DatabaseError(SNoPrimeFieldsGen);

  FCurrentDataSet:=DeltaDataSetCurrentValueDataSet;
  FOldValuesDataSet.First;
  TmpParams:=TParams.Create;
  FGenerated.Clear;
  try
    while not FOldValuesDataSet.Eof do
    begin
      TmpParams.Clear;
      Action := FOldValuesDataSet.FieldByName(sfld_Delta).AsInteger;
      HotSpot := FOldValuesDataSet.FieldByName(sfld_BookMark).AsInteger;
      try
        case Action of
          ord(dtEdit):
            begin
              // OldValues have corresponding CurrentValues
              if FCurrentDataSet.Locate(sfld_BookMark, HotSpot, []) then
              begin
                SQLString := 'UPDATE ' + FUpdateTableName + ' SET ';
                FSQLTextString := SQLString;
                modcnt := 0;
                for i := 0 to FCurrentDataSet.FieldCount - 1 do
                begin
                  if (FCurrentDataSet.Fields[i].AsString <> FOldValuesDataSet.FieldbyName(FCurrentDataSet.Fields[i].FieldName).AsString)
                    and KosherForSQL(FCurrentDataSet.Fields[i]) then
                  begin
                    inc(modcnt);
                    TmpParams.CreateParam(FCurrentDataSet.Fields[i].DataType, FCurrentDataSet.Fields[i].FullName, ptInput);

                    if FTrimStringFields and (FCurrentDataSet.Fields[i].DataType = ftString) then
                      TmpParams[TmpParams.Count - 1].Value:=Trim(FCurrentDataSet.Fields[i].Value)
                    else
                      TmpParams[TmpParams.Count - 1].Value:=FCurrentDataSet.Fields[i].Value;
                    if FCurrentDataSet.Fields[i].IsNull then
                      TmpParams[TmpParams.Count - 1].Clear;

                    SQLString := SQLString + FCurrentDataSet.Fields[i].FullName
                      + '=:' + FCurrentDataSet.Fields[i].FullName + ',';

                    FSQLTextString := FSQLTextString + FCurrentDataSet.Fields[i].FullName + '=';

                    if FCurrentDataSet.Fields[i].IsNull then
                      FSQLTextString := FSQLTextString + 'NULL' + ','
                    else
                      FSQLTextString := FSQLTextString + Quote(FCurrentDataSet.Fields[i])
                                                       + FCurrentDataSet.Fields[i].AsString
                                                       + Quote(FCurrentDataSet.Fields[i]) + ',';
                  end;
                end;
                if modcnt > 0 then
                begin
                  SQLString := copy(SQLString, 1, Length(SQLString) - 1) + WhereSQL(FUpdateMode);
                  FSQLTextString := copy(FSQLTextString, 1, Length(FSQLTextString) - 1) + FWhereSQL;
                end;
              end;
            end;

          ord(dtDelete):
            begin
              // Only OldValues, no corresponding CurrentValues
              modcnt:=1;
              if FUpdateMode = upWhereChanged then
                SQLString := 'DELETE FROM ' + FUpdateTableName + WhereSQL(upWhereAll)
              else
                SQLString := 'DELETE FROM ' + FUpdateTableName + WhereSQL(FUpdateMode);
              FSQLTextString := 'DELETE FROM ' + FUpdateTableName + FWhereSQL;
            end;

          ord(dtAppend):
            begin
              // OldValues is NULL, BookMark points to corresponding CurrentValues
              if FCurrentDataSet.Locate(sfld_BookMark, HotSpot, []) then
              begin
                SQLString := 'INSERT INTO ' + FUpdateTableName + '(';
                FSQLTextString := SQLString;

                InsertSQL := ' VALUES (';
                FInsertSQL := ' VALUES (';
                modcnt := 0;
                for i := 0 to FCurrentDataSet.FieldCount - 2 do
                begin
                  if not FCurrentDataSet.Fields[i].IsNull
                    and KosherForSQL(FCurrentDataSet.Fields[i]) then
                  begin
                    inc(modcnt);
                    SQLString := SQLString + FCurrentDataSet.Fields[i].FullName + ',';
                    TmpParams.CreateParam(FCurrentDataSet.Fields[i].DataType, FCurrentDataSet.Fields[i].FullName, ptInput);

                    if FTrimStringFields and (FCurrentDataSet.Fields[i].DataType = ftString) then
                      TmpParams[TmpParams.Count - 1].Value:=Trim(FCurrentDataSet.Fields[i].Value)
                    else
                      TmpParams[TmpParams.Count - 1].Value:=FCurrentDataSet.Fields[i].Value;

                    if FCurrentDataSet.Fields[i].IsNull then
                      TmpParams[TmpParams.Count - 1].Clear;

                    InsertSQL := InsertSQL + ':' + FCurrentDataSet.Fields[i].FullName + ',';

                    if FCurrentDataSet.Fields[i].IsNull then
                      FInsertSQL := FInsertSQL + 'NULL' + ','
                    else
                      FInsertSQL := FInsertSQL + Quote(FCurrentDataSet.Fields[i])
                                               + FCurrentDataSet.Fields[i].AsString
                                               + Quote(FCurrentDataSet.Fields[i]) + ',';
                  end;
                end;
                if modcnt > 0 then
                begin
                  SQLString := copy(SQLString, 1, Length(SQLString) - 1);
                  InsertSQL := copy(InsertSQL, 1, Length(InsertSQL) - 1); // Remove the last ,
                  FInsertSQL := copy(FInsertSQL, 1, Length(FInsertSQL) - 1);

                  InsertSQL := InsertSQL + ')';
                  FInsertSQL := FInsertSQL + ')';

                  FSQLTextString := SQLString + ')' + FInsertSQL;
                  SQLString := SQLString + ') ' + InsertSQL;
                end;
              end;
            end;

          ord(dtAppendAndDelete):
            begin
              // OldValues is NULL, BookMark points to corresponding CurrentValues
              if FCurrentDataSet.Locate(sfld_BookMark, HotSpot, []) then
              begin

              end;
            end;
        end;//case
      except
        break;
      end;
      if modcnt > 0 then
        with FGenerated.Add do
        begin
          SQL:=SQLString;
          SQLText:=FSQLTextString;
          Params.Assign(TmpParams);
          EditAction:=TDeltaType(Action);
        end;
      FOldValuesDataSet.Next;
    end;

    if ClearChanges then FOldValuesDataSet.Empty;
  finally
    FCurrentDataSet.Free;
    TmpParams.Free;
  end;
end;

function TAstaIOSQLDataSet.KosherForSQL(Field: TField): Boolean;
begin
  Result:=True;
  if Field.FieldKind <> fkData then Result:=False;
  if Result then
    Result:=FNoSQLFields.IndexOf(Field.FieldName) < 0;
end;

function TAstaIOSQLDataSet.GetGenerated(Index: Integer): TGeneratedItem;
begin
  Result:=FGenerated.Items[Index];
end;

function TAstaIOSQLDataSet.GetGeneratedCount: Integer;
begin
  Result:=FGenerated.Count;
end;

{ TGeneratedItem }

constructor TGeneratedItem.Create(Collection :TCollection);
begin
  Inherited Create(Collection);
  FOwner:=TGenerated(Collection);
  FParams:=TParams.Create;
end;

destructor TGeneratedItem.Destroy;
begin
  FParams.Free;
  Inherited Destroy;
end;

function TGeneratedItem.GetParams: TParams;
begin
  Result:=FParams;
end;

procedure TGeneratedItem.SetParams(Value: TParams);
begin
  FParams.Assign(Value);
end;

{ TGenerated }

function TGenerated.Add: TGeneratedItem;
begin
  Result:=TGeneratedItem(inherited Add);
end;

constructor TGenerated.Create;
begin
  inherited Create(TGeneratedItem);
end;

destructor TGenerated.Destroy;
begin
  inherited Destroy;
end;

function TGenerated.GetItem(Index: Integer): TGeneratedItem;
begin
  Result:=TGeneratedItem(inherited Items[Index]);
end;

function TGenerated.Insert(Index: Integer): TGeneratedItem;
begin
  Result:=Add;
  Result.Index:=Index;
end;

procedure TGenerated.SetItem(Index: Integer; Value: TGeneratedItem);
begin
  inherited Items[Index].Assign(Value);
end;

end.
