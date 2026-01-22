{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10317: AstaIOSQLGenerator.pas 
{
{   Rev 1.0    4/10/2003 6:32:16 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:38:16 PM  Steve    Version: 1.505
}
unit AstaIOSQLGenerator;
{*********************************************************}
{*   Copyright (c) 1997-2002 Asta Technology Group Inc.  *}
{*               All rights reserved.                    *}
{*               www.astatech.com                        *}
{*********************************************************}

{$I AstaIO.inc}

interface

uses DB, Classes, SysUtils,
  AstaIOUpdateObjectBase, AstaIOCustomDataSet, AstaIOParamList, AstaIODBConst,
  AstaIOSQLUtils;

type
  TDataModuleDataSetSessionSetupEvent = procedure(Sender: TObject; Session: TComponent; DS: TDataSet) of object;
  TAstaCustomAuditDataSetHack = class(TAstaCustomAuditDataSet);

type
  TAstaIOSQLGenerator = class(TAstaIOUpdateObjectBase)
  private
    FSQLText: string;
    FUseNULLSyntax    :Boolean;

    FDataModuleDataSetSessionSetup: TDataModuleDataSetSessionSetupEvent;
    FUpdateMode: TUpdateMode;
    function KosherForSQL(Field: TField): Boolean;
  protected
  public
    constructor Create(AOwner:TComponent); override;
    procedure UpdateFromClientDataSet(DataSet: TAstaCustomAuditDataSet); override;

    procedure MultiCreateSQL;
    procedure CreateSQL; override;

    property SQLText: string read FSQLText;
  published
    property UseNULLSyntax :Boolean read FUseNULLSyntax write FUseNULLSyntax;
    property OnSetupDataModuleDataSets: TDataModuleDataSetSessionSetupEvent read FDataModuleDataSetSessionSetup write FDataModuleDataSetSessionSetup;
    property UpdateMode: TUpdateMode read FUpdateMode write FUpdateMode;
    property OnProcessDetails;
  end;

implementation
uses
  AstaIOResources, AstaIOConst;
type
  TDataSetHack = class(TDataSet);
{ TAstaIOSQLGenerator }


procedure TAstaIOSQLGenerator.CreateSQL;
var
  i, modcnt: Integer;
  Action: Integer;
  HotSpot: Integer;
  Params: TAstaParamList;
  SQL: string;
  FWhereSQL: string;
  InsertSQL: string;
  FInsertSQL: string;
  RefetchParams: TAstaParamList;
  OraLobParams : TAstaParamList;

  function Quote(Field: TField): string;
  begin
    result := '';
    if Field.DataType in [ftString, ftWideString, ftDateTime, ftDate, ftTime, ftMemo] then Result := '''';
  end;

  function WhereSQL: string;
  var
    i: Integer;
    fname :String; // sm - 4/4/2003
  begin
    Result := '';
    Result := ' WHERE 1 = 1';
    FWhereSQL := Result;
    case FUpdateMode of
      upWhereKeyOnly:
        begin
          for i := 0 to FPrimeFields.Count - 1 do
          begin
            if FOldValuesDataSet.FieldbyName(FPrimeFields[i]).IsNull and FUseNULLSyntax then
              Result := Result + ' AND ' + FPrimeFields[i] + ' IS NULL'
            else
            begin
              Result := Result + ' AND ' + FormatFieldName(FPrimeFields[i]) + '=:' + FormatParamName(FPrimeFields[i]);
              Params.FastAdd(FormatParamName(FPrimeFields[i]), FOldValuesDataSet.FieldbyName(FPrimeFields[i]).Value);
              Params[Params.Count - 1].DataType := FOldValuesDataSet.FieldbyName(FPrimeFields[i]).DataType;
              Params[Params.Count - 1].IsNull := FOldValuesDataSet.FieldbyName(FPrimeFields[i]).IsNull;

              with RefetchParams.Add do
              begin
                Name:=FPrimeFields[i];
                IsNull := FOldValuesDataSet.FieldbyName(FPrimeFields[i]).IsNull;
                Value:=FOldValuesDataSet.FieldbyName(FPrimeFields[i]).Value;
                DataType:=FOldValuesDataSet.FieldbyName(FPrimeFields[i]).DataType;
              end;
            end;
            FWhereSQL := FWhereSQL + ' AND ' + FormatFieldName(FPrimeFields[i]) + '=' + Quote(FOldValuesDataSet.FieldbyName(FPrimeFields[i]))
              + FOldValuesDataSet.FieldbyName(FPrimeFields[i]).AsString
              + Quote(FOldValuesDataSet.FieldbyName(FPrimeFields[i]));
            {if i < FPrimeFields.Count - 1 then
            begin
              Result := Result + ' AND ';
              FWhereSQL := FWhereSQL + ' AND ';
            end;}
          end;
        end;

      upWhereAll:
        begin
          for i := 0 to FOldValuesDataSet.FieldCount - OldValsSpecFields - 1 do
          begin
            //if (FOldValuesDataSet.Fields[i].DataType = ftDataSet) or (FOldValuesDataSet.Fields[i].FieldKind = fkInternalCalc) then continue; // Original Asta
            if (FOldValuesDataSet.Fields[i].DataType = ftDataSet) or (FOldValuesDataSet.Fields[i].FieldKind = fkInternalCalc) or not KosherForSQL(FOldValuesDataSet.Fields[i]) then continue; // sm 08/17/2006
            if FOldValuesDataSet.Fields[i].IsNull and FUseNULLSyntax then
              Result := Result + ' AND ' + FOldValuesDataSet.Fields[i].FullName + ' IS NULL'
            else
            begin
              Result := Result + ' AND ' + FormatFieldName(FOldValuesDataSet.Fields[i].FullName) + '=:' + FormatParamName(FOldValuesDataSet.Fields[i].FullName);
              //Params.FastAdd(FOldValuesDataSet.Fields[i].FullName, FOldValuesDataSet.Fields[i].Value); // Original Asta
              Params.FastAdd(FormatParamName(FOldValuesDataSet.Fields[i].FullName), FOldValuesDataSet.Fields[i].Value); // jn 08/10/2006
              Params[Params.Count - 1].DataType := FOldValuesDataSet.Fields[i].DataType;
              Params[Params.Count - 1].IsNull := FOldValuesDataSet.Fields[i].IsNull;

              with RefetchParams.Add do
              begin
                Name:=FOldValuesDataSet.Fields[i].FullName;
                IsNull := FOldValuesDataSet.Fields[i].IsNull;
                Value:=FOldValuesDataSet.Fields[i].Value;
                DataType:=FOldValuesDataSet.Fields[i].DataType;
              end;
            end;
            FWhereSQL := FWhereSQL + ' AND ' + FormatFieldName(FOldValuesDataSet.Fields[i].FullName) + '=' + Quote(FOldValuesDataSet.Fields[i])
              + FOldValuesDataSet.Fields[i].AsString
              + Quote(FOldValuesDataSet.Fields[i]);
            {if i < FOldValuesDataSet.FieldCount - 4 then
            begin
              Result := Result + ' AND ';
              FWhereSQL := FWhereSQL + ' AND ';
            end;}
          end;
        end;

      upWhereChanged:
        begin
          for i := 0 to FCurrentDataSet.FieldCount - 1 do
          begin
            // sm - 4/4/2003 - maybe not the safest, but must stay until I find a cleaner way
            // start
            fname:=Uppercase(FCurrentDataSet.Fields[i].FieldName);
            if (fname = 'BOOKMARK') or (fname = 'DELTA') or (fname = 'SAVEPOINT') or (fname = 'ERRORCODE') or (fname = 'ERRORINFO') then continue;
            // end
            //if (FCurrentDataSet.Fields[i].DataType = ftDataSet) or (FCurrentDataSet.Fields[i].FieldKind = fkInternalCalc) then continue; // Original Asta
            if (FCurrentDataSet.Fields[i].DataType = ftDataSet) or (FCurrentDataSet.Fields[i].FieldKind = fkInternalCalc) or not KosherForSQL(FCurrentDataSet.Fields[i]) then continue; // sm 08/17/2006
            if (FCurrentDataSet.Fields[i].AsString <> FOldValuesDataSet.FieldbyName(FCurrentDataSet.Fields[i].FieldName).AsString) then
            begin
              if FOldValuesDataSet.FieldbyName(FCurrentDataSet.Fields[i].FieldName).IsNull and FUseNULLSyntax then
                Result := Result + ' AND ' + FormatFieldName(FCurrentDataSet.Fields[i].FullName) + ' IS NULL'
              else
              begin
                Result := Result + ' AND ' + FormatfieldName(FCurrentDataSet.Fields[i].FullName) + '=:' +  FormatParamName(FCurrentDataSet.Fields[i].FullName);
                Params.FastAdd(FormatParamName(FCurrentDataSet.Fields[i].FullName), FOldValuesDataSet.FieldbyName(FCurrentDataSet.Fields[i].FieldName).Value);
                Params[Params.Count - 1].DataType := FCurrentDataSet.Fields[i].DataType;
                Params[Params.Count - 1].IsNull := FOldValuesDataSet.FieldbyName(FCurrentDataSet.Fields[i].FieldName).IsNull;

                with RefetchParams.Add do
                begin
                  Name:=FCurrentDataSet.Fields[i].FullName;
                  DataType:=FCurrentDataSet.Fields[i].DataType;
                  IsNull := FCurrentDataSet.Fields[i].IsNull;
                  Value:=FCurrentDataSet.Fields[i].Value;
                end;
              end;
              FWhereSQL := FWhereSQL + ' AND ' + FormatFieldName(FCurrentDataSet.Fields[i].FullName) + '=' + Quote(FOldValuesDataSet.FieldbyName(FCurrentDataSet.Fields[i].FieldName))
                + FOldValuesDataSet.FieldbyName(FCurrentDataSet.Fields[i].FieldName).AsString
                + Quote(FOldValuesDataSet.FieldbyName(FCurrentDataSet.Fields[i].FieldName));
            end;
          end;
          //Result := Copy(Result, 1, Length(Result) - 5); // Remove the last AND
          //FWhereSQL := Copy(FWhereSQL, 1, Length(FWhereSQL) - 5);
        end;
    end;
    PrimeFieldsValues.Add(RefetchParams.AsTokenizedString);
    RefetchParams.Clear; // sm - 7/5/2003
  end;

  procedure ProcessDetails;
  var
    i: Integer;
    ds: TAstaCustomAuditDataSet;
  begin
    if Assigned(FOnProcessDetails) and (FBaseDataSet <> nil) then
      for i := 0 to TDataSetHack(FBaseDataSet).NestedDataSets.Count - 1 do begin
        ds := TDataSet(TDataSetHack(FBaseDataSet).NestedDataSets[i]) as TAstaCustomAuditDataSet;
        if ds.ChangeCount <> 0 then
          FOnProcessDetails(Self, ds, FSQLList);
      end;
  end;

begin
  if (MultiTableDataSet<>nil) then begin
   MultiCreateSQL;
   exit;
  end;
  FOldValuesDataSet.First;
  RefetchParams:=TAstaParamList.Create;
  Params := TAstaParamList.Create; // sm - 10/1/2002
  OraLobParams := TAstaParamList.Create; // jn - 09/22/2011

  while not FOldValuesDataSet.Eof do
  begin
    Action := FOldValuesDataSet.FieldByName(sfld_Delta).AsInteger;
    HotSpot := FOldValuesDataSet.FieldByName(sfld_BookMark).AsInteger;
    //Params := TAstaParamList.Create; // sm - 10/1/2002 - Moved up
    try
    case Action of
      ord(dtEdit):
        begin
          // OldValues have corresponding CurrentValues
          if FCurrentDataSet.Locate(sfld_BookMark, HotSpot, []) then
          begin
            SQL := 'UPDATE ' + FormatTableName(FUpdateTableName) + ' SET ';
            FSQLText := SQL;
            modcnt := 0;
            OraLobParams.Clear; 
            for i := 0 to FCurrentDataSet.FieldCount - 1 do
            begin
              if (FCurrentDataSet.Fields[i].FieldKind = fkData)
                and (FCurrentDataSet.Fields[i].AsString <> FOldValuesDataSet.FieldbyName(FCurrentDataSet.Fields[i].FieldName).AsString)
                and KosherForSQL(FCurrentDataSet.Fields[i]) then
              begin
                inc(modcnt);
                if FTrimStringFields and (FCurrentDataSet.Fields[i].DataType = ftString) then
                  Params.FastAdd(FCurrentDataSet.Fields[i].FullName, Trim(FCurrentDataSet.Fields[i].Value))
                else
                //If FCurrentDataSet.Fields[i].DataType = ftOraBlob Then // jn 09/23/2011
                If FCurrentDataSet.Fields[i].DataType in [ftOraBlob, ftOraClob] Then // jn 09/23/2011
                begin
                  //OraLobParams.FastAdd(FCurrentDataSet.Fields[i].FullName, FCurrentDataSet.Fields[i].Value); // Ok ! Funciona !
                  OraLobParams.FastAdd(FCurrentDataSet.Fields[i].FullName, FCurrentDataSet.Fields[i].DataType, FCurrentDataSet.Fields[i].Value);
                end
                else
                  Params.FastAdd(FCurrentDataSet.Fields[i].FullName, FCurrentDataSet.Fields[i].Value);

                if FTrimStringFields and (FCurrentDataSet.Fields[i].DataType = ftString) then
                  Params[Params.Count - 1].AsString := Trim(FCurrentDataSet.Fields[i].AsString)
                else
                //If FCurrentDataSet.Fields[i].DataType <> ftOraBlob Then // jn 09/23/2011
                If not ( FCurrentDataSet.Fields[i].DataType in [ftOraBlob, ftOraClob] ) Then // jn 09/23/2011
                  Params[Params.Count - 1].AssignField(FCurrentDataSet.Fields[i]);
                //Params[Params.Count - 1].IsNull := FCurrentDataSet.Fields[i].IsNull;
                //Params[Params.Count - 1].DataType := FCurrentDataSet.Fields[i].DataType;

                if FCurrentDataSet.Fields[i].DataType = ftOraBlob Then
                begin
                   SQL := SQL + FormatFieldName(FCurrentDataSet.Fields[i].FullName)
                     + '= EMPTY_BLOB() ' + ',';

                   FSQLText := FSQLText + FormatFieldName(FCurrentDataSet.Fields[i].FullName)
                     + '= EMPTY_BLOB() ' + ',';
                end
                else
                if FCurrentDataSet.Fields[i].DataType = ftOraClob Then // jn 09/26/2011
                begin
                   SQL := SQL + FormatFieldName(FCurrentDataSet.Fields[i].FullName)
                     + '= EMPTY_CLOB() ' + ',';

                   FSQLText := FSQLText + FormatFieldName(FCurrentDataSet.Fields[i].FullName)
                     + '= EMPTY_CLOB() ' + ',';
                end
                else
                begin
                   SQL := SQL + FormatFieldName(FCurrentDataSet.Fields[i].FullName)
                     + '=:'
                     + FCurrentDataSet.Fields[i].FullName + ','; // 1/9/2004 - sm. Was + FormatParamname(FCurrentDataSet.Fields[i].FullName) + ',';

                   FSQLText := FSQLText + FormatFieldName(FCurrentDataSet.Fields[i].FullName) // 1/9/2004 - sm. Was FSQLText := FSQLText + FCurrentDataSet.Fields[i].FullName
                     + '='
                     + Quote(FCurrentDataSet.Fields[i])
                     + FCurrentDataSet.Fields[i].AsString
                     + Quote(FCurrentDataSet.Fields[i]) + ',';
                end;
              end;
            end;

            if modcnt > 0 then
            begin
              If OraLobParams.Count = 0 Then // jn 09/23/2011
              Begin
                 // * Original code
                 SQL := copy(SQL, 1, Length(SQL) - 1) + WhereSQL;
                 FSQLText := copy(FSQLText, 1, Length(FSQLText) - 1) + FWhereSQL;
              End
              Else
              Begin
                 SQL := copy(SQL, 1, Length(SQL) - 1) + WhereSQL;
                 FSQLText := copy(FSQLText, 1, Length(FSQLText) - 1) + FWhereSQL;

                 For i:=0 to OraLobParams.Count - 1 do
                 Begin
                    //Params.FastAdd(FormatParamName(OraLobParams.Items[i].Name), ftOraBlob, OraLobParams.Items[i].AsString); // Ok ! Funciona !
                    Params.FastAdd(FormatParamName(OraLobParams.Items[i].Name), OraLobParams.Items[i].DataType, OraLobParams.Items[i].AsString);
                 End;

                 SQL := SQL + ' RETURNING ';
                 FSQLText := FSQLText + ' RETURNING ';

                 For i:=0 to OraLobParams.Count - 1 do
                 Begin
                    SQL := SQL + OraLobParams.Items[i].Name + ',';
                    FSQLText := FSQLText + OraLobParams.Items[i].Name + ',';
                 End;

                 SQL := copy(SQL, 1, Length(SQL) - 1);                // Remove the last
                 FSQLText := copy(FSQLText, 1, Length(FSQLText) - 1); // Remove the last

                 SQL := SQL + ' INTO ';
                 FSQLText := FSQLText + ' INTO ';

                 For i:=0 to OraLobParams.Count - 1 do
                 Begin
                    SQL := SQL +':' + FormatParamName(OraLobParams.Items[i].Name) + ',';
                    FSQLText := FSQLText + OraLobParams.Items[i].Value + ',';
                 End;

                 SQL := copy(SQL, 1, Length(SQL) - 1);                // Remove the last
                 FSQLText := copy(FSQLText, 1, Length(FSQLText) - 1); // Remove the last
              End;
              InternalAddSQL(SQL, Params, dtEdit);
            end
            else
            begin
              FreeAndNil(Params);
              //DatabaseError(SNoValuesToUpdate, Self);
            end;
            ProcessDetails;
        end;
        end;

      ord(dtDelete):
        begin
          // Only OldValues, no corresponding CurrentValues
          ProcessDetails;
          SQL := 'DELETE FROM ' + FormatTableName(FUpdateTableName) + WhereSQL;
          FSQLText := 'DELETE FROM ' + FormatTableName(FUpdateTableName) + FWhereSQL;
          InternalAddSQL(SQL, Params, dtDelete);
        end;

      ord(dtAppend):
        begin
          // OldValues is NULL, BookMark points to corresponding CurrentValues
          if FCurrentDataSet.Locate(sfld_BookMark, HotSpot, []) then
          begin
            SQL := 'INSERT INTO ' + FormatTableName(FUpdateTableName) + ' (';
            FSQLText := SQL;

            InsertSQL := ' VALUES (';
            FInsertSQL := ' VALUES (';
            modcnt := 0;
            OraLobParams.Clear;
            for i := 0 to FCurrentDataSet.FieldCount - 2 do
            begin
              if not FCurrentDataSet.Fields[i].IsNull
                and KosherForSQL(FCurrentDataSet.Fields[i])
                and (FCurrentDataSet.Fields[i].FieldKind = fkData) then
              begin
                inc(modcnt);
                SQL := SQL + FormatFieldName(FCurrentDataSet.Fields[i].FullName) + ',';
                if FTrimStringFields and (FCurrentDataSet.Fields[i].DataType = ftString) then
                  Params.FastAdd(FCurrentDataSet.Fields[i].FullName, Trim(FCurrentDataSet.Fields[i].Value)) // // 1/9/2004 - sm. Removed FormatParamName()
                else
                //If FCurrentDataSet.Fields[i].DataType = ftOraBlob Then // jn 09/23/2011
                If FCurrentDataSet.Fields[i].DataType in [ftOraBlob, ftOraClob] Then // jn 09/26/2011                
                begin
                  Params.FastAdd(FCurrentDataSet.Fields[i].FullName, FCurrentDataSet.Fields[i].Value);
                  OraLobParams.FastAdd(FCurrentDataSet.Fields[i].FullName, FCurrentDataSet.Fields[i].Value);
                end
                else
                  Params.FastAdd(FCurrentDataSet.Fields[i].FullName, FCurrentDataSet.Fields[i].Value); // 1/9/2004 - sm Removed FormatParamName()
                Params[Params.Count - 1].AssignField(FCurrentDataSet.Fields[i]);
                // Params[Params.Count - 1].DataType := FCurrentDataSet.Fields[i].DataType;
                //Params[Params.Count - 1].IsNull := FCurrentDataSet.Fields[i].IsNull;

                if FCurrentDataSet.Fields[i].DataType = ftOraBlob Then // jn 09/23/2011
                begin
                   InsertSQL := InsertSQL + 'EMPTY_BLOB() ' + ',';
                   FInsertSQL := FInsertSQL + 'EMPTY_BLOB() ' + ',';
                end
                else
                if FCurrentDataSet.Fields[i].DataType = ftOraClob Then // jn 09/26/2011
                begin
                   InsertSQL := InsertSQL + 'EMPTY_CLOB() ' + ',';
                   FInsertSQL := FInsertSQL + 'EMPTY_CLOB() ' + ',';
                end
                else
                begin
                   InsertSQL := InsertSQL + ':' + FCurrentDataSet.Fields[i].FullName + ','; // 1/9/2004 - sm Removed FormatParamName()
                   FInsertSQL := FInsertSQL + Quote(FCurrentDataSet.Fields[i])
                     + FCurrentDataSet.Fields[i].AsString
                     + Quote(FCurrentDataSet.Fields[i])
                     + ',';
                end;
              end;
            end;
            if modcnt > 0 then
            begin
              if OraLobParams.Count = 0 Then // jn 09/23/2011
              begin
                SQL := copy(SQL, 1, Length(SQL) - 1);
                InsertSQL := copy(InsertSQL, 1, Length(InsertSQL) - 1); // Remove the last ,
                FInsertSQL := copy(FInsertSQL, 1, Length(FInsertSQL) - 1);

                InsertSQL := InsertSQL + ')';
                FInsertSQL := FInsertSQL + ')';

                FSQLText := SQL + ')' + FInsertSQL;
                SQL := SQL + ') ' + InsertSQL;
              end
              else
              begin
                SQL := copy(SQL, 1, Length(SQL) - 1);
                InsertSQL := copy(InsertSQL, 1, Length(InsertSQL) - 1); // Remove the last ,
                FInsertSQL := copy(FInsertSQL, 1, Length(FInsertSQL) - 1);

                InsertSQL := InsertSQL + ')';
                FInsertSQL := FInsertSQL + ')';

                FSQLText := SQL + ')' + FInsertSQL;
                SQL := SQL + ') ' + InsertSQL;

                SQL := SQL + ' RETURNING ';
                FSQLText := FSQLText + ' RETURNING ';

                For i:=0 to OraLobParams.Count - 1 do
                Begin
                   SQL := SQL + OraLobParams.Items[i].Name + ',';
                   FSQLText := FSQLText + OraLobParams.Items[i].Name + ',';
                End;

                SQL := copy(SQL, 1, Length(SQL) - 1);                // Remove the last
                FSQLText := copy(FSQLText, 1, Length(FSQLText) - 1); // Remove the last

                SQL := SQL + ' INTO ';
                FSQLText := FSQLText + ' INTO ';

                For i:=0 to OraLobParams.Count - 1 do
                Begin
                   SQL := SQL +':' + FormatParamName(OraLobParams.Items[i].Name) + ',';
                   FSQLText := FSQLText + OraLobParams.Items[i].Value + ',';
                End;

                SQL := copy(SQL, 1, Length(SQL) - 1);                // Remove the last
                FSQLText := copy(FSQLText, 1, Length(FSQLText) - 1); // Remove the last

              end;
              InternalAddSQL(SQL, Params, dtAppend);
            end
            else
            begin
              FreeAndNil(Params);
              DatabaseError(SNoValuesToUpdate, Self);
            end;

            for i := 0 to FPrimeFields.Count - 1 do
            begin
              with RefetchParams.Add do
              begin
                Name:=FPrimeFields[i];
                DataType:=FCurrentDataSet.FieldbyName(FPrimeFields[i]).DataType;
                IsNull := FCurrentDataSet.FieldbyName(FPrimeFields[i]).IsNull;
                Value:=FCurrentDataSet.FieldbyName(FPrimeFields[i]).Value;
              end;
            end;
            PrimeFieldsValues.Add(RefetchParams.AsTokenizedString);
            RefetchParams.Clear;  // sm - 7/5/2003
            ProcessDetails;
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
    Except
      if Assigned(Params) then // sm - 12/15/2002
     FreeAndNil(Params);
     break;
    end;
    if Assigned(Params) then // sm - 12/15/2002
    Params.Clear; // sm - 10/1/2002 undone sg    FOldValuesDataSet.Next;  end;
    FOldValuesDataSet.Next;
  end;
  if Assigned(Params) then // sm - 12/15/2002
  Params.Free; // sm - 10/1/2002
  if Assigned(RefetchParams) then // sm - 12/15/2002
  FreeAndNil(RefetchParams);
  if Assigned(OraLobParams) then // jn - 09/22/2011
  FreeAndNil(OraLobParams);
end;

procedure TAstaIOSQLGenerator.MultiCreateSQL;
var
  UpdateTableList:TStringList;
  i,j, modcnt: Integer;
  Action: Integer;
  HotSpot: Integer;
  Params: TAstaParamList;
  SQL: string;
  FWhereSQL: string;
  InsertSQL: string;
  FInsertSQL: string;
  RefetchParams: TAstaParamList;

  function Quote(Field: TField): string;
  begin
    result := '';
    if Field.DataType in [ftString, ftDateTime, ftDate, ftTime, ftMemo] then Result := '''';
  end;

  Function PrimekeyFieldsTable:TStringList;
  begin
   result:=TStringList(UpdateTableList.Objects[j]);
  end;
   Function MultiFieldCheck(FieldName:String):Boolean;
   begin
    result:=MultiTableDataSet.Locate('FieldName',FieldName,[loCaseInsensitive])
    and (comparetext(MultiTableDataSet.FieldByName('UpdateTableName').AsString,UpdateTableList[j])=0);
   end;

  function WhereSQL: string;
  var
    i: Integer;
  begin
    Result := '';
    Result := ' WHERE 1 = 1';
    FWhereSQL := Result;
    case FUpdateMode of
      upWhereKeyOnly:
        begin
          for i := 0 to PrimekeyFieldsTable.Count - 1 do
          begin
            if FOldValuesDataSet.FieldbyName(PrimekeyFieldsTable[i]).IsNull and FUseNULLSyntax then
              Result := Result + ' AND ' + PrimekeyFieldsTable[i] + ' IS NULL'
            else
            begin
              Result := Result + ' AND ' + PrimekeyFieldsTable[i] + '=:' + FormatParamName(PrimekeyFieldsTable[i]);
              Params.FastAdd(FormatParamName(PrimekeyFieldsTable[i][i]), FOldValuesDataSet.FieldbyName(PrimekeyFieldsTable[i]).Value);
              Params[Params.Count - 1].DataType := FOldValuesDataSet.FieldbyName(PrimekeyFieldsTable[i]).DataType;
              Params[Params.Count - 1].IsNull := FOldValuesDataSet.FieldbyName(PrimekeyFieldsTable[i]).IsNull;

              with RefetchParams.Add do
              begin
                Name:=PrimekeyFieldsTable[i];
                DataType:=FOldValuesDataSet.FieldbyName(PrimekeyFieldsTable[i]).DataType;
                IsNull := FOldValuesDataSet.FieldbyName(PrimekeyFieldsTable[i]).IsNull;
                Value:=FOldValuesDataSet.FieldbyName(PrimekeyFieldsTable[i]).Value;
              end;
            end;
            FWhereSQL := FWhereSQL + ' AND ' + PrimekeyFieldsTable[i] + '=' + Quote(FOldValuesDataSet.FieldbyName(PrimekeyFieldsTable[i]))
              + FOldValuesDataSet.FieldbyName(PrimekeyFieldsTable[i]).AsString
              + Quote(FOldValuesDataSet.FieldbyName(PrimekeyFieldsTable[i]));
            {if i < FPrimeFields.Count - 1 then
            begin
              Result := Result + ' AND ';
              FWhereSQL := FWhereSQL + ' AND ';
            end;}
          end;
        end;

      upWhereAll:
        begin
          for i := 0 to FOldValuesDataSet.FieldCount - OldValsSpecFields - 1 do
          begin
            if (not MultiFieldCheck(FOldValuesDataSet.Fields[i].FieldName)) or (FOldValuesDataSet.Fields[i].DataType = ftDataSet) or (FOldValuesDataSet.Fields[i].FieldKind = fkInternalCalc) then continue;
            if FOldValuesDataSet.Fields[i].IsNull and FUseNULLSyntax then
              Result := Result + ' AND ' + FOldValuesDataSet.Fields[i].FullName + ' IS NULL'
            else
            begin
              Result := Result + ' AND ' + FOldValuesDataSet.Fields[i].FullName + '=:' + FormatParamName(FOldValuesDataSet.Fields[i].FullName);
              Params.FastAdd(FormatParamName(FOldValuesDataSet.Fields[i].FullName), FOldValuesDataSet.Fields[i].Value);
              Params[Params.Count - 1].DataType := FOldValuesDataSet.Fields[i].DataType;
              Params[Params.Count - 1].IsNull := FOldValuesDataSet.Fields[i].IsNull;

              with RefetchParams.Add do
              begin
                Name:=FOldValuesDataSet.Fields[i].FullName;
                DataType:=FOldValuesDataSet.Fields[i].DataType;
                IsNull := FOldValuesDataSet.Fields[i].IsNull;
                Value:=FOldValuesDataSet.Fields[i].Value;
              end;
            end;
            FWhereSQL := FWhereSQL + ' AND ' + FOldValuesDataSet.Fields[i].FullName + '=' + Quote(FOldValuesDataSet.Fields[i])
              + FOldValuesDataSet.Fields[i].AsString
              + Quote(FOldValuesDataSet.Fields[i]);
            {if i < FOldValuesDataSet.FieldCount - 4 then
            begin
              Result := Result + ' AND ';
              FWhereSQL := FWhereSQL + ' AND ';
            end;}
          end;
        end;

      upWhereChanged:
        begin
          for i := 0 to FCurrentDataSet.FieldCount - 1 do
          begin
            if (not MultiFieldcheck(FCurrentDataSet.Fields[i].FieldName)) or (FCurrentDataSet.Fields[i].DataType = ftDataSet) or (FCurrentDataSet.Fields[i].FieldKind = fkInternalCalc) then continue;
            if (FCurrentDataSet.Fields[i].AsString <> FOldValuesDataSet.FieldbyName(FCurrentDataSet.Fields[i].FieldName).AsString) then
            begin
              if FOldValuesDataSet.FieldbyName(FCurrentDataSet.Fields[i].FieldName).IsNull and FUseNULLSyntax then
                Result := Result + ' AND ' + FCurrentDataSet.Fields[i].FullName + ' IS NULL'
              else
              begin
                Result := Result + ' AND ' + FCurrentDataSet.Fields[i].FullName + '=:' + FormatParamName(FCurrentDataSet.Fields[i].FullName);
                Params.FastAdd(FormatParamName(FCurrentDataSet.Fields[i].FullName), FOldValuesDataSet.FieldbyName(FCurrentDataSet.Fields[i].FieldName).Value);
                Params[Params.Count - 1].DataType := FCurrentDataSet.Fields[i].DataType;
                Params[Params.Count - 1].IsNull := FOldValuesDataSet.FieldbyName(FCurrentDataSet.Fields[i].FieldName).IsNull;

                with RefetchParams.Add do
                begin
                  Name:=FCurrentDataSet.Fields[i].FullName;
                  DataType:=FCurrentDataSet.Fields[i].DataType;
                  IsNull := FCurrentDataSet.Fields[i].IsNull;
                  Value:=FCurrentDataSet.Fields[i].Value;
                end;
              end;
              FWhereSQL := FWhereSQL + ' AND ' + FCurrentDataSet.Fields[i].FullName + '=' + Quote(FOldValuesDataSet.FieldbyName(FCurrentDataSet.Fields[i].FieldName))
                + FOldValuesDataSet.FieldbyName(FCurrentDataSet.Fields[i].FieldName).AsString
                + Quote(FOldValuesDataSet.FieldbyName(FCurrentDataSet.Fields[i].FieldName));
            end;
          end;
          //Result := Copy(Result, 1, Length(Result) - 5); // Remove the last AND
          //FWhereSQL := Copy(FWhereSQL, 1, Length(FWhereSQL) - 5);
        end;
    end;
    PrimeFieldsValues.Add(RefetchParams.AsTokenizedString);
  end;
   function GetMultiUpdateTableNames:TStringList;
   var
   spot:Integer;
     procedure PrimekeyAdjust;
     begin
       if MultiTableDataSet.FieldByName('PrimeKey').AsBoolean
        then TStringList(result.Objects[spot]).Add(MultiTAbleDataSet.FieldbyName('FieldName').AsString);
     end;
   begin
    result:=TStringList.Create;
    MultiTableDataSet.first;
    while not MultiTableDataSet.Eof do begin
     if result.Indexof(MultiTableDataSet.FieldByname('UpdateTableName').AsString)<0
      then result.addobject(MultiTableDataSet.FieldByname('UpdateTableName').AsString,TStringList.Create);
     spot:=result.Indexof(MultiTableDataSet.FieldByname('UpdateTableName').AsString);
     PrimeKeyAdjust;
     MultiTableDatASet.Next
    end;
   end;


begin
  UpdateTableList:=GetMultiUpdateTableNames;
  try
  for j:=0 to UpdateTableList.count-1 do begin
  FOldValuesDataSet.First;
  RefetchParams:=TAstaParamList.Create;
  while not FOldValuesDataSet.Eof do
  begin
    Action := FOldValuesDataSet.FieldByName('Delta').AsInteger;
    HotSpot := FOldValuesDataSet.FieldByName('BookMark').AsInteger;
    Params := TAstaParamList.Create;
    try
    case Action of
      ord(dtEdit):
        begin
          // OldValues have corresponding CurrentValues
          if FCurrentDataSet.Locate('BookMark', HotSpot, []) then
          begin
            SQL := 'UPDATE ' + UpdateTableList[j] + ' SET ';
            FSQLText := SQL;
            modcnt := 0;
            for i := 0 to FCurrentDataSet.FieldCount - 1 do
            begin
              if (FCurrentDataSet.Fields[i].FieldKind = fkData) and MultiFieldCheck(FCurrentDataSet.Fields[i].FieldName)
               and
                 (FCurrentDataSet.Fields[i].AsString <> FOldValuesDataSet.FieldbyName(FCurrentDataSet.Fields[i].FieldName).AsString)
                and KosherForSQL(FCurrentDataSet.Fields[i]) then
              begin
                inc(modcnt);
                if FTrimStringFields and (FCurrentDataSet.Fields[i].DataType = ftString) then
                  Params.FastAdd(FCurrentDataSet.Fields[i].FullName, Trim(FCurrentDataSet.Fields[i].Value))
                else
                  Params.FastAdd(FCurrentDataSet.Fields[i].FullName, FCurrentDataSet.Fields[i].Value);
                Params[Params.Count - 1].DataType := FCurrentDataSet.Fields[i].DataType;
                if FTrimStringFields and (FCurrentDataSet.Fields[i].DataType = ftString) then
                  Params[Params.Count - 1].AsString := Trim(FCurrentDataSet.Fields[i].AsString)
                else
                  Params[Params.Count - 1].Value := FCurrentDataSet.Fields[i].Value;
                Params[Params.Count - 1].IsNull := FCurrentDataSet.Fields[i].IsNull;

                SQL := SQL + FCurrentDataSet.Fields[i].FullName
                  + '=:'
                  + FCurrentDataSet.Fields[i].FullName + ','; // 1/9/2004 - sm Removed FormatParamName()

                FSQLText := FSQLText + FCurrentDataSet.Fields[i].FullName
                  + '='
                  + Quote(FCurrentDataSet.Fields[i])
                  + FCurrentDataSet.Fields[i].AsString
                  + Quote(FCurrentDataSet.Fields[i]) + ',';
              end;
            end;
            if modcnt > 0 then
            begin
              SQL := copy(SQL, 1, Length(SQL) - 1) + WhereSQL;
              FSQLText := copy(FSQLText, 1, Length(FSQLText) - 1) + FWhereSQL;
              InternalAddSQL(SQL, Params, dtEdit);
            end
            else
            begin
              FreeAndNil(Params);
              //DatabaseError(SNoValuesToUpdate, Self);
            end;
          end;
        end;

      ord(dtDelete):
        begin
          // Only OldValues, no corresponding CurrentValues
          //we need to do something with deletes?
          SQL := 'DELETE FROM ' + UpdateTableList[j] + WhereSQL;
          FSQLText := 'DELETE FROM ' + UpdateTableList[j] + FWhereSQL;
          InternalAddSQL(SQL, Params, dtDelete);
        end;

      ord(dtAppend):
        begin
          // OldValues is NULL, BookMark points to corresponding CurrentValues
          if FCurrentDataSet.Locate('BookMark', HotSpot, []) then
          begin
            SQL := 'INSERT INTO ' + UpdateTableList[j] + '(';
            FSQLText := SQL;

            InsertSQL := ' VALUES (';
            FInsertSQL := ' VALUES (';
            modcnt := 0;
            for i := 0 to FCurrentDataSet.FieldCount - 2 do
            begin
              if not FCurrentDataSet.Fields[i].IsNull
                and KosherForSQL(FCurrentDataSet.Fields[i])
                and (FCurrentDataSet.Fields[i].FieldKind = fkData) then
              begin
                inc(modcnt);
                SQL := SQL + FCurrentDataSet.Fields[i].FullName + ',';
                if FTrimStringFields and (FCurrentDataSet.Fields[i].DataType = ftString) then
                  Params.FastAdd(FCurrentDataSet.Fields[i].FullName, Trim(FCurrentDataSet.Fields[i].Value))
                else
                  Params.FastAdd(FCurrentDataSet.Fields[i].FullName, FCurrentDataSet.Fields[i].Value);
                Params[Params.Count - 1].DataType := FCurrentDataSet.Fields[i].DataType;
                Params[Params.Count - 1].IsNull := FCurrentDataSet.Fields[i].IsNull;

                InsertSQL := InsertSQL + ':' + FCurrentDataSet.Fields[i].FullName + ','; // 1/9/2004 - sm Removed FormatParamName()
                FInsertSQL := FInsertSQL + Quote(FCurrentDataSet.Fields[i])
                  + FCurrentDataSet.Fields[i].AsString
                  + Quote(FCurrentDataSet.Fields[i])
                  + ',';
              end;
            end;
            if modcnt > 0 then
            begin
              SQL := copy(SQL, 1, Length(SQL) - 1);
              InsertSQL := copy(InsertSQL, 1, Length(InsertSQL) - 1); // Remove the last ,
              FInsertSQL := copy(FInsertSQL, 1, Length(FInsertSQL) - 1);

              InsertSQL := InsertSQL + ')';
              FInsertSQL := FInsertSQL + ')';

              FSQLText := SQL + ')' + FInsertSQL;
              SQL := SQL + ') ' + InsertSQL;

              InternalAddSQL(SQL, Params, dtAppend);
            end
            else
            begin
              FreeAndNil(Params);
              DatabaseError(SNoValuesToUpdate, Self);
            end;

            for i := 0 to PrimekeyFieldsTable.Count - 1 do
            begin
              with RefetchParams.Add do
              begin
                Name:=PrimekeyFieldsTable[i];
                DataType:=FCurrentDataSet.FieldbyName(PrimekeyFieldsTable[i]).DataType;
                IsNull := FCurrentDataSet.FieldbyName(PrimekeyFieldsTable[i]).IsNull;
                Value:=FCurrentDataSet.FieldbyName(PrimekeyFieldsTable[i]).Value;
              end;
            end;
            PrimeFieldsValues.Add(RefetchParams.AsTokenizedString);
          end;
        end;

      ord(dtAppendAndDelete):
        begin
          // OldValues is NULL, BookMark points to corresponding CurrentValues
          if FCurrentDataSet.Locate('BookMark', HotSpot, []) then
          begin

          end;
        end;
    end;//case
    Except
     FreeAndNil(Params);
     break;
    end;
    FOldValuesDataSet.Next;
  end;
  end;
  FreeAndNil(RefetchParams);
  Finally
   for j:=0 to UpdateTableList.count-1 do
    UpdateTAbleList.objects[j].Free;
   UpdateTableList.Free;
  end;
end;

function TAstaIOSQLGenerator.KosherForSQL(Field: TField): Boolean;
begin
  Result := True;
  if Field.FieldKind <> fkData then Result := False;
  if Field.DataType = ftDataSet then Result := False;
  if (Field.FullName = FAutoIncrementField) or (Field.FullName = FSequence) then Result := False;
  if result then result := FNoSQLFields.IndexOf(Field.FieldName) < 0;
end;

procedure TAstaIOSQLGenerator.UpdateFromClientDataSet(DataSet: TAstaCustomAuditDataSet);
begin
  inherited UpdateFromClientDataSet(DataSet);
  // changed by AI, 26 Nov 2001
  UpdateMode := TAstaCustomAuditDataSetHack(DataSet).UpdateMode;
end;

constructor TAstaIOSQLGenerator.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FUseNULLSyntax:=True;

end;

end.
