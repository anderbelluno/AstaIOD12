{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10349: AstaIOUpdateSQL.pas 
{
{   Rev 1.0    4/10/2003 6:32:30 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:38:26 PM  Steve    Version: 1.505
}
unit AstaIOUpdateSQL;
{*********************************************************}
{*   Copyright (c) 1997-2002 Asta Technology Group Inc.  *}
{*               All rights reserved.                    *}
{*               www.astatech.com                        *}
{*********************************************************}
{$I AstaIO.inc}

interface

uses SysUtils, Classes, DB,
  AstaIOUpdateObjectBase,
  AstaIOCustomDataSet,
  AstaIOParamList,
  AstaIODBConst,
  AstaIOSQLUtils;

type

  TAstaIOUpdateSQL = class(TAstaIOUpdateObjectBase)
  private
    FDeleteSQL: TStrings;
    FInsertSQL: TStrings;
    FModifySQL: TStrings;
    FParams: TParams;

    procedure SetDeleteSql(Value :TStrings);
    procedure SetInsertSql(Value :TStrings);
    procedure SetModifySql(Value :TStrings);
  protected
    procedure ParseParams(TheParseSQL :String);
    procedure CreateSQL; override;
  public
    constructor Create(AOwner:TComponent); override;
    destructor Destroy; override;

    function GetSQL(UpdateKind :TUpdateKind) :TStrings;
  published
    property DeleteSQL :TStrings read FDeleteSql write SetDeleteSql;
    property InsertSQL :TStrings read FInsertSql write SetInsertSql;
    property ModifySQL :TStrings read FModifySql write SetModifySql;
  end;

implementation
uses {AstaIOClientRemoteDataSet,}
     AstaIOResources,
     AstaIOConst;

{ TAstaIOUpdateSQL }

constructor TAstaIOUpdateSQL.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDeleteSQL:=TStringList.Create;
  FInsertSQL:=TStringList.Create;
  FModifySQL:=TStringList.Create;

  FParams:=TParams.Create;
end;

destructor TAstaIOUpdateSQL.Destroy;
begin
  FParams.Free;

  FDeleteSQL.Free;
  FInsertSQL.Free;
  FModifySQL.Free;

  inherited Destroy;
end;

procedure TAstaIOUpdateSQL.CreateSQL;
var
  i: Integer;
  Action: Integer;
  HotSpot: Integer;
  AstaParams: TAstaParamList;
  SQL: String;
  ParseSQL: String;
  DataType: TFieldType;
  IsNull: Boolean;
  EditAction: TDeltaType;
  Found: Boolean;
begin
  FOldValuesDataSet.First;
  while not FOldValuesDataSet.Eof do
  begin
    Action := FOldValuesDataSet.FieldByName(sfld_Delta).AsInteger;
    HotSpot := FOldValuesDataSet.FieldByName(sfld_BookMark).AsInteger;
    AstaParams := TAstaParamList.Create;
    EditAction:=dtEdit; // Just to prevent compiler warnings
    case Action of
      ord(dtEdit):
        begin
          SQL:=FModifySQL.Text;
          ParseSQL:=FModifySQL.Text;
          ParseParams(ParseSQL);
          EditAction:=dtEdit;
        end;
      ord(dtDelete):
        begin
          SQL:=FDeleteSQL.Text;
          ParseSQL:=FDeleteSQL.Text;
          ParseParams(ParseSQL);
          EditAction:=dtDelete;
        end;
      ord(dtAppend):
        begin
          SQL:=FInsertSQL.Text;
          ParseSQL:=FInsertSQL.Text;
          ParseParams(ParseSQL);
          EditAction:=dtAppend;
        end;
    end;

    Found:=False;
    if EditAction <> dtDelete then
    begin
       if FCurrentDataSet.Locate(sfld_BookMark, HotSpot, []) then
        Found:=True;
    end
    else
    begin
      Found:=True;
    end;
      Found:=True;

    if Found then
    begin
      for i:=0 to FParams.Count - 1 do
      begin
        if CompareText(Copy(FParams[i].Name, 1, 4 ), 'OLD_') = 0 then
        begin
          FParams[i].Value:=FOldValuesDataSet.FieldByName(Copy(FParams[i].Name, 5, Length(FParams[i].Name)-4)).Value;
          DataType := FOldValuesDataSet.FieldByName(Copy(FParams[i].Name, 5, Length(FParams[i].Name)-4)).DataType;
          IsNull := FOldValuesDataSet.FieldByName(Copy(FParams[i].Name, 5, Length(FParams[i].Name)-4)).IsNull;
        end
        else
        begin
          if EditAction in [dtEdit, dtAppend] then
          begin
            FParams[i].Value:=FCurrentDataSet.FieldByName(FParams[i].Name).Value;
            DataType := FCurrentDataSet.FieldByName(FParams[i].Name).DataType;
            IsNull := FCurrentDataSet.FieldByName(FParams[i].Name).IsNull;
          end
          else
          begin
            FParams[i].Value:=FOldValuesDataSet.FieldByName(FParams[i].Name).Value;
            DataType := FOldValuesDataSet.FieldByName(FParams[i].Name).DataType;
            IsNull := FOldValuesDataSet.FieldByName(FParams[i].Name).IsNull;
          end;
        end;

        With AstaParams.Add do
        begin
          Name:=FParams[i].Name;
          Value:=FParams[i].Value;
        end;
        //AstaParams.FastAdd(FParams[i].Name, FParams[i].Value);
        AstaParams[AstaParams.Count - 1].DataType := DataType;
        AstaParams[AstaParams.Count - 1].IsNull := IsNull;
      end;
      InternalAddSQL(SQL, AstaParams, EditAction);
    end;

    FOldValuesDataSet.Next;
  end;
end;

function TAstaIOUpdateSQL.GetSQL(UpdateKind: TUpdateKind): TStrings;
begin
  Result:=nil;
  case UpdateKind of
    ukModify:Result:=FModifySQL;
    ukInsert:Result:=FInsertSQL;
    ukDelete:Result:=FDeleteSQL;
  end;
end;

procedure TAstaIOUpdateSQL.ParseParams(TheParseSQL :String);
var List   :TParams;
    i      :Integer;
    FText  :String;
begin
  List:=TParams.Create(Self);
  try
    FText:=List.ParseSQL(TheParseSQL, True);
    List.AssignValues(FParams);
    FParams.Clear;
    FParams.Assign(List);
  finally
    List.Free;
  end;
  for i:=0 to FParams.Count-1 do
    FParams[i].ParamType:=DB.ptInput;
end;

procedure TAstaIOUpdateSQL.SetDeleteSql(Value: TStrings);
begin
  FDeleteSQL.Assign(Value);
end;

procedure TAstaIOUpdateSQL.SetInsertSql(Value: TStrings);
begin
  FInsertSQL.Assign(Value);
end;

procedure TAstaIOUpdateSQL.SetModifySql(Value: TStrings);
begin
  FModifySQL.Assign(Value);
end;

end.
