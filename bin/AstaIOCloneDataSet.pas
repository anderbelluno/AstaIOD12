{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10085: AstaIOCloneDataSet.pas 
{
{   Rev 1.0    4/10/2003 6:30:20 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:36:48 PM  Steve    Version: 1.505
}
unit AstaIOCloneDataSet;
{*********************************************************}
{*   Copyright (c) 1997-2002 Asta Technology Group Inc.  *}
{*               All rights reserved.                    *}
{*               www.astatech.com                        *}
{*********************************************************}

{$I AstaIO.inc}
interface

uses SysUtils, Classes, DB, AstaIOCustomDataSet, AstaIOSQLDataSet, AstaIODBConst;

type
  TAstaIOCloneDataSet = class(TAstaIOSQLDataSet)
  private
    { Private declarations }
    FCloned, FStreamedCloned: Boolean;
    FAddData: Boolean;
    FCloneDataSet: TDataSet;
    FFieldsToSkip:TStrings;
    procedure SetFieldsToSkip(Value: TStrings);
    procedure SetCloneDataSet(Value: TDataSet);
    procedure SetCloned(Value: Boolean);
    procedure UpdateClone;
    procedure DoClone;
    procedure DoUnClone;
  protected
    { Protected declarations }
    procedure Loaded; override;
  public
    { Public declarations }
    constructor Create(AOwner: Tcomponent); override;
    destructor Destroy; override;
  published
    { Published declarations }
    property Cloned: Boolean read FCloned write SetCloned default false;
    property AddData: Boolean read FAddData write FAddData default false;
    property CloneDataSet: TDataSet read FCloneDataSet write SetCloneDataSet;
    property FieldsToSkip: TStrings read FFieldsToSkip write SetFieldsToSkip;
  end;

implementation

constructor TAstaIOCloneDataSet.Create(AOwner: Tcomponent);
begin
  inherited Create(AOwner);
  FCloned := False;
  FAddData := False;
  FFieldsToSkip := TStringList.Create;
end;

destructor TAstaIOCloneDataSet.Destroy;
begin
  FFieldsToSkip.Free;
  inherited Destroy;
end;

procedure TAstaIOCloneDataSet.SetFieldsToSkip(Value: TStrings);
begin
  FFieldsToSkip.Assign(Value);
end;

procedure TAstaIOCloneDataSet.SetCloneDataSet(Value: TDataSet);
begin
  if (Value <> Self) and (FCloneDataSet <> Value) then begin
    FCloneDataSet := Value;
    if not (csLoading in ComponentState) then
      UpdateClone;
  end;
end;

procedure TAstaIOCloneDataSet.SetCloned(Value: Boolean);
begin
  if (csLoading in ComponentState) then
    FStreamedCloned := Value
  else if Cloned <> Value then begin
    FCloned := Value;
    UpdateClone;
  end;
end;

procedure TAstaIOCloneDataSet.Loaded;
begin
  inherited Loaded;
  if FStreamedCloned then
    Cloned := True; 
end;

procedure TAstaIOCloneDataSet.UpdateClone;
begin
  if Cloned and (FCloneDataSet <> nil) then
    DoClone
  else
    DoUnClone;
end;

procedure TAstaIOCloneDataSet.DoClone;
var
  T: TAstaUpdateMethod;
begin
  T := UpdateMethod;
  UpdateMethod := umManual;
  try
    CleanCloneFromDataSet(FCloneDataset, AddData, False, FFieldsToSkip);
  finally
    UpdateMethod := T;
  end;
end;

procedure TAstaIOCloneDataSet.DoUnClone;
begin
end;

end.
