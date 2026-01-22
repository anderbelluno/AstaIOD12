{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10265: AstaIOParamsScrollBox.pas 
{
{   Rev 1.0    4/10/2003 6:31:50 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:37:56 PM  Steve    Version: 1.505
}
unit AstaIOParamsScrollBox;
{*********************************************************}
{*   Copyright (c) 1997-2002 Asta Technology Group Inc.  *}
{*               All rights reserved.                    *}
{*               www.astatech.com                        *}
{*********************************************************}

{$I AstaIO.inc}

interface

uses
{$IFDEF LINUX}
  QForms, QStdCtrls,  QControls,
{$ELSE}
  Forms, StdCtrls, Controls,
{$ENDIF}
  SysUtils, Classes, DB,
  AstaIOParamList,
  AstaIODBCOnst,
  AstaIOClientWire,
  AstaIOClientRemoteDataSet,
  AstaIOCustomDataSet;

type
  PExplParams = ^TExplParams;
  TExplParams = record
    ObjectName: string;
    Param: string[25];
    ParamType: TAstaParamType;
    DataType: TFieldType;
  end;

type
  TAstaIOParamsScrollBox = class(TScrollBox)
  private
    FLabelLeft: Integer;
    FFirstTop: Integer;
    FParmLeft: Integer;
    FYOffSet: Integer;
    FSelObjectType: TAstaMetaData;
    FObjectName: string;
    FParamDataSet: TAstaIOCustomDataset;
    FClientWire: TAstaClientWire;
    function GetParamCount: Integer;
  protected
    { Protected declarations }
  public
    ParamList: TAstaParamList;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ClearParams;
    procedure SetParamValue;
    property ParamCount: Integer read GetParamCount;
    property SelObjectType: TAstaMetaData read FSelObjectType write FSelObjectType;
    property ParamDataSet: TAstaIOCustomDataset read FParamDataSet;
    property ObjectName: string read FObjectName write FObjectName;
  published
    property LabelLeft: Integer read FLabelLeft write FLabelLeft;
    property ParmLeft: Integer read FParmLeft write FParmLeft;
    property FirstTop: Integer read FFirstTop write FFirstTop;
    property YOffSet: Integer read FYOffSet write FYOffSet;
    property ClientWire: TAstaClientWire read FClientWire write FClientWire;
  end;

implementation

{ TAstaIOParamsScrollBox }

constructor TAstaIOParamsScrollBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ParamList := TAstaParamList.Create;
  FLabelLeft := 10;
  FFirstTop := 10;
  FParmLeft := 150;
  FYOffSet := 27;
  FParamDataSet := TAstaIOCustomDataset.Create(Self);
  FParamDataSet.AddField('Param', ftString, 25);
  FParamDataSet.AddField('ParamType', ftString, 12);
  FParamDataSet.AddField('DataType', ftString, 15);
  FParamDataSet.AddField('Value', ftString, 100);
  ParamDataSet.Active := True;
end;

destructor TAstaIOParamsScrollBox.Destroy;
begin
  inherited;
  ParamList.Free;
end;

procedure TAstaIOParamsScrollBox.ClearParams;
var
  i: Integer;
begin
  for i := ParamList.Count - 1 downto 0 do
  begin
// comment can be removed with D5
//    ParamList.Delete(i);
  end;
  ParamList.Clear;

  for i := ComponentCount - 1 downto 0 do
    if Components[i].Tag in [88, 99] then
      Components[i].Free;
end;

function TAstaIOParamsScrollBox.GetParamCount: Integer;
begin
  Result := ParamList.Count;
end;

procedure TAstaIOParamsScrollBox.SetParamValue;
var
  i: Integer;
begin
  ParamDataSet.First;
  while not ParamDataSet.Eof do
  begin
    for i := 1 to ComponentCount - 1 do
    begin
      if Components[i].Tag = 88 then
      begin
        ParamDataSet.Locate('Param', Copy(TEdit(Components[i]).Name, 3, Length(TEdit(Components[i]).Name) - 2), []);
        ParamDataSet.Edit;
        ParamDataSet.FieldByName('Value').AsString := TEdit(Components[i]).Text;
        ParamDataSet.Post;
      end;
    end;
    ParamDataSet.Next;
  end;
end;

end.

