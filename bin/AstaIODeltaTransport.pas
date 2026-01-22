{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10133: AstaIODeltaTransport.pas 
{
{   Rev 1.0    4/10/2003 6:30:44 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:37:08 PM  Steve    Version: 1.505
}
unit AstaIODeltaTransport;
{*********************************************************}
{*   Copyright (c) 1997-2002 Asta Technology Group Inc.  *}
{*               All rights reserved.                    *}
{*               www.astatech.com                        *}
{*********************************************************}

(* This unit will provide tools to allow for the efficient marshalling of
OldValuesDataSets and CurrentValue DataSets so that SQL can always be generated
only on the server.

It will also contain maps to all master/detail relationships

*)

interface

uses AstaIOCustomDataSet,
  AstaIOParamList,
  AstaIODBConst;
type
  TAstaDeltaTransportType = class(TAstaParamList)
  private
    FMasterList: TAstaParamList;
    FSQLOptions:
  public
    procedure AddDataSet(TheDataSet, OldValueDataSet, CurrentValueDataset: TAstaAduitDataset);
    procedure MasterListCheck(TheDataSet: TAstaAduitDataset);
    procedure AdjustBlobsAndMemos; // only changed or new blobs memos need to streamed
                                 //for performance reasons
  end;

implementation

procedure TAstaDeltaTransporttype.MasterListCheck(TheDataSet: TAstaAduitDataset);
begin
  if TheDataSet.MasterSource = nil then exit;
  if TheDataSet.masterSource.DataSet = nil then exit;
  FMasterList.FastAdd(TheDataSet.Name, TheDataSet.MasterSource.DataSet.Name);
end;

procedure TAstaDeltaTransportType.AddDataSet(TheDataSet, OldValueDataSet, CurrentValueDataset: TAstaAduitDataset);
var
  p: TastaParamList;
begin
  p := TAstaParamList.Create;
  try
    MasterListCheck(TheDataSet);
    p.FastAdd('Name', TheDataSet.Name);
  //these datasets need to be adjusted so that they contain efficiently transportable fields
    p.FastAdd('OldValueDataset', DataSetToString(OldValueDataSet));
    p.FastAdd('CurrentValueDataset', DataSetToString(CurrentValueDataSet));
    p.FastAdd('Masterfields', TheDataSet.MasterFields);
    p.FastAdd('Detailfields', TheDataSet.DetailFields);
  //client side options
    if CurrentValueDataSet is TAstaCustomClientSQLDataSet then
    begin
      p.FastAdd(FAstaClientDataSet.UpdateTableName);
      p.FastAdd(FAstaClientDataSet.PrimeFields.Text);
      p.FastAdd(SQLOptionsToInteger(TAstaSelectSQLOptionSet(FAstaClientDataSet.SQLOptions)));
      p.FastAdd(FAstaClientDataSet.AutoIncrementField);
      p.FastAdd(FAstaClientDataSet.RefetchOnInsert.Text);
      p.FastAdd(ord(FAstaClientDataSet.UpdateMode));
      p.FastAdd(ReadOnlyFieldsAsString(D));
    end;
    FastAdd(TheDataSet.name, p.AsTokenizedString(False));
  finally
    p.free;
  end;
end;

end.

