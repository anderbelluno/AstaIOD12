{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10089: AstaIOCompEdt.pas 
{
{   Rev 1.0    4/10/2003 6:30:22 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:36:50 PM  Steve    Version: 1.505
}
unit AstaIOCompEdt;

{*********************************************************}
{*   Copyright (c) 1997-2002 Asta Technology Group Inc.  *}
{*               All rights reserved.                    *}
{*               www.astatech.com                        *}
{*********************************************************}

{$I AstaIO.inc}

interface
uses Classes, Controls,
     Dialogs,
     {$IFNDEF LINUX}
     {$IFNDEF BCB}
     DBReg,
     {$ENDIF}
     {$ENDIF}
     {$IFNDEF Delphi6AndUP}
     DsgnIntf,
     {$ELSE}
     DesignEditors, DesignIntf,
     {$ENDIF}
     Windows,
     SysUtils,
     Forms,
     DB, DBCtrls;

type

{$IFNDEF BCB}
  TAstaIOCustomDatasetEditor = class(TDatasetEditor)
    function GetVerbCount :Integer; override;
    function GetVerb(Index :Integer) :String; override;
    procedure ExecuteVerb(Index :Integer); override;
  end;
{$ENDIF}

  TAstaIOUpdateSQLEditor = class(TComponentEditor)
    function GetVerbCount :Integer; override;
    function GetVerb(Index :Integer) :String; override;
    procedure ExecuteVerb(Index :Integer); override;
  end;

  procedure Register;

implementation
uses AstaIOCustomDataSet,
     {$IFDEF LINUX}
     AstaIOKylixCompUpdateSQLForm,
     {$ELSE}
     AstaIOCompUpdateSQLForm,
     {$ENDIF}
     AstaIOCompLoadForm,
     AstaIOUpdateSQL;

procedure Register;
begin
  {$IFNDEF BCB}
  RegisterComponentEditor(TAstaIOCustomDataset, TAstaIOCustomDatasetEditor);
  {$ENDIF}
  RegisterComponentEditor(TAstaIOUpdateSQL, TAstaIOUpdateSQLEditor);
end;

{ TAstaIOCustomDatasetEditor }
{$IFNDEF BCB}
procedure TAstaIOCustomDatasetEditor.ExecuteVerb(Index: Integer);
var Dialog             :TOpenDialog;
    AstaIOCompLoadFrm  :TAstaIOCompLoadFrm;
    i                  :Integer;
    IsSourceOpen       :Boolean;
    SourceDataSet      :TDataSet;
    Bm                 :TBookmark;

begin
  case Index of
    2,
    3:begin
        if (Index = 2) and ((Component as TAstaIOCustomDataset).FieldDefs.Count = 0) then exit;
        Dialog:=TOpenDialog.Create(nil);
        try
          Dialog.Title:='Open file';
          Dialog.Filter:='Asta DataSet Files (*.ctd)|*.ctd|Text files (*.txt)|*.txt|All files (*.*)|*.*';
          Dialog.FilterIndex:=1;
          if not Dialog.Execute then exit;
          case Index of
            2:(Component as TAstaIOCustomDataset).LoadFromFile(Dialog.FileName);
            3:(Component as TAstaIOCustomDataset).LoadFromFileWithFields(Dialog.FileName);
          end;
        finally
          Dialog.Free;
        end;
      end;
    4:begin
        AstaIOCompLoadFrm:=TAstaIOCompLoadFrm.Create(nil);
        AstaIOCompLoadFrm.gb_title.Caption:=' Assign Data From ';
        AstaIOCompLoadFrm.Caption:='DataSet : ' + Component.Name;
        for i:=0 to Screen.ActiveForm.ComponentCount-1 do
        begin
          if (Screen.ActiveForm.Components[i] is TDataSet) and not (Screen.ActiveForm.Components[i] = (Component as TAstaIOCustomDataset)) then
            AstaIOCompLoadFrm.DataSetList.Items.Add(Screen.ActiveForm.Components[i].Name);
        end;

        try
          if AstaIOCompLoadFrm.ShowModal = mrOk then
          begin
            SourceDataSet:=TDataSet(Screen.ActiveForm.FindComponent(AstaIOCompLoadFrm.DataSetName));
            IsSourceOpen:=SourceDataSet.Active;
            if not IsSourceOpen then
              SourceDataSet.Open;
            Bm:=SourceDataSet.Bookmark;
            try
              SourceDataSet.First;
              (Component as TAstaIOCustomDataset).CleanCloneFromDataSet(SourceDataSet);
            finally
              SourceDataSet.Bookmark:=Bm;
            end;
            if not IsSourceOpen then
              SourceDataSet.Close;
          end;
        finally
          AstaIOCompLoadFrm.Release;
        end;
      end;
    5:begin
        AstaIOCompLoadFrm:=TAstaIOCompLoadFrm.Create(nil);
        AstaIOCompLoadFrm.gb_title.Caption:=' Assign Fields From ';
        AstaIOCompLoadFrm.Caption:='DataSet : ' + Component.Name;
        for i:=0 to Screen.ActiveForm.ComponentCount-1 do
        begin
          if (Screen.ActiveForm.Components[i] is TDataSet) and not (Screen.ActiveForm.Components[i] = (Component as TAstaIOCustomDataset)) then
            AstaIOCompLoadFrm.DataSetList.Items.Add(Screen.ActiveForm.Components[i].Name);
        end;

        try
          if AstaIOCompLoadFrm.ShowModal = mrOk then
          begin
            SourceDataSet:=TDataSet(Screen.ActiveForm.FindComponent(AstaIOCompLoadFrm.DataSetName));
            IsSourceOpen:=SourceDataSet.Active;
            if (Component as TAstaIOCustomDataset).Active then
              (Component as TAstaIOCustomDataset).Close;
            (Component as TAstaIOCustomDataset).Empty;
            if not IsSourceOpen then
              SourceDataSet.Open;

            (Component as TAstaIOCustomDataset).CloneFieldsFromDataSet(SourceDataSet, False, True);
          end;
        finally
          AstaIOCompLoadFrm.Release;
        end;
      end;
    else
      inherited ExecuteVerb(Index);
  end;
end;

function TAstaIOCustomDatasetEditor.GetVerb(Index: Integer): String;
begin
  case Index of
    1:Result:='-';
    2:Result:='Load fro&m file...';
    3:Result:='Load from file with fields...';
    4:Result:='Assign Local Data...';
    5:Result:='Clone Local Fields...';
   else Result:=inherited GetVerb(Index);
  end;
end;

function TAstaIOCustomDatasetEditor.GetVerbCount: Integer;
begin
  Result:=inherited GetVerbCount + 5;
end;
{$endif}

{ TAstaIOUpdateSQLEditor }

procedure TAstaIOUpdateSQLEditor.ExecuteVerb(Index: Integer);
{$IFDEF LINUX}
var AstaIOCompUpdateSQLFrm: TAstaIOKylixCompUpdateSQLFrm;
{$ELSE}
var AstaIOCompUpdateSQLFrm: TAstaIOCompUpdateSQLFrm;
{$ENDIF}
begin
  case Index of
    0:begin
        {$IFDEF LINUX}
        AstaIOCompUpdateSQLFrm:=TAstaIOKylixCompUpdateSQLFrm.Create(nil);
        {$ELSE}
        AstaIOCompUpdateSQLFrm:=TAstaIOCompUpdateSQLFrm.Create(nil);
        {$ENDIF}
        try
          AstaIOCompUpdateSQLFrm.UpdateObject:=(Component as TAstaIOUpdateSQL);
          AstaIOCompUpdateSQLFrm.Caption:='SQL of ' + Component.Name;
          AstaIOCompUpdateSQLFrm.m_update.Lines.Assign((Component as TAstaIOUpdateSQL).ModifySQL);
          AstaIOCompUpdateSQLFrm.m_insert.Lines.Assign((Component as TAstaIOUpdateSQL).InsertSQL);
          AstaIOCompUpdateSQLFrm.m_delete.Lines.Assign((Component as TAstaIOUpdateSQL).DeleteSQL);
          AstaIOCompUpdateSQLFrm.ShowModal;
          Self.Designer.Modified;
        finally
          AstaIOCompUpdateSQLFrm.Release;
        end;
      end;
    else
      inherited ExecuteVerb(Index);
  end;
end;

function TAstaIOUpdateSQLEditor.GetVerb(Index: Integer): String;
begin
  case Index of
    0:Result:='&SQL...';
  end;
end;

function TAstaIOUpdateSQLEditor.GetVerbCount: Integer;
begin
  Result:=1;
end;

end.
