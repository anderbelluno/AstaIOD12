{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10203: AstaIOKylixCompUpdateSQLForm.pas 
{
{   Rev 1.0    4/10/2003 6:31:18 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:37:32 PM  Steve    Version: 1.505
}
unit AstaIOKylixCompUpdateSQLForm;
{*********************************************************}
{*   Copyright (c) 1997-2002 Asta Technology Group Inc.  *}
{*               All rights reserved.                    *}
{*               www.astatech.com                        *}
{*********************************************************}

{$I AstaIO.inc}

interface

uses
  Classes,
  AstaIOUpdateSQL,
  QForms, QDialogs, QImgList, QExtCtrls, QControls, QStdCtrls, QComCtrls;

type
  TAstaIOKylixCompUpdateSQLFrm = class(TForm)
    tlb_cttableview: TToolBar;
    btn_close: TToolButton;
    il_cttableview: TImageList;
    btn_refresh: TToolButton;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    pc_sql: TPageControl;
    ts_insert: TTabSheet;
    ts_update: TTabSheet;
    ts_delete: TTabSheet;
    m_insert: TMemo;
    m_update: TMemo;
    m_delete: TMemo;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    ToolButton1: TToolButton;
    btn_open: TToolButton;
    btn_save: TToolButton;
    ToolButton2: TToolButton;
    procedure btn_closeClick(Sender: TObject);
    procedure btn_refreshClick(Sender: TObject);
    procedure btn_openClick(Sender: TObject);
    procedure btn_saveClick(Sender: TObject);
  private
    { Private declarations }
  public
    UpdateObject    :TAstaIOUpdateSQL;
  end;

var
  AstaIOKylixCompUpdateSQLFrm: TAstaIOKylixCompUpdateSQLFrm;

implementation

{$R *.dfm}

procedure TAstaIOKylixCompUpdateSQLFrm.btn_closeClick(Sender: TObject);
begin
  Close;
end;

procedure TAstaIOKylixCompUpdateSQLFrm.btn_refreshClick(Sender: TObject);
begin
  if Assigned(UpdateObject) then
  begin
    UpdateObject.InsertSQL.Assign(m_insert.Lines);
    UpdateObject.ModifySQL.Assign(m_update.Lines);
    UpdateObject.DeleteSQL.Assign(m_delete.Lines);
  end;
end;

procedure TAstaIOKylixCompUpdateSQLFrm.btn_openClick(Sender: TObject);
begin
  if not OpenDialog.Execute then exit;
  if pc_sql.ActivePage = ts_insert then
    m_insert.Lines.LoadFromFile(OpenDialog.FIleName)
  else
  if pc_sql.ActivePage = ts_update then
    m_update.Lines.LoadFromFile(OpenDialog.FIleName)
  else
  if pc_sql.ActivePage = ts_delete then
    m_delete.Lines.LoadFromFile(OpenDialog.FIleName)
end;

procedure TAstaIOKylixCompUpdateSQLFrm.btn_saveClick(Sender: TObject);
begin
  if not SaveDialog.Execute then exit;
  if pc_sql.ActivePage = ts_insert then
    m_insert.Lines.SaveToFile(SaveDialog.FIleName)
  else
  if pc_sql.ActivePage = ts_update then
    m_update.Lines.SaveToFile(SaveDialog.FIleName)
  else
  if pc_sql.ActivePage = ts_delete then
    m_delete.Lines.SaveToFile(SaveDialog.FIleName)
end;

end.
