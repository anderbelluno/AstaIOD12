{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10091: AstaIOCompLoadForm.pas 
{
{   Rev 1.0    4/10/2003 6:30:22 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:36:50 PM  Steve    Version: 1.505
}
unit AstaIOCompLoadForm;
{*********************************************************}
{*   Copyright (c) 1997-2002 Asta Technology Group Inc.  *}
{*               All rights reserved.                    *}
{*               www.astatech.com                        *}
{*********************************************************}

{$I AstaIO.inc}

interface

uses
  DB, SysUtils, Classes
  {$IFNDEF LINUX}
  {$IFNDEF Delphi6AndUp}
  ,DsgnIntf
  {$ENDIF}
  ,Forms, StdCtrls, Buttons, Controls;
  {$ELSE}
  ,QForms, QStdCtrls, QControls, DesignEditors, DesignIntf;
  {$ENDIF}

type
  TAstaIOCompLoadFrm = class(TForm)
    gb_title: TGroupBox;
    DataSetList: TListBox;
    OkBtn: TButton;
    CancelBtn: TButton;
    HelpBtn: TButton;
    procedure HelpBtnClick(Sender: TObject);
    procedure OkBtnClick(Sender: TObject);
    procedure DataSetListDblClick(Sender: TObject);
  private
  public
    DataSetName :String;
  end;

implementation

{$R *.dfm}

procedure TAstaIOCompLoadFrm.HelpBtnClick(Sender: TObject);
begin
  {$IFDEF LINUX}
  {$ELSE}
  Application.HelpContext(HelpContext);
  {$ENDIF}
end;

procedure TAstaIOCompLoadFrm.OkBtnClick(Sender: TObject);
begin
  if DataSetList.ItemIndex < 0 then exit;

  DataSetName:=DataSetList.Items[DataSetList.ItemIndex];
end;

procedure TAstaIOCompLoadFrm.DataSetListDblClick(Sender: TObject);
begin
  OkBtn.Click;
end;

end.
