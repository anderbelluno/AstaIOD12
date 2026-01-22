{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10201: AstaIOKylixCompAbout.pas 
{
{   Rev 1.0    4/10/2003 6:31:18 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:37:32 PM  Steve    Version: 1.505
}
{*********************************************************}
{*   Copyright (c) 1997-2002 Asta Technology Group Inc.  *}
{*               All rights reserved.                    *}
{*               www.astatech.com                        *}
{*********************************************************}
unit AstaIOKylixCompAbout;

interface

uses Classes,
     QForms, QGraphics, QControls, QExtCtrls, QStdCtrls;

type
  Tf_kylixcompabout = class(TForm)
    Panel2: TPanel;
    btn_ok: TButton;
    Panel1: TPanel;
    Panel3: TPanel;
    Label1: TLabel;
    lab_comp_name: TLabel;
    lab_url: TLabel;
    Image1: TImage;
    lab_txt_comp_version: TLabel;
    lab_comp_version: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Panel4: TPanel;
    Panel5: TPanel;
    Panel7: TPanel;
    procedure btn_okClick(Sender: TObject);
    procedure lab_urlClick(Sender: TObject);
  private
  public
    constructor Create(AOwner: TComponent; CompName: string; Version: string);
  end;

var
  f_kylixcompabout: Tf_kylixcompabout;

implementation

{$R *.dfm}

constructor Tf_kylixcompabout.Create(AOwner: TComponent; CompName: string; Version: string);
begin
  inherited create(AOwner);
  lab_comp_name.caption := compname;
  lab_comp_version.caption := Version;
  Caption := 'About ' + compname;
end;

procedure Tf_kylixcompabout.btn_okClick(Sender: TObject);
begin
  close;
end;

procedure Tf_kylixcompabout.lab_urlClick(Sender: TObject);
begin
//  ShellExecute(TForm(Owner).Handle , 'open' , pchar(lab_url.caption) , '' , '' ,0 );
end;

end.

