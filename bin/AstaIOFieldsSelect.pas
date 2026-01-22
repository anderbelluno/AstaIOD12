{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10153: AstaIOFieldsSelect.pas 
{
{   Rev 1.0    4/10/2003 6:30:52 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:37:14 PM  Steve    Version: 1.505
}
unit AstaIOFieldsSelect;

interface

uses SysUtils, Classes,
     {$IFDEF LINUX}
     QForms, QControls, QStdCtrls, QCheckLst;
     {$ELSE}
     Forms, Controls, StdCtrls, Buttons, ExtCtrls, CheckLst;
     {$ENDIF}

type
  TAstaIOFieldsSelectDialog = class(TForm)
    OKBtn: TButton;
    CancelBtn: TButton;
    gb_fields: TGroupBox;
    clb_fields: TCheckListBox;
    procedure FormActivate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  AstaIOFieldsSelectDialog: TAstaIOFieldsSelectDialog;

implementation

{$R *.dfm}

procedure TAstaIOFieldsSelectDialog.FormActivate(Sender: TObject);
begin
  clb_fields.SetFocus;
end;

end.
