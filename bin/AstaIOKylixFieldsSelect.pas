{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10207: AstaIOKylixFieldsSelect.pas 
{
{   Rev 1.0    4/10/2003 6:31:20 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:37:34 PM  Steve    Version: 1.505
}
unit AstaIOKylixFieldsSelect;

interface

uses SysUtils, Classes,
     QForms, QControls, QStdCtrls, QCheckLst;

type
  TAstaIOKylixFieldsSelectDialog = class(TForm)
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
  AstaIOKylixFieldsSelectDialog: TAstaIOKylixFieldsSelectDialog;

implementation

{$R *.dfm}

procedure TAstaIOKylixFieldsSelectDialog.FormActivate(Sender: TObject);
begin
  clb_fields.SetFocus;
end;

end.
