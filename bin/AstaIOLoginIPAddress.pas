{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10225: AstaIOLoginIPAddress.pas 
{
{   Rev 1.0    4/10/2003 6:31:28 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:37:42 PM  Steve    Version: 1.505
}
unit AstaIOLoginIPAddress;

interface

uses Windows, SysUtils, Classes, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls, Dialogs;

type
  TTAstaIOLoginAddressDialog = class(TForm)
    OKBtn: TButton;
    CancelBtn: TButton;
    Bevel1: TBevel;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    EditUserName: TEdit;
    EditPassword: TEdit;
    EditPort: TEdit;
    saveCheckBox: TCheckBox;
    EditServer: TComboBox;
    procedure OKBtnClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  TAstaIOLoginAddressDialog: TTAstaIOLoginAddressDialog;

implementation

{$R *.dfm}

uses AstaIOResources;

procedure TTAstaIOLoginAddressDialog.OKBtnClick(Sender: TObject);
begin
  if EditUserName.Text='' then
  begin
    ShowMessage(SNoLoginUserName);
    EditUserName.SetFocus;
  end else
    ModalResult := mrOk;
end;

end.

