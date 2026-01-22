{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10227: AstaIOLoginUnit.pas 
{
{   Rev 1.0    4/10/2003 6:31:30 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:37:42 PM  Steve    Version: 1.505
}
unit AstaIOLoginUnit;

interface

uses
  SysUtils, Classes,
{$IFDEF LINUX}
  QDialogs, QForms, QStdCtrls, QControls, QExtCtrls

{$ELSE}
  Forms, Dialogs, StdCtrls, Controls, ExtCtrls, Windows
{$ENDIF}
;

type
  TAstaLoginDialog = class(TForm)
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
           
function AstaIOLogin(var UserName, Password, IPAddress: string; var Port: Word): Boolean;
implementation
uses AstaIOResources;

{$R *.dfm}

function AstaIOLogin(var UserName, Password, IPAddress: string; var Port: Word): Boolean;
var
  AstaIOLoginDialog: TAstaLoginDialog;
begin
  result := False;
  AstaIOLoginDialog := TAstaLoginDialog.Create(nil);
  try
    with AstaIOLoginDialog do
    begin
      EditUserName.Text := UserName;
      EditPassword.Text := Password;
      EditServer.Text := IPAddress;
      EditPort.text := IntToStr(Port);
      ShowModal;
      if ModalResult = mrok then
      begin
        result := True;
        UserName := EditUserName.Text;
        Password := EditPassWord.Text;
        IPAddress := EditServer.text;
        Port := StrToInt(EditPort.Text);
      end;
    end;
  finally
    AstaIOLoginDialog.Free;
  end;
end;

procedure TAstaLoginDialog.OKBtnClick(Sender: TObject);
begin
  if EditUserName.Text = '' then
  begin
{$IFDEF LINUX}
    MessageDlg(SEnterUserName, mtWarning, [mbOk], 0);
{$ELSE}
    MessageBox(GetActiveWindow, PChar(SEnterUserName), '', mb_ok + mb_iconerror);
{$ENDIF}
    EditUserName.SetFocus;
  end
  else
    ModalResult := mrOk;
end;

end.

