{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10367: AstaIOWireLogin.pas 
{
{   Rev 1.0    4/10/2003 6:32:38 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:38:32 PM  Steve    Version: 1.505
}
unit AstaIOWireLogin;
{*********************************************************}
{*   Copyright (c) 1997-2002 Asta Technology Group Inc.  *}
{*               All rights reserved.                    *}
{*               www.astatech.com                        *}
{*********************************************************}

{$I AstaIO.inc}

interface

uses SysUtils, Classes,
  Windows, Graphics, Forms, Controls, StdCtrls,
  Buttons, ExtCtrls;

type
  TAstaIOPasswordDialog = class(TForm)
    OKBtn: TButton;
    CancelBtn: TButton;
    Panel2: TPanel;
    Label3: TLabel;
    Label4: TLabel;
    eUserName: TEdit;
    ePassword: TEdit;
    Panel1: TPanel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

function GetClientLogin(var UserName, Password: string): Boolean;

implementation

{$R *.dfm}

function GetClientLogin(var UserName, Password: string): Boolean;
var
  PasswordDlg: TAstaIOPasswordDialog;
begin
  Result := False;
  PasswordDlg := TAstaIOPasswordDialog.Create(nil);
  try
    PasswordDlg.ShowModal;
    if PasswordDlg.ModalResult = mrOK then
    begin
      result := True;
      UserName := PasswordDlg.eUserName.Text;
      Password := PasswordDlg.ePassword.Text;
    end
  finally
    PasswordDlg.Free;
  end;
end;

end.

