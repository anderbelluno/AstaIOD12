{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10027: AstaClientUpgradeForm.pas 
{
{   Rev 1.0    4/10/2003 6:29:54 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:36:28 PM  Steve    Version: 1.505
}
unit AstaClientUpgradeForm;
{*********************************************************}
{*     Copyright (c) 1997-99 Asta Technology Group LLC   *}
{*                 All rights reserved.                  *}
{*                 www.astatech.com                      *}
{*********************************************************}
{$I AstaIO.INC}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, Buttons, DBCtrls, Db, Grids, DBGrids, ComCtrls,
  ComCtrls;

type
  TClientUpdateForm = class(TForm)
    Label1: TLabel;
    Panel1: TPanel;
    Memo1: TMemo;
    PBar: TProgressBar;
    Panel2: TPanel;
    Label2: TLabel;
    procedure FormCreate(Sender: TObject);

  private
    StartTime : TTime;

  protected
  public
  end;

procedure CreateClientUpdateForm;
procedure CloseClientUpdateForm;
procedure UpdateAutoClientProgBar(CurReceived: Integer);
procedure SetUpdateClientExpectedMessageSize(ExpectedMsgSize: Integer);

Var
  ClientUpdateForm: TClientUpdateForm;

implementation

Uses
  AstaResourceString;

{$R *.DFM}

procedure CloseClientUpdateForm;
begin
with ClientUpdateForm do
  begin
  Close;
  end;
end;

procedure CreateClientUpdateForm;
begin
ClientUpdateForm :=TClientUpdateForm.Create(Application);

with ClientUpdateForm do
  begin
  StartTime := Now;

  Show;  // can't use ShowModal, need input from the App
//  ClientUpdateForm.Refresh;  // This must be here or the screen doesn't paint completely!
  end;
end;

procedure SetUpdateClientExpectedMessageSize(ExpectedMsgSize: Integer);
begin
with ClientUpdateForm do
  begin
  PBar.Position := 0;
  PBar.Max := ExpectedMsgSize;

  Label2.Caption := arsReceivingUpdateMsg;
  end;
end;

procedure UpdateAutoClientProgBar(CurReceived: Integer);
var
  hh,mm,ss,msec : word;

begin
with ClientUpdateForm do
  begin
  DecodeTime (Now-StartTime,hh,mm,ss,msec);

  if mm = 0 then
    Caption := Format('Update of '+ExtractFileName(ParamStr(0))+' in progress . . . %d Secs',[ss])
  else
    Caption := Format('Update of '+ExtractFileName(ParamStr(0))+' in progress . . . %d Min %d Secs',[mm,ss]);

  Label2.Caption := Format(' Received %d of %d bytes',[PBar.Position,PBar.Max]);

  PBar.Position := CurReceived;

  Refresh;
  end;
end;

procedure TClientUpdateForm.FormCreate(Sender: TObject);
{Var
  S: String;}
begin

// S was moved to AstaResourceString as arsClientAutoUpdateMessage
{  S:= 'This software program must be updated.' + ^M^J + ^M^J +

  'You are not required to take any action.  This program has an intelligent update feature and it knows how to update itself.  Please wait while the update progresses.' + ^M^J + ^M^J +

  'When the updated program has been received by your machine, it will start automatically and you can continue with your work.  Thank you.';

}
  Memo1.Lines.Text :=  arsClientAutoUpdateMsg;
  Label2.Caption   :=  arsWaitingForServerMsg;
end;


end.



