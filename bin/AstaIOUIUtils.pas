{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10345: AstaIOUIUtils.pas 
{
{   Rev 1.0    4/10/2003 6:32:28 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:38:24 PM  Steve    Version: 1.505
}
unit AstaIOUIUtils;

interface
uses
{$ifdef Linux}
 QForms,QControls,QDialogs
 {$else}
 Forms,Controls, Windows, Dialogs
{$endif};

procedure Display(Msg :String);
function GetTheParentForm(Control: TControl): TForm;
function MessageboxYes(const Msg: string): Boolean;
implementation
procedure Display(Msg :String);
begin
  {$IFDEF LINUX}
  MessageDlg(Msg, mtWarning, [mbOk], 0); // Don't know now what Info's code is
  {$ELSE}
  MessageBox(GetActiveWindow,PChar(Msg),'',MB_OK + MB_ICONINFORMATION);
  {$ENDIF}
end;
function GetTheParentForm(Control: TControl): TForm;
var
  TheParent: TControl;
begin
  TheParent := Control.Parent;
  while Assigned(TheParent) and (not (TheParent is TCustomForm)) do
    TheParent := TheParent.Parent;
  Result := TForm(TheParent);
end;
function MessageboxYes(const Msg: string): Boolean;
begin
{$IFDEF LINUX}
  Result := MessageDlg(Msg, mtInformation, [mbYes, mbNo], 0) = mrYes;
{$ELSE}
  Result := MessageDlg(Msg, mtConfirmation, [mbYes, mbNo], 0) = mrYes;
{$ENDIF}
end;

end.
