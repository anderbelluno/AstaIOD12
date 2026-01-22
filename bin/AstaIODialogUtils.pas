{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10137: AstaIODialogUtils.pas 
{
{   Rev 1.0    4/10/2003 6:30:46 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:37:08 PM  Steve    Version: 1.505
}
unit AstaIODialogUtils;

interface

procedure Display(Msg :String);
implementation
uses
{$IFDEF LINUX}
  QDialogs
{$ELSE}
  Dialogs,Windows
{$ENDIF}
;

procedure Display(Msg :String);
begin
  {$IFDEF LINUX}
  MessageDlg(Msg, mtWarning, [mbOk], 0); // Don't know now what Info's code is
  {$ELSE}
  MessageBox(GetActiveWindow,PChar(Msg),'',MB_OK + MB_ICONINFORMATION);
  {$ENDIF}
end;

end.
 
