{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10331: AstaIOStitchSocket.pas 
{
{   Rev 1.0    4/10/2003 6:32:22 AM  Steve
}
{
{   Rev 1.0    11/8/2002 9:47:54 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:38:20 PM  Steve    Version: 1.505
}
unit AstaIOStitchSocket;
//changed by DB 17.05.2001 (whole unit)
{$D+,L+,O-,Y+}

interface
{$I AstaIO.inc}

uses
  AstaIOWinBase,
  WinSock,
  SyncObjs;

type
  TStitchSocket = class(TAstaSocket)
  protected
  public
    function ReadString: AnsiString; override;
    procedure WriteString(S: AnsiString); override;
 end;

implementation

function TStitchSocket.ReadString: AnsiString;
begin
  Result := inherited ReadString;
end;

procedure TStitchSocket.WriteString(S: AnsiString);
begin
  inherited WriteString(S);
end;

end.



