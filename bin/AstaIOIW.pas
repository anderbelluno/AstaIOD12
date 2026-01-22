{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10195: AstaIOIW.pas 
{
{   Rev 1.0    4/10/2003 6:31:14 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:37:30 PM  Steve    Version: 1.505
}
unit AstaIOIW;
{*********************************************************}
{*   Copyright (c) 1997-2002 Asta Technology Group Inc.  *}
{*               All rights reserved.                    *}
{*               www.astatech.com                        *}
{*********************************************************}

{$I AstaIO.inc}

interface
uses
  Classes,
  DB,
  SysUtils,
  IWAstaCustomDataSet,
  IWAstaSQLDataSet;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('IntraWeb ASTA', [ TIWAstaSQLDataSet]);
end;

end.



