{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10385: AstaWebCacheReg.pas 
{
{   Rev 1.0    4/10/2003 6:32:48 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:38:38 PM  Steve    Version: 1.505
}
 unit AstaWebCacheReg;
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
  AstaWebCacheCustomDataSet,
  AstaWebCacheDataSet,
  AstaWebCacheSQLDataSet;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('ASTA', [ TAstaWebCacheDataSet,TAstaWebCacheSQLDataSet]);
end;

end.



