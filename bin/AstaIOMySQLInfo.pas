{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10247: AstaIOMySQLInfo.pas 
{
{   Rev 1.0    4/10/2003 6:31:40 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:37:50 PM  Steve    Version: 1.505
}
unit AstaIOMySQLInfo;
{*********************************************************}
{*   Copyright (c) 1997-2002 Asta Technology Group Inc.  *}
{*               All rights reserved.                    *}
{*               www.astatech.com                        *}
{*********************************************************}

interface

uses
  Classes,
  DB,
  SysUtils,
  AstaIOBaseRdbmsInfo;

type
  TAstaIOMySQLInfo = class(TAstaIOBaseRdbmsInfo)
  private
  protected
    procedure GetTables; override;
    procedure GetTriggers(ObjectName: string = ''); override;
    procedure GetIndexes(ObjectName: string); override;
    procedure GetFields(ObjectName: string); override;
    procedure GetViews; override;
    procedure GetStoredProcs; override;
    procedure GetForeignKeys(ObjectName: string); override;
    procedure GetSystemTables; override;
    procedure GetPrimeKeys(ObjectName: string); override;
    procedure GetStoredProcColumns(ObjectName: string); override;
  public
  published
  end;

implementation
uses AstaIOUtil;
{ TAstaIOMySQLInfo }

procedure TAstaIOMySQLInfo.GetIndexes(ObjectName: string);
begin
  FSQL.Clear;
  FSQL.Add('Show Index from '+ObjectName);
end;

procedure TAstaIOMySQLInfo.GetFields(ObjectName: string);
begin
  FSQL.Clear;
  FSQL.Add('Show columns from '+ObjectName);
end;

procedure TAstaIOMySQLInfo.GetPrimeKeys(ObjectName: string);
begin
  FSQL.Clear;
  FSQL.Add('Show Keys from '+ObjectName);
end;

procedure TAstaIOMySQLInfo.GetForeignKeys(ObjectName: string);
begin
  FSQL.Clear;
end;

procedure TAstaIOMySQLInfo.GetStoredProcColumns(ObjectName: string);
begin
  FSQL.Clear;
end;

procedure TAstaIOMySQLInfo.GetStoredProcs;
begin
  FSQL.Clear;
end;

procedure TAstaIOMySQLInfo.GetSystemTables;
begin
  FSQL.Clear;
  FSQL.Add('show tables from mySQL');
end;

procedure TAstaIOMySQLInfo.GetTables;
begin
  FSQL.Clear;
  FSQL.Add('show tables');
end;

procedure TAstaIOMySQLInfo.GetTriggers(ObjectName: string);
begin
  FSQL.Clear;
end;

procedure TAstaIOMySQLInfo.GetViews;
begin
  FSQL.Clear;
end;

end.
