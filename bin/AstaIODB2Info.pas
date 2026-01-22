{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10123: AstaIODB2Info.pas 
{
{   Rev 1.0    4/10/2003 6:30:40 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:37:04 PM  Steve    Version: 1.505
}
unit AstaIODB2Info;
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
  TAstaIODB2Info = class(TAstaIOBaseRdbmsInfo)
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
{ TAstaIODB2Info }

procedure TAstaIODB2Info.GetIndexes(ObjectName: string);
begin
  FSQL.Clear;
  FSQL.Add('Show Index from '+ObjectName);
end;

procedure TAstaIODB2Info.GetFields(ObjectName: string);
begin
  FSQL.Clear;
  FSQL.Add('Show columns from '+ObjectName);
end;

procedure TAstaIODB2Info.GetPrimeKeys(ObjectName: string);
begin
  FSQL.Clear;
  FSQL.Add('Show Keys from '+ObjectName);
end;

procedure TAstaIODB2Info.GetForeignKeys(ObjectName: string);
begin
  FSQL.Clear;
end;

procedure TAstaIODB2Info.GetStoredProcColumns(ObjectName: string);
begin
  FSQL.Clear;
end;

procedure TAstaIODB2Info.GetStoredProcs;
begin
  FSQL.Clear;
end;

procedure TAstaIODB2Info.GetSystemTables;
begin
  FSQL.Clear;
  FSQL.Add('show tables from mySQL');
end;

procedure TAstaIODB2Info.GetTables;
begin
  FSQL.Clear;
  FSQL.Add('show tables');
end;

procedure TAstaIODB2Info.GetTriggers(ObjectName: string);
begin
  FSQL.Clear;
end;

procedure TAstaIODB2Info.GetViews;
begin
  FSQL.Clear;
end;

end.
