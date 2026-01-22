{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10245: AstaIOMSSqlInfo.pas 
{
{   Rev 1.0    4/10/2003 6:31:40 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:37:50 PM  Steve    Version: 1.505
}
unit AstaIOMSSqlInfo;
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
  TAstaIOMSSqlInfo = class(TAstaIOBaseRdbmsInfo)
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

{ TAstaIOASEInfo }

procedure TAstaIOMSSqlInfo.GetIndexes(ObjectName: string);
begin
  FSQL.Clear;
  FSQL.Add('SELECT CONVERT(VARCHAR(40), si.name)');
  FSQL.Add('FROM sysobjects so, sysindexes si');
  FSQL.Add('WHERE si.id = so.id');
  FSQL.Add('AND so.name = ''' + Trim(ObjectName) + '''');
  FSQL.Add('ORDER BY si.name');
end;

procedure TAstaIOMSSqlInfo.GetFields(ObjectName: string);
begin
  FSQL.Clear;
  FSQL.Add('SELECT column_name = CONVERT(VARCHAR(40), sc.name), type_name = CONVERT(VARCHAR(40), st.name), length = sc.length');
  FSQL.Add('FROM dbo.syscolumns sc, dbo.sysobjects so, dbo.systypes st');
  FSQL.Add('WHERE so.id = sc.id');
  FSQL.Add('AND sc.usertype = st.usertype');
  FSQL.Add('AND so.type != ''P''');
  FSQL.Add('AND so.name = ''' + Trim(ObjectName) + '''');
  FSQL.Add('ORDER BY sc.colid');
end;

procedure TAstaIOMSSqlInfo.GetPrimeKeys(ObjectName: string);
begin
  FSQL.Clear;
  FSQL.Add('SELECT CONVERT(VARCHAR(40), sc.name)');
  FSQL.Add('FROM syscolumns sc, sysobjects so, sysindexkeys sik');
  FSQL.Add('WHERE sik.id = so.id');
  FSQL.Add('AND sc.id = so.id');
  FSQL.Add('AND sik.colid = sc.colid');
  FSQL.Add('AND so.name = ''' + Trim(ObjectName) + '''');
  FSQL.Add('AND sik.keyno = 1');
end;

procedure TAstaIOMSSqlInfo.GetForeignKeys(ObjectName: string);
begin
  // This is NOT correct, but will do for now
  FSQL.Clear;
  FSQL.Add('SELECT CONVERT(VARCHAR(40), sc.name), TableName = '''', IndexName = CONVERT(VARCHAR(40), si.name)');
  FSQL.Add('FROM syscolumns sc, sysobjects so, sysindexkeys sik, sysindexes si');
  FSQL.Add('WHERE sik.id = so.id');
  FSQL.Add('AND sc.id = so.id');
  FSQL.Add('AND sik.colid = sc.colid');
  FSQL.Add('and si.id = so.id');
  FSQL.Add('and sik.indid = si.indid');
  FSQL.Add('AND so.name = ''' + Trim(ObjectName) + '''');
  FSQL.Add('AND sik.keyno > 1');
end;

procedure TAstaIOMSSqlInfo.GetStoredProcColumns(ObjectName: string);
begin
  FSQL.Clear;
  FSQL.Add('SELECT column_name = CONVERT(VARCHAR(40), sc.name), type_name = CONVERT(VARCHAR(40), st.name), length = sc.length');
  FSQL.Add('FROM dbo.syscolumns sc, dbo.sysobjects so, dbo.systypes st');
  FSQL.Add('WHERE so.id = sc.id');
  FSQL.Add('AND sc.usertype = st.usertype');
  FSQL.Add('AND so.name = ''' + Trim(ObjectName) + '''');
  FSQL.Add('ORDER BY sc.colid');
end;

procedure TAstaIOMSSqlInfo.GetStoredProcs;
begin
  FSQL.Clear;
  FSQL.Add('SELECT Name = CONVERT(VARCHAR(40), so.name), Owner = CONVERT(VARCHAR(40), su.name), Description = ''''');
  FSQL.Add('FROM dbo.sysusers su, dbo.sysobjects so WHERE su.uid = so.uid AND so.type=''P'' ORDER BY so.name');
end;

procedure TAstaIOMSSqlInfo.GetSystemTables;
begin
  FSQL.Clear;
  FSQL.Add('SELECT Name = CONVERT(VARCHAR(40), so.name), Owner = CONVERT(VARCHAR(40), su.name), Description = ''''');
  FSQL.Add('FROM dbo.sysusers su, dbo.sysobjects so WHERE su.uid = so.uid AND so.type=''S'' ORDER BY so.name');
end;

procedure TAstaIOMSSqlInfo.GetTables;
begin
  FSQL.Clear;
  FSQL.Add('SELECT Name = CONVERT(VARCHAR(40), so.name), Owner = CONVERT(VARCHAR(40), su.name), Description = ''''');
  FSQL.Add('FROM dbo.sysusers su, dbo.sysobjects so WHERE su.uid = so.uid AND so.type=''U'' ORDER BY so.name');
end;

procedure TAstaIOMSSqlInfo.GetTriggers(ObjectName: string);
begin
  FSQL.Clear;
  FSQL.Add('SELECT Name = CONVERT(VARCHAR(40), so.name), RelationName = '''', TrigerType = '''', Description = ''''');
  if Trim(ObjectName) <> '' then
  begin
    FSQL.Add('FROM dbo.sysobjects so where so.type=''TR'' ORDER BY so.name');
  end
  else
  begin
    FSQL.Add('FROM sysobjects so, sysobjects so2');
    FSQL.Add('WHERE so.type = ''TR''');
    FSQL.Add('AND (so.deltrig = so2.id');
    FSQL.Add('OR so.instrig = so2.id');
    FSQL.Add('OR so.updtrig = so2.id)');
    FSQL.Add('AND so2.name = ''' + Trim(ObjectName) + '''');
  end;
end;

procedure TAstaIOMSSqlInfo.GetViews;
begin
  FSQL.Clear;
  FSQL.Add('SELECT Name = CONVERT(VARCHAR(40), so.name), Owner = CONVERT(VARCHAR(40), su.name), Description = ''''');
  FSQL.Add('FROM dbo.sysusers su, dbo.sysobjects so WHERE su.uid = so.uid AND so.type=''V'' ORDER BY so.name');
end;

end.
