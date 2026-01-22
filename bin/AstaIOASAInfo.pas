{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10051: AstaIOASAInfo.pas 
{
{   Rev 1.0    4/10/2003 6:30:02 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:36:36 PM  Steve    Version: 1.505
}
unit AstaIOASAInfo;
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
  TAstaIOASAInfo = class(TAstaIOBaseRdbmsInfo)
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

{ TAstaIOASAInfo }

procedure TAstaIOASAInfo.GetFields(ObjectName: string);
begin
  FSQL.Clear;
  FSQL.Add('SELECT column_name = sc.column_name, type_name = ISNULL(sut.type_name, sd.domain_name),');
  FSQL.Add('lngth = isnull(sc.width, 0)');
  FSQL.Add('FROM SYSTABLE st, SYSCOLUMN sc, SYSDOMAIN sd, SYSUSERTYPE sut');
  FSQL.Add('WHERE st.table_id = sc.table_id');
  FSQL.Add('AND sc.domain_id *= sut.domain_id');
  FSQL.Add('AND sc.user_type = sut.type_id');
  FSQL.Add('AND sc.domain_id = sd.domain_id');
  FSQL.Add('AND st.table_name = ''' + Trim(ObjectName) + '''');
  FSQL.Add('ORDER BY sc.column_id');
end;

procedure TAstaIOASAInfo.GetForeignKeys(ObjectName: string);
begin
  FSQL.Clear;
  FSQL.Add('SELECT ftc.column_name,');
  FSQL.Add('Srelated_object = pt.table_name,');
  FSQL.Add('SIndexName = ''''');
  FSQL.Add('SFROM SYS.SYSFOREIGNKEY fk,');
  FSQL.Add('SSYS.SYSFKCOL fkc,');
  FSQL.Add('SYS.SYSTABLE pt,');
  FSQL.Add('SYS.SYSCOLUMN ptc,');
  FSQL.Add('SYS.SYSTABLE ft,');
  FSQL.Add('SYS.SYSCOLUMN ftc');
  FSQL.Add('WHERE fk.primary_table_id=pt.table_id');
  FSQL.Add('AND pt.table_id=ptc.table_id');
  FSQL.Add('AND ptc.column_id=fkc.primary_column_id');
  FSQL.Add('AND fk.foreign_table_id=ft.table_id');
  FSQL.Add('AND ft.table_id=ftc.table_id');
  FSQL.Add('AND ftc.column_id=fkc.foreign_column_id');
  FSQL.Add('AND fk.foreign_table_id=fkc.foreign_table_id');
  FSQL.Add('AND fk.foreign_key_id=fkc.foreign_key_id');
  FSQL.Add('AND ft.table_name = ''' + Trim(ObjectName) + '''');
  FSQL.Add('ORDER BY related_object, fkc.primary_column_id asc');
end;

procedure TAstaIOASAInfo.GetIndexes(ObjectName: string);
begin
  FSQL.Clear;
  FSQL.Add('SELECT si.index_name');
  FSQL.Add('FROM SYSINDEX si, SYSTABLE st');
  FSQL.Add('WHERE si.table_id = st.table_id');
  FSQL.Add('AND st.table_name = ''' + Trim(ObjectName) + '''');
  FSQL.Add('ORDER BY si.index_name');
end;

procedure TAstaIOASAInfo.GetPrimeKeys(ObjectName: string);
begin
  FSQL.Clear;
  FSQL.Add('SELECT sc.column_name');
  FSQL.Add('FROM SYSCOLUMN sc, SYSTABLE st');
  FSQL.Add('WHERE sc.table_id = st.table_id AND sc.pkey IN(''Y'', ''M'')');
  FSQL.Add('AND st.table_name = ''' + Trim(ObjectName) + '''');
  FSQL.Add('ORDER BY sc.column_id');
end;

procedure TAstaIOASAInfo.GetStoredProcColumns(ObjectName: string);
begin
  FSQL.Clear;
  FSQL.Add('SELECT sproc.parm_name, ISNULL(sut.type_name, sd.domain_name), ISNULL(sproc.width,0),');
  FSQL.Add('(if (sproc.parm_mode_in=''Y'' and sproc.parm_mode_out=''N'' and sproc.parm_type=0) then 1 else');
  FSQL.Add('(if (sproc.parm_mode_in=''Y'' and sproc.parm_mode_out=''Y'' and sproc.parm_type=0) then 2 else');
  FSQL.Add('(if (sproc.parm_mode_in=''N'' and sproc.parm_mode_out=''Y'' and sproc.parm_type=0) then 3 else');
  FSQL.Add('(if (sproc.parm_mode_in=''N'' and sproc.parm_mode_out=''Y'' and sproc.parm_type=4) then 4 else 0');
  FSQL.Add('endif) endif) endif) endif) as parm_type');
  FSQL.Add('FROM SYS.SYSPROCPARM sproc, SYS.SYSPROCEDURE sp, SYS.SYSUSERTYPE sut, SYS.SYSDOMAIN sd');
  FSQL.Add('WHERE sproc.proc_id = sp.proc_id');
  FSQL.Add('AND sproc.parm_type = 0');
  FSQL.Add('AND sproc.domain_id = sd.domain_id');
  FSQL.Add('AND sproc.domain_id *= sut.domain_id');
  FSQL.Add('AND sproc.user_type *= sut.type_id');
  FSQL.Add('AND sp.proc_name = ''' + Trim(ObjectName) + '''');
  FSQL.Add('ORDER BY sproc.parm_id');
end;

procedure TAstaIOASAInfo.GetStoredProcs;
begin
  FSQL.Clear;
  FSQL.Add('SELECT ObjName = so.name, UserName = su.name, Description = '''' FROM sysobjects so, sysusers su WHERE so.uid *= su.uid AND so.type = ''P'' ORDER BY so.name');
end;

procedure TAstaIOASAInfo.GetSystemTables;
begin
  FSQL.Clear;
  FSQL.Add('SELECT ObjName = so.name, UserName = su.name, Description = '''' FROM sysobjects so, sysusers su WHERE so.uid *= su.uid AND so.type = ''S'' ORDER BY so.name');
end;

procedure TAstaIOASAInfo.GetTables;
begin
  FSQL.Clear;
  FSQL.Add('SELECT ObjName = so.name, UserName = su.name, RelationName = '''', Description = '''' FROM sysobjects so, sysusers su WHERE so.uid *= su.uid AND so.type = ''U'' ORDER BY so.name');
end;

procedure TAstaIOASAInfo.GetTriggers(ObjectName: string);
begin
  FSQL.Clear;
  FSQL.Add('SELECT st.trigger_name, stab.table_name, st.event, Description = '''' FROM SYSTRIGGER st, SYSTABLE stab');
  FSQL.Add('WHERE st.table_id = stab.table_id AND st.trigger_name IS NOT NULL');
  if ObjectName <> '' then
    FSQL.Add('AND stab.table_name = ''' + ObjectName + '''');
end;

procedure TAstaIOASAInfo.GetViews;
begin
  FSQL.Clear;
  FSQL.Add('SELECT ObjName = so.name, UserName = su.name, Description = '''' FROM sysobjects so, sysusers su WHERE so.uid *= su.uid AND so.type = ''V'' ORDER BY so.name');
end;

end.
