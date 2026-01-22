{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10053: AstaIOASEInfo.pas 
{
{   Rev 1.0    4/10/2003 6:30:04 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:36:38 PM  Steve    Version: 1.505
}
unit AstaIOASEInfo;
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
  TAstaIOASEInfo = class(TAstaIOBaseRdbmsInfo)
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

procedure TAstaIOASEInfo.GetFields(ObjectName: string);
begin
  FSQL.Clear;
  FSQL.Add('SELECT column_name = c.name,');
  FSQL.Add('type_name = rtrim(t.name),');
  FSQL.Add('length = isnull(isnull(convert(int, c.length), 0), convert(int, d.length)) + convert(int, isnull(d.aux, ascii(substring("AAA<BB<DDDHJSPP", 2*(d.ss_dtype%35+1)+2-8/c.length,1))-64)),');
  FSQL.Add('nullable = convert(smallint, convert(bit, c.status&8))');
  FSQL.Add('FROM dbo.syscolumns c,');
  FSQL.Add('dbo.sysobjects o,');
  FSQL.Add('sybsystemprocs.dbo.spt_datatype_info d,');
  FSQL.Add('dbo.systypes t');
  FSQL.Add('WHERE o.name = ''' + Trim(ObjectName) + '''');
  FSQL.Add('AND o.id = c.id');
  FSQL.Add('AND c.usertype = t.usertype');
  FSQL.Add('AND t.type = d.ss_dtype');
  FSQL.Add('AND o.type in ("S", "U")');
  FSQL.Add('AND (d.ss_dtype NOT IN (111, 109, 38, 110) OR c.usertype >= 100)');
  FSQL.Add('ORDER BY colid');
end;

procedure TAstaIOASEInfo.GetForeignKeys(ObjectName: string);
begin
  FSQL.Clear;
  FSQL.Add('SELECT ');
  FSQL.Add('keys = col_name(k.id, key1)');
  FSQL.Add(' + substring('';'', 1, char_length(col_name(k.id, key2))) + col_name(k.id, key2)');
  FSQL.Add(' + substring('';'', 1, char_length(col_name(k.id, key3))) + col_name(k.id, key3)');
  FSQL.Add(' + substring('';'', 1, char_length(col_name(k.id, key4))) + col_name(k.id, key4)');
  FSQL.Add(' + substring('';'', 1, char_length(col_name(k.id, key5))) + col_name(k.id, key5)');
  FSQL.Add(' + substring('';'', 1, char_length(col_name(k.id, key6))) + col_name(k.id, key6)');
  FSQL.Add(' + substring('';'', 1, char_length(col_name(k.id, key7))) + col_name(k.id, key7)');
  FSQL.Add(' + substring('';'', 1, char_length(col_name(k.id, key8))) + col_name(k.id, key8),');
  FSQL.Add('related_object = object_name(k.depid),');
  FSQL.Add('indexname = ''''');
  FSQL.Add('FROM dbo.syskeys k, master.dbo.spt_values v');
  FSQL.Add('WHERE k.type = v.number AND v.type = ''K''');
  FSQL.Add('AND v.number = 2');
  FSQL.Add('AND k.id = object_id(''' + Trim(ObjectName) + ''')');
end;

procedure TAstaIOASEInfo.GetIndexes(ObjectName: string);
begin
  FSQL.Clear;
  FSQL.Add('SELECT si.name');
  FSQL.Add('FROM sysobjects so, sysindexes si');
  FSQL.Add('WHERE si.id = so.id');
  FSQL.Add('AND so.name = ''' + Trim(ObjectName) + '''');
  FSQL.Add('ORDER BY si.name');
end;

procedure TAstaIOASEInfo.GetPrimeKeys(ObjectName: string);
begin
  FSQL.Clear;
  FSQL.Add('SELECT name = col_name(k.id, key1), keyid = 1');
  FSQL.Add('FROM dbo.syskeys k, master.dbo.spt_values v');
  FSQL.Add('WHERE k.type = v.number AND v.type = ''K''');
  FSQL.Add('AND v.number = 1');
  FSQL.Add('AND k.id = object_id(''' + Trim(ObjectName) + ''')');
  FSQL.Add('AND col_name(k.id, key1) IS NOT NULL');
  FSQL.Add('UNION');
  FSQL.Add('SELECT name = col_name(k.id, key2), keyid = 2');
  FSQL.Add('FROM dbo.syskeys k, master.dbo.spt_values v');
  FSQL.Add('WHERE k.type = v.number AND v.type = ''K''');
  FSQL.Add('AND v.number = 1');
  FSQL.Add('AND k.id = object_id(''' + Trim(ObjectName) + ''')');
  FSQL.Add('-AND col_name(k.id, key2) IS NOT NULL');
  FSQL.Add('UNION');
  FSQL.Add('SELECT name = col_name(k.id, key3), keyid = 3');
  FSQL.Add('FROM dbo.syskeys k, master.dbo.spt_values v');
  FSQL.Add('WHERE k.type = v.number AND v.type = ''K''');
  FSQL.Add('AND v.number = 1');
  FSQL.Add('AND k.id = object_id(''' + Trim(ObjectName) + ''')');
  FSQL.Add('AND col_name(k.id, key3) IS NOT NULL');
  FSQL.Add('UNION');
  FSQL.Add('SELECT name = col_name(k.id, key4), keyid = 4');
  FSQL.Add('FROM dbo.syskeys k, master.dbo.spt_values v');
  FSQL.Add('WHERE k.type = v.number AND v.type = ''K''');
  FSQL.Add('AND v.number = 1');
  FSQL.Add('AND k.id = object_id(''' + Trim(ObjectName) + ''')');
  FSQL.Add('AND col_name(k.id, key4) IS NOT NULL');
  FSQL.Add('UNION');
  FSQL.Add('SELECT name = col_name(k.id, key5), keyid = 5');
  FSQL.Add('FROM dbo.syskeys k, master.dbo.spt_values v');
  FSQL.Add('WHERE k.type = v.number AND v.type = ''K''');
  FSQL.Add('AND v.number = 1');
  FSQL.Add('AND k.id = object_id(''' + Trim(ObjectName) + ''')');
  FSQL.Add('AND col_name(k.id, key5) IS NOT NULL');
  FSQL.Add('UNION');
  FSQL.Add('SELECT name = col_name(k.id, key6), keyid = 6');
  FSQL.Add('FROM dbo.syskeys k, master.dbo.spt_values v');
  FSQL.Add('WHERE k.type = v.number AND v.type = ''K''');
  FSQL.Add('AND v.number = 1');
  FSQL.Add('AND k.id = object_id(''' + Trim(ObjectName) + ''')');
  FSQL.Add('AND col_name(k.id, key6) IS NOT NULL');
  FSQL.Add('UNION');
  FSQL.Add('SELECT name = col_name(k.id, key7), keyid = 7');
  FSQL.Add('FROM dbo.syskeys k, master.dbo.spt_values v');
  FSQL.Add('WHERE k.type = v.number AND v.type = ''K''');
  FSQL.Add('AND v.number = 1');
  FSQL.Add('AND k.id = object_id(''' + Trim(ObjectName) + ''')');
  FSQL.Add('AND col_name(k.id, key7) IS NOT NULL');
  FSQL.Add('UNION');
  FSQL.Add('SELECT name = col_name(k.id, key8), keyid = 8');
  FSQL.Add('FROM dbo.syskeys k, master.dbo.spt_values v');
  FSQL.Add('WHERE k.type = v.number AND v.type = ''K''');
  FSQL.Add('AND v.number = 1');
  FSQL.Add('AND k.id = object_id(''' + Trim(ObjectName) + ''')');
  FSQL.Add('AND col_name(k.id, key8) IS NOT NULL');
  FSQL.Add('ORDER BY keyid');
end;

procedure TAstaIOASEInfo.GetStoredProcColumns(ObjectName: string);
begin
  FSQL.Clear;
  FSQL.Add('SELECT c.name,');
  FSQL.Add('type_name = t.name,');
  FSQL.Add('ISNULL(c.length,0),');
  FSQL.Add('0,');
  FSQL.Add('nullable = convert(smallint, convert(bit, c.status&8))');
  FSQL.Add('FROM dbo.syscolumns c,');
  FSQL.Add('dbo.systypes t');
  FSQL.Add('WHERE c.usertype = t.usertype');
  FSQL.Add('AND c.id = object_id ("' + trim(ObjectName) + '")');
  FSQL.Add('AND c.number = 1');
  FSQL.Add('ORDER BY colid ASC');
end;

procedure TAstaIOASEInfo.GetStoredProcs;
begin
  FSQL.Clear;
  FSQL.Add('SELECT Name = so.name, Owner = su.name, Description = ''''');
  FSQL.Add('FROM dbo.sysusers su, dbo.sysobjects so WHERE su.uid = so.uid AND so.type=''P'' ORDER BY so.name');
end;

procedure TAstaIOASEInfo.GetSystemTables;
begin
  FSQL.Clear;
  FSQL.Add('SELECT Name = so.name, Owner = su.name, Description = ''''');
  FSQL.Add('FROM dbo.sysusers su, dbo.sysobjects so WHERE su.uid = so.uid AND so.type=''S'' ORDER BY so.name');
end;

procedure TAstaIOASEInfo.GetTables;
begin
  FSQL.Clear;
  FSQL.Add('SELECT Name = so.name, Owner = su.name, Description = ''''');
  FSQL.Add('FROM dbo.sysusers su, dbo.sysobjects so WHERE su.uid = so.uid AND so.type=''U'' ORDER BY so.name');
end;

procedure TAstaIOASEInfo.GetTriggers(ObjectName: string);
begin
  FSQL.Clear;
  if ObjectName <> '' then
  begin
    FSQL.Add('SELECT Name = so.name, RelationName = '''', TrigerType = '''', Description = ''''');
    FSQL.Add('FROM dbo.sysobjects so WHERE so.type=''TR'' ORDER BY so.name');
  end
  else
  begin
    FSQL.Add('SELECT Name = so.name, RelationName = '''', TrigerType = '''', Description = ''''');
    FSQL.Add('FROM sysobjects so, sysobjects so2');
    FSQL.Add('WHERE so.type = ''TR''');
    FSQL.Add('AND (so.deltrig = so2.id');
    FSQL.Add('OR so.instrig = so2.id');
    FSQL.Add('OR so.updtrig = so2.id)');
    FSQL.Add('AND so2.name = ''' + Trim(ObjectName) + '''');
  end;
end;

procedure TAstaIOASEInfo.GetViews;
begin
  FSQL.Clear;
  FSQL.Add('SELECT Name = so.name, Owner = su.name, Description = ''''');
  FSQL.Add('FROM dbo.sysusers su, dbo.sysobjects so WHERE su.uid = so.uid AND so.type=''V'' ORDER BY so.name');
end;

end.
