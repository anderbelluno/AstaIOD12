{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10275: AstaIOPostgreSQLInfo.pas 
{
{   Rev 1.0    4/10/2003 6:31:54 AM  Steve
}
unit AstaIOPostgreSQLInfo;
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
  TAstaIOPostgreSQLInfo = class(TAstaIOBaseRdbmsInfo)
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

{ TAstaIOIBInfo }

procedure TAstaIOPostgreSQLInfo.GetTriggers(ObjectName: string = '');
begin
  //Roberto Amorim
  FSQL.Clear;
  FSQL.Add('select tgname, pg_class.relname,tgtype, null AS DESCRIPTION  from pg_trigger, pg_class');
  if ObjectName <> '' then
    FSQL.Add(' WHERE pg_class.oid=tgconstrrelid AND pg_class.relname = '''+ObjectName+ ''' and (not tgisconstraint)')
  else
    FSQL.Add(' WHERE pg_class.oid=tgconstrrelid AND (not tgisconstraint)');
  FSQL.Add('ORDER BY tgname');
end;

procedure TAstaIOPostgreSQLInfo.GetFields(ObjectName: string);
begin
  //Roberto Amorim
  FSQL.Clear;
  FSQL.Add('SELECT attname AS FIELD_NAME, atttypid AS FIELD_TYPE , atttypmod-4 as FIELD_LENGTH, ');
  FSQL.Add('    typname AS TYPE_NAME, 0 as FIELD_SCALE, case when attnotnull = true then 1 else 0 end as NULL_FLAG,attnum ');
  FSQL.Add('FROM pg_attribute, pg_class, pg_type, pg_attrdef ');
  FSQL.Add('WHERE pg_class.oid=attrelid AND pg_type.oid=atttypid AND attnum>0 ');
  FSQL.Add('      AND pg_class.oid=adrelid AND adnum=attnum AND atthasdef=''t'' ');
  FSQL.Add('      AND UPPER(relname)=''' + UpperCase(ObjectName) + ''' ');
  FSQL.Add(' UNION ');
  FSQL.Add('SELECT attname AS FIELD_NAME, atttypid AS FIELD_TYPE , atttypmod-4 as FIELD_LENGTH, ');
  FSQL.Add('    typname AS TYPE_NAME, 0 as FIELD_SCALE, case when attnotnull = true then 1 else 0 end as NULL_FLAG,attnum ');
  FSQL.Add('FROM pg_attribute, pg_class, pg_type ');
  FSQL.Add('WHERE pg_class.oid=attrelid AND pg_type.oid=atttypid AND attnum>0 ');
  FSQL.Add('      AND atthasdef=''f'' AND UPPER(relname)=''' + UpperCase(ObjectName) + ''' ');
  FSQL.Add('order by attnum' );
end;

procedure TAstaIOPostgreSQLInfo.GetForeignKeys(ObjectName: string);
begin
  FSQL.Clear;
  FSQL.Add('SELECT RDB$INDEX_SEGMENTS.RDB$FIELD_NAME, ');
  FSQL.Add('RDB$RELATION_CONSTRAINTS.RDB$RELATION_NAME, ');
  FSQL.Add('RDB$RELATION_CONSTRAINTS.RDB$INDEX_NAME ');
  FSQL.Add('FROM RDB$RELATION_CONSTRAINTS, RDB$INDEX_SEGMENTS ');
  FSQL.Add('WHERE RDB$RELATION_CONSTRAINTS.RDB$CONSTRAINT_TYPE=''FOREIGN KEY'' ');
  FSQL.Add('AND RDB$INDEX_SEGMENTS.RDB$INDEX_NAME = RDB$RELATION_CONSTRAINTS.RDB$INDEX_NAME ');
  FSQL.Add('AND UPPER(RDB$RELATION_CONSTRAINTS.RDB$RELATION_NAME) = ''' + ObjectName + ''' ');
  FSQL.Add('ORDER BY RDB$INDEX_SEGMENTS.RDB$FIELD_POSITION');
end;

procedure TAstaIOPostgreSQLInfo.GetIndexes(ObjectName: string);
begin
  // Roberto Amorim
  FSQL.Clear;
  FSQL.Add('select indexrelname from "pg_statio_user_indexes" where UPPER(relname) = ''' + UpperCase(ObjectName) + '''');
end;

procedure TAstaIOPostgreSQLInfo.GetPrimeKeys(ObjectName: string);
var
  ADataSet: TDataSet;
  s: String;
  i: integer;
begin
  FSQL.Clear;
  FSQL.Add('SELECT a.attname');
  FSQL.Add('FROM pg_class, pg_attribute a');
  FSQL.Add('WHERE UPPER(pg_class.relname)=''' + UpperCase(ObjectName) + ''' and  a.attrelid = pg_class.oid and');
  FSQL.Add('  (SELECT indisprimary FROM pg_index i, pg_class ic, pg_attribute ia');
  FSQL.Add('   WHERE i.indrelid = a.attrelid AND i.indexrelid = ic.oid AND');
  FSQL.Add('         ic.oid = ia.attrelid AND ia.attname = a.attname LIMIT 1)');
  FSQL.Add('ORDER BY attnum');
end;

procedure TAstaIOPostgreSQLInfo.GetStoredProcColumns(ObjectName: string);
begin
  FSQL.Clear;
  FSQL.Add('SELECT RDB$PROCEDURE_PARAMETERS.RDB$PARAMETER_NAME, ');
  FSQL.Add(' RDB$FIELDS.RDB$FIELD_TYPE, ');
  FSQL.Add('RDB$FIELDS.RDB$FIELD_LENGTH, ');
  FSQL.Add('RDB$PROCEDURE_PARAMETERS.RDB$PARAMETER_TYPE, ');
  //FSQL.Add('RDB$FIELDS.RDB$FIELD_SCALE, ');
  FSQL.Add('RDB$TYPES.RDB$TYPE_NAME ');
  FSQL.Add('FROM RDB$PROCEDURE_PARAMETERS, RDB$FIELDS, RDB$TYPES ');
  FSQL.Add('WHERE UPPER(RDB$PROCEDURE_PARAMETERS.RDB$PROCEDURE_NAME) = ''' + ObjectName + ''' ');
  FSQL.Add('AND RDB$FIELDS.RDB$FIELD_NAME = RDB$PROCEDURE_PARAMETERS.RDB$FIELD_SOURCE ');
  FSQL.Add('AND RDB$FIELDS.RDB$FIELD_TYPE = RDB$TYPES.RDB$TYPE ');
  FSQL.Add('AND RDB$TYPES.RDB$FIELD_NAME = ''RDB$FIELD_TYPE''');
  FSQL.Add('ORDER BY RDB$PROCEDURE_PARAMETERS.RDB$PARAMETER_NUMBER'); //

end;

procedure TAstaIOPostgreSQLInfo.GetStoredProcs;
begin
  //Roberto Amorim
  FSQL.Clear;
  FSQL.Add('SELECT proname AS PROCEDURE_NAME, 0 AS PROCEDURE_ID,');
  FSQL.Add('pg_shadow.usename AS OWNER_NAME, NULL AS DESCRIPTION  from pg_proc, pg_shadow ');
  FSQL.Add('where pg_shadow.usesysid=proowner and (not proistrusted)');
end;

procedure TAstaIOPostgreSQLInfo.GetSystemTables;
begin
  //Roberto Amorim
  FSQL.Clear;
  FSQL.Add('SELECT tablename, tableowner FROM pg_tables WHERE tablename ~''^pg_''');
end;

procedure TAstaIOPostgreSQLInfo.GetTables;
begin
  //Roberto Amorim
  FSQL.Clear;
  FSQL.Add('SELECT tablename, tableowner FROM pg_tables WHERE tablename !~''^pg_''');
end;

procedure TAstaIOPostgreSQLInfo.GetViews;
begin
  //Roberto Amorim
  FSQL.Clear;
  FSQL.Add('select viewname from "pg_views"');
end;

end.

