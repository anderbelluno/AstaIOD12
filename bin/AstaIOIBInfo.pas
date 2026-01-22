{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10167: AstaIOIBInfo.pas 
{
{   Rev 1.0    4/10/2003 6:31:02 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:37:22 PM  Steve    Version: 1.505
}
unit AstaIOIBInfo;
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
  TAstaIOIBInfo = class(TAstaIOBaseRdbmsInfo)
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

procedure TAstaIOIBInfo.GetTriggers(ObjectName: string = '');
begin
  FSQL.Clear;
  FSQL.Add('SELECT RDB$TRIGGER_NAME, RDB$RELATION_NAME, RDB$TRIGGER_TYPE, RDB$DESCRIPTION ');
  FSQL.Add('FROM RDB$TRIGGERS');
  if ObjectName <> '' then
    FSQL.Add('WHERE UPPER(RDB$RELATION_NAME) = ''' + ObjectName + ''' ');
  FSQL.Add('ORDER BY RDB$TRIGGER_NAME');
end;

procedure TAstaIOIBInfo.GetFields(ObjectName: string);
begin
  FSQL.Clear;
  FSQL.Add('SELECT RDB$RELATION_FIELDS.RDB$FIELD_NAME,');
  FSQL.Add('RDB$FIELDS.RDB$FIELD_TYPE,');
  FSQL.Add('RDB$FIELDS.RDB$FIELD_LENGTH,');
  FSQL.Add('RDB$TYPES.RDB$TYPE_NAME,');
  FSQL.Add('RDB$FIELDS.RDB$FIELD_SCALE,');
  FSQL.Add('RDB$RELATION_FIELDS.RDB$NULL_FLAG');
  FSQL.Add('FROM RDB$FIELDS F LEFT OUTER JOIN RDB$FIELD_DIMENSIONS FD ON F.RDB$FIELD_NAME = FD.RDB$FIELD_NAME,');
  FSQL.Add('     RDB$RELATION_FIELDS RF, RDB$TYPES');
  FSQL.Add('WHERE F.RDB$FIELD_NAME = RF.RDB$FIELD_SOURCE');
  FSQL.Add('      AND UPPER(RF.RDB$RELATION_NAME) = ''' + ObjectName + ''' ');
  FSQL.Add('      AND RDB$TYPES.RDB$FIELD_NAME = ''RDB$FIELD_TYPE'' ');
  FSQL.Add('      AND RDB$FIELDS.RDB$FIELD_TYPE = RDB$TYPES.RDB$TYPE');
  FSQL.Add('ORDER BY RF.RDB$FIELD_POSITION, FD.RDB$DIMENSION');
end;

procedure TAstaIOIBInfo.GetForeignKeys(ObjectName: string);
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

procedure TAstaIOIBInfo.GetIndexes(ObjectName: string);
begin
  FSQL.Clear;
  FSQL.Add('SELECT RDB$INDEX_NAME FROM RDB$INDICES WHERE RDB$INDEX_INACTIVE = 0 AND UPPER(RDB$RELATION_NAME) = ''' + ObjectName + '''');
end;

procedure TAstaIOIBInfo.GetPrimeKeys(ObjectName: string);
begin
  FSQL.Clear;
  FSQL.Add('SELECT RDB$INDEX_SEGMENTS.RDB$FIELD_NAME ');
  FSQL.Add('FROM RDB$RELATION_CONSTRAINTS, RDB$INDEX_SEGMENTS ');
  FSQL.Add('WHERE RDB$RELATION_CONSTRAINTS.RDB$CONSTRAINT_TYPE=''PRIMARY KEY'' ');
  FSQL.Add('AND RDB$INDEX_SEGMENTS.RDB$INDEX_NAME = RDB$RELATION_CONSTRAINTS.RDB$INDEX_NAME ');
  FSQL.Add('AND UPPER(RDB$RELATION_CONSTRAINTS.RDB$RELATION_NAME) = ''' + ObjectName + ''' ');
  FSQL.Add('ORDER BY RDB$INDEX_SEGMENTS.RDB$FIELD_POSITION');
end;

procedure TAstaIOIBInfo.GetStoredProcColumns(ObjectName: string);
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

procedure TAstaIOIBInfo.GetStoredProcs;
begin
  FSQL.Clear;
  FSQL.Add('SELECT RDB$PROCEDURE_NAME, RDB$PROCEDURE_ID, RDB$OWNER_NAME, RDB$DESCRIPTION  FROM RDB$PROCEDURES ORDER BY RDB$PROCEDURE_NAME');
end;

procedure TAstaIOIBInfo.GetSystemTables;
begin
  FSQL.Clear;
  FSQL.Add('SELECT RDB$RELATION_NAME, RDB$OWNER_NAME, RDB$DESCRIPTION FROM RDB$RELATIONS WHERE RDB$SYSTEM_FLAG > 0 ORDER BY RDB$RELATION_NAME');
end;

procedure TAstaIOIBInfo.GetTables;
begin
  FSQL.Clear;
  FSQL.Add('SELECT RDB$RELATION_NAME, RDB$OWNER_NAME, RDB$DESCRIPTION ');
  FSQL.Add('FROM RDB$RELATIONS WHERE RDB$SYSTEM_FLAG = 0 ');
  FSQL.Add('AND RDB$DBKEY_LENGTH = 8 ORDER BY RDB$RELATION_NAME');
end;

procedure TAstaIOIBInfo.GetViews;
begin
  FSQL.Clear;
  FSQL.Add('SELECT RDB$RELATION_NAME, RDB$OWNER_NAME, RDB$DESCRIPTION ');
  FSQL.Add('FROM RDB$RELATIONS WHERE RDB$SYSTEM_FLAG = 0 ');
  FSQL.Add('AND RDB$DBKEY_LENGTH > 8');
  FSQL.Add('ORDER BY RDB$RELATION_NAME');
end;

end.

