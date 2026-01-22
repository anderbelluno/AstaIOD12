{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10257: AstaIOOracleInfo.pas 
{
{   Rev 1.0    4/10/2003 6:31:46 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:37:54 PM  Steve    Version: 1.505
}
unit AstaIOOracleInfo;
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
  TAstaIOOracleInfo = class(TAstaIOBaseRdbmsInfo)
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
{ TAstaIOOracleInfo }

procedure TAstaIOOracleInfo.GetIndexes(ObjectName: string);
begin
  FSQL.Clear;
  FSQL.Add('SELECT INDEX_NAME FROM ALL_INDEXES WHERE TABLE_NAME = ' + QuotedStr(Trim(ObjectName)));
end;

procedure TAstaIOOracleInfo.GetFields(ObjectName: string);
begin
  FSQL.Clear;
  FSQL.Add('SELECT column_name as FieldName, data_type as FieldType, data_length as FieldSize,');
  FSQL.Add('DATA_SCALE, DATA_PRECISION');
  FSQL.Add('FROM ALL_TAB_COLUMNS ');
  FSQL.Add('WHERE TABLE_NAME = ' + QuotedStr(Trim(ObjectName)));
end;

procedure TAstaIOOracleInfo.GetPrimeKeys(ObjectName: string);
begin
  FSQL.Clear;
  FSQL.Add('SELECT '' '' as FieldName FROM DUAL');
end;

procedure TAstaIOOracleInfo.GetForeignKeys(ObjectName: string);
begin
  FSQL.Clear;
  FSQL.Add('SELECT '' '' as FieldName, '' '' as TableName, '' '' as IndexName FROM DUAL');
end;

procedure TAstaIOOracleInfo.GetStoredProcColumns(ObjectName: string);
begin
  FSQL.Clear;
  FSQL.Add('SELECT ARGUMENT_NAME ColumnName, DATA_TYPE as ColumnType, DATA_LENGTH as ColumnSize, IN_OUT as ParamType,');
  FSQL.Add('DATA_SCALE, DATA_PRECISION');
  FSQL.Add('FROM sys.ALL_ARGUMENTS');
  FSQL.Add('WHERE OBJECT_NAME = ' + QuotedStr(ObjectName));
  FSQL.Add(' AND PACKAGE_NAME IS Null ORDER BY OBJECT_NAME');
end;

procedure TAstaIOOracleInfo.GetStoredProcs;
begin
  FSQL.Clear;
  FSQL.Add('SELECT object_name, owner, object_type FROM sys.ALL_OBJECTS ');
  FSQL.Add('WHERE object_type IN (''FUNCTION'', ''PACKAGE'', ''PROCEDURE'')');
  FSQL.Add('ORDER BY owner, object_name');
end;

procedure TAstaIOOracleInfo.GetSystemTables;
begin
  FSQL.Clear;
  FSQL.Add('SELECT TABLE_NAME, OWNER, '' '' as Description FROM ALL_ALL_TABLES');
  FSQL.Add('WHERE OWNER IN (''SYS'', ''SYSTEM'', ''MDSYS'', ''MTSSYS'')');
  FSQL.Add('ORDER BY OWNER, TABLE_NAME');
end;

procedure TAstaIOOracleInfo.GetTables;
begin
  FSQL.Clear;
  if (rdbmOwnerInTableNames in Options) then
     FSQL.Add('SELECT Table_Name, Owner, '' '' as Description  FROM ALL_TABLES ORDER BY owner, table_Name') else
    FSQL.Add('SELECT Table_Name, '' '' Owner,  '' '' as Description  FROM User_Tables ORDER BY Table_Name');
end;

procedure TAstaIOOracleInfo.GetTriggers(ObjectName: string);
begin
  FSQL.Clear;
  FSQL.Add('SELECT trigger_name Name, '' '' RelationName, TRIGGER_TYPE TrigerType, SUBSTR(DESCRIPTION, 1, 80) Description');
  FSQL.Add('FROM ALL_TRIGGERS');
  if Trim(ObjectName) <> '' then
    FSQL.Add('WHERE table_name = ''' + Trim(ObjectName) + '''');
  FSQL.Add('ORDER BY trigger_name');
end;

procedure TAstaIOOracleInfo.GetViews;
begin
  FSQL.Clear;
  FSQL.Add('SELECT view_name, owner, '' '' Description FROM ALL_VIEWS ORDER BY view_name');
end;

end.
