{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10047: AstaIOADOUtils.pas 
{
{   Rev 1.0    4/10/2003 6:30:00 AM  Steve
}
unit AstaIOADOUtils;

interface
uses classes,AstaIOCustomDataSet,adodb,db,ADOX_TLB;



type
 TAstaADOMetaData=Class
   private
    FCataLog:Catalog;
   protected
    Function CreateColumnDataset:TAstaIODataSet;
   public
    constructor Create(ConnectionString:String);
    Destructor Destroy;override;
    Function ColumnDataset(Tablename:String):TAstaIODataSet;
    function GetDataType(DT: Integer): String;
 end;

function adoColumnDataSet(ConnectionString,TableName:String):TAstaIODataSet;
Function AstaADOParamTranslate(Parameter:TParameter):TParamType;
function DataTypeToVCLDataType(DT: Integer): TFieldType;
implementation
uses sysutils;

function adoColumnDataSet(ConnectionString,TableName:String):TAstaIODataSet;
var
 a:TAstaadoMetaData;
begin
 a:=TAstaadoMetaData.Create(connectionSTring);
 result:=a.ColumnDataset(TableName);
 a.free;
end;

function DataTypeToVCLDataType(DT: Integer): TFieldType;
begin
 Case DT of
    0 : result := ftunknown;
    2 : result := ftsmallint;
    3 : result := ftInteger;
    4 : result := ftFloat;
    5 : result := ftFloat;
    6 : result := ftCurrency;
    7 : result := ftDate;
    8 : result := ftString;
    9 : result := ftstring; //'adIDispatch;
   10 : result := ftstring; //'adError;
   11 : result := ftboolean;
   12 : result := ftstring; //'adVariant';
   13 : result := ftunknown; //'adIUnknown';
   14 : result := ftfloat; //'adDecimal';
   16 : result := ftsmallint; //'adTinyInt';
   17 : result := ftsmallint; //'adUnsignedTinyInt';
   18 : result := ftword; //'adUnsignedSmallInt';
   19 : result := ftInteger;//'adUnsignedInt';
   20 : result := ftLargeInt;//'adBigInt';
   21 : result := ftLargeInt; //'asUnsignedBigInt';
   64 : result := ftdateTime;//'adFileTime';
   72 : result := ftGuid; //'adGUID';
  128 : result := ftblob;//'adBinary';
  129 : result := ftsmallint;// 'adChar';
  130 : result := ftstring;//'adWChar';
  131 : result := ftfloat;//'adNumeric';
  132 : result := ftunknown;//'adUserDefined';
  133 : result := ftdate;//'adDBDate';
  134 : result := fttime;//'adDBTime';
  135 : result := ftdatetime;//'adDBTimeStamp';
  136 : result := ftstring;//'adChapter';
  137 : result := ftdatetime;//'adDBFileTime';
  138 : result := ftstring;//'adPropVariant';
  139 : result := ftfloat;//'adVarNumeric';
  200 : result := ftmemo;//'adVarChar';
  201 : result := ftmemo;//'adLongVarChar';
  202 : result := ftString;//'adVarWChar';
  203 : result := ftstring;//'adLongVarWChar';
  204 : result := ftblob;//'adVarBinary';
  205 : result := ftblob;//'adLongVarBinary';
  else result:=ftunknown;
 end;
end;

constructor TAstaADOMetaData.Create(ConnectionString:String);
begin
 inherited create;
 FCatalog := CoCatalog.Create;
 FCataLog._Set_ActiveConnection(ConnectionString);
end;

Destructor TAstaADOMetaData.Destroy;
begin
 FCatalog:=nil;
 inherited;
end;

Function TAstaAdoMetaData.CreateColumnDataset:TAstaIODataSet;
begin
 result:=TAstaIODataSet.Create(nil);
 result.AddField('ColumnName',ftString,255);
 result.AddField('VCLDataType',ftInteger,0);
 result.AddField('MaxLength',ftInteger,0);
 result.AddField('DataType',ftString,25);
 result.AddField('Nullable',ftboolean,0);
 result.Open;
end;

function TAstaADOMetaData.GetDataType(DT: Integer): String;
begin
 Case DT of
    0 : GetDataType := 'adEmpty';
    2 : GetDataType := 'adSmallInt';
    3 : GetDataType := 'adInteger';
    4 : GetDataType := 'adSingle';
    5 : GetDataType := 'adDouble';
    6 : GetDataType := 'adCurrency';
    7 : GetDataType := 'adDate';
    8 : GetDataType := 'adBSTR';
    9 : GetDataType := 'adIDispatch';
   10 : GetDataType := 'adError';
   11 : GetDataType := 'adBoolean';
   12 : GetDataType := 'adVariant';
   13 : GetDataType := 'adIUnknown';
   14 : GetDataType := 'adDecimal';
   16 : GetDataType := 'adTinyInt';
   17 : GetDataType := 'adUnsignedTinyInt';
   18 : GetDataType := 'adUnsignedSmallInt';
   19 : GetDataType := 'adUnsignedInt';
   20 : GetDataType := 'adBigInt';
   21 : GetDataType := 'asUnsignedBigInt';
   64 : GetDataType := 'adFileTime';
   72 : GetDataType := 'adGUID';
  128 : GetDataType := 'adBinary';
  129 : GetDataType := 'adChar';
  130 : GetDataType := 'adWChar';
  131 : GetDataType := 'adNumeric';
  132 : GetDataType := 'adUserDefined';
  133 : GetDataType := 'adDBDate';
  134 : GetDataType := 'adDBTime';
  135 : GetDataType := 'adDBTimeStamp';
  136 : GetDataType := 'adChapter';
  137 : GetDataType := 'adDBFileTime';
  138 : GetDataType := 'adPropVariant';
  139 : GetDataType := 'adVarNumeric';
  200 : GetDataType := 'adVarChar';
  201 : GetDataType := 'adLongVarChar';
  202 : GetDataType := 'adVarWChar';
  203 : GetDataType := 'adLongVarWChar';
  204 : GetDataType := 'adVarBinary';
  205 : GetDataType := 'adLongVarBinary';
 end;
end;

Function TAstaAdoMetaData.ColumnDataset(Tablename:String):TAstaIODataSet;
var
 Table    : _Table;
 i:integer;
 s:string;
begin
 result:=CreateColumnDataSet;
 Table:=CoTable.Create;
 for i:=0 to FCatalog.Tables.count-1 do
  if comparetext(Tablename,FCataLog.tables[i].Name)=0 then begin
  Table := FCatalog.Tables[i];
  break;
 end;
 // for i:=0 to Table.Keys
 For i := 0 to Table.Columns.Count-1 do begin
  s:=Table.Columns[I].Name;
  result.Appendrecord([S
                        ,ord(DataTypeToVCLDataType(Table.Columns[I].Type_))
                        ,Table.Columns[I].DefinedSize
                        ,GetDataType(Table.Columns[I].Type_),
                        (Table.Columns[I].Attributes AND adColNullable) = adColNullable]);
 end;
 result.First;
 Table:=nil;
end;

Function AstaADOParamTranslate(Parameter:TParameter):TParamType;
begin
 case Parameter.Direction of
   pdInput:result:=ptInput;
   pdOutput:result:=ptOutput;
   pdInputOutput:result:=ptInputOutput;
   pdReturnValue:result:=ptresult
  else result:=ptUnknown;
 end;
end;

end.
