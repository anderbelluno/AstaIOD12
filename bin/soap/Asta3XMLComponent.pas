unit Asta3XMLComponent;

interface

uses classes,XML_Thunk, db;


procedure AstaSaveMIDAS_XML(D: TDataSet; const FileName: string); overload;
procedure AstaSaveMIDAS_XML(D: TDataSet; Stream: TStream); overload;
procedure AstaSaveMS_XML(D: TDataSet; const FileName: string); overload;
procedure AstaSaveMS_XML(D: TDataSet; Stream: TStream); overload;
type
  ThunkFieldDef = record
    mName: string;
    mType: TFieldType;
    mSize: Integer;
    mRequired: Boolean;
  end;
funGenericDataSetFactory = function(const dsName: string; const fldDefs: array of ThunkFieldDef): TDataSet of object;

Procedure LoadFromXMLGeneric(const fileName: string;  dsFactory: fungenericDataSetFactory);

implementation
uses XML_DS,AstaDrv2;

function Asta3DataSetFactory(Sender:TObject;const dsName: string;
  const fldDefs: array of ThunkFieldDef): TDataSet;
var
  i: integer;
begin
  result:=TAstaDataSet(Sender) as tDataSet;
  with TAstaDataSet(Sender) do begin
   if Active then Empty;
   Close;
   NukeAllFieldInfo;
  for i := Low(fldDefs) to High(fldDefs) do
    with fldDefs[i] do
      FastFieldDefine(mname,mtype,msize);
  end;    
end;

Procedure LoadFromXMLGeneric(const fileName: string;  dsFactory: fungenericDataSetFactory);
begin
 LoadFromXML(FileName,funDataSetFactory(dsfactory));
end;

procedure AstaSaveMIDAS_XML(D: TDataSet; const FileName: string);
begin
  SaveMIDAS_XML(D, FileName);
end;

procedure AstaSaveMIDAS_XML(D: TDataSet; Stream: TStream);
begin
  SaveMIDAS_XML(D, Stream);
end;

procedure AstaSaveMS_XML(D: TDataSet; const FileName: string);
begin
  SaveMS_XML(D, FileName);
end;

procedure AstaSaveMS_XML(D: TDataSet; Stream: TStream);
begin
  SaveMS_XML(D, Stream);
end;


end.

