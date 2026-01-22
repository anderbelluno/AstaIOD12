{ $HDR$}
{**********************************************************************}
{ Unit archived using Team Coherence                                   }
{ Team Coherence is Copyright 2002 by Quality Software Components      }
{                                                                      }
{ For further information / comments, visit our WEB site at            }
{ http://www.TeamCoherence.com                                         }
{**********************************************************************}
{}
{ $Log:  10373: AstaIOXMLComponent.pas 
{
{   Rev 1.0    4/10/2003 6:32:42 AM  Steve
}
{
{   Rev 1.0    10/30/2002 8:38:34 PM  Steve    Version: 1.505
}
unit AstaIOXMLComponent;

interface

uses classes,XML_Thunk, db;


procedure AstaSaveMIDAS_XML(D: TDataSet; const FileName: string); overload;
procedure AstaSaveMIDAS_XML(D: TDataSet; Stream: TStream); overload;
procedure AstaSaveMS_XML(D: TDataSet; const FileName: string); overload;
procedure AstaSaveMS_XML(D: TDataSet; Stream: TStream); overload;
type
TAstaXMLDataSetFormat=(taxADO,TaxMidas);
  ThunkFieldDef = record
    mName: string;
    mType: TFieldType;
    mSize: Integer;
    mRequired: Boolean;
  end;
funGenericDataSetFactory = function(const dsName: string; const fldDefs: array of ThunkFieldDef): TDataSet of object;
Procedure LoadFromXMLGeneric(const fileName: string;  dsFactory: fungenericDataSetFactory);overload;
Procedure LoadFromXMLGeneric(const Stream: TStream;  dsFactory: fungenericDataSetFactory);overload;


implementation
uses XML_DS;

Procedure LoadFromXMLGeneric(const fileName: string;  dsFactory: fungenericDataSetFactory);
begin
 LoadFromXML(FileName,funDataSetFactory(dsfactory));
end;

Procedure LoadFromXMLGeneric(const Stream: TStream;  dsFactory: fungenericDataSetFactory);overload;
var
 doc:TXmldoc;
begin
 doc:=AXML_LoadStream(Stream);
 try
  LoadFromXML(doc,funDataSetFactory(dsfactory));
  finally
   doc.free
  end;
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

