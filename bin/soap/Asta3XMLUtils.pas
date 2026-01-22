unit Asta3XMLUtils;

interface
uses db;

implementation
uses xml_ds,AstaDrv2;

type
 TAstaFactoryStub = class(TObject)
  private
    FDataSet: TDataSet;
  public
    constructor Create(ds: TDataSet);
    function AstaFactory(const dsName: string;
      const fldDefs: array of rFieldDef): TDataSet;
  end;

constructor TAstafactoryStub.Create(ds: TDataSet);
begin
 FDataSet:=DS;
end;


function TAstaFactoryStub.AstaFactory(const dsName: string;
  const fldDefs: array of rFieldDef): TDataSet;
var
  tbl: TDataSet;
  i: integer;
begin
  result:=FDataSet;
  if FDataSet.Active then TAstaDataSet(FDataSet).Empty;
  FDataSet.Close;
  TAstaDataSet(FDataSet).NukeAllFieldInfo;
  for i := Low(fldDefs) to High(fldDefs) do
    with fldDefs[i] do
      TAstaDataSet(FDataSet).FastFieldDefine(mname,mtype,msize);


end;


end.
