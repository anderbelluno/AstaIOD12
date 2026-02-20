unit AstaSoapServerPEdit;
interface

uses Classes, Forms,AstaSoapMethod, AstaSoapElmEditor,
  AstaSoapTypeEditor,
  DesignEditors, DesignIntf;

type
  TAstaSoapElmEditor = class(TClassProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

  TAstaSoapTypeEditor = class(TClassProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

implementation

{ TAstaSoapElmEditor }

procedure TAstaSoapElmEditor.Edit;
var
  ElementList: TAstaSoapMethodElementList;
  ElementListEditor: TfrmAstaSoapElmEditor;
  Owner: TPersistent;
  Server: TAstaSoapMethodOwner;
  Types: TAstaSoapComplexTypeList;
begin
  ElementList:= TAstaSoapMethodElementList(GetOrdValue);
  Owner:= ElementList.Owner;
  Types:= nil;
  if Owner is TAstaSoapMethod then
  begin
    Server:= (Owner as TAstaSoapMethod).Server;
    if Server <> nil then Types:= Server.ComplexTypes;
  end;
  if Owner is TAstaSoapComplexType then
    Types:= (Owner as TAstaSoapComplexType).Collection as TAstaSoapComplexTypeList;
  ElementListEditor:= TfrmAstaSoapElmEditor.Create(Application);
  try
    ElementListEditor.Caption:= ElementList.GetNamePath;
    ElementListEditor.Edit(ElementList, Types);
    if ElementListEditor.Modified then Modified;
  finally
    ElementListEditor.Free;
  end;
end;

function TAstaSoapElmEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

{ TAstaSoapTypeEditor }

procedure TAstaSoapTypeEditor.Edit;
var
  TypeList: TAstaSoapComplexTypeList;
  TypeListEditor: TfrmAstaSoapTypeEditor;
begin
  TypeList:= TAstaSoapComplexTypeList(GetOrdValue);
  TypeListEditor:= TfrmAstaSoapTypeEditor.Create(Application);
  try
    TypeListEditor.Caption:= TypeList.GetNamePath;
    TypeListEditor.Edit(TypeList);
    if TypeListEditor.Modified then Modified;
  finally
    TypeListEditor.Free;
  end;
end;

function TAstaSoapTypeEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

end.
