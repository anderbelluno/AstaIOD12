unit AstaSoap_wizWSDL;

interface

uses
  Windows, SysUtils, Classes, ToolsAPI, {DsgnIntf, }Forms, Menus,
  Controls;

type
  TOTAFile = class(TInterfacedObject, IOTAFile)
  private
    FContent: string;
  public
    constructor Create(const AContent: string);
    { IOTAFile }
    function GetSource: string;
    function GetAge: TDateTime;
  end;

  TCreator = class(TInterfacedObject)
  public
    { IOTACreator }
    function GetExisting: Boolean;
    function GetFileSystem: string;
    function GetOwner: IOTAModule;
    function GetUnnamed: Boolean;
  end;

  TModuleCreator = class(TCreator, IOTACreator, IOTAModuleCreator)
  private
    FProject: IOTAProject;
    FUnitName: String;
    FUnitText: String;
  public
    constructor Create(const AProject: IOTAProject; const UnitName,
      UnitText: String);
    { IOTACreator }
    function GetCreatorType: string;
    function GetOwner: IOTAModule;
    { IOTAModuleCreator }
    function GetAncestorName: string;
    function GetImplFileName: string;
    function GetIntfFileName: string;
    function GetFormName: string;
    function GetMainForm: Boolean;
    function GetShowForm: Boolean;
    function GetShowSource: Boolean;
    function NewFormFile(const FormIdent, AncestorIdent: string): IOTAFile;
    function NewImplSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
    function NewIntfSource(const ModuleIdent, FormIdent, AncestorIdent: string): IOTAFile;
    procedure FormCreated(const FormEditor: IOTAFormEditor);
  end;

  TAstaWSDLWizard = class(TNotifierObject, IOTANotifier, IOTAWizard, IOTARepositoryWizard, IOTAFormWizard)
  public
    { IOTANotifier }
    procedure Destroyed;
    { IOTAWizard }
    function GetIDString: string;
    function GetName: string;
    function GetState: TWizardState;
    procedure Execute;
    { IOTARepositoryWizard }
    function GetAuthor: string;
    function GetComment: string;
    function GetPage: string;
    {$ifdef VER130}
    function GetGlyph: HICON;
    {$else}
    function GetGlyph: Cardinal;
    {$endif}

    constructor Create;
    destructor Destroy; override;
  end;

procedure Register;

implementation

uses
  AstaSoap_WizardForm;

{$R *.res}

procedure Register;
begin
  RegisterPackageWizard(TAstaWSDLWizard.Create as IOTARepositoryWizard);
end;

function FindModuleInterface(AInterface: TGUID): IUnknown;
var
  I: Integer;
begin
  Result := nil;
  with BorlandIDEServices as IOTAModuleServices do
    for I := 0 to ModuleCount - 1 do
      if (Modules[I].QueryInterface(AInterface, Result) = S_OK) then
        Break;
end;

function GetCurrentProjectGroup: IOTAProjectGroup;
begin
  Result := FindModuleInterface(IOTAProjectGroup) as IOTAProjectGroup;
end;

function GetCurrentProject: IOTAProject;
var
  ProjectGroup: IOTAProjectGroup;
begin
  ProjectGroup := GetCurrentProjectGroup;
  if Assigned(ProjectGroup) then
    Result := ProjectGroup.ActiveProject
  else
    Result := FindModuleInterface(IOTAProject) as IOTAProject;
end;

{ TOTAFile }

constructor TOTAFile.Create(const AContent: string);
begin
  FContent := AContent;
end;

function TOTAFile.GetAge: TDateTime;
begin
  Result := -1;
end;

function TOTAFile.GetSource: string;
begin
  Result := FContent;
end;

{ TCreator }

function TCreator.GetExisting: Boolean;
begin
  Result := False;
end;

function TCreator.GetFileSystem: string;
begin
  Result := '';
end;

function TCreator.GetOwner: IOTAModule;
begin
  Result := nil;
end;

function TCreator.GetUnnamed: Boolean;
begin
  Result := True;
end;

{ TModuleCreator }

constructor TModuleCreator.Create(const AProject: IOTAProject; const UnitName,
  UnitText: String);
begin
  FProject := AProject;
  FUnitName := UnitName;
  FUnitText := UnitText;
end;

procedure TModuleCreator.FormCreated(const FormEditor: IOTAFormEditor);
begin
end;

function TModuleCreator.GetAncestorName: string;
begin
  Result := '';
end;

function TModuleCreator.GetCreatorType: string;
begin
  Result := sUnit;
end;

function TModuleCreator.GetFormName: string;
begin
  Result := '';
end;

function TModuleCreator.GetImplFileName: string;
begin
  Result := FUnitName;
end;

function TModuleCreator.GetIntfFileName: string;
begin
  Result := '';
end;

function TModuleCreator.GetMainForm: Boolean;
begin
  Result := False;
end;

function TModuleCreator.GetOwner: IOTAModule;
begin
  Result := FProject as IOTAModule;
end;

function TModuleCreator.GetShowForm: Boolean;
begin
  Result := True;
end;

function TModuleCreator.GetShowSource: Boolean;
begin
  Result := True;
end;

function TModuleCreator.NewFormFile(const FormIdent,
  AncestorIdent: string): IOTAFile;
begin
  Result := nil;
end;

function TModuleCreator.NewImplSource(const ModuleIdent, FormIdent,
  AncestorIdent: string): IOTAFile;
begin
  if ModuleIdent <> FUnitName then
    FUnitText := StringReplace(FUnitText, 'unit ' + FUnitName,
      'unit ' + ModuleIdent, []);
  Result := TOTAFile.Create(FUnitText);
end;

function TModuleCreator.NewIntfSource(const ModuleIdent, FormIdent,
  AncestorIdent: string): IOTAFile;
begin
  Result := nil;
end;

{ TNewKeyBindingWizard }

constructor TAstaWSDLWizard.Create;
begin
  inherited Create;
end;

destructor TAstaWSDLWizard.Destroy;
begin
  inherited Destroy;
end;

procedure TAstaWSDLWizard.Destroyed;
begin
  { do nothing for now }
end;

procedure TAstaWSDLWizard.Execute;
var
  UnitName, UnitText: String;
begin
  with TWSDLWizardForm.Create(Application) do
    try
      if Execute(UnitName, UnitText) then
        begin
          UnitName := UnitName + '.pas';
          (BorlandIDEServices as IOTAModuleServices).CreateModule(
            TModuleCreator.Create(GetCurrentProject, UnitName, UnitText));
        end;
    finally
      Free;
    end;
end;

function TAstaWSDLWizard.GetAuthor: string;
begin
  Result := 'ASTA Techology Group Inc';
end;

function TAstaWSDLWizard.GetComment: string;
begin
  Result := 'Creates a Client Proxy from a WSDL Document';
end;

{$ifdef VER130}
function TAstaWSDLWizard.GetGlyph: HICON;
{$else}
function TAstaWSDLWizard.GetGlyph: Cardinal;
{$endif}
begin
  Result := LoadIcon(HInstance, 'WSDL_WIZARD');
end;

function TAstaWSDLWizard.GetIDString: string;
begin
  Result := 'Asta.SOAP.WSDLWizard';
end;

function TAstaWSDLWizard.GetName: string;
begin
  Result := 'ASTA WSDL Importer';
end;

function TAstaWSDLWizard.GetPage: string;
begin
  Result := 'New';
end;

function TAstaWSDLWizard.GetState: TWizardState;
begin
  Result := [wsEnabled];
end;

end.
