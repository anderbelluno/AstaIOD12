unit DBAnySessionUnit;

interface

{$I Compiler.inc}

uses
  SysUtils, Classes, {$IFDEF D6ANDUP}Variants, {$ELSE} Forms, {$ENDIF}
  AstaIOServerWire, AstaIODBInfo, AstaIOMetaData, AstaIODataBasePlugin,
  AnyCommon;

type
  TDBAnyPluginClass = class of TDBAnyPlugin;
  TDBAnySessionClass = class of TDBAnySession;
  TDBAnySession = class;
  TDBAnyPlugin = class(TAstaIODataBasePlugin)
  private
    { Private declarations }
    FOnServerLog: TDMServerLogEvent;
    FServerWire: TAstaIOServerWire;
  protected
    { Protected declarations }
    FDBSessionClass: TDBAnySessionClass;
    procedure LogMessage(AMessage: string; const AFilePrefix: string = ''; const AToFile: Boolean = False); virtual;
  public
    { Public declarations }
    property DBSessionClass: TDBAnySessionClass read FDBSessionClass;
    property ServerWire: TAstaIOServerWire read FServerWire write FServerWire;
    property OnServerLog: TDMServerLogEvent read FOnServerLog write FOnServerLog;

    constructor Create(AOwner: TComponent); override;
    function CreateDBSession: TDBAnySession;
  end;

  TDBAnySession = class(TDataModule)
    MetaData: TAstaIOMetaData;
    DBInfo: TAstaIODBInfo;
    procedure DataModuleCreate(Sender: TObject);
    procedure DataModuleDestroy(Sender: TObject);
  private
    { Private declarations }
    FOnServerLog: TDMServerLogEvent;
  protected
    { Protected declarations }
    procedure LogMessage(AMessage: string; const AFilePrefix: string = ''; const AToFile: Boolean = False); virtual;
  public
    { Public declarations }
    property OnServerLog: TDMServerLogEvent read FOnServerLog write FOnServerLog;

    procedure ReadSettings; virtual; abstract;
    procedure OpenConnection; virtual; abstract;
    procedure CloseConnection; virtual; abstract;
  end;

implementation

{$R *.dfm}

{=== TDBAnyPlugin =============================================================}

constructor TDBAnyPlugin.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDBSessionClass := TDBAnySession;
end;

function TDBAnyPlugin.CreateDBSession: TDBAnySession;
begin
  Result := FDBSessionClass.Create(nil);
  Result.OnServerLog := Self.OnServerLog;
  Result.DBInfo.DataBasePlugin := Self;
  Result.ReadSettings;
  Result.OpenConnection;
end;

procedure TDBAnyPlugin.LogMessage(AMessage: string; const AFilePrefix: string; const AToFile: Boolean);
begin
  if AToFile then
    LogToFile(AMessage, AFilePrefix);

  if Assigned(FOnServerLog) then
    FOnServerLog(Self, AMessage);
end;


{=== TDBAnySession ============================================================}

procedure TDBAnySession.DataModuleCreate(Sender: TObject);
begin
  ReadSettings;
end;

procedure TDBAnySession.DataModuleDestroy(Sender: TObject);
begin
  CloseConnection;
end;

procedure TDBAnySession.LogMessage(AMessage: string; const AFilePrefix: string; const AToFile: Boolean);
begin
  if AToFile then
    LogToFile(AMessage, AFilePrefix);

  if Assigned(FOnServerLog) then
    FOnServerLog(Self, AMessage);
end;


end.
