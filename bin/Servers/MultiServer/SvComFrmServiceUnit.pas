unit SvComFrmServiceUnit;

interface

uses
  Windows, Messages, SysUtils, {$IFDEF D6ANDUP}Variants, {$ENDIF}Classes,
  Forms, StdCtrls, Controls, ShellAPI,
  NTFrmServiceUnit, SvCom_LogonSensor, SvCom_Surviver;

type
  TSvComFrmService = class(TNTFrmService)
    svSurviver: TsvSurviver;
    svLogonSensor: TsvLogonSensor;
    procedure svLogonSensorLogon(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  SvComFrmService: TSvComFrmService;

implementation

{$R *.dfm}

procedure TSvComFrmService.svLogonSensorLogon(Sender: TObject);
begin
  if Visible then exit;
  SystemIconActive := False;
  SystemIconActive := True;
end;

end.
 