unit ASTARemControlReg;

interface

uses
   AstaIORemote_Server
 , AstaIORemote_Client
  , Classes;


procedure Register;
  {$R 'ASTARemControlReg.dcr'}

implementation

procedure Register;
begin
  RegisterComponents('AstaIO', [TAstaIORemoteControlServer]);
  RegisterComponents('AstaIO', [TAstaIORemoteControlClient]);
end;

end.