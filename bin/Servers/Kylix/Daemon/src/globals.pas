unit globals;

interface

uses serverdm, logger;

const
  AppVersion = '1.0';

var
  AppLog: TLogger;
  server: TServerDataModule;

  g_Port : integer;
  g_DbUserID : String;
  g_DbPassword : String;
  g_DataBaseName : String;

implementation

end.
