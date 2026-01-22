object HDXLauncherService: THDXLauncherService
  OldCreateOrder = False
  AllowPause = False
  DisplayName = 'HDXLauncher Service'
  ErrorSeverity = esIgnore
  Interactive = True
  OnExecute = ServiceExecute
  OnStart = ServiceStart
  OnStop = ServiceStop
  Left = 192
  Top = 114
  Height = 150
  Width = 215
end
