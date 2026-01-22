object SvComMainService: TSvComMainService
  OldCreateOrder = False
  OnCreate = NtServiceCreate
  OnDestroy = NtServiceDestroy
  DisplayName = 'AnyFares Booking Application Server'
  Interactive = True
  ShareProcess = True
  UseSynchronizer = False
  AfterInstall = NtServiceAfterInstall
  AfterUninstall = NtServiceAfterUninstall
  OnContinue = NtServiceContinue
  OnPause = NtServicePause
  OnStart = NtServiceStart
  OnStop = NtServiceStop
  ServiceName = 'AnyFares'
  FailureActions = <
    item
      ActionType = faRestart
      Delay = 0
    end>
  Left = 524
  Top = 317
  Height = 150
  Width = 215
end
