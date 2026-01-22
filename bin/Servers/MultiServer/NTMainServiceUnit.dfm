object NTMainService: TNTMainService
  OldCreateOrder = False
  OnCreate = ServiceCreate
  OnDestroy = ServiceDestroy
  DisplayName = 'NTMainService'
  Interactive = True
  OnContinue = ServiceContinue
  OnPause = ServicePause
  OnStart = ServiceStart
  OnStop = ServiceStop
  Left = 265
  Top = 319
  Height = 151
  Width = 252
end
