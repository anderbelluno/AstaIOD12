inherited SvComFrmService: TSvComFrmService
  Left = 745
  Top = 180
  Caption = 'svComAstaIOMultiServer'
  PixelsPerInch = 96
  TextHeight = 13
  object svSurviver: TsvSurviver
    Left = 56
    Top = 160
  end
  object svLogonSensor: TsvLogonSensor
    OnLogon = svLogonSensorLogon
    Left = 136
    Top = 160
  end
end
