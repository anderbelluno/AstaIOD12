object ServerDM: TServerDM
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Left = 273
  Top = 381
  Height = 308
  Width = 434
  object ServerWire: TAstaIOSocketServerWire
    Port = 9050
    LogEvents = [slfAllEvents]
    DataBasePlugin = DataBasePlugin
    KeysExchange = keNoKeysExchange
    Left = 24
    Top = 16
  end
  object DataBasePlugin: TAstaIODataBasePlugin
    Sessions = <
      item
        SessionName = 'Session1'
        Default = True
      end>
    Left = 72
    Top = 24
  end
end
