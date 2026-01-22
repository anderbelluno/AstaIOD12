object ServerDM: TServerDM
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Left = 230
  Top = 113
  Height = 231
  Width = 421
  object ServerWire: TAstaIOSocketServerWire
    Port = 9050
    LogEvents = [slfAllEvents]
    DataBasePlugin = dmAstaIOIBOsupplement.AstaIOIBObjectsPlugin
    OnClientLogin = ServerWireClientLogin
    OnLogEvent = ServerWireLogEvent
    OnCreatePooledSession = ServerWireCreatePooledSession
    KeysExchange = keNoKeysExchange
    Left = 32
    Top = 16
  end
  object AstaIOSQLGenerator1: TAstaIOSQLGenerator
    TrimStringFields = False
    UpdateMode = upWhereAll
    Left = 144
    Top = 8
  end
  object UserDataSet: TAstaIODataSet
    StoreDefs = True
    Indexes = <>
    Aggregates = <>
    Constraints = <>
    Left = 120
    Top = 64
  end
  object UserHistoryDataSet: TAstaIODataSet
    StoreDefs = True
    Indexes = <>
    Aggregates = <>
    Constraints = <>
    Left = 40
    Top = 72
  end
end
