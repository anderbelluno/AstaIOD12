object ServerDM: TServerDM
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Left = 536
  Top = 227
  Height = 383
  Width = 452
  object ServerWire: TAstaIOSocketServerWire
    Port = 9050
    LogEvents = [slfAllEvents]
    DataBasePlugin = AstaIOADODBPluginDM.DatabasePlugin
    OnClientLogin = ServerWireClientLogin
    OnLogEvent = ServerWireLogEvent
    OnUserListChange = ServerWireUserListChange
    OnCreatePooledSession = ServerWireCreatePooledSession
    KeysExchange = keNoKeysExchange
    Left = 40
    Top = 16
  end
end
