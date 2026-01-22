object ServerDM: TServerDM
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Left = 274
  Top = 208
  Height = 235
  Width = 297
  object ServerWire: TAstaIOSocketServerWire
    Port = 9050
    LogEvents = [slfAllEvents]
    DataBasePlugin = AstaIOAdvantageDBPluginDM.DatabasePlugin
    OnClientLogin = ServerWireClientLogin
    OnLogEvent = ServerWireLogEvent
    OnUserListChange = ServerWireUserListChange
    OnCreatePooledSession = ServerWireCreatePooledSession
    KeysExchange = keNoKeysExchange
    Left = 40
    Top = 16
  end
end
