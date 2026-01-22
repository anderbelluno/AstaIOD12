object ServerDM: TServerDM
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Left = 216
  Top = 189
  Height = 479
  Width = 741
  object ServerWire: TAstaIOSocketServerWire
    Port = 9050
    LogEvents = [slfAllEvents]
    DataBasePlugin = dmAstaIOIBXSupplement.IBXDataPlugin
    OnClientLogin = ServerWireClientLogin
    OnLogEvent = ServerWireLogEvent
    OnUserListChange = ServerWireUserListChange
    OnCreatePooledSession = ServerWireCreatePooledSession
    KeysExchange = keNoKeysExchange
    Left = 120
    Top = 32
  end
end
