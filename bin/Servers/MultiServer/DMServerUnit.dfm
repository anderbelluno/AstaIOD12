object DMServer: TDMServer
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Left = 270
  Top = 110
  Height = 189
  Width = 223
  object ServerWire: TAstaIOSocketServerWire
    Port = 9020
    LogEvents = [slfAllEvents]
    OnClientLogin = ServerWireClientLogin
    OnClientError = ServerWireClientError
    OnCodedMessage = ServerWireCodedMessage
    OnLogEvent = ServerWireLogEvent
    OnAssignPersisentSession = ServerWireAssignPersisentSession
    OnCreatePooledSession = ServerWireCreatePooledSession
    KeysExchange = keNoKeysExchange
    Left = 40
    Top = 16
  end
end
