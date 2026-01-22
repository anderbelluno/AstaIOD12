object ServerDataModule: TServerDataModule
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Left = 661
  Top = 275
  Height = 177
  Width = 233
  object ServerWire: TAstaIOSocketServerWire
    Port = 9050
    LogEvents = [slfAllEvents]
    OnClientConnect = ServerWireClientConnect
    OnClientDisconnect = ServerWireClientDisconnect
    OnCodedMessage = ServerWireCodedMessage
    OnLogEvent = ServerWireLogEvent
    KeysExchange = keNoKeysExchange
    Left = 40
    Top = 24
  end
end
