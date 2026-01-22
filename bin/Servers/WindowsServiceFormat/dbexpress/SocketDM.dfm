object ServerDM: TServerDM
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  Left = 373
  Top = 393
  Height = 308
  Width = 434
  object ServerWire: TAstaIOSocketServerWire
    Port = 9050
    LogEvents = [slfAllEvents]
    DataBasePlugin = DataBasePlugin
    OnClientLogin = ServerWireClientLogin
    OnLogEvent = ServerWireLogEvent
    OnCreatePooledSession = ServerWireCreatePooledSession
    KeysExchange = keNoKeysExchange
    Left = 24
    Top = 16
  end
  object DataBasePlugin: TAstaIODataBasePlugin
    OnSetProviderParams = DataBasePluginSetProviderParams
    OnSubmitSQL = DataBasePluginSubmitSQL
    OnCreateProviderParamsEvent = DataBasePluginCreateProviderParamsEvent
    OnExecSQL = DataBasePluginExecSQL
    OnSupplyDBComponent = DataBasePluginSupplyDBComponent
    OnFetchMetaData = DataBasePluginFetchMetaData
    Sessions = <
      item
        SessionName = 'Session1'
        Default = True
      end>
    OnTransactionBegin = DataBasePluginTransactionBegin
    OnTransactionEnd = DataBasePluginTransactionEnd
    Left = 72
    Top = 24
  end
end
