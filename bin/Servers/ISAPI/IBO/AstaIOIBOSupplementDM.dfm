object AstaIOIBODBPluginDM: TAstaIOIBODBPluginDM
  OldCreateOrder = False
  Left = 428
  Top = 285
  Height = 479
  Width = 499
  object Serverwire: TAstaIOStringserverWire
    Port = 0
    LogEvents = [slfAllEvents]
    DataBasePlugin = IBODataPlugin
    OnClientLogin = ServerwireClientLogin
    OnCreatePooledSession = ServerwireCreatePooledSession
    KeysExchange = keNoKeysExchange
    Left = 48
    Top = 48
  end
  object IBODataPlugin: TAstaIODataBasePlugin
    OnSetProviderParams = IBODataPluginSetProviderParams
    OnSubmitSQL = IBODataPluginSubmitSQL
    OnCreateProviderParamsEvent = IBODataPluginCreateProviderParamsEvent
    OnExecSQL = IBODataPluginExecSQL
    OnSupplyDBComponent = IBODataPluginSupplyDBComponent
    OnFetchMetaData = IBODataPluginFetchMetaData
    Sessions = <>
    OnTransactionBegin = IBODataPluginTransactionBegin
    OnTransactionEnd = IBODataPluginTransactionEnd
    OnSetSQLParamsEvent = DatabasePluginSetSQLParamsEvent
    Left = 176
    Top = 64
  end
end
