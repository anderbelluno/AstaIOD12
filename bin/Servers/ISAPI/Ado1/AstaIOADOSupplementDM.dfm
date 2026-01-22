object AstaIOADODBPluginDM: TAstaIOADODBPluginDM
  OldCreateOrder = False
  Left = 291
  Top = 285
  Height = 479
  Width = 741
  object DatabasePlugin: TAstaIODataBasePlugin
    OnSubmitSQL = DatabasePluginSubmitSQL
    OnAutoIncrementFetch = DatabasePluginAutoIncrementFetch
    OnExecSQL = DatabasePluginExecSQL
    OnSupplyDBComponent = DatabasePluginSupplyDBComponent
    OnFetchMetaData = DatabasePluginFetchMetaData
    Sessions = <
      item
        SessionName = 'Default'
        Default = True
      end>
    OnTransactionBegin = DatabasePluginTransactionBegin
    OnTransactionEnd = DatabasePluginTransactionEnd
    Left = 120
    Top = 48
  end
  object Serverwire: TAstaIOStringserverWire
    Port = 0
    LogEvents = [slfAllEvents]
    DataBasePlugin = DatabasePlugin
    OnClientLogin = ServerwireClientLogin
    OnCreatePooledSession = ServerwireCreatePooledSession
    KeysExchange = keNoKeysExchange
    Left = 32
    Top = 40
  end
end
