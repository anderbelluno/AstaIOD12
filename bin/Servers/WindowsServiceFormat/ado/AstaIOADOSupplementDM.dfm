object AstaIOADODBPluginDM: TAstaIOADODBPluginDM
  OldCreateOrder = False
  Left = 248
  Top = 105
  Height = 263
  Width = 432
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
    Left = 24
    Top = 32
  end
end
