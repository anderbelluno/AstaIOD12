object AstaIOAdvantageDBPluginDM: TAstaIOAdvantageDBPluginDM
  OldCreateOrder = False
  Left = 455
  Top = 298
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
    Left = 40
    Top = 32
  end
end
