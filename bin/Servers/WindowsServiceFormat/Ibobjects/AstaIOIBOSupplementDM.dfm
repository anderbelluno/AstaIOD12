object dmAstaIOIBOSupplement: TdmAstaIOIBOSupplement
  OldCreateOrder = False
  Left = 285
  Top = 161
  Height = 479
  Width = 741
  object IBODataPlugin: TAstaIODataBasePlugin
    OnSetProviderParams = IBODataPluginSetProviderParams
    OnCreateProviderParamsEvent = IBODataPluginCreateProviderParamsEvent
    OnExecSQL = IBODataPluginExecSQL
    OnSupplyDBComponent = IBODataPluginSupplyDBComponent
    OnFetchMetaData = IBODataPluginFetchMetaData
    Sessions = <
      item
        SessionName = 'Default'
        Default = True
      end>
    OnTransactionBegin = IBODataPluginTransactionBegin
    OnTransactionEnd = IBODataPluginTransactionEnd
    OnSetSQLParamsEvent = IBODataPluginSetSQLParamsEvent
    Left = 48
    Top = 32
  end
end
