object dmAstaIOIBXSupplement: TdmAstaIOIBXSupplement
  OldCreateOrder = False
  Left = 586
  Top = 392
  Height = 299
  Width = 487
  object IBXDataPlugin: TAstaIODataBasePlugin
    OnSetProviderParams = IBXDataPluginSetProviderParams
    OnCreateProviderParamsEvent = IBXDataPluginCreateProviderParamsEvent
    OnExecSQL = IBXDataPluginExecSQL
    OnSupplyDBComponent = IBXDataPluginSupplyDBComponent
    OnFetchMetaData = IBXDataPluginFetchMetaData
    Sessions = <
      item
        SessionName = 'Default'
        Default = True
      end>
    OnTransactionBegin = IBXDataPluginTransactionBegin
    OnTransactionEnd = IBXDataPluginTransactionEnd
    OnSetSQLParamsEvent = IBXDataPluginSetSQLParamsEvent
    Left = 40
    Top = 32
  end
end
