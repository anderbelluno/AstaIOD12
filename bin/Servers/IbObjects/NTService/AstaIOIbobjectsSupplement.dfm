object dmAstaIOIBOsupplement: TdmAstaIOIBOsupplement
  OldCreateOrder = False
  Left = 442
  Top = 274
  Height = 264
  Width = 336
  object AstaIOIBObjectsPlugin: TAstaIODataBasePlugin
    OnSetProviderParams = AstaIOIBObjectsPluginSetProviderParams
    OnSubmitSQL = AstaIOIBObjectsPluginSubmitSQL
    OnCreateProviderParamsEvent = AstaIOIBObjectsPluginCreateProviderParamsEvent
    OnExecSQL = AstaIOIBObjectsPluginExecSQL
    OnSupplyDBComponent = AstaIOIBObjectsPluginSupplyDBComponent
    OnFetchMetaData = AstaIOIBObjectsPluginFetchMetaData
    Sessions = <
      item
        SessionName = 'Default'
        Default = True
      end>
    OnTransactionBegin = AstaIOIBObjectsPluginTransactionBegin
    OnTransactionEnd = AstaIOIBObjectsPluginTransactionEnd
    Left = 48
    Top = 32
  end
end
