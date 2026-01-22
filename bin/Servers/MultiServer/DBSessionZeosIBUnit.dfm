inherited DBSessionZeosIB: TDBSessionZeosIB
  OldCreateOrder = True
  Left = 256
  Top = 275
  Width = 624
  inherited MetaData: TAstaIOMetaData
    OnDBMSName = MetaDataDBMSName
  end
  inherited DBInfo: TAstaIODBInfo
    OnSetSQL = DBInfoSetSQL
  end
  object Database: TZIbSqlDatabase
    Encoding = etNone
    LoginPrompt = False
    SqlDialect = 1
    Connected = False
    Left = 16
    Top = 64
  end
  object Transaction: TZIbSqlTransact
    Options = []
    AutoCommit = True
    Database = Database
    TransIsolation = itDefault
    Left = 80
    Top = 64
  end
  object Query: TZIbSqlQuery
    Database = Database
    CachedUpdates = False
    ShowRecordTypes = [ztModified, ztInserted, ztUnmodified]
    Options = [doAutoFillDefs, doUseRowId]
    LinkOptions = [loAlwaysResync]
    Constraints = <>
    ExtraOptions = []
    Macros = <>
    RequestLive = False
    Left = 136
    Top = 64
  end
  object StoredProc: TZIbSqlStoredProc
    Database = Database
    CachedUpdates = False
    ShowRecordTypes = [ztModified, ztInserted, ztUnmodified]
    Options = [doAutoFillDefs, doUseRowId]
    LinkOptions = [loAlwaysResync]
    Constraints = <>
    ExtraOptions = []
    Left = 192
    Top = 64
  end
end
