inherited DBSessionZeosSQL: TDBSessionZeosSQL
  OldCreateOrder = True
  Left = 508
  Top = 289
  inherited MetaData: TAstaIOMetaData
    OnDBMSName = MetaDataDBMSName
  end
  inherited DBInfo: TAstaIODBInfo
    RdbmsInfoKind = rdbMSSQL
    OnSetSQL = DBInfoSetSQL
  end
  object Database: TZMsSqlDatabase
    Encoding = etNone
    LoginPrompt = False
    Connected = False
    Left = 40
    Top = 64
  end
  object Transaction: TZMsSqlTransact
    Options = []
    AutoCommit = True
    Database = Database
    TransactSafe = True
    Left = 96
    Top = 64
  end
  object Query: TZMsSqlQuery
    Database = Database
    Transaction = Transaction
    CachedUpdates = False
    ShowRecordTypes = [ztModified, ztInserted, ztUnmodified]
    Options = [doAutoFillDefs, doUseRowId]
    LinkOptions = [loAlwaysResync]
    Constraints = <>
    ExtraOptions = [soStoreResult]
    Macros = <>
    RequestLive = False
    Left = 168
    Top = 64
  end
  object ExecQuery: TZMsSqlQuery
    Database = Database
    Transaction = Transaction
    CachedUpdates = False
    ShowRecordTypes = [ztModified, ztInserted, ztUnmodified]
    Options = [doAutoFillDefs, doUseRowId]
    LinkOptions = [loAlwaysResync]
    Constraints = <>
    ExtraOptions = [soStoreResult]
    Macros = <>
    RequestLive = False
    Left = 232
    Top = 64
  end
  object StoredProc: TZMsSqlStoredProc
    Database = Database
    Transaction = Transaction
    CachedUpdates = False
    ShowRecordTypes = [ztModified, ztInserted, ztUnmodified]
    Options = [doAutoFillDefs, doUseRowId]
    LinkOptions = [loAlwaysResync]
    Constraints = <>
    ParamBindMode = zpbByName
    Left = 296
    Top = 64
  end
end
