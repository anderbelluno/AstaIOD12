inherited DBSessionZeosPg: TDBSessionZeosPg
  Left = 604
  Top = 113
  inherited MetaData: TAstaIOMetaData
    OnDBMSName = MetaDataDBMSName
  end
  inherited DBInfo: TAstaIODBInfo
    RdbmsInfoKind = rdbPostgreSQL
  end
  object Database: TZPgSqlDatabase
    Port = '5432'
    Encoding = etNone
    LoginPrompt = False
    Connected = False
    Left = 32
    Top = 64
  end
  object Transaction: TZPgSqlTransact
    Options = []
    AutoCommit = True
    Database = Database
    AutoRecovery = True
    TransactSafe = True
    TransIsolation = ptDefault
    Left = 96
    Top = 64
  end
  object Query: TZPgSqlQuery
    Database = Database
    Transaction = Transaction
    CachedUpdates = False
    ShowRecordTypes = [ztModified, ztInserted, ztUnmodified]
    Options = [doHourGlass, doAutoFillDefs, doUseRowId]
    LinkOptions = [loAlwaysResync]
    Constraints = <>
    ExtraOptions = [poTextAsMemo, poOidAsBlob]
    Macros = <>
    RequestLive = False
    Left = 168
    Top = 64
  end
end
