inherited DBSessionZeosMySQL: TDBSessionZeosMySQL
  OldCreateOrder = True
  Left = 659
  Top = 285
  inherited MetaData: TAstaIOMetaData
    OnDBMSName = MetaDataDBMSName
  end
  inherited DBInfo: TAstaIODBInfo
    RdbmsInfoKind = rdbMySQL
  end
  object Database: TZMySqlDatabase
    Port = '3306'
    Encoding = etNone
    LoginPrompt = False
    Connected = False
    Left = 32
    Top = 64
  end
  object Transact: TZMySqlTransact
    Options = []
    AutoCommit = True
    Database = Database
    TransactSafe = False
    Left = 96
    Top = 64
  end
  object Query: TZMySqlQuery
    Database = Database
    Transaction = Transact
    CachedUpdates = False
    ShowRecordTypes = [ztModified, ztInserted, ztUnmodified]
    Options = [doAutoFillDefs, doUseRowId]
    LinkOptions = [loAlwaysResync]
    Constraints = <>
    ExtraOptions = [moStoreResult]
    Macros = <>
    RequestLive = False
    Left = 176
    Top = 64
  end
end
