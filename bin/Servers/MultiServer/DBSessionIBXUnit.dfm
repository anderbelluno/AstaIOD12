inherited DBSessionIBX: TDBSessionIBX
  OldCreateOrder = True
  Left = 695
  Top = 170
  Height = 194
  object Database: TIBDatabase
    DatabaseName = 'D:\Projects\Any Key\AnyFares\DB\ANYFARESDB.GDB'
    Params.Strings = (
      'user_name=sysdba'
      'password=masterkey')
    LoginPrompt = False
    DefaultTransaction = Transaction
    IdleTimer = 0
    SQLDialect = 1
    TraceFlags = []
    Left = 48
    Top = 72
  end
  object Transaction: TIBTransaction
    Active = False
    DefaultDatabase = Database
    Params.Strings = (
      'read_committed'
      'rec_version'
      'nowait')
    AutoStopAction = saNone
    Left = 120
    Top = 72
  end
  object Query: TIBQuery
    Database = Database
    Transaction = Transaction
    BufferChunks = 1000
    CachedUpdates = False
    UniDirectional = True
    Left = 192
    Top = 72
  end
  object StoredProc: TIBStoredProc
    Database = Database
    Transaction = Transaction
    Left = 256
    Top = 72
  end
end
