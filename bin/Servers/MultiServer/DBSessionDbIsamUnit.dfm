inherited DBSessionDbIsam: TDBSessionDbIsam
  OldCreateOrder = True
  Left = 540
  Top = 156
  Height = 208
  inherited MetaData: TAstaIOMetaData
    OnDBMSName = MetaDataDBMSName
    OnTables = MetaDataTables
    DBInfo = nil
  end
  object Database: TDBISAMDatabase
    EngineVersion = '3.21'
    DatabaseName = 'AstaDataBase'
    SessionName = 'Session1_1'
    Left = 24
    Top = 80
  end
  object Session1: TDBISAMSession
    EngineVersion = '3.21'
    Active = True
    AutoSessionName = True
    ForceBufferFlush = True
    KeepConnections = False
    LockRetryCount = 15
    LockWaitTime = 100
    LockProtocol = lpPessimistic
    ProgressSteps = 20
    StrictChangeDetection = True
    SessionType = stLocal
    RemoteType = rtLAN
    RemoteAddress = '127.0.0.1'
    RemotePort = 12001
    RemoteTrace = False
    Left = 88
    Top = 80
  end
  object Query: TDBISAMQuery
    AutoDisplayLabels = False
    CopyOnAppend = False
    DatabaseName = 'DBDEMOS'
    SessionName = 'Session1_1'
    EngineVersion = '3.21'
    RequestLive = True
    MaxRowCount = -1
    SQL.Strings = (
      'select * from country')
    Params = <>
    Left = 144
    Top = 80
  end
end
