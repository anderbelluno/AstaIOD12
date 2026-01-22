inherited DBSessionADO: TDBSessionADO
  OldCreateOrder = True
  Left = 263
  Top = 274
  Height = 169
  inherited MetaData: TAstaIOMetaData
    OnTables = MetaDataTables
  end
  inherited DBInfo: TAstaIODBInfo
    RdbmsInfoKind = rdbMSSQL
    OnSetSQL = DBInfoSetSQL
  end
  object StoredProc: TADOStoredProc
    Connection = Connection
    Parameters = <>
    Left = 208
    Top = 64
  end
  object ExecQuery: TADOQuery
    Connection = Connection
    Parameters = <>
    Left = 136
    Top = 64
  end
  object Connection: TADOConnection
    ConnectionString = 
      'Provider=MSDataShape;Data Provider=SQLOLEDB.1;Server=(local);Use' +
      'r ID=sa;Initial Catalog=Northwind'
    LoginPrompt = False
    Provider = 'MSDataShape'
    Left = 16
    Top = 64
  end
  object Query: TADOQuery
    Connection = Connection
    CursorLocation = clUseServer
    Parameters = <>
    Left = 80
    Top = 64
  end
end
