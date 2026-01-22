object DBAnySession: TDBAnySession
  OldCreateOrder = False
  OnCreate = DataModuleCreate
  OnDestroy = DataModuleDestroy
  Left = 243
  Top = 111
  Height = 150
  Width = 558
  object MetaData: TAstaIOMetaData
    DBInfo = DBInfo
    Left = 80
    Top = 8
  end
  object DBInfo: TAstaIODBInfo
    RdbmsInfoKind = rdbInterbase
    DBInfoSupply = [isTables, isSysTables, isFields, isFkeys, isPKeys, isTriggers, isIndexes, isViews, isSProcs, isSProcCols]
    CaseSensitive = False
    Left = 16
    Top = 8
  end
end
