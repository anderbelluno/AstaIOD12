object AstaModule: TAstaModule
  OldCreateOrder = False
  OnCreate = WebModuleCreate
  OnDestroy = WebModuleDestroy
  Actions = <
    item
      Default = True
      MethodType = mtPost
      Name = 'Asta'
      PathInfo = '/asta'
      OnAction = AstaModuleTransferAction
    end>
  Left = 347
  Top = 247
  Height = 223
  Width = 280
  object Client: TAstaIOStringClientWire
    Port = 0
    UserName = 'abc'
    KeysExchange = keNoKeysExchange
    Left = 144
    Top = 88
  end
end
