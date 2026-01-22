object AstaModule: TAstaModule
  OldCreateOrder = False
  OnCreate = WebModuleCreate
  Actions = <
    item
      Default = True
      MethodType = mtPost
      Name = 'Asta'
      PathInfo = '/asta'
      OnAction = AstaModuleTransferAction
    end>
  Left = 441
  Top = 412
  Height = 479
  Width = 741
  object Client: TAstaIOStringClientWire
    Port = 0
    UserName = 'abc'
    KeysExchange = keNoKeysExchange
    ServerWire = AstaStringDataModule.Server
    Left = 144
    Top = 88
  end
end
