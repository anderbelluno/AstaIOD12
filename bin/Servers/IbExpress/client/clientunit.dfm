object Form1: TForm1
  Left = 195
  Top = 202
  Width = 783
  Height = 540
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object DBGrid1: TDBGrid
    Left = 136
    Top = 112
    Width = 320
    Height = 120
    DataSource = DataSource1
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
  end
  object Button1: TButton
    Left = 152
    Top = 32
    Width = 75
    Height = 25
    Caption = 'applyUpdates'
    TabOrder = 1
    OnClick = Button1Click
  end
  object AstaIONativeClientWire1: TAstaIONativeClientWire
    Address = '63.224.240.84'
    ConnectAction = caUseDesignAddress
    Port = 9050
    UserName = 'test'
    KeysExchange = keNoKeysExchange
    Timeout = 0
    Left = 160
    Top = 64
  end
  object Prov: TAstaIOProviderDataSet
    AutoCreateCalcFields = <>
    StoreDefs = True
    Active = True
    Indexes = <>
    Aggregates = <>
    Constraints = <>
    FieldDefs = <
      item
        Name = 'CUST_NO'
        DataType = ftInteger
      end
      item
        Name = 'CUSTOMER'
        DataType = ftString
        Size = 25
      end
      item
        Name = 'CONTACT_FIRST'
        DataType = ftString
        Size = 15
      end
      item
        Name = 'CONTACT_LAST'
        DataType = ftString
        Size = 20
      end
      item
        Name = 'PHONE_NO'
        DataType = ftString
        Size = 20
      end
      item
        Name = 'ADDRESS_LINE1'
        DataType = ftString
        Size = 30
      end
      item
        Name = 'ADDRESS_LINE2'
        DataType = ftString
        Size = 30
      end
      item
        Name = 'CITY'
        DataType = ftString
        Size = 25
      end
      item
        Name = 'STATE_PROVINCE'
        DataType = ftString
        Size = 15
      end
      item
        Name = 'COUNTRY'
        DataType = ftString
        Size = 15
      end
      item
        Name = 'POSTAL_CODE'
        DataType = ftString
        Size = 12
      end
      item
        Name = 'ON_HOLD'
        DataType = ftString
        Size = 1
      end
      item
        Name = 'AFLOAT'
        DataType = ftBCD
      end>
    AstaClientWire = AstaIONativeClientWire1
    Options = []
    IndexDefs = <>
    ProviderName = 'prov'
    Params = <>
    UpdateMethod = umCached
    Left = 112
    Top = 40
  end
  object DataSource1: TDataSource
    DataSet = Prov
    Left = 56
    Top = 144
  end
end
