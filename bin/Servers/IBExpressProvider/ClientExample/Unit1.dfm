object Form1: TForm1
  Left = 205
  Top = 199
  Width = 696
  Height = 480
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
  object Label1: TLabel
    Left = 16
    Top = 304
    Width = 84
    Height = 13
    Caption = 'Customer Number'
  end
  object DBGrid1: TDBGrid
    Left = 0
    Top = 0
    Width = 688
    Height = 249
    Align = alTop
    DataSource = DataSource1
    TabOrder = 0
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
  end
  object Button1: TButton
    Left = 16
    Top = 264
    Width = 129
    Height = 25
    Caption = 'ApplyUpdates'
    TabOrder = 1
    OnClick = Button1Click
  end
  object AstaIOStatusBar1: TAstaIOStatusBar
    Left = 0
    Top = 434
    Width = 688
    Height = 19
    Panels = <
      item
        Width = 175
      end
      item
        Width = 346
      end
      item
        Text = ' ASTA Technology Group '
        Width = 175
      end>
    SimplePanel = False
    ClientWire = AstaIONativeClientWire1
  end
  object Edit1: TEdit
    Left = 120
    Top = 304
    Width = 121
    Height = 21
    TabOrder = 3
    Text = '1002'
  end
  object Button2: TButton
    Left = 16
    Top = 336
    Width = 129
    Height = 25
    Caption = 'Refetch from Server'
    TabOrder = 4
    OnClick = Button2Click
  end
  object AstaIONativeClientWire1: TAstaIONativeClientWire
    Address = '63.224.240.84'
    ConnectAction = caUseDesignAddress
    Port = 9050
    UserName = 'a'
    KeysExchange = keNoKeysExchange
    Timeout = 0
    Left = 224
    Top = 48
  end
  object DataSource1: TDataSource
    DataSet = Provider
    Left = 32
    Top = 24
  end
  object Provider: TAstaIOProviderDataSet
    AutoCreateCalcFields = <>
    StoreDefs = True
    About = 'Press ... to Display'
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
    Params = <
      item
        DataType = ftInteger
        Name = 'cust_no'
        ParamType = ptInput
        Value = '1001'
      end>
    UpdateMethod = umCached
    Left = 24
    Top = 64
  end
end
