object Form1: TForm1
  Left = 51
  Top = 116
  Width = 569
  Height = 414
  Caption = 'Form1'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object DBNavigator1: TDBNavigator
    Left = 0
    Top = 362
    Width = 561
    Height = 25
    DataSource = DataSource1
    Align = alBottom
    TabOrder = 1
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 561
    Height = 115
    Align = alTop
    TabOrder = 0
    object Button1: TButton
      Left = 5
      Top = 10
      Width = 121
      Height = 25
      Caption = 'Connect / Disconnect'
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 131
      Top = 10
      Width = 75
      Height = 25
      Caption = 'Open / Close'
      TabOrder = 1
      OnClick = Button2Click
    end
    object Button3: TButton
      Left = 403
      Top = 10
      Width = 75
      Height = 25
      Caption = 'ApllyUpdates'
      TabOrder = 2
      OnClick = Button3Click
    end
  end
  object DBGrid1: TDBGrid
    Left = 0
    Top = 115
    Width = 561
    Height = 247
    Align = alClient
    DataSource = DataSource1
    TabOrder = 2
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clWindowText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Sans Serif'
    TitleFont.Style = []
  end
  object AstaIONativeClientWire: TAstaIONativeClientWire
    Address = '127.0.0.1'
    Password = 'Asta'
    Port = 9050
    UserName = 'Steve'
    KeysExchange = keNoKeysExchange
    Timeout = 0
    Left = 16
    Top = 128
  end
  object AstaIOProviderDataSet: TAstaIOProviderDataSet
    AutoCreateCalcFields = <>
    StoreDefs = True
    Indexes = <>
    Aggregates = <>
    Constraints = <>
    ObjectView = False
    FieldDefs = <
      item
        Name = 'EMP_NO'
        DataType = ftSmallint
      end
      item
        Name = 'FIRST_NAME'
        DataType = ftString
        Size = 15
      end
      item
        Name = 'LAST_NAME'
        DataType = ftString
        Size = 20
      end
      item
        Name = 'PHONE_EXT'
        DataType = ftString
        Size = 4
      end
      item
        Name = 'HIRE_DATE'
        DataType = ftTimeStamp
      end
      item
        Name = 'DEPT_NO'
        DataType = ftString
        Size = 3
      end
      item
        Name = 'JOB_CODE'
        DataType = ftString
        Size = 5
      end
      item
        Name = 'JOB_GRADE'
        DataType = ftSmallint
      end
      item
        Name = 'JOB_COUNTRY'
        DataType = ftString
        Size = 15
      end
      item
        Name = 'SALARY'
        DataType = ftFMTBcd
      end
      item
        Name = 'FULL_NAME'
        DataType = ftString
        Size = 37
      end>
    AstaClientWire = AstaIONativeClientWire
    Options = []
    IndexDefs = <>
    ProviderName = 'AstaIOProvider1'
    Params = <>
    UpdateMethod = umCached
    Left = 56
    Top = 128
  end
  object DataSource1: TDataSource
    DataSet = AstaIOProviderDataSet
    Left = 96
    Top = 128
  end
  object AstaIOExecServerMethod: TAstaIOExecServerMethod
    Params = <>
    ServerMethodName = 'AstaIOServerMethodExec1'
    AstaClientWire = AstaIONativeClientWire
    Left = 136
    Top = 128
  end
end
