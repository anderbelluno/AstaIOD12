object frmSetupDBExpressConnection: TfrmSetupDBExpressConnection
  Left = 545
  Top = 297
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'DBExpress Connection Setup'
  ClientHeight = 184
  ClientWidth = 360
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox: TGroupBox
    Left = 0
    Top = 0
    Width = 360
    Height = 137
    Align = alTop
    TabOrder = 0
    object LblConnectionName: TLabel
      Left = 8
      Top = 20
      Width = 88
      Height = 13
      Caption = 'Connection Name:'
    end
    object LblDriverName: TLabel
      Left = 34
      Top = 43
      Width = 62
      Height = 13
      Caption = 'Driver Name:'
    end
    object lblLibName: TLabel
      Left = 31
      Top = 66
      Width = 65
      Height = 13
      Caption = 'Library Name:'
    end
    object LblGetDriverFunc: TLabel
      Left = 18
      Top = 88
      Width = 78
      Height = 13
      Caption = 'Get Driver Func:'
    end
    object LblVendorLib: TLabel
      Left = 42
      Top = 110
      Width = 54
      Height = 13
      Caption = 'Vendor Lib:'
    end
    object CbxConnectionName: TComboBox
      Left = 99
      Top = 13
      Width = 145
      Height = 21
      ItemHeight = 13
      TabOrder = 0
    end
    object CbxDriverName: TComboBox
      Left = 99
      Top = 36
      Width = 145
      Height = 21
      ItemHeight = 13
      TabOrder = 1
    end
    object edtLibName: TEdit
      Left = 99
      Top = 59
      Width = 144
      Height = 21
      TabOrder = 2
    end
    object edtGetDrvFunc: TEdit
      Left = 99
      Top = 81
      Width = 144
      Height = 21
      TabOrder = 3
    end
    object Edit1: TEdit
      Left = 99
      Top = 103
      Width = 144
      Height = 21
      TabOrder = 4
    end
    object Button1: TButton
      Left = 272
      Top = 57
      Width = 65
      Height = 25
      Hint = 'Need to be a Value List Editor !!'
      Caption = 'Params...'
      ParentShowHint = False
      ShowHint = True
      TabOrder = 5
    end
  end
  object Panel1: TPanel
    Left = 0
    Top = 137
    Width = 360
    Height = 47
    Align = alClient
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 1
    object ButtonApply: TButton
      Left = 95
      Top = 11
      Width = 66
      Height = 25
      Caption = 'Apply'
      TabOrder = 0
    end
    object ButtonCancel: TButton
      Left = 199
      Top = 11
      Width = 66
      Height = 25
      Caption = 'Cancel'
      TabOrder = 1
    end
  end
  object AstaIODataSet: TAstaIODataSet
    AutoCreateCalcFields = <>
    StoreDefs = True
    Indexes = <>
    Aggregates = <>
    Constraints = <>
    Left = 328
    Top = 8
  end
  object SQLConnection1: TSQLConnection
    ConnectionName = 'Oracle'
    DriverName = 'Oracle'
    GetDriverFunc = 'getSQLDriverORACLE'
    LibraryName = 'dbexpora.dll'
    Params.Strings = (
      'BlobSize=-1'
      'DataBase=Database Name'
      'DriverName=Oracle'
      'ErrorResourceFile='
      'LocaleCode=0000'
      'Password=password'
      'Oracle TransIsolation=ReadCommited'
      'User_Name=user')
    VendorLib = 'OCI.DLL'
    Left = 272
    Top = 8
  end
end
