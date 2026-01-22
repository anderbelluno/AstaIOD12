object AstaIODBExpressForm: TAstaIODBExpressForm
  Left = 209
  Top = 191
  Width = 507
  Height = 313
  Caption = 'AstaIO Dbexpress Alias Manager'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object DBGrid1: TDBGrid
    Left = 0
    Top = 0
    Width = 499
    Height = 120
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
    Left = 360
    Top = 128
    Width = 121
    Height = 25
    Caption = 'Test Connection'
    TabOrder = 1
    OnClick = Button1Click
  end
  object DBComboBox1: TDBComboBox
    Left = 8
    Top = 128
    Width = 145
    Height = 21
    DataField = 'Database'
    DataSource = DataSource1
    ItemHeight = 13
    Items.Strings = (
      'Oracle'
      'Interbase'
      'MySQL'
      'DB2')
    TabOrder = 2
  end
  object GroupBox1: TGroupBox
    Left = 8
    Top = 168
    Width = 441
    Height = 105
    Caption = 'Connection '
    TabOrder = 3
    object DBMemo1: TDBMemo
      Left = 8
      Top = 16
      Width = 425
      Height = 73
      DataField = 'Base'
      DataSource = DataSource1
      TabOrder = 0
    end
  end
  object Button2: TButton
    Left = 176
    Top = 128
    Width = 137
    Height = 25
    Caption = 'Save Configuration '
    TabOrder = 4
    OnClick = Button2Click
  end
  object AliasDataSet: TAstaIODataSet
    StoreDefs = True
    FileName = 'Dbexpress.Asta'
    Active = True
    Constraints = <>
    FieldDefs = <
      item
        Name = 'Alias'
        DataType = ftString
        Size = 25
      end
      item
        Name = 'Database'
        DataType = ftString
        Size = 20
      end
      item
        Name = 'UserName'
        DataType = ftString
        Size = 50
      end
      item
        Name = 'Password'
        DataType = ftString
        Size = 25
      end
      item
        Name = 'Base'
        DataType = ftMemo
      end>
    Left = 280
    Top = 80
    object AliasDataSetAlias: TStringField
      DisplayWidth = 16
      FieldName = 'Alias'
      Size = 25
    end
    object AliasDataSetDatabase: TStringField
      DisplayWidth = 15
      FieldName = 'Database'
    end
    object AliasDataSetUserName: TStringField
      DisplayWidth = 15
      FieldName = 'UserName'
      Size = 50
    end
    object AliasDataSetPassword: TStringField
      DisplayWidth = 15
      FieldName = 'Password'
      Size = 25
    end
    object AliasDataSetBase: TMemoField
      FieldName = 'Base'
      Visible = False
      BlobType = ftMemo
    end
  end
  object DataSource1: TDataSource
    DataSet = AliasDataSet
    Left = 312
    Top = 80
  end
  object SQLConn: TSQLConnection
    ConnectionName = 'Interbase'
    DriverName = 'Interbase'
    GetDriverFunc = 'getSQLDriverINTERBASE'
    LibraryName = 'dbexpint.dll'
    LoginPrompt = False
    Params.Strings = (
      'DriverName=Interbase'
      'BlobSize=-1'
      'CommitRetain=False'
      'Database=192.168.3.241:/home/interbase/data/dadosdecisao.gdb'
      'ErrorResourceFile='
      'LocaleCode=0000'
      'Password=htdados'
      'RoleName=RoleName'
      'ServerCharSet='
      'SQLDialect=1'
      'Interbase TransIsolation=ReadCommited'
      'User_Name=SYSDBA'
      'WaitOnLocks=True')
    TableScope = [tsTable]
    VendorLib = 'GDS32.DLL'
    Left = 240
    Top = 78
  end
end
