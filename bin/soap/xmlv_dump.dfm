object Form1: TForm1
  Left = 192
  Top = 109
  Width = 696
  Height = 478
  Caption = 'XMLv dump'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object TreeView1: TTreeView
    Left = 16
    Top = 16
    Width = 665
    Height = 193
    Indent = 19
    TabOrder = 0
    OnChange = TreeView1Change
  end
  object GroupBox1: TGroupBox
    Left = 16
    Top = 336
    Width = 657
    Height = 105
    Caption = ' Info '
    TabOrder = 1
    object Label1: TLabel
      Left = 16
      Top = 56
      Width = 59
      Height = 13
      Caption = 'NameSpace'
      FocusControl = NSEdit
    end
    object Label2: TLabel
      Left = 16
      Top = 16
      Width = 24
      Height = 13
      Caption = 'Type'
      FocusControl = NSEdit
    end
    object Label3: TLabel
      Left = 336
      Top = 16
      Width = 27
      Height = 13
      Caption = 'Value'
      FocusControl = NSEdit
    end
    object NSEdit: TEdit
      Left = 16
      Top = 72
      Width = 625
      Height = 21
      ReadOnly = True
      TabOrder = 0
    end
    object TypeEdit: TEdit
      Left = 16
      Top = 32
      Width = 305
      Height = 21
      ReadOnly = True
      TabOrder = 1
    end
    object ValueEdit: TEdit
      Left = 336
      Top = 32
      Width = 305
      Height = 21
      ReadOnly = True
      TabOrder = 2
    end
  end
  object LoadButton: TButton
    Left = 104
    Top = 304
    Width = 105
    Height = 33
    Caption = 'Load'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 2
    OnClick = LoadButtonClick
  end
  object SaveButton: TButton
    Left = 496
    Top = 304
    Width = 105
    Height = 33
    Caption = 'Save'
    Enabled = False
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 3
    OnClick = SaveButtonClick
  end
  object attrlist: TStringGrid
    Left = 16
    Top = 216
    Width = 665
    Height = 81
    ColCount = 2
    DefaultColWidth = 200
    Enabled = False
    RowCount = 1
    FixedRows = 0
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRowSelect]
    TabOrder = 4
  end
  object OpenDialog1: TOpenDialog
    Options = [ofHideReadOnly, ofNoChangeDir, ofPathMustExist, ofFileMustExist, ofShareAware, ofEnableSizing]
    Left = 568
    Top = 65528
  end
  object SaveDialog1: TSaveDialog
    Left = 600
    Top = 65528
  end
end
