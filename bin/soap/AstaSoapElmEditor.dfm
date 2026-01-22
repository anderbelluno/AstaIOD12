object frmAstaSoapElmEditor: TfrmAstaSoapElmEditor
  Left = 341
  Top = 173
  Width = 534
  Height = 500
  BorderIcons = [biSystemMenu]
  Color = clBtnFace
  Constraints.MinHeight = 300
  Constraints.MinWidth = 400
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object lvwElements: TListView
    Left = 0
    Top = 29
    Width = 263
    Height = 444
    Align = alClient
    Columns = <
      item
        Caption = 'Name'
        Width = 150
      end
      item
        Caption = 'Type'
        Width = 100
      end>
    ColumnClick = False
    HideSelection = False
    ReadOnly = True
    RowSelect = True
    TabOrder = 0
    ViewStyle = vsReport
    OnKeyDown = lvwElementsKeyDown
    OnSelectItem = lvwElementsSelectItem
  end
  object Panel1: TPanel
    Left = 263
    Top = 29
    Width = 263
    Height = 444
    Align = alRight
    BevelInner = bvRaised
    BevelOuter = bvLowered
    TabOrder = 1
    object lblName: TLabel
      Left = 15
      Top = 19
      Width = 28
      Height = 13
      Caption = 'Name'
    end
    object edtName: TEdit
      Left = 55
      Top = 15
      Width = 190
      Height = 21
      TabOrder = 0
      OnChange = edtNameChange
    end
    object rbnSimple: TRadioButton
      Left = 15
      Top = 57
      Width = 81
      Height = 17
      Caption = 'Simple type'
      TabOrder = 1
      OnClick = rbnSimpleClick
    end
    object rbnComplex: TRadioButton
      Left = 15
      Top = 92
      Width = 86
      Height = 17
      Caption = 'Complex type'
      TabOrder = 2
      OnClick = rbnComplexClick
    end
    object cbxSimple: TComboBox
      Left = 100
      Top = 55
      Width = 145
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 3
      OnChange = cbxSimpleChange
    end
    object cbxComplex: TComboBox
      Left = 100
      Top = 90
      Width = 145
      Height = 21
      Style = csDropDownList
      ItemHeight = 13
      TabOrder = 4
      OnChange = cbxComplexChange
    end
  end
  object ToolBar1: TToolBar
    Left = 0
    Top = 0
    Width = 526
    Height = 29
    AutoSize = True
    BorderWidth = 1
    ButtonHeight = 21
    ButtonWidth = 50
    Caption = 'ToolBar1'
    EdgeBorders = [ebLeft, ebTop, ebRight, ebBottom]
    Flat = True
    ShowCaptions = True
    TabOrder = 2
    object tbnAdd: TToolButton
      Left = 0
      Top = 0
      Caption = 'Add'
      ImageIndex = 0
      OnClick = tbnAddClick
    end
    object tbnDelete: TToolButton
      Left = 50
      Top = 0
      Caption = '  Delete  '
      ImageIndex = 1
      OnClick = tbnDeleteClick
    end
    object tbnClose: TToolButton
      Left = 100
      Top = 0
      Caption = 'Close'
      ImageIndex = 2
      OnClick = tbnCloseClick
    end
  end
end
