object KylixOffLineDialog: TKylixOffLineDialog
  Left = 384
  Top = 261
  Width = 333
  Height = 331
  HorzScrollBar.Range = 20
  VertScrollBar.Range = 65
  ActiveControl = btn_ok
  Caption = 'Off-Line properties'
  Color = clButton
  Font.Color = clText
  Font.Height = 11
  Font.Name = 'MS Sans Serif'
  Font.Pitch = fpVariable
  Font.Style = []
  Font.Weight = 0
  ParentFont = False
  Position = poScreenCenter
  OnActivate = FormActivate
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  TextWidth = 5
  object pan_bot: TPanel
    Left = 0
    Top = 299
    Width = 333
    Height = 32
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 0
    object btn_ok: TBitBtn
      Left = 79
      Top = 3
      Width = 75
      Height = 25
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
      OnClick = btn_okClick
      NumGlyphs = 2
    end
    object btn_cancel: TBitBtn
      Left = 179
      Top = 3
      Width = 75
      Height = 25
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
      NumGlyphs = 2
    end
  end
  object pan_top: TPanel
    Left = 0
    Top = 0
    Width = 333
    Height = 33
    Align = alTop
    TabOrder = 1
    object BitBtn1: TBitBtn
      Left = 9
      Top = 3
      Width = 65
      Height = 25
      Caption = 'Open'
      TabOrder = 0
      Visible = False
      Glyph.Data = {
        52010000424D4E01000000000000760000002800000012000000120000000100
        040000000000D800000000000000000000001000000010000000000000000000
        BF0000BF000000BFBF00BF000000BF00BF00BFBF0000C0C0C000808080000000
        FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00333333333333
        3333330000003333333333333333330000003333333333333333330000003800
        000000000003330000003007B7B7B7B7B7B03300000030F07B7B7B7B7B703300
        000030B0B7B7B7B7B7B70300000030FB0B7B7B7B7B7B0300000030BF07B7B7B7
        B7B7B000000030FBF000007B7B7B7000000030BFBFBFBF0000000300000030FB
        FBFBFBFBFB033300000030BFBFBFBFBFBF033300000030FBFBF0000000333300
        0000330000033333333333000000333333333333333333000000333333333333
        333333000000333333333333333333000000}
    end
  end
  object pan_left: TPanel
    Left = 0
    Top = 33
    Width = 10
    Height = 266
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 2
  end
  object pan_right: TPanel
    Left = 323
    Top = 33
    Width = 10
    Height = 266
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 3
  end
  object pg_props: TPageControl
    Left = 10
    Top = 33
    Width = 313
    Height = 266
    ActivePage = ts_general
    Align = alClient
    TabOrder = 4
    object ts_PrimeFields: TTabSheet
      Caption = 'PrimeFields'
      ImageIndex = 1
      object sb_PrimeFields: TSpeedButton
        Left = 274
        Top = 176
        Width = 23
        Height = 22
        Caption = '...'
        Visible = False
        OnClick = sb_PrimeFieldsClick
      end
      object sb_PrimeFields_up: TSpeedButton
        Left = 277
        Top = 56
        Width = 23
        Height = 22
        Glyph.Data = {
          E2000000424DDE0000000000000076000000280000000D0000000D0000000100
          0400000000006800000000000000000000001000000010000000000000000000
          BF0000BF000000BFBF00BF000000BF00BF00BFBF0000C0C0C000808080000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777777777777
          7000777777777777700077770000077770007777066607777000777706660777
          7000777706660777700070000666000070007706666666077000777066666077
          7000777706660777700077777060777770007777770777777000777777777777
          7000}
        OnClick = sb_PrimeFields_upClick
      end
      object sb_PrimeFields_down: TSpeedButton
        Left = 277
        Top = 88
        Width = 23
        Height = 22
        Glyph.Data = {
          E2000000424DDE0000000000000076000000280000000D0000000D0000000100
          0400000000006800000000000000000000001000000010000000000000000000
          BF0000BF000000BFBF00BF000000BF00BF00BFBF0000C0C0C000808080000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777777777777
          7000777777777777700077777707777770007777706077777000777706660777
          7000777066666077700077066666660770007000066600007000777706660777
          7000777706660777700077770666077770007777000007777000777777777777
          7000}
        OnClick = sb_PrimeFields_downClick
      end
      object lb_PrimeFields: TListBox
        Left = 5
        Top = 16
        Width = 267
        Height = 153
        ItemHeight = 13
        Items.Strings = (
          '1'
          '2'
          '3'
          '4'
          '5'
          '6'
          '7'
          '8'
          '9'
          '10'
          '11'
          '12'
          '13'
          '14'
          '15')
        Rows = 15
        TabOrder = 0
        OnClick = lb_PrimeFieldsClick
      end
      object e_PrimeFields: TEdit
        Left = 5
        Top = 176
        Width = 267
        Height = 21
        TabOrder = 1
      end
      object btn_PrimeFields_replace: TButton
        Left = 5
        Top = 208
        Width = 75
        Height = 25
        Caption = 'Replace'
        TabOrder = 2
        OnClick = btn_PrimeFields_replaceClick
      end
      object btn_PrimeFields_add: TButton
        Left = 87
        Top = 208
        Width = 75
        Height = 25
        Caption = 'Add'
        TabOrder = 3
        OnClick = btn_PrimeFields_addClick
      end
      object btn_PrimeFields_delete: TButton
        Left = 168
        Top = 208
        Width = 75
        Height = 25
        Caption = 'Delete'
        TabOrder = 4
        OnClick = btn_PrimeFields_deleteClick
      end
    end
    object ts_NoSQLFields: TTabSheet
      Caption = 'NoSQLFields'
      object sb_NoSQLFields_up: TSpeedButton
        Left = 277
        Top = 56
        Width = 23
        Height = 22
        Glyph.Data = {
          E2000000424DDE0000000000000076000000280000000D0000000D0000000100
          0400000000006800000000000000000000001000000010000000000000000000
          BF0000BF000000BFBF00BF000000BF00BF00BFBF0000C0C0C000808080000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777777777777
          7000777777777777700077770000077770007777066607777000777706660777
          7000777706660777700070000666000070007706666666077000777066666077
          7000777706660777700077777060777770007777770777777000777777777777
          7000}
        OnClick = sb_NoSQLFields_upClick
      end
      object sb_NoSQLFields_down: TSpeedButton
        Left = 277
        Top = 88
        Width = 23
        Height = 22
        Glyph.Data = {
          E2000000424DDE0000000000000076000000280000000D0000000D0000000100
          0400000000006800000000000000000000001000000010000000000000000000
          BF0000BF000000BFBF00BF000000BF00BF00BFBF0000C0C0C000808080000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777777777777
          7000777777777777700077777707777770007777706077777000777706660777
          7000777066666077700077066666660770007000066600007000777706660777
          7000777706660777700077770666077770007777000007777000777777777777
          7000}
        OnClick = sb_NoSQLFields_downClick
      end
      object SpeedButton3: TSpeedButton
        Left = 281
        Top = 176
        Width = 23
        Height = 22
        Caption = '...'
        Visible = False
        OnClick = sb_PrimeFieldsClick
      end
      object lb_NoSQLFields: TListBox
        Left = 5
        Top = 16
        Width = 267
        Height = 153
        ItemHeight = 13
        Items.Strings = (
          '1'
          '2'
          '3'
          '4'
          '5'
          '6'
          '7'
          '8'
          '9'
          '10'
          '11'
          '12'
          '13'
          '14'
          '15')
        Rows = 15
        TabOrder = 0
        OnClick = lb_NoSQLFieldsClick
      end
      object e_NoSQLFields: TEdit
        Left = 5
        Top = 176
        Width = 267
        Height = 21
        TabOrder = 1
      end
      object btn_NoSQLFields_add: TButton
        Left = 87
        Top = 208
        Width = 75
        Height = 25
        Caption = 'Add'
        TabOrder = 2
        OnClick = btn_NoSQLFields_addClick
      end
      object btn_NoSQLFields_replace: TButton
        Left = 5
        Top = 208
        Width = 75
        Height = 25
        Caption = 'Replace'
        TabOrder = 3
        OnClick = btn_NoSQLFields_replaceClick
      end
      object btn_NoSQLFields_delete: TButton
        Left = 168
        Top = 208
        Width = 75
        Height = 25
        Caption = 'Delete'
        TabOrder = 4
        OnClick = btn_NoSQLFields_deleteClick
      end
    end
    object ts_RefetchFields: TTabSheet
      Caption = 'RefetchFields'
      ImageIndex = 2
      object SpeedButton1: TSpeedButton
        Left = 281
        Top = 176
        Width = 23
        Height = 22
        Caption = '...'
        Visible = False
        OnClick = sb_PrimeFieldsClick
      end
      object sb_RefetchSQLFields_down: TSpeedButton
        Left = 277
        Top = 88
        Width = 23
        Height = 22
        Glyph.Data = {
          E2000000424DDE0000000000000076000000280000000D0000000D0000000100
          0400000000006800000000000000000000001000000010000000000000000000
          BF0000BF000000BFBF00BF000000BF00BF00BFBF0000C0C0C000808080000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777777777777
          7000777777777777700077777707777770007777706077777000777706660777
          7000777066666077700077066666660770007000066600007000777706660777
          7000777706660777700077770666077770007777000007777000777777777777
          7000}
        OnClick = sb_RefetchSQLFields_downClick
      end
      object sb_RefetchSQLFields_up: TSpeedButton
        Left = 277
        Top = 56
        Width = 23
        Height = 22
        Glyph.Data = {
          E2000000424DDE0000000000000076000000280000000D0000000D0000000100
          0400000000006800000000000000000000001000000010000000000000000000
          BF0000BF000000BFBF00BF000000BF00BF00BFBF0000C0C0C000808080000000
          FF0000FF000000FFFF00FF000000FF00FF00FFFF0000FFFFFF00777777777777
          7000777777777777700077770000077770007777066607777000777706660777
          7000777706660777700070000666000070007706666666077000777066666077
          7000777706660777700077777060777770007777770777777000777777777777
          7000}
        OnClick = sb_RefetchSQLFields_upClick
      end
      object lb_RefetchFields: TListBox
        Left = 5
        Top = 16
        Width = 267
        Height = 153
        ItemHeight = 13
        Items.Strings = (
          '1'
          '2'
          '3'
          '4'
          '5'
          '6'
          '7'
          '8'
          '9'
          '10'
          '11'
          '12'
          '13'
          '14'
          '15')
        Rows = 15
        TabOrder = 0
        OnClick = lb_RefetchFieldsClick
      end
      object e_RefetchFields: TEdit
        Left = 5
        Top = 176
        Width = 267
        Height = 21
        TabOrder = 1
      end
      object btn_RefetchSQLFields_delete: TButton
        Left = 168
        Top = 208
        Width = 75
        Height = 25
        Caption = 'Delete'
        TabOrder = 2
        OnClick = btn_RefetchSQLFields_deleteClick
      end
      object btn_RefetchSQLFields_add: TButton
        Left = 87
        Top = 208
        Width = 75
        Height = 25
        Caption = 'Add'
        TabOrder = 3
        OnClick = btn_RefetchSQLFields_addClick
      end
      object btn_RefetchSQLFields_replace: TButton
        Left = 5
        Top = 208
        Width = 75
        Height = 25
        Caption = 'Replace'
        TabOrder = 4
        OnClick = btn_RefetchSQLFields_replaceClick
      end
    end
    object ts_general: TTabSheet
      Caption = 'General'
      ImageIndex = 3
      object lab_database: TLabel
        Left = 10
        Top = 20
        Width = 46
        Height = 13
        Caption = 'Database:'
      end
      object lab_tablename: TLabel
        Left = 10
        Top = 44
        Width = 52
        Height = 13
        Caption = 'TableName:'
      end
      object lab_updatetablename: TLabel
        Left = 10
        Top = 68
        Width = 86
        Height = 13
        Caption = 'UpdateTableName:'
      end
      object Label1: TLabel
        Left = 10
        Top = 120
        Width = 66
        Height = 13
        Caption = 'Update Mode:'
      end
      object Label2: TLabel
        Left = 10
        Top = 152
        Width = 76
        Height = 13
        Caption = 'Update Method:'
      end
      object e_database: TEdit
        Left = 112
        Top = 16
        Width = 160
        Height = 21
        TabOrder = 0
      end
      object e_tablename: TEdit
        Left = 112
        Top = 40
        Width = 160
        Height = 21
        TabOrder = 1
      end
      object e_updatetablename: TEdit
        Left = 112
        Top = 64
        Width = 160
        Height = 21
        TabOrder = 2
      end
      object cmb_updatemode: TComboBox
        Left = 112
        Top = 118
        Width = 145
        Height = 21
        Style = csDropDownList
        ItemHeight = 15
        TabOrder = 8
      end
      object cmb_updatemethod: TComboBox
        Left = 112
        Top = 144
        Width = 145
        Height = 21
        Style = csDropDownList
        ItemHeight = 15
        TabOrder = 9
      end
      object Label3: TLabel
        Left = 10
        Top = 92
        Width = 89
        Height = 13
        Caption = 'AutoIncrementField'
      end
      object e_AutoIncrement: TEdit
        Left = 112
        Top = 88
        Width = 160
        Height = 21
        TabOrder = 11
      end
    end
  end
end
