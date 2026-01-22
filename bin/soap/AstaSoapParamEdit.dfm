object frmAstaSoapParamEdit: TfrmAstaSoapParamEdit
  Left = 244
  Top = 107
  Width = 696
  Height = 480
  BorderIcons = [biSystemMenu]
  Caption = 'frmAstaSoapParamEdit'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDefault
  OnClose = FormClose
  PixelsPerInch = 96
  TextHeight = 13
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 688
    Height = 2
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
  end
  object pnlClient: TPanel
    Left = 0
    Top = 2
    Width = 688
    Height = 451
    Align = alClient
    BevelOuter = bvLowered
    Caption = 'pnlClient'
    TabOrder = 1
    object PageControl1: TPageControl
      Left = 1
      Top = 1
      Width = 686
      Height = 449
      ActivePage = TabSheet3
      Align = alClient
      TabOrder = 0
      TabStop = False
      object TabSheet1: TTabSheet
        Caption = 'Input Parameters'
      end
      object TabSheet2: TTabSheet
        Caption = 'Output Parameters'
        ImageIndex = 1
      end
      object TabSheet3: TTabSheet
        Caption = 'Complex Types'
        ImageIndex = 2
        object Panel2: TPanel
          Left = 240
          Top = 29
          Width = 438
          Height = 392
          Align = alRight
          BevelInner = bvRaised
          BevelOuter = bvLowered
          TabOrder = 0
          object Label2: TLabel
            Left = 15
            Top = 20
            Width = 28
            Height = 13
            Caption = 'Name'
          end
          object edtComplexTypeName: TEdit
            Left = 55
            Top = 15
            Width = 196
            Height = 21
            TabOrder = 0
          end
          object rbnStruct: TRadioButton
            Left = 15
            Top = 55
            Width = 81
            Height = 17
            Caption = 'Structure'
            TabOrder = 1
            OnClick = rbnStructClick
          end
          object rbnArray: TRadioButton
            Left = 15
            Top = 80
            Width = 51
            Height = 17
            Caption = 'Array'
            TabOrder = 2
            OnClick = rbnStructClick
          end
        end
        object ToolBar1: TToolBar
          Left = 0
          Top = 0
          Width = 678
          Height = 29
          AutoSize = True
          BorderWidth = 1
          ButtonHeight = 21
          ButtonWidth = 56
          Caption = 'ToolBar1'
          EdgeBorders = [ebLeft, ebTop, ebRight, ebBottom]
          Flat = True
          ShowCaptions = True
          TabOrder = 1
          object tbnAdd: TToolButton
            Left = 0
            Top = 0
            Caption = '  Append  '
            ImageIndex = 0
            OnClick = tbnAddClick
          end
          object tbnDelete: TToolButton
            Left = 56
            Top = 0
            Caption = '  Delete  '
            ImageIndex = 1
            OnClick = tbnDeleteClick
          end
        end
        object lvwComplexTypes: TListView
          Left = 0
          Top = 29
          Width = 240
          Height = 392
          Align = alClient
          Columns = <
            item
              Width = 150
            end>
          ColumnClick = False
          HideSelection = False
          ReadOnly = True
          TabOrder = 2
          ViewStyle = vsReport
          OnSelectItem = lvwComplexTypesSelectItem
        end
      end
    end
  end
end
