object WSDLWizardForm: TWSDLWizardForm
  Left = 481
  Top = 64
  BorderStyle = bsDialog
  Caption = 'WSDL Import Wizard'
  ClientHeight = 310
  ClientWidth = 391
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  PixelsPerInch = 96
  TextHeight = 13
  object TBevel
    Left = 1
    Top = 216
    Width = 388
    Height = 50
    Shape = bsBottomLine
  end
  object CancelButton: TButton
    Left = 310
    Top = 277
    Width = 73
    Height = 24
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 0
  end
  object NextButton: TButton
    Left = 223
    Top = 277
    Width = 73
    Height = 24
    Action = NextAction
    Caption = '&Next >'
    Default = True
    TabOrder = 2
  end
  object PriorButton: TButton
    Left = 150
    Top = 277
    Width = 73
    Height = 24
    Action = PriorAction
    Caption = '< &Previous'
    TabOrder = 3
  end
  object WizardPages: TPageControl
    Left = 0
    Top = 0
    Width = 391
    Height = 257
    ActivePage = TabSheetResults
    Align = alTop
    Style = tsFlatButtons
    TabOrder = 1
    object TabSheetGetWSDL: TTabSheet
      Caption = 'TabSheetGetWSDL'
      TabVisible = False
      object ServerRadioButton: TRadioButton
        Left = 8
        Top = 2
        Width = 145
        Height = 17
        Caption = 'Get &WSDL from server'
        Checked = True
        TabOrder = 0
        TabStop = True
        OnClick = ServerRadioButtonClick
      end
      object FileRadioButton: TRadioButton
        Left = 8
        Top = 22
        Width = 145
        Height = 17
        Caption = '&Get WSDL from local file'
        TabOrder = 1
        OnClick = ServerRadioButtonClick
      end
      object PageControlWSDL: TPageControl
        Left = -2
        Top = 42
        Width = 387
        Height = 209
        ActivePage = TabSheetHTTP
        Style = tsFlatButtons
        TabOrder = 2
        object TabSheetHTTP: TTabSheet
          Caption = 'TabSheet1'
          TabVisible = False
          object TGroupBox
            Left = 1
            Top = 0
            Width = 377
            Height = 112
            Caption = ' SOAP Server '
            TabOrder = 0
            object TLabel
              Left = 8
              Top = 20
              Width = 75
              Height = 13
              Caption = 'WSDL &address:'
              FocusControl = ServerAddressEdit
            end
            object TLabel
              Left = 8
              Top = 60
              Width = 51
              Height = 13
              Caption = '&Username:'
              FocusControl = ServerUsernameEdit
            end
            object TLabel
              Left = 192
              Top = 60
              Width = 49
              Height = 13
              Caption = '&Password:'
              FocusControl = ServerPasswordEdit
            end
            object ServerAddressEdit: TEdit
              Left = 8
              Top = 36
              Width = 359
              Height = 21
              Hint = 'Enter server address'
              TabOrder = 0
            end
            object ServerUsernameEdit: TEdit
              Left = 8
              Top = 76
              Width = 177
              Height = 21
              Hint = 'Enter username'
              TabOrder = 1
            end
            object ServerPasswordEdit: TEdit
              Left = 192
              Top = 76
              Width = 175
              Height = 21
              Hint = 'Enter password'
              TabOrder = 2
            end
          end
          object TGroupBox
            Left = 1
            Top = 120
            Width = 377
            Height = 73
            Caption = ' Proxy '
            TabOrder = 1
            object TLabel
              Left = 8
              Top = 20
              Width = 69
              Height = 13
              Caption = 'P&roxy address:'
              FocusControl = ProxyAddressEdit
            end
            object TLabel
              Left = 296
              Top = 20
              Width = 22
              Height = 13
              Caption = 'P&ort:'
            end
            object ProxyAddressEdit: TEdit
              Left = 8
              Top = 36
              Width = 281
              Height = 21
              Hint = 'Enter proxy address'
              TabOrder = 0
            end
            object ProxyPortEdit: TEdit
              Left = 296
              Top = 36
              Width = 55
              Height = 21
              TabOrder = 1
              Text = '80'
            end
            object ProxyPortUpDown: TUpDown
              Left = 351
              Top = 36
              Width = 15
              Height = 21
              Associate = ProxyPortEdit
              Min = 0
              Max = 32767
              Position = 80
              TabOrder = 2
              Wrap = False
            end
          end
        end
        object TabSheetFile: TTabSheet
          Caption = 'TabSheet2'
          ImageIndex = 1
          TabVisible = False
          object TGroupBox
            Left = 1
            Top = 0
            Width = 377
            Height = 112
            Caption = 'Select local file with WSDL:'
            TabOrder = 0
            object TLabel
              Left = 8
              Top = 28
              Width = 76
              Height = 13
              Caption = '&File with WSDL:'
            end
            object FileNameEdit: TEdit
              Left = 8
              Top = 44
              Width = 359
              Height = 21
              TabOrder = 0
            end
            object FileBrowseBtn: TButton
              Left = 288
              Top = 72
              Width = 79
              Height = 24
              Caption = '&Browse ...'
              TabOrder = 1
              OnClick = FileBrowseBtnClick
            end
          end
        end
      end
    end
    object TabSheetProgress: TTabSheet
      Caption = 'TabSheetProgress'
      ImageIndex = 3
      TabVisible = False
      object TLabel
        Left = 8
        Top = 8
        Width = 44
        Height = 13
        Caption = 'Progress:'
      end
      object ProgressListBox: TListBox
        Left = 8
        Top = 24
        Width = 369
        Height = 217
        ItemHeight = 13
        TabOrder = 0
      end
    end
    object TabSheetOptions: TTabSheet
      Caption = 'TabSheetOptions'
      ImageIndex = 1
      TabVisible = False
      object TGroupBox
        Left = 4
        Top = 4
        Width = 373
        Height = 49
        Caption = ' Options '
        TabOrder = 0
        object StringsCheckBox: TCheckBox
          Left = 12
          Top = 20
          Width = 173
          Height = 17
          Caption = 'Map &Strings to WideString'
          TabOrder = 0
        end
      end
    end
    object TabSheetResults: TTabSheet
      Caption = 'TabSheetResults'
      ImageIndex = 2
      TabVisible = False
      object TLabel
        Left = 8
        Top = 8
        Width = 69
        Height = 13
        Caption = 'Code Preview:'
      end
      object CodeListBox: TMemo
        Left = 8
        Top = 24
        Width = 369
        Height = 217
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clWindowText
        Font.Height = -11
        Font.Name = 'Courier New'
        Font.Style = []
        ParentFont = False
        ScrollBars = ssBoth
        TabOrder = 0
        WordWrap = False
      end
    end
  end
  object OpenDialogWSDL: TOpenDialog
    DefaultExt = '.wsdl'
    Filter = 
      'WSDL files (*.wsdl)|*.wsdl|XML files (*.xml)|*.xml|All files (*.' +
      '*)|*.*'
    InitialDir = 'D:\Projects\SOAP\AstaWSDL\Tests'
    Options = [ofHideReadOnly, ofPathMustExist, ofFileMustExist, ofEnableSizing]
    Title = 'Open WDSL file'
    Left = 350
    Top = 8
  end
  object ActionList: TActionList
    Left = 316
    Top = 8
    object NextAction: TAction
      Caption = 'Next >'
      OnExecute = NextActionExecute
      OnUpdate = NextActionUpdate
    end
    object PriorAction: TAction
      Caption = '< Previous'
      OnExecute = PriorActionExecute
      OnUpdate = PriorActionUpdate
    end
  end
  object AstaHTTPConnection: TAstaHTTPConnection
    Port = 80
    Page = '/'
    Method = 'GET'
    ProxyPort = 3128
    UseSSL = False
    KeepAlive = True
    OnConnect = AstaHTTPConnectionConnect
    OnDisconnect = AstaHTTPConnectionDisconnect
    OnRequest = AstaHTTPConnectionRequest
    OnResponse = AstaHTTPConnectionResponse
    Left = 283
    Top = 8
  end
end
