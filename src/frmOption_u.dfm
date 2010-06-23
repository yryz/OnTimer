object frmOption: TfrmOption
  Left = 359
  Top = 292
  BorderIcons = [biSystemMenu]
  BorderStyle = bsNone
  Caption = #35774#32622
  ClientHeight = 148
  ClientWidth = 267
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnMouseDown = FormMouseDown
  OnPaint = FormPaint
  OnShow = FormPaint
  PixelsPerInch = 96
  TextHeight = 13
  object grpSBox: TGroupBox
    Left = 8
    Top = 8
    Width = 249
    Height = 129
    Caption = #35774#32622'(SMTP'#12289#28909#38190')'
    TabOrder = 0
    OnMouseDown = FormMouseDown
    object InfoLabel2: TLabel
      Left = 6
      Top = 24
      Width = 67
      Height = 17
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'SMTP'#20027#26426':'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 13977088
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object InfoLabel3: TLabel
      Left = 6
      Top = 50
      Width = 67
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'SMTP'#36134#25143':'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 13977088
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object InfoLabel4: TLabel
      Left = 6
      Top = 74
      Width = 67
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = 'SMTP'#23494#30721':'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 13977088
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object lbl1: TLabel
      Left = 6
      Top = 100
      Width = 67
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = #31243#24207#28909#38190':'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 13977088
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object edtServer: TEdit
      Left = 75
      Top = 22
      Width = 120
      Height = 21
      Hint = #22914'smtp.126.com'
      ImeName = #20013#25991' ('#31616#20307') - '#32654#24335#38190#30424
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
    end
    object edtUser: TEdit
      Left = 75
      Top = 46
      Width = 120
      Height = 21
      ImeName = #20013#25991' ('#31616#20307') - '#32654#24335#38190#30424
      TabOrder = 2
    end
    object edtPass: TEdit
      Left = 75
      Top = 70
      Width = 120
      Height = 21
      ImeName = #20013#25991' ('#31616#20307') - '#32654#24335#38190#30424
      PasswordChar = '*'
      TabOrder = 3
      OnKeyPress = edtPassKeyPress
    end
    object sePort: TSpinEdit
      Left = 201
      Top = 22
      Width = 43
      Height = 22
      Hint = 'SMTP'#31471#21475','#19968#33324#20026'25'
      MaxValue = 65535
      MinValue = 25
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      Value = 25
    end
    object btnOk: TButton
      Left = 200
      Top = 51
      Width = 43
      Height = 28
      Caption = #30830#23450
      ModalResult = 1
      TabOrder = 4
      OnClick = btnOkClick
    end
    object btnCancel: TButton
      Left = 200
      Top = 88
      Width = 43
      Height = 28
      Cancel = True
      Caption = #21462#28040
      ModalResult = 2
      TabOrder = 5
    end
    object hk1: THotKey
      Left = 75
      Top = 97
      Width = 120
      Height = 19
      HotKey = 16496
      Modifiers = [hkCtrl]
      TabOrder = 6
    end
  end
end
