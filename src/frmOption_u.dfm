object frmOption: TfrmOption
  Left = 359
  Top = 292
  BorderIcons = [biSystemMenu]
  BorderStyle = bsNone
  Caption = #35774#32622
  ClientHeight = 132
  ClientWidth = 250
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
    Width = 233
    Height = 113
    Caption = #21457#36865#37038#20214#35774#32622'(SMTP)'
    TabOrder = 0
    OnMouseDown = FormMouseDown
    object InfoLabel2: TLabel
      Left = 6
      Top = 24
      Width = 46
      Height = 17
      Alignment = taRightJustify
      AutoSize = False
      Caption = #26381#21153#22120':'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 13977088
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object InfoLabel3: TLabel
      Left = 6
      Top = 56
      Width = 46
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = #36134' '#25143':'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 13977088
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object InfoLabel4: TLabel
      Left = 6
      Top = 85
      Width = 46
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = #23494' '#30721':'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 13977088
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object edtServer: TEdit
      Left = 57
      Top = 22
      Width = 124
      Height = 21
      Hint = #22914'smtp.126.com'
      ImeName = #20013#25991' ('#31616#20307') - '#32654#24335#38190#30424
      ParentShowHint = False
      ShowHint = True
      TabOrder = 0
    end
    object edtUser: TEdit
      Left = 57
      Top = 52
      Width = 124
      Height = 21
      ImeName = #20013#25991' ('#31616#20307') - '#32654#24335#38190#30424
      TabOrder = 2
    end
    object edtPass: TEdit
      Left = 57
      Top = 81
      Width = 124
      Height = 21
      ImeName = #20013#25991' ('#31616#20307') - '#32654#24335#38190#30424
      PasswordChar = '*'
      TabOrder = 3
      OnKeyPress = edtPassKeyPress
    end
    object sePort: TSpinEdit
      Left = 185
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
      Left = 184
      Top = 51
      Width = 43
      Height = 23
      Caption = #30830#23450
      ModalResult = 1
      TabOrder = 4
      OnClick = btnOkClick
    end
    object btnCancel: TButton
      Left = 184
      Top = 80
      Width = 43
      Height = 23
      Cancel = True
      Caption = #21462#28040
      ModalResult = 2
      TabOrder = 5
    end
  end
end
