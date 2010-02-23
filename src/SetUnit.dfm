object FSet: TFSet
  Left = 649
  Top = 179
  BorderIcons = [biSystemMenu]
  BorderStyle = bsToolWindow
  Caption = #35774#32622
  ClientHeight = 171
  ClientWidth = 401
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object grpSBox: TGroupBox
    Left = 8
    Top = 8
    Width = 385
    Height = 105
    Caption = #21457#37038#20214#35774#32622
    TabOrder = 0
    object InfoLabel2: TLabel
      Left = 1
      Top = 23
      Width = 50
      Height = 17
      Alignment = taRightJustify
      AutoSize = False
      Caption = #26381#21153#22120':'
    end
    object InfoLabel3: TLabel
      Left = 15
      Top = 63
      Width = 39
      Height = 13
      AutoSize = False
      Caption = #36134' '#25143':'
    end
    object InfoLabel4: TLabel
      Left = 212
      Top = 63
      Width = 39
      Height = 13
      AutoSize = False
      Caption = #23494' '#30721':'
    end
    object InfoLabel1: TLabel
      Left = 216
      Top = 24
      Width = 113
      Height = 13
      Caption = #22914':smtp.qq.com'
    end
    object edtSer: TEdit
      Left = 58
      Top = 21
      Width = 124
      Height = 21
      TabOrder = 1
    end
    object edtUser: TEdit
      Left = 57
      Top = 61
      Width = 125
      Height = 21
      TabOrder = 2
    end
    object edtPass: TEdit
      Left = 256
      Top = 61
      Width = 120
      Height = 21
      PasswordChar = '*'
      TabOrder = 0
      Text = #21704#21704'!'
    end
  end
  object btn1: TButton
    Left = 152
    Top = 127
    Width = 89
    Height = 25
    Caption = #30830#23450
    TabOrder = 1
    OnClick = btn1Click
  end
end
