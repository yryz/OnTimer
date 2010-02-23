object AddTaskF: TAddTaskF
  Left = 832
  Top = 170
  BorderIcons = [biSystemMenu]
  BorderStyle = bsToolWindow
  Caption = #28155#21152#20219#21153
  ClientHeight = 199
  ClientWidth = 415
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object GroupBox1: TGroupBox
    Left = 11
    Top = 10
    Width = 393
    Height = 148
    Caption = #28155#21152#20219#21153
    TabOrder = 0
    object Label1: TLabel
      Left = 16
      Top = 57
      Width = 30
      Height = 13
      Caption = #26102#38388': '
    end
    object Label2: TLabel
      Left = 15
      Top = 27
      Width = 30
      Height = 13
      Caption = #31867#22411': '
    end
    object Label3: TLabel
      Left = 15
      Top = 88
      Width = 30
      Height = 13
      Caption = #21442#25968': '
    end
    object InfoLabel1: TLabel
      Left = 15
      Top = 120
      Width = 34
      Height = 13
      AutoSize = False
      Caption = #20869#23481':'
    end
    object ComboBox1: TComboBox
      Left = 57
      Top = 24
      Width = 97
      Height = 21
      ItemHeight = 13
      TabOrder = 0
    end
    object edt2: TEdit
      Left = 55
      Top = 85
      Width = 314
      Height = 24
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 3
      OnKeyPress = edt2KeyPress
    end
    object edt1: TEdit
      Left = 56
      Top = 53
      Width = 137
      Height = 24
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      MaxLength = 19
      ParentFont = False
      TabOrder = 1
    end
    object CheckBox1: TCheckBox
      Left = 202
      Top = 61
      Width = 57
      Height = 17
      Caption = #27599#26085
      TabOrder = 2
      OnClick = CheckBox1Click
    end
    object CheckBox2: TCheckBox
      Left = 261
      Top = 61
      Width = 59
      Height = 17
      Caption = #24490#29615
      TabOrder = 4
      OnClick = CheckBox1Click
    end
    object edt3: TEdit
      Left = 55
      Top = 117
      Width = 314
      Height = 24
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 5
      OnKeyPress = edt2KeyPress
    end
  end
  object Button1: TButton
    Left = 105
    Top = 163
    Width = 75
    Height = 25
    Caption = #30830#23450
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 257
    Top = 162
    Width = 75
    Height = 25
    Caption = #21462#28040
    TabOrder = 2
    OnClick = Button2Click
  end
end
