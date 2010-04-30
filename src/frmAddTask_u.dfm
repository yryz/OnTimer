object frmAddTask: TfrmAddTask
  Left = 326
  Top = 245
  BorderIcons = [biSystemMenu]
  BorderStyle = bsToolWindow
  Caption = #28155#21152#20219#21153
  ClientHeight = 199
  ClientWidth = 401
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
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
    Width = 383
    Height = 153
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
    object InfoLabel1: TLabel
      Left = 15
      Top = 88
      Width = 34
      Height = 13
      AutoSize = False
      Caption = #20869#23481':'
    end
    object lbl1: TLabel
      Left = 166
      Top = 27
      Width = 31
      Height = 13
      AutoSize = False
      Caption = #27425#25968':'
    end
    object lbl2: TLabel
      Left = 15
      Top = 120
      Width = 30
      Height = 13
      Caption = #21442#25968': '
    end
    object cbbType: TComboBox
      Left = 57
      Top = 24
      Width = 97
      Height = 21
      ItemHeight = 13
      TabOrder = 0
      OnChange = cbbTypeChange
    end
    object edtTime: TEdit
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
      ParentShowHint = False
      ShowHint = False
      TabOrder = 1
    end
    object chkEveryDay: TCheckBox
      Left = 202
      Top = 61
      Width = 57
      Height = 17
      Caption = #27599#26085
      TabOrder = 2
      OnClick = chkEveryDayClick
    end
    object chkLoop: TCheckBox
      Left = 261
      Top = 61
      Width = 59
      Height = 17
      Caption = #24490#29615
      TabOrder = 3
      OnClick = chkEveryDayClick
    end
    object edtContent: TEdit
      Left = 55
      Top = 85
      Width = 314
      Height = 21
      TabOrder = 4
      OnKeyPress = edtParamKeyPress
    end
    object seExecNum: TSpinEdit
      Left = 204
      Top = 24
      Width = 73
      Height = 22
      MaxValue = 999999999
      MinValue = 1
      TabOrder = 5
      Value = 1
    end
    object edtParam: TEdit
      Left = 55
      Top = 117
      Width = 314
      Height = 21
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 6
      OnKeyPress = edtParamKeyPress
    end
  end
  object Button1: TButton
    Left = 110
    Top = 167
    Width = 57
    Height = 25
    Caption = #30830#23450
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 230
    Top = 166
    Width = 57
    Height = 25
    Caption = #21462#28040
    TabOrder = 2
    OnClick = Button2Click
  end
end
