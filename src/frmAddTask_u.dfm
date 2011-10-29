object frmAddTask: TfrmAddTask
  Left = 446
  Top = 249
  BorderIcons = [biSystemMenu]
  BorderStyle = bsNone
  Caption = #28155#21152#20219#21153
  ClientHeight = 293
  ClientWidth = 424
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = #23435#20307
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnMouseDown = FormMouseDown
  OnPaint = FormPaint
  OnShow = FormPaint
  PixelsPerInch = 96
  TextHeight = 12
  object grpTask: TGroupBox
    Left = 12
    Top = 8
    Width = 400
    Height = 240
    Caption = #20219#21153#32534#36753
    TabOrder = 0
    OnMouseDown = FormMouseDown
    object Label1: TLabel
      Left = 5
      Top = 48
      Width = 38
      Height = 16
      Alignment = taRightJustify
      AutoSize = False
      Caption = #26102#38388':'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 13977088
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object Label2: TLabel
      Left = 5
      Top = 25
      Width = 38
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = #31867#22411':'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 13977088
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object InfoLabel1: TLabel
      Left = 5
      Top = 118
      Width = 38
      Height = 16
      Alignment = taRightJustify
      AutoSize = False
      Caption = #20869#23481':'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 13977088
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object lbl1: TLabel
      Left = 137
      Top = 25
      Width = 38
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = #27425#25968':'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 13977088
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object lblParam: TLabel
      Left = 5
      Top = 212
      Width = 38
      Height = 16
      Alignment = taRightJustify
      AutoSize = False
      Caption = #21442#25968':'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 13977088
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object lbl2: TLabel
      Left = 5
      Top = 98
      Width = 38
      Height = 13
      Alignment = taRightJustify
      AutoSize = False
      Caption = #20998#31867':'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 13977088
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
    end
    object cbbType: TComboBox
      Left = 48
      Top = 21
      Width = 82
      Height = 20
      Style = csDropDownList
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = #23435#20307
      Font.Style = []
      ImeName = #20013#25991' ('#31616#20307') - '#32654#24335#38190#30424
      ItemHeight = 12
      ParentFont = False
      TabOrder = 0
      OnChange = cbbTypeChange
    end
    object seExecNum: TSpinEdit
      Left = 180
      Top = 21
      Width = 65
      Height = 21
      Hint = #25191#34892#27425#25968', -1'#19981#38480
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = #23435#20307
      Font.Style = []
      MaxLength = 9
      MaxValue = 999999999
      MinValue = -1
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      TabOrder = 1
      Value = 1
    end
    object edtTime: TEdit
      Left = 48
      Top = 46
      Width = 125
      Height = 20
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = #23435#20307
      Font.Style = []
      ImeName = #20013#25991' ('#31616#20307') - '#32654#24335#38190#30424
      MaxLength = 19
      ParentFont = False
      ParentShowHint = False
      ShowHint = False
      TabOrder = 3
    end
    object chkEveryDay: TCheckBox
      Left = 180
      Top = 48
      Width = 48
      Height = 17
      Caption = #27599#26085
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 4
      OnClick = chkEveryDayClick
    end
    object chkLoop: TCheckBox
      Left = 276
      Top = 48
      Width = 48
      Height = 17
      Caption = #24490#29615
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 6
      OnClick = chkEveryDayClick
    end
    object edtParam: TEdit
      Left = 48
      Top = 210
      Width = 337
      Height = 20
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = #23435#20307
      Font.Style = []
      ImeName = #20013#25991' ('#31616#20307') - '#32654#24335#38190#30424
      ParentFont = False
      ParentShowHint = False
      ShowHint = False
      TabOrder = 17
      OnEnter = edtParamEnter
      OnExit = mmoContentExit
    end
    object chkWeek: TCheckBox
      Left = 48
      Top = 72
      Width = 48
      Height = 17
      Caption = #26143#26399
      Enabled = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 7
      OnClick = chkWeekClick
    end
    object chkMon: TCheckBox
      Left = 103
      Top = 72
      Width = 35
      Height = 17
      Caption = #19968
      Enabled = False
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = #23435#20307
      Font.Style = []
      ParentFont = False
      TabOrder = 8
    end
    object chkTue: TCheckBox
      Left = 144
      Top = 72
      Width = 35
      Height = 17
      Caption = #20108
      Enabled = False
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = #23435#20307
      Font.Style = []
      ParentFont = False
      TabOrder = 9
    end
    object chkWed: TCheckBox
      Left = 186
      Top = 72
      Width = 35
      Height = 17
      Caption = #19977
      Enabled = False
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = #23435#20307
      Font.Style = []
      ParentFont = False
      TabOrder = 10
    end
    object chkThu: TCheckBox
      Left = 227
      Top = 72
      Width = 35
      Height = 17
      Caption = #22235
      Enabled = False
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = #23435#20307
      Font.Style = []
      ParentFont = False
      TabOrder = 11
    end
    object chkFri: TCheckBox
      Left = 269
      Top = 72
      Width = 35
      Height = 17
      Caption = #20116
      Enabled = False
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = #23435#20307
      Font.Style = []
      ParentFont = False
      TabOrder = 12
    end
    object chkSat: TCheckBox
      Left = 310
      Top = 72
      Width = 35
      Height = 17
      Caption = #20845
      Enabled = False
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = #23435#20307
      Font.Style = []
      ParentFont = False
      TabOrder = 13
    end
    object chkSun: TCheckBox
      Left = 352
      Top = 72
      Width = 35
      Height = 17
      Caption = #19971
      Enabled = False
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = #23435#20307
      Font.Style = []
      ParentFont = False
      TabOrder = 14
    end
    object chkTmpExecNum: TCheckBox
      Left = 262
      Top = 23
      Width = 75
      Height = 17
      Hint = #36719#20214#20851#38381#21518#65292#21097#20313#27425#25968#22797#20301
      Caption = #20869#23384#35745#25968
      Checked = True
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = #23435#20307
      Font.Style = []
      ParentFont = False
      ParentShowHint = False
      ShowHint = True
      State = cbChecked
      TabOrder = 2
    end
    object chkMonthly: TCheckBox
      Left = 228
      Top = 48
      Width = 48
      Height = 17
      Caption = #27599#26376
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 5
      OnClick = chkEveryDayClick
    end
    object cbbClass: TComboBox
      Left = 48
      Top = 95
      Width = 337
      Height = 20
      Style = csDropDownList
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = #23435#20307
      Font.Style = []
      ImeName = #20013#25991' ('#31616#20307') - '#32654#24335#38190#30424
      ItemHeight = 12
      ParentFont = False
      TabOrder = 15
      OnChange = cbbTypeChange
    end
    object mmoContent: TMemo
      Left = 48
      Top = 119
      Width = 337
      Height = 87
      ImeName = 'Chinese (Simplified) - US Keyboard'
      ParentShowHint = False
      ScrollBars = ssVertical
      ShowHint = True
      TabOrder = 16
      OnEnter = mmoContentEnter
      OnExit = mmoContentExit
    end
  end
  object btnOk: TButton
    Left = 152
    Top = 256
    Width = 113
    Height = 24
    Caption = #30830#23450
    TabOrder = 1
    OnClick = btnOkClick
  end
  object btnCancel: TButton
    Left = 288
    Top = 256
    Width = 102
    Height = 24
    Cancel = True
    Caption = #21462#28040
    ModalResult = 2
    TabOrder = 2
  end
  object chkActive: TCheckBox
    Left = 50
    Top = 259
    Width = 75
    Height = 17
    Hint = #28155#21152#21518#21363#29983#25928
    Caption = #28608#27963#20219#21153
    Checked = True
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = #23435#20307
    Font.Style = []
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    State = cbChecked
    TabOrder = 3
  end
end
