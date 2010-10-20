object frmAddTask: TfrmAddTask
  Left = 306
  Top = 265
  BorderIcons = [biSystemMenu]
  BorderStyle = bsNone
  Caption = #28155#21152#20219#21153
  ClientHeight = 195
  ClientWidth = 374
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
    Width = 350
    Height = 145
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
      Top = 95
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
      Top = 119
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
      Hint = #31243#24207#36816#34892#26399#38388#65292#26368#22823#25191#34892#27425#25968
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = #23435#20307
      Font.Style = []
      MaxLength = 9
      MaxValue = 999999999
      MinValue = 1
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
      Height = 21
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
      Left = 236
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
      TabOrder = 5
      OnClick = chkEveryDayClick
    end
    object edtContent: TEdit
      Left = 48
      Top = 93
      Width = 290
      Height = 20
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = #23435#20307
      Font.Style = []
      ImeName = #20013#25991' ('#31616#20307') - '#32654#24335#38190#30424
      ParentFont = False
      TabOrder = 14
    end
    object edtParam: TEdit
      Left = 48
      Top = 117
      Width = 290
      Height = 20
      Font.Charset = ANSI_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = #23435#20307
      Font.Style = []
      ImeName = #20013#25991' ('#31616#20307') - '#32654#24335#38190#30424
      ParentFont = False
      TabOrder = 15
    end
    object chkActive: TCheckBox
      Left = 262
      Top = 23
      Width = 52
      Height = 17
      Hint = #28155#21152#21518#21363#29983#25928
      Caption = #28608#27963
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
    object chkWeek: TCheckBox
      Left = 292
      Top = 48
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
      TabOrder = 6
      OnClick = chkWeekClick
    end
    object chkMon: TCheckBox
      Left = 48
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
      TabOrder = 7
    end
    object chkTue: TCheckBox
      Left = 91
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
      TabOrder = 8
    end
    object chkWed: TCheckBox
      Left = 133
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
      TabOrder = 9
    end
    object chkThu: TCheckBox
      Left = 176
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
      TabOrder = 10
    end
    object chkFri: TCheckBox
      Left = 219
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
      TabOrder = 11
    end
    object chkSat: TCheckBox
      Left = 261
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
      TabOrder = 12
    end
    object chkSun: TCheckBox
      Left = 304
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
      TabOrder = 13
    end
  end
  object btnOk: TButton
    Left = 60
    Top = 160
    Width = 173
    Height = 24
    Caption = #30830#23450
    TabOrder = 1
    OnClick = btnOkClick
  end
  object btnCancel: TButton
    Left = 248
    Top = 160
    Width = 102
    Height = 24
    Cancel = True
    Caption = #21462#28040
    ModalResult = 2
    TabOrder = 2
  end
end
