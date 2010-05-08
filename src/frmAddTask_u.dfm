object frmAddTask: TfrmAddTask
  Left = 489
  Top = 309
  BorderIcons = [biSystemMenu]
  BorderStyle = bsNone
  Caption = #28155#21152#20219#21153
  ClientHeight = 190
  ClientWidth = 373
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnMouseDown = FormMouseDown
  OnPaint = FormPaint
  OnShow = FormPaint
  PixelsPerInch = 96
  TextHeight = 13
  object grpTask: TGroupBox
    Left = 12
    Top = 8
    Width = 350
    Height = 144
    Caption = #20219#21153#32534#36753
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'MS Sans Serif'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    OnMouseDown = FormMouseDown
    object Label1: TLabel
      Left = 5
      Top = 55
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
      Top = 26
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
      Top = 86
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
      Left = 149
      Top = 26
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
    object lbl2: TLabel
      Left = 5
      Top = 115
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
    object seExecNum: TSpinEdit
      Left = 192
      Top = 21
      Width = 65
      Height = 22
      MaxLength = 9
      MaxValue = 999999999
      MinValue = 1
      TabOrder = 1
      Value = 1
    end
    object edtTime: TEdit
      Left = 49
      Top = 51
      Width = 137
      Height = 24
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -13
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ImeName = #20013#25991' ('#31616#20307') - '#32654#24335#38190#30424
      MaxLength = 19
      ParentFont = False
      ParentShowHint = False
      ShowHint = False
      TabOrder = 2
    end
    object chkEveryDay: TCheckBox
      Left = 196
      Top = 57
      Width = 52
      Height = 17
      Caption = #27599#26085
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 3
      OnClick = chkEveryDayClick
    end
    object chkLoop: TCheckBox
      Left = 255
      Top = 57
      Width = 52
      Height = 17
      Caption = #24490#29615
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ParentFont = False
      TabOrder = 4
      OnClick = chkEveryDayClick
    end
    object edtContent: TEdit
      Left = 49
      Top = 84
      Width = 290
      Height = 21
      ImeName = #20013#25991' ('#31616#20307') - '#32654#24335#38190#30424
      TabOrder = 5
    end
    object edtParam: TEdit
      Left = 49
      Top = 113
      Width = 290
      Height = 21
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -12
      Font.Name = 'MS Sans Serif'
      Font.Style = []
      ImeName = #20013#25991' ('#31616#20307') - '#32654#24335#38190#30424
      ParentFont = False
      TabOrder = 6
    end
    object cbbType: TComboBox
      Left = 49
      Top = 22
      Width = 97
      Height = 21
      ImeName = #20013#25991' ('#31616#20307') - '#32654#24335#38190#30424
      ItemHeight = 13
      TabOrder = 0
      OnChange = cbbTypeChange
    end
    object chkActive: TCheckBox
      Left = 274
      Top = 24
      Width = 52
      Height = 17
      Caption = #28608#27963
      Checked = True
      State = cbChecked
      TabOrder = 7
    end
  end
  object btnOk: TButton
    Left = 102
    Top = 157
    Width = 57
    Height = 24
    Caption = #30830#23450
    TabOrder = 2
    OnClick = btnOkClick
  end
  object btnCancel: TButton
    Left = 222
    Top = 156
    Width = 57
    Height = 25
    Cancel = True
    Caption = #21462#28040
    ModalResult = 2
    TabOrder = 1
  end
end
