object FAbout: TFAbout
  Left = 456
  Top = 264
  Width = 273
  Height = 172
  BorderIcons = [biSystemMenu]
  Caption = #20851#20110
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
  object pnl1: TPanel
    Left = 8
    Top = 8
    Width = 249
    Height = 121
    BevelInner = bvRaised
    BevelOuter = bvLowered
    Color = clWindow
    TabOrder = 0
    object InfoLabelL3: TLabel
      Left = 80
      Top = 77
      Width = 49
      Height = 13
      AutoSize = False
      Caption = #20027#39029': '
    end
    object InfoLabelUrl: TLabel
      Left = 134
      Top = 76
      Width = 84
      Height = 13
      Cursor = crHandPoint
      Caption = 'Www.YrYz.Net'
      Font.Charset = ANSI_CHARSET
      Font.Color = clBlue
      Font.Height = -13
      Font.Name = #26032#23435#20307
      Font.Style = [fsUnderline]
      ParentFont = False
      OnClick = InfoLabelUrlClick
    end
    object InfoLabelQQ: TLabel
      Left = 134
      Top = 52
      Width = 48
      Height = 13
      Cursor = crHelp
      Caption = '83803049'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlue
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsUnderline]
      ParentFont = False
      OnClick = InfoLabelQQClick
    end
    object InfoLabelL2: TLabel
      Left = 80
      Top = 51
      Width = 49
      Height = 13
      AutoSize = False
      Caption = 'Q  Q :'
    end
    object InfoLabelL1: TLabel
      Left = 80
      Top = 27
      Width = 49
      Height = 13
      AutoSize = False
      Caption = #20316#32773':'
    end
    object InfoLabelZZ: TLabel
      Left = 136
      Top = 27
      Width = 65
      Height = 13
      AutoSize = False
      Caption = #21402#24314#21191
    end
  end
end
