object frmCountdown: TfrmCountdown
  Left = 365
  Top = 172
  Width = 571
  Height = 130
  BorderIcons = [biSystemMenu, biMinimize]
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  FormStyle = fsStayOnTop
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object pb1: TProgressBar
    Left = 0
    Top = 0
    Width = 555
    Height = 92
    Align = alClient
    Step = 1
    TabOrder = 0
    OnMouseDown = pb1MouseDown
  end
  object tmr1: TTimer
    Enabled = False
    OnTimer = tmr1Timer
    Left = 32
    Top = 24
  end
end
