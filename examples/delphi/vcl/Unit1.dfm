object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 502
  ClientWidth = 1025
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OnCreate = FormCreate
  TextHeight = 15
  object Memo1: TMemo
    Left = 638
    Top = 72
    Width = 377
    Height = 377
    Lines.Strings = (
      'Memo1')
    TabOrder = 0
  end
  object Timer1: TTimer
    Interval = 10
    OnTimer = Timer1Timer
    Left = 336
    Top = 96
  end
end
