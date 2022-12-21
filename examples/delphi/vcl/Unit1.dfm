object Form1: TForm1
  Left = 0
  Top = 0
  Margins.Left = 10
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
  DesignSize = (
    1025
    502)
  TextHeight = 15
  object Memo1: TMemo
    AlignWithMargins = True
    Left = 638
    Top = 10
    Width = 377
    Height = 442
    Margins.Left = 10
    Margins.Top = 10
    Margins.Right = 10
    Margins.Bottom = 50
    Align = alRight
    ScrollBars = ssBoth
    TabOrder = 0
  end
  object Button3D: TButton
    Left = 726
    Top = 469
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '3D'
    TabOrder = 1
    OnClick = Button3DClick
  end
  object Button2D: TButton
    Left = 835
    Top = 469
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '2D'
    TabOrder = 2
    OnClick = Button2DClick
  end
  object ButtonUI: TButton
    Left = 940
    Top = 469
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'UI'
    TabOrder = 3
    OnClick = ButtonUIClick
  end
  object Timer1: TTimer
    Interval = 10
    OnTimer = Timer1Timer
    Left = 892
    Top = 112
  end
end
