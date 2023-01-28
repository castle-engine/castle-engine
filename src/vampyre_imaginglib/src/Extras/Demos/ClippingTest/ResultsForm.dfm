object ResultForm: TResultForm
  Left = 86
  Top = 156
  BorderStyle = bsToolWindow
  Caption = 'Results'
  ClientHeight = 388
  ClientWidth = 994
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnMouseDown = FormMouseDown
  PixelsPerInch = 96
  TextHeight = 13
  object ImageMy: TImage
    Left = 30
    Top = 35
    Width = 450
    Height = 337
    DragCursor = crDefault
    Proportional = True
    OnMouseDown = ImageMyMouseDown
  end
  object ImageWin: TImage
    Left = 510
    Top = 35
    Width = 450
    Height = 337
    DragCursor = crDefault
    Proportional = True
    OnMouseDown = ImageWinMouseDown
  end
  object Label1: TLabel
    Left = 30
    Top = 16
    Width = 99
    Height = 13
    Caption = 'Drawing by Imaging:'
  end
  object Label2: TLabel
    Left = 510
    Top = 16
    Width = 96
    Height = 13
    Caption = 'Drawing by WinAPI:'
  end
end
