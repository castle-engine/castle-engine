object MainForm: TMainForm
  Left = 108
  Top = 118
  BorderIcons = [biSystemMenu]
  BorderStyle = bsSingle
  Caption = 'Clipping Test'
  ClientHeight = 652
  ClientWidth = 1036
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object PanelConf: TPanel
    Left = 0
    Top = 0
    Width = 1036
    Height = 497
    Align = alTop
    Color = clSkyBlue
    DragCursor = crDefault
    TabOrder = 0
    object ImageDst: TImage
      Left = 494
      Top = 80
      Width = 450
      Height = 337
      DragCursor = crDefault
      Proportional = True
    end
    object ClipDst: TJvMovableBevel
      Left = 592
      Top = 136
      Width = 329
      Height = 241
      Hint = 'Dest Clip Rect'
      ParentShowHint = False
      Shape = bsFrame
      ShowHint = True
      Style = bsRaised
      BorderSize = 5
    end
    object ImageSrc: TImage
      Left = 64
      Top = 120
      Width = 300
      Height = 225
      DragCursor = crDefault
      Proportional = True
    end
    object SelDst: TJvMovableBevel
      Left = 568
      Top = 208
      Width = 265
      Height = 97
      Hint = 'Dest Selection'
      ParentShowHint = False
      Shape = bsFrame
      ShowHint = True
      Style = bsRaised
    end
    object SelSrc: TJvMovableBevel
      Left = 128
      Top = 176
      Width = 177
      Height = 97
      Hint = 'Source Selection'
      ParentShowHint = False
      Shape = bsFrame
      ShowHint = True
      Style = bsRaised
    end
  end
  object PanelCmd: TPanel
    Left = 0
    Top = 497
    Width = 1036
    Height = 155
    Align = alClient
    TabOrder = 1
    object Button1: TButton
      Left = 424
      Top = 16
      Width = 137
      Height = 25
      Caption = 'Reset Copy Selections'
      TabOrder = 0
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 424
      Top = 47
      Width = 137
      Height = 25
      Caption = 'Reset Stretch Selections'
      TabOrder = 1
      OnClick = Button2Click
    end
    object Button3: TButton
      Left = 344
      Top = 96
      Width = 145
      Height = 25
      Caption = 'CopyRect Test'
      TabOrder = 2
      OnClick = Button3Click
    end
    object Button4: TButton
      Left = 512
      Top = 96
      Width = 145
      Height = 25
      Caption = 'StretchRect Test'
      TabOrder = 3
      OnClick = Button4Click
    end
    object Button5: TButton
      Left = 344
      Top = 127
      Width = 145
      Height = 25
      Caption = 'Canvas.Draw Test'
      TabOrder = 4
      OnClick = Button5Click
    end
    object Button6: TButton
      Left = 512
      Top = 127
      Width = 145
      Height = 25
      Caption = 'Canvas.StretchDraw Test'
      TabOrder = 5
      OnClick = Button6Click
    end
    object CheckGenCanvas: TCheckBox
      Left = 704
      Top = 24
      Width = 209
      Height = 17
      Caption = 'Force generic canvas class'
      TabOrder = 6
    end
  end
end
