object MainForm: TMainForm
  Left = 289
  Top = 115
  AutoSize = True
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'HqResampler Demo'
  ClientHeight = 281
  ClientWidth = 556
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
  object Image: TImage
    AlignWithMargins = True
    Left = 288
    Top = 13
    Width = 256
    Height = 256
    Margins.Left = 12
    Margins.Top = 12
    Margins.Right = 12
    Margins.Bottom = 12
    Proportional = True
    Stretch = True
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 281
    Height = 281
    Align = alCustom
    TabOrder = 0
    object Label1: TLabel
      Left = 8
      Top = 13
      Width = 53
      Height = 13
      Caption = 'Image File:'
    end
    object EdFileName: TEdit
      Left = 8
      Top = 32
      Width = 201
      Height = 21
      ReadOnly = True
      TabOrder = 0
    end
    object Button1: TButton
      Left = 215
      Top = 30
      Width = 58
      Height = 25
      Caption = 'Browse'
      TabOrder = 1
      OnClick = Button1Click
    end
    object Btn2x: TButton
      Left = 8
      Top = 72
      Width = 105
      Height = 25
      Align = alCustom
      Caption = 'hq2x Resample'
      Enabled = False
      TabOrder = 2
      OnClick = Btn2xClick
    end
    object Btn3x: TButton
      Left = 8
      Top = 103
      Width = 105
      Height = 25
      Caption = 'hq3x Resample'
      Enabled = False
      TabOrder = 3
      OnClick = Btn3xClick
    end
    object Btn4x: TButton
      Left = 8
      Top = 134
      Width = 105
      Height = 25
      Caption = 'hq4x Resample'
      Enabled = False
      TabOrder = 4
      OnClick = Btn4xClick
    end
    object Button2: TButton
      Left = 8
      Top = 244
      Width = 75
      Height = 25
      Caption = 'About'
      TabOrder = 5
      OnClick = Button2Click
    end
  end
  object DlgOpen: TOpenPictureDialog
    Left = 216
    Top = 72
  end
end
