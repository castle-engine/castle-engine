object ViewForm: TViewForm
  Left = 128
  Top = 232
  AutoSize = True
  BorderIcons = [biSystemMenu, biMinimize]
  Caption = 'View Form'
  ClientHeight = 357
  ClientWidth = 642
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
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Image: TImage
    Left = 0
    Top = 39
    Width = 642
    Height = 318
    Align = alCustom
    Proportional = True
    Stretch = True
  end
  object PnlBle: TPanel
    Left = 0
    Top = 0
    Width = 642
    Height = 41
    Align = alTop
    TabOrder = 0
    ExplicitLeft = 8
    DesignSize = (
      642
      41)
    object Button1: TButton
      Left = 257
      Top = 8
      Width = 129
      Height = 25
      Anchors = [akLeft, akTop, akRight]
      Caption = 'Save To File'
      TabOrder = 0
      OnClick = Button1Click
    end
  end
  object DlgSave: TSavePictureDialog
    Left = 528
    Top = 64
  end
end
