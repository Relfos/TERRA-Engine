object Form1: TForm1
  Left = 192
  Top = 125
  Width = 875
  Height = 691
  Caption = 'Multipanel Demo'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Image1: TImage
    Left = 456
    Top = 104
    Width = 256
    Height = 256
    Transparent = True
  end
  object Panel1: TPanel
    Left = 24
    Top = 16
    Width = 313
    Height = 305
    TabOrder = 0
  end
  object Button1: TButton
    Left = 24
    Top = 336
    Width = 89
    Height = 41
    Caption = 'Load'
    TabOrder = 1
    OnClick = Button1Click
  end
  object OpenDialog1: TOpenDialog
    Filter = 'Image files|*.png'
    Options = [ofHideReadOnly, ofNoChangeDir, ofEnableSizing]
    Left = 24
    Top = 392
  end
end
