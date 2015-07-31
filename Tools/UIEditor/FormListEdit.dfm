object ListEditForm: TListEditForm
  Left = 603
  Top = 309
  BorderStyle = bsDialog
  Caption = 'List Editor'
  ClientHeight = 234
  ClientWidth = 352
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object ListBox1: TListBox
    Left = 8
    Top = 8
    Width = 337
    Height = 169
    ItemHeight = 13
    TabOrder = 0
  end
  object Button1: TButton
    Left = 8
    Top = 192
    Width = 89
    Height = 33
    Caption = 'Add'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 112
    Top = 192
    Width = 89
    Height = 33
    Caption = 'Delete'
    TabOrder = 2
  end
  object Button3: TButton
    Left = 256
    Top = 192
    Width = 89
    Height = 33
    Caption = 'Close'
    TabOrder = 3
    OnClick = Button3Click
  end
  object OpenDialog1: TOpenDialog
    Options = [ofHideReadOnly, ofNoChangeDir, ofEnableSizing]
    Left = 208
    Top = 192
  end
end
