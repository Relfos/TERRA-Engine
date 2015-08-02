object DataSourceBrowserForm: TDataSourceBrowserForm
  Left = 562
  Top = 365
  BorderStyle = bsDialog
  Caption = 'DataSourceBrowserForm'
  ClientHeight = 383
  ClientWidth = 276
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object TreeView1: TTreeView
    Left = 8
    Top = 8
    Width = 257
    Height = 281
    Indent = 19
    TabOrder = 0
    OnClick = TreeView1Click
  end
  object Button1: TButton
    Left = 152
    Top = 344
    Width = 113
    Height = 33
    Caption = 'Cancel'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 8
    Top = 344
    Width = 113
    Height = 33
    Caption = 'Select'
    TabOrder = 2
    OnClick = Button2Click
  end
  object Edit1: TEdit
    Left = 8
    Top = 296
    Width = 257
    Height = 33
    TabOrder = 3
    Text = 'Edit1'
  end
end
