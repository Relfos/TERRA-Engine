object UIEditForm: TUIEditForm
  Left = 229
  Top = 146
  Width = 926
  Height = 705
  HorzScrollBar.Visible = False
  VertScrollBar.Visible = False
  Caption = 'TERRA GUI Editor'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu1
  OldCreateOrder = False
  Visible = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object TabList: TIceTabSet
    Left = 0
    Top = 0
    Width = 910
    Height = 41
    Align = alTop
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    SelectedFont.Charset = DEFAULT_CHARSET
    SelectedFont.Color = clBlack
    SelectedFont.Height = -11
    SelectedFont.Name = 'Tahoma'
    SelectedFont.Style = []
    ModifiedFont.Charset = DEFAULT_CHARSET
    ModifiedFont.Color = 11777023
    ModifiedFont.Height = -11
    ModifiedFont.Name = 'Tahoma'
    ModifiedFont.Style = []
    Tabs = <>
    MaintainMenu = False
    ModifiedTabStartColor = 10588280
    ModifiedTabStopColor = 10588280
  end
  object RenderPanel: TPanel
    Left = 208
    Top = 40
    Width = 577
    Height = 401
    TabOrder = 1
    OnMouseDown = RenderPanelMouseDown
  end
  object PropertyList: TValueListEditor
    Left = 0
    Top = 288
    Width = 209
    Height = 193
    TabOrder = 2
    OnValidate = PropertyListValidate
    ColWidths = (
      101
      102)
  end
  object WidgetList: TTreeView
    Left = 0
    Top = 40
    Width = 209
    Height = 249
    Indent = 19
    TabOrder = 3
    OnClick = WidgetListClick
  end
  object MainMenu1: TMainMenu
    Left = 120
    Top = 496
    object Project1: TMenuItem
      Caption = '&Project'
      object New1: TMenuItem
        Caption = '&New'
        object Project2: TMenuItem
          Caption = '&Project'
        end
        object N3: TMenuItem
          Caption = '-'
        end
        object View1: TMenuItem
          Caption = '&View'
        end
        object Component1: TMenuItem
          Caption = '&Component'
        end
      end
      object Open1: TMenuItem
        Caption = '&Open'
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Save1: TMenuItem
        Caption = '&Save'
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Caption = '&Exit'
      end
    end
    object View2: TMenuItem
      Caption = '&View'
      object Add1: TMenuItem
        Caption = '&Add'
        object Button1: TMenuItem
          Caption = '&Button'
          OnClick = Button1Click
        end
        object Label1: TMenuItem
          Caption = '&Label'
        end
        object Window1: TMenuItem
          Caption = '&Window'
        end
        object Checkbox1: TMenuItem
          Caption = '&Checkbox'
        end
        object Radiobox1: TMenuItem
          Caption = '&Radiobox'
        end
        object Combobox1: TMenuItem
          Caption = '&Combobox'
        end
        object Icon1: TMenuItem
          Caption = '&Icon'
        end
        object Sprite1: TMenuItem
          Caption = '&Sprite'
        end
      end
    end
  end
end
