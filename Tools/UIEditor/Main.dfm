object UIEditForm: TUIEditForm
  Left = 229
  Top = 146
  Width = 926
  Height = 705
  HorzScrollBar.Visible = False
  VertScrollBar.Visible = False
  Caption = 'TERRA GUI Editor'
  Color = 2500134
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  Menu = MainMenu
  OldCreateOrder = False
  Visible = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  OnMouseWheel = FormMouseWheel
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
    OnMouseMove = RenderPanelMouseMove
    OnMouseUp = RenderPanelMouseUp
  end
  object WidgetList: TTreeView
    Left = 0
    Top = 40
    Width = 209
    Height = 249
    Indent = 19
    TabOrder = 2
    OnClick = WidgetListClick
    OnEdited = WidgetListEdited
    OnMouseDown = WidgetListMouseDown
  end
  object PropertyList: TCustomPropertyEditor
    Left = 0
    Top = 288
    Width = 209
    Height = 169
    BevelOuter = bvLowered
    BevelWidth = 2
    Ctl3D = False
    ParentCtl3D = False
    TabOrder = 3
  end
  object MainMenu: TMainMenu
    OwnerDraw = True
    Left = 120
    Top = 496
    object ProjectMenu: TMenuItem
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
        OnClick = Open1Click
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Save1: TMenuItem
        Caption = '&Save'
        OnClick = Save1Click
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Caption = '&Exit'
      end
    end
    object WidgetMenu: TMenuItem
      Caption = '&Widgets'
      object Button1: TMenuItem
        Caption = '&Button'
        OnClick = Button1Click
      end
      object Label1: TMenuItem
        Caption = '&Label'
        OnClick = Label1Click
      end
      object Window1: TMenuItem
        Caption = '&Window'
        OnClick = Window1Click
      end
      object Checkbox1: TMenuItem
        Caption = '&Checkbox'
        OnClick = Checkbox1Click
      end
      object Radiobox1: TMenuItem
        Caption = '&Radiobox'
        OnClick = Radiobox1Click
      end
      object Combobox1: TMenuItem
        Caption = '&Combobox'
        OnClick = Combobox1Click
      end
      object Icon1: TMenuItem
        Caption = '&Icon'
      end
      object Sprite1: TMenuItem
        Caption = '&Sprite'
        OnClick = Sprite1Click
      end
      object ProgressBar1: TMenuItem
        Caption = '&Progress Bar'
        OnClick = ProgressBar1Click
      end
    end
    object ViewMenu: TMenuItem
      Caption = '&View'
      object GridMenu: TMenuItem
        Caption = '&Grid'
        object GridOffMenu: TMenuItem
          Caption = '&Off'
          OnClick = GridOffMenuClick
        end
        object GridSmallMenu: TMenuItem
          Caption = '&Small'
          OnClick = GridSmallMenuClick
        end
        object GridMediumMenu: TMenuItem
          Caption = '&Medium'
          OnClick = GridMediumMenuClick
        end
        object GridLargeMenu: TMenuItem
          Caption = '&Large'
          OnClick = GridLargeMenuClick
        end
      end
    end
  end
  object PopupMenu: TPopupMenu
    OwnerDraw = True
    Left = 208
    Top = 496
    object Copy1: TMenuItem
      Caption = '&Copy'
    end
    object Delete1: TMenuItem
      Caption = '&Delete'
      OnClick = Delete1Click
    end
  end
end
