object UIEditForm: TUIEditForm
  Left = 229
  Top = 23
  Width = 942
  Height = 764
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
  OldCreateOrder = True
  Visible = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  OnMouseWheel = FormMouseWheel
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object TabList: TTabControl
    Left = 0
    Top = 0
    Width = 857
    Height = 561
    TabHeight = 25
    TabOrder = 0
    OnChange = TabListChange
    object WidgetList: TTreeView
      Left = 0
      Top = 80
      Width = 209
      Height = 249
      Indent = 19
      TabOrder = 0
      OnClick = WidgetListClick
      OnEdited = WidgetListEdited
      OnMouseDown = WidgetListMouseDown
    end
    object RenderPanel: TPanel
      Left = 208
      Top = -48
      Width = 577
      Height = 401
      TabOrder = 1
      OnMouseDown = RenderPanelMouseDown
      OnMouseMove = RenderPanelMouseMove
      OnMouseUp = RenderPanelMouseUp
    end
  end
  object MainMenu: TMainMenu
    Left = 120
    Top = 496
    object ProjectMenu: TMenuItem
      Caption = '&Project'
      object New1: TMenuItem
        Caption = '&New'
        object NewProject: TMenuItem
          Caption = '&Project'
          OnClick = NewProjectClick
        end
        object N3: TMenuItem
          Caption = '-'
        end
        object View1: TMenuItem
          Caption = '&View'
          OnClick = View1Click
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
      object N4: TMenuItem
        Caption = '-'
      end
      object DataSources1: TMenuItem
        Caption = '&Data Sources'
        OnClick = DataSources1Click
      end
      object Paths1: TMenuItem
        Caption = '&Paths'
        OnClick = Paths1Click
      end
      object Settings1: TMenuItem
        Caption = '&Settings'
        OnClick = Settings1Click
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
      object Image1: TMenuItem
        Caption = '&Image'
        OnClick = Image1Click
      end
      object TiledRect1: TMenuItem
        Caption = '&Tiled Rect'
        OnClick = TiledRect1Click
      end
      object Label1: TMenuItem
        Caption = '&Label'
        OnClick = Label1Click
      end
      object InputText1: TMenuItem
        Caption = '&Input Text'
        OnClick = InputText1Click
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
    Left = 96
    Top = 560
    object ResetSize1: TMenuItem
      Caption = '&Reset Size'
      OnClick = ResetSize1Click
    end
    object N5: TMenuItem
      Caption = '-'
    end
    object Copy1: TMenuItem
      Caption = '&Copy'
    end
    object Delete1: TMenuItem
      Caption = '&Delete'
      OnClick = Delete1Click
    end
  end
end
