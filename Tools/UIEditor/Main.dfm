object UIEditForm: TUIEditForm
  Left = 229
  Height = 705
  Top = 23
  Width = 926
  HorzScrollBar.Visible = False
  VertScrollBar.Visible = False
  Caption = 'TERRA GUI Editor'
  ClientHeight = 705
  ClientWidth = 926
  Color = 2500134
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  KeyPreview = True
  Menu = MainMenu
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyDown = FormKeyDown
  OnMouseWheel = FormMouseWheel
  OnResize = FormResize
  LCLVersion = '1.4.0.4'
  Visible = True
  object TabList: TTabControl
    Left = 0
    Height = 561
    Top = 0
    Width = 857
    OnChange = TabListChange
    TabHeight = 25
    TabOrder = 0
    object WidgetList: TTreeView
      Left = 0
      Height = 249
      Top = 80
      Width = 209
      DefaultItemHeight = 16
      Indent = 19
      TabOrder = 0
      OnClick = WidgetListClick
      OnEdited = WidgetListEdited
      OnMouseDown = WidgetListMouseDown
    end
    object RenderPanel: TPanel
      Left = 208
      Height = 401
      Top = -48
      Width = 577
      TabOrder = 2
      OnMouseDown = RenderPanelMouseDown
      OnMouseMove = RenderPanelMouseMove
      OnMouseUp = RenderPanelMouseUp
    end
  end
  object MainMenu: TMainMenu
    left = 120
    top = 496
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
    left = 96
    top = 560
    object Copy1: TMenuItem
      Caption = '&Copy'
    end
    object Delete1: TMenuItem
      Caption = '&Delete'
      OnClick = Delete1Click
    end
  end
end
