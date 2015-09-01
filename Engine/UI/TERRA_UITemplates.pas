Unit TERRA_UITemplates;

{$I terra.inc}

Interface
Uses TERRA_Object, TERRA_String, TERRA_Math, TERRA_Texture, TERRA_Color,
  TERRA_UIWidget, TERRA_UIDimension, TERRA_UIImage, TERRA_UITiledRect, TERRA_UILabel, TERRA_UIEditText;

Type
  UIWindowTemplate = Class(UIWidget)
    Public
      Constructor Create(Const Name:TERRAString; Tex:TERRATexture; Const X1, Y1, X2, Y2:Integer);
  End;

  UITabbedWindowTemplate = Class(UIWidget)
    Public
      Constructor Create(Const Name:TERRAString; Tex:TERRATexture; Const X1, Y1, X2, Y2:Integer; Const TabCount:Integer; Const TabTemplate:TERRAString);
  End;

  UIButtonTemplate = Class(UIWidget)
    Public
      Constructor Create(Const Name:TERRAString; Tex:TERRATexture; Const X1, Y1, X2, Y2:Integer);
  End;

  UIEditTextTemplate = Class(UIWidget)
    Public
      Constructor Create(Const Name:TERRAString; Tex:TERRATexture; Const X1, Y1, X2, Y2:Integer);
  End;

Implementation
Uses TERRA_EngineManager, TERRA_Localization;


{ UIWindowTemplate }

Constructor UIWindowTemplate.Create(Const Name:TERRAString; Tex:TERRATexture; Const X1, Y1, X2, Y2:Integer);
Var
  TileRect:UITiledRect;
Begin
  Inherited Create(Name, Nil);

  Tex.Prefetch();
  TileRect := UITiledRect.Create('rect', Self, UIPixels(0), UIPixels(0), 0, UIPercent(100), UIPercent(100), X1/Tex.Width, Y1/Tex.Height, X2/Tex.Width, Y2/Tex.Height);
  TileRect.Texture := Tex;
End;


{ UITabbedWindowTemplate }
Constructor UITabbedWindowTemplate.Create(const Name: TERRAString; Tex: TERRATexture; const X1, Y1, X2, Y2: Integer; Const TabCount:Integer; Const TabTemplate:TERRAString);
Const
  TabHeight = 20;
Var
  TabLayout, TabGroup:UIWidgetGroup;
  Tab:UIWidget;
  TileRect:UITiledRect;
  I:Integer;
Begin
  Inherited Create(Name, Nil);

  TabLayout := UIWidgetGroup.Create('tab_layout', Self, UIPixels(0), UIPixels(0), 0, UIPercent(100), UIPercent(TabHeight));
  TabLayout.Layout := UILayout_Horizontal;
  TabLayout.Align := UIAlign_TopLeft;
  TabLayout.Padding := UIPixels(10);

  Tex.Prefetch();
  TileRect := UITiledRect.Create('rect', Self, UIPixels(0), UIPercent(TabHeight), 0.2, UIPercent(100), UIPercent(100 - TabHeight), X1/Tex.Width, Y1/Tex.Height, X2/Tex.Width, Y2/Tex.Height);
  TileRect.Texture := Tex;

  // create the widget groups inside the gui window
  // the group size is 100% of the parent
  For I:=1 To TabCount Do
  Begin
    Tab := UIInstancedWidget.Create('tab_button', TabLayout, UIPixels(0), UIPixels(0), 0.1, UIPixels(100), UIPercent(100), TabTemplate);
    TabGroup := UIWidgetGroup.Create('tab_group', TileRect, UIPixels(0), UIPixels(0), 1, UIPercent(100), UIPercent(100));
    TabGroup.Visible := (I=1);
  End;
End;

{ UIButtonTemplate }
Constructor UIButtonTemplate.Create(const Name: TERRAString; Tex: TERRATexture; Const X1, Y1, X2, Y2:Integer);
Var
  TileRect:UITiledRect;
  Caption:UILabel;
Begin
  Inherited Create(Name, Nil);

  Tex.Prefetch();
  TileRect := UITiledRect.Create('button', Self, UIPixels(0), UIPixels(0), 0.0, UIPercent(100), UIPercent(100), X1/Tex.Width, Y1/Tex.Height, X2/Tex.Width, Y2/Tex.Height);
  TileRect.Texture := Tex;
  //TileRect.Color := ColorBlue;
  //TileRect.Scale := 2;

  Caption := UILabel.Create('label', TileRect, UIPixels(0), UIPixels(0), 0.1, UIPercent(100), UIPercent(100), UIPropertyMacro('value'));
  Caption.Align := UIAlign_Center;

  Self.AddProperty(StringProperty.Create('value', 'untitled'), True);

  //Self.Rotation := 45*RAD;
  //Self.Scale := 2;
End;

{ UIEditTextTemplate }
Constructor UIEditTextTemplate.Create(const Name: TERRAString; Tex: TERRATexture; const X1, Y1, X2, Y2: Integer);
Var
  TileRect:UITiledRect;
  EditText:UIEditText;
Begin
  Inherited Create(Name, Nil);

  Tex.Prefetch();
  TileRect := UITiledRect.Create('button', Self, UIPixels(0), UIPixels(0), 0, UIPercent(100), UIPercent(100), X1/Tex.Width, Y1/Tex.Height, X2/Tex.Width, Y2/Tex.Height);
  TileRect.Texture := Tex;

  EditText := UIEditText.Create('edit', Self, UIPixels(20), UIPixels(15), 0.1, UIPercent(90), UIPercent(50),
  //'dsafsdfdsjkasdakjdaskjdadfsfsfsfsdfgdgd'
  //'test 12345 12345 12345 12345 12345 2'
  'test 12345'
 //   GetLanguageDescription(language_Portuguese)+ ' '+    GetLanguageDescription(language_Japanese)+ ' '+ GetLanguageDescription(language_Korean)+ ' '+ GetLanguageDescription(language_Russian)+ ' '+ GetLanguageDescription(language_Chinese)
    );

  EditText.Align := UIAlign_TopLeft;
  EditText.MultiLine := True;

  //EditText.AddAnimation(widget_Selected, 'color', '#FF2222FF');

  //EditText.AddAnimation(widget_Highlighted, 'color', '#FF2222FF');
End;

End.
