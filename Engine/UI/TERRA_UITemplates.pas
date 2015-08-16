Unit TERRA_UITemplates;

{$I terra.inc}

Interface
Uses TERRA_Object, TERRA_String, TERRA_Math, TERRA_Texture,
  TERRA_UIWidget, TERRA_UIDimension, TERRA_UIImage, TERRA_UITiledRect, TERRA_UILabel, TERRA_UIEditText;

Type
  UIWindowTemplate = Class(UIWidget)
    Public
      Constructor Create(Const Name:TERRAString; Tex:TERRATexture; Const X1, Y1, X2, Y2:Integer);
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
Uses TERRA_Localization;


{ UIWindowTemplate }

Constructor UIWindowTemplate.Create(Const Name:TERRAString; Tex:TERRATexture; Const X1, Y1, X2, Y2:Integer);
Var
  TileRect:UITiledRect;
Begin
  Inherited Create(Name, Nil);

  Tex.Prefetch();
  TileRect := UITiledRect.Create('rect', Self, 0, 0, 1, UIPercent(100), UIPercent(100), X1/Tex.Width, Y1/Tex.Height, X2/Tex.Width, Y2/Tex.Height);
  TileRect.Texture := Tex;
  TileRect.Draggable := True;

  Self.CanReceiveEvents := True;
End;

{ UIButtonTemplate }
Constructor UIButtonTemplate.Create(const Name: TERRAString; Tex: TERRATexture; Const X1, Y1, X2, Y2:Integer);
Var
  TileRect:UITiledRect;
  Caption:UILabel;
Begin
  Inherited Create(Name, Nil);

  Tex.Prefetch();
  TileRect := UITiledRect.Create('button', Self, 0, 0, 1, UIPercent(100), UIPercent(100), X1/Tex.Width, Y1/Tex.Height, X2/Tex.Width, Y2/Tex.Height);
  TileRect.Texture := Tex;
  TileRect.Draggable := True;
  //TileRect.Scale := 2;

  Caption := UILabel.Create('label', Self, 0, 0, 1, UIPercent(100), UIPercent(100), UIPropertyMacro('text'));
  Caption.Align := waCenter;

  Self.AddProperty(StringProperty.Create('text', 'untitled'), True);

  Self.CanReceiveEvents := True;
End;

{ UIEditTextTemplate }
Constructor UIEditTextTemplate.Create(const Name: TERRAString; Tex: TERRATexture; const X1, Y1, X2, Y2: Integer);
Var
  TileRect:UITiledRect;
  EditText:UILabel;
Begin
  Inherited Create(Name, Nil);

  Tex.Prefetch();
  TileRect := UITiledRect.Create('button', Self, 0, 0, 1, UIPercent(100), UIPercent(100), X1/Tex.Width, Y1/Tex.Height, X2/Tex.Width, Y2/Tex.Height);
  TileRect.Texture := Tex;
  TileRect.Draggable := True;

  EditText := UIEditText.Create('label', Self, 0, 0, 1, UIPercent(90), UIPercent(100),
  //'dsafsdfdsjkasdakjdaskjdadfsfsfsfsdfgdgd'
    GetLanguageDescription(language_Portuguese)+ ' '+    GetLanguageDescription(language_Japanese)+ ' '+ GetLanguageDescription(language_Korean)+ ' '+ GetLanguageDescription(language_Russian)+ ' '+ GetLanguageDescription(language_Chinese)
    );

  EditText.Align := waCenter;

  Self.AddProperty(StringProperty.Create('text', 'untitled'), True);

  Self.CanReceiveEvents := True;
End;

End.
