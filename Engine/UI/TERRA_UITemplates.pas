Unit TERRA_UITemplates;

{$I terra.inc}

Interface
Uses TERRA_Object, TERRA_String, TERRA_Texture, TERRA_UIWidget, TERRA_UIDimension, TERRA_UIImage, TERRA_UITiledRect, TERRA_UILabel;

Type
  UIWindowTemplate = Class(UIWidget)
    Public
      Constructor Create(Const Name:TERRAString; Tex:TERRATexture; Const X1, Y1, X2, Y2:Integer);
  End;

  UIButtonTemplate = Class(UIWidget)
    Public
      Constructor Create(Const Name:TERRAString; Tex:TERRATexture; Const X1, Y1, X2, Y2:Integer);
  End;

Implementation

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

  Self.AddChild(TileRect);
End;

{ UIButtonTemplate }
Constructor UIButtonTemplate.Create(const Name: TERRAString; Tex: TERRATexture; Const X1, Y1, X2, Y2:Integer);
Var
  TileRect:UITiledRect;
  Caption:UILabel;
Begin
  Inherited Create(Name, Nil);

  Tex.Prefetch();
  TileRect := UITiledRect.Create('btn', Self, 0, 0, 1, UIPercent(100), UIPercent(100), X1/Tex.Width, Y1/Tex.Height, X2/Tex.Width, Y2/Tex.Height);
  TileRect.Texture := Tex;
  TileRect.Draggable := True;

  Caption := UILabel.Create('text', Self, 0, 0, 1, UIPercent(100), UIPercent(100), 'test!');

  Self.AddChild(TileRect);
End;

End.
