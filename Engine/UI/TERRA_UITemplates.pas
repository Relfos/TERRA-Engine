Unit TERRA_UITemplates;

{$I terra.inc}

Interface
Uses TERRA_Object, TERRA_String, TERRA_Texture, TERRA_UIWidget, TERRA_UIDimension, TERRA_UIImage, TERRA_UITiledRect;

Type
  UIWindowTemplate = Class(UITiledRect)
    Public
      Constructor Create(Const Name:TERRAString; Tex:TERRATexture);
  End;

Implementation

{ UIWindowTemplate }

Constructor UIWindowTemplate.Create(Const Name:TERRAString; Tex:TERRATexture);
Begin
  Tex.Prefetch();

  Inherited Create(Name, Nil, 0, 0, 10, UIPercent(100), UIPercent(100), 45/Tex.Width, 28/Tex.Height, 147/Tex.Width, 98/Tex.Height);

  Self.Texture := Tex;
  Self.Draggable := True;
End;

End.
