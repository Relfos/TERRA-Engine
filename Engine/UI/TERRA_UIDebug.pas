Unit TERRA_UIDebug;

{$I terra.inc}

Interface
Uses TERRA_MemoryManager, TERRA_Utils, TERRA_String, TERRA_Object, TERRA_UIWidget, TERRA_UIDimension, TERRA_UICursor, TERRA_Vector2D, TERRA_Color, TERRA_Font,
  TERRA_EnumProperty, TERRA_FontRenderer, TERRA_Viewport, TERRA_UIText, TERRA_DebugDraw, TERRA_UITemplates;

Type
  UIMemoryAllocDebugWidget = Class(UIText)
    Protected
      Procedure UpdateSprite(View:TERRAViewport); Override;
    Public
  End;

  UIMemoryAllocGraphWidget = Class(UIWidget)
    Protected
      Procedure UpdateSprite(View:TERRAViewport); Override;
    Public
  End;

Implementation
Uses TERRA_OS, TERRA_Localization, TERRA_FontManager, TERRA_EngineManager, TERRA_Math;

Procedure UIMemoryAllocDebugWidget.UpdateSprite(View:TERRAViewport);
Var
  I:Integer;
  Stats:PMemoryAllocStats;
  S:TERRAString;
Begin
  MemoryManager.EnableStats(False);

  _Text :='';
  For I:=0 To Pred(MemoryManager.GetAllocStatsCount) Do
  Begin
    Stats := MemoryManager.GetAllocStatsInfo(I);
    If Assigned(Stats.Name) Then
      S := Stats.Name^
    Else
      S := '???';
    _Text := _Text + S +': '+ MemoryToString(Stats.AllocSize)+' '+ IntegerProperty.Stringify(Stats.AllocCount)+'x' +CrLf;
  End;


  Inherited UpdateSprite(View);

  MemoryManager.EnableStats(True);
End;


{ UIMemoryAllocGraphWidget }
Procedure UIMemoryAllocGraphWidget.UpdateSprite(View: TERRAViewport);
Begin
  Inherited;
              sds
End;

End.