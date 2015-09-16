Unit TERRA_RendererStats;

Interface
Uses TERRA_Object;

Type
  RendererStatType = (
    RendererStat_Frames,
    RendererStat_Triangles,
    RendererStat_Shaders,
    RendererStat_Renderables,
    RendererStat_Lights,
    RendererStat_Occluders
  );

  RendererStats = Class(TERRAObject)
    Protected
      _TriangleCount:IntegerProperty;
      _ShaderSwitches:IntegerProperty;
      _DrawCalls:IntegerProperty;
      _LightCount:IntegerProperty;
      _OccluderCount:IntegerProperty;
      _RenderableCount:IntegerProperty;
      _FramesPerSecond:IntegerProperty;

      Function GetDrawCalls: Integer;
      Function GetFramesPerSecond: Integer;
      Function GetLightCount: Integer;
      Function GetOccluderCount: Integer;
      Function GetRenderableCount: Integer;
      Function GetShaderSwitches: Integer;
      Function GetTriangleCount: Integer;

    Public
      Constructor Create;
      Procedure Release(); Override;

      Procedure Reset;
      Procedure Update(Stat:RendererStatType; Count:Integer = 1);

      Function GetPropertyByIndex(Index:Integer):TERRAObject; Override;

      Property TriangleCount:Integer Read GetTriangleCount;
      Property ShaderSwitches:Integer Read GetShaderSwitches;
      Property DrawCalls:Integer Read GetDrawCalls;
      Property LightCount:Integer Read GetLightCount;
      Property OccluderCount:Integer Read GetOccluderCount;
      Property RenderableCount:Integer Read GetRenderableCount;
      Property FramesPerSecond:Integer Read GetFramesPerSecond;
  End;


Implementation

{ RendererStats }
Constructor RendererStats.Create;
Begin
  _TriangleCount := IntegerProperty.Create('triangles', 0);
  _ShaderSwitches := IntegerProperty.Create('shaderswitches', 0);
  _DrawCalls := IntegerProperty.Create('drawcalls', 0);
  _LightCount := IntegerProperty.Create('lights', 0);
  _OccluderCount := IntegerProperty.Create('occluders', 0);
  _RenderableCount := IntegerProperty.Create('renderables', 0);
  _FramesPerSecond := IntegerProperty.Create('fps', 0);
End;

Procedure RendererStats.Release;
Begin
  ReleaseObject(_TriangleCount);
  ReleaseObject(_ShaderSwitches);
  ReleaseObject(_DrawCalls);
  ReleaseObject(_LightCount);
  ReleaseObject(_OccluderCount);
  ReleaseObject(_RenderableCount);
  ReleaseObject(_FramesPerSecond);
End;

Procedure RendererStats.Reset;
Begin
  _TriangleCount.Value := 0;
  _ShaderSwitches.Value := 0;
  _DrawCalls.Value := 0;
  _LightCount.Value := 0;
  _OccluderCount.Value := 0;
  _RenderableCount.Value := 0;
  _FramesPerSecond.Value := 0;
End;

Function RendererStats.GetDrawCalls: Integer;
Begin
  Result := _DrawCalls.Value;
End;

Function RendererStats.GetFramesPerSecond: Integer;
Begin
  Result := _FramesPerSecond.Value;
End;

Function RendererStats.GetLightCount: Integer;
Begin
  Result := _LightCount.Value;
End;

Function RendererStats.GetOccluderCount: Integer;
Begin
  Result := _OccluderCount.Value;
End;

function RendererStats.GetRenderableCount: Integer;
Begin
  Result := _RenderableCount.Value;
End;

Function RendererStats.GetShaderSwitches: Integer;
Begin
  Result := _ShaderSwitches.Value;
End;

function RendererStats.GetTriangleCount: Integer;
Begin
  Result := _TriangleCount.Value;
End;

Procedure RendererStats.Update(Stat:RendererStatType; Count: Integer);
Begin
  Case Stat Of
  RendererStat_Frames:
    _FramesPerSecond.Value := Count;
    
  RendererStat_Triangles:
    Begin
      _TriangleCount.Value := _TriangleCount.Value + Count;
      _DrawCalls.Value := _DrawCalls.Value + 1;
    End;

  RendererStat_Shaders:
    Begin
      _ShaderSwitches.Value := _ShaderSwitches.Value + Count;
    End;

  RendererStat_Renderables:
    Begin
      _RenderableCount.Value := _RenderableCount.Value + Count;
    End;

  RendererStat_Lights:
    Begin
      _LightCount.Value := _LightCount.Value + Count;
    End;

  RendererStat_Occluders:
    Begin
      _OccluderCount.Value := _OccluderCount.Value + Count;
    End;

  End;
End;

Function RendererStats.GetPropertyByIndex(Index: Integer): TERRAObject;
Begin
  Case Index Of
      0: Result := _TriangleCount;
      1: Result := _ShaderSwitches;
      2: Result := _DrawCalls;
      3: Result := _LightCount;
      4: Result := _OccluderCount;
      5: Result := _RenderableCount;
      6: Result := _FramesPerSecond;
  Else
    Result := Nil;
  End;
End;

End.
