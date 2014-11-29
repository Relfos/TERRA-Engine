Unit TERRA_RenderTarget;
{$I terra.inc}

Interface
Uses {$IFDEF USEDEBUGUNIT}TERRA_Debug,{$ENDIF}
  TERRA_Utils, TERRA_Color, TERRA_Image, TERRA_Vector2D, {$IFDEF DEBUG_GL}TERRA_DebugGL{$ELSE}TERRA_GL{$ENDIF}, TERRA_Texture;

Const
  captureTargetColor    = 0;
  captureTargetNormal   = 1;
  captureTargetEmission = 2;
  captureTargetRefraction = 3;
  captureTargetOutline = 4;
  captureTargetReflection = 5;
  captureTargetPosition = 6;
  MaxCaptureTargets = 7;

  TargetNames:Array[0..Pred(MaxCaptureTargets)] Of AnsiString =
  ('diffuse', 'normal', 'emission', 'refraction', 'outline', 'reflection', 'position');

Var
  TargetTextureNames:Array[0..Pred(MaxCaptureTargets)] Of AnsiString;


Type
  RenderTarget = Class(Texture)
    Protected
      _ClearColor:Color;
      _Active:Boolean;

	    Procedure InitCapture(Flags:Cardinal); Virtual; Abstract;

    Public
      TargetType:Integer;

	    // Render to this target
	    Procedure BeginCapture(Flags:Cardinal = GL_COLOR_BUFFER_BIT Or GL_DEPTH_BUFFER_BIT Or GL_STENCIL_BUFFER_BIT); 
	    Procedure EndCapture; Virtual; Abstract;

	    Procedure Save(FileName:AnsiString);

      Property ClearColor:Color Read _ClearColor Write _ClearColor;
      Property Active:Boolean Read _Active;
  End;

  Function CreateRenderTarget(Name:AnsiString; Width, Height:Integer; DepthBuffer, StencilBuffer:Boolean):RenderTarget;

Implementation
Uses TERRA_Application, TERRA_GraphicsManager, TERRA_Log, TERRA_TextureTarget
  {$IFDEF FRAMEBUFFEROBJECTS}, TERRA_FrameBufferObject{$ENDIF};

Type
  NullTarget = Class(RenderTarget)
    Protected
      Procedure InitCapture(Flags:Cardinal);  Override;
    Public
      Destructor Destroy; Override;

	    // Use it as a texture
	    Procedure Bind(Slot:Integer); Override;

	    // Render to this target
	    Procedure EndCapture;  Override;

      Function GetPixel(X,Y:Integer):Color;  Override;
      Function GetImage():Image; Override;
  End;

Function CreateRenderTarget(Name:AnsiString; Width, Height:Integer; DepthBuffer, StencilBuffer:Boolean):RenderTarget;
Begin
{$IFDEF FRAMEBUFFEROBJECTS}
  If GraphicsManager.Instance.Settings.FrameBufferObject.Avaliable Then
    Result := FrameBufferObject.Create(Name, Width, Height, FBO_COLOR8, DepthBuffer, StencilBuffer, False, 1)
  Else
{$ENDIF}
    Result := TextureRenderTarget.Create(Name, Width, Height);

  Log(logDebug, 'Texture', 'Created '+Result.ClassName+', width:'+IntToString(Width)+', height:'+IntToString(Height));
End;

{ RenderTarget }
Procedure RenderTarget.BeginCapture(Flags: Cardinal);
Begin
  InitCapture(Flags);
End;

Procedure RenderTarget.Save(fileName:AnsiString);
Var
  MyImage:Image;
Begin
  MyImage := Self.GetImage();
  MyImage.Save(FileName);
	MyImage.Destroy;
End;

{ NullTarget }
Destructor NullTarget.Destroy;
Begin
End;

Procedure NullTarget.InitCapture(Flags: Cardinal);
Begin
  glColorMask(False, False, False, False);
End;

Procedure NullTarget.EndCapture;
Begin
  glColorMask(True, True, True, True);
End;

Procedure NullTarget.Bind(Slot: Integer);
Begin
End;

Function NullTarget.GetImage():Image;
Begin
  Result := Nil;
End;

Function NullTarget.GetPixel(X, Y: Integer): Color;
Begin
  Result := ColorBlack;
End;

Var
  I:Integer;
Initialization
  For I:=0 To Pred(MaxCaptureTargets) Do
    TargetTextureNames[I] := TargetNames[I] + '_texture';
End.
