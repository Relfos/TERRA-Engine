{$I terra.inc}
{$IFDEF MOBILE}Library{$ELSE}Program{$ENDIF} MaterialDemo;

Uses
{$IFDEF DEBUG_LEAKS}MemCheck,{$ELSE}  TERRA_MemoryManager,{$ENDIF}
  TERRA_DemoApplication, TERRA_Utils, TERRA_Object, TERRA_GraphicsManager,
  TERRA_OS, TERRA_Vector3D, TERRA_Font, TERRA_Lights, TERRA_Viewport,
  TERRA_JPG, TERRA_PNG, TERRA_String, Math,
  TERRA_Engine,
  TERRA_Vector2D, TERRA_Mesh, TERRA_MeshSkeleton, TERRA_MeshAnimation, TERRA_MeshAnimationNodes,
  TERRA_FileManager, TERRA_Color, TERRA_DebugDraw, TERRA_Resource, TERRA_Ray,
  TERRA_ScreenFX, TERRA_Math, TERRA_Matrix3x3, TERRA_Matrix4x4, TERRA_Quaternion, TERRA_InputManager,
  TERRA_FileStream, TERRA_Texture, TERRA_Sprite, TERRA_IKBone2D;

Type
  MyDemo = Class(DemoApplication)
    Public

			Procedure OnCreate; Override;
			Procedure OnDestroy; Override;

			Procedure OnMouseDown(Const X,Y:Single; Const Button:Word); Override;
			Procedure OnMouseUp(Const X,Y:Single; Const Button:Word); Override;
			Procedure OnMouseMove(Const X,Y:Single); Override;

      Procedure OnRender2D(V:TERRAViewport); Override;
  End;

Const
  SnakeSize = 100;
  SnakeJointCount = 7;

Var
  SnakeRoot:IKBone2D;

  BodyTex, HeadTex:TERRATexture;
  Dragging:Boolean;

Procedure DrawBone(Bone:IKBone2D; V:TERRAViewport);
Var
  Mat:Matrix3x3;
  S:TERRASprite;
  Visual:TERRATexture;
Begin
  Mat := Bone.GetAbsoluteMatrix();

  If Bone.Child = Nil Then
    Visual := HeadTex
  Else
    Visual := BodyTex;

  S := Engine.FetchSprite();
  S.Layer := 10;
  S.SetTexture(Visual);

  S.AddQuad(spriteAnchor_Center, Vector2D_Create(0.0, 0.0), 0.0, SnakeSize, SnakeSize);
  S.SetTransform(Mat);

  Engine.Graphics.AddRenderable(V, S);

  If Assigned(Bone.Child) Then
    DrawBone(Bone.Child, V);
End;



{ MyDemo }
Procedure MyDemo.OnCreate;
Var
  I:Integer;
Begin
  Inherited;

  Self.GUI.Viewport.Visible := True;

  BodyTex := Engine.Textures['snake_body'];
  HeadTex := Engine.Textures['snake_head'];

	SnakeRoot := IKBone2D.Create(SnakeJointCount);

  For I:=1 To Pred(SnakeJointCount) Do
	  SnakeRoot.GetChainBone(I).Position := Vector2D_Create(0, -(SnakeSize - 20));
End;

Procedure MyDemo.OnDestroy;
Begin
  ReleaseObject(SnakeRoot);

  Inherited;
End;


Procedure MyDemo.OnRender2D(V:TERRAViewport);
Begin
  //SnakeRoot.Position := Vector2D_Create(V.Width  * 0.5,  2* SnakeSize * 0.5);

  SnakeRoot.Position := Vector2D_Create(V.Width  * 0.5,  V.Height - SnakeSize * 0.5);
  DrawBone(SnakeRoot, V);

  Inherited;
End;

Procedure MyDemo.OnMouseDown(Const X,Y:Single; Const Button:Word);
Var
  TX, TY:Integer;
Begin
  If Button = keyMouseRight Then
  Begin
    Self.GUI.GetLocalCoords(X, Y, TX, TY);
    SnakeRoot.Solve(Vector2D_Create(TX, TY), True, True);
    Exit;
  End;

  Dragging := True;
End;

Procedure MyDemo.OnMouseMove(Const X,Y:Single);
Var
  TX, TY:Integer;
Begin
  If Not Dragging Then
    Exit;

  Self.GUI.GetLocalCoords(X, Y, TX, TY);
  SnakeRoot.Solve(Vector2D_Create(TX, TY), True, True);
End;

Procedure MyDemo.OnMouseUp(Const X,Y:Single; Const Button:Word);
Begin
  Dragging := False;
End;

{$IFDEF IPHONE}
Procedure StartGame; cdecl; export;
{$ENDIF}
Begin
  MyDemo.Create();
{$IFDEF IPHONE}
End;
{$ENDIF}

End.

