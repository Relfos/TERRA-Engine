Unit TERRA_UICursor;

{$I terra.inc}
Interface

Type
  UICursor = Class(TERRAObject)
    Protected
      _UI:UI;
      _Name:TERRAString;
      _Item:TextureAtlasItem;
      _OfsX:Integer;
      _OfsY:Integer;

      Procedure Render;

    Public
      Constructor Create(Name:TERRAString; UI:UI; OfsX:Integer = 0; OfsY:Integer=0);
      Procedure Release; Override;

      Property UI:UI Read _UI;
  End;



Implementation

{ UICursor }
Constructor UICursor.Create(Name:TERRAString; UI:UI; OfsX, OfsY:Integer);
Var
  Source:Image;
  MyStream:Stream;
Begin
  _UI := UI;
  _Item := UIManager.Instance.GetTextureAtlas.Get(Name);

  If Not Assigned(_Item) Then
  Begin
    Name := FileManager.Instance.SearchResourceFile(Name);
    If Name<>'' Then
    Begin
      MyStream := FileManager.Instance.OpenStream(Name);
      Source := Image.Create(MyStream);
      _Item := UIManager.Instance.GetTextureAtlas.Add(Source, Name);
      UIManager.Instance._UpdateTextureAtlas := True;
      Source.Release;
      MyStream.Release;
    End;
  End;

  _Name := Name;
  _OfsX := OfsY;
  _OfsY := OfsY;
End;

Procedure UICursor.Release;
Begin
  // do nothing
End;

Procedure UICursor.Render;
Var
  StartPos, EndPos:Vector2D;
  T1, T2:Vector2D;
  MyTextureAtlas:TextureAtlas;
  MyColor:Color;
  CR:ClipRect;
Begin
  If (Not Assigned(_Item)) Then
    Exit;

  CR.Style := clipNothing;

  StartPos := VectorCreate2D(UI._CursorPos.X - _OfsX, UI._CursorPos.Y - _OfsY);
  EndPos.X := StartPos.X + _Item.Buffer.Width;
  EndPos.Y := StartPos.Y + _Item.Buffer.Height;
  T1 := VectorCreate2D(_Item.X, _Item.Y);
  MyTextureAtlas := UIManager.Instance.GetTextureAtlas;
  T2.X := T1.X + (_Item.Buffer.Width / MyTextureAtlas.Width);
  T2.Y := T1.Y + (_Item.Buffer.Height / MyTextureAtlas.Height);
  MyColor := ColorGrey(255, UI._Color.A);
  UI.AddQuad(StartPos, EndPos, T1, T2, MyColor, 99, _Item.PageID, MatrixIdentity3x3, 1, Nil, CR);
End;


  _CursorPos.X := InputManager.Instance.Mouse.X;
  _CursorPos.Y := InputManager.Instance.Mouse.Y;

End.