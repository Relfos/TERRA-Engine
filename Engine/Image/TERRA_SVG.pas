{***********************************************************************************************************************
 *
 * TERRA Game Engine
 * ==========================================
 *
 * Copyright (C) 2003, 2014 by Sérgio Flores (relfos@gmail.com)
 *
 ***********************************************************************************************************************
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on
 * an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations under the License.
 *
 **********************************************************************************************************************
 * TERRA_SVG
 * Implements loading of images in SVG format
 ***********************************************************************************************************************
}
Unit TERRA_SVG;

{$I terra.inc}

//http://dev.w3.org/SVG/tools/svgweb/samples/svg-files/
//http://www.w3.org/TR/SVG/painting.html

Interface
Uses TERRA_Utils, TERRA_Stream, TERRA_Image, TERRA_Color;

Const
  // All of the commands above can also be expressed with lower letters. Capital letters means absolutely positioned, lower cases means relatively positioned.
  svg_path_moveto = 'M';
  svg_path_lineto = 'L';
  svg_path_horizontal_lineto = 'H';
  svg_path_vertical_lineto = 'V';
  svg_path_curveto = 'C';
  svg_path_smooth_curveto = 'S';
  svg_path_quadratic_bezier_curve = 'Q';
  svg_path_smooth_quadratic_bezier_curveto = 'T';
  svg_path_elliptical_Arc = 'A';
  svg_path_closepath = 'Z';

Type
  SVGCommand = Class(TERRAObject)
    Stroke:Color;
    Fill:Color;
  End;

  SVGPathCommand = Record
    Opcode:TERRAChar;
    X:Single;
    Y:Single;
  End;

  SVGPath = Class(SVGCommand)
    Protected
      _Commands:Array Of SVGPathCommand;
      _CommandCount:Integer;
  End;

  SVG = Class(TERRAObject)
    Protected
      _Commands:Array Of SVGCommand;
      _CommandCount:Integer;

    Public

      Constructor Create;

      Procedure Load(Source:Stream);

      Function Render(TargetWidth, TargetHeight:Integer):Image;
  End;

Implementation
Uses TERRA_Error, TERRA_Log, TERRA_XML, TERRA_Tesselator;

Function GetColorFromNode(Node:XMLNode; Const Name:TERRAString):Color;
Var
  P:XMLNode;
Begin
  P := Node.GetNode(Name);
  If Assigned(P) Then
  Begin
    Result := ColorCreate(P.Value);
  End Else
    Result := ColorBlack;
End;

Function CreatePathFromNode(Node:XMLNode; Width, Height:Integer):SVGPath;
Var
  P:XMLNode;
  S, S2:TERRAString;
  Cmd:SVGPathCommand;
Begin
  Result := SVGPath.Create;

  Result.Fill := GetColorFromNode(Node, 'fill');
  P := Node.GetNode('d');

  If Assigned(P) Then
  Begin
    S := TrimLeft(P.Value);
    While S<>'' Do
    Begin
      Cmd.Opcode := UpCase(S[1]);
      S := Copy(S, 2, MaxInt);

      If (Cmd.Opcode = svg_path_moveto) Or (Cmd.Opcode = svg_path_lineto) Then
      Begin
        Cmd.X := StringToInt(StringExtractNextWord(S, ' ')) / Width;
        Cmd.Y := StringToInt(StringExtractNextWord(S, ' ')) / Height;

      End Else
      Begin
        RaiseError('SVG path opcode not supported : '+Cmd.Opcode);
        Exit;
      End;

      Inc(Result._CommandCount);
      SetLength(Result._Commands, Result._CommandCount);
      Result._Commands[Pred(Result._CommandCount)] := Cmd;
    End;
  End;
End;

{ SVG }
Constructor SVG.Create;
Begin
End;

Procedure SVG.Load(Source: Stream);
Var
  Doc:XMLDocument;
  Width, Height:Integer;
  P, PP:XMLNode;
  I:Integer;

  S:TERRAString;
  Cmd:SVGCommand;
Begin
  Doc := XMLDocument.Create();
  Doc.Load(Source);

  P := Doc.Root;

  PP := P.GetNode('width');
  If Assigned(PP) Then
  Begin
    S := PP.Value;
    ReplaceText('pt', '', S);
    Width := StringToInt(S);
  End;

  PP := P.GetNode('height');
  If Assigned(PP) Then
  Begin
    S := PP.Value;
    ReplaceText('pt', '', S);
    Height := StringToInt(S);
  End;

  For I:=0 To Pred(P.ChildCount) Do
  Begin
    PP := P.GetChild(I);

    If (PP.Name = 'path') Then
    Begin
      Cmd := CreatePathFromNode(PP, Width, Height);
    End Else
      Cmd := Nil;

    If Assigned(Cmd) Then
    Begin
      Inc(_CommandCount);
      SetLength(_Commands, _CommandCount);
      _Commands[Pred(_CommandCount)] := Cmd;
    End;
  End;

  Doc.Release();
End;

Function SVG.Render(TargetWidth, TargetHeight:Integer): Image;
Begin
  Result := Image.Create(TargetWidth, TargetHeight);
End;

End.