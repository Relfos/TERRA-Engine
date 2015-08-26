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
 * TERRA_JPG
 * Implements JPG loader
 ***********************************************************************************************************************
}

Unit TERRA_JPG;

{$i terra.inc}

Interface
Uses TERRA_Object, TERRA_Utils, TERRA_Stream, TERRA_Image, TERRA_Log, TERRA_FileFormat, TERRA_Application,
  sdJpegImage, sdJpegTypes, sdMapIterator;

Type
  JPEGFormat = Class(TERRAFileFormat)
    Public
      Function Identify(Source:TERRAStream):Boolean; Override;
      Function Load(Target:TERRAObject; Source:TERRAStream):Boolean; Override;
      //Function Save(Target:TERRAObject; Dest:TERRAStream):Boolean; Override;
  End;

Implementation
Uses TERRA_Error, TERRA_EngineManager, TERRA_FileStream, TERRA_FileUtils, TERRA_Color;

{ JPEGFormat }
Function JPEGFormat.Identify(Source: TERRAStream): Boolean;
Var
  ID:FileHeader;
Begin
  Source.Read(@ID, 4);
  Result := CompareFileHeader(ID, 'ÿØÿà');
End;

Function JPEGFormat.Load(Target: TERRAObject; Source: TERRAStream): Boolean;
Var
  Image:TERRAImage;
  jpeg:TsdJpegImage;
Begin
  Result := False;
  Image := TERRAImage(Target);

  jpeg := TsdJpegImage.Create();
  jpeg.LoadFromStream(Source, Image);
  jpeg.LoadJpeg(jsFull, True);
  jpeg.Destroy();

  Result := True;
End;

Begin
  Engine.Formats.Add(JPEGFormat.Create(TERRAImage, 'jpg'));
End.

