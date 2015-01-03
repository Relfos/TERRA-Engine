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
 * TERRA_GilesImport
 * Implements [g]iles lightmap mesh importer
 ***********************************************************************************************************************
}
Unit TERRA_GilesImport;

Interface
Uses TERRA_Application, TERRA_Giles, TERRA_MeshAnimation, TERRA_Utils, TERRA_OS;

implementation

Uses TERRA_Mesh, TERRA_INI, TERRA_IO, TERRA_Matrix, TERRA_ResourceManager,
  TERRA_Vector3D, TERRA_Vector2D, TERRA_Math, TERRA_Color, TERRA_Log,
  TERRA_DXTools, SysUtils, TERRA_MeshFilter, TERRA_FileImport, TERRA_FileIO,
  TERRA_FileUtils;

Function GilesImporter(SourceFile, TargetDir:AnsiString; TargetPlatform:Integer; Settings:AnsiString):AnsiString;
Var
  I,J,K:Integer;
  S:AnsiString;
  Src, Dest:Stream;
  Model:GilesModel;
  G:MeshGroup;
  MyMesh:Mesh;
Begin
  Log(logConsole, 'Import', 'Reading Giles file ('+GetFileName(SourceFile, False)+')...');
  Src := MemoryStream.Create(SourceFile);
  Model := GilesModel.Create;
  Model.Load(Src);
  Src.Destroy;

  Log(logConsole, 'Import', 'Converting mesh...');
  MyMesh := Mesh.CreateFromFilter(Model);

  Log(logConsole, 'Import', 'Saving mesh...');
  S := TargetDir + PathSeparator + GetFileName(SourceFile, True)+ '.mesh';
  Dest := FileStream.Create(S);
  MyMesh.Save(Dest);
  MyMesh.Destroy;
  Dest.Destroy;

  Model.Destroy();
End;


Initialization
  RegisterFileImporter('gls', 'mesh', GilesImporter);
End.
