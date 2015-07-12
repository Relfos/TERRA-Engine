Program stack_tool_print;

{$I terra.inc}

Uses TERRA_Utils, TERRA_String, TERRA_Stream, TERRA_MemoryStream;

Type
  FuncInfo = Record
    Name:String;
    UnitID:Cardinal;
    ClassID:Cardinal;
    Addr:Cardinal;
  End;

Var
  Src:Stream;
  I:Integer;

  Funcs:Array Of FuncInfo;
  FuncCount:Cardinal;

  Units:Array Of String;
  UnitCount:Cardinal;

  Classes:Array Of String;
  ClassCount:Cardinal;

Function LookUp(Addr:Cardinal):String;
Var
  I:Integer;
Begin
  For I:=0 To Pred(FuncCount) Do
  If (Funcs()
End;

Begin
  Src := MemoryStream.Create('D:\code\minimonhd\trunk\Android\libs\armeabi-v7a\debug.bin');
  Src.ReadCardinal(FuncCount);
  SetLength(Funcs, FuncCount);

  For I:=0 To Pred(FuncCount) Do
  Begin
    Src.ReadCardinal(Funcs[I].Addr);
    Src.ReadCardinal(Funcs[I].UnitID);
    Src.ReadCardinal(Funcs[I].ClassID);
    Src.ReadString(Funcs[I].Name);
  End;

  Src.ReadCardinal(UnitCount);
  SetLength(Units, UnitCount);
  For I:=0 To Pred(UnitCount) Do
    Src.ReadString(Units[I]);

  Src.ReadCardinal(ClassCount);
  SetLength(Classes, ClassCount);
  For I:=0 To Pred(ClassCount) Do
    Src.ReadString(Classes[I]);

  Src.Release();
End.