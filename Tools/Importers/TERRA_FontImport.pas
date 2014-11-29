Unit TERRA_FontImport;
{$I terra.inc}

Interface

Uses {$IFDEF USEDEBUGUNIT}TERRA_Debug,{$ENDIF}
  TERRA_Utils, TERRA_IO, TERRA_Application, TERRA_Image, {$IFDEF DEBUG_GL}TERRA_DebugGL{$ELSE}TERRA_GL{$ENDIF}, TERRA_PNG, TERRA_JPG, TERRA_TGA, TERRA_Color;

Function ImportFont(SourceFile, TargetDir:AnsiString; TargetPlatform:Integer):AnsiString;

Implementation

Function ImportFont(SourceFile, TargetDir:AnsiString; TargetPlatform:Integer):AnsiString;
Begin
End;

End.