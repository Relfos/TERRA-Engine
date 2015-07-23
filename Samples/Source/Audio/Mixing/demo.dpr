PROGRAM demo;
{$APPTYPE CONSOLE}

USES Windows,MiniMod,trackfile;

VAR MiniMODClass:TMiniMOD;
BEGIN
 MiniMODClass:=TMiniMOD.Create(44100,TRUE,TRUE);
 MiniMODClass.Load(@TrackData,TrackSize);
 MiniMODClass.Play;

 ReadLn;

 MiniMODClass.Destroy;


END.
