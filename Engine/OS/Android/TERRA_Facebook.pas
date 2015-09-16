Unit TERRA_Facebook;

{$I terra.inc}

Interface
Uses TERRA_Object, TERRA_String, TERRA_Utils, TERRA_Java, JNI;

Const
  FacebookClassPath = 'com.pascal.terra.TERRAFacebook';

Type
  Facebook = Class(TERRAObject)
    Protected
      _Facebook:JavaObject;

    Public
      Constructor Create();
      Procedure Release; Override;

      Procedure Post(msg, link, desc, imageURL:TERRAString);
      Procedure LikePage(page, url:TERRAString);
  End;

Implementation
Uses TERRA_Error, TERRA_Application, TERRA_OS, TERRA_Log;

{ Facebook }
Constructor Facebook.Create();
Var
  Params:JavaArguments;
  Frame:JavaFrame;
Begin
  Java_Begin(Frame);
  Params := JavaArguments.Create(Frame);
  Params.AddString(Application.Instance.GetFacebookID());
  _Facebook := JavaObject.Create(FacebookClassPath, Params, Frame);
  ReleaseObject(Params);
  Java_End(Frame);
End;

Procedure Facebook.Release;
Var
  Frame:JavaFrame;
Begin
  Log(logDebug, 'App', 'Deleting facebook object');

  Java_Begin(Frame);
  ReleaseObject(_Facebook);
  Java_End(Frame);
End;

Procedure Facebook.LikePage(page, url:TERRAString);
Var
  Params:JavaArguments;
  Frame:JavaFrame;
Begin
  Page := '"id":"'+Page+'"';

  Java_Begin(Frame);
  Params := JavaArguments.Create(Frame);
  Params.AddString(Page);
  Params.AddString(URL);
  _Facebook.CallVoidMethod(Frame, 'likePage', Params);
  ReleaseObject(Params);
  Java_End(Frame);
End;

Procedure Facebook.Post(msg, link, desc, imageURL:TERRAString);
Var
  Params:JavaArguments;
  Frame:JavaFrame;
Begin
  Log(logDebug, 'App', 'Posting to facebook: '+Msg);

  Java_Begin(Frame);
  Params := JavaArguments.Create(Frame);
  Params.AddString(Msg);
  Params.AddString(Link);
  Params.AddString(Desc);
  Params.AddString(ImageURL);
  _Facebook.CallVoidMethod(Frame, 'post', Params);
  ReleaseObject(Params);
  Java_End(Frame);
End;

End.


