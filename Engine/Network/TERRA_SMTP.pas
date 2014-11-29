{
@abstract(SMTP Mail)
@author(Sergio Flores <relfos@gmail.com>)
@created(February 3, 2006)
@lastmod(February 25, 2006)
The SMTP unit provides a Mail class that allows user to send emails.

References:
  RFC 2821  Simple Mail Transfer Protocol
  RFC 2554  SMTP Service Extension for Authentication

Version History
  23/7/06   • First version
            • Added PLAIN authentication supoort
}

Unit TERRA_SMTP;
{$I terra.inc}

Interface
Uses TERRA_Utils, TERRA_IO, TERRA_FileIO, TERRA_Sockets;

Type
  Mail = Class
    Protected
      _Stream:Socket;
      _Attachments:Array Of AnsiString;
      _AttachCount:Integer;

      Function GetStatus(Var S:AnsiString):Integer;
    Public

      Sender:AnsiString;        // sender userID (optional)
      SenderName:AnsiString;    // sender display name (optional)
      ReplyTo:AnsiString;       // reply to userID (optional)
      ReplyToName:AnsiString;   // reply to display name (optional)
      MessageID:AnsiString;     // message ID (optional)
      Subject:AnsiString;       // subject of message
      Message:AnsiString;       // message text

      HostName:AnsiString;      // SMTP Provider
      UserName:AnsiString;      // SMTP Username
      Password:AnsiString;      // SMTP Password

      Port:Integer;

      Procedure AddAttachment(FileName:AnsiString);
      Function Send(Address:AnsiString):Boolean;

      Constructor Create;
  End;

Implementation
Uses TERRA_Network, TERRA_FileUtils, TERRA_Application;

Const
  SMTP_PORT=25;
  MailerID='X-Mailer: LEAF Mail V1.0';

Constructor Mail.Create;
Begin
  Port := SMTP_PORT;
End;

Procedure Mail.AddAttachment(FileName:AnsiString);
Begin
  Inc(_AttachCount);
  SetLength(_Attachments,_AttachCount);
  _Attachments[Pred(_AttachCount)]:=FileName;
End;

Function Mail.GetStatus(Var S:AnsiString):Integer;
Var
  I:Integer;
  S2:AnsiString;
Begin
  Repeat
    _Stream.ReadString(S2);
  Until (Pos('250-',S2)<=0);
  I:=Pos(' ',S2);
  If I>0 Then
  Begin
    S:=Copy(S2,Succ(I),MaxInt);
    S2:=Copy(S2,1,Pred(I));
  End Else
    S:='';
  Result := StringToInt(S2);
End;

Function Mail.Send(Address:AnsiString):Boolean;
Var
  S,S2,MS:AnsiString;
  I:Integer;
  K:Byte;
  Host:AnsiString;
  Code:Integer;
  FileName,Attach:AnsiString;
  Bin:FileStream;
Begin
  Result:=False;

  If (Address='') Then
  Begin
    RaiseError('SMTP.Send: Invalid recipient.');
    Exit;
  End;

  If (Subject='') Then
  Begin
    RaiseError('SMTP.Send: Invalid subject.');
    Exit;
  End;

  If HostName<>'' Then
    Host:=HostName
  Else
  Begin
    I:=Pos('@',Address);
    Host:=Copy(Address,Succ(I),MaxInt);
  End;

  // check for required parameters
  If (Host='') Then
  Begin
    RaiseError('SMTP.Send: Invalid host.');
    Exit;
  End;

  _Stream := Socket.Create(Host, Port);

// receive signon message
  Code:=GetStatus(S2);
  If Code<>220 Then
  Begin
    RaiseError('SMTP.SendMail: Signon failed. [Code='+IntToString(Code)+']');
    Exit;
  End;

// send HELO message
  S:='EHLO '+Username+#13#10;
  _Stream.WriteString(S);

  Code:=GetStatus(S2);
  If Code<>250 Then
  Begin
    RaiseError('SMTP.SendMail: Failed username identify.[Code='+IntToString(Code)+']'+#13#10+S2);
    Exit;
  End;

  // Authenticate
  S:='AUTH PLAIN'+#13#10;
  _Stream.WriteString(S);
  Code:=GetStatus(S2);
  If Code<>334 Then
  Begin
    RaiseError('SMTP.SendMail: Failed auth request[Code='+IntToString(Code)+']'+#13#10+S2);
    Exit;
  End;

  S:=StringToBase64(#0+UserName+#0+Password)+#13#10;
  _Stream.WriteString(S);
  Code:=GetStatus(S2);
  If Code<>235 Then
  Begin
    RaiseError('SMTP.SendMail: Authentication failed[Code='+IntToString(Code)+']'+#13#10+S2);
    Exit;
  End;

// send MAIL message
  If (Sender<>'') Then
  Begin
    S:='MAIL FROM: <'+Sender;
    S:=S+'>'+#13#10;
  End Else
    S:='MAIL FROM:<'+UserName+'>'+#13#10;

  _Stream.WriteString(S);
  Code:=GetStatus(S2);
  If Code<>250 Then
  Begin
    RaiseError('SMTP.SendMail: Failed "mail from" message.[Code='+IntToString(Code)+']'+#13#10+S2);
    Exit;
  End;

// send RCPT message
  S:='RCPT TO: <'+Address+'>'#13#10;

  _Stream.WriteString(S);
  Code:=GetStatus(S2);
  If (Code<>250)And(Code<>251) Then
  Begin
    RaiseError('SMTP.SendMail: Failed "rcpt" message.[Code='+IntToString(Code)+']'+#13#10+S2);
    Exit;
  End;

// send DATA message
  S:='DATA'+#13#10;

  _Stream.WriteString(S);
  Code:=GetStatus(S2);
  If Code<>354 Then
  Begin
    RaiseError('SMTP.SendMail: Failed data message.[Code='+IntToString(Code)+']'+#13#10+S2);
    Exit;
  End;

// X-Mailer:
  _Stream.WriteString(MailerID);

// Message-ID:
  If (MessageID<>'') Then
  Begin
    S:='Message-ID: '+MessageID+#13#10;
    _Stream.WriteString(S);
  End;

// To:
  S:='To: '+Address+#13#10;
  _Stream.WriteString(S);

// From:
  If (Sender<>'') Then
  Begin
    S:='From: '+Sender;
    If (SenderName<>'') Then
      S:=S+' ('+SenderName+')';
    S:=S+#13#10;
  End Else
    S:='From: '+UserName+#13#10;
  _Stream.WriteString(S);

// Reply-To:
  If (ReplyTo<>'') Then
  Begin
    S:='Reply-To: '+ReplyTo;
    If (ReplyToName<>'') Then
      S:=S+' ('+ReplyToName+')';
    S:=S+#13#10;
    _Stream.WriteString(S);
	End;

// Subject:
  S:='Subject: '+Subject+#13#10;
  _Stream.WriteString(S);

// Mime version:
  S:='MIME-Version: 1.0'+#13#10;
  _Stream.WriteString(S);

// Content type
  S:='Content-Type: multipart/mixed; boundary= "KkK170891tpbkKk__FV_KKKkkkjjwq"'+#13#10+#13#10;
  _Stream.WriteString(S);

// empty line needed after headers, RFC822
  S:=#13#10;
  _Stream.WriteString(S);

  MS:='';
  For I:=0 To _AttachCount Do
  Begin
    If I=0 Then
    Begin
      MS:=MS+ '--KkK170891tpbkKk__FV_KKKkkkjjwq'#13#10+
              'Content-Type: text/plain; charset=US-ASCII'+#13#10+#13#10+
              Message+#13#10+#13#10;
    End Else
    Begin
      S2 := '';
      Bin := FileStream.Open(_Attachments[Pred(I)]);
      While Not Bin.EOF Do
      Begin
        Bin.Read(K,1);
        S2:=S2+Char(K);
      End;
      Bin.Destroy;

      FileName:=GetFileName(_Attachments[Pred(I)],False);

      Attach:=StringToBase64(S2);
      MS:=MS+ 'Content-Type: application/octet-stream'+#13#10+
              'Content-Transfer-Encoding: base64'+#13#10+
              'Content-Disposition: attachment; filename= "'+FileName+'"'+#13#10+#13#10+
              Attach+#13#10;
    End;

    MS:=MS+'--KkK170891tpbkKk__FV_KKKkkkjjwq';
    If I=_AttachCount Then
      MS:=MS+'--';
    MS:=MS+#13#10;
  End;

// send message text
  _Stream.WriteString(MS);

// send message terminator and receive reply
  S:=#13#10+'.'+#13#10;
  _Stream.WriteString(S);
  Code:=GetStatus(S2);
  If Code<>250 Then
  Begin
    RaiseError('SMTP.SendMail: Failed message data.[Code='+IntToString(Code)+']'+#13#10+S2);
    Exit;
  End;

// send QUIT message
  S:='QUIT'#13#10;
  _Stream.WriteString(S);
  Code:=GetStatus(S2);
  If Code<>221 Then
  Begin
    RaiseError('SMTP.SendMail: Failed "quit" message.[Code='+IntToString(Code)+']'+#13#10+S2);
    Exit;
  End;

  _Stream.Destroy;
  Result:=True;
End;

End.
