Unit TERRA_ColorNames;

Interface
Uses TERRA_String, TERRA_Color;

Function CreateColorFromName(Const Name:TERRAString):ColorRGBA;

Implementation

Type
  NamedColor = Record
    Name:TERRAString;
    Value:ColorRGBA;
  End;

Const
 NamedColorsCount = 147;

 NamedColors :Array[0..Pred(NamedColorsCount)] of NamedColor =
  ((name:'aliceblue';            Value:( R:240; g:248; b:255; a:255 )) ,
   (name:'antiquewhite';         Value:( R:250; g:235; b:215; a:255 )) ,
   (name:'aqua';                 Value:( R:0;   g:255; b:255; a:255 )) ,
   (name:'aquamarine';           Value:( R:127; g:255; b:212; a:255 )) ,
   (name:'azure';                Value:( R:240; g:255; b:255; a:255 )) ,
   (name:'beige';                Value:( R:245; g:245; b:220; a:255 )) ,
   (name:'bisque';               Value:( R:255; g:228; b:196; a:255 )) ,
   (name:'black';                Value:( R:0;   g:0;   b:0;   a:255 )) ,
   (name:'blanchedalmond';       Value:( R:255; g:235; b:205; a:255 )) ,
   (name:'blue';                 Value:( R:0;   g:0;   b:255; a:255 )) ,
   (name:'blueviolet';           Value:( R:138; g:43;  b:226; a:255 )) ,
   (name:'brown';                Value:( R:165; g:42;  b:42;  a:255 )) ,
   (name:'burlywood';            Value:( R:222; g:184; b:135; a:255 )) ,
   (name:'cadetblue';            Value:( R:95;  g:158; b:160; a:255 )) ,
   (name:'chartreuse';           Value:( R:127; g:255; b:0;   a:255 )) ,
   (name:'chocolate';            Value:( R:210; g:105; b:30;  a:255 )) ,
   (name:'coral';                Value:( R:255; g:127; b:80;  a:255 )) ,
   (name:'cornflowerblue';       Value:( R:100; g:149; b:237; a:255 )) ,
   (name:'cornsilk';             Value:( R:255; g:248; b:220; a:255 )) ,
   (name:'crimson';              Value:( R:220; g:20;  b:60;  a:255 )) ,
   (name:'cyan';                 Value:( R:0;   g:255; b:255; a:255 )) ,
   (name:'darkblue';             Value:( R:0;   g:0;   b:139; a:255 )) ,
   (name:'darkcyan';             Value:( R:0;   g:139; b:139; a:255 )) ,
   (name:'darkgoldenrod';        Value:( R:184; g:134; b:11;  a:255 )) ,
   (name:'darkgray';             Value:( R:169; g:169; b:169; a:255 )) ,
   (name:'darkgreen';            Value:( R:0;   g:100; b:0;   a:255 )) ,
   (name:'darkgrey';             Value:( R:169; g:169; b:169; a:255 )) ,
   (name:'darkkhaki';            Value:( R:189; g:183; b:107; a:255 )) ,
   (name:'darkmagenta';          Value:( R:139; g:0;   b:139; a:255 )) ,
   (name:'darkolivegreen';       Value:( R:85;  g:107; b:47;  a:255 )) ,
   (name:'darkorange';           Value:( R:255; g:140; b:0;   a:255 )) ,
   (name:'darkorchid';           Value:( R:153; g:50;  b:204; a:255 )) ,
   (name:'darkred';              Value:( R:139; g:0;   b:0;   a:255 )) ,
   (name:'darksalmon';           Value:( R:233; g:150; b:122; a:255 )) ,
   (name:'darkseagreen';         Value:( R:143; g:188; b:143; a:255 )) ,
   (name:'darkslateblue';        Value:( R:72;  g:61;  b:139; a:255 )) ,
   (name:'darkslategray';        Value:( R:47;  g:79;  b:79;  a:255 )) ,
   (name:'darkslategrey';        Value:( R:47;  g:79;  b:79;  a:255 )) ,
   (name:'darkturquoise';        Value:( R:0;   g:206; b:209; a:255 )) ,
   (name:'darkviolet';           Value:( R:148; g:0;   b:211; a:255 )) ,
   (name:'deeppink';             Value:( R:255; g:20;  b:147; a:255 )) ,
   (name:'deepskyblue';          Value:( R:0;   g:191; b:255; a:255 )) ,
   (name:'dimgray';              Value:( R:105; g:105; b:105; a:255 )) ,
   (name:'dimgrey';              Value:( R:105; g:105; b:105; a:255 )) ,
   (name:'dodgerblue';           Value:( R:30;  g:144; b:255; a:255 )) ,
   (name:'firebrick';            Value:( R:178; g:34;  b:34;  a:255 )) ,
   (name:'floralwhite';          Value:( R:255; g:250; b:240; a:255 )) ,
   (name:'forestgreen';          Value:( R:34;  g:139; b:34;  a:255 )) ,
   (name:'fuchsia';              Value:( R:255; g:0;   b:255; a:255 )) ,
   (name:'gainsboro';            Value:( R:220; g:220; b:220; a:255 )) ,
   (name:'ghostwhite';           Value:( R:248; g:248; b:255; a:255 )) ,
   (name:'gold';                 Value:( R:255; g:215; b:0;   a:255 )) ,
   (name:'goldenrod';            Value:( R:218; g:165; b:32;  a:255 )) ,
   (name:'gray';                 Value:( R:128; g:128; b:128; a:255 )) ,
   (name:'green';                Value:( R:0;   g:128; b:0;   a:255 )) ,
   (name:'greenyellow';          Value:( R:173; g:255; b:47;  a:255 )) ,
   (name:'grey';                 Value:( R:128; g:128; b:128; a:255 )) ,
   (name:'honeydew';             Value:( R:240; g:255; b:240; a:255 )) ,
   (name:'hotpink';              Value:( R:255; g:105; b:180; a:255 )) ,
   (name:'indianred';            Value:( R:205; g:92;  b:92;  a:255 )) ,
   (name:'indigo';               Value:( R:75;  g:0;   b:130; a:255 )) ,
   (name:'ivory';                Value:( R:255; g:255; b:240; a:255 )) ,
   (name:'khaki';                Value:( R:240; g:230; b:140; a:255 )) ,
   (name:'lavender';             Value:( R:230; g:230; b:250; a:255 )) ,
   (name:'lavenderblush';        Value:( R:255; g:240; b:245; a:255 )) ,
   (name:'lawngreen';            Value:( R:124; g:252; b:0;   a:255 )) ,
   (name:'lemonchiffon';         Value:( R:255; g:250; b:205; a:255 )) ,
   (name:'lightblue';            Value:( R:173; g:216; b:230; a:255 )) ,
   (name:'lightcoral';           Value:( R:240; g:128; b:128; a:255 )) ,
   (name:'lightcyan';            Value:( R:224; g:255; b:255; a:255 )) ,
   (name:'lightgoldenrodyellow'; Value:( R:250; g:250; b:210; a:255 )) ,
   (name:'lightgray';            Value:( R:211; g:211; b:211; a:255 )) ,
   (name:'lightgreen';           Value:( R:144; g:238; b:144; a:255 )) ,
   (name:'lightgrey';            Value:( R:211; g:211; b:211; a:255 )) ,
   (name:'lightpink';            Value:( R:255; g:182; b:193; a:255 )) ,
   (name:'lightsalmon';          Value:( R:255; g:160; b:122; a:255 )) ,
   (name:'lightseagreen';        Value:( R:32;  g:178; b:170; a:255 )) ,
   (name:'lightskyblue';         Value:( R:135; g:206; b:250; a:255 )) ,
   (name:'lightslategray';       Value:( R:119; g:136; b:153; a:255 )) ,
   (name:'lightslategrey';       Value:( R:119; g:136; b:153; a:255 )) ,
   (name:'lightsteelblue';       Value:( R:176; g:196; b:222; a:255 )) ,
   (name:'lightyellow';          Value:( R:255; g:255; b:224; a:255 )) ,
   (name:'lime';                 Value:( R:0;   g:255; b:0;   a:255 )) ,
   (name:'limegreen';            Value:( R:50;  g:205; b:50;  a:255 )) ,
   (name:'linen';                Value:( R:250; g:240; b:230; a:255 )) ,
   (name:'magenta';              Value:( R:255; g:0;   b:255; a:255 )) ,
   (name:'maroon';               Value:( R:128; g:0;   b:0;   a:255 )) ,
   (name:'mediumaquamarine';     Value:( R:102; g:205; b:170; a:255 )) ,
   (name:'mediumblue';           Value:( R:0;   g:0;   b:205; a:255 )) ,
   (name:'mediumorchid';         Value:( R:186; g:85;  b:211; a:255 )) ,
   (name:'mediumpurple';         Value:( R:147; g:112; b:219; a:255 )) ,
   (name:'mediumseagreen';       Value:( R:60;  g:179; b:113; a:255 )) ,
   (name:'mediumslateblue';      Value:( R:123; g:104; b:238; a:255 )) ,
   (name:'mediumspringgreen';    Value:( R:0;   g:250; b:154; a:255 )) ,
   (name:'mediumturquoise';      Value:( R:72;  g:209; b:204; a:255 )) ,
   (name:'mediumvioletred';      Value:( R:199; g:21;  b:133; a:255 )) ,
   (name:'midnightblue';         Value:( R:25;  g:25;  b:112; a:255 )) ,
   (name:'mintcream';            Value:( R:245; g:255; b:250; a:255 )) ,
   (name:'mistyrose';            Value:( R:255; g:228; b:225; a:255 )) ,
   (name:'moccasin';             Value:( R:255; g:228; b:181; a:255 )) ,
   (name:'navajowhite';          Value:( R:255; g:222; b:173; a:255 )) ,
   (name:'navy';                 Value:( R:0;   g:0;   b:128; a:255 )) ,
   (name:'oldlace';              Value:( R:253; g:245; b:230; a:255 )) ,
   (name:'olive';                Value:( R:128; g:128; b:0;   a:255 )) ,
   (name:'olivedrab';            Value:( R:107; g:142; b:35;  a:255 )) ,
   (name:'orange';               Value:( R:255; g:165; b:0;   a:255 )) ,
   (name:'orangered';            Value:( R:255; g:69;  b:0;   a:255 )) ,
   (name:'orchid';               Value:( R:218; g:112; b:214; a:255 )) ,
   (name:'palegoldenrod';        Value:( R:238; g:232; b:170; a:255 )) ,
   (name:'palegreen';            Value:( R:152; g:251; b:152; a:255 )) ,
   (name:'paleturquoise';        Value:( R:175; g:238; b:238; a:255 )) ,
   (name:'palevioletred';        Value:( R:219; g:112; b:147; a:255 )) ,
   (name:'papayawhip';           Value:( R:255; g:239; b:213; a:255 )) ,
   (name:'peachpuff';            Value:( R:255; g:218; b:185; a:255 )) ,
   (name:'peru';                 Value:( R:205; g:133; b:63;  a:255 )) ,
   (name:'pink';                 Value:( R:255; g:192; b:203; a:255 )) ,
   (name:'plum';                 Value:( R:221; g:160; b:221; a:255 )) ,
   (name:'powderblue';           Value:( R:176; g:224; b:230; a:255 )) ,
   (name:'purple';               Value:( R:128; g:0;   b:128; a:255 )) ,
   (name:'red';                  Value:( R:255; g:0;   b:0;   a:255 )) ,
   (name:'rosybrown';            Value:( R:188; g:143; b:143; a:255 )) ,
   (name:'royalblue';            Value:( R:65;  g:105; b:225; a:255 )) ,
   (name:'saddlebrown';          Value:( R:139; g:69;  b:19;  a:255 )) ,
   (name:'salmon';               Value:( R:250; g:128; b:114; a:255 )) ,
   (name:'sandybrown';           Value:( R:244; g:164; b:96;  a:255 )) ,
   (name:'seagreen';             Value:( R:46;  g:139; b:87;  a:255 )) ,
   (name:'seashell';             Value:( R:255; g:245; b:238; a:255 )) ,
   (name:'sienna';               Value:( R:160; g:82;  b:45;  a:255 )) ,
   (name:'silver';               Value:( R:192; g:192; b:192; a:255 )) ,
   (name:'skyblue';              Value:( R:135; g:206; b:235; a:255 )) ,
   (name:'slateblue';            Value:( R:106; g:90;  b:205; a:255 )) ,
   (name:'slategray';            Value:( R:112; g:128; b:144; a:255 )) ,
   (name:'slategrey';            Value:( R:112; g:128; b:144; a:255 )) ,
   (name:'snow';                 Value:( R:255; g:250; b:250; a:255 )) ,
   (name:'springgreen';          Value:( R:0;   g:255; b:127; a:255 )) ,
   (name:'steelblue';            Value:( R:70;  g:130; b:180; a:255 )) ,
   (name:'tan';                  Value:( R:210; g:180; b:140; a:255 )) ,
   (name:'teal';                 Value:( R:0;   g:128; b:128; a:255 )) ,
   (name:'thistle';              Value:( R:216; g:191; b:216; a:255 )) ,
   (name:'tomato';               Value:( R:255; g:99;  b:71;  a:255 )) ,
   (name:'turquoise';            Value:( R:64;  g:224; b:208; a:255 )) ,
   (name:'violet';               Value:( R:238; g:130; b:238; a:255 )) ,
   (name:'wheat';                Value:( R:245; g:222; b:179; a:255 )) ,
   (name:'white';                Value:( R:255; g:255; b:255; a:255 )) ,
   (name:'whitesmoke';           Value:( R:245; g:245; b:245; a:255 )) ,
   (name:'yellow';               Value:( R:255; g:255; b:0;   a:255 )) ,
   (name:'yellowgreen';          Value:( R:154; g:205; b:50;  a:255 ))
   );



Function CreateColorFromName(Const Name:TERRAString):ColorRGBA;
Var
  I:Integer;
Begin
  For I:=0 To Pred(NamedColorsCount) Do
  If StringEquals(NamedColors[I].Name, Name) Then
  Begin
    Result := NamedColors[I].Value;
    Exit;
  End;

  Result := ColorNull;
End;

End.