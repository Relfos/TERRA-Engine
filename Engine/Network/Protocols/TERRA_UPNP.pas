{
UPNP engine, for port forwarding
BASED ON:
http://www.codeproject.com/Articles/27237/Easy-Port-Forwarding-and-Managing-Router-with-UPnP
}

Unit TERRA_UPNP;
{$I terra.inc}

Interface
Uses TERRA_String, TERRA_Utils, TERRA_Network, TERRA_Sockets;

Const
  natDefaultTimeOut  = 10;
  natDefaultInterval = 200;

Type
  NAT_STAT = (
    NAT_INIT=0,
		NAT_FOUND,
		NAT_TCP_CONNECTED,
		NAT_GETDESCRIPTION,
		NAT_GETCONTROL,
		NAT_ADD,
		NAT_DEL,
		NAT_GET,
		NAT_ERROR
  );

  UPNP = Class(TERRAObject)
    Protected
	    udp_socket_fd:Integer;
	    tcp_socket_fd:Integer;
	    status:NAT_STAT;
      time_out:Integer;
      interval:Integer;
	    service_type:AnsiString;
	    describe_ur:AnsiString;
	    control_url:AnsiString;
	    base_url:AnsiString;
      describe_url:AnsiString;
	    description_info:AnsiString;
	    last_error:AnsiString;
	    mapping_info:AnsiString;

	    Function get_description():Boolean;
	    Function parser_description():Boolean;
	    Function tcp_connect(Const _host:AnsiString; _port:Word):Boolean;
	    //Function parse_mapping_info():Boolean;

    Public
	    Constructor Create(time_out:Integer=natDefaultTimeOut; interval:Integer=natDefaultInterval); //init
	    Function discovery():Boolean;//find router

	(****
	 **** _description: port mapping name
	 **** _destination_ip: internal ip address
	 **** _port_ex:external: external listen port
	 **** _destination_port: internal port
	 **** _protocal: TCP or UDP
	 ***)
	    Function add_port_mapping(_description, _destination_ip:AnsiString; _port_ex, _port_in:Word; _protocol:AnsiString):Boolean;//add port mapping

      Function get_last_error():AnsiString;
  End;

Function AddUPnPEntry(Port: Integer; const Name: ShortString; LAN_IP:AnsiString):Boolean;

Implementation
Uses TERRA_Log, TERRA_XML, TERRA_Application, TERRA_OS
{$IFDEF WINDOWS},ActiveX, Comobj{$ENDIF};

Function AddUPnPEntry(Port: Integer; const Name: ShortString; LAN_IP:AnsiString):Boolean;
{$IFDEF WINDOWS}
Var
  Nat: Variant;
  Ports: Variant;
  SavedCW: Word;
  res:HRESULT ;
Begin
  Result := False;

  res := CoInitialize(Nil); // Must be NULL
  if (FAILED(Res)) Then
    Exit;

  if NOT(LAN_IP = '127.0.0.1') then
  begin
    try
      Nat := CreateOleObject('HNetCfg.NATUPnP');
      Ports := Nat.StaticPortMappingCollection;

      // Error Raized From Here!!!
      Log(logDebug, 'UPNP', Ports.count);

      Ports.Add(Port, 'TCP', Port, LAN_IP, True, name);
      Result := True;
    except
      Log(logError, 'UPNP', 'An Error occured with adding UPnP Ports. The ' + name +
        ' port was not added to the router. Please check to see if  your ' +
        'router supports UPnP and has it enabled or disable UPnP.');
    end;
  end;
End;
{$ELSE}
Begin
  Result := False;
End;
{$ENDIF}

Function UPNP.get_last_error():AnsiString;
Begin
  Result := last_error;
End;

Const
  MAX_BUFF_SIZE = 102400;

Function parseUrl(url:AnsiString; Var host:AnsiString; Var port:Word; Var path:AnsiString):Boolean;
Var
  str_url, str_port:AnsiString;
  I, pos1,pos2,pos3:Integer;
Begin
	str_url := url;

	pos1 := Pos('://', str_url);
	If (pos1<=0) Then
  Begin
    Result := False;
    Exit;
  End;

	pos1 := pos1+3;
  pos2 := 0;
  For I:=pos1 To Length(str_url) Do
	If (str_url[I] = ':') Then
  Begin
    pos2 := I;
    Break;
  End;

	If (pos2<=0) Then
	Begin
		port := 80;
		pos3 := 0;
    For I:=pos1 To Length(str_url) Do
    If str_url[I] = '/' Then
    Begin
      pos3 := I;
      Break;
    End;

		If (pos3<=0) Then
		Begin
			Result := False;
      Exit;
    End;

		host := Copy(str_url, pos1, pos3-pos1);
	End Else
	Begin
		host := Copy(str_url, pos1, pos2-pos1);
		pos3 := 0;
    For I:=pos1 To Length(str_url) Do
    If (str_url[I] = '/') Then
    Begin
      pos3 := I;
      Break;
    End;

		If (pos3<=0) Then
		Begin
			Result := False;
      Exit;
		End;

		str_port := Copy(str_url, pos2+1, pos3-pos2-1);
		port := StringToInt(str_port);
	End;

	If (pos3+1 >= Length(str_url)) Then
		path := '/'
	Else
		path := Copy(str_url, pos3, Length(str_url));

	Result := True;
End;

// Discovery constants
Const
  HTTPMU_HOST_ADDRESS = '239.255.255.250';
  HTTPMU_HOST_PORT = 1900;
  SEARCH_REQUEST_STRING =
  'M-SEARCH * HTTP/1.1'+#13#10+
  'ST:UPnP:rootdevice'+#13#10+
	'MX: 3'+#13#10+
	'Man:"ssdp:discover"'+#13#10+
  'HOST: 239.255.255.250:1900'+#13#10+
	#13#10;
  HTTP_OK = '200 OK';
  DEFAULT_HTTP_PORT = 80;


(******************************************************************
** Device and Service  Defines                                                 *
*******************************************************************)

  DEVICE_TYPE_1	= 'urn:schemas-upnp-org:device:InternetGatewayDevice:1';
  DEVICE_TYPE_2	= 'urn:schemas-upnp-org:device:WANDevice:1';
  DEVICE_TYPE_3	= 'urn:schemas-upnp-org:device:WANConnectionDevice:1';

  SERVICE_WANIP	= 'urn:schemas-upnp-org:service:WANIPConnection:1';
  SERVICE_WANPPP	= 'urn:schemas-upnp-org:service:WANPPPConnection:1';

(******************************************************************
** Action Defines
*******************************************************************)
  HTTP_HEADER_ACTION =
    'POST $A HTTP/1.1'+#13#10+
    'HOST: $B:$C'+#13#10+
    'SOAPACTION:"$D#$E"'+#13#10+
    'CONTENT-TYPE: text/xml ; charset="utf-8"'+#13#10+
    'Content-Length: $F'+#13#10+#13#10;

  SOAP_ACTION =
    '<?xml version=\"1.0\" encoding=\"utf-8\"?>'+#13#10+
    '<s:Envelope xmlns:s='+
    '"http://schemas.xmlsoap.org/soap/envelope/" '+
    's:encodingStyle='+
    '"http://schemas.xmlsoap.org/soap/encoding/">'+#13#10+
    '<s:Body>'+#13#10+
    '<u:$A xmlns:u="$B">'+#13#10+'$C'+
    '</u:$D>'+#13#10+
    '</s:Body>'+#13#10+
    '</s:Envelope>'+#13#10;

  PORT_MAPPING_LEASE_TIME = '63072000'; //two year

  ADD_PORT_MAPPING_PARAMS =
    '<NewRemoteHost></NewRemoteHost>'+#13#10+
    '<NewExternalPort>$A</NewExternalPort>'+#13#10+
    '<NewProtocol>$B</NewProtocol>'+#13#10+
    '<NewInternalPort>$C</NewInternalPort>'+#13#10+
    '<NewInternalClient>$D</NewInternalClient>'+#13#10+
    '<NewEnabled>1</NewEnabled>'+#13#10+
    '<NewPortMappingDescription>$E</NewPortMappingDescription>'+#13#10+
    '<NewLeaseDuration>'+
    PORT_MAPPING_LEASE_TIME+
    '</NewLeaseDuration>'+#13#10;

  ACTION_ADD = 'AddPortMapping';
//*********************************************************************************

Constructor UPNP.Create(time_out, interval:Integer);
Begin
  Self.time_out := time_out;
  Self.interval := interval;
	status := NAT_INIT;
End;

Function UPNP.tcp_connect(Const _host:AnsiString; _port:Word):Boolean;
Var
  ret,i:Integer;
  T, StartTime:Cardinal;
  r_address:SocketAddress;
Begin
	tcp_socket_fd := socket(PF_INET{AF_INET}, SOCK_STREAM, 0);

  r_address.Family := PF_INET;
	r_address.Port := htons(_port);
  r_address.Address := inet_addr(PAnsiChar(_host));

  StartTime := GetTime();
	For i:=1 To time_out Do
	Begin
		If(i>1) Then
    Repeat
      T := GetTime - StartTime;
    Until (T>=1000);
    StartTime := GetTime;

		ret := connect(tcp_socket_fd, r_address, sizeof(SocketAddress));
		If (ret=0) Then
		Begin
			status := NAT_TCP_CONNECTED;
			Result := True;
      Exit;
    End;
  End;

	status := NAT_ERROR;
	last_error := 'Fail to connect to '+_Host+':'+IntToString(_Port)+' (using TCP)';
	Result := False;
End;

Function UPNP.Discovery():Boolean;
Var
  val:Integer;
  udp_socket_fd:Integer;
  i,j,ret:Integer;
  send_buff, recv_buff:AnsiString;
  buff:Array[0..MAX_BUFF_SIZE+1] Of AnsiChar; //buff should be enough big
  r_address, temp2:SocketAddress;
  bOptVal,bOptLen:Integer;
  Temp, _begin, _end:Integer;
Begin
  udp_socket_fd := socket(PF_INET, SOCK_DGRAM, 0);
  send_buff := SEARCH_REQUEST_STRING;

  FillChar(r_address, SizeOf(r_address), 0);
  r_address.Family := PF_INET;
  r_address.port := htons(HTTPMU_HOST_PORT);
  r_address.Address := inet_addr(HTTPMU_HOST_ADDRESS);

  bOptVal := 1;
  bOptLen := sizeof(bOptVal);

  ret := setsockopt(udp_socket_fd, SOL_SOCKET, SO_BROADCAST, @bOptVal, bOptLen);
  IntToString(reT);

  ret := sendto(udp_socket_fd, send_buff[1], Length(send_buff), 0, r_address, sizeof(r_address));
  IntToString(reT);

  val := 1;
  ioctlsocket(udp_socket_fd, FIONBIO, val);//none block

  time_out := 30;
	for I:=1 To time_out Do
  Begin
		FillChar(buff, sizeof(buff), 0);
    Temp := SizeOf(Temp2);
    ret := recvfrom(udp_socket_fd, buff, MAX_BUFF_SIZE, 0, Temp2, Temp);
		if (ret=SOCKET_ERROR) Then
    Begin
      ret := WSAGetLastError();
      IntToString(reT);

			Sleep(1000);
      Continue;
		End;

		recv_buff := buff;
    ret := Pos(HTTP_OK, recv_buff);
    if (ret<=0) Then
      Continue; //invalid response

    _begin := Pos('http://', recv_buff);
    If (_begin<=0) Then
      Continue; //invalid response
    _end := 0;
    For J:=_begin To Length(recv_buff) Do
    If (recv_buff[J]=#10) Then
    Begin
      _end := J;
      Break;
    End;

    If (_end<=0) Then
			Continue;    //invalid response

		describe_url := Copy(recv_buff, _begin, _end - _begin);

		if(Not get_description()) Then
    Begin
			Sleep(1000);
			Continue;
		End;

		if (Not parser_description()) Then
    Begin
			Sleep(1000);
			Continue;
		End;

    Closesocket(udp_socket_fd);
		status := NAT_FOUND; //founnd a router
		Result := True;
    Exit;
  End;

	status := NAT_ERROR;
	last_error := 'Fail to find an UPNP NAT.';
  Result := False;                               //no router finded
End;

Function UPNP.get_description():Boolean;
Var
	host,path:AnsiString;
  port:Word;
  ret:Boolean;
  http_request, response:AnsiString;
  request:String[200];
  buff:Array[0..MAX_BUFF_SIZE+1] Of AnsiChar;
Begin
	ret := parseUrl(describe_url, host, port, path);
	if (Not ret) Then
	Begin
		status := NAT_ERROR;
		last_error := 'Failed to parseURl: '+describe_url;
		Result := False;
	End;

	//connect
	ret := tcp_connect(host, port);
	if(Not ret) Then
  Begin
		Result := False;
    Exit;
	End;

	request := 'GET '+ path +' HTTP/1.1'+#13#10+'Host: '+host+':'+IntToString(Port)+#13#10#13#10;
	http_request := request;

	//send request
	ret := send(tcp_socket_fd, http_request, Length(http_request), 0)>0;
	//get description xml file
	FillChar(buff, 0, sizeof(buff));

  ret := recv(tcp_socket_fd,buff,MAX_BUFF_SIZE,0) >0;
	While (ret) Do
	Begin
		response := response + buff;
		FillChar(buff, Sizeof(buff), 0);
    ret := recv(tcp_socket_fd,buff,MAX_BUFF_SIZE,0) >0;
	End;

	description_info := response;
	Result := True;
End;

Function UPNP.parser_description():Boolean;
Var
  doc:XMLDocument;
  serviceType:AnsiString;
  node, baseURL_node, controlURL_node:XMLNode;
  device_node,deviceList_node,deviceType_node:XMLNode;
  serviceList_node,service_node,serviceType_node:XMLNode;
  is_found:Boolean;
  I, num, index:Integer;
Begin
	doc := XMLDocument.Create;
  doc.LoadFromString(description_info); //"root"
  node := doc.Root;
	If (node = Nil) Then
	Begin
		status := NAT_ERROR;
		last_error := 'The device descripe XML file is not a valid XML file. Cant find root element.';
		Result := False;
	End;

	baseURL_node := node.GetNodeByName('URLBase');
	If (baseURL_node=Nil) Or (baseURL_node.Value='') Then
	Begin
    index := 0;
    For I:= 7 To Length(describe_url) Do
    If (describe_url[I] = '/') Then
    Begin
      index := I;
      Break;
    End;

		If (index<=0) Then
		Begin
			status := NAT_ERROR;
			last_error := 'Fail to get base_URL from XMLNode URLBase or describe_url.';
			Result := False;
      Exit;
		End;

		base_url := Copy(describe_url, 1, index);
  End Else
    base_url := baseURL_node.Value;

  device_node := Nil;
	For i:=0 To Pred(node.NodeCount) Do
	Begin
		device_node := node.GetNodeByIndex(I);
		If (device_node = Nil) Then
			break;

    If (Not StringEquals(device_node.Name, 'device')) Then
    Begin
      device_node := Nil;
      Continue;
    End;

		If (device_node.NodeCount<=0) Then
    Begin
      device_node := Nil;
			break;
    End;

		deviceType_node := device_node.GetNodeByName('deviceType');
		if (deviceType_node.Value = DEVICE_TYPE_1) Then
			break;
	End;

	If (device_node = Nil) Then
	Begin
		status := NAT_ERROR;
		last_error := 'Fail to find device "urn:schemas-upnp-org:device:InternetGatewayDevice:1"';
		Result := False;
    Exit;
	End;

	deviceList_node := device_node.GetNodeByName('deviceList');
	If (deviceList_node = Nil) Or (deviceList_node.NodeCount<=0) Then
	Begin
		status := NAT_ERROR;
		last_error := ' Fail to find deviceList of device "urn:schemas-upnp-org:device:InternetGatewayDevice:1 "';
		Result := False;
    Exit;
	End;

	// get urn:schemas-upnp-org:device:WANDevice:1 and it's devicelist
	For I:=0 To Pred(deviceList_node.NodeCount) Do
	Begin
		device_node := deviceList_node.GetNodeByIndex(I);
		If (device_node = Nil) Then
			break;
    If (device_node.Name<>'device') Then
    Begin
      device_node := Nil;
      Continue;
    End;

		deviceType_node := device_node.GetNodeByName('deviceType');
		If (deviceType_node.Value = DEVICE_TYPE_2) Then
			Break;
	End;

	If (device_node = Nil) Then
	Begin
		status := NAT_ERROR;
		last_error := 'Fail to find device "urn:schemas-upnp-org:device:WANDevice:1 "';
		Result := False;
    Exit;
	End;

	deviceList_node := device_node.GetNodeByName('deviceList');
	If (deviceList_node = Nil) Then
	Begin
		status := NAT_ERROR;
		last_error := ' Fail to find deviceList of device "urn:schemas-upnp-org:device:WANDevice:1 "';
		Result := False;
	End;

	// get urn:schemas-upnp-org:device:WANConnectionDevice:1 and it's servicelist
	For i:=0 To Pred(deviceList_node.NodeCount) Do
	Begin
		device_node := deviceList_node.GetNodeByIndex(i);
		If (device_node = Nil) Then
			break;
    if (device_node.Name<>'device') Then
    Begin
      device_node := Nil;
      Continue;
    End;

		deviceType_node := device_node.GetNodeByName('deviceType');
		If (deviceType_node.Value = DEVICE_TYPE_3) Then
			Break;
	End;

	If (device_node = Nil) Then
	Begin
		status := NAT_ERROR;
		last_error := 'Fail to find device "urn:schemas-upnp-org:device:WANConnectionDevice:1"';
		Result := False;
    Exit;
  End;

	serviceList_node := device_node.GetNodeByName('serviceList');
	if (serviceList_node = Nil) Then
	Begin
		status := NAT_ERROR;
		last_error := 'Fail to find serviceList of device "urn:schemas-upnp-org:device:WANDevice:1 "';
		Result := False;
    Exit;
	End;

	is_found := False;
	for I:=0 To Pred(serviceList_node.NodeCount) Do
	Begin
		service_node := serviceList_node.GetNodeByIndex(I);
		if (service_node = Nil) Then
			break;
    If (service_node.Name<>'service') Then
    Begin
      service_node := Nil;
      Continue;
    End;

		serviceType_node := service_node.GetNodeByName('serviceType');
		if(serviceType_node = Nil) Then
			Continue;

		serviceType := serviceType_node.Value;
		if (serviceType=SERVICE_WANIP) Or (serviceType=SERVICE_WANPPP) Then
		Begin
			is_found := True;
			break;
		End;
	End;

	If (Not is_found) Then
	Begin
		status := NAT_ERROR;
		last_error := 'cant find  " SERVICE_WANIP " or " SERVICE_WANPPP " service.';
		Result := False;
    Exit;
	End;

	Self.service_type := serviceType;

	controlURL_node := service_node.GetNodeByName('controlURL');
	control_url := controlURL_node.Value;

	//make the complete control_url;
	if(Pos('http://', control_url)<=0) And (Pos('HTTP://', control_url)<=0) Then
		control_url := base_url + control_url;
	If (Pos('http://', describe_url)<=0) And (Pos('HTTP://', describe_url)<=0) Then
		describe_url := base_url + describe_url;

	closesocket(tcp_socket_fd);
	status := NAT_GETCONTROL;
	Result := True;
End;

Function UPNP.add_port_mapping(_description, _destination_ip:AnsiString; _port_ex, _port_in:Word; _protocol:AnsiString):Boolean;
Var
	ret:Boolean;
	host,path:AnsiString;
	port:Word;
	action_params, soap_message, action_message:AnsiString;
  response, http_request:AnsiString;
  buff:Array[0..MAX_BUFF_SIZE+1] Of AnsiChar;
Begin
	ret := parseUrl(control_url, host, port, path);
	if (Not ret) Then
	Begin
		status := NAT_ERROR;
		last_error := 'Fail to parseURl: '+describe_url;
		Result := False;
    Exit;
	End;

	//connect
	ret := tcp_connect(host, port);
	if (Not ret) Then
  Begin
		Result := false;
    Exit;
  End;

  action_params := ADD_PORT_MAPPING_PARAMS;
  StringReplaceText('$A', IntToString(_port_ex), action_params);
  StringReplaceText('$B', _protocol, action_params);
  StringReplaceText('$C', IntToString(_port_in), action_params);
  StringReplaceText('$D', _destination_ip, action_params);
  StringReplaceText('$E', _description, action_params);

	soap_message := SOAP_ACTION;
  StringReplaceText('$A', ACTION_ADD, soap_message);
  StringReplaceText('$B', service_type, soap_message);
  StringReplaceText('$C', action_params, soap_message);
  StringReplaceText('$D', ACTION_ADD, soap_message);

	action_message := HTTP_HEADER_ACTION;
  StringReplaceText('$A', path, action_message);
  StringReplaceText('$B', host, action_message);
  StringReplaceText('$C', IntToString(port), action_message);
  StringReplaceText('$D', service_type, action_message);
  StringReplaceText('$E', ACTION_ADD, action_message);
  StringReplaceText('$F', IntToString(Length(soap_message)), action_message);

	http_request := action_message + soap_message;

	//send request
	ret := send(tcp_socket_fd, http_request[1] , Length(http_request),0)>0;

	//wait for response
	FillChar(buff, sizeof(buff), 0);
  Ret := recv(tcp_socket_fd, buff, MAX_BUFF_SIZE,0) >0;
	while (Ret) Do
	Begin
		response := response + buff;
	  FillChar(buff, sizeof(buff), 0);
    Ret := recv(tcp_socket_fd, buff, MAX_BUFF_SIZE,0) >0;
	End;

	if (Pos(HTTP_OK, response)<=0) Then
	Begin
		status := NAT_ERROR;
		last_error := 'Fail to add port mapping ('+_description+'/'+_protocol+')';
		Result := False;
    Exit;
	End;

	closesocket(tcp_socket_fd);
	Status := NAT_ADD;
	Result := True;
End;


End.
