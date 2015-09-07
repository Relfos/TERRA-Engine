Unit TERRA_Ethernet;

Interface
Uses TERRA_Object, TERRA_String, TERRA_Utils;

Const
  MAX_INTERFACE_NAME_LEN = $100;
  ERROR_SUCCESS = 0;
  MAXLEN_IFDESCR = $100;
  MAXLEN_PHYSADDR = 8;

  MIB_IF_OPER_STATUS_NON_OPERATIONAL = 0 ;
  MIB_IF_OPER_STATUS_UNREACHABLE = 1;
  MIB_IF_OPER_STATUS_DISCONNECTED = 2;
  MIB_IF_OPER_STATUS_CONNECTING = 3;
  MIB_IF_OPER_STATUS_CONNECTED = 4;
  MIB_IF_OPER_STATUS_OPERATIONAL = 5;

  MIB_IF_TYPE_OTHER = 1;
  MIB_IF_TYPE_ETHERNET = 6;
  MIB_IF_TYPE_TOKENRING = 9;
  MIB_IF_TYPE_FDDI = 15;
  MIB_IF_TYPE_PPP = 23;
  MIB_IF_TYPE_LOOPBACK = 24;
  MIB_IF_TYPE_SLIP = 28;

  MIB_IF_ADMIN_STATUS_UP = 1;
  MIB_IF_ADMIN_STATUS_DOWN = 2;
  MIB_IF_ADMIN_STATUS_TESTING = 3;

Type
  MIB_IFROW = Packed Record
    wszName : Array[0 .. (MAX_INTERFACE_NAME_LEN*2-1)] of AnsiChar;
    dwIndex : LongInt;
    dwType : LongInt;
    dwMtu : LongInt;
    dwSpeed : LongInt;
    dwPhysAddrLen : LongInt;
    bPhysAddr : Array[0 .. (MAXLEN_PHYSADDR-1)] of Byte;
    dwAdminStatus : LongInt;
    dwOperStatus : LongInt;
    dwLastChange : LongInt;
    dwInOctets : LongInt;
    dwInUcastPkts : LongInt;
    dwInNUcastPkts : LongInt;
    dwInDiscards : LongInt;
    dwInErrors : LongInt;
    dwInUnknownProtos : LongInt;
    dwOutOctets : LongInt;
    dwOutUcastPkts : LongInt;
    dwOutNUcastPkts : LongInt;
    dwOutDiscards : LongInt;
    dwOutErrors : LongInt;
    dwOutQLen : LongInt;
    dwDescrLen : LongInt;
    bDescr : Array[0 .. (MAXLEN_IFDESCR - 1)] of AnsiChar;
  End;

Function GetMACAdress():TERRAString;


implementation

Function GetIfTable(pIfTable:Pointer; Var pdwSize:LongInt; bOrder:LongInt): LongInt; Stdcall; External 'IPHLPAPI.DLL';

Const
  _MAX_ROWS_ = 2000;

Type
   MIB_ROWARRAY = Array[0..Pred(_MAX_ROWS_)] of MIB_IFROW;

  _IfTable = Record
    nRows:LongInt;
    ifRow:MIB_ROWARRAY;
  End;

Function GetMACAdress():TERRAString;
Var
  pIfTable:^_IfTable;
  TableSize : LongInt;
  tmp:TERRAString;
  i,j :Integer;
  ErrCode:LongInt;
Begin
  Result := '';
  pIfTable := Nil;

  // First: just get the buffer size.
  // TableSize returns the size needed.
  TableSize := 0; // Set to zero so the GetIfTabel function

  // won't try to fill the buffer yet,
  // but only return the actual size it needs.
  GetIfTable(pIfTable, TableSize, 1);

  If (TableSize < SizeOf(MIB_IFROW)+Sizeof(LongInt)) Then
    Exit; // less than 1 table entry?!

  // allocate memory for the buffer and retrieve the entire table.
  GetMem(pIfTable, TableSize);
  ErrCode := GetIfTable(pIfTable, TableSize, 1);
  If ErrCode<>ERROR_SUCCESS then
    Exit; // OK, that did not work.

  // Read the ETHERNET addresses.
  For I:=0 to Pred(pIfTable^.nRows) Do
  If (pIfTable^.ifRow[i].dwType = MIB_IF_TYPE_ETHERNET) Then
  Begin
    tmp := '';
    For j:=0 To Pred(pIfTable^.ifRow[i].dwPhysAddrLen) Do
    Begin
      If Tmp<>'' Then
        Tmp := Tmp + '-';
      Tmp := Tmp + HexStr(pIfTable^.ifRow[i].bPhysAddr[j]);
    End;

    If Tmp<>'' Then
    Begin
      Result := Tmp;
      Break;
    End;
  End;

  If Assigned(pIfTable) Then
    FreeMem(pIfTable,TableSize);
End;

End.
