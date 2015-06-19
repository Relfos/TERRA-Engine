unit VMTInfo;

interface

uses Classes, TypInfo;
type 
  // The type info interfaces apply to any type for which the 
  // compiler stores runtime type information (RTTI). 
  ITypeInfo = interface 
  ['{6CF8D121-16D3-11D3-B1A8-00105AA9C2AD}'] 
    function GetKind: TTypeKind; 
    function GetName:TERRAString; 
    function GetTypeInfo: PTypeInfo; 
    function GetTypeData: PTypeData; 
 
    property Kind: TTypeKind read GetKind; 
    property Name:TERRAString read GetName; 
    property TypeInfo: PTypeInfo read GetTypeInfo; 
    property TypeData: PTypeData read GetTypeData; 
  end; 
  IOrdinalTypeInfo = interface(ITypeInfo) 
  ['{6CF8D122-16D3-11D3-B1A8-00105AA9C2AD}'] 
    function GetOrdType: TOrdType; 
    property OrdType: TOrdType read GetOrdType; 
  end; 
  ICharTypeInfo = interface(IOrdinalTypeInfo) 
  ['{A89454C1-1AA9-11D3-B1A8-00105AA9C2AD}'] 
    function GetMinValue: Char; 
    function GetMaxValue: Char; 
 
    property MinValue: Char read GetMinValue; 
    property MaxValue: Char read GetMaxValue; 
  end; 
  IWideCharTypeInfo = interface(IOrdinalTypeInfo) 
  ['{A89454C2-1AA9-11D3-B1A8-00105AA9C2AD}'] 
    function GetMinValue: WideChar; 
    function GetMaxValue: WideChar; 
 
    property MinValue: WideChar read GetMinValue; 
    property MaxValue: WideChar read GetMaxValue; 
  end; 
  IRangeTypeInfo = interface(IOrdinalTypeInfo) 
  ['{6CF8D123-16D3-11D3-B1A8-00105AA9C2AD}'] 
    function GetMinValue: Integer; 
    function GetMaxValue: Integer; 
 
    property MinValue: Integer read GetMinValue; 
    property MaxValue: Integer read GetMaxValue; 
  end; 
  IIntegerTypeInfo = interface(IRangeTypeInfo) 
  ['{A89454C3-1AA9-11D3-B1A8-00105AA9C2AD}'] 
  end; 
  IEnumTypeInfo = interface(IRangeTypeInfo) 
  ['{6CF8D124-16D3-11D3-B1A8-00105AA9C2AD}'] 
    function GetBaseType: IEnumTypeInfo; 
    function GetNames(Index: Integer):TERRAString; 
    function GetNameCount: Integer; 
 
    function IsSubRange: Boolean; 
 
    property NameCount: Integer read GetNameCount; 
    property Names[Index: Integer]:TERRAString read GetNames; 
    property BaseType: IEnumTypeInfo read GetBaseType; 
  end; 
  ISetTypeInfo = interface(IOrdinalTypeInfo) 
  ['{6CF8D125-16D3-11D3-B1A8-00105AA9C2AD}'] 
    function GetCompType: IEnumTypeInfo; 
    property CompType: IEnumTypeInfo read GetCompType; 
  end; 
  IInt64TypeInfo = interface(ITypeInfo) 
  ['{6CF8D126-16D3-11D3-B1A8-00105AA9C2AD}'] 
    function GetMinValue: Int64; 
    function GetMaxValue: Int64; 
    property MinValue: Int64 read GetMinValue; 
    property MaxValue: Int64 read GetMaxValue; 
  end; 
  IFloatTypeInfo = interface(ITypeInfo) 
  ['{6CF8D127-16D3-11D3-B1A8-00105AA9C2AD}'] 
    function GetFloatType: TFloatType; 
    property FloatType: TFloatType read GetFloatType; 
  end; 
  IShortStringTypeInfo = interface(ITypeInfo) 
  ['{6CF8D128-16D3-11D3-B1A8-00105AA9C2AD}'] 
    function GetMaxLength: Integer; 
    property MaxLength: Integer read GetMaxLength; 
  end; 
  TStoredType = (ptStaticMethod, ptVirtualMethod, ptField, ptConstant); 
  TProcType = ptStaticMethod..ptField; 
  IPropInfo = interface 
  ['{6CF8D12D-16D3-11D3-B1A8-00105AA9C2AD}'] 
    function GetPropType: ITypeInfo; 
    function GetReader: Pointer; 
    function GetWriter: Pointer; 
    function GetStoredProc: Pointer; 
    function GetIndex: Integer; 
    function GetDefault: LongInt; 
    function GetNameIndex: SmallInt; 
    function GetName:TERRAString; 
    function GetReaderType: TProcType; 
    function GetWriterType: TProcType; 
    function GetStoredType: TStoredType; 
    function GetReaderValue: Integer; 
    function GetWriterValue: Integer; 
    function GetStoredValue: Integer; 
 
    function HasDefault: Boolean; 
    function HasIndex: Boolean; 
 
    property PropType: ITypeInfo read GetPropType; 
    property Reader: Pointer read GetReader; 
    property Writer: Pointer read GetWriter; 
    property StoredProc: Pointer read GetStoredProc; 
    property ReaderType: TProcType read GetReaderType; 
    property WriterType: TProcType read GetWriterType; 
    property StoredType: TStoredType read GetStoredType; 
    // Values are field offsets, virtual method numbers, or constant ordinals 
    property ReaderValue: Integer read GetReaderValue; 
    property WriterValue: Integer read GetWriterValue; 
    property StoredValue: Integer read GetStoredValue; 
    property Index: Integer read GetIndex; 
    property Default: LongInt read GetDefault; 
    property NameIndex: SmallInt read GetNameIndex; 
    property Name:TERRAString read GetName; 
  end; 
  IClassTypeInfo = interface(ITypeInfo) 
  ['{6CF8D129-16D3-11D3-B1A8-00105AA9C2AD}'] 
    function GetClassType: TClass; 
    function GetParentInfo: IClassTypeInfo; 
    function GetPropCount: Integer; 
    function GetUnitName:TERRAString; 
    function GetProperties(Index: Integer): IPropInfo; 
 
    property ClassType: TClass read GetClassType; 
    property ParentInfo: IClassTypeInfo read GetParentInfo; 
    property PropCount: Integer read GetPropCount; 
    property UnitName:TERRAString read GetUnitName; 
    property Properties[Index: Integer]: IPropInfo read GetProperties; 
  end; 
  IParamInfo = interface 
  ['{6CF8D12B-16D3-11D3-B1A8-00105AA9C2AD}'] 
    function GetFlags: TParamFlags; 
    function GetName:TERRAString; 
    function GetTypeName:TERRAString; 
 
    property Flags: TParamFlags read GetFlags; 
    property Name:TERRAString read GetName; 
    property TypeName:TERRAString read GetTypeName; 
  end; 
  IMethodTypeInfo = interface(ITypeInfo) 
  ['{6CF8D12A-16D3-11D3-B1A8-00105AA9C2AD}'] 
    function GetMethodKind: TMethodKind; 
    function GetParamCount: Byte; 
    function GetParams(Index: Integer): IParamInfo; 
    function GetResultType:TERRAString; 
 
    property MethodKind: TMethodKind read GetMethodKind; 
    property ParamCount: Byte read GetParamCount; 
    property Params[Index: Integer]: IParamInfo read GetParams; 
    property ResultType:TERRAString read GetResultType; 
  end; 
  IInterfaceTypeInfo = interface(ITypeInfo) 
  ['{6CF8D12C-16D3-11D3-B1A8-00105AA9C2AD}'] 
    function GetIntfParent: IInterfaceTypeInfo; 
    function GetIntfFlags: TIntfFlagsBase; 
    function GetGuid: TGUID; 
    function GetIntfUnit:TERRAString; 
 
    property IntfParent: IInterfaceTypeInfo read GetIntfParent; 
    property IntfFlags: TIntfFlagsBase read GetIntfFlags; 
    property Guid: TGUID read GetGuid; 
    property IntfUnit:TERRAString read GetIntfUnit; 
  end; 
 
 
  // The VMT interfaces apply only to classes with RTTI, that is, 
  // classes compiled with the $M+ directive or derived from classes 
  // that were compiled with $M+ (such as TPersistent). 
  IVmtInitEntry = interface 
  ['{4C479B91-113D-11D3-B1A6-00105AA9C2AD}'] 
    function GetOffset: Integer;            // byte offset of field in object 
    function GetTypeName:TERRAString;           // name of the field's type 
    function GetTypeKind: TTypeKind;        // kind of the field's type 
 
    property Offset: Integer read GetOffset; 
    property TypeName:TERRAString read GetTypeName; 
    property TypeKind: TTypeKind read GetTypeKind; 
  end; 
  IVmtInitAggregate = interface(IVmtInitEntry) 
  ['{4C479B92-113D-11D3-B1A6-00105AA9C2AD}'] 
    function GetCount: Integer;             // number of sub-items 
    function GetInitializer(Index: Integer): IVmtInitEntry; 
 
    property Count: Integer read GetCount; 
    property Initializer[Index: Integer]: IVmtInitEntry read GetInitializer; default; 
  end; 
  IVmtInitRecord = interface(IVmtInitAggregate) 
  ['{4C479B93-113D-11D3-B1A6-00105AA9C2AD}'] 
    function GetRecordSize: Integer; 
    property RecordSize: Integer read GetRecordSize; 
  end; 
  IVmtInitArray = interface(IVmtInitAggregate) 
  ['{4C479B94-113D-11D3-B1A6-00105AA9C2AD}'] 
    function GetElementSize: Integer; 
    property ElementSize: Integer read GetElementSize; 
  end; 
 
  { Automation entry flags } 
  TVmtAutoFlag = (afMethod, afPropGet, afPropSet, afVirtual); 
  TVmtAutoFlags = set of TVmtAutoFlag; 
  TVmtAutoType = type Byte; // a Variant type code, e.g., varInteger 
 
  IVmtAutoParam = interface 
  ['{E8476685-1615-11D3-B1A8-00105AA9C2AD}'] 
    function GetAutoType: TVmtAutoType; 
    function GetIsByRef: Boolean; // true if passed by reference 
 
    property AutoType: TVmtAutoType read GetAutoType; 
    property IsByRef: Boolean read GetIsByRef; 
  end; 
 
  IVmtAutoEntry = interface 
  ['{E8476686-1615-11D3-B1A8-00105AA9C2AD}'] 
    function GetDispID: Integer; 
    function GetName:TERRAString; 
    function GetFlags: TVmtAutoFlags; 
    function GetAddress: Pointer; 
    function GetParamCount: Integer; 
    function GetParam(Index: Integer): IVmtAutoParam; 
    function GetResultType: TVmtAutoType; 
 
    property Address: Pointer read GetAddress; 
    property Count: Integer read GetParamCount; 
    property DispID: Integer read GetDispID; 
    property Flags: TVmtAutoFlags read GetFlags; 
    property Name:TERRAString read GetName; 
    property Params[Index: Integer]: IVmtAutoParam read GetParam; default; 
    property ResultType: TVmtAutoType read GetResultType; 
  end; 
 
  IVmtMethodParam = interface 
  ['{30099302-17CF-11D3-B1A8-00105AA9C2AD}'] 
    function GetName:TERRAString; 
    function GetType: ITypeInfo; 
    function GetTypeInfo: PTypeInfo; 
    property Name:TERRAString read GetName; 
    property ParamType: ITypeInfo read GetType; 
    property TypeInfo: PTypeInfo read GetTypeInfo; 
  end; 
 
  IVmtMethod = interface 
  ['{30099301-17CF-11D3-B1A8-00105AA9C2AD}'] 
    function GetAddress: Pointer; 
    function GetName:TERRAString; 
    function GetParamCount: Integer; 
    function GetParams(Index: Integer): IVmtMethodParam; 
    function HasParamInfo: Boolean; 
 
    property Address: Pointer read GetAddress; 
    property Name:TERRAString read GetName; 
    property ParamCount: Integer read GetParamCount; 
    property Params[Index: Integer]: IVmtMethodParam read GetParams; 
  end; 
 
  IVmtField = interface 
  ['{30099303-17CF-11D3-B1A8-00105AA9C2AD}'] 
    function GetOffset: Integer; 
    function GetClass: TClass; 
    function GetClassIndex: Word; 
    function GetName:TERRAString; 
    function GetType: IClassTypeInfo; 
 
    property Offset: Integer read GetOffset; 
    property ClassIndex: Word read GetClassIndex; 
    property FieldClass: TClass read GetClass; 
    property Name:TERRAString read GetName; 
    property FieldType: IClassTypeInfo read GetType; 
  end; 
 
  IVmtInfo = interface
  ['{102C2541-160F-11D3-B1A8-00105AA9C2AD}']
    function GetFieldClassTable: Pointer; 
    function GetClass(Index: Integer): TClass; 
    function GetClassCount: Integer; 
    function GetClassName:TERRAString; 
    function GetClassType: TClass; 
    function GetDynamicTable: Pointer; 
    function GetDynamicAddress(Index: Integer): Pointer; 
    function GetDynamicNumber(Index: Integer): SmallInt; 
    function FindDynamicAddress(Number: SmallInt): POinter; 
    function GetDynamicMethodCount: Integer; 
    function GetFieldTable: Pointer; 
    function GetField(Index: Integer): IVmtField; 
    function GetFieldType(Index: Integer): TClass; 
    function GetFieldCount: Integer; 
    function GetMethodTable: Pointer; 
    function GetMethod(Index: Integer): IVmtMethod; 
    function GetMethodCount: Integer; 
    function GetParentVmt: IVmtInfo; 
    function GetInitTable: Pointer; 
    function GetInitCount: Integer; 
    function GetInitEntry(Index: Integer): IVmtInitEntry; 
    function GetAutoTable: Pointer; 
    function GetTypeInfo: PTypeInfo; 
    function GetIntfTable: Pointer; 
    function GetInterfaceCount: Integer;
    function GetInterfaceEntry(Index: Integer): TInterfaceEntry; 
    function GetAutoCount: Integer; 
    function GetAutoEntry(Index: Integer): IVmtAutoEntry; 
    function GetVmt: Pointer; 
 
    function GetInstanceSize:       Integer; 
    function GetSafeCallException:  Pointer; 
    function GetAfterConstruction:  Pointer; 
    function GetBeforeDestruction:  Pointer; 
    function GetDispatch:           Pointer; 
    function GetDefaultHandler:     Pointer; 
    function GetNewInstance:        Pointer; 
    function GetFreeInstance:       Pointer; 
    function GetDestroy:            Pointer; 
 
    property Vmt: Pointer read GetVmt; 
 
    property ParentVmt: IVmtInfo read GetParentVmt; 
    property ClassName:TERRAString read GetClassName; 
 
    property MethodTable: Pointer read GetMethodTable; 
    property MethodCount: Integer read GetMethodCount; 
    property Methods[Index: Integer]: IVmtMethod read GetMethod; 
 
    property FieldTable: Pointer read GetFieldTable; 
    property FieldCount: Integer read GetFieldCount;
    property Fields[Index: Integer]: IVmtField read GetField; 
    property FieldType[Index: Integer]: TClass read GetFieldType; 
 
    property FieldClassTable: Pointer read GetFieldClassTable; 
    property ClassCount: Integer read GetClassCount; 
    property Classes[Index: Integer]: TClass read GetClass; 
 
    property DynamicTable: Pointer read GetDynamicTable; 
    property DynamicMethodCount: Integer read GetDynamicMethodCount; 
    property DynamicMethods[Index: Integer]: Pointer read GetDynamicAddress; 
    property MessageNumbers[Index: Integer]: SmallInt read GetDynamicNumber; 
    property Messages[Index: SmallInt]: Pointer read FindDynamicAddress; 
 
    property InitTable: Pointer read GetInitTable; 
    property InitializerCount: Integer read GetInitCount; 
    property Initializer[Index: Integer]: IVmtInitEntry read GetInitEntry; 
 
    property AutoTable: Pointer read GetAutoTable; 
    property AutoCount: Integer read GetAutoCount; 
    property AutoEntry[Index: Integer]: IVmtAutoEntry read GetAutoEntry; 
 
    property IntfTable: Pointer read GetIntfTable; 
    property InterfaceCount: Integer read GetInterfaceCount; 
    property Interfaces[Index: Integer]: TInterfaceEntry read GetInterfaceEntry; 
 
    property TypeInfo: PTypeInfo read GetTypeInfo;
  end;

// The following methods create interfaces for class references,
// object references, or type information.
function GetVmtInfo(ClassRef: TClass): IVmtInfo; 
function GetTypeInfo(TypeInfo: PTypeInfo): ITypeInfo;

implementation

uses Windows, ComObj, SysUtils, Consts;

// Any out-of-bounds error for an array property calls IndexError.
procedure IndexError(Index: Integer);
begin
  WriteLn('IndexError:' ,Index);
  ReadLn;
  Halt;
end; 
 
// Convert a character value to a printable string. 
function CharToStr(Ch: Char):TERRAString; overload; 
begin 
  if Ch in [' ' .. '~'] then 
    Result := '''' + Ch + '''' 
  else 
    Result := Format('#%d', [Ord(Ch)]); 
end; 
 
function CharToStr(Ch: WideChar):TERRAString; overload; 
begin 
  if Ch in [WideChar(' ') .. WideChar('~')] then 
    Result := '''' + Char(Ch) + '''' 
  else 
    Result := Format('#%d', [Ord(Ch)]); 
end; 
 
// Convert a Variant type code to a type name. 
function VarTypeToString(VarType: Integer):TERRAString; 
begin 
  case VarType of 
  varStrArg: 
    Result := 'string'; 
  varSmallint: 
    Result := 'SmallInt'; 
  varInteger: 
    Result := 'Integer'; 
  varSingle: 
    Result := 'Single'; 
  varDouble: 
    Result := 'Double'; 
  varCurrency: 
    Result := 'Currency'; 
  varDate: 
    Result := 'TDateTime'; 
  varOleStr: 
    Result := 'WideString'; 
  varDispatch: 
    Result := 'IDispatch'; 
  varBoolean: 
    Result := 'Boolean'; 
  varVariant: 
    Result := 'Variant'; 
  varUnknown: 
    Result := 'IUnknown'; 
  else 
    Result := 'unknown'; 
  end; 
end; 
 
 
type 
  { Published method record } 
  PMethod = ^TMethod; 
  TMethod = packed record 
    Size: Word;          { Size of the TVmtMethod record. } 
    Address: Pointer;    { Pointer to the method entry point. } 
    Name: ShortString;   { Name of the published method. } 
    // Some methods have an additional 4 zero bytes, which means the method 
    // takes no parameters. 
    // Some methods have an additional 6 bytes, followed by a series of 
    // TMethodParam records, for each parameter. 
    // It seems that only stdcall methods have this extra information. 
    // You can identify the extra info by the TMethod.Size value being 
    // too big for just the Size, Address, and Name members. 
  end; 
  PMethodParam = ^TMethodParam; 
  TMethodParam = packed record 
    TypeInfo: PPTypeInfo; 
    Name: ShortString; 
    // The name is followed by a trailing #0 byte. 
  end; 
 
  { Published method table } 
  PMethodTable = ^TMethodTable; 
  TMethodTable = packed record 
    Count: Word; 
    Methods: array[0..MaxListSize] of Byte; 
    { Methods: array[1..Count] of TVmtMethod; } 
  end; 
 
  { Field class table } 
  PFieldClassTable = ^TFieldClassTable; 
  TFieldClassTable = packed record 
    Count: Word; 
    Classes: packed array[0..MaxListSize] of ^TClass; 
    { Classes: packed array[1..Count] of ^TClass; } 
  end; 
 
  { Published field record } 
  PField = ^TField; 
  TField = packed record 
    Offset: Integer;     { Byte offset of field in the object. } 
    ClassIndex: Word;    { Index in the FieldClassTable of the field's type. } 
    Name: ShortString;   { Name of the published field. } 
  end; 
 
  { Published field table } 
  PFieldTable = ^TFieldTable; 
  TFieldTable = packed record 
    Count: Word; 
    FieldClassTable: PFieldClassTable; 
    Fields: packed array[0..MaxListSize] of Byte; 
    { Fields: packed array [1..Count] of TVmtField; } 
  end; 
 
  { Dynamic method table } 
  PDynMethodTable = ^TDynMethodTable; 
  TDynMethodTable = packed record 
    Count: Word; 
    Data: packed array[0..MaxListSize] of Byte; 
    { Indexes: packed array[1..Count] of SmallInt; 
      Addresses: packed array[1..Count] of Pointer; } 
  end; 
  PDynIndexes = ^TDynIndexes; 
  TDynIndexes = packed array[0..MaxListSize] of SmallInt; 
  PDynAddresses = ^TDynAddresses; 
  TDynAddresses = packed array[0..MaxListSize] of Pointer; 
 
  { Initialization/finalization record } 
  PTypeKind = ^TTypeKind; 
  PInitTable = ^TInitTable; 
  PInitRecord = ^TInitRecord; 
  TInitRecord = packed record 
    InitTable: ^PInitTable; 
    Offset: Integer;   { offset of field in object } 
  end; 
  PInitArray = ^TInitArray; 
  TInitArray = array[0..MaxListSize] of TInitRecord; 
 
  { Initialization/finalization table } 
  TInitTable = packed record 
    TypeKind: Byte; 
    Data: packed array[0..MaxListSize] of Byte; 
    { TypeName: ShortString; 
      DataSize: Integer; 
      Count: Integer; 
      Records: array[1..Count] of TInitRecord; } 
  end; 
 
  { OLE Automation Table (see OleAuto.pas) } 
const 
  { Parameter type masks } 
  atTypeMask = $7F; 
  atByRef    = $80; 
  MaxAutoEntries = 4095; 
  MaxAutoParams = 255; 
 
type 
  { Automation entry parameter list } 
  PAutoParamList = ^TAutoParamList; 
  TAutoParamList = packed record 
    ReturnType: TVmtAutoType; 
    Count: Byte; 
    Types: array[0..MaxAutoParams] of TVmtAutoType; 
  end; 
  { Automation entry flags } 
  { Automation table entry } 
  PAutoEntry = ^TAutoEntry; 
  TAutoEntry = packed record 
    DispID: Integer; 
    Name: PShortString; 
    Flags: Integer; { Lower byte is TVmtAutoFlags } 
    Params: PAutoParamList; 
    Address: Pointer; 
  end; 
 
  { Automation table layout } 
  PAutoTable = ^TAutoTable; 
  TAutoTable = packed record 
    Count: Integer; 
    Entries: array[0..MaxAutoEntries] of TAutoEntry; 
  end; 
 
  { Virtual Method Table } 
  PVmt = ^TVmt; 
  TVmt = record 
    SelfPtr:            TClass; 
    IntfTable:          PInterfaceTable; 
    AutoTable:          PAutoTable; 
    InitTable:          PInitTable; 
    TypeInfo:           PTypeInfo; 
    FieldTable:         PFieldTable; 
    MethodTable:        PMethodTable; 
    DynMethodTable:     PDynMethodTable; 
    ClassName:          PShortString; 
    InstanceSize:       Integer; 
    ClassParent:        ^TClass; 
    SafeCallException:  Pointer; { The following hidden fields point to } 
    AfterConstruction:  Pointer; { special virtual methods that are } 
    BeforeDestruction:  Pointer; { inherited from TObject. } 
    Dispatch:           Pointer; 
    DefaultHandler:     Pointer; 
    NewInstance:        Pointer; 
    FreeInstance:       Pointer; 
    Destroy:            Pointer; 
    { Here begin the virtual method pointers. 
      Each virtual method is stored as a code pointer, e.g., 
        VirtualMethodTable: array[0..Count] of Pointer; 
      The compiler does not store the Count of the number of method 
      pointers in the table. } 
  end; 
 
  // The classes below implement the VmtInfo interfaces. 
  TVmtAutoParam = class(TInterfacedObject, IVmtAutoParam)
  private
    fParam: Byte;
  public
    constructor Create(Param: TVmtAutoType);
    function GetAutoType: TVmtAutoType;
    function GetIsByRef: Boolean; // true if passed by reference
  end;

  TVmtAutoEntry = class(TInterfacedObject, IVmtAutoEntry)
  private
    fEntry: PAutoEntry;
    property Entry: PAutoEntry read fEntry;
  public
    constructor Create(Entry: PAutoEntry);
    function GetDispID: Integer;
    function GetName:TERRAString;
    function GetFlags: TVmtAutoFlags;
    function GetAddress: Pointer;
    function GetParamCount: Integer;
    function GetParam(Index: Integer): IVmtAutoParam;
    function GetResultType: TVmtAutoType;
  end;

  TVmtInitEntry = class(TInterfacedObject, IVmtInitEntry)
  private
    fTable: PInitTable;
    fOffset: Integer;
    property Table: PInitTable read fTable;
  public
    constructor Create(Table: PInitTable; Offset: Integer = 0);
    function GetOffset: Integer;            // byte offset of field in object 
    function GetTypeName:TERRAString;           // name of the field's type 
    function GetTypeKind: TTypeKind;        // kind of the field's type 

    property Offset: Integer read GetOffset; 
    property TypeName:TERRAString read GetTypeName; 
    property TypeKind: TTypeKind read GetTypeKind; 
  end; 
 
  TVmtInitAggregate = class(TVmtInitEntry, IVmtInitAggregate) 
    function GetInitArray: PInitArray; 
  protected 
    property InitArray: PInitArray read GetInitArray; 
    function GetDataSize: Integer; 
  public 
    function GetCount: Integer;             // number of sub-items 
    function GetInitializer(Index: Integer): IVmtInitEntry; 
 
    property Count: Integer read GetCount; 
    property Initializer[Index: Integer]: IVmtInitEntry read GetInitializer; 
  end; 
 
  TVmtInitRecord = class(TVmtInitAggregate, IVmtInitRecord) 
  public 
    function GetRecordSize: Integer; 
    property RecordSize: Integer read GetRecordSize; 
  end; 
 
  TVmtInitArray = class(TVmtInitAggregate, IVmtInitArray) 
  public 
    function GetElementSize: Integer; 
    property ElementSize: Integer read GetElementSize; 
  end; 
 
  TVmtMethod = class(TInterfacedObject, IVmtMethod) 
  private 
    fMethod: PMethod; 
    fParamCount: Integer; 
    procedure CountParams; 
  protected 
    function GetBaseSize: Integer; // size of Size, Address, and Name 
    function HasParamInfo: Boolean;   // True if the method has parameter info 
    function GetParamData(Index: Integer): PMethodParam; 
    property Method: PMethod read fMethod; 
  public 
    constructor Create(Method: PMethod); 
    function GetAddress: Pointer; 
    function GetName:TERRAString; 
    function GetParamCount: Integer; 
    function GetParams(Index: Integer): IVmtMethodParam; 
 
    property Address: Pointer read GetAddress; 
    property Name:TERRAString read GetName; 
    property ParamCount: Integer read GetParamCount; 
    property Params[Index: Integer]: IVmtMethodParam read GetParams; 
  end; 
 
  TVmtField = class(TInterfacedObject, IVmtField) 
  private 
    fField: PField; 
    fClass: TClass; 
  protected 
    property Field: PField read fField; 
  public 
    constructor Create(Field: PField; FieldClass: TClass); 
    function GetOffset: Integer; 
    function GetClassIndex: Word; 
    function GetName:TERRAString; 
    function GetClass: TClass; 
    function GetType: IClassTypeInfo; 
 
    property Offset: Integer read GetOffset; 
    property ClassIndex: Word read GetClassIndex; 
    property Name:TERRAString read GetName; 
    property FieldClass: TClass read GetClass; 
    property FieldType: IClassTypeInfo read GetType; 
  end; 
 
  TVmtInfo = class(TInterfacedObject, IVmtInfo) 
  private 
    fVmt: PVmt; 
    fInitRecord: IVmtInitRecord; 
    function GetInitRecord: IVmtInitRecord; 
  protected 
    property Vmt: PVmt read fVmt; 
    property InitRecord: IVmtInitRecord read GetInitRecord; 
  public 
    constructor Create(ClassRef: TClass); 
 
    function GetInstanceSize:       Integer; 
    function GetSafeCallException:  Pointer; 
    function GetAfterConstruction:  Pointer; 
    function GetBeforeDestruction:  Pointer; 
    function GetDispatch:           Pointer; 
    function GetDefaultHandler:     Pointer; 
    function GetNewInstance:        Pointer; 
    function GetFreeInstance:       Pointer; 
    function GetDestroy:            Pointer; 
 
    function GetFieldClassTable: Pointer; 
    function GetClass(Index: Integer): TClass; 
    function GetClassCount: Integer; 
    function GetClassName:TERRAString; 
    function GetClassType: TClass; 
    function GetDynamicTable: Pointer; 
    function GetDynamicAddress(Index: Integer): Pointer; 
    function GetDynamicNumber(Index: Integer): SmallInt; 
    function FindDynamicAddress(Number: SmallInt): Pointer; 
    function GetDynamicMethodCount: Integer; 
    function GetFieldTable: Pointer; 
    function GetField(Index: Integer): IVmtField; 
    function GetFieldType(Index: Integer): TClass; 
    function GetFieldCount: Integer; 
    function GetMethodTable: Pointer; 
    function GetMethod(Index: Integer): IVmtMethod; 
    function GetMethodCount: Integer; 
    function GetParentVmt: IVmtInfo; 
    function GetInitTable: Pointer; 
    function GetInitCount: Integer; 
    function GetInitEntry(Index: Integer): IVmtInitEntry; 
    function GetAutoTable: Pointer; 
    function GetAutoCount: Integer; 
    function GetAutoEntry(Index: Integer): IVmtAutoEntry; 
    function GetTypeInfo: PTypeInfo; 
    function GetIntfTable: Pointer; 
    function GetInterfaceCount: Integer; 
    function GetInterfaceEntry(Index: Integer): TInterfaceEntry; 
    function GetVmt: Pointer; 
  end; 
 
   
  TVmtTypeInfo = class(TInterfacedObject, ITypeInfo)
  private 
    fTypeInfo: PTypeInfo; 
    fTypeData: PTypeData; 
  public 
    constructor Create(TypeInfo: PTypeInfo); virtual; 
    class function CreateTypeInfo(TypeInfo: PTypeInfo): TVmtTypeInfo; 
    function GetKind: TTypeKind; 
    function GetName:TERRAString; 
    function GetTypeInfo: PTypeInfo; 
    function GetTypeData: PTypeData; 

    property TypeInfo: PTypeInfo read fTypeInfo; 
    property TypeData: PTypeData read GetTypeData; 
  end; 
  TVmtOrdinalTypeInfo = class(TVmtTypeInfo, IOrdinalTypeInfo) 
  public 
    function GetOrdType: TOrdType; 
  end; 
  TVmtRangeTypeInfo = class(TVmtOrdinalTypeInfo, IRangeTypeInfo) 
  public 
    function GetMinValue: Integer; 
    function GetMaxValue: Integer; 
  end; 
  TVmtIntegerTypeInfo = class(TVmtRangeTypeInfo, IIntegerTypeInfo) 
  public 
  end;

  TVmtCharTypeInfo = class(TVmtRangeTypeInfo, ICharTypeInfo)
  public 
    function GetMinValue: Char; 
    function GetMaxValue: Char; 
  end;

  TVmtWideCharTypeInfo = class(TVmtRangeTypeInfo, IWideCharTypeInfo)
  public 
    function GetMinValue: WideChar; 
    function GetMaxValue: WideChar; 
  end;

  TVmtEnumTypeInfo = class(TVmtRangeTypeInfo, IEnumTypeInfo) 
  private 
    fNames: TStrings; 
    property Names: TStrings read fNames; 
  public 
    constructor Create(TypeInfo: PTypeInfo); override; 
    Procedure Release; override; 
    function GetBaseType: IEnumTypeInfo; 
    function GetNames(Index: Integer):TERRAString; 
    function GetNameCount: Integer; 
    function IsSubRange: Boolean; 
  end; 
  TVmtSetTypeInfo = class(TVmtOrdinalTypeInfo, ISetTypeInfo) 
  public 
    function GetCompType: IEnumTypeInfo; 
  end; 
  TVmtInt64TypeInfo = class(TVmtTypeInfo, IInt64TypeInfo) 
  public 
    function GetMinValue: Int64; 
    function GetMaxValue: Int64; 
  end; 
  TVmtFloatTypeInfo = class(TVmtTypeInfo, IFloatTypeInfo) 
  public 
    function GetFloatType: TFloatType; 
  end; 
  TVmtShortStringTypeInfo = class(TVmtTypeInfo, IShortStringTypeInfo) 
  public 
    function GetMaxLength: Integer; 
  end; 
 
  TVmtPropInfo = class(TInterfacedObject, IPropInfo) 
  private 
    fPropInfo: PPropInfo; 
    property PropInfo: PPropInfo read fPropInfo; 
  public 
    constructor Create(PropInfo: PPropInfo); 
    function GetPropType: ITypeInfo; 
    function GetReader: Pointer; 
    function GetWriter: Pointer; 
    function GetStoredProc: Pointer; 
    function GetReaderType: TProcType; 
    function GetWriterType: TProcType; 
    function GetStoredType: TStoredType; 
    function GetReaderValue: Integer; 
    function GetWriterValue: Integer; 
    function GetStoredValue: Integer; 
    function GetIndex: Integer; 
    function GetDefault: LongInt; 
    function GetNameIndex: SmallInt; 
    function GetName:TERRAString; 

    function HasDefault: Boolean; 
    function HasIndex: Boolean; 
  end; 
  TVmtClassTypeInfo = class(TVmtTypeInfo, IClassTypeInfo) 
  private 
    fPropList: PPropList; 
    property PropList: PPropList read fPropList; 
  public 
    constructor Create(TypeInfo: PTypeInfo); override; 
    Procedure Release; override; 
    function GetClassType: TClass; 
    function GetParentInfo: IClassTypeInfo; 
    function GetPropCount: Integer; 
    function GetUnitName:TERRAString; 
    function GetProperties(Index: Integer): IPropInfo; 
  end; 
  TVmtParamInfo = class(TInterfacedObject, IParamInfo)
  private
    fParamData: PTERRAChar; 
  public 
    constructor Create(Data: Pointer); 
    function GetFlags: TParamFlags; 
    function GetName:TERRAString; 
    function GetTypeName:TERRAString; 
  end; 
  TVmtMethodTypeInfo = class(TVmtTypeInfo, IMethodTypeInfo) 
  private 
    fParams: array of Pointer; 
    fResultType: PShortString; 
    procedure GetParamList; 
  public 
    constructor Create(TypeInfo: PTypeInfo); override; 
    function GetMethodKind: TMethodKind; 
    function GetParamCount: Byte; 
    function GetParams(Index: Integer): IParamInfo; 
    function GetResultType:TERRAString; 
  end; 
  TVmtInterfaceTypeInfo = class(TVmtTypeInfo, IInterfaceTypeInfo) 
  public 
    function GetIntfParent: IInterfaceTypeInfo; 
    function GetIntfFlags: TIntfFlagsBase; 
    function GetGuid: TGUID; 
    function GetIntfUnit:TERRAString; 
  end; 
 
  TVmtMethodParam = class(TInterfacedObject, IVmtMethodParam) 
  private 
    fParam: PMethodParam; 
  protected 
    property Param: PMethodParam read fParam; 
  public 
    constructor Create(Param: PMethodParam); 
    function GetName:TERRAString; 
    function GetType: ITypeInfo; 
    function GetTypeInfo: PTypeInfo; 
    property Name:TERRAString read GetName; 
    property ParamType: ITypeInfo read GetType; 
    property TypeInfo: PTypeInfo read GetTypeInfo; 
  end; 
 
 
function GetVmtInfo(ClassRef: TClass): IVmtInfo;
begin
  Result := TVmtInfo.Create(ClassRef) 
end; 
 
function GetTypeInfo(TypeInfo: PTypeInfo): ITypeInfo; 
begin 
  Result := TVmtTypeInfo.CreateTypeInfo(TypeInfo); 
end; 
 
{ TVmtInitEntry } 
 
constructor TVmtInitEntry.Create(Table: PInitTable; Offset: Integer); 
begin 
  inherited Create; 
  fTable := Table; 
  fOffset := Offset; 
end; 
 
function TVmtInitEntry.GetOffset: Integer; 
begin 
  Result := fOffset; 
end; 
 
function TVmtInitEntry.GetTypeKind: TTypeKind; 
begin 
  Result := TTypeKind(Table.TypeKind); 
end; 
 
function TVmtInitEntry.GetTypeName:TERRAString; 
begin 
  Result := PShortString(@Table.Data)^; 
end; 


{ TVmtInitAggregate } 
 
function TVmtInitAggregate.GetCount: Integer; 
var 
  Ptr: PByte; 
begin 
  if Table = nil then 
    Result := 0 
  else 
  begin 
    Ptr := PByte(@Table.Data); 
    { Skip over the TypeName and the DataSize. } 
    Inc(Ptr, Ptr^ + 1 + SizeOf(Integer)); 
    Result := PInteger(Ptr)^; 
  end; 
end; 
 
function TVmtInitAggregate.GetDataSize: Integer; 
var 
  Ptr: PByte; 
begin 
  Ptr := PByte(@Table.Data); 
  { Skip over the TypeName. } 
  Inc(Ptr, Ptr^ + 1); 
  Result := PInteger(Ptr)^; 
end; 
 
function TVmtInitAggregate.GetInitArray: PInitArray; 
var 
  Ptr: PByte; 
begin 
  Ptr := PByte(@Table.Data); 
  { Skip over the TypeName, DataSize, and Count. } 
  Inc(Ptr, Ptr^ + 1 + 2*SizeOf(Integer)); 
  Result := PInitArray(Ptr); 
end; 
 
function TVmtInitAggregate.GetInitializer(Index: Integer): IVmtInitEntry; 
var 
  Table: PInitTable; 
  Kind: TTypeKind; 
begin 
  if (Index < 0) or (Index >= Count) then 
    IndexError(Index); 
 
  Table := InitArray[Index].InitTable^; 
  if Table = nil then 
    Kind := tkUnknown 
  else 
    Kind := TTypeKind(Table.TypeKind); 
  case Kind of 
  tkArray: 
    Result := TVmtInitArray.Create(InitArray[Index].InitTable^, InitArray[Index].Offset); 
  tkRecord: 
    Result := TVmtInitRecord.Create(InitArray[Index].InitTable^, InitArray[Index].Offset); 
  else 
    Result := TVmtInitAggregate.Create(InitArray[Index].InitTable^, InitArray[Index].Offset); 
  end; 
end; 
 
{ TVmtInitRecord }

function TVmtInitRecord.GetRecordSize: Integer; 
begin 
  Result := GetDataSize 
end; 
 
{ TVmtInitArray }

function TVmtInitArray.GetElementSize: Integer;
begin
  Result := GetDataSize
end;

{ TVmtInfo } 
 
constructor TVmtInfo.Create(ClassRef: TClass); 
begin 
  fVmt := PVmt(ClassRef); 
  Dec(fVmt); 
end; 
 
function TVmtInfo.GetClass(Index: Integer): TClass; 
begin 
  if (Index < 0) or (Index >= GetClassCount) then 
    IndexError(Index); 
  Result := Vmt.FieldTable.FieldClassTable.Classes[Index]^; 
end; 
 
function TVmtInfo.GetClassCount: Integer; 
begin 
  if Vmt.FieldTable = nil then 
    Result := 0 
  else 
    Result := Vmt.FieldTable.FieldClassTable.Count; 
end; 
 
function TVmtInfo.GetClassName:TERRAString; 
begin 
  Result := Vmt.ClassName^; 
end; 
 
function TVmtInfo.GetClassType: TClass; 
begin 
  Result := Vmt.SelfPtr; 
end; 
 
function TVmtInfo.GetDynamicMethodCount: Integer; 
begin 
  if Vmt.DynMethodTable = nil then 
    Result := 0 
  else 
    Result := Vmt.DynMethodTable.Count; 
end; 
 
function TVmtInfo.GetField(Index: Integer): IVmtField; 
var 
  I: Integer; 
  Offset: Integer; 
  Field: PField; 
begin 
  if (Index < 0) or (Index >= GetFieldCount) then 
    IndexError(Index); 
 
  Offset := 0; 
  for I := 0 to Index-1 do 
    Offset := Offset + 
        SizeOf(Integer) + 
        SizeOf(Word) + 
        Length(PField(@Vmt.FieldTable.Fields[Offset]).Name) + 
        1 (* for the name length *); 
 
  Field := PField(@Vmt.FieldTable.Fields[Offset]); 
  Result := TVmtField.Create(Field, Vmt.FieldTable.FieldClassTable.Classes[Field.ClassIndex]^); 
end; 
 
function TVmtInfo.GetFieldCount: Integer; 
begin 
  if Vmt.FieldTable = nil then 
    Result := 0 
  else 
    Result := Vmt.FieldTable.Count; 
end; 
 
function TVmtInfo.GetFieldType(Index: Integer): TClass; 
begin 
  Result := GetClass(GetField(Index).ClassIndex) 
end; 
 
function TVmtInfo.GetInitCount: Integer; 
begin 
  Result := InitRecord.Count; 
end; 
 
function TVmtInfo.GetInitEntry(Index: Integer): IVmtInitEntry; 
begin 
  Result := InitRecord[Index]; 
end; 
 
function TVmtInfo.GetMethod(Index: Integer): IVmtMethod; 
var 
  I: Integer; 
  Method: PMethod; 
begin 
  if (Index < 0) or (Index >= GetMethodCount) then 
    IndexError(Index); 
 
  Method := PMethod(@Vmt.MethodTable.Methods[0]); 
  for I := 1 to Index do 
    Method := PMethod(PTERRAChar(Method) + Method.Size); 
 
  Result := TVmtMethod.Create(Method); 
end; 
 
function TVmtInfo.GetMethodCount: Integer; 
begin 
  if Vmt.MethodTable = nil then 
    Result := 0 
  else 
    Result := Vmt.MethodTable.Count; 
end; 
 
function TVmtInfo.GetParentVmt: IVmtInfo; 
begin 
  if Vmt.ClassParent = nil then 
    Result := nil 
  else 
    Result := GetVmtInfo(Vmt.ClassParent^);
end; 
 

function TVmtInfo.FindDynamicAddress(Number: SmallInt): Pointer;
var
  I: Integer;
  Indexes: PDynIndexes;
  Addresses: PDynAddresses;
begin
  Result := nil;
  if Vmt.DynMethodTable = nil then
    Exit; 
 
  Indexes := PDynIndexes(@Vmt.DynMethodTable.Data); 
  Addresses := PDynAddresses(PTERRAChar(Indexes) + GetDynamicMethodCount*SizeOf(SmallInt)); 
  for I := 0 to GetDynamicMethodCount-1 do 
    if Indexes[I] = Number then 
    begin 
      Result := Addresses[I]; 
      Break; 
    end; 
end; 
 
function TVmtInfo.GetDynamicAddress(Index: Integer): Pointer; 
var 
  Ptr: PByte; 
begin 
  if (Index < 0) or (Index >= GetDynamicMethodCount) then 
    IndexError(Index); 
  Ptr := PByte(@Vmt.DynMethodTable.Data); 
  Inc(Ptr, Vmt.DynMethodTable.Count * SizeOf(SmallInt)); 
  Result := PDynAddresses(Ptr)[Index]; 
end; 
 
function TVmtInfo.GetDynamicNumber(Index: Integer): SmallInt; 
begin 
  if (Index < 0) or (Index >= GetDynamicMethodCount) then 
    IndexError(Index);
  Result := PDynIndexes(@Vmt.DynMethodTable.Data)[Index]; 
end; 
 
function TVmtInfo.GetInitRecord: IVmtInitRecord; 
begin 
  if fInitRecord = nil then 
    fInitRecord := TVmtInitRecord.Create(Vmt.InitTable); 
  Result := fInitRecord; 
end; 
 
function TVmtInfo.GetAfterConstruction: Pointer; 
begin 
  Result := Vmt.AfterConstruction; 
end; 
 
function TVmtInfo.GetBeforeDestruction: Pointer; 
begin 
  Result := Vmt.BeforeDestruction 
end; 
 
function TVmtInfo.GetDefaultHandler: Pointer; 
begin 
  Result := Vmt.DefaultHandler 
end; 
 
function TVmtInfo.GetDestroy: Pointer; 
begin 
  Result := Vmt.Release 
end; 
 
function TVmtInfo.GetDispatch: Pointer; 
begin 
  Result := Vmt.Dispatch 
end; 
 
function TVmtInfo.GetFreeInstance: Pointer; 
begin 
  Result := Vmt.FreeInstance 
end; 
 
function TVmtInfo.GetInstanceSize: Integer; 
begin 
  Result := Vmt.InstanceSize 
end; 
 
function TVmtInfo.GetNewInstance: Pointer; 
begin 
  Result := Vmt.NewInstance 
end; 
 
function TVmtInfo.GetSafeCallException: Pointer; 
begin 
  Result := Vmt.SafeCallException 
end; 
 
function TVmtInfo.GetVmt: Pointer; 
begin 
  Result := Vmt; 
end; 
 
function TVmtInfo.GetAutoTable: Pointer; 
begin 
  Result := Vmt.AutoTable 
end; 
 
function TVmtInfo.GetDynamicTable: Pointer; 
begin 
  Result := Vmt.DynMethodTable 
end; 
 
function TVmtInfo.GetFieldClassTable: Pointer; 
begin 
  if Vmt.FieldTable = nil then 
    Result := nil 
  else 
    Result := Vmt.FieldTable.FieldClassTable; 
end; 
 
function TVmtInfo.GetFieldTable: Pointer; 
begin 
  Result := Vmt.FieldTable 
end; 
 
function TVmtInfo.GetInitTable: Pointer; 
begin 
  Result := Vmt.InitTable 
end; 
 
function TVmtInfo.GetIntfTable: Pointer; 
begin 
  Result := Vmt.IntfTable 
end; 
 
function TVmtInfo.GetMethodTable: Pointer; 
begin 
  Result := Vmt.MethodTable 
end; 
 
function TVmtInfo.GetTypeInfo: PTypeInfo; 
begin 
  Result := Vmt.TypeInfo 
end; 
 
function TVmtInfo.GetInterfaceCount: Integer; 
begin 
  if Vmt.IntfTable = nil then 
    Result := 0 
  else 
    Result := Vmt.IntfTable.EntryCount; 
end; 
 
function TVmtInfo.GetInterfaceEntry(Index: Integer): TInterfaceEntry; 
begin 
  if (Index < 0) or (Index >= GetInterfaceCount) then 
    IndexError(Index); 
  Result := Vmt.IntfTable.Entries[Index]; 
end; 
 
function TVmtInfo.GetAutoCount: Integer; 
begin 
  if Vmt.AutoTable = nil then 
    Result := 0 
  else 
    Result := Vmt.AutoTable.Count; 
end; 
 
function TVmtInfo.GetAutoEntry(Index: Integer): IVmtAutoEntry; 
begin 
  if (Index < 0) or (Index >= GetAutoCount) then 
    IndexError(Index); 
  Result := TVmtAutoEntry.Create(@Vmt.AutoTable.Entries[Index]); 
end; 
 
{ TVmtAutoParam } 
 
constructor TVmtAutoParam.Create(Param: TVmtAutoType); 
begin 
  inherited Create; 
  fParam := Param; 
end; 
 
function TVmtAutoParam.GetAutoType: TVmtAutoType; 
begin 
  Result := fParam and atTypeMask; 
end; 
 
function TVmtAutoParam.GetIsByRef: Boolean; 
begin 
  Result := (fParam and atByRef) <> 0; 
end; 
 
{ TVmtAutoEntry } 
 
constructor TVmtAutoEntry.Create(Entry: PAutoEntry); 
begin 
  inherited Create; 
  fEntry := Entry; 
end; 
 
function TVmtAutoEntry.GetAddress: Pointer; 
begin 
  Result := Entry.Address; 
end; 
 
function TVmtAutoEntry.GetDispID: Integer; 
begin 
  Result := Entry.DispID; 
end; 
 
function TVmtAutoEntry.GetFlags: TVmtAutoFlags; 
var 
  Tmp: Byte; 
begin 
  Tmp := Entry.Flags; 
  Result := TVmtAutoFlags(Tmp); 
end; 
 
function TVmtAutoEntry.GetName:TERRAString; 
begin 
  Result := Entry.Name^; 
end; 
 
function TVmtAutoEntry.GetParam(Index: Integer): IVmtAutoParam; 
begin 
  if (Index < 0) or (Index >= GetParamCount) then 
    IndexError(Index); 
  Result := TVmtAutoParam.Create(Entry.Params.Types[Index]); 
end; 
 
function TVmtAutoEntry.GetParamCount: Integer; 
begin 
  Result := Entry.Params.Count; 
end; 
 
function TVmtAutoEntry.GetResultType: TVmtAutoType; 
begin 
  Result := TVmtAutoType(Entry.Params.ReturnType); 
end; 
 

{ TVmtTypeInfo }

constructor TVmtTypeInfo.Create(TypeInfo: PTypeInfo);
begin
  inherited Create;
  fTypeInfo := TypeInfo;
end;

function TVmtTypeInfo.GetKind: TTypeKind;
begin
  Result := TypeInfo.Kind;
end;

function TVmtTypeInfo.GetName:TERRAString; 
begin 
  Result := TypeInfo.Name; 
end; 
 
function TVmtTypeInfo.GetTypeData: PTypeData; 
begin 
  if fTypeData = nil then 
    fTypeData := TypInfo.GetTypeData(TypeInfo); 
  Result := fTypeData; 
end; 
 
function TVmtTypeInfo.GetTypeInfo: PTypeInfo; 
begin 
  Result := fTypeInfo; 
end; 


class function TVmtTypeInfo.CreateTypeInfo(TypeInfo: PTypeInfo): TVmtTypeInfo; 
begin 
  case TypeInfo.Kind of 
  tkInteger: 
    Result := TVmtIntegerTypeInfo.Create(TypeInfo); 
  tkChar: 
    Result := TVmtCharTypeInfo.Create(TypeInfo); 
  tkWChar: 
    Result := TVmtWideCharTypeInfo.Create(TypeInfo); 
  tkInt64: 
    Result := TVmtInt64TypeInfo.Create(TypeInfo); 
  tkEnumeration: 
    Result := TVmtEnumTypeInfo.Create(TypeInfo); 
  tkFloat: 
    Result := TVmtFloatTypeInfo.Create(TypeInfo); 
  tkString: 
    Result := TVmtShortStringTypeInfo.Create(TypeInfo); 
  tkSet: 
    Result := TVmtSetTypeInfo.Create(TypeInfo); 
  tkClass: 
    Result := TVmtClassTypeInfo.Create(TypeInfo); 
  tkMethod: 
    Result := TVmtMethodTypeInfo.Create(TypeInfo); 
  tkInterface: 
    Result := TVmtInterfaceTypeInfo.Create(TypeInfo); 
  else 
    Result := TVmtTypeInfo.Create(TypeInfo); 
  end; 
end; 
 
// The hash value is the PTypeInfo pointer. Two type info interfaces are the 
// same when their type info pointers are the same. 
{ TVmtOrdinalTypeInfo }
 
function TVmtOrdinalTypeInfo.GetOrdType: TOrdType; 
begin 
  Result := TypeData.OrdType; 
end; 
 
{ TVmtRangeTypeInfo } 
 
function TVmtRangeTypeInfo.GetMaxValue: Integer; 
begin 
  Result := TypeData.MaxValue; 
end; 
 
function TVmtRangeTypeInfo.GetMinValue: Integer; 
begin 
  Result := TypeData.MinValue; 
end; 
 
{ TVmtIntegerTypeInfo }


function TVmtCharTypeInfo.GetMaxValue: Char; 
begin 
  Result := Char(inherited GetMaxValue) 
end; 
 
function TVmtCharTypeInfo.GetMinValue: Char; 
begin 
  Result := Char(inherited GetMinValue) 
end; 
 
{ TVmtWideCharTypeInfo }

function TVmtWideCharTypeInfo.GetMaxValue: WideChar; 
begin 
  Result := WideChar(inherited GetMaxValue) 
end; 
 
function TVmtWideCharTypeInfo.GetMinValue: WideChar; 
begin 
  Result := WideChar(inherited GetMinValue) 
end; 
 
 
{ TVmtEnumTypeInfo } 
 
constructor TVmtEnumTypeInfo.Create(TypeInfo: PTypeInfo); 
var 
  Ptr: PShortString; 
  I: Integer; 
  BaseTypeData: PTypeData; 
begin 
  Assert(TypeInfo.Kind = tkEnumeration); 
  inherited; 
 
  // The names are stored with the base enumerated type. Copy the names 
  // into the subrange type. It's a little wasteful, but only a little. 
  fNames := TStringList.Create; 
  BaseTypeData := TypInfo.GetTypeData(TypeData.BaseType^); 
  Ptr := PShortString(@BaseTypeData.NameList); 
  for I := BaseTypeData.MinValue to BaseTypeData.MaxValue do 
  begin 
    Names.Add(Ptr^); 
    Ptr := PShortString(PTERRAChar(Ptr) + Length(Ptr^) + 1); 
  end; 
end; 
 
Procedure TVmtEnumTypeInfo.Release; 
begin 
  fNames.Free; 
  fNames := nil; 
  inherited; 
end; 
 
 
function TVmtEnumTypeInfo.GetBaseType: IEnumTypeInfo; 
begin 
  Result := TVmtEnumTypeInfo.Create(TypeData.BaseType^); 
end; 
 
function TVmtEnumTypeInfo.GetNameCount: Integer; 
begin 
  Result := Names.Count; 
end; 
 
function TVmtEnumTypeInfo.GetNames(Index: Integer):TERRAString; 
begin 
  Result := Names[Index]; 
end; 
 
function TVmtEnumTypeInfo.IsSubRange: Boolean; 
begin 
  Result := TypeData.BaseType^ <> TypeInfo; 
end; 
 
 
{ TVmtSetTypeInfo } 


function TVmtSetTypeInfo.GetCompType: IEnumTypeInfo; 
begin 
  Result := TVmtEnumTypeInfo.Create(TypeData.CompType^); 
end; 
 
{ TVmtInt64TypeInfo } 


function TVmtInt64TypeInfo.GetMaxValue: Int64; 
begin 
  Result := TypeData.MaxInt64Value 
end; 
 
function TVmtInt64TypeInfo.GetMinValue: Int64; 
begin 
  Result := TypeData.MinInt64Value 
end; 
 
{ TVmtFloatTypeInfo } 
 
function TVmtFloatTypeInfo.GetFloatType: TFloatType; 
begin 
  Result := TypeData.FloatType; 
end; 


{ TVmtShortStringTypeInfo } 


function TVmtShortStringTypeInfo.GetMaxLength: Integer; 
begin 
  Result := TypeData.MaxLength; 
end; 
 
{ TVmtPropInfo } 
 
constructor TVmtPropInfo.Create(PropInfo: PPropInfo); 
begin 
  inherited Create; 
  fPropInfo := PropInfo; 
end; 
 
function TVmtPropInfo.GetDefault: LongInt; 
begin 
  Result := PropInfo.Default 
end; 
 
function TVmtPropInfo.GetIndex: Integer; 
begin 
  Result := PropInfo.Index 
end; 
 
function TVmtPropInfo.GetName:TERRAString; 
begin 
  Result := PropInfo.Name 
end; 
 
function TVmtPropInfo.GetNameIndex: SmallInt; 
begin 
  Result := PropInfo.NameIndex 
end; 
 
function TVmtPropInfo.GetPropType: ITypeInfo; 
begin 
  Result := TVmtTypeInfo.CreateTypeInfo(PropInfo.PropType^); 
end; 
 
function GetProcType(Value: Pointer): TStoredType; 
begin 
  if (Integer(Value) and $FFFFFF00) = 0 then 
    Result := ptConstant 
  else if (Integer(Value) and $FF000000) = $FE000000 then 
    Result := ptVirtualMethod 
  else if (Integer(Value) and $FF000000) = $FF000000 then 
    Result := ptField 
  else 
    Result := ptStaticMethod 
end; 
 
function GetProcValue(Value: Pointer): Integer; 
begin 
  case GetProcType(Value) of 
  ptVirtualMethod, ptField: 
    Result := Integer(Value) and $00FFFFFF; 
  else 
    Result := Integer(Value); 
  end; 
end; 
 
function TVmtPropInfo.GetReader: Pointer; 
begin 
  Result := PropInfo.GetProc 
end; 
 
function TVmtPropInfo.GetReaderType: TProcType; 
begin 
  Result := GetProcType(GetReader) 
end; 
 
function TVmtPropInfo.GetReaderValue: Integer; 
begin 
  Result := GetProcValue(GetReader) 
end; 
 
function TVmtPropInfo.GetStoredProc: Pointer; 
begin 
  Result := PropInfo.StoredProc 
end; 
 
function TVmtPropInfo.GetStoredType: TStoredType; 
begin 
  Result := GetProcType(GetStoredProc) 
end; 
 
function TVmtPropInfo.GetStoredValue: Integer; 
begin 
  Result := GetProcValue(GetStoredProc) 
end; 
 
function TVmtPropInfo.GetWriter: Pointer; 
begin 
  Result := PropInfo.SetProc 
end; 
 
function TVmtPropInfo.GetWriterType: TProcType; 
begin 
  Result := GetProcType(GetWriter) 
end; 
 
function TVmtPropInfo.GetWriterValue: Integer; 
begin 
  Result := GetProcValue(GetWriter) 
end; 
 
function TVmtPropInfo.HasDefault: Boolean; 
begin 
  Result := GetDefault <> Low(LongInt); 
end; 
 
function TVmtPropInfo.HasIndex: Boolean; 
begin 
  Result := GetIndex <> Low(Integer); 
end; 
 

{ TVmtClassTypeInfo }

constructor TVmtClassTypeInfo.Create(TypeInfo: PTypeInfo);
begin
  Assert(TypeInfo.Kind = tkClass);
  inherited;
  GetMem(fPropList, GetPropCount * SizeOf(TPropInfo));
  GetPropInfos(TypeInfo, PropList);
end;

Procedure TVmtClassTypeInfo.Release; 
begin 
  FreeMem(PropList); 
  fPropList := nil; 
  inherited; 
end; 
 
function TVmtClassTypeInfo.GetClassType: TClass; 
begin 
  Result := TypeData.ClassType 
end; 
 
function TVmtClassTypeInfo.GetParentInfo: IClassTypeInfo; 
begin 
  Result := TVmtClassTypeInfo.Create(TypeData.ParentInfo^); 
end; 
 
function TVmtClassTypeInfo.GetPropCount: Integer; 
begin 
  Result := TypeData.PropCount 
end; 
 
function TVmtClassTypeInfo.GetProperties(Index: Integer): IPropInfo; 
begin 
  if (Index < 0) or (Index >= GetPropCount) then 
    IndexError(Index); 
  Result := TVmtPropInfo.Create(PropList[Index]); 
end; 
 
function TVmtClassTypeInfo.GetUnitName:TERRAString; 
begin 
  Result := TypeData.UnitName 
end; 
 

{ TVmtParamInfo }

constructor TVmtParamInfo.Create(Data: Pointer);
begin
  inherited Create;
  fParamData := Data;
end;

function TVmtParamInfo.GetFlags: TParamFlags;
type
  PParamFlags = ^TParamFlags;
begin 
  Result := PParamFlags(fParamData)^ 
end; 
 
function TVmtParamInfo.GetName:TERRAString; 
begin 
  Result := PShortString(fParamData + 1)^; 
end; 
 
function TVmtParamInfo.GetTypeName:TERRAString; 
begin 
  Result := PShortString(fParamData + 1 + Length(GetName) + 1)^; 
end; 
 

{ TVmtMethodTypeInfo }

constructor TVmtMethodTypeInfo.Create(TypeInfo: PTypeInfo);
begin
  Assert(TypeInfo.Kind = tkMethod);
  inherited;
  GetParamList;
end;

function TVmtMethodTypeInfo.GetMethodKind: TMethodKind;
begin
  Result := TypeData.MethodKind 
end; 
 
function TVmtMethodTypeInfo.GetParamCount: Byte; 
begin 
  Result := TypeData.ParamCount 
end; 
 
procedure TVmtMethodTypeInfo.GetParamList; 
var 
  Ptr: PByte; 
  I: Integer; 
begin 
  if fResultType = nil then 
  begin 
    SetLength(fParams, TypeData.ParamCount); 
    Ptr := PByte(@TypeData.ParamList); 
    for I := 0 to TypeData.ParamCount-1 do 
    begin 
      fParams[I] := Ptr; 
      Inc(Ptr, 1);         // skip type flags; 
      Inc(Ptr, Ptr^ + 1);  // skip param name 
      Inc(Ptr, Ptr^ + 1);  // skip type name 
    end; 
    fResultType := PShortString(Ptr); 
  end; 
end; 
 
function TVmtMethodTypeInfo.GetParams(Index: Integer): IParamInfo; 
begin 
  Result := TVmtParamInfo.Create(fParams[Index]); 
end; 
 
function TVmtMethodTypeInfo.GetResultType:TERRAString; 
begin 
  Result := fResultType^; 
end; 
 

{ TVmtInterfaceTypeInfo }

function TVmtInterfaceTypeInfo.GetGuid: TGUID; 
begin 
  Result := TypeData.Guid 
end; 
 
function TVmtInterfaceTypeInfo.GetIntfFlags: TIntfFlagsBase; 
begin 
  Result := TypeData.IntfFlags 
end; 
 
function TVmtInterfaceTypeInfo.GetIntfParent: IInterfaceTypeInfo; 
begin 
  Result := TVmtInterfaceTypeInfo.Create(TypeData.IntfParent^); 
end; 
 
function TVmtInterfaceTypeInfo.GetIntfUnit:TERRAString; 
begin 
  Result := TypeData.IntfUnit 
end; 
 
 
{ TVmtField } 
 
constructor TVmtField.Create(Field: PField; FieldClass: TClass); 
begin 
  inherited Create; 
  fField := Field; 
  fClass := FieldClass; 
end; 
 
function TVmtField.GetClass: TClass; 
begin 
  Result := fClass; 
end; 
 
function TVmtField.GetClassIndex: Word; 
begin 
  Result := Field.ClassIndex; 
end; 
 
function TVmtField.GetName:TERRAString; 
begin 
  Result := Field.Name; 
end; 
 
function TVmtField.GetOffset: Integer; 
begin 
  Result := Field.Offset; 
end; 
 
function TVmtField.GetType: IClassTypeInfo; 
begin 
  Result := TVmtClassTypeInfo.Create(FieldClass.ClassInfo); 
end; 
 
{ TVmtMethod } 
 
constructor TVmtMethod.Create(Method: PMethod); 
begin 
  inherited Create; 
  fMethod := Method; 
  CountParams; 
end; 
 
function TVmtMethod.GetAddress: Pointer; 
begin 
  Result := Method.Address; 
end; 
 
function TVmtMethod.GetBaseSize: Integer; 
begin 
  Result := SizeOf(Word) + SizeOf(Pointer) + Length(Method.Name) + 1; 
end; 
 
function TVmtMethod.GetName:TERRAString; 
begin 
  Result := Method.Name; 
end; 
 
// The parameters begin 6 bytes after the base method record. 
const 
  MethodPadding = 6; 
 
procedure TVmtMethod.CountParams; 
var 
  ParamPtr: PMethodParam; 
  ParamSize: Integer; 
  TotalSize: Integer; 
begin 
  fParamCount := 0; 
  TotalSize := GetBaseSize + MethodPadding; 
  if Method.Size > TotalSize then 
  begin 
    ParamPtr := PMethodParam(PTERRAChar(Method) + TotalSize); 
 
    // Loop through all the method parameters. 
    while Method.Size - TotalSize > SizeOf(PTypeInfo) do 
    begin 
      // Increment the pointer past the TypeInfo pointer, the param name, 
      // its length, and a trailing #0 byte. 
      ParamSize := SizeOf(PTypeInfo) + Length(ParamPtr.Name) + 2; 
      ParamPtr := PMethodParam(PTERRAChar(ParamPtr) + ParamSize); 
      Inc(TotalSize, ParamSize); 
      Inc(fParamCount); 
    end; 
  end; 
end; 
 
function TVmtMethod.GetParamData(Index: Integer): PMethodParam; 
var 
  I: Integer; 
begin 
  if (Index < 0) or (Index >= GetParamCount) then 
    IndexError(Index); 
 
  Result := PMethodParam(PTERRAChar(Method) + GetBaseSize + MethodPadding); 
  for I := 0 to Index-1 do 
    // Increment the pointer past the TypeInfo pointer, the param name, 
    // its length, and a trailing #0 byte. 
    Result := PMethodParam(PTERRAChar(Result) + 
                           SizeOf(PTypeInfo) + Length(Result.Name) + 2); 
end; 
 
function TVmtMethod.GetParams(Index: Integer): IVmtMethodParam; 
begin 
  Result := TVmtMethodParam.Create(GetParamData(Index)); 
end; 
 
function TVmtMethod.HasParamInfo: Boolean; 
begin 
  Result := Method.Size > GetBaseSize; 
end; 
 
function TVmtMethod.GetParamCount: Integer; 
begin 
  Result := fParamCount; 
end; 
 

{ TVmtMethodParam }

constructor TVmtMethodParam.Create(Param: PMethodParam);
begin
  inherited Create;
  fParam := Param;
end;

function TVmtMethodParam.GetName:TERRAString;
begin
  Result := Param.Name;
end;
 
function TVmtMethodParam.GetType: ITypeInfo; 
begin 
  Result := TVmtTypeInfo.CreateTypeInfo(TypeInfo); 
end; 
 
function TVmtMethodParam.GetTypeInfo: PTypeInfo; 
begin 
  Result := Param.TypeInfo^ 
end; 
 
end. 


