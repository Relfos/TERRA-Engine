Unit IOHIDLib;

{$I terra.inc}

Interface

Uses CoreFoundation,IOKitReturn;

Type
IOOptionBits = Cardinal;
IOFixed = Integer;
IOVersion = Cardinal;
IOItemCount = Cardinal;
IOCacheMode = Cardinal;

IOByteCount32 = Cardinal;
IOByteCount64 = Int64;

IOPhysicalAddress32 = Cardinal;
IOPhysicalAddress64 = Int64;
IOPhysicalLength32 = Cardinal;
IOPhysicalLength64 = Int64;

(*typedef mach_vm_address_t	IOVirtualAddress;
#else
typedef vm_address_t		IOVirtualAddress;
#endif

#if defined(__LP64__) && defined(KERNEL)
typedef IOByteCount64		IOByteCount;
#else
typedef IOByteCount32	 	IOByteCount;
#endif

typedef IOVirtualAddress    IOLogicalAddress;

#if defined(__LP64__) && defined(KERNEL)

typedef IOPhysicalAddress64	 IOPhysicalAddress;
typedef IOPhysicalLength64	 IOPhysicalLength;
#define IOPhysical32( hi, lo )		((UInt64) lo + ((UInt64)(hi) << 32))
#define IOPhysSize	64

#else

typedef IOPhysicalAddress32	 IOPhysicalAddress;
typedef IOPhysicalLength32	 IOPhysicalLength;
#define IOPhysical32( hi, lo )		(lo)
#define IOPhysSize	32

#endif


typedef struct
{
    IOPhysicalAddress	address;
    IOByteCount		length;
} IOPhysicalRange;

typedef struct
{
    IOVirtualAddress	address;
    IOByteCount		length;
} IOVirtualRange;

#ifdef __LP64__
typedef IOVirtualRange	IOAddressRange;
#else /* !__LP64__ */
typedef struct
{
    mach_vm_address_t	address;
    mach_vm_size_t	length;
} IOAddressRange;
*)

// Map between #defined or enum'd constants and text description.

  IONamedValue = Packed Record
	  value:Integer;
  	name:PAnsiChar;
  End;



// _IOKIT_HID_IOHIDKEYS_H_

// The following keys are used to search the IORegistry for HID related services

// This is used to find HID Devices in the IORegistry
  kIOHIDDeviceKey                     = 'IOHIDDevice';

kIOHIDTransportKey                  = 'Transport';
kIOHIDVendorIDKey                   = 'VendorID';
kIOHIDVendorIDSourceKey             = 'VendorIDSource';
kIOHIDProductIDKey                  = 'ProductID';
kIOHIDVersionNumberKey              = 'VersionNumber';
kIOHIDManufacturerKey               = 'Manufacturer';
kIOHIDProductKey                    = 'Product';
kIOHIDSerialNumberKey               = 'SerialNumber';
kIOHIDCountryCodeKey                = 'CountryCode';
kIOHIDStandardTypeKey               = 'StandardType';
kIOHIDLocationIDKey                 = 'LocationID';
kIOHIDDeviceUsageKey                = 'DeviceUsage';
kIOHIDDeviceUsagePageKey            = 'DeviceUsagePage';
kIOHIDDeviceUsagePairsKey           = 'DeviceUsagePairs';
kIOHIDPrimaryUsageKey               = 'PrimaryUsage';
kIOHIDPrimaryUsagePageKey           = 'PrimaryUsagePage';
kIOHIDMaxInputReportSizeKey         = 'MaxInputReportSize';
kIOHIDMaxOutputReportSizeKey	    = 'MaxOutputReportSize';
kIOHIDMaxFeatureReportSizeKey	    = 'MaxFeatureReportSize';
kIOHIDReportIntervalKey             = 'ReportInterval';
kIOHIDReportDescriptorKey           = 'ReportDescriptor';

kIOHIDElementKey                    = 'Elements';

kIOHIDElementCookieKey                      = 'ElementCookie';
kIOHIDElementTypeKey                        = 'Type';
kIOHIDElementCollectionTypeKey              = 'CollectionType';
kIOHIDElementUsageKey                       = 'Usage';
kIOHIDElementUsagePageKey                   = 'UsagePage';
kIOHIDElementMinKey                         = 'Min';
kIOHIDElementMaxKey                         = 'Max';
kIOHIDElementScaledMinKey                   = 'ScaledMin';
kIOHIDElementScaledMaxKey                   = 'ScaledMax';
kIOHIDElementSizeKey                        = 'Size';
kIOHIDElementReportSizeKey                  = 'ReportSize';
kIOHIDElementReportCountKey                 = 'ReportCount';
kIOHIDElementReportIDKey                    = 'ReportID';
kIOHIDElementIsArrayKey                     = 'IsArray';
kIOHIDElementIsRelativeKey                  = 'IsRelative';
kIOHIDElementIsWrappingKey                  = 'IsWrapping';
kIOHIDElementIsNonLinearKey                 = 'IsNonLinear';
kIOHIDElementHasPreferredStateKey           = 'HasPreferredState';
kIOHIDElementHasNullStateKey                = 'HasNullState';
kIOHIDElementFlagsKey                       = 'Flags';
kIOHIDElementUnitKey                        = 'Unit';
kIOHIDElementUnitExponentKey                = 'UnitExponent';
kIOHIDElementNameKey                        = 'Name';
kIOHIDElementValueLocationKey               = 'ValueLocation';
kIOHIDElementDuplicateIndexKey              = 'DuplicateIndex';
kIOHIDElementParentCollectionKey            = 'ParentCollection';
kIOHIDElementVendorSpecificKey          = 'VendorSpecific';
kIOHIDElementCookieMinKey           = 'ElementCookieMin';
kIOHIDElementCookieMaxKey           = 'ElementCookieMax';
kIOHIDElementUsageMinKey            = 'UsageMin';
kIOHIDElementUsageMaxKey            = 'UsageMax';
kIOHIDElementCalibrationMinKey              = 'CalibrationMin';
kIOHIDElementCalibrationMaxKey              = 'CalibrationMax';
kIOHIDElementCalibrationSaturationMinKey    = 'CalibrationSaturationMin';
kIOHIDElementCalibrationSaturationMaxKey    = 'CalibrationSaturationMax';
kIOHIDElementCalibrationDeadZoneMinKey      = 'CalibrationDeadZoneMin';
kIOHIDElementCalibrationDeadZoneMaxKey      = 'CalibrationDeadZoneMax';
kIOHIDElementCalibrationGranularityKey      = 'CalibrationGranularity';


Type
  IOHIDElementType = Integer;

Const
	kIOHIDElementTypeInput_Misc        = 1;
	kIOHIDElementTypeInput_Button      = 2;
	kIOHIDElementTypeInput_Axis        = 3;
	kIOHIDElementTypeInput_ScanCodes   = 4;
	kIOHIDElementTypeOutput            = 129;
	kIOHIDElementTypeFeature           = 257;
	kIOHIDElementTypeCollection        = 513;

IOHIDElementCollectionType =(
	kIOHIDElementCollectionTypePhysical	= 0,
	kIOHIDElementCollectionTypeApplication,
    kIOHIDElementCollectionTypeLogical,
    kIOHIDElementCollectionTypeReport,
    kIOHIDElementCollectionTypeNamedArray,
    kIOHIDElementCollectionTypeUsageSwitch,
    kIOHIDElementCollectionTypeUsageModifier
);


IOHIDReportType = (
    kIOHIDReportTypeInput = 0,
    kIOHIDReportTypeOutput,
    kIOHIDReportTypeFeature,
    kIOHIDReportTypeCount
);

IOHIDOptionsType = (
    kIOHIDOptionsTypeNone	 = 0,
    kIOHIDOptionsTypeSeizeDevice = 1
    );

IOHIDQueueOptionsType = (
    kIOHIDQueueOptionsTypeNone	 = 0,
    kIOHIDQueueOptionsTypeEnqueueAll = 1
);


IOHIDElementFlags = (
    kIOHIDElementFlagsConstantMask        = $0001,
    kIOHIDElementFlagsVariableMask        = $0002,
    kIOHIDElementFlagsRelativeMask        = $0004,
    kIOHIDElementFlagsWrapMask            = $0008,
    kIOHIDElementFlagsNonLinearMask       = $0010,
    kIOHIDElementFlagsNoPreferredMask     = $0020,
    kIOHIDElementFlagsNullStateMask       = $0040,
    kIOHIDElementFlagsVolativeMask        = $0080,
    kIOHIDElementFlagsBufferedByteMask    = $0100
);

IOHIDStandardType = (
    kIOHIDStandardTypeANSI                = 0,
    kIOHIDStandardTypeISO                 = 1,
    kIOHIDStandardTypeJIS                 = 2
);

IOHIDElementCookie = Cardinal;

IOHIDEventStruct = Packed Record
    _type:IOHIDElementType;
    elementCookie:IOHIDElementCookie;
    value:Integer;
    timestamp:Int64;
    longValueSize:Cardinal;
    longValue:Pointer;
  End;

// FA12FA38-6F1A-11D4-BA0C-0005028F18D5
(*
kIOHIDDeviceUserClientTypeID CFUUIDGetConstantUUIDWithBytes(NULL, \
    0xFA, 0x12, 0xFA, 0x38, 0x6F, 0x1A, 0x11, 0xD4,			\
    0xBA, 0x0C, 0x00, 0x05, 0x02, 0x8F, 0x18, 0xD5)

/* 13AA9C44-6F1B-11D4-907C-0005028F18D5 */
#define kIOHIDDeviceFactoryID CFUUIDGetConstantUUIDWithBytes(NULL,	\
    0x13, 0xAA, 0x9C, 0x44, 0x6F, 0x1B, 0x11, 0xD4,			\
    0x90, 0x7C, 0x00, 0x05, 0x02, 0x8F, 0x18, 0xD5)

/* 78BD420C-6F14-11D4-9474-0005028F18D5 */
/*! @defined kIOHIDDeviceInterfaceID
    @discussion Interface ID for the IOHIDDeviceInterface. Corresponds to an
                 available HID device. */
#define kIOHIDDeviceInterfaceID CFUUIDGetConstantUUIDWithBytes(NULL, 	\
    0x78, 0xBD, 0x42, 0x0C, 0x6F, 0x14, 0x11, 0xD4,			\
    0x94, 0x74, 0x00, 0x05, 0x02, 0x8F, 0x18, 0xD5)

/* 8138629E-6F14-11D4-970E-0005028F18D5 */
/*! @defined kIOHIDQueueInterfaceID
    @discussion Interface ID for the kIOHIDQueueInterfaceID. Corresponds to a
                queue for a specific HID device. */
#define kIOHIDQueueInterfaceID CFUUIDGetConstantUUIDWithBytes(NULL, 	\
    0x81, 0x38, 0x62, 0x9E, 0x6F, 0x14, 0x11, 0xD4,			\
    0x97, 0x0E, 0x00, 0x05, 0x02, 0x8F, 0x18, 0xD5)

/* 80CDCC00-755D-11D4-8E0F-0005028F18D5 */
/*! @defined kIOHIDOutputTransactionInterfaceID
    @discussion Interface ID for the kIOHIDOutputTransactionInterfaceID.
                Corresponds to a output transaction for one or more report IDs
                on a specific device. */
#define kIOHIDOutputTransactionInterfaceID CFUUIDGetConstantUUIDWithBytes(NULL,\
    0x80, 0xCD, 0xCC, 0x00, 0x75, 0x5D, 0x11, 0xD4,			\
    0x80, 0xEF, 0x00, 0x05, 0x02, 0x8F, 0x18, 0xD5)*)

/*! @typedef IOHIDCallbackFunction
    @discussion Type and arguments of callout C function that is used when a
                completion routine is called, see
                IOHIDLib.h:setRemovalCallback().
    @param target void * pointer to your data, often a pointer to an object.
    @param result completion result of desired operation
    @param refcon void * pointer to more data.
    @param sender interface instance sending the completion routine.
*/

IOHIDCallbackFunction = Procedure (target:Pointer; result:IOReturn; refcon, sender:Pointer); MWPascal;

IOHIDElementCallbackFunction = Procedure (Target:Pointer; result:IOReturn; refcon, sender:Pointer; elementCookie:IOHIDElementCookie); MWPascal;

{
IOHIDDeviceInterface = Packed Record
    IUNKNOWN_C_GUTS;

    // Completion plumbing (must be set before any callbacks used)
    // Either createAsyncEventSource or createAsyncPort should be used
    IOReturn (*createAsyncEventSource)(void * 			self,
                                       CFRunLoopSourceRef * 	source);
    CFRunLoopSourceRef (*getAsyncEventSource)(void * self);

    IOReturn (*createAsyncPort)(void * self, mach_port_t * port);
    mach_port_t (*getAsyncPort)(void * self);

    /* Basic IOHIDDevice interface */
    IOReturn (*open)(void * self, UInt32 flags);
    IOReturn (*close)(void * self);

    /* removalCallback is called if the device is removed. */
    /* removeTarget and removalRefcon are passed to the callback. */
    IOReturn (*setRemovalCallback)(void * 			self,
                                   IOHIDCallbackFunction *	removalCallback,
                                   void *			removalTarget,
                                   void *			removalRefcon);

    /* Polling the most recent value of an element */
    /* The timestamp in the event is the last time the element was changed. */
    /* This call is most useful for the input element type. */
    IOReturn (*getElementValue)(void * 			self,
                                IOHIDElementCookie	elementCookie,
                                IOHIDEventStruct *	valueEvent);

    /* This call sets a value in a device. */
    /* It is most useful for the feature element type. */
    /* Using IOOutputTransaction is a better choice for output elements  */
    /* timoutMS is the timeout in milliseconds, a zero timeout will cause */
    /*	this call to be non-blocking (returning queue empty) if there */
    /*	is a NULL callback, and blocking forever until the queue is */
    /*	non-empty if their is a valid callback */
    /* callback, if non-NULL is a callback to be called when data is */
    /*  inserted to the queue  */
    /* callbackTarget and callbackRefcon are passed to the callback */
    IOReturn (*setElementValue)(void *	 			self,
                                IOHIDElementCookie		elementCookie,
                                IOHIDEventStruct *		valueEvent,
                                UInt32 				timeoutMS,
                                IOHIDElementCallbackFunction *	callback,
                                void * 				callbackTarget,
                                void *				callbackRefcon);

    /* This call actually querys the device. */
    /* It is most useful for the feature element type. */
    /* Not all devices support this call for inputs, but all do for features */
    /* timoutMS is the timeout in milliseconds, a zero timeout will cause */
    /*	this call to be non-blocking (returning queue empty) if there */
    /*	is a NULL callback, and blocking forever until the queue is */
    /*	non-empty if their is a valid callback */
    /* callback, if non-NULL is a callback to be called when data is */
    /*  inserted to the queue  */
    /* callbackTarget and callbackRefcon are passed to the callback */
    IOReturn (*queryElementValue)(void * 			self,
                                IOHIDElementCookie		elementCookie,
                                IOHIDEventStruct *		valueEvent,
                                UInt32 				timeoutMS,
                                IOHIDElementCallbackFunction *	callback,
                                void * 				callbackTarget,
                                void *				callbackRefcon);

    /* start/stop data delivery every queue for a device */
    IOReturn (*startAllQueues)(void * self);
    IOReturn (*stopAllQueues)(void * self);

    /* Wrapper to return instances of the IOHIDQueueInterface */
    IOHIDQueueInterface ** (*allocQueue) (void *self);

    /* Wrapper to return instances of the IOHIDOutputTransactionInterface */
    IOHIDOutputTransactionInterface ** (*allocOutputTransaction) (void *self);
};


struct IOHIDQueueInterface
{
    IUNKNOWN_C_GUTS;
    
    /* Completion plumbing (overrides that set in IOHIDDevice) */
    IOReturn (*createAsyncEventSource)(void * 			self, 
                                        CFRunLoopSourceRef * 	source);
    CFRunLoopSourceRef (*getAsyncEventSource)(void * self);

    IOReturn (*createAsyncPort)(void * self, mach_port_t * port);
    mach_port_t (*getAsyncPort)(void * self);
    
    /* Basic IOHIDQueue interface */
    /* depth is the maximum number of elements in the queue before	*/
    /*   the oldest elements in the queue begin to be lost		*/
    IOReturn (*create)(void * 			self, 
                        UInt32 			flags,
                        UInt32			depth);
    IOReturn (*dispose)(void * self);
    
    /* Any number of hid elements can feed the same queue */
    IOReturn (*addElement)(void * self,
                           IOHIDElementCookie elementCookie,
                           UInt32 flags);
    IOReturn (*removeElement)(void * self, IOHIDElementCookie elementCookie);
    Boolean (*hasElement)(void * self, IOHIDElementCookie elementCookie);

    /* start/stop data delivery to a queue */
    IOReturn (*start)(void * self);
    IOReturn (*stop)(void * self);
    
    /* read next event from a queue */
    /* maxtime, if non-zero, limits read events to those that occured */
    /*   on or before maxTime */
    /* timoutMS is the timeout in milliseconds, a zero timeout will cause */
    /*	this call to be non-blocking (returning queue empty) if there */
    /*	is a NULL callback, and blocking forever until the queue is */
    /*	non-empty if their is a valid callback */
    IOReturn (*getNextEvent)(void * 			self,
                            IOHIDEventStruct *		event,
                            AbsoluteTime		maxTime,
                            UInt32 			timeoutMS);
    
    /* set a callback for notification when queue transistions from non-empty */
    /* callback, if non-NULL is a callback to be called when data is */
    /*  inserted to the queue  */
    /* callbackTarget and callbackRefcon are passed to the callback */
    IOReturn (*setEventCallout)(void * 			self,
                                IOHIDCallbackFunction * callback,
                                void * 			callbackTarget,
                                void *			callbackRefcon);

    /* Get the current notification callout */
    IOReturn (*getEventCallout)(void * 			self,
                                IOHIDCallbackFunction ** outCallback,
                                void ** 		outCallbackTarget,
                                void **			outCallbackRefcon);
};

struct IOHIDOutputTransactionInterface
{
    IUNKNOWN_C_GUTS;

    /* Completion plumbing (overrides that set in IOHIDDevice) */
    IOReturn (*createAsyncEventSource)(void * 			self,
                                       CFRunLoopSourceRef *	source);
    CFRunLoopSourceRef (*getAsyncEventSource)(void * self);

    IOReturn (*createAsyncPort)(void * self, mach_port_t * port);
    mach_port_t (*getAsyncPort)(void * self);
    
    /* Basic IOHIDOutputTransaction interface */
    IOReturn (*create)(void * self);
    IOReturn (*dispose)(void * self);
    
    /* Any number of hid elements can be part of the same transaction. */
    /* Elements only need to be added once */
    IOReturn (*addElement)(void * self, IOHIDElementCookie elementCookie);
    IOReturn (*removeElement)(void * self, IOHIDElementCookie elementCookie);
    Boolean (*hasElement)(void * self, IOHIDElementCookie elementCookie);
    
    /* This changes the default value of an element, when the values of the */
    /* elements are cleared, on clear or commit, they are reset to the */
    /* default value */
    /* This call can be made on elements that are not in the transaction, but */
    /* has undefined behavior if made on elements not in the transaction */
    /* which are later added to the transaction. */
    /* In other words, an element should be added before its default is */
    /* set, for well defined behavior. */
    IOReturn (*setElementDefault)(void *	 	self,
                                  IOHIDElementCookie	elementCookie,
                                  IOHIDEventStruct *	valueEvent);

    /* Get the current setting of an element's default value */
    IOReturn (*getElementDefault)(void *	 	self,
                                  IOHIDElementCookie	elementCookie,
                                  IOHIDEventStruct *	outValueEvent);

    /* Add a change to the transaction, by setting an element value */
    /* The change is not actually made until it is commited */
    /* The element must be part of the transaction or this call will fail */
    IOReturn (*setElementValue)(void *	 		self,
                                IOHIDElementCookie	elementCookie,
                                IOHIDEventStruct *	valueEvent);

    /* Get the current setting of an element value */
    IOReturn (*getElementValue)(void *	 		self,
                                IOHIDElementCookie	elementCookie,
                                IOHIDEventStruct *	outValueEvent);

    /* Commit the transaction, or clear all the changes and start over */
    /* timoutMS is the timeout in milliseconds, a zero timeout will cause */
    /*	this call to be non-blocking (returning queue empty) if there */
    /*	is a NULL callback, and blocking forever until the queue is */
    /*	non-empty if their is a valid callback */
    /* callback, if non-NULL is a callback to be called when data is */
    /*  inserted to the queue  */
    /* callbackTarget and callbackRefcon are passed to the callback */
   IOReturn (*commit)(void * 			self,
                        UInt32 			timeoutMS,
                        IOHIDCallbackFunction * callback,
                        void * 			callbackTarget,
                        void *			callbackRefcon);

    /* Clear all the changes and start over */
    IOReturn (*clear)(void * self);

};


