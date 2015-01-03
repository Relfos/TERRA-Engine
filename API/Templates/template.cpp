//TERRA Engine Header

#ifndef TERRA_ENGINE
#define TERRA_ENGINE

#ifdef __cplusplus
extern "C" {
#endif

#ifdef _WIN32
#include "stdafx.h"
#include <windows.h>
#define TERRA_DYNAMIC_LOAD

#else
#define NULL 0
#endif

#include "stdint.h"

#define INT8 int8_t
#define INT16 int16_t
#define INT32 int32_t
#define UINT8 uint8_t
#define UINT16 uint16_t
#define UINT32 uint32_t

#define TERRASkybox void*
#define TERRAMesh void*
#define TERRAMeshInstance void*
#define TERRAOccluder void*
#define TERRATexture void*
#define TERRAFont void*
#define TERRASound void*
#define TERRASoundSource void*
#define TERRASprite void*
#define TERRAViewport void*
#define TERRACamera void*
#define TERRAImage void*
#define TERRAStream void*
#define TERRATween void*
#define TERRAShader void*
#define TERRAMeshGroup void*
#define TERRAPointLight void*
#define TERRATileSheet void*
#define TERRAPath void*
#define TERRAPathNode void*
#define TERRATileMap void*
#define TERRAXML void*
#define TERRAXMLNode void*
#define TERRAWidget void*
#define TERRASession void*
#define TERRALight void*
#define TERRAUI void*
#define TERRANetMessage void*
#define TERRABillboard void*
#define TERRAParticles void*

typedef void (*TERRAMouseEventCallback) (INT32, INT32, UINT16);
typedef void (*TERRAMouseMoveCallback) (INT32, INT32);
typedef void (*TERRAMouseWheelCallback) (INT32);
typedef void (*TERRAKeyEventCallback) (INT32);
typedef void (*TERRAEventCallback) (void);
typedef void (*TERRAAccelerometerCallback) (float, float, float);
typedef void (*TERRAStateChangeCallback) (INT32);
typedef void (*TERRASceneCallback) ();
typedef void (*TERRAFadeCallback) (void*);
typedef void (*TERRAWidgetEventHandler) (void*);
typedef int (*TERRAPathCostCallback) (INT32, INT32, INT32, INT32);
typedef void (*TERRAVisitNodeCallback) (INT32, INT32);
typedef void (*TERRAErrorCallback) (INT32 ErrorCode);
typedef void (*TERRANetworkCallback) (INT32 Code);
typedef void (*TERRANetworkHandler) (INT32 Owner, TERRAStream Msg);


typedef struct
{
	float x;
	float y;
	float z;
} Vector3D ;

typedef struct
{
	float x;
	float y;
} Vector2D ;

typedef struct
{
	float x;
	float y;
	float z;
	float w;
} Quaternion ;

typedef struct
{
	Vector3D start;
	Vector3D end;
} BoundingBox ;

typedef struct
{
	Vector3D origin;
	Vector3D direction;
} Ray ;

typedef struct
{
	float values[16];
} Matrix4x4;

typedef struct
{
	float values[8];
} Matrix3x3;

typedef struct
{
	UINT8 red;
	UINT8 green;
	UINT8 blue;
	UINT8 alpha;
} Color ;

typedef struct
{
	float a;
	float b;
	float c;
	float d;
} Plane ;

typedef struct {
	Plane planes[6];
	Vector3D vertices[8];
} Frustum ;

typedef Color (*SpriteLightingCallback) (Color, Vector3D);

Color ColorCreate(int red, int green, int blue, int alpha)
{
	Color result;
	result.red = red;
	result.green = green;
	result.blue = blue;
	result.alpha = alpha;
	return result;
}

#ifdef TERRA_DYNAMIC_LOAD
$DYNAMIC_DECLARATIONS

int LoadTERRALibrary() 
{
	HINSTANCE terraDLL = LoadLibrary("terra_engine.dll");  
	if (terraDLL == NULL) return -1;
 
$INITIALIZATION

	return 0;
}
#else

$STATIC_DECLARATIONS

#endif

#ifdef __cplusplus
}
#endif
#endif
