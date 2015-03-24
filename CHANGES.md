Engine/API Changes

February 2015

TERRA_Unicode -> Replaced with TERRA_String
TERRA_IO -> Replaced with TERRA_Stream and TERRA_MemoryStream
TERRA_FileIO -> Replaced with TERRA_FileStream
SpriteManager.Instance.AddSprite ->  SpriteManager.Instance.DrawSprite
GraphicsManager.Instance.BackgroundColor -> GraphicsManager.Instance.ActiveViewport.BackgroundColor
Keys -> No longer array, now class with methods isDown(), wasPressed(), wasReleased() etc

March 2015

All Destructors -> Replaced with Release() method (in preparation for Oxygene support)
FreeAndNil() -> Replaced with ReleaseObject()
Added PhysicsManager class, which works as generic front end to physics engines (basic Newton physics engine support)

Added InputManager class, all input now goes through this class

Changed NetClient and NetServer, now they use message handlers with a different signature