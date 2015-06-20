TERRA Game Engine
============

_TERRA_ is a cross platform game engine written in Object Pascal.
While compatible with Lazarus and Delphi, it does not use visual components.
The engine was developed with focus on performance and optimized to run on mobile devices.
Does not require any external library or framework (except linking to OpenGL and OpenAL).

* Author: [Sérgio Flores](https://github.com/relfos)
* License: [APACHE](http://opensource.org/licenses/Apache-2.0)
* [Reporting Issues](https://github.com/relfos/terra_engine/issues)
* Support can be obtained via [Email](mailto:admin@minimon3d.com)
* If you require some specific feature please contact for a quote.

[![Support via Paypal](https://www.paypalobjects.com/en_US/i/btn/btn_donateCC_LG.gif)](https://www.paypal.com/cgi-bin/webscr?cmd=_s-xclick&hosted_button_id=MQ7UQ6V92PZ8L)

What is supported?
----------------

Compilers:
* FPC 2.6.2 and up
* Delphi 7

Platforms:
* Windows
* Linux
* OSX
* iOS
* Android
* OUYA

Renderers:
* OpenGL (both shader-based and fixed pipeline)
* OpenGL ES

File formats
----------------

Images:
* PNG
* JPG
* BMP
* TGA
* DDS
* PSD
* GIF (including animation)
* PVR (Android)

Fonts:
* TTF
* FNT (Angelcode)

Audio:
* WAV
* OGG
* MOD
* MIDI 
* MP3 (Windows/Android/iOS only) 

3D Models:
* OBJ
* MS3D (Milkshape)
* SMD (Valve)
* 3DS
* Collada
* GLS ([g]iles)
* X (Microsoft DirectX)

Core Features
----------------
* SIMD optimized math library (Vectors, Matrix, Colors, Rays, etc)
* Full cross platform abstraction (write once, compile everywhere)
* Automatic resource management with lazy loading/multithreading (Textures, Meshes, Sounds, Animations, etc)
* Logging with advanced options (filtering, console output)
* Custom memory manager
* Input management (keyboard, mouse, touch, gamepad)
* Thread management system (tasks, etc)
* Image manipulation framework (resize, crop, etc)
* 2D/3D sound system based on OpenAL
* Can be used standalone or integrated into a Delphi/Lazarus form
* Localization system (with UTF8 support)
* In-app purchase integration (iOS and Android)
* Steam integration

Renderer Features
----------------
* Optimized sprite rendering with batching
* Mesh skinning with skeletal animation (both on GPU and CPU)
* Inverse kinematics animation
* Mesh dynamic deformation system (water, explosions, cloth rendering, etc)
* Uber shader system (forward and deferred rendering)
* Shadows (stencil-based and shadowmaps)
* Particle systems (sprites, lines and meshes)
* Dynamic light system with culling (infinite number of lights per scene, supports point, directional and spot)
* Infinite reflective surfaces (water, mirrors, etc)
* Water shading with flow map support
* Texture palettes/color grading (both meshes and sprites)
* Animated textures (GIF or video based)
* Screen based effects (color correction, outlines, refractions, SSAO, etc)
* Font rendering (including density field fonts)
* Billboards and decals
* Fur rendering
* Terrain rendering
* Voxel/metaballs rendering
* Software rasterization support
* Lightmapping support (both rendering and generation)
* Cartoon/celshading support
* Skyboxes, skydomes and volumetric cloud rendering
* Standard solids procedural meshes
* Spline based mesh extrusions
* Optimized 2D sprite rendering (including tilemaps from Tiled map editor)

GUI Features
----------------
* Optimized batched rendering
* Standard widgets (buttons, labels, images, comboboxes, etc)
* Widget animation (colors, opacity, size, rotation, etc)
* Custom virtual keyboard (supports Russian, Korean, Japonese and Chinese)
* Transition effects (fade out, slides, etc)

Artificial Inteligence
----------------
* Path finding
* Boids
* Navigation mesh

Networking features
----------------
* Portable sockets API
* Client and Server for multiplayer
* Support for HTTP and FTP protocols 
* UPNP support for NAT punchthrough

Integrated Mobile SDKs 
---------------------
* Flurry
* iAd
* Admob
* Tapjoy
* Fortumo
* Chartboost
* Adbuddiz
* Vungle

Projects using TERRA
---------------------
* Minimon (http://minimon3d.com)
