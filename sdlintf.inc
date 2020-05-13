{ Interface to SDL 2 }

type
  Float = Single;
  TSDL_Bool = (SDL_FALSE, SDL_TRUE);
  TWindowShapeMode = (ShapeModeDefault,
                      ShapeModeBinarizeAlpha,
                      ShapeModeReverseBinarizeAlpha,
                      ShapeModeColorKey);

  PSInt32 = ^SInt32;
  SInt32 = Int32;
  PSInt64 = ^SInt64;
  SInt64 = Int64;
  PFloat = ^Float;
  PInt = ^LongInt;

  PSDL_DisplayMode = ^TSDL_DisplayMode;
  PSDL_Color = ^TSDL_Color;
  PSDL_Window = ^TSDL_Window;
  PSDL_Palette = ^TSDL_Palette;
  PSDL_PixelFormat = ^TSDL_PixelFormat;
  PSDL_Point = ^TSDL_Point;
  PSDL_Rect = ^TSDL_Rect;
  PSDL_Renderer = ^TSDL_Renderer;
  PSDL_Surface = ^TSDL_Surface;
  PSDL_Texture = ^TSDL_Texture;
  PSDL_WindowShaper = ^TSDL_WindowShaper;
  PSDL_WindowShapeMode = ^TSDL_WindowShapeMode;
  PSDL_WindowUserData = ^TSDL_WindowUserData;

  TSDL_Renderer = record
  end;

  TSDL_Texture = record
  end;

  PPSDL_Renderer = ^PSDL_Renderer;
  PPSDL_Window = ^PSDL_Window;

  TSDL_Color = record
    r: UInt8;
    g: UInt8;
    b: UInt8;
    a: UInt8;
  end;

  TSDL_Palette = record
    ncolors: SInt32;
    colors: PSDL_Color;
    version: UInt32;
    refcount: SInt32;
  end;

  TSDL_PixelFormat = record
    format: UInt32;
    palette: PSDL_Palette;
    BitsPerPixel: UInt8;
    BytesPerPixel: UInt8;
    padding: array[0..1] of UInt8;
    Rmask: UInt32;
    Gmask: UInt32;
    Bmask: UInt32;
    Amask: UInt32;
    Rloss: UInt8;
    Gloss: UInt8;
    Bloss: UInt8;
    Aloss: UInt8;
    Rshift: UInt8;
    Gshift: UInt8;
    Bshift: UInt8;
    Ashift: UInt8;
    refcount: SInt32;
    next: PSDL_PixelFormat;
  end;


  TSDL_DisplayMode = record
    format: UInt32;
    w: SInt32;
    h: SInt32;
    refresh_rate: SInt32;
    driverdata: Pointer;
  end;

  TSDL_Point = record
    x: SInt32;
    y: SInt32;
  end;

  TSDL_Rect = record
    x,y: SInt32;
    w,h: SInt32;
  end;

  TSDL_WindowShapeParams = record
    case Integer of
      0: (binarizationCutoff: UInt8;);
      1: (colorKey: TSDL_Color;);
  end;

  TSDL_WindowShapeMode = record
    mode: TWindowShapeMode;
    parameters: TSDL_WindowShapeParams;
  end;

  TSDL_WindowShaper = record
    window: PSDL_Window;
    userx,usery: UInt32;
    mode: TSDL_WindowShapeMode;
    hasshape: TSDL_Bool;
    driverdata: Pointer;
  end;

  TSDL_HitTestResult = (
    SDL_HITTEST_NORMAL,
    SDL_HITTEST_DRAGGABLE,
    SDL_HITTEST_RESIZE_TOPLEFT,
    SDL_HITTEST_RESIZE_TOP,
    SDL_HITTEST_RESIZE_TOPRIGHT,
    SDL_HITTEST_RESIZE_RIGHT,
    SDL_HITTEST_RESIZE_BOTTOMRIGHT,
    SDL_HITTEST_RESIZE_BOTTOM,
    SDL_HITTEST_RESIZE_BOTTOMLEFT,
    SDL_HITTEST_RESIZE_LEFT
  );

  TSDL_HitTest = function(win: PSDL_Window; const area: PSDL_Point; data: Pointer): TSDL_HitTestResult; cdecl;

  TSDL_WindowUserData = record
    name: PAnsiChar;
    data: Pointer;
    next: PSDL_WindowUserData;
  end;

  TSDL_Window = record
    magic: Pointer;
    id: UInt32;
    title: PAnsiChar;
    icon: PSDL_Surface;
    x,y: SInt32;
    w,h: SInt32;
    min_w, min_h: SInt32;
    max_w, max_h: SInt32;
    flags: UInt32;
    last_fullscreen_flags: UInt32;
    windowed: TSDL_Rect;
    fullscreen_mode: TSDL_DisplayMode;
    opacity: Float;
    brightness: Float;
    gamma: PUInt16;
    saved_gamma: PUInt16;
    surface: PSDL_Surface;
    surface_valid: TSDL_Bool;
    is_hiding: TSDL_Bool;
    is_destroying: TSDL_Bool;
    is_dropping: TSDL_Bool;
    shaper: PSDL_WindowShaper;
    hit_test: TSDL_HitTest;
    hit_test_data: Pointer;
    data: PSDL_WindowUserData;
    driverdata: Pointer;
    prev: PSDL_Window;
    next: PSDL_Window;
  end;

  TSDL_Surface = record
    flags: UInt32;
    format: PSDL_PixelFormat;
    w, h: SInt32;
    pitch: SInt32;
    pixels: Pointer;
    userdata: Pointer;
    locked: SInt32;
    lock_data: Pointer;
    clip_rect: PSDL_Rect;
    map: Pointer;
    refcount: SInt32;
  end;

  { General functions }
  function SDL_Init(flags: UInt32): SInt32 cdecl; external SDL_LibName;
  procedure SDL_Quit() cdecl; external SDL_LibName;

  { Window }
  function SDL_CreateWindow(const title: PAnsiChar; x: SInt32; y: SInt32; w: SInt32; h: SInt32; flags: UInt32): PSDL_Window cdecl; external SDL_LibName;
  procedure SDL_DestroyWindow(window: PSDL_Window) cdecl; external SDL_LibName;

  { Video }
  function SDL_CreateTextureFromSurface(renderer: PSDL_Renderer; surface: PSDL_Surface): PSDL_Texture cdecl; external SDL_LibName;
  function SDL_CreateRenderer(window: PSDL_Window; index: SInt32; flags: UInt32): PSDL_Renderer cdecl; external SDL_LibName;
  function SDL_RenderCopy(renderer: PSDL_Renderer; texture: PSDL_Texture; srcrect: PSDL_Rect; dstrect: PSDL_Rect): SInt32 cdecl; external SDL_LibName;
  function SDL_RenderCopyEx(renderer: PSDL_Renderer; texture: PSDL_Texture; const srcrect: PSDL_Rect; dstrect: PSDL_Rect; angle: Double; center: PSDL_Point; flip: Integer): SInt32 cdecl; external SDL_LibName;
  function SDL_RenderClear(renderer: PSDL_Renderer): SInt32 cdecl; external SDL_LibName;
  function SDL_SetRenderDrawColor(renderer: PSDL_Renderer; r: UInt8; g: UInt8; b: UInt8; a: UInt8): SInt32 cdecl; external SDL_LibName;
  procedure SDL_DestroyRenderer(renderer: PSDL_Renderer) cdecl; external SDL_LibName;
  procedure SDL_DestroyTexture(texture: PSDL_Texture) cdecl; external SDL_LibName;
  procedure SDL_FreeSurface(surface: PSDL_Surface) cdecl; external SDL_LibName;
  procedure SDL_RenderPresent(renderer: PSDL_Renderer) cdecl; external SDL_LibName;

  { Hinting }
  function SDL_SetHint(const name: PChar; const value: PChar): boolean; cdecl; external SDL_LibName;

  { Images }
  function IMG_Load(_file: PAnsiChar): PSDL_Surface cdecl; external IMG_LibName;

  { Events }
  procedure SDL_PumpEvents cdecl; external SDL_LibName;
  function SDL_GetKeyboardState(numkeys: PInt): PUInt8 cdecl; external SDL_LibName;

  { Timer }
  procedure SDL_Delay(ms: UInt32) cdecl; external SDL_LibName;
