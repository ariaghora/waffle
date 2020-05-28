{ Interface to SDL 2 }

type
  Float = Single;
  TSDL_Bool = (SDL_FALSE, SDL_TRUE);
  TWindowShapeMode = (ShapeModeDefault,
                      ShapeModeBinarizeAlpha,
                      ShapeModeReverseBinarizeAlpha,
                      ShapeModeColorKey);

  PSInt16 = ^SInt16;
  SInt16 = smallint;
  PSInt32 = ^SInt32;
  SInt32 = Int32;
  PSInt64 = ^SInt64;
  SInt64 = Int64;
  PFloat = ^Float;
  PInt = ^LongInt;

  PSDL_BlendMode = ^TSDL_BlendMode;
  TSDL_BlendMode = DWord;

  PSDL_DisplayMode = ^TSDL_DisplayMode;
  PSDL_Color = ^TSDL_Color;
  PSDL_Event = ^TSDL_Event;
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
  PTTF_Font = ^TTTF_Font;

  TSDL_Renderer = record
  end;

  TSDL_Texture = record
  end;

  TTTF_Font = record
  end;

  PPSDL_Renderer = ^PSDL_Renderer;
  PPSDL_Window = ^PSDL_Window;

  PSDL_Version = ^TSDL_Version;
  TSDL_Version = record
    major,         {**< major version *}
    minor,         {**< minor version *}
    patch: UInt8;  {**< update version *}
  end;

  TSDL_SYSWM_TYPE = (
      SDL_SYSWM_UNKNOWN,
      SDL_SYSWM_WINDOWS,
      SDL_SYSWM_X11,
      SDL_SYSWM_DIRECTFB,
      SDL_SYSWM_COCOA,
      SDL_SYSWM_UIKIT,
      SDL_SYSWM_WAYLAND, // Since SDL 2.0.2
      SDL_SYSWM_MIR,     // Since SDL 2.0.2
      SDL_SYSWM_WINRT,   // Since SDL 2.0.3
      SDL_SYSWM_ANDROID, // Since SDL 2.0.4
      SDL_SYSWM_VIVANTE  // Since SDL 2.0.5
   );

  PSDL_SysWMmsg = ^TSDL_SysWMmsg;
  TSDL_SysWMmsg = record
      version: TSDL_version;
      subsystem: TSDL_SYSWM_TYPE;
      {$IFDEF SDL_VIDEO_DRIVER_WINDOWS}
        win: __SYSWM_WINDOWS;
      {$ELSE} {$IFDEF SDL_VIDEO_DRIVER_X11}
        x11: __SYSWM_X11;
      {$ELSE} {$IFDEF SDL_VIDEO_DRIVER_DIRECTFB}
        dfb: __SYSWM_DIRECTFB;
      {$ELSE} {$IFDEF SDL_VIDEO_DRIVER_COCOA}
        cocoa: __SYSWM_COCOA;
      {$ELSE} {$IFDEF SDL_VIDEO_DRIVER_UIKIT}
        uikit: __SYSWM_UIKIT;
      {$ELSE} {$IFDEF SDL_VIDEO_DRIVER_VIVANTE}
        vivante: __SYSWM_VIVANTE;
      {$ELSE}
        (* Cannot have empty record case *)
        dummy: integer;
      {$ENDIF} {$ENDIF} {$ENDIF} {$ENDIF} {$ENDIF} {$ENDIF}
   end;

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

  TSDL_TextInputEvent = record
    type_: UInt32;                                          // SDL_TEXTINPUT
    timestamp: UInt32;
    windowID: UInt32;                                       // The window with keyboard focus, if any
    text: array[0..32] of Char;   // The input text
  end;

  {**
   *  Mouse motion event structure (event.motion.*)
   *}

  TSDL_MouseMotionEvent = record
    type_: UInt32;       // SDL_MOUSEMOTION
    timestamp: UInt32;
    windowID: UInt32;    // The window with mouse focus, if any
    which: UInt32;       // The mouse instance id, or SDL_TOUCH_MOUSEID
    state: UInt8;        // The current button state
    padding1: UInt8;
    padding2: UInt8;
    padding3: UInt8;
    x: SInt32;           // X coordinate, relative to window
    y: SInt32;           // Y coordinate, relative to window
    xrel: SInt32;        // The relative motion in the X direction
    yrel: SInt32;        // The relative motion in the Y direction
  end;

  {**
   *  Mouse button event structure (event.button.*)
   *}

  TSDL_MouseButtonEvent = record
    type_: UInt32;       // SDL_MOUSEBUTTONDOWN or SDL_MOUSEBUTTONUP
    timestamp: UInt32;
    windowID: UInt32;    // The window with mouse focus, if any
    which: UInt32;       // The mouse instance id, or SDL_TOUCH_MOUSEID
    button: UInt8;       // The mouse button index
    state: UInt8;        // SDL_PRESSED or SDL_RELEASED
    clicks: UInt8;       // 1 for single-click, 2 for double-click, etc.
    padding1: UInt8;
    x: SInt32;           // X coordinate, relative to window
    y: SInt32;           // Y coordinate, relative to window
  end;

  {**
   *  Mouse wheel event structure (event.wheel.*)
   *}

  TSDL_MouseWheelEvent = record
    type_: UInt32;        // SDL_MOUSEWHEEL
    timestamp: UInt32;
    windowID: UInt32;    // The window with mouse focus, if any
    which: UInt32;       // The mouse instance id, or SDL_TOUCH_MOUSEID
    x: SInt32;           // The amount scrolled horizontally
    y: SInt32;           // The amount scrolled vertically
    direction: UInt32;   // Set to one of the SDL_MOUSEWHEEL_* defines. When FLIPPED the values in X and Y will be opposite. Multiply by -1 to change them back
  end;

  {**
   *  Joystick axis motion event structure (event.jaxis.*)
   *}

  TSDL_JoystickID = SInt32;
  TSDL_JoyAxisEvent = record
    type_: UInt32;         // SDL_JOYAXISMOTION
    timestamp: UInt32;
    which: TSDL_JoystickID; // The joystick instance id
    axis: UInt8;           // The joystick axis index
    padding1: UInt8;
    padding2: UInt8;
    padding3: UInt8;
    value: SInt16;         // The axis value (range: -32768 to 32767)
    padding4: UInt16;
  end;

  {**
   *  Joystick trackball motion event structure (event.jball.*)
   *}

  TSDL_JoyBallEvent = record
    type_: UInt32;         // SDL_JOYBALLMOTION
    timestamp: UInt32;
    which: TSDL_JoystickID; // The joystick instance id
    ball: UInt8;           // The joystick trackball index
    padding1: UInt8;
    padding2: UInt8;
    padding3: UInt8;
    xrel: SInt16;          // The relative motion in the X direction
    yrel: SInt16;          // The relative motion in the Y direction
  end;

  {**
   *  Joystick hat position change event structure (event.jhat.*)
   *}

  TSDL_JoyHatEvent = record
    type_: UInt32;         // SDL_JOYHATMOTION
    timestamp: UInt32;
    which: TSDL_JoystickID; // The joystick instance id
    hat: UInt8;            // The joystick hat index
    value: UInt8;         {*  The hat position value.
                           *  SDL_HAT_LEFTUP   SDL_HAT_UP       SDL_HAT_RIGHTUP
                           *  SDL_HAT_LEFT     SDL_HAT_CENTERED SDL_HAT_RIGHT
                           *  SDL_HAT_LEFTDOWN SDL_HAT_DOWN     SDL_HAT_RIGHTDOWN
                           *
                           *  Note that zero means the POV is centered.
                           *}
    padding1: UInt8;
    padding2: UInt8;
  end;

  {**
   *  Joystick button event structure (event.jbutton.*)
   *}

  TSDL_JoyButtonEvent = record
    type_: UInt32;        // SDL_JOYBUTTONDOWN or SDL_JOYBUTTONUP
    timestamp: UInt32;
    which: TSDL_JoystickID; // The joystick instance id
    button: UInt8;         // The joystick button index
    state: UInt8;          // SDL_PRESSED or SDL_RELEASED
    padding1: UInt8;
    padding2: UInt8;
  end;

  {**
   *  Joystick device event structure (event.jdevice.*)
   *}

  TSDL_JoyDeviceEvent = record
    type_: UInt32;      // SDL_JOYDEVICEADDED or SDL_JOYDEVICEREMOVED
    timestamp: UInt32;
    which: SInt32;      // The joystick device index for the ADDED event, instance id for the REMOVED event
  end;

  {**
   *  Game controller axis motion event structure (event.caxis.*)
   *}

  TSDL_ControllerAxisEvent = record
    type_: UInt32;         // SDL_CONTROLLERAXISMOTION
    timestamp: UInt32;
    which: TSDL_JoystickID; // The joystick instance id
    axis: UInt8;           // The controller axis (SDL_GameControllerAxis)
    padding1: UInt8;
    padding2: UInt8;
    padding3: UInt8;
    value: SInt16;         // The axis value (range: -32768 to 32767)
    padding4: UInt16;
  end;

  {**
   *  Game controller button event structure (event.cbutton.*)
   *}

  TSDL_ControllerButtonEvent = record
    type_: UInt32;         // SDL_CONTROLLERBUTTONDOWN or SDL_CONTROLLERBUTTONUP
    timestamp: UInt32;
    which: TSDL_JoystickID; // The joystick instance id
    button: UInt8;         // The controller button (SDL_GameControllerButton)
    state: UInt8;          // SDL_PRESSED or SDL_RELEASED
    padding1: UInt8;
    padding2: UInt8;
  end;


  {**
   *  Controller device event structure (event.cdevice.*)
   *}

  TSDL_ControllerDeviceEvent = record
    type_: UInt32;       // SDL_CONTROLLERDEVICEADDED, SDL_CONTROLLERDEVICEREMOVED, or SDL_CONTROLLERDEVICEREMAPPED
    timestamp: UInt32;
    which: SInt32;       // The joystick device index for the ADDED event, instance id for the REMOVED or REMAPPED event
  end;

  {**
   *  Audio device event structure (event.adevice.*)
   *}

  TSDL_AudioDeviceEvent = record
    type_: UInt32;        // ::SDL_AUDIODEVICEADDED, or ::SDL_AUDIODEVICEREMOVED
    timestamp: UInt32;
    which: UInt32;        // The audio device index for the ADDED event (valid until next SDL_GetNumAudioDevices() call), SDL_AudioDeviceID for the REMOVED event
    iscapture: UInt8;     // zero if an output device, non-zero if a capture device.
    padding1: UInt8;
    padding2: UInt8;
    padding3: UInt8;
  end;


  {**
   *  Touch finger event structure (event.tfinger.*)
   *}

  TSDL_TouchID = SInt32;
  TSDL_FingerID = SInt32;
  TSDL_TouchFingerEvent = record
    type_: UInt32;         // SDL_FINGERMOTION or SDL_FINGERDOWN or SDL_FINGERUP
    timestamp: UInt32;
    touchId: TSDL_TouchID;  // The touch device id
    fingerId: TSDL_FingerID;
    x: Float;              // Normalized in the range 0...1
    y: Float;              // Normalized in the range 0...1
    dx: Float;             // Normalized in the range 0...1
    dy: Float;             // Normalized in the range 0...1
    pressure: Float;       // Normalized in the range 0...1
  end;

  {**
   *  Multiple Finger Gesture Event (event.mgesture.*)
   *}
  TSDL_MultiGestureEvent = record
    type_: UInt32;        // SDL_MULTIGESTURE
    timestamp: UInt32;
    touchId: TSDL_TouchID; // The touch device index
    dTheta: Float;
    dDist: Float;
    x: Float;
    y: Float;
    numFingers: UInt16;
    padding: UInt16;
  end;


  {* (event.dgesture.*) *}
  TSDL_GestureID = SInt32;
  TSDL_DollarGestureEvent = record
    type_: UInt32;         // SDL_DOLLARGESTURE
    timestamp: UInt32;
    touchId: TSDL_TouchID;  // The touch device id
    gestureId: TSDL_GestureID;
    numFingers: UInt32;
    error: Float;
    x: Float;              // Normalized center of gesture
    y: Float;              // Normalized center of gesture
  end;


  {**
   *  An event used to request a file open by the system (event.drop.*)
   *  This event is disabled by default, you can enable it with SDL_EventState()
   *  If you enable this event, you must free the filename in the event.
   *}

  TSDL_DropEvent = record
    type_: UInt32;      // SDL_DROPFILE
    timestamp: UInt32;
    _file: PAnsiChar;   // The file name, which should be freed with SDL_free()
  end;

  {**
   *  The "quit requested" event
   *}

  TSDL_QuitEvent = record
    type_: UInt32;        // SDL_QUIT
    timestamp: UInt32;
  end;

  {**
   *  A user-defined event type (event.user.*)
   *}

  TSDL_UserEvent = record
    type_: UInt32;       // SDL_USEREVENT through SDL_NUMEVENTS-1
    timestamp: UInt32;
    windowID: UInt32;    // The associated window if any
    code: SInt32;        // User defined event code
    data1: Pointer;      // User defined data pointer
    data2: Pointer;      // User defined data pointer
  end;

  {**
   *  A video driver dependent system event (event.syswm.*)
   *  This event is disabled by default, you can enable it with SDL_EventState()
   *
   *  If you want to use this event, you should include SDL_syswm.h.
   *}

  PSDL_SysWMEvent = ^TSDL_SysWMEvent;
  TSDL_SysWMEvent = record
    type_: UInt32;       // SDL_SYSWMEVENT
    timestamp: UInt32;
    msg: PSDL_SysWMmsg;  // driver dependent data (defined in SDL_syswm.h)
  end;

  {**
   *  General event structure
   *}

  TSDL_CommonEvent = record
    type_: UInt32;
    timestamp: UInt32;
  end;

  TSDL_WindowEvent = record
    type_: UInt32;       // SDL_WINDOWEVENT
    timestamp: UInt32;
    windowID: UInt32;    // The associated window
    event: UInt8;        // SDL_WindowEventID
    padding1: UInt8;
    padding2: UInt8;
    padding3: UInt8;
    data1: SInt32;       // event dependent data
    data2: SInt32;       // event dependent data
  end;

  PSDL_ScanCode = ^TSDL_ScanCode;
  TSDL_ScanCode = DWord;
  PSDL_KeyCode = ^TSDL_KeyCode;
  TSDL_KeyCode = SInt32;
  PSDL_Keysym = ^TSDL_Keysym;

  TSDL_Keysym = record
    scancode: TSDL_ScanCode;      // SDL physical key code - see SDL_Scancode for details
    sym: TSDL_KeyCode;            // SDL virtual key code - see SDL_Keycode for details
    _mod: UInt16;                 // current key modifiers
    unicode: UInt32;              // (deprecated) use SDL_TextInputEvent instead
  end;

  TSDL_KeyboardEvent = record
    type_: UInt32;        // SDL_KEYDOWN or SDL_KEYUP
    timestamp: UInt32;
    windowID: UInt32;     // The window with keyboard focus, if any
    state: UInt8;         // SDL_PRESSED or SDL_RELEASED
    _repeat: UInt8;       // Non-zero if this is a key repeat
    padding2: UInt8;
    padding3: UInt8;
    keysym: TSDL_KeySym;  // The key that was pressed or released
  end;

  TSDL_TextEditingEvent = record
    type_: UInt32;                               // SDL_TEXTEDITING
    timestamp: UInt32;
    windowID: UInt32;                            // The window with keyboard focus, if any
    text: array[0..32] of Char;  // The editing text
    start: SInt32;                               // The start cursor of selected editing text
    length: SInt32;                              // The length of selected editing text
  end;

  TSDL_Event = record
    case Integer of
      0:  (type_: UInt32);

      SDL_COMMONEVENT:  (common: TSDL_CommonEvent);
      SDL_WINDOWEVENT:  (window: TSDL_WindowEvent);

      SDL_KEYUP,
      SDL_KEYDOWN:  (key: TSDL_KeyboardEvent);
      SDL_TEXTEDITING:  (edit: TSDL_TextEditingEvent);
      SDL_TEXTINPUT:  (text: TSDL_TextInputEvent);

      SDL_MOUSEMOTION:  (motion: TSDL_MouseMotionEvent);
      SDL_MOUSEBUTTONUP,
      SDL_MOUSEBUTTONDOWN:  (button: TSDL_MouseButtonEvent);
      SDL_MOUSEWHEEL:  (wheel: TSDL_MouseWheelEvent);

      SDL_JOYAXISMOTION:  (jaxis: TSDL_JoyAxisEvent);
      SDL_JOYBALLMOTION: (jball: TSDL_JoyBallEvent);
      SDL_JOYHATMOTION: (jhat: TSDL_JoyHatEvent);
      SDL_JOYBUTTONDOWN,
      SDL_JOYBUTTONUP: (jbutton: TSDL_JoyButtonEvent);
      SDL_JOYDEVICEADDED,
      SDL_JOYDEVICEREMOVED: (jdevice: TSDL_JoyDeviceEvent);

      SDL_CONTROLLERAXISMOTION: (caxis: TSDL_ControllerAxisEvent);
      SDL_CONTROLLERBUTTONUP,
      SDL_CONTROLLERBUTTONDOWN: (cbutton: TSDL_ControllerButtonEvent);
      SDL_CONTROLLERDEVICEADDED,
      SDL_CONTROLLERDEVICEREMOVED,
      SDL_CONTROLLERDEVICEREMAPPED: (cdevice: TSDL_ControllerDeviceEvent);

      SDL_AUDIODEVICEADDED,
      SDL_AUDIODEVICEREMOVED: (adevice: TSDL_AudioDeviceEvent);

      SDL_QUITEV: (quit: TSDL_QuitEvent);

      SDL_USEREVENT: (user: TSDL_UserEvent);
      SDL_SYSWMEVENT: (syswm: TSDL_SysWMEvent);

      SDL_FINGERDOWN,
      SDL_FINGERUP,
      SDL_FINGERMOTION: (tfinger: TSDL_TouchFingerEvent);
      SDL_MULTIGESTURE: (mgesture: TSDL_MultiGestureEvent);
      SDL_DOLLARGESTURE,SDL_DOLLARRECORD: (dgesture: TSDL_DollarGestureEvent);

      SDL_DROPFILE: (drop: TSDL_DropEvent);
  end;

  TSDL_TimerCallback = function(interval: UInt32; param: Pointer): UInt32; cdecl;

  { General functions }
  function SDL_Init(flags: UInt32): SInt32 cdecl; external SDL_LibName;
  procedure SDL_Quit() cdecl; external SDL_LibName;

  { Window }
  function SDL_CreateWindow(const title: PAnsiChar; x: SInt32; y: SInt32; w: SInt32; h: SInt32; flags: UInt32): PSDL_Window cdecl; external SDL_LibName;
  procedure SDL_DestroyWindow(window: PSDL_Window) cdecl; external SDL_LibName;
  procedure SDL_SetWindowFullscreen(window: PSDL_Window; flags: UInt32) cdecl; external SDL_LibName;
  procedure SDL_SetWindowPosition(window: PSDL_Window; x, y: SInt32) cdecl; external SDL_LibName;
  procedure SDL_SetWindowSize(window: PSDL_Window; w, h: SInt32) cdecl; external SDL_LibName;
  procedure SDL_SetWindowTitle(window: PSDL_Window; title: PChar) cdecl; external SDL_LibName;

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
  function SDL_SetRenderDrawBlendMode(renderer: PSDL_Renderer; blendMode: TSDL_BlendMode): SInt32 cdecl; external SDL_LibName;
  procedure SDL_SetTextureAlphaMod(texture: PSDL_Texture; alpha: UInt8) cdecl; external SDL_LibName;

  { Hinting }
  function SDL_SetHint(const name: PChar; const value: PChar): boolean; cdecl; external SDL_LibName;

  { Images }
  function IMG_Load(_file: PAnsiChar): PSDL_Surface cdecl; external IMG_LibName;

  { Events }
  function SDL_GetKeyboardState(numkeys: PInt): PUInt8 cdecl; external SDL_LibName;
  function SDL_PollEvent(event: PSDL_Event): SInt32 cdecl; external SDL_LibName;
  function SDL_WaitEvent(event: PSDL_Event): SInt32 cdecl; external SDL_LibName;
  procedure SDL_PumpEvents cdecl; external SDL_LibName;

  { Keyboard }
  function SDL_GetKeyName(key: TSDL_Keycode): PChar cdecl; external SDL_LibName;

  { Timer }
  function SDL_AddTimer(interval: UInt32; callback: TSDL_TimerCallback; param: Pointer): UInt32 cdecl; external SDL_LibName;
  function SDL_GetPerformanceCounter: UInt64 cdecl; external SDL_LibName;
  function SDL_GetPerformanceFrequency: UInt64 cdecl; external SDL_LibName;
  function SDL_GetTicks: UInt32 cdecl; external SDL_LibName;
  function SDL_RemoveTimer(TimerID: UInt32): TSDL_Bool cdecl; external SDL_LibName;
  procedure SDL_Delay(ms: UInt32) cdecl; external SDL_LibName;

  { Font }
  function TTF_Init(): Integer cdecl; external TTF_LibName;
  function TTF_OpenFont(_file: PAnsiChar; ptsize: Integer): PTTF_Font cdecl; external TTF_LibName;
  function TTF_RenderText_Shaded(font: PTTF_Font; text: PAnsiChar; fg, bg: TSDL_Color): PSDL_Surface cdecl; external TTF_LibName;
  function TTF_RenderText_Solid(font: PTTF_Font; text: PAnsiChar; fg: TSDL_Color): PSDL_Surface cdecl; external TTF_LibName;
  procedure TTF_SetFontHinting(font: PTTF_Font; hinting: Integer) cdecl; external TTF_LibName;
  procedure TTF_SetFontOutline(font: PTTF_Font; outline: Integer) cdecl; external TTF_LibName;
  function TTF_SizeText(font: PTTF_Font; text: PAnsiChar; w, h: PInt): Integer cdecl; external TTF_LibName;

  { Primitive drawing }
  function SDL_RenderFillRect(renderer: PSDL_Renderer; rect: PSDL_Rect): UInt64 cdecl; external SDL_LibName;

