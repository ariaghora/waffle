const
  SDL_LibName = 'SDL2.dll';
  IMG_LibName = 'SDL2_image.dll';
  TTF_LibName = 'SDL2_ttf.dll';

  SDL_INIT_TIMER          = $00000001;
  {$EXTERNALSYM SDL_INIT_TIMER}
  SDL_INIT_AUDIO          = $00000010;
  {$EXTERNALSYM SDL_INIT_AUDIO}
  SDL_INIT_VIDEO          = $00000020;   // SDL_INIT_VIDEO implies SDL_INIT_EVENTS
  {$EXTERNALSYM SDL_INIT_VIDEO}
  SDL_INIT_JOYSTICK       = $00000200;   // SDL_INIT_JOYSTICK implies SDL_INIT_EVENTS
  {$EXTERNALSYM SDL_INIT_JOYSTICK}
  SDL_INIT_HAPTIC         = $00001000;
  {$EXTERNALSYM SDL_INIT_HAPTIC}
  SDL_INIT_GAMECONTROLLER = $00002000;  //turn on game controller also implicitly does JOYSTICK
  {$EXTERNALSYM SDL_INIT_GAMECONTROLLER}// SDL_INIT_GAMECONTROLLER implies SDL_INIT_JOYSTICK
  SDL_INIT_EVENTS         = $00004000;
  {$EXTERNALSYM SDL_INIT_EVENTS}
  SDL_INIT_NOPARACHUTE    = $00100000;  //Don't catch fatal signals
  {$EXTERNALSYM SDL_INIT_NOPARACHUTE}   // compatibility; this flag is ignored.
  SDL_INIT_EVERYTHING     = SDL_INIT_TIMER    or
                              SDL_INIT_AUDIO    or
                              SDL_INIT_VIDEO    or
                              SDL_INIT_EVENTS   or
                              SDL_INIT_JOYSTICK or
                              SDL_INIT_HAPTIC   or
                              SDL_INIT_GAMECONTROLLER;
  {$EXTERNALSYM SDL_INIT_EVERYTHING}

  SDL_WINDOW_FULLSCREEN = $00000001;         {**< fullscreen window *}
  SDL_WINDOW_OPENGL = $00000002;             {**< window usable with OpenGL context *}
  SDL_WINDOW_SHOWN = $00000004;              {**< window is visible *}
  SDL_WINDOW_HIDDEN = $00000008;             {**< window is not visible *}
  SDL_WINDOW_BORDERLESS = $00000010;         {**< no window decoration *}
  SDL_WINDOW_RESIZABLE = $00000020;          {**< window can be resized *}
  SDL_WINDOW_MINIMIZED = $00000040;          {**< window is minimized *}
  SDL_WINDOW_MAXIMIZED = $00000080;          {**< window is maximized *}
  SDL_WINDOW_INPUT_GRABBED = $00000100;      {**< window has grabbed input focus *}
  SDL_WINDOW_INPUT_FOCUS = $00000200;        {**< window has input focus *}
  SDL_WINDOW_MOUSE_FOCUS = $00000400;        {**< window has mouse focus *}
  SDL_WINDOW_FULLSCREEN_DESKTOP = SDL_WINDOW_FULLSCREEN or $00001000;
  SDL_WINDOW_FOREIGN = $00000800;            {**< window not created by SDL *}
  SDL_WINDOW_ALLOW_HIGHDPI = $00002000;      {**< window should be created in high-DPI mode if supported.
                                                   On macOS NSHighResolutionCapable must be set true in the
                                                   application's Info.plist for this to have any effect. *}
  SDL_WINDOW_MOUSE_CAPTURE = $00004000;      {**< window has mouse captured (unrelated to INPUT_GRABBED) *}
  SDL_WINDOW_ALWAYS_ON_TOP = $00008000;      {**< window should always be above others *}
  SDL_WINDOW_SKIP_TASKBAR  = $00010000;      {**< window should not be added to the taskbar *}
  SDL_WINDOW_UTILITY       = $00020000;      {**< window should be treated as a utility window *}
  SDL_WINDOW_TOOLTIP       = $00040000;      {**< window should be treated as a tooltip *}
  SDL_WINDOW_POPUP_MENU    = $00080000;      {**< window should be treated as a popup menu *}
  SDL_WINDOW_VULKAN        = $10000000;      {**< window usable for Vulkan surface *}

  { SDL inputs }
  SDL_RELEASED         = 0;
  SDL_PRESSED          = 1;

  SDL_FIRSTEVENT       = 0;     // Unused (do not remove) (needed in pascal?)

  SDL_COMMONEVENT      = 1;     //added for pascal-compatibility

  { Application events }
  SDL_QUITEV           = $100;  // User-requested quit (originally SDL_QUIT, but changed, cause theres a method called SDL_QUIT)


  {* These application events have special meaning on iOS, see README.iOS for details *}
  SDL_APP_TERMINATING  = $101;   {**< The application is being terminated by the OS
                                      Called on iOS in applicationWillTerminate()
                                      Called on Android in onDestroy()
                                  *}
  SDL_APP_LOWMEMORY    = $102;   {**< The application is low on memory, free memory if possible.
                                      Called on iOS in applicationDidReceiveMemoryWarning()
                                      Called on Android in onLowMemory()
                                  *}
  SDL_APP_WILLENTERBACKGROUND = $103; {**< The application is about to enter the background
                                           Called on iOS in applicationWillResignActive()
                                           Called on Android in onPause()
                                       *}
  SDL_APP_DIDENTERBACKGROUND = $104;  {**< The application did enter the background and may not get CPU for some time
                                           Called on iOS in applicationDidEnterBackground()
                                           Called on Android in onPause()
                                       *}
  SDL_APP_WILLENTERFOREGROUND = $105; {**< The application is about to enter the foreground
                                           Called on iOS in applicationWillEnterForeground()
                                           Called on Android in onResume()
                                       *}
  SDL_APP_DIDENTERFOREGROUND = $106;  {**< The application is now interactive
                                           Called on iOS in applicationDidBecomeActive()
                                           Called on Android in onResume()
                                       *}

  { Window events }
  SDL_WINDOWEVENT      = $200;  // Window state change
  SDL_SYSWMEVENT       = $201;  // System specific event

  { Keyboard events }
  SDL_KEYDOWN          = $300;  // Key pressed
  SDL_KEYUP            = $301;  // Key released
  SDL_TEXTEDITING      = $302;  // Keyboard text editing (composition)
  SDL_TEXTINPUT        = $303;  // Keyboard text input
  SDL_KEYMAPCHANGED    = $304;  // Keymap changed due to a system event such as an
                                // input language or keyboard layout change.

  { Mouse events }
  SDL_MOUSEMOTION      = $400;  // Mouse moved
  SDL_MOUSEBUTTONDOWN  = $401;  // Mouse button pressed
  SDL_MOUSEBUTTONUP    = $402;  // Mouse button released
  SDL_MOUSEWHEEL       = $403;  // Mouse wheel motion

  { Joystick events }
  SDL_JOYAXISMOTION    = $600;  // Joystick axis motion
  SDL_JOYBALLMOTION    = $601;  // Joystick trackball motion
  SDL_JOYHATMOTION     = $602;  // Joystick hat position change
  SDL_JOYBUTTONDOWN    = $603;  // Joystick button pressed
  SDL_JOYBUTTONUP      = $604;  // Joystick button released
  SDL_JOYDEVICEADDED   = $605;  // A new joystick has been inserted into the system
  SDL_JOYDEVICEREMOVED = $606;  // An opened joystick has been removed

  { Game controller events }
  SDL_CONTROLLERAXISMOTION     = $650;  // Game controller axis motion
  SDL_CONTROLLERBUTTONDOWN     = $651;  // Game controller button pressed
  SDL_CONTROLLERBUTTONUP       = $652;  // Game controller button released
  SDL_CONTROLLERDEVICEADDED    = $653;  // A new Game controller has been inserted into the system
  SDL_CONTROLLERDEVICEREMOVED  = $654;  // An opened Game controller has been removed
  SDL_CONTROLLERDEVICEREMAPPED = $655;  // The controller mapping was updated

  { Touch events }
  SDL_FINGERDOWN      = $700;
  SDL_FINGERUP        = $701;
  SDL_FINGERMOTION    = $702;

  { Gesture events }
  SDL_DOLLARGESTURE   = $800;
  SDL_DOLLARRECORD    = $801;
  SDL_MULTIGESTURE    = $802;

  { Clipboard events }
  SDL_CLIPBOARDUPDATE = $900; // The clipboard changed

  { Drag and drop events }
  SDL_DROPFILE        = $1000; // The system requests a file open
  SDL_DROPTEXT        = $1001; // text/plain drag-and-drop event
  SDL_DROPBEGIN       = $1002; // A new set of drops is beginning (NULL filename)
  SDL_DROPCOMPLETE    = $1003; // Current set of drops is now complete (NULL filename)

  { Audio hotplug events }
  SDL_AUDIODEVICEADDED     = $1100; // A new audio device is available
  SDL_AUDIODEVICEREMOVED   = $1101; // An audio device has been removed.

  { Render events }
  SDL_RENDER_TARGETS_RESET = $2000; // The render targets have been reset
  SDL_RENDER_DEVICE_RESET  = $2001; // The device has been reset and all textures need to be recreated

  {** Events SDL_USEREVENT through SDL_LASTEVENT are for your use,
   *  and should be allocated with SDL_RegisterEvents()
   *}
  SDL_USEREVENT    = $8000;

  {**
   *  This last event is only for bounding internal arrays (needed in pascal ??)
   *}
  SDL_LASTEVENT    = $FFFF;

  { SDL hints }
  SDL_HINT_RENDER_SCALE_QUALITY = 'SDL_RENDER_SCALE_QUALITY';

  SDL_FLIP_NONE       = $0; { Do not flip }
  SDL_FLIP_HORIZONTAL = $1; { flip horizontally }
  SDL_FLIP_VERTICAL   = $2; { flip vertically }

  SDL_ALPHA_OPAQUE = 255;
  SDL_ALPHA_TRANSPARENT = 0;

  { Font }
  TTF_HINTING_NORMAL  = 0;
  TTF_HINTING_LIGHT   = 1;
  TTF_HINTING_MONO    = 2;
  TTF_HINTING_NONE    = 3;

