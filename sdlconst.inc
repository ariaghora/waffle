const
  SDL_LibName = 'SDL2.dll';
  IMG_LibName = 'SDL2_image.dll';

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

  { SDL hints }
  SDL_HINT_RENDER_SCALE_QUALITY = 'SDL_RENDER_SCALE_QUALITY';

  SDL_FLIP_NONE       = $0; { Do not flip }
  SDL_FLIP_HORIZONTAL = $1; { flip horizontally }
  SDL_FLIP_VERTICAL   = $2; { flip vertically }

  SDL_ALPHA_OPAQUE = 255;
  SDL_ALPHA_TRANSPARENT = 0;
