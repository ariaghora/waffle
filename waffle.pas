unit waffle;

{$mode objfpc}{$H+}

interface

uses
  Classes, fgl, SysUtils;

const
  KEY_ESC   = 41;
  KEY_SPACE = 44;
  KEY_RIGHT = 79;
  KEY_LEFT  = 80;
  KEY_DOWN  = 81;
  KEY_UP    = 82;

  {$I sdlconst.inc }

{$I sdlintf.inc}

type
  TEventTimer = class;
  TSprite     = class;
  TScene      = class;
  TWaffleGame = class;

  TIntProc  = procedure(arg: integer);
  TGameProc = procedure(Game: TWaffleGame);
  TProc     = procedure;
  TSpriteList = specialize TFPGObjectList<TSprite>;
  //TSpriteList = array of TSprite;

  { TWaffleGame }

  TWaffleGame = class
    FramePerSecond: integer;
    Quit:  boolean;
    ScreenHeight, ScreenWidth: integer;
    Title: PChar;
  private
    CurrentScene: TScene;
    FFullscreen:  boolean;
    procedure SetFullscreen(AValue: boolean);
  public
    OnKeyDown: TIntProc;
    constructor Create(ATitle: PChar; AScreenWidth, AScreenHeight: integer);
    procedure Cleanup;
    procedure Exit;
    procedure SetScene(Scene: TScene);
    procedure Start;
    property Fullscreen: boolean read FFullscreen write SetFullscreen;
  end;

  { TSprite }

  TSprite = class
    DstRect: TSDL_Rect;
  private
    FFlipHorizontal, FFlipVertical: boolean;
    FOpacity:   UInt8;
    FParentScene: TScene;
    FPosX, FPosY: Float;
    FShown:     boolean;
    FSmoothing: boolean;
    FWidth, FHeight: Float;
    FRotation:  double;

    CurrentFlip: integer;
    GameRef:     TWaffleGame;
    ImageFileName: PChar;
    Surface:     PSDL_Surface;
    //DstRect:     TSDL_Rect;
    Texture:     PSDL_Texture;
    function GetPosX: Float;
    function GetPosY: Float;
    procedure SetFlipHorizontal(AValue: boolean);
    procedure SetFlipVertical(AValue: boolean);
    procedure SetOpacity(AValue: UInt8);
    procedure SetPosX(AValue: Float);
    procedure SetPosY(AValue: Float);
  public
    constructor Create(AGame: TWaffleGame; APosX, APosY, AWidth, AHeight: Float);
      virtual;
    procedure Delete;
    procedure Draw; virtual;
    procedure OnUpdate(Game: TWaffleGame; dt: Float); virtual; abstract;
    procedure SetTexture(ATextre: PSDL_Texture);
    property FlipHorizontal: boolean read FFlipHorizontal write SetFlipHorizontal;
    property FlipVertical: boolean read FFlipVertical write SetFlipVertical;
    property Opacity: UInt8 read FOpacity write SetOpacity;
    property Shown: boolean read FShown write FShown;
    property ParentScene: TScene read FParentScene write FParentScene;
    property PosX: Float read FPosX write SetPosX;
    property PosY: Float read FPosY write SetPosY;
    property Rotation: double read FRotation write FRotation;
    property Width: Float read FWidth write FWidth;
    property Height: Float read FHeight write FHeight;
  end;

  { TAnimatedSprite }

  TAnimatedSprite = class(TSprite)
    FrameChangeDelay: integer;
    CurrFrame: integer;
    FrameCols, FrameRows: integer;
    SrcRect:   TSDL_Rect;
    InternalCounter: integer;
  private
    function GetFrameHeight: integer;
    function GetFrameWidth: integer;
    procedure SetFrameHeight(AValue: integer);
    procedure SetFrameWidth(AValue: integer);
  public
    constructor Create(AGame: TWaffleGame; APosX, APosY, AWidth, AHeight: Float);
      overload;
    procedure Draw; override;
    property FrameWidth: integer read GetFrameWidth write SetFrameWidth;
    property FrameHeight: integer read GetFrameHeight write SetFrameHeight;
  end;

  { TScene }

  TScene = class
    GameRef: TWaffleGame;
    OnInit:  TProc;
  private
    FSpriteList: TSpriteList;
    function GetScreenWidth: SInt32;
  public
    constructor Create;
    procedure AddSprite(sprite: TSprite);
    procedure Cleanup;
    procedure OnUpdate(Game: TWaffleGame; dt: Float); virtual; abstract;
    procedure OnKeyDown(Key: UInt32); virtual; abstract;
    property ScreenWidth: SInt32 read GetScreenWidth;
    property SpriteList: TSpriteList read FSpriteList;
  end;

  { TDefaultScene }

  TDefaultScene = class(TScene)
    procedure OnUpdate(Game: TWaffleGame; dt: Float); override;
  end;

  { TEventTimer }

  TEventTimer = class
  public
    OnTimer: TProc;
    TimerId: UInt32;
    Delay, Interval: UInt32;
    constructor Create(ADelay, AInterval: integer); virtual;
    procedure Start;
    procedure Stop;
  end;

function CreateTextureFromFile(FileName: PChar; Smooth: boolean = True): PSDL_Texture;
function IsKeyDown(KeyCode: integer): boolean;
function SpriteRectsIntersect(s1, s2: TSprite): boolean;

var
  Surf: PSDL_Surface;
  Tex:  PSDL_Texture;
  GameKeyboardState: PUInt8;

  GameWindow:   PSDL_Window;
  GameRenderer: PSDL_Renderer;

implementation

function CreateTextureFromFile(FileName: PChar; Smooth: boolean): PSDL_Texture;
begin
  if Smooth then
    SDL_SetHint(SDL_HINT_RENDER_SCALE_QUALITY, 'linear');
  Exit(SDL_CreateTextureFromSurface(GameRenderer, IMG_Load(FileName)));
end;

function IsKeyDown(KeyCode: integer): boolean;
begin
  Exit(GameKeyboardState[KeyCode] = 1);
  //Exit(False);
end;

function SpriteRectsIntersect(s1, s2: TSprite): boolean;
var
  r1, r2: TSDL_Rect;
begin
  r1     := s1.DstRect;
  r2     := s2.DstRect;
  Result := not ((r1.x > (r2.x + r2.w)) or (r1.x + r1.w < r2.x) or
    (r1.y > r2.y + r2.h) or (r1.y + r1.h < r2.y));
end;

{ TEventTimer }

function Callback(interval: UInt32; param: Pointer): UInt32; cdecl;
begin
  if Assigned(param) then
    TProc(param);
  exit(interval);
end;

constructor TEventTimer.Create(ADelay, AInterval: integer);
begin
  Delay    := ADelay;
  Interval := AInterval;
end;

procedure TEventTimer.Start;
begin
  if Assigned(OnTimer) then
  begin
    OnTimer;
    TimerId := SDL_AddTimer(50, @Callback, OnTimer);
  end;
end;

procedure TEventTimer.Stop;
begin
  SDL_RemoveTimer(TimerId);
end;


{ TDefaultScene }

procedure TDefaultScene.OnUpdate(Game: TWaffleGame; dt: Float);
begin

end;

{ TAnimatedSprite }

function TAnimatedSprite.GetFrameHeight: integer;
begin
  Exit(SrcRect.h);
end;

function TAnimatedSprite.GetFrameWidth: integer;
begin
  Exit(SrcRect.w);
end;

procedure TAnimatedSprite.SetFrameHeight(AValue: integer);
begin
  SrcRect.h := AValue;
end;

procedure TAnimatedSprite.SetFrameWidth(AValue: integer);
begin
  SrcRect.w := AValue;
end;

constructor TAnimatedSprite.Create(AGame: TWaffleGame;
  APosX, APosY, AWidth, AHeight: Float);
begin
  inherited;
  CurrFrame := 0;
  FrameChangeDelay := 1000;
  FrameCols := 1;
  FrameRows := 1;

  SrcRect.x := 0;
  SrcRect.y := 0;
  SrcRect.w := round(AWidth);
  SrcRect.h := round(AHeight);
end;

procedure TAnimatedSprite.Draw;
begin
  SrcRect.x := (CurrFrame * (FrameWidth));
  SDL_RenderCopyEx(GameRenderer, self.Texture, @SrcRect, @DstRect, self.Rotation,
    nil, CurrentFlip);

  InternalCounter := (InternalCounter + 1) mod
    round(GameRef.FramePerSecond * (FrameChangeDelay / 1000));
  if InternalCounter >= (round(GameRef.FramePerSecond *
    (FrameChangeDelay / 1000)) - 1) then
    CurrFrame := (CurrFrame + 1) mod (FrameCols * FrameRows);
end;

{ TScene }

function TScene.GetScreenWidth: SInt32;
begin
  Exit(GameRef.ScreenWidth);
end;

constructor TScene.Create;
begin
  FSpriteList := TSpriteList.Create(True);
  //FSpriteList := nil;
end;

procedure TScene.AddSprite(sprite: TSprite);
begin
  //SetLength(FSpriteList, Length(FSpriteList) + 1);
  //FSpriteList[Length(FSpriteList) - 1] := sprite;

  SpriteList.Add(sprite);
  sprite.ParentScene := self;
end;

procedure TScene.Cleanup;
var
  s: TSprite;
begin
  for s in SpriteList do
    s.Delete;
  FreeAndNil(self);
end;

{ TSprite }

function TSprite.GetPosX: Float;
begin
  Exit(DstRect.x);
end;

function TSprite.GetPosY: Float;
begin
  Exit(DstRect.y);
end;

procedure TSprite.SetFlipHorizontal(AValue: boolean);
begin
  FFlipHorizontal := AValue;

  if (not FFlipHorizontal) and (not FFlipVertical) then
    CurrentFlip := SDL_FLIP_NONE;
  if FFlipHorizontal and not FFlipVertical then
    CurrentFlip := SDL_FLIP_HORIZONTAL;
  if (not FFlipHorizontal) and FFlipVertical then
    CurrentFlip := SDL_FLIP_VERTICAL;
  if FFlipHorizontal and FFlipVertical then
    CurrentFlip := SDL_FLIP_HORIZONTAL or SDL_FLIP_VERTICAL;
end;

procedure TSprite.SetFlipVertical(AValue: boolean);
begin
  FFlipVertical := AValue;

  if (not FFlipHorizontal) and (not FFlipVertical) then
    CurrentFlip := SDL_FLIP_NONE;
  if FFlipHorizontal and not FFlipVertical then
    CurrentFlip := SDL_FLIP_HORIZONTAL;
  if (not FFlipHorizontal) and FFlipVertical then
    CurrentFlip := SDL_FLIP_VERTICAL;
  if FFlipHorizontal and FFlipVertical then
    CurrentFlip := SDL_FLIP_HORIZONTAL or SDL_FLIP_VERTICAL;
end;

procedure TSprite.SetOpacity(AValue: UInt8);
begin
  //SDL_SetTextureAlphaMod(Self.Texture, AValue);
  FOpacity := AValue;
end;

procedure TSprite.SetPosX(AValue: Float);
begin
  FPosX     := AValue;
  DstRect.x := round(AValue);
end;

procedure TSprite.SetPosY(AValue: Float);
begin
  FPosY     := AValue;
  DstRect.y := round(AValue);
end;

constructor TSprite.Create(AGame: TWaffleGame; APosX, APosY, AWidth, AHeight: Float);
begin
  CurrentFlip := SDL_FLIP_NONE;
  FlipHorizontal := False;
  FlipVertical := False;
  GameRef := AGame;
  Shown   := True;
  PosX    := APosX;
  PosY    := APosY;
  Width   := AWidth;
  Height  := AHeight;

  DstRect.x := round(PosX);
  DstRect.y := round(PosY);
  DstRect.w := round(Width);
  DstRect.h := round(Height);

  Opacity := 255;

  Rotation := 0;

end;

procedure TSprite.Delete;
var
  s: TSprite;
begin
  ParentScene.SpriteList.Remove(self);
  self := nil;
end;

procedure TSprite.Draw;
begin
  SDL_SetTextureAlphaMod(self.Texture, Opacity);
  SDL_RenderCopyEx(GameRenderer, self.Texture, nil, @DstRect, self.Rotation,
    nil, CurrentFlip);
end;

procedure TSprite.SetTexture(ATextre: PSDL_Texture);
begin
  self.Texture := ATextre;
end;

{ TWaffleGame }

procedure DefaultSceneUpdate(Game: TWaffleGame; dt: Float);
begin
  if IsKeyDown(KEY_ESC) then
    Game.Exit;
end;

procedure TWaffleGame.SetFullscreen(AValue: boolean);
begin
  FFullscreen := AValue;
  if AValue then
    SDL_SetWindowFullscreen(GameWindow, SDL_WINDOW_FULLSCREEN)
  else
    SDL_SetWindowFullscreen(GameWindow, 0);
end;

constructor TWaffleGame.Create(ATitle: PChar; AScreenWidth, AScreenHeight: integer);
var
  DefaultScene: TScene;
begin
  DefaultScene := TDefaultScene.Create;
  CurrentScene := DefaultScene;

  FramePerSecond := 60;
  Quit  := False;
  ScreenHeight := AScreenHeight;
  ScreenWidth := AScreenWidth;
  Title := ATitle;

  if SDL_Init(SDL_INIT_EVERYTHING) < 0 then
    WriteLn('Failed');

  if TTF_Init() = -1 then
    WriteLn('Failed');

  GameWindow   := SDL_CreateWindow(Title, 400, 100, ScreenWidth,
    ScreenHeight, SDL_WINDOW_SHOWN);
  GameRenderer := SDL_CreateRenderer(GameWindow, -1, 0);
end;

procedure TWaffleGame.Cleanup;
begin
  FreeAndNil(self);
end;

procedure TWaffleGame.Exit;
begin
  self.Quit := True;
end;

procedure TWaffleGame.SetScene(Scene: TScene);
begin
  CurrentScene  := Scene;
  Scene.GameRef := self;
end;

procedure TWaffleGame.Start;
var
  spr: TSprite;
  StartingTick: UInt32;
  TNow, TLast: UInt64;
  dt: Float;
  ev: PSDL_Event;

  f: PTTF_Font;
begin
  if Assigned(CurrentScene.OnInit) then
    CurrentScene.OnInit;

  New(ev);

  f: TTF_OpenFont();

  TNow := SDL_GetPerformanceCounter;
  while not quit do
  begin

    GameKeyboardState := SDL_GetKeyboardState(nil);
    SDL_PumpEvents;

    StartingTick := SDL_GetTicks;
    TLast := TNow;
    TNow  := SDL_GetPerformanceCounter;
    dt    := (TNow - TLast) / Float(SDL_GetPerformanceFrequency);

    { Event }
    while SDL_PollEvent(ev) = 1 do
      case ev^.type_ of
        SDL_KEYDOWN: begin end;//WriteLn(ev^.key.keysym.scancode);//WriteLn(SDL_GetKeyName(ev^.key.keysym.sym));
      end;

    { Update logic }
    CurrentScene.OnUpdate(self, dt);

    for spr in CurrentScene.SpriteList do
      if not (TMethod(@spr.OnUpdate).Code = Pointer(@system.AbstractError)) then
        spr.OnUpdate(self, dt);

    { Drawing logic }
    SDL_SetRenderDrawColor(GameRenderer, 0, 0, 0, SDL_ALPHA_OPAQUE);
    SDL_RenderClear(GameRenderer);

    for spr in CurrentScene.SpriteList do
      spr.Draw;

    SDL_RenderPresent(GameRenderer);

    if (1000 div FramePerSecond) > (SDL_GetTicks - StartingTick) then
      SDL_Delay(1000 div FramePerSecond - (SDL_GetTicks - StartingTick));
  end;

  { Cleanup renderer & window }
  SDL_DestroyRenderer(GameRenderer);
  SDL_DestroyWindow(GameWindow);

  SDL_Quit();
end;

initialization

end.
