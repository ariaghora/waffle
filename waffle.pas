unit waffle;

{$mode objfpc}{$H+}

interface

uses
  Classes, fgl, math, SysUtils;

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
  TGameEntity = class;
  TLayer      = class;
  TSprite     = class;
  TScene      = class;
  TWaffleGame = class;

  TIntProc   = procedure(arg: integer);
  TGameProc  = procedure(Game: TWaffleGame);
  TProc      = procedure;
  TLayerList = specialize TFPGObjectList<TLayer>;
  TEntityList = specialize TFPGObjectList<TGameEntity>;
  TSpriteList = specialize TFPGObjectList<TSprite>;

  { TGameEntity }

  TGameEntity = class
  private
    FParentLayer: TLayer;
    FParentScene: TScene;
  public
    procedure Delete; virtual;
    property ParentLayer: TLayer read FParentLayer write FParentLayer;
    property ParentScene: TScene read FParentScene write FParentScene;
  end;

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
    constructor Create(ATitle: PChar; AX, AY, AScreenWidth, AScreenHeight: integer);
    procedure Cleanup;
    procedure Exit;
    procedure SetScene(Scene: TScene);
    procedure Start;
    property Fullscreen: boolean read FFullscreen write SetFullscreen;
  end;

  { TSprite }

  TSprite = class(TGameEntity)
    DstRect: TSDL_Rect;
  private
    FFlipHorizontal, FFlipVertical: boolean;
    FOpacity:   SInt32;
    FPosX, FPosY: Float;
    FShown:     boolean;
    FSmoothing: boolean;
    FWidth, FHeight: Float;
    FRotation:  double;

    CurrentFlip: integer;
    ImageFileName: PChar;
    Surface: PSDL_Surface;
    Texture: PSDL_Texture;
    function GetPosX: Float;
    function GetPosY: Float;
    procedure SetFlipHorizontal(AValue: boolean);
    procedure SetFlipVertical(AValue: boolean);
    procedure SetOpacity(AValue: SInt32);
    procedure SetPosX(AValue: Float);
    procedure SetPosY(AValue: Float);
  public
    constructor Create(APosX, APosY, AWidth, AHeight: Float);
      virtual;
    procedure Draw; virtual;
    procedure OnUpdate(Game: TWaffleGame; dt: Float); virtual; abstract;
    procedure SetTexture(ATextre: PSDL_Texture);
    property FlipHorizontal: boolean read FFlipHorizontal write SetFlipHorizontal;
    property FlipVertical: boolean read FFlipVertical write SetFlipVertical;
    property Opacity: SInt32 read FOpacity write SetOpacity;
    property Shown: boolean read FShown write FShown;
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
    constructor Create(APosX, APosY, AWidth, AHeight: Float); overload;
    procedure Draw; override;
    property FrameWidth: integer read GetFrameWidth write SetFrameWidth;
    property FrameHeight: integer read GetFrameHeight write SetFrameHeight;
  end;

  { TText }

  TText = class(TSprite)
  private
    FFont:      PTTF_Font;
    FFontColor: TSDL_Color;
    FFontSize:  integer;
    FText:      PChar;
  public
    constructor Create(APosX, APosY: Float; AFontSize: integer); overload;
    procedure LoadFontFromFile(FileName: PChar);
    procedure SetColor(r, g, b, a: integer);
    procedure SetText(AText: PChar);
    procedure UpdateTexture;
  end;

  { TLayer }

  TLayer = class
  private
    FSPriteList: TSpriteList;
  public
    constructor Create;
    procedure AddSprite(Sprite: TSprite);
    procedure Cleanup;
    procedure Draw;
    procedure OnUpdate(Game: TWaffleGame; dt: Float);
    property SpriteList: TSpriteList read FSpriteList;
  end;


  { TParticle }

  TParticle = class(TSprite)
    vx, vy: Float;
    life:   Float;
  public
    constructor Create(APosX, APosY, AWidth, AHeight, AVx, AVy: Float);
    procedure OnUpdate(Game: TWaffleGame; dt: Float); override;
  end;

  { TParticleEmitter }

  TParticleEmitter = class
    PosX, PosY:      float;
    MinVx, MaxVx:    float;
    MinVy, MaxVy:    float;
    ParentLayer:     TLayer;
    ParticleTexture: PSDL_Texture;
  private
    FNumParticle: integer;
    FSpriteList:  TSpriteList;
  public
    constructor Create(ATexture: PSDL_Texture; APosX, APosY: Float;
      ANumParticle: integer);
    constructor Delete;
    procedure Draw;
    procedure OnUpdate(Game: TWaffleGame; dt: Float);
    property SpriteList: TSpriteList read FSpriteList write FSpriteList;
  end;

  { TScene }

  TScene = class
    GameRef: TWaffleGame;
    OnInit:  TProc;
  private
    FLayerList:  TLayerList;
    FSpriteList: TSpriteList;
    BaseLayer:   TLayer;
    function GetScreenWidth: SInt32;
  public
    constructor Create;
    procedure AddLayer(layer: TLayer);
    procedure AddSprite(sprite: TSprite);
    procedure Cleanup;
    procedure OnUpdate(Game: TWaffleGame; dt: Float); virtual; abstract;
    procedure OnKeyDown(Key: UInt32); virtual; abstract;
    property LayerList: TLayerList read FLayerList;
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

function CreateFontTextureFromFile(FileName: PChar; FontSize: integer): PSDL_Texture;
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

function CreateFontTextureFromFile(FileName: PChar; FontSize: integer): PSDL_Texture;
var
  f: PTTF_Font;
  fs: PSDL_Surface;
  ft: PSDL_Texture;
  fr: TSDL_Rect;
  c1: TSDL_Color;
begin
  c1.r := 255;
  c1.g := 255;
  c1.b := 255;
  c1.a := 255;

  f := TTF_OpenFont(FileName, FontSize);
  TTF_SetFontHinting(f, TTF_HINTING_NORMAL);
  fs := TTF_RenderText_Solid(f, 'Score: 0', c1);
  ft := SDL_CreateTextureFromSurface(GameRenderer, fs);

  fr.x := 10;
  fr.y := 10;
  TTF_SizeText(f, 'Score: 0', @fr.w, @fr.h);
end;

function CreateTextureFromFile(FileName: PChar; Smooth: boolean): PSDL_Texture;
begin
  if Smooth then
    SDL_SetHint(SDL_HINT_RENDER_SCALE_QUALITY, 'linear');
  Exit(SDL_CreateTextureFromSurface(GameRenderer, IMG_Load(FileName)));
end;

function IsKeyDown(KeyCode: integer): boolean;
begin
  Exit(GameKeyboardState[KeyCode] = 1);
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

{ TGameEntity }

procedure TGameEntity.Delete;
begin
  ParentLayer.SpriteList.Remove(TSprite(self));
  self := nil;
end;

{ TParticle }

constructor TParticle.Create(APosX, APosY, AWidth, AHeight, AVx, AVy: Float);
begin
  inherited Create(APosX, APosY, AWidth, AHeight);
  vx   := AVx;
  vy   := AVy;
  life := 0;
end;

procedure TParticle.OnUpdate(Game: TWaffleGame; dt: Float);
begin
  life := life + 1;
  PosX := PosX + vx * dt;
  PosY := PosY + vy * dt;

  Opacity := Opacity - 5;
  if Opacity < 0 then
    self.Delete;

  if life > 100 then
    self.Delete;
end;

{ TParticleEmitter }

constructor TParticleEmitter.Create(ATexture: PSDL_Texture;
  APosX, APosY: Float; ANumParticle: integer);
begin
  PosX := APosX;
  PosY := APosY;
  FNumParticle := ANumParticle;
  FSpriteList := TSpriteList.Create();
  ParticleTexture := ATexture;
end;

constructor TParticleEmitter.Delete;
var
  s: TSprite;
begin
  for s in SpriteList do
    s.Delete;
end;

procedure TParticleEmitter.Draw;
begin

end;

procedure TParticleEmitter.OnUpdate(Game: TWaffleGame; dt: Float);
var
  p: TParticle;
begin
  p := TParticle.Create(PosX, PosY, 30, 40, RandomRange(-20, 20),
    RandomRange(20, 100));
  p.SetTexture(ParticleTexture);
  ParentLayer.AddSprite(p);
end;

{ TLayer }

constructor TLayer.Create;
begin
  FSPriteList := TSpriteList.Create();
end;

procedure TLayer.AddSprite(Sprite: TSprite);
begin
  SpriteList.Add(Sprite);
  Sprite.ParentLayer := Self;
end;

procedure TLayer.Cleanup;
var
  s: TSprite;
begin
  for s in FSpriteList do
    s.Draw;
end;

procedure TLayer.Draw;
var
  Spr: TSprite;
begin
  for Spr in SpriteList do
    Spr.Draw;
end;

procedure TLayer.OnUpdate(Game: TWaffleGame; dt: Float);
var
  spr: TSprite;
begin
  for spr in SpriteList do
    if not (TMethod(@spr.OnUpdate).Code = Pointer(@system.AbstractError)) then
      spr.OnUpdate(Game, dt);
end;

{ TText }

constructor TText.Create(APosX, APosY: Float; AFontSize: integer);
begin
  Create(APosX, APosY, 0, 0);
  SetColor(255, 255, 255, 255);
  FText     := '';
  FFontSize := AFontSize;
end;

procedure TText.LoadFontFromFile(FileName: PChar);
begin
  FFont := TTF_OpenFont(FileName, FFontSize);
  TTF_SetFontHinting(FFont, TTF_HINTING_NORMAL);
end;

procedure TText.SetColor(r, g, b, a: integer);
begin
  FFontColor.r := r;
  FFontColor.g := g;
  FFontColor.b := b;
  FFontColor.a := a;
  UpdateTexture;
end;

procedure TText.SetText(AText: PChar);
begin
  FText := AText;
  UpdateTexture;
end;

procedure TText.UpdateTexture;
begin
  Surface := TTF_RenderText_Solid(FFont, FText, FFontColor);
  Texture := SDL_CreateTextureFromSurface(GameRenderer, Surface);
  TTF_SizeText(FFont, FText, @DstRect.w, @DstRect.h);
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

constructor TAnimatedSprite.Create(APosX, APosY, AWidth, AHeight: Float);
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
    round(ParentScene.GameRef.FramePerSecond * (FrameChangeDelay / 1000));
  if InternalCounter >= (round(ParentScene.GameRef.FramePerSecond *
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
  FLayerList  := TLayerList.Create();
  FSpriteList := TSpriteList.Create();
  BaseLayer   := TLayer.Create;
end;

procedure TScene.AddLayer(layer: TLayer);
begin
  LayerList.Add(layer);
end;

procedure TScene.AddSprite(sprite: TSprite);
begin
  BaseLayer.SpriteList.Add(sprite);
  //sprite.ParentLayer := BaseLayer;
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

procedure TSprite.SetOpacity(AValue: SInt32);
begin
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

constructor TSprite.Create(APosX, APosY, AWidth, AHeight: Float);
begin
  CurrentFlip := SDL_FLIP_NONE;
  FlipHorizontal := False;
  FlipVertical := False;
  Shown  := True;
  PosX   := APosX;
  PosY   := APosY;
  Width  := AWidth;
  Height := AHeight;

  DstRect.x := round(PosX);
  DstRect.y := round(PosY);
  DstRect.w := round(Width);
  DstRect.h := round(Height);

  Opacity := 255;

  Rotation := 0;

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

constructor TWaffleGame.Create(ATitle: PChar;
  AX, AY, AScreenWidth, AScreenHeight: integer);
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

  SDL_SetWindowSize(GameWindow, AScreenWidth, AScreenHeight);
  SDL_SetWindowPosition(GameWindow, AX, AY);
  SDL_SetWindowTitle(GameWindow, ATitle);
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
  layer: TLayer;
  StartingTick: UInt32;
  TNow, TLast: UInt64;
  dt: Float;
  ev: PSDL_Event;
begin
  if Assigned(CurrentScene.OnInit) then
    CurrentScene.OnInit;

  New(ev);

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
        SDL_KEYDOWN: ;
        //WriteLn(ev^.key.keysym.scancode);//WriteLn(SDL_GetKeyName(ev^.key.keysym.sym));
      end;

    { Update logic }
    CurrentScene.OnUpdate(self, dt);

    for spr in CurrentScene.SpriteList do
      if not (TMethod(@spr.OnUpdate).Code = Pointer(@system.AbstractError)) then
        spr.OnUpdate(self, dt);

    CurrentScene.BaseLayer.OnUpdate(self, dt);
    for layer in CurrentScene.LayerList do
      layer.OnUpdate(self, dt);


    { Drawing logic }
    SDL_SetRenderDrawColor(GameRenderer, 0, 0, 0, SDL_ALPHA_OPAQUE);
    SDL_RenderClear(GameRenderer);

    for spr in CurrentScene.SpriteList do
      spr.Draw;

    CurrentScene.BaseLayer.Draw;
    for layer in CurrentScene.LayerList do
      layer.Draw;

    SDL_RenderPresent(GameRenderer);

    if (1000 div FramePerSecond) > (SDL_GetTicks - StartingTick) then
      SDL_Delay(1000 div FramePerSecond - (SDL_GetTicks - StartingTick));
  end;

  SDL_Quit();
end;

initialization
  { TODO: add error handling }
  SDL_Init(SDL_INIT_EVERYTHING);
  TTF_Init();

  GameWindow   := SDL_CreateWindow('', 0, 0, 0, 0, SDL_WINDOW_SHOWN);
  GameRenderer := SDL_CreateRenderer(GameWindow, -1, 0);

finalization
  SDL_DestroyRenderer(GameRenderer);
  SDL_DestroyWindow(GameWindow);

end.
