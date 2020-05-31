unit waffle;

{$mode objfpc}{$H+}

interface

uses
  Classes, fgl, fptimer, Math, SysUtils;

const
  KEY_RETURN = 40;
  KEY_ESC = 41;
  KEY_SPACE = 44;
  KEY_RIGHT = 79;
  KEY_LEFT = 80;
  KEY_DOWN = 81;
  KEY_UP = 82;

  {$I sdlconst.pp }

{$I sdlintf.pp}

type
  TEventTimer = class;
  TGameEntity = class;
  TLayer = class;
  TSprite = class;
  TScene = class;
  TWaffleGame = class;

  TIntProc = procedure(arg: integer);
  TGameProc = procedure(Game: TWaffleGame);
  TProc = procedure;
  TLayerList = specialize TFPGObjectList<TLayer>;
  TEntityList = specialize TFPGObjectList<TGameEntity>;
  TSpriteList = specialize TFPGObjectList<TSprite>;

  TTexture = record
    TextureData: PSDL_Texture;
  end;

  TGameEntity = class
  private
    FParentLayer: TLayer;
    FParentScene: TScene;
  public
    procedure Delete; virtual;
    property ParentLayer: TLayer read FParentLayer write FParentLayer;
    property ParentScene: TScene read FParentScene write FParentScene;
  end;

  TWaffleGame = class
    FPSLimit: integer;
    Quit: boolean;
    ScreenHeight, ScreenWidth: integer;
    Title: PChar;
  private
    CurrentScene: TScene;
    FFullscreen: boolean;
    procedure SetFullscreen(AValue: boolean);
  public
    OnKeyDown: TIntProc;
    constructor Create(ATitle: PChar; AX, AY, AScreenWidth, AScreenHeight: integer);
    procedure Cleanup;
    procedure Exit;
    procedure OnInit; virtual; abstract;
    procedure SetScene(Scene: TScene);
    procedure Start;
    property Fullscreen: boolean read FFullscreen write SetFullscreen;
  end;

  TSprite = class(TGameEntity)
    DstRect: TSDL_Rect;
  private
    FFlipHorizontal, FFlipVertical: boolean;
    FOpacity: integer;
    FPosX, FPosY: Float;
    FShown: boolean;
    FWidth, FHeight: Float;
    FRotation: double;

    CurrentFlip: integer;
    Texture: TTexture;
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
    procedure SetTexture(ATexture: TTexture);
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

  PFrameInfo = ^TFrameInfo;

  TFrameInfo = record
    CurrentFrame: pInt32;
    TotalFrame: integer;
  end;

  TAnimatedSprite = class(TSprite)
    FFrameChangeDelay: integer;
    CurrFrame: integer;
    FFrameCols, FFrameRows: integer;
    SrcRect: TSDL_Rect;
    InternalCounter: integer;
    FrameInfo: TFrameInfo;
  private
    FrameNum: integer;
    AnimationTimerID: integer;
    Playing: boolean;
    function GetFrameHeight: integer;
    function GetFrameWidth: integer;
    procedure SetFrameChangeDelay(AValue: integer);
    procedure SetFrameCols(AValue: integer);
    procedure SetFrameHeight(AValue: integer);
    procedure SetFrameRows(AValue: integer);
    procedure SetFrameWidth(AValue: integer);
    procedure UpdateFrameNum;
  public
    constructor Create(APosX, APosY, AWidth, AHeight: Float); overload;
    procedure Draw; override;
    procedure PlayAnimation;
    procedure StopAnimation;
    procedure RestartAnimation;
    property FrameChangeDelay: integer read FFrameChangeDelay write SetFrameChangeDelay;
    property FrameCols: integer read FFrameCols write SetFrameCols;
    property FrameRows: integer read FFrameRows write SetFrameRows;
    property FrameWidth: integer read GetFrameWidth write SetFrameWidth;
    property FrameHeight: integer read GetFrameHeight write SetFrameHeight;
  end;

  TText = class(TSprite)
  private
    Surface: PSDL_Surface;
    FFont: PTTF_Font;
    FFontColor: TSDL_Color;
    FFontSize: integer;
    FText: PChar;
  public
    constructor Create(APosX, APosY: Float; AFontSize: integer); overload;
    procedure LoadFontFromFile(FileName: PChar);
    procedure SetColor(r, g, b, a: integer);
    procedure SetText(AText: PChar);
    procedure UpdateTexture;
  end;

  TLayer = class
  private
    FParentScene: TScene;
    FEntityList: TEntityList;
  public
    constructor Create;
    procedure AddEntitiy(Entity: TGameEntity);
    procedure Cleanup;
    procedure Draw;
    procedure OnUpdate(Game: TWaffleGame; dt: Float);
    property EntityList: TEntityList read FEntityList;
    property ParentScene: TScene read FParentScene write FParentScene;
  end;

  TParticle = class(TSprite)
    vx, vy: Float;
    life: Float;
  public
    constructor Create(APosX, APosY, AWidth, AHeight, AVx, AVy: Float);
    procedure OnUpdate(Game: TWaffleGame; dt: Float); override;
  end;

  TParticleEmitter = class
    PosX, PosY: float;
    MinVx, MaxVx: float;
    MinVy, MaxVy: float;
    Width, Height: float;
    ParentLayer: TLayer;
    ParticleTexture: TTexture;
  private
    FNumParticle: integer;
    FSpriteList: TSpriteList;
  public
    constructor Create(ATexture: TTexture; APosX, APosY, AWidth, AHeight: Float;
      ANumParticle: integer);
    constructor Delete;
    procedure Draw;
    procedure OnUpdate(Game: TWaffleGame; dt: Float);
    property SpriteList: TSpriteList read FSpriteList write FSpriteList;
  end;

  TScene = class
    GameRef: TWaffleGame;
    OnInit: TProc;
  private
    FLayerList: TLayerList;
    FSpriteList: TSpriteList;
    BaseLayer: TLayer;
    function GetScreenWidth: SInt32;
  public
    constructor Create;
    procedure AddEntity(Entity: TGameEntity);
    procedure AddLayer(layer: TLayer);
    procedure Cleanup;
    procedure OnDraw; virtual;
    procedure OnUpdate(Game: TWaffleGame; dt: Float); virtual;
    procedure OnKeyDown(Key: UInt32); virtual;
    property LayerList: TLayerList read FLayerList;
    property ScreenWidth: SInt32 read GetScreenWidth;
    property SpriteList: TSpriteList read FSpriteList;
  end;

  TDefaultScene = class(TScene)
    procedure OnUpdate(Game: TWaffleGame; dt: Float); override;
  end;

  TEventTimer = class
  private
    Timer: TFPTimer;
    FInterval: integer;
    procedure SetInterval(AValue: integer);
  public
    Delay: integer;
    constructor Create(AInterval: integer); virtual;
    procedure DoAction; virtual; abstract;
    procedure OnTimer(Sender: TObject);
    procedure Start;
    procedure Stop;
    property Interval: integer read FInterval write SetInterval;
  end;

function CreateTextureFromFile(FileName: PChar; Smooth: boolean = True): TTexture;
function IsKeyDown(KeyCode: integer): boolean;
function SpriteRectsIntersect(s1, s2: TSprite): boolean;

procedure ClearScreen(r, g, b, a: integer);
procedure FillRect(Rect: PSDL_Rect; r, g, b, a: integer);

var
  Surf: PSDL_Surface;
  Tex: PSDL_Texture;
  GameKeyboardState: PUInt8;
  GameWindow: PSDL_Window;
  GameRenderer: PSDL_Renderer;

implementation

function CreateTextureFromFile(FileName: PChar; Smooth: boolean): TTexture;
begin
  if Smooth then
    SDL_SetHint(SDL_HINT_RENDER_SCALE_QUALITY, 'linear');
  Result.TextureData := SDL_CreateTextureFromSurface(GameRenderer, IMG_Load(FileName));
end;

function IsKeyDown(KeyCode: integer): boolean;
begin
  Exit(GameKeyboardState[KeyCode] = 1);
end;

function SpriteRectsIntersect(s1, s2: TSprite): boolean;
var
  r1, r2: TSDL_Rect;
begin
  r1 := s1.DstRect;
  r2 := s2.DstRect;
  Result := not ((r1.x > (r2.x + r2.w)) or (r1.x + r1.w < r2.x) or
    (r1.y > r2.y + r2.h) or (r1.y + r1.h < r2.y));
end;

procedure ClearScreen(r, g, b, a: integer);
begin
  SDL_SetRenderDrawColor(GameRenderer, r, g, b, a);
  SDL_SetRenderDrawBlendMode(GameRenderer, $00000001);
  SDL_RenderClear(GameRenderer);
end;

procedure FillRect(Rect: PSDL_Rect; r, g, b, a: integer);
begin
  SDL_SetRenderDrawColor(GameRenderer, r, g, b, a);
  SDL_SetRenderDrawBlendMode(GameRenderer, $00000001);
  SDL_RenderFillRect(GameRenderer, Rect);
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
  //ParentLayer.SpriteList.Remove(TSprite(self));
  ParentLayer.EntityList.Remove(self);
  self := nil;
end;

{ TParticle }

constructor TParticle.Create(APosX, APosY, AWidth, AHeight, AVx, AVy: Float);
begin
  inherited Create(APosX, APosY, AWidth, AHeight);
  vx := AVx;
  vy := AVy;
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

constructor TParticleEmitter.Create(ATexture: TTexture;
  APosX, APosY, AWidth, AHeight: Float; ANumParticle: integer);
begin
  PosX := APosX;
  PosY := APosY;
  Width := AWidth;
  Height := AHeight;
  MinVx := -100;
  MaxVx := 100;
  MinVy := -100;
  MaxVy := 100;
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

function RandomRangeF(min, max: single): single;
begin
  Result := min + Random * (max - min);
end;

procedure TParticleEmitter.OnUpdate(Game: TWaffleGame; dt: Float);
var
  p: TParticle;
begin
  p := TParticle.Create(PosX, PosY, Width, Height, RandomRangeF(MinVx, MaxVx),
    RandomRangeF(MinVy, MaxVy));
  p.SetTexture(ParticleTexture);
  ParentLayer.AddEntitiy(p);
end;

{ TLayer }

constructor TLayer.Create;
begin
  FEntityList := TEntityList.Create();
end;

procedure TLayer.AddEntitiy(Entity: TGameEntity);
begin
  EntityList.Add(Entity);
  Entity.ParentLayer := self;
end;

procedure TLayer.Cleanup;
var
  e: TGameEntity;
begin
  for e in EntityList do
    e.Delete;
end;

procedure TLayer.Draw;
var
  e: TGameEntity;
begin
  for e in EntityList do
    if e is TSprite then
      TSprite(e).Draw;
end;

procedure TLayer.OnUpdate(Game: TWaffleGame; dt: Float);
var
  e: TGameEntity;
begin
  for e in EntityList do
    if e is TSprite then
      if not (TMethod(@TSprite(e).OnUpdate).Code = Pointer(@system.AbstractError)) then
        TSprite(e).OnUpdate(Game, dt);
end;

{ TText }

constructor TText.Create(APosX, APosY: Float; AFontSize: integer);
begin
  Create(APosX, APosY, 0, 0);
  SetColor(255, 255, 255, 255);
  FText := '';
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
  Texture.TextureData := SDL_CreateTextureFromSurface(GameRenderer, Surface);
  TTF_SizeText(FFont, FText, @DstRect.w, @DstRect.h);
  Width := DstRect.w;
  Height := DstRect.h;
end;

procedure TEventTimer.SetInterval(AValue: integer);
begin
  if FInterval = AValue then
    Exit;
  FInterval := AValue;
  Timer.Interval := AValue;
end;

constructor TEventTimer.Create(AInterval: integer);
begin
  FInterval := AInterval;
end;

procedure TEventTimer.OnTimer(Sender: TObject);
begin
  if Assigned(@DoAction) then
    DoAction;
end;


procedure TEventTimer.Start;
begin
  Timer := TFPTimer.Create(nil);
  Timer.OnTimer := @OnTimer;
  Timer.Interval := Interval;
  Timer.UseTimerThread := True;
  Timer.Enabled := True;
end;

procedure TEventTimer.Stop;
begin
  Timer.Free;
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

procedure TAnimatedSprite.SetFrameChangeDelay(AValue: integer);
begin
  FFrameChangeDelay := AValue;
  RestartAnimation;
end;

procedure TAnimatedSprite.SetFrameCols(AValue: integer);
begin
  if FFrameCols = AValue then
    Exit;
  FFrameCols := AValue;
  UpdateFrameNum;
end;

procedure TAnimatedSprite.SetFrameHeight(AValue: integer);
begin
  SrcRect.h := AValue;
end;

procedure TAnimatedSprite.SetFrameRows(AValue: integer);
begin
  if FFrameRows = AValue then
    Exit;
  FFrameRows := AValue;
  UpdateFrameNum;
end;

procedure TAnimatedSprite.SetFrameWidth(AValue: integer);
begin
  SrcRect.w := AValue;
end;

procedure TAnimatedSprite.UpdateFrameNum;
begin
  FrameNum := FrameCols * FrameRows;
  FrameInfo.TotalFrame := FrameNum;
end;

function foo(interval: UInt32; param: Pointer): UInt32; cdecl;
begin
  PFrameInfo(param)^.CurrentFrame^ :=
    (PFrameInfo(param)^.CurrentFrame^ + 1) mod (PFrameInfo(param)^.TotalFrame);
  Exit(interval);
end;

constructor TAnimatedSprite.Create(APosX, APosY, AWidth, AHeight: Float);
begin
  inherited;
  CurrFrame := 0;
  FrameChangeDelay := 1000;
  FrameCols := 1;
  FrameRows := 1;

  FrameInfo.CurrentFrame := @CurrFrame;
  FrameInfo.TotalFrame := FrameCols * FrameRows;

  Playing := False;

  SrcRect.x := 0;
  SrcRect.y := 0;
  SrcRect.w := round(AWidth);
  SrcRect.h := round(AHeight);
end;

procedure TAnimatedSprite.Draw;
begin
  SrcRect.x := (CurrFrame * (FrameWidth));
  SDL_RenderCopyEx(GameRenderer, Texture.TextureData, @SrcRect,
    @DstRect, self.Rotation,
    nil, CurrentFlip);
end;

procedure TAnimatedSprite.PlayAnimation;
begin
  if not Playing then
  begin
    Playing := not Playing;
    AnimationTimerID := SDL_AddTimer(FrameChangeDelay, @foo, @FrameInfo);
  end;
end;

procedure TAnimatedSprite.StopAnimation;
begin
  if Playing then
  begin
    Playing := not Playing;
    SDL_RemoveTimer(AnimationTimerID);
  end;
end;

procedure TAnimatedSprite.RestartAnimation;
begin
  StopAnimation;
  PlayAnimation;
end;

{ TScene }

function TScene.GetScreenWidth: SInt32;
begin
  Exit(GameRef.ScreenWidth);
end;

constructor TScene.Create;
begin
  FLayerList := TLayerList.Create();
  FSpriteList := TSpriteList.Create();

  BaseLayer := TLayer.Create;
  AddLayer(BaseLayer);
end;

procedure TScene.AddEntity(Entity: TGameEntity);
begin
  BaseLayer.AddEntitiy(Entity);
end;

procedure TScene.AddLayer(layer: TLayer);
begin
  LayerList.Add(layer);
  layer.ParentScene := self;
end;

procedure TScene.Cleanup;
var
  s: TSprite;
  l: TLayer;
begin
  for s in SpriteList do
    s.Delete;

  for l in LayerList do
    l.Cleanup;

  FreeAndNil(self);
end;

procedure TScene.OnDraw;
var
  l: TLayer;
begin
  BaseLayer.Draw;
  for l in LayerList do
    l.Draw;
end;

procedure TScene.OnUpdate(Game: TWaffleGame; dt: Float);
var
  l: TLayer;
begin
  BaseLayer.OnUpdate(Game, dt);
  for l in LayerList do
    l.OnUpdate(Game, dt);
end;

procedure TScene.OnKeyDown(Key: UInt32);
begin

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
  FPosX := AValue;
  DstRect.x := round(AValue);
end;

procedure TSprite.SetPosY(AValue: Float);
begin
  FPosY := AValue;
  DstRect.y := round(AValue);
end;

constructor TSprite.Create(APosX, APosY, AWidth, AHeight: Float);
begin
  CurrentFlip := SDL_FLIP_NONE;
  FlipHorizontal := False;
  FlipVertical := False;
  Shown := True;
  PosX := APosX;
  PosY := APosY;
  Width := AWidth;
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
  SDL_SetTextureAlphaMod(self.Texture.TextureData, Opacity);
  SDL_RenderCopyEx(GameRenderer, self.Texture.TextureData, nil, @DstRect, self.Rotation,
    nil, CurrentFlip);
end;

procedure TSprite.SetTexture(ATexture: TTexture);
begin
  self.Texture := ATexture;
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

  FPSLimit := 60;
  Quit := False;
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
  CurrentScene := Scene;
  Scene.GameRef := self;
end;

procedure TWaffleGame.Start;
var
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
    TNow := SDL_GetPerformanceCounter;
    dt := (TNow - TLast) / Float(SDL_GetPerformanceFrequency);

    { Event }
    while SDL_PollEvent(ev) = 1 do
      case ev^.type_ of
        SDL_KEYDOWN: CurrentScene.OnKeyDown(ev^.key.keysym.scancode);
      end;

    { Update logic }
    if not (TMethod(@CurrentScene.OnUpdate).Code = Pointer(@system.AbstractError)) then
      CurrentScene.OnUpdate(self, dt);

    { Drawing logic }
    SDL_SetRenderDrawColor(GameRenderer, 0, 0, 0, SDL_ALPHA_OPAQUE);
    SDL_RenderClear(GameRenderer);

    CurrentScene.OnDraw;

    SDL_RenderPresent(GameRenderer);

    if (1000 div FPSLimit) > (SDL_GetTicks - StartingTick) then
      SDL_Delay(1000 div FPSLimit - (SDL_GetTicks - StartingTick));
  end;

  SDL_Quit();
end;

initialization
  { TODO: add error handling }
  SDL_Init(SDL_INIT_EVERYTHING);
  TTF_Init();

  GameWindow := SDL_CreateWindow('', 0, 0, 0, 0, SDL_WINDOW_SHOWN);
  GameRenderer := SDL_CreateRenderer(GameWindow, -1, 0);

finalization
  SDL_DestroyRenderer(GameRenderer);
  SDL_DestroyWindow(GameWindow);

end.
