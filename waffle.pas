unit waffle;

{$mode objfpc}{$H+}

interface

uses
  Classes, fgl, SysUtils;

const
  KEY_ESC   = 41;
  KEY_RIGHT = 79;
  KEY_LEFT  = 80;
  KEY_DOWN  = 81;
  KEY_UP    = 82;

  {$I sdlconst.inc }

{$I sdlintf.inc}

type
  TSprite = class;
  TScene  = class;
  TGame   = class;

  TIntProc = procedure(arg: integer);
  TProc    = procedure;
  TSpriteList = specialize TFPGObjectList<TSprite>;

  { TGame }

  TGame = class
    FramePerSecond: integer;
    GameWindow: PSDL_Window;
    GameRenderer: PSDL_Renderer;
    Quit:  boolean;
    ScreenHeight, ScreenWidth: integer;
    Title: PChar;
  private
    CurrentScene: TScene;
  public
    OnKeyDown: TIntProc;
    constructor Create(ATitle: PChar; AScreenWidth, AScreenHeight: integer);
    procedure Cleanup;
    procedure Exit;
    procedure SetScene(Scene: TScene);
    procedure Start;
  end;

  { TSprite }

  TSprite = class
  private
    FFlipHorizontal, FFlipVertical: boolean;
    FShown:    boolean;
    FWidth, FHeight: SInt32;
    FRotation: double;

    CurrentFlip: integer;
    GameRef:     TGame;
    ImageFileName: PChar;
    Surface:     PSDL_Surface;
    DstRect:     TSDL_Rect;
    Texture:     PSDL_Texture;
    function GetPosX: SInt32;
    function GetPosY: SInt32;
    procedure SetFlipHorizontal(AValue: boolean);
    procedure SetFlipVertical(AValue: boolean);
    procedure SetPosX(AValue: SInt32);
    procedure SetPosY(AValue: SInt32);
  public
    constructor Create(AGame: TGame; AFileName: PChar;
      APosX, APosY, AWidth, AHeight: SInt32);
    procedure Cleanup;
    procedure Draw;
    procedure SetImage(AFileName: PChar);
    procedure SetImageScaleFilter(Filter: PChar);
    property FlipHorizontal: boolean read FFlipHorizontal write SetFlipHorizontal;
    property FlipVertical: boolean read FFlipVertical write SetFlipVertical;
    property Shown: boolean read FShown write FShown;
    property PosX: SInt32 read GetPosX write SetPosX;
    property PosY: SInt32 read GetPosY write SetPosY;
    property Rotation: double read FRotation write FRotation;
    property Width: SInt32 read FWidth write FWidth;
    property Height: SInt32 read FHeight write FHeight;
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
    constructor Create(AGame: TGame; AFileName: PChar;
      APosX, APosY, AWidth, AHeight: SInt32); overload;
    procedure Draw; overload;
    property FrameWidth: integer read GetFrameWidth write SetFrameWidth;
    property FrameHeight: integer read GetFrameHeight write SetFrameHeight;
  end;

  { TScene }

  TScene = class
    OnInit:   TProc;
    OnUpdate: TProc;
  private
    GameRef:    TGame;
    SpriteList: TSpriteList;
    function GetScreenWidth: SInt32;
  public
    constructor Create;
    procedure AddSprite(sprite: TSprite);
    procedure Cleanup;
    property ScreenWidth: SInt32 read GetScreenWidth;
  end;

function IsKeyDown(KeyCode: integer): boolean;

var
  Surf: PSDL_Surface;
  Tex:  PSDL_Texture;
  GameKeyboardState: PUInt8;

implementation

function IsKeyDown(KeyCode: integer): boolean;
begin
  Exit(GameKeyboardState[KeyCode] = 1);
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

constructor TAnimatedSprite.Create(AGame: TGame; AFileName: PChar;
  APosX, APosY, AWidth, AHeight: SInt32);
begin
  inherited;
  CurrFrame := 0;
  FrameChangeDelay := 1000;
  FrameCols := 1;
  FrameRows := 1;

  SrcRect.x := 0;
  SrcRect.y := 0;
  SrcRect.w := AWidth;
  SrcRect.h := AHeight;
end;

procedure TAnimatedSprite.Draw;
begin
  SrcRect.x := (CurrFrame * (FrameWidth));
  SDL_RenderCopyEx(GameRef.GameRenderer, self.Texture, @SrcRect, @DstRect, self.Rotation,
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
  SpriteList := TSpriteList.Create();
end;

procedure TScene.AddSprite(sprite: TSprite);
begin
  SpriteList.Add(sprite);
end;

procedure TScene.Cleanup;
var
  s: TSprite;
begin
  for s in SpriteList do
    s.Cleanup;
  FreeAndNil(self);
end;

{ TSprite }

function TSprite.GetPosX: SInt32;
begin
  Exit(DstRect.x);
end;

function TSprite.GetPosY: SInt32;
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

procedure TSprite.SetPosX(AValue: SInt32);
begin
  DstRect.x := AValue;
end;

procedure TSprite.SetPosY(AValue: SInt32);
begin
  DstRect.y := AValue;
end;

constructor TSprite.Create(AGame: TGame; AFileName: PChar;
  APosX, APosY, AWidth, AHeight: SInt32);
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

  DstRect.x := PosX;
  DstRect.y := PosY;
  DstRect.w := Width;
  DstRect.h := Height;

  Rotation := 0;

  ImageFileName := AFileName;
  SetImage(ImageFileName);
end;

procedure TSprite.Cleanup;
begin
  SDL_FreeSurface(Surface);
  SDL_DestroyTexture(Texture);
  FreeAndNil(self);
end;

procedure TSprite.Draw;
begin
  SDL_RenderCopyEx(GameRef.GameRenderer, self.Texture, nil, @DstRect, self.Rotation,
    nil, CurrentFlip);
end;

procedure TSprite.SetImage(AFileName: PChar);
begin
  Surface := IMG_Load(AFileName);
  Texture := SDL_CreateTextureFromSurface(GameRef.GameRenderer, Surface);
end;

procedure TSprite.SetImageScaleFilter(Filter: PChar);
begin
  SDL_SetHint(SDL_HINT_RENDER_SCALE_QUALITY, Filter);
  SetImage(ImageFileName);
end;

{ TGame }

constructor TGame.Create(ATitle: PChar; AScreenWidth, AScreenHeight: integer);
begin
  CurrentScene := nil;
  FramePerSecond := 60;
  Quit  := False;
  ScreenHeight := AScreenHeight;
  ScreenWidth := AScreenWidth;
  Title := ATitle;

  if SDL_Init(SDL_INIT_EVERYTHING) < 0 then
    WriteLn('Failed');
  GameWindow   := SDL_CreateWindow(Title, 100, 100, ScreenWidth,
    ScreenHeight, SDL_WINDOW_SHOWN);
  GameRenderer := SDL_CreateRenderer(GameWindow, -1, 0);
end;

procedure TGame.Cleanup;
begin
  FreeAndNil(self);
end;

procedure TGame.Exit;
begin
  self.Quit := True;
end;

procedure TGame.SetScene(Scene: TScene);
begin
  CurrentScene  := Scene;
  Scene.GameRef := self;
end;

procedure TGame.Start;
var
  spr: TSprite;
begin
  if Assigned(CurrentScene.OnInit) then
    CurrentScene.OnInit;

  while not quit do
  begin
    { Event }
    SDL_PumpEvents;
    GameKeyboardState := SDL_GetKeyboardState(nil);

    { Update logic }
    if Assigned(CurrentScene.OnUpdate) then
      CurrentScene.OnUpdate();

    { Drawing logic }
    SDL_SetRenderDrawColor(GameRenderer, 0, 0, 0, SDL_ALPHA_OPAQUE);
    SDL_RenderClear(GameRenderer);

    for spr in CurrentScene.SpriteList do
      if spr is TAnimatedSprite then
        TAnimatedSprite(spr).Draw
      else
        spr.Draw;

    SDL_RenderPresent(GameRenderer);
    SDL_Delay(1000 div FramePerSecond);
  end;

  { Cleanup renderer & window }
  SDL_DestroyRenderer(GameRenderer);
  SDL_DestroyWindow(GameWindow);

  SDL_Quit();
end;

initialization

end.
