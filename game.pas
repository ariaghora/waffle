unit game;

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
    FShown:    boolean;
    FWidth, FHeight: SInt32;
    FRotation: double;

    GameRef: TGame;
    Surface: PSDL_Surface;
    Rect:    TSDL_Rect;
    Texture: PSDL_Texture;
    function GetPosX: SInt32;
    function GetPosY: SInt32;
    procedure SetPosX(AValue: SInt32);
    procedure SetPosY(AValue: SInt32);
  public
    constructor Create(AGame: TGame; AFileName: PChar;
      APosX, APosY, AWidth, AHeight: SInt32);
    procedure Cleanup;
    procedure Draw;
    procedure SetImage(AFileName: PChar);

    property Shown: boolean read FShown write FShown;
    property PosX: SInt32 read GetPosX write SetPosX;
    property PosY: SInt32 read GetPosY write SetPosY;
    property Rotation: double read FRotation write FRotation;
    property Width: SInt32 read FWidth write FWidth;
    property Height: SInt32 read FHeight write FHeight;
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
  Exit(Rect.x);
end;

function TSprite.GetPosY: SInt32;
begin
  Exit(Rect.y);
end;

procedure TSprite.SetPosX(AValue: SInt32);
begin
  Rect.x := AValue;
end;

procedure TSprite.SetPosY(AValue: SInt32);
begin
  Rect.y := AValue;
end;

constructor TSprite.Create(AGame: TGame; AFileName: PChar;
  APosX, APosY, AWidth, AHeight: SInt32);
begin
  GameRef := AGame;
  Shown   := True;
  PosX    := APosX;
  PosY    := APosY;
  Width   := AWidth;
  Height  := AHeight;

  rect.x := PosX;
  rect.y := PosY;
  rect.w := Width;
  rect.h := Height;

  Rotation := 0;

  SDL_SetHint(SDL_HINT_RENDER_SCALE_QUALITY, 'linear');
  Surface := IMG_Load(AFileName);
  Texture := SDL_CreateTextureFromSurface(AGame.GameRenderer, Surface);
end;

procedure TSprite.Cleanup;
begin
  SDL_FreeSurface(Surface);
  SDL_DestroyTexture(Texture);
  FreeAndNil(self);
end;

procedure TSprite.Draw;
begin
  SDL_RenderCopyEx(GameRef.GameRenderer, self.Texture, nil, @Rect, self.Rotation,
    nil, SDL_FLIP_NONE);
end;

procedure TSprite.SetImage(AFileName: PChar);
begin
  SDL_SetHint(SDL_HINT_RENDER_SCALE_QUALITY, 'linear');

  Surface := IMG_Load(AFileName);
  Texture := SDL_CreateTextureFromSurface(GameRef.GameRenderer, Surface);
end;

{ TGame }

constructor TGame.Create(ATitle: PChar; AScreenWidth, AScreenHeight: integer);
begin
  CurrentScene := nil;
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
  spr: tsprite;
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
      spr.Draw;

    SDL_RenderPresent(GameRenderer);
    SDL_Delay(16);
  end;

  { Cleanup renderer & window }
  SDL_DestroyRenderer(GameRenderer);
  SDL_DestroyWindow(GameWindow);

  SDL_Quit();
end;

initialization

end.
