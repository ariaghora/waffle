program shooter;

{$mode objfpc}{$H+}

uses
  waffle;

const
  SCREEN_WIDTH = 1024;
  SCREEN_HEIGHT = 640;

type

  { TPlayerSprite }

  TPlayerSprite = class(TSprite)
    Counter: integer;
    IsShooting: boolean;
    constructor Create(AGame: TWaffleGame; APosX, APosY, AWidth, AHeight: Float);
      override;
    procedure OnUpdate(Game: TWaffleGame; dt: Float); override;
    procedure SpawnBullet;
  end;

  { TEnemySprite }

  TEnemySprite = class(TSprite)
    Counter: integer;
    constructor Create(AGame: TWaffleGame; APosX, APosY, AWidth, AHeight: Float);
      override;
    procedure OnUpdate(Game: TWaffleGame; dt: Float); override;
    procedure SpawnBullet;
  end;


  { TPlayerBullet }

  TPlayerBullet = class(TSprite)
    procedure OnUpdate(Game: TWaffleGame; dt: Float); override;
  end;

  { TEnemyBullet }

  TEnemyBullet = class(TSprite)
    procedure OnUpdate(Game: TWaffleGame; dt: Float); override;
  end;

  { TMainScene }

  TMainScene = class(TScene)
    procedure OnUpdate(Game: TWaffleGame; dt: Float); override;
  end;

var
  ShooterGame: TWaffleGame;
  MainScene: TScene;
  Player: TPlayerSprite;
  PlayerTexture, PlayerBulletTexture: PSDL_Texture;
  EnemyTexture, EnemyBulletTexture: PSDL_Texture;
  Stars: array[0..199] of TSprite;
  StarTexture, StarTextureDim: PSDL_Texture;
  i: integer;
  enemyspawntimer: float;




  { TMainScene }
  procedure TMainScene.OnUpdate(Game: TWaffleGame; dt: Float);
  var
    en: TEnemySprite;
  begin
    if IsKeyDown(KEY_ESC) then
      Game.Exit;

    if IsKeyDown(KEY_RIGHT) then
      Player.PosX := Player.PosX + 500 * dt;

    if IsKeyDown(KEY_LEFT) then
      Player.PosX := Player.PosX - 500 * dt;

    if IsKeyDown(KEY_UP) then
      Player.PosY := Player.PosY - 500 * dt;

    if IsKeyDown(KEY_DOWN) then
      Player.PosY := Player.PosY + 500 * dt;

    if IsKeyDown(KEY_SPACE) then
      Player.IsShooting := True
    else
    begin
      Player.IsShooting := False;
      Player.Counter    := 0;
    end;

    { Update stars movement }
    for i := 0 to 199 do
    begin
      if i > 100 then
        Stars[i].PosY := Stars[i].PosY + 0.3
      else
        Stars[i].PosY := Stars[i].PosY + 0.6;

      if Stars[i].PosY > SCREEN_HEIGHT then
        Stars[i].PosY := 0;
    end;

    enemyspawntimer := enemyspawntimer + dt;
    if enemyspawntimer > dt * 50 then
    begin
      enemyspawntimer := 0;
      en := TEnemySprite.Create(ShooterGame,
        Random(SCREEN_WIDTH - 80), -80,
        80,
        80);
      en.SetTexture(EnemyTexture);
      MainScene.AddSprite(en);
    end;
  end;

  { TPlayerBullet }

  procedure TPlayerBullet.OnUpdate(Game: TWaffleGame; dt: Float);
  var
    e: TSprite;
  begin
    PosY := PosY - 1000 * dt;

    if PosY < -Height then
      Delete;

    for e in ParentScene.SpriteList do
      if (e is TEnemySprite) and SpriteRectsIntersect(self, e) then
      begin
        e.Delete;
        self.Delete;
      end;
  end;

  { TPlayer }

  constructor TPlayerSprite.Create(AGame: TWaffleGame;
    APosX, APosY, AWidth, AHeight: Float);
  begin
    inherited Create(AGame, APosX, APosY, AWidth, AHeight);
    IsShooting := False;
  end;

  procedure TPlayerSprite.OnUpdate(Game: TWaffleGame; dt: Float);
  var
    SpawnDelay: Float;
  begin
    SpawnDelay := 0.2;  // in seconds
    if (Counter = 0) and IsShooting then
      SpawnBullet;

    if IsShooting then
      Counter := (Counter + 1) mod round(Game.FramePerSecond * SpawnDelay);

  end;

  procedure TPlayerSprite.SpawnBullet;
  var
    Bullet: TPlayerBullet;
  begin
    Bullet := TPlayerBullet.Create(ParentScene.GameRef, PosX + Width /
      2 - 5, PosY - 40, 10, 40);
    Bullet.SetTexture(PlayerBulletTexture);
    ParentScene.AddSprite(Bullet);
  end;

  { TBullet }

  procedure TEnemyBullet.OnUpdate(Game: TWaffleGame; dt: Float);
  begin
    PosY := PosY + 500 * dt;

    if PosY > SCREEN_HEIGHT then
      self.Delete;

    if SpriteRectsIntersect(self, player) then
      self.Delete;

  end;

  constructor TEnemySprite.Create(AGame: TWaffleGame;
    APosX, APosY, AWidth, AHeight: Float);
  begin
    inherited Create(AGame, APosX, APosY, AWidth, AHeight);
    Counter := 0;

  end;

  procedure TEnemySprite.OnUpdate(Game: TWaffleGame; dt: Float);
  var
    SpawnDelay: Float;
  begin
    PosY := PosY + 100 * dt;

    if PosY > SCREEN_HEIGHT then
      Delete;

    if Counter = 0 then
      SpawnBullet;
    SpawnDelay := 1;  // in seconds
    Counter    := (Counter + 1) mod round(Game.FramePerSecond * SpawnDelay);

  end;

  procedure TEnemySprite.SpawnBullet;
  var
    Bullet: TEnemyBullet;
  begin
    Bullet := TEnemyBullet.Create(ParentScene.GameRef, PosX + Width / 2 - 5, PosY +
      Height - 10, 10, 30);
    Bullet.SetTexture(EnemyBulletTexture);
    ParentScene.AddSprite(Bullet);
  end;

  procedure SpawnEnemy;
  begin
    MainScene.AddSprite(TEnemySprite.Create(ShooterGame,
      Random(SCREEN_WIDTH - 80), -80,
      80,
      80));
  end;

begin
  ShooterGame := TWaffleGame.Create('Space Shooter', SCREEN_WIDTH, SCREEN_HEIGHT);
  ShooterGame.FramePerSecond := 60;
  ShooterGame.Fullscreen:=True;

  PlayerTexture := CreateTextureFromFile('assets/playerShip1_red.png');
  PlayerBulletTexture := CreateTextureFromFile('assets/laserRed01.png');
  EnemyTexture  := CreateTextureFromFile('assets/enemyBlue1.png');
  EnemyBulletTexture := CreateTextureFromFile('assets/laserBlue15.png');
  StarTexture   := CreateTextureFromFile('assets/star1.png');

  MainScene := TMainScene.Create;

  Player := TPlayerSprite.Create(ShooterGame,
    SCREEN_WIDTH div 2 - 40,
    SCREEN_HEIGHT - 120,
    99,
    75);

  Player.SetTexture(PlayerTexture);
  MainScene.AddSprite(Player);

  { create stars }
  for i := 0 to 199 do
  begin
    Stars[i] := TSprite.Create(ShooterGame, random(SCREEN_WIDTH),
      random(SCREEN_HEIGHT), 10, 10);
    stars[i].SetTexture(StarTexture);
    if i > 100 then
    begin
      Stars[i].Opacity := 150;
      stars[i].SetTexture(StarTexture);
    end;
    MainScene.AddSprite(Stars[i]);
  end;

  { set current scene }
  ShooterGame.SetScene(MainScene);

  ShooterGame.Start;
  ShooterGame.Cleanup;
end.
