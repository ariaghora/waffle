program shooter;

{$mode objfpc}{$H+}

uses
  waffle,
  SysUtils;

const
  SCREEN_WIDTH = 1024;
  SCREEN_HEIGHT = 640;

type

  { TPlayerSprite }

  TPlayerSprite = class(TSprite)
    Counter: integer;
    IsShooting: boolean;
    constructor Create(APosX, APosY, AWidth, AHeight: Float);
      override;
    procedure OnUpdate(Game: TWaffleGame; dt: Float); override;
    procedure SpawnBullet;
  end;

  { TEnemySprite }

  TEnemySprite = class(TSprite)
    Counter: integer;
    constructor Create(APosX, APosY, AWidth, AHeight: Float);
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
  Asteroid: TSprite;
  Stars: array[0..199] of TSprite;
  StarTexture, AsteroidTexture: PSDL_Texture;
  i: integer;
  score: integer;
  enemyspawntimer: float;
  txt: TText;
  BackgroundLayer, GameObjectLayer, UILayer: TLayer;

  procedure SpawnEnemy;
  var
    en: TEnemySprite;
  begin
    en := TEnemySprite.Create(
      Random(SCREEN_WIDTH - 80), -80,
      80,
      80);
    en.SetTexture(EnemyTexture);
    GameObjectLayer.AddSprite(en);
  end;

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

    Asteroid.PosY     := Asteroid.PosY + 50 * dt;
    Asteroid.Rotation := Asteroid.Rotation + 100 * dt;
    if Asteroid.PosY > SCREEN_HEIGHT then
    begin
      Asteroid.PosX := Random(SCREEN_WIDTH - 120);
      Asteroid.PosY := -98;
    end;

    enemyspawntimer := enemyspawntimer + dt;
    if enemyspawntimer > dt * 50 then
    begin
      enemyspawntimer := 0;
      SpawnEnemy;
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

    for e in GameObjectLayer.SpriteList do
      if (e is TEnemySprite) and SpriteRectsIntersect(self, e) then
      begin
        e.Delete;
        self.Delete;
        Inc(score);
        txt.SetText(PChar('Score: ' + IntToStr(score)));
      end;
  end;

  { TPlayer }

  constructor TPlayerSprite.Create(APosX, APosY, AWidth, AHeight: Float);
  begin
    inherited Create(APosX, APosY, AWidth, AHeight);
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
    Bullet := TPlayerBullet.Create(PosX + Width /
      2 - 5, PosY - 40, 10, 40);
    Bullet.SetTexture(PlayerBulletTexture);
    GameObjectLayer.AddSprite(Bullet);
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

  constructor TEnemySprite.Create(APosX, APosY, AWidth, AHeight: Float);
  begin
    inherited Create(APosX, APosY, AWidth, AHeight);
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
    Bullet := TEnemyBullet.Create(PosX + Width / 2 - 5, PosY +
      Height - 10, 10, 30);
    Bullet.SetTexture(EnemyBulletTexture);
    GameObjectLayer.AddSprite(Bullet);
  end;

  procedure LoadTextures;
  begin
    PlayerTexture   := CreateTextureFromFile('assets/playerShip1_red.png');
    PlayerBulletTexture := CreateTextureFromFile('assets/laserRed01.png');
    EnemyTexture    := CreateTextureFromFile('assets/enemyBlue1.png');
    EnemyBulletTexture := CreateTextureFromFile('assets/laserBlue15.png');
    AsteroidTexture := CreateTextureFromFile('assets/meteorBrown_big2.png');
    StarTexture     := CreateTextureFromFile('assets/star1.png');
  end;

begin
  Randomize;
  LoadTextures;

  score := 0;

  ShooterGame := TWaffleGame.Create('Space Shooter', 100, 100,
    SCREEN_WIDTH, SCREEN_HEIGHT);
  ShooterGame.FramePerSecond := 60;

  txt := TText.Create(10, 10, 20);
  txt.LoadFontFromFile('assets/neuropol.ttf');
  txt.SetText(PChar('Score: ' + IntToStr(score)));
  txt.SetColor(255, 0, 0, 255);

  Player := TPlayerSprite.Create(
    SCREEN_WIDTH div 2 - 40,
    SCREEN_HEIGHT - 120,
    99,
    75);
  Player.SetTexture(PlayerTexture);

  BackgroundLayer := TLayer.Create;
  { create stars and put them in background layer }
  for i := 0 to 199 do
  begin
    Stars[i] := TSprite.Create(random(SCREEN_WIDTH),
      random(SCREEN_HEIGHT), 10, 10);
    stars[i].SetTexture(StarTexture);
    if i > 100 then
    begin
      Stars[i].Opacity := 150;
      stars[i].SetTexture(StarTexture);
    end;
    BackgroundLayer.AddSprite(Stars[i]);
  end;
  Asteroid := TSprite.Create(Random(SCREEN_WIDTH - 120), 0, 120, 98);
  Asteroid.SetTexture(AsteroidTexture);
  BackgroundLayer.AddSprite(Asteroid);

  GameObjectLayer := TLayer.Create;
  GameObjectLayer.AddSprite(Player);

  UILayer := TLayer.Create;
  UILayer.AddSprite(txt);

  MainScene := TMainScene.Create;
  MainScene.AddLayer(BackgroundLayer);
  MainScene.AddLayer(GameObjectLayer);
  MainScene.AddLayer(UILayer);

  { set current scene }
  ShooterGame.SetScene(MainScene);
  ShooterGame.Start;
  ShooterGame.Cleanup;
end.