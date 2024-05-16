{
  Copyright 2011-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Game initialization and logic. }
unit GameInitialize;

interface

implementation

uses
  { standard units }
  SysUtils, Math,
  { Castle Game Engine units }
  CastleWindow, CastleFilesUtils, CastleStringUtils,
  CastleUtils, CastleGLUtils, CastleKeysMouse, CastleMessages, CastleGLImages,
  CastleImages, CastleColors, CastleLog, CastleApplicationProperties,
  { game units }
  GameMap, GamePlayer, GameWindow;

var
  Player: TPlayer;
  ViewMoveX, ViewMoveY: Single;
  ViewFollowsPlayer: boolean = true;

procedure WindowRender(Container: TCastleContainer);
var
  RealViewMoveX, RealViewMoveY: Integer;

  procedure DrawImageOnTile(X, Y: Cardinal; DrawableImage: TDrawableImage;
    const SpecialMoveX: Integer = 0;
    const SpecialMoveY: Integer = 0);
  var
    PosX, PosY: Integer;
  begin
    PosX := X * BaseWidth;
    if Odd(Y) then
      PosX := PosX + BaseWidth div 2;
    PosX := PosX + RealViewMoveX + SpecialMoveX;
    PosY := Y * (BaseHeight div 2);
    PosY := PosY + RealViewMoveY + SpecialMoveY;
    DrawableImage.Alpha := acTest;
    DrawableImage.Draw(PosX, PosY);
  end;

var
  X, Y: Cardinal;
  MapTile: TMapTile;
  BaseFitX, BaseFitY: Cardinal;
  X1, X2, Y1, Y2: Integer;
begin
  BaseFitX := Ceil(Window.Width / BaseWidth) + 1;
  BaseFitY := Ceil(2 * Window.Height / BaseHeight) + 1;

  if ViewFollowsPlayer then
  begin
    { Ignore ViewMoveX/Y, calculate RealView such that the player
      is in the middle. }
    RealViewMoveX := Player.XPixel;
    RealViewMoveY := Player.YPixel;
    if Player.Moving then
    begin
      RealViewMoveX := RealViewMoveX - Round(Player.MovingSmallMoveX);
      RealViewMoveY := RealViewMoveY - Round(Player.MovingSmallMoveY);
    end;
  end else
  begin
    RealViewMoveX := Round(ViewMoveX);
    RealViewMoveY := Round(ViewMoveY);
  end;

  { First: this is what would be seen if RealViewMoveX/Y is zero. }
  X1 := -1;
  X2 := Integer(BaseFitX) - 2;
  Y1 := -1;
  Y2 := Integer(BaseFitY) - 2;
  { Now translate taking RealViewMoveX/Y into account. }
  X1 := X1 - Ceil(RealViewMoveX / BaseWidth);
  X2 := X2 - Floor(RealViewMoveX / BaseWidth);
  Y1 := Y1 - Ceil(2 * RealViewMoveY / BaseHeight);
  Y2 := Y2 - Floor(2 * RealViewMoveY / BaseHeight);
  { Eventually correct to be inside 0..Map.Width/Height - 1 range }
  ClampVar(X1, 0, Map.Width - 1);
  ClampVar(X2, 0, Map.Width - 1);
  ClampVar(Y1, 0, Map.Height - 1);
  ClampVar(Y2, 0, Map.Height - 1);

  for X := X1 to X2 do
    for Y := Y1 to Y2 do
    begin
      MapTile := Map.Items[X, Y];
      DrawImageOnTile(X, Y, MapTile.BaseTile.DrawableImage);
    end;

  { TODO: unoptimal code, should draw only the part that fits within the window.
    We should auto-check width/height of bonus tile, to know when to draw it.
    Even better, we should record this on the map --- which tile is visible
    where. }
  for Y := Map.Height - 1 downto 0 do
  begin
    { The order of drawing is important. Player must be drawn
      on top of some objects and below some others. }
    if Y = Player.Y then
    begin
      if Player.Moving then
        DrawImageOnTile(Player.X, Player.Y, Player.DrawableImage[Player.Direction],
          Round(Player.MovingSmallMoveX),
          Round(Player.MovingSmallMoveY)) else
        DrawImageOnTile(Player.X, Player.Y, Player.DrawableImage[Player.Direction]);
    end;

    for X := 0 to Map.Width - 1 do
    begin
      MapTile := Map.Items[X, Y];
      if MapTile.BonusTile <> nil then
        DrawImageOnTile(X, Y, MapTile.BonusTile.DrawableImage);
    end;
  end;
end;

procedure WindowPress(Container: TCastleContainer; const Event: TInputPressRelease);
var
  NewViewMoveX, NewViewMoveY: Integer;

  { Get character from user. Returns #0 if cancelled. }
  function MessageChar(const S: string): char;
  var
    Event: TInputPressRelease;
  begin
    Event := MessageKeyMouse(Window,
      'Enter the character code of new base tile, or Escape to cancel');
    if (Event.EventType = itKey) and
       (Event.KeyCharacter <> CharEscape) and
       (Event.KeyCharacter <> #0)  then
      Result := Event.KeyCharacter else
      Result := #0;
  end;

  procedure EditBaseTile;
  var
    BaseTile: TBaseTile;
    C: Char;
  begin
    C := MessageChar('Enter the character code of new base tile, or Escape to cancel');
    if C <> #0 then
    begin
      BaseTile := Map.BaseTiles[C];
      if BaseTile = nil then
        MessageOK(Window, Format('The character "%s" is not a code ' +
          'for any base tile', [C])) else
      Map.Items[Player.X, Player.Y].BaseTile := BaseTile;
    end;
  end;

  procedure EditBonusTile;
  var
    BonusTile: TBonusTile;
    C: Char;
  begin
    C := MessageChar('Enter the character code of new bonus tile, or "_" to clear or Escape to cancel');
    if C <> #0 then
    begin
      if C = '_' then
        Map.Items[Player.X, Player.Y].BonusTile := nil else
      begin
        BonusTile := Map.BonusTiles[C];
        if BonusTile = nil then
          MessageOK(Window, Format('The character "%s" is not a code ' +
            'for any bonus tile', [C])) else
        Map.Items[Player.X, Player.Y].BonusTile := BonusTile;
      end;
    end;
  end;

  procedure ShowFieldInfo;

    function TileDescr(Tile: TTile): string;
    begin
      if Tile = nil then
        Result := '<none>' else
        Result := Format('"%s" (URL "%s")',
          [Tile.CharCode, Tile.RelativeUrl]);
    end;

  begin
    MessageOK(Window, Format(
      'Position: %d, %d' +nl+
      'Base tile: %s' +nl+
      'Bonus tile: %s',
      [ Player.X, Player.Y,
        TileDescr(Map.Items[Player.X, Player.Y].BaseTile),
        TileDescr(Map.Items[Player.X, Player.Y].BonusTile) ]));
  end;

var
  Url: String;
begin
  if Event.EventType = itKey then
  begin
    case Event.KeyCharacter of
      'f': begin
             ViewFollowsPlayer := not ViewFollowsPlayer;
             if not ViewFollowsPlayer then
             begin
               { Set ViewMoveX/Y initial values such that the player is still
                 in the middle. This is less confusing for user. }
               ViewMoveToCenterPosition(Player.X, Player.Y,
                 NewViewMoveX, NewViewMoveY);
               ViewMoveX := NewViewMoveX;
               ViewMoveY := NewViewMoveY;
             end;
           end;
      'e': EditBaseTile;
      'E': EditBonusTile;
      's': begin
             Url := 'new';
             if MessageInputQuery(Window, 'Save map as name' +
               ' (don''t specify here initial path and .map extension)', Url) then
               Map.SaveToFile('castle-data:/maps/' + Url + '.map');
           end;
      'i': ShowFieldInfo;
      CharEscape: Window.Close;
    end;
  end;
end;

procedure WindowUpdate(Container: TCastleContainer);
const
  ViewMoveChangeSpeed = 10.0 * 50.0;
begin
  if not ViewFollowsPlayer then
  begin
    if Window.Pressed[keyArrowUp]    then ViewMoveY := ViewMoveY - ViewMoveChangeSpeed * Window.Fps.SecondsPassed;
    if Window.Pressed[keyArrowDown]  then ViewMoveY := ViewMoveY + ViewMoveChangeSpeed * Window.Fps.SecondsPassed;
    if Window.Pressed[keyArrowRight] then ViewMoveX := ViewMoveX - ViewMoveChangeSpeed * Window.Fps.SecondsPassed;
    if Window.Pressed[keyArrowLeft]  then ViewMoveX := ViewMoveX + ViewMoveChangeSpeed * Window.Fps.SecondsPassed;
  end else
  begin
    { At first I placed the commands below in KeyDown, as they work
      like KeyDown: non-continuously. However, thanks to smooth scrolling
      of the screen, user is easily fooled and thinks that they work
      continuously. So he keeps pressing them. So we should check them
      here. }
    if Window.Pressed[keyArrowUp]    then Player.Move(dirNorth);
    if Window.Pressed[keyArrowDown]  then Player.Move(dirSouth);
    if Window.Pressed[keyArrowLeft]  then Player.Move(dirWest);
    if Window.Pressed[keyArrowRight] then Player.Move(dirEast);

    if Window.Pressed[keyNumpad7] then Player.Move(dirNorthWest);
    if Window.Pressed[keyNumpad9] then Player.Move(dirNorthEast);
    if Window.Pressed[keyNumpad1] then Player.Move(dirSouthWest);
    if Window.Pressed[keyNumpad3] then Player.Move(dirSouthEast);
    if Window.Pressed[keyNumpad4] then Player.Move(dirWest);
    if Window.Pressed[keyNumpad6] then Player.Move(dirEast);
    if Window.Pressed[keyNumpad2] then Player.Move(dirSouth);
    if Window.Pressed[keyNumpad8] then Player.Move(dirNorth);

    if Window.Pressed[keyF10] then
    begin
      { simulate OpenGL context close + open, this may happen at any time on Android/iOS }
      Window.Close(false);
      Window.Open;
    end;
  end;

  GameTime := GameTime + Window.Fps.SecondsPassed;

  Player.Update;
end;

{ One-time initialization of resources. }
procedure ApplicationInitialize;
begin
  { Assign Window callbacks }
  Window.OnUpdate := @WindowUpdate;
  Window.OnPress := @WindowPress;
  Window.OnRender := @WindowRender;

  { For a scalable UI (adjusts to any window size in a smart way), use UIScaling }
  // Window.Container.UIReferenceWidth := 1024;
  // Window.Container.UIReferenceHeight := 768;
  // Window.Container.UIScaling := usEncloseReferenceSize;

  Map := TMap.CreateFromFile('castle-data:/maps/1.map');
  Player := TPlayer.Create;
  Player.Teleport(Map.PlayerStartX, Map.PlayerStartY, dirSouth);
  Player.CalculatePixelPosition;
end;

initialization
  { This initialization section configures:
    - Application.OnInitialize
    - Application.MainWindow
    - determines initial window size

    You should not need to do anything more in this initialization section.
    Most of your actual application initialization (in particular, any file reading)
    should happen inside ApplicationInitialize. }

  Application.OnInitialize := @ApplicationInitialize;

  Window := TCastleWindow.Create(Application);
  Application.MainWindow := Window;

  { Optionally, adjust window fullscreen state and size at this point.
    Examples:

    Run fullscreen:

      Window.FullScreen := true;

    Run in a 600x400 window:

      Window.FullScreen := false; // default
      Window.Width := 600;
      Window.Height := 400;

    Run in a window taking 2/3 of screen (width and height):

      Window.FullScreen := false; // default
      Window.Width := Application.ScreenWidth * 2 div 3;
      Window.Height := Application.ScreenHeight * 2 div 3;

    Note that some platforms (like mobile) ignore these window sizes.
  }

  { Handle command-line parameters like --fullscreen and --window.
    By doing this last, you let user to override your fullscreen / mode setup. }
  Window.ParseParameters;

  { Our drawing routine is not prepared to react perfectly to window size change
    at runtime. So disable it for now. }
  Window.ResizeAllowed := raOnlyAtOpen;
  Window.FpsShowOnCaption := true;
finalization
  FreeAndNil(Player);
  FreeAndNil(Map);
end.
