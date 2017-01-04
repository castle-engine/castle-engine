program SandBox;

{$apptype GUI}

uses
  { standard units }
  SysUtils, Math,
  { Castle Game Engine units }
  CastleWindow, CastleFilesUtils, CastleWindowModes, CastleStringUtils,
  CastleUtils, CastleGLUtils, CastleKeysMouse, CastleMessages, CastleGLImages,
  CastleImages, CastleColors, CastleLog,
  { game units }
  SandBoxMap, SandBoxPlayer, SandBoxGame;

var
  Player: TPlayer;
  ViewMoveX, ViewMoveY: Single;
  ViewFollowsPlayer: boolean = true;

procedure WindowRender(Container: TUIContainer);
var
  RealViewMoveX, RealViewMoveY: Integer;

  procedure DrawImageOnTile(X, Y: Cardinal; GLImage: TGLImage;
    const SpecialMoveX: Integer = 0;
    const SpecialMoveY: Integer = 0);
  var
    PosX, PosY: Integer;
  begin
    PosX := X * BaseWidth;
    if Odd(Y) then
      PosX += BaseWidth div 2;
    PosX += RealViewMoveX + SpecialMoveX;
    PosY := Y * (BaseHeight div 2);
    PosY += RealViewMoveY + SpecialMoveY;
    GLImage.Alpha := acTest;
    GLImage.Draw(PosX, PosY);
  end;

var
  X, Y: Cardinal;
  MapTile: TMapTile;
  BaseFitX, BaseFitY: Cardinal;
  X1, X2, Y1, Y2: Integer;
begin
  RenderContext.Clear([cbColor], Black);

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
      RealViewMoveX -= Round(Player.MovingSmallMoveX);
      RealViewMoveY -= Round(Player.MovingSmallMoveY);
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
  X1 -= Ceil(RealViewMoveX / BaseWidth);
  X2 -= Floor(RealViewMoveX / BaseWidth);
  Y1 -= Ceil(2 * RealViewMoveY / BaseHeight);
  Y2 -= Floor(2 * RealViewMoveY / BaseHeight);
  { Eventually correct to be inside 0..Map.Width/Height - 1 range }
  ClampVar(X1, 0, Map.Width - 1);
  ClampVar(X2, 0, Map.Width - 1);
  ClampVar(Y1, 0, Map.Height - 1);
  ClampVar(Y2, 0, Map.Height - 1);

  for X := X1 to X2 do
    for Y := Y1 to Y2 do
    begin
      MapTile := Map.Items[X, Y];
      DrawImageOnTile(X, Y, MapTile.BaseTile.GLImage);
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
        DrawImageOnTile(Player.X, Player.Y, Player.GLImage[Player.Direction],
          Round(Player.MovingSmallMoveX),
          Round(Player.MovingSmallMoveY)) else
        DrawImageOnTile(Player.X, Player.Y, Player.GLImage[Player.Direction]);
    end;

    for X := 0 to Map.Width - 1 do
    begin
      MapTile := Map.Items[X, Y];
      if MapTile.BonusTile <> nil then
        DrawImageOnTile(X, Y, MapTile.BonusTile.GLImage);
    end;
  end;
end;

procedure WindowPress(Container: TUIContainer; const Event: TInputPressRelease);
var
  NewViewMoveX, NewViewMoveY: Integer;

  { Get character from user. Returns #0 if cancelled. }
  function MessageChar(const S: string): char;
  var
    Event: TInputPressRelease;
  begin
    MessageKeyMouse(Window,
      'Enter the character code of new base tile, or Escape to cancel', Event);
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
          [Tile.CharCode, Tile.RelativeURL]);
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
  URL: string;
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
             URL := 'new';
             if MessageInputQuery(Window, 'Save map as name' +
               ' (don''t specify here initial path and .map extension)', URL) then
               Map.SaveToFile(ApplicationData('maps/' + URL + '.map'));
           end;
      'i': ShowFieldInfo;
      CharEscape: Window.Close;
    end;
  end;
end;

procedure WindowUpdate(Container: TUIContainer);
const
  ViewMoveChangeSpeed = 10.0 * 50.0;
begin
  if not ViewFollowsPlayer then
  begin
    if Window.Pressed[K_Up]    then ViewMoveY -= ViewMoveChangeSpeed * Window.Fps.UpdateSecondsPassed;
    if Window.Pressed[K_Down]  then ViewMoveY += ViewMoveChangeSpeed * Window.Fps.UpdateSecondsPassed;
    if Window.Pressed[K_Right] then ViewMoveX -= ViewMoveChangeSpeed * Window.Fps.UpdateSecondsPassed;
    if Window.Pressed[K_Left]  then ViewMoveX += ViewMoveChangeSpeed * Window.Fps.UpdateSecondsPassed;
  end else
  begin
    { At first I placed the commands below in KeyDown, as they work
      like KeyDown: non-continuously. However, thanks to smooth scrolling
      of the screen, user is easily fooled and thinks that they work
      continuously. So he keeps pressing them. So we should check them
      here. }
    if Window.Pressed[K_Up]    then Player.Move(dirNorth);
    if Window.Pressed[K_Down]  then Player.Move(dirSouth);
    if Window.Pressed[K_Left]  then Player.Move(dirWest);
    if Window.Pressed[K_Right] then Player.Move(dirEast);

    if Window.Pressed[K_Numpad_7] then Player.Move(dirNorthWest);
    if Window.Pressed[K_Numpad_9] then Player.Move(dirNorthEast);
    if Window.Pressed[K_Numpad_1] then Player.Move(dirSouthWest);
    if Window.Pressed[K_Numpad_3] then Player.Move(dirSouthEast);
    if Window.Pressed[K_Numpad_4] then Player.Move(dirWest);
    if Window.Pressed[K_Numpad_6] then Player.Move(dirEast);
    if Window.Pressed[K_Numpad_2] then Player.Move(dirSouth);
    if Window.Pressed[K_Numpad_8] then Player.Move(dirNorth);

    if Window.Pressed[K_F10] then
    begin
      { simulate OpenGL context close + open, this may happen at any time on Android/iOS }
      Window.Close(false);
      Window.Open;
    end;
  end;

  GameTime += Window.Fps.UpdateSecondsPassed;

  Player.Update;
end;


begin
  InitializeLog;

  Window := TCastleWindowCustom.Create(Application);

  Window.Caption := 'The Sandbox';
  { Our drawing routine is not prepared to react perfectly to window size change
    at runtime. So disable it, for now. }
  Window.ResizeAllowed := raOnlyAtOpen;
  Window.AutoRedisplay := true;
  Window.FpsShowOnCaption := true;
  Window.OnResize := @Resize2D;
  Window.OnRender := @WindowRender;
  Window.OnPress := @WindowPress;
  Window.OnUpdate := @WindowUpdate;

  Map := TMap.CreateFromFile(ApplicationData('maps/1.map'));
  Player := TPlayer.Create;
  Player.Teleport(Map.PlayerStartX, Map.PlayerStartY, dirSouth);
  try
    Window.Open;
    Player.CalculatePixelPosition;
    Application.Run;
  finally
    FreeAndNil(Player);
    FreeAndNil(Map);
  end;
end.
