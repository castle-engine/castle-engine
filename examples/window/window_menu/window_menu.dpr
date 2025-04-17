{
  Copyright 2004-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ A simple program using CastleWindow.
  Demonstrates the use of MainMenu in TCastleWindow.

  Shows
  - menu,
  - submenus,
  - menus with keyboard shortcuts,
  - checked menu items,
  - radio menu items,
  - adding menus at runtime,
  - using menu mnemonics (underscore, for Alt+keypress),
  - replacing whole main menu temporarily with other main menu.
}
program window_menu;

{$ifdef MSWINDOWS} {$apptype GUI} {$endif}

uses SysUtils,
  CastleVectors, CastleKeysMouse, CastleColors, CastleLog,
  CastleWindow, CastleGLUtils, CastleMessages, CastleStringUtils,
  CastleViewport, CastleScene, X3DNodes;

var
  Window: TCastleWindow;
  MenuHorizLeft, MenuHorizMiddle, MenuHorizRight: TMenuItemRadio;
  MainMenu, AlternativeMainMenu: TMenu;

const
  Colors: array [0..6] of TCastleColorRGB =
  ( (X: 1; Y: 0; Z: 0),
    (X: 0; Y: 1; Z: 0),
    (X: 0; Y: 0; Z: 1),
    (X: 1; Y: 1; Z: 0),
    (X: 1; Y: 1; Z: 1),
    (X: 0.5; Y: 0.5; Z: 0.5),
    (X: 0; Y: 0; Z: 0)
  );

var
  { Scene 3D information. Displayed only to show that menu commands actually work. }
  Scene: TCastleScene;

  Filled: boolean = true;
  Translation: TVector3;

  Shape: TShapeNode;
  Material: TMaterialNode;
  Transform: TTransformNode;
  GeometryRect: TQuadSetNode;
  GeometryTriangle: TTriangleSetNode;

procedure CreateScene;
var
  Appearance: TAppearanceNode;
  CoordRect, CoordTriangle: TCoordinateNode;
  Root: TX3DRootNode;
begin
  CoordRect := TCoordinateNode.Create;
  CoordRect.SetPoint([
    Vector3(-0.5, -0.5, 0),
    Vector3( 0.5, -0.5, 0),
    Vector3( 0.5,  0.5, 0),
    Vector3(-0.5,  0.5, 0)]);

  CoordTriangle := TCoordinateNode.Create;
  CoordTriangle.SetPoint([
    Vector3(-0.5, -0.5, 0),
    Vector3( 0.5, -0.5, 0),
    Vector3(   0,  0.5, 0)]);

  GeometryRect := TQuadSetNode.Create;
  GeometryRect.Coord := CoordRect;

  GeometryTriangle := TTriangleSetNode.Create;
  GeometryTriangle.Coord := CoordTriangle;

  Material := TMaterialNode.Create;
  Material.DiffuseColor := Colors[0]; // initial value

  Appearance := TAppearanceNode.Create;
  Appearance.Material := Material;

  Shape := TShapeNode.Create;
  Shape.Geometry := GeometryTriangle; // initial value
  Shape.Appearance := Appearance;

  Transform := TTransformNode.Create;
  Transform.Translation := Translation;
  Transform.AddChildren(Shape);

  Root := TX3DRootNode.Create;
  Root.AddChildren(Transform);

  Scene := TCastleScene.Create(nil);
  Scene.Load(Root, true);

  { Do not free GeometryXxx nodes automatically when unused.
    Usually, X3D nodes are freed automatically when they are no longer part
    of scene hierarchy, but in case of GeometryXxx, one of them is always *not*
    in hierarchy. }
  GeometryTriangle.KeepExistingBegin;
  GeometryRect.KeepExistingBegin;
end;

procedure DestroyScene;
begin
  GeometryTriangle.KeepExistingEnd;
  GeometryRect.KeepExistingEnd;
  FreeIfUnusedAndNil(GeometryTriangle);
  FreeIfUnusedAndNil(GeometryRect);
  FreeAndNil(Scene);
end;

var
  ChangeableMenu: TMenu;

procedure MenuClick(Container: TCastleContainer; Item: TMenuItem);

  procedure ChangeChecked(Item: TMenuItemRadio);
  begin
    Item.Checked := MessageYesNo(Window, 'Should menu item "' +
      SRemoveMnemonics(Item.Caption) + '" be checked ?');
    if Item.Checked then
      case Item.IntData of
        25: begin Translation.X := -0.5; Transform.Translation := Translation; end;
        26: begin Translation.X :=  0.0; Transform.Translation := Translation; end;
        27: begin Translation.X :=  0.5; Transform.Translation := Translation; end;
      end;
  end;

var
  M: TMenu;
begin
  WritelnLog('You clicked menu item "%s" with id %d', [
    SRemoveMnemonics(Item.Caption),
    Item.SmallId
  ]);
  case Item.IntData of
    10: begin
          Shape.FdGeometry.Value := GeometryRect;
          Shape.FdGeometry.Changed;
        end;
    11: begin
          Shape.FdGeometry.Value := GeometryTriangle;
          Shape.FdGeometry.Changed;
        end;
    20: Window.Close;

    21: begin Translation.Y :=  0.5; Transform.Translation := Translation; end;
    22: begin Translation.Y :=    0; Transform.Translation := Translation; end;
    23: begin Translation.Y := -0.5; Transform.Translation := Translation; end;

    25..27: ChangeChecked(Item as TMenuItemRadio);

    31: begin
          Filled := not Filled;
          if Filled then
            Scene.RenderOptions.WireframeEffect := weNormal
          else
            Scene.RenderOptions.WireframeEffect := weWireframeOnly;
        end;
    40: Window.MainMenu.Append(TMenuItem.Create('New item', -1));
    41: begin
         M := TMenu.Create('New submenu');
         M.Append(TMenuItem.Create('_One', -1));
         M.Append(TMenuItem.Create('_Two', -1));
         ChangeableMenu.Append(M);
        end;
    42: ChangeableMenu.Append(TMenuItem.Create('New item', -1));

    100: Window.MainMenu := AlternativeMainMenu;
    101: Window.MainMenu := MainMenu;

    200.. 200 + High(Colors): Material.DiffuseColor := Colors[Item.IntData - 200];

    else Exit;
  end;
  Window.Invalidate;
end;

var
  M, M2: TMenu;
  { Helper variables for setting up radio items }
  Radio: TMenuItemRadio;
  RadioGroup: TMenuItemRadioGroup;
  Viewport: TCastleViewport;
begin
  InitializeLog;

  Window := TCastleWindow.Create(Application);
  Application.MainWindow := Window; // makes MessageYesNo work OK

  Viewport := TCastleViewport.Create(Application);
  Viewport.FullSize := true;
  Window.Controls.InsertFront(Viewport);

  CreateScene;

  Viewport.Items.Add(Scene);

  // headlight
  Viewport.Camera.Add(TCastleDirectionalLight.Create(Application));

  // good position to see default shape
  Viewport.AssignDefaultCamera;

  { create menu }
  MainMenu := TMenu.Create('Main menu');
  M := TMenu.Create('_File');
    M.Append(TMenuItem.Create('Change to alternative main menu', 100));
    M.Append(TMenuItem.Create('_Exit', 20));
    MainMenu.Append(M);
  M := TMenu.Create('_Color');
    M.AppendRadioGroup([
        '_Red',
        '_Green',
        '_Blue',
        '_Yellow',
        '_White',
        'Gr_ay',
        'B_lack'
      ],
      { BaseIntData: MenuClick should handle Item.IntData values
        from 200 to High(Colors) + 200 }
      200,
      { SelectedIndex, corresponds to dafault color. }
      0,
      { AutoCheckToggle: The clicked radio item will automatically
        have Checked := true, other radio items in group will have Checked := false. }
      true,
      { QuoteCaption: no, because we use _ above for mnemonics. }
      false
    );
    MainMenu.Append(M);
  M := TMenu.Create('_Placement');
    { Radio menu items test }
    { First radio test: simple use AutoCheckToggle := true }
    Radio := TMenuItemRadio.Create('_Top', 21, false, true);
    RadioGroup := Radio.Group; { First: get Group }
    M.Append(Radio);
    Radio := TMenuItemRadio.Create('_Middle', 22, true, true);
    Radio.Group := RadioGroup;
    M.Append(Radio);
    Radio := TMenuItemRadio.Create('_Bottom', 23, false, true);
    Radio.Group := RadioGroup;
    M.Append(Radio);
    { Second radio test: use without AutoCheckToggle := true,
      we will explicitly set Checked. }
    M.Append(TMenuSeparator.Create);
    MenuHorizLeft := TMenuItemRadio.Create('_Left', 25, false, false);
    RadioGroup := MenuHorizLeft.Group;
    M.Append(MenuHorizLeft);
    MenuHorizMiddle := TMenuItemRadio.Create('M_iddle', 26, true, false);
    MenuHorizMiddle.Group := RadioGroup;
    M.Append(MenuHorizMiddle);
    MenuHorizRight := TMenuItemRadio.Create('_Right', 27, true, false);
    MenuHorizRight.Group := RadioGroup;
    M.Append(MenuHorizRight);
    MainMenu.Append(M);
  M := TMenu.Create('_Shape');
    M.Append(TMenuItemChecked.Create('_Filled', 31, Filled, true));
    M2 := TMenu.Create('_More options');
      M2.Append(TMenuItem.Create('_foo with underscore : __', 101));
      M2.Append(TMenuItem.Create('_bar', 102));
      M.Append(M2);
    M.Append(TMenuItem.Create('_Rectangle', 10, 'r'));
    M.Append(TMenuItem.Create('_Triangle',  11, 't'));
    MainMenu.Append(M);
  M := TMenu.Create('_Change menu');
  ChangeableMenu := M;
    M.Append(TMenuItem.Create('Create new _main menu item', 40));
    M.Append(TMenuItem.Create('Create new _submenu here',   41));
    M.Append(TMenuItem.Create('Create new menu _item here', 42));
    M.Append(TMenuSeparator.Create);
    MainMenu.Append(M);

  AlternativeMainMenu := TMenu.Create('Alternative main menu');
  M := TMenu.Create('Second menu');
    M.Append(TMenuItem.Create('Change back to first menu', 101));
    AlternativeMainMenu.Append(M);

  Window.MainMenu := MainMenu;
  { this allows to free MainMenu and AlternativeMainMenu easier at the end }
  Window.OwnsMainMenu := false;

  Window.OnMenuClick := @MenuClick;
  Window.ParseParameters;
  Window.Width := 300;
  Window.Height := 300;
  Window.DepthBits := 0;
  Window.SetDemoOptions(keyF11, CharEscape, true);
  Window.Caption := 'Demo CastleWindow Menu';
  Window.OpenAndRun;

  DestroyScene;
  Window.MainMenu := nil;
  FreeAndNil(MainMenu);
  FreeAndNil(AlternativeMainMenu);
end.
