{ Main state, where most of the application logic takes place.

  Feel free to use this code as a starting point for your own projects.
  (This code is in public domain, unlike most other CGE code which
  is covered by the LGPL license variant, see the COPYING.txt file.) }
unit GameStateMain;

interface

uses Classes,
  CastleVectors, CastleUIState, CastleComponentSerialize,
  CastleUIControls, CastleControls, CastleKeysMouse, CastleTester;

type
  { Main state, where most of the application logic takes place. }
  TStateMain = class(TUIState)
  private
    { Components designed using CGE editor, loaded from gamestatemain.castle-user-interface. }
    LabelFps: TCastleLabel;

    ButtonStartTests: TCastleButton;

    Tester: TCastleTester;

    procedure ClickStartTests(Sender: TObject);

  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
  end;

var
  StateMain: TStateMain;

implementation

{$define CASTLE_TESTER}

uses SysUtils,
  TestCastleURIUtils, TestCastleBoxes, TestCastleVectors, TestCastleCameras,
  TestCastleClassUtils, TestCastleColors, TestCastleComponentSerialize,
  TestCastleCompositeImage, TestCastleControls, TestCastleCubeMaps,
  TestCastleCurves, TestCastleFilesUtils, TestCastleFonts, TestCastleFrustum,
  TestCastleGame, {$ifdef FPC}TestCastleGenericLists, {$endif}
  TestCastleGLVersion, TestCastleImages, TestCastleImagesDraw,
  TestCastleKeysMouse, TestCastleOpeningAndRendering3D, TestCastleParameters;

{ TStateMain ----------------------------------------------------------------- }

procedure TStateMain.ClickStartTests(Sender: TObject);
begin
  Tester.Run;
end;

constructor TStateMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gamestatemain.castle-user-interface';
end;

procedure TStateMain.Start;
var
  TestC: TCastleTestCase;
begin
  inherited;

  { Find components, by name, that we need to access from code }
  ButtonStartTests := DesignedComponent('ButtonStartTests') as TCastleButton;
  ButtonStartTests.OnClick := {$ifdef FPC}@{$endif}ClickStartTests;

  Tester := TCastleTester.Create(FreeAtStop);
  TestC := TCastleTestCase.Create;
  Tester.AddTestCase(TestC);
  Tester.AddTestCase(TTestURIUtils.Create);
  Tester.AddTestCase(TTestCastleBoxes.Create);
  Tester.AddTestCase(TTestCastleVectors.Create);
  Tester.AddTestCase(TTestCameras.Create);
  Tester.AddTestCase(TTestCastleClassUtils.Create);
  Tester.AddTestCase(TTestCastleColors.Create);
  Tester.AddTestCase(TTestCastleComponentSerialize.Create);
  //Tester.AddTestCase(TTestCastleCompositeImage.Create);
  Tester.AddTestCase(TTestCastleControls.Create);
  Tester.AddTestCase(TTestCubeMap.Create);
  Tester.AddTestCase(TTestCastleCurves.Create);
  //Tester.AddTestCase(TTestCastleFilesUtils.Create);
  Tester.AddTestCase(TTestCastleFonts.Create);
  Tester.AddTestCase(TTestCastleFrustum.Create);
  Tester.AddTestCase(TTestGame.Create);
  {$ifdef FPC}Tester.AddTestCase(TTestGenericLists.Create);{$endif}
  Tester.AddTestCase(TTestGLVersion.Create);
  Tester.AddTestCase(TTestImages.Create);
  Tester.AddTestCase(TTestImagesDraw.Create);
  Tester.AddTestCase(TTestKeysMouse.Create);
  Tester.AddTestCase(TTestOpeningAndRendering3D.Create);
  Tester.AddTestCase(TTestParsingParameters.Create);
end;

procedure TStateMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
end;

function TStateMain.Press(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
  if Result then Exit; // allow the ancestor to handle keys

  { This virtual method is executed when user presses
    a key, a mouse button, or touches a touch-screen.

    Note that each UI control has also events like OnPress and OnClick.
    These events can be used to handle the "press", if it should do something
    specific when used in that UI control.
    The TStateMain.Press method should be used to handle keys
    not handled in children controls.
  }

  // Use this to handle keys:
  {
  if Event.IsKey(keyXxx) then
  begin
    // DoSomething;
    Exit(true); // key was handled
  end;
  }
end;

end.
