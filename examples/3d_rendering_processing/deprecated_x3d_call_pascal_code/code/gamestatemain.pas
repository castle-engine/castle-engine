{
  Copyright 2003-2021 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}
{ Main state, where most of the application logic takes place. }
unit GameStateMain;

interface

uses Classes,
  CastleVectors, CastleUIState, CastleComponentSerialize,
  CastleUIControls, CastleControls, CastleKeysMouse, CastleScene,
  X3DNodes, X3DFields, X3DTime;

type
  { Main state, where most of the application logic takes place. }
  TStateMain = class(TUIState)
  private
    { Components designed using CGE editor, loaded from gamestatemain.castle-user-interface. }
    LabelFps: TCastleLabel;
    MainScene: TCastleScene;

    procedure ScriptTouchInitialize(Value: TX3DField; const Time: TX3DTime);
    procedure ScriptTouch(Value: TX3DField; const Time: TX3DTime);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
  end;

var
  StateMain: TStateMain;

implementation

uses SysUtils,
  CastleLog;

{ TStateMain ----------------------------------------------------------------- }

constructor TStateMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gamestatemain.castle-user-interface';
end;

procedure TStateMain.Start;
begin
  inherited;

  { Find components, by name, that we need to access from code }
  LabelFps := DesignedComponent('LabelFps') as TCastleLabel;
  MainScene := DesignedComponent('MainScene') as TCastleScene;

  { initialize events procesing }
  MainScene.RegisterCompiledScript('touch_initialize',  {$ifdef FPC}@{$endif} ScriptTouchInitialize);
  MainScene.RegisterCompiledScript('touch', {$ifdef FPC}@{$endif} ScriptTouch);

  { initialize ProcessEvents *after* attaching ScriptTouchInitialize, to receive it }
  MainScene.ProcessEvents := true;
end;

procedure TStateMain.ScriptTouchInitialize(Value: TX3DField; const Time: TX3DTime);
begin
  WritelnLog(Format('Script is initialized (absolute time: %f, time since load: %f)', [
    Time.Seconds,
    Time.Seconds - MainScene.TimeAtLoad
  ]));
end;

procedure TStateMain.ScriptTouch(Value: TX3DField; const Time: TX3DTime);
begin
  WritelnLog(Format('Touch! (absolute time: %f, time since load: %f)', [
    Time.Seconds,
    Time.Seconds - MainScene.TimeAtLoad
  ]));
end;

procedure TStateMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  { This virtual method is executed every frame.}
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;
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
