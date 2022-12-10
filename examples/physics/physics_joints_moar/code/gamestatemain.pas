{ Main state, where most of the application logic takes place.

  Feel free to use this code as a starting point for your own projects.
  (This code is in public domain, unlike most other CGE code which
  is covered by the LGPL license variant, see the COPYING.txt file.) }
unit GameStateMain;

interface

uses Classes,
  CastleVectors, CastleUIState, CastleComponentSerialize,
  CastleUIControls, CastleControls, CastleKeysMouse, CastleViewport;

type
  { Main state, where most of the application logic takes place. }
  TStateMain = class(TUIState)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    LabelFps: TCastleLabel;
    ButtonHinge: TCastleButton;
    ButtonBall: TCastleButton;
    ButtonGrab: TCastleButton;
    ButtonRope: TCastleButton;
    ButtonDistance: TCastleButton;
    DesignContent: TCastleDesign;
  private
    procedure ClickButtonHinge(Sender: TObject);
    procedure ClickButtonBall(Sender: TObject);
    procedure ClickButtonGrab(Sender: TObject);
    procedure ClickButtonRope(Sender: TObject);
    procedure ClickButtonDistance(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
  end;

var
  StateMain: TStateMain;

implementation

uses SysUtils;

{ TStateMain ----------------------------------------------------------------- }

constructor TStateMain.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gamestatemain.castle-user-interface';
end;

procedure TStateMain.Start;
begin
  inherited;

  ButtonHinge.OnClick := {$ifdef FPC}@{$endif} ClickButtonHinge;
  ButtonBall.OnClick := {$ifdef FPC}@{$endif} ClickButtonBall;
  ButtonGrab.OnClick := {$ifdef FPC}@{$endif} ClickButtonGrab;
  ButtonRope.OnClick := {$ifdef FPC}@{$endif} ClickButtonRope;
  ButtonDistance.OnClick := {$ifdef FPC}@{$endif} ClickButtonDistance;
end;

procedure TStateMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  { This virtual method is executed every frame.}
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;
end;

procedure TStateMain.ClickButtonHinge(Sender: TObject);
begin
  { Each joint demo is in a separate file, and we switch between them
    by changing DesignContent.Url.

    This is in contrast to another solution:
    having multiple controls all in the gamestatemain.castle-user-interface
    design and switching their existence like ViewporHinge.Exists := true/false.

    Using the DesignContent.Url is better in this case because:

    - It means that each change of demo restarts this demo
      (e.g. go to "Hinge", then "Ball", then "Hinge" again -- the "Hinge"
      demo will start again). This is desirable in case of this demo.

    - It's a bit easier to design and test in CGE editor:
      each joint demo is separate, must use separate components,
      and you can run the simulation of it.
  }
  DesignContent.Url := 'castle-data:/viewport_hinge.castle-user-interface';
end;

procedure TStateMain.ClickButtonBall(Sender: TObject);
begin
  DesignContent.Url := 'castle-data:/viewport_ball.castle-user-interface';
end;

procedure TStateMain.ClickButtonGrab(Sender: TObject);
begin
  DesignContent.Url := 'castle-data:/viewport_grab.castle-user-interface';
end;

procedure TStateMain.ClickButtonRope(Sender: TObject);
begin
  DesignContent.Url := 'castle-data:/viewport_rope.castle-user-interface';
end;

procedure TStateMain.ClickButtonDistance(Sender: TObject);
begin
  DesignContent.Url := 'castle-data:/viewport_distance.castle-user-interface';
end;

end.
