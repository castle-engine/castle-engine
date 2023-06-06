{ Main view, where most of the application logic takes place.

  Feel free to use this code as a starting point for your own projects.
  This template code is in public domain, unlike most other CGE code which
  is covered by BSD or LGPL (see https://castle-engine.io/license). }
unit GameViewMain;

interface

uses Classes,
  CastleVectors, CastleComponentSerialize, CastleGlImages,
  CastleUIControls, CastleControls, CastleKeysMouse, CastleFonts, CastleGlShaders;

type
  { Main view, where most of the application logic takes place. }
  TViewMain = class(TCastleView)
  private
    NonManagedDrawableImageThatDoesntGetItsCustomShaderResetToNilEveryFrame: TDrawableImage;
    NewProgram: TGLSLProgram;
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    LabelFps: TCastleLabel;

    //ImageControl1: TCastleImageControl;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Start; override;
    procedure Render; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
    function Press(const Event: TInputPressRelease): Boolean; override;
  end;

var
  ViewMain: TViewMain;

implementation

uses SysUtils;

{ TViewMain ----------------------------------------------------------------- }

constructor TViewMain.Create(AOwner: TComponent);
var
  VS, FS: String;
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewmain.castle-user-interface';

  VS := 'attribute vec2 vertex;' + LineEnding +
        'attribute vec2 tex_coord;' + LineEnding +
        'uniform vec2 viewport_size;' + LineEnding +
        'varying vec2 tex_coord_frag;' + LineEnding +
        'void main(void)' + LineEnding +
        '{' + LineEnding +
        '  gl_Position = vec4(vertex * 2.0 / viewport_size - vec2(1.0), 0.0, 1.0);' + LineEnding +
        '  tex_coord_frag = tex_coord;' + LineEnding +
        '}' + LineEnding;
  FS := 'varying vec2 tex_coord_frag;' + LineEnding +
        'uniform sampler2D image_texture;' + LineEnding +
        'void main(void)' + LineEnding +
        '{' + LineEnding +
        'gl_FragColor = texture2D(image_texture, tex_coord_frag);' + LineEnding +
        'if (gl_FragColor.x < 0.75) discard; else gl_FragColor = vec4(1.0,1.0,1.0,1.0);' +
        '}' + LineEnding;

  NewProgram := TGLSLProgram.Create;
  NewProgram.Name := 'TDistanceFieldCut';
  NewProgram.AttachVertexShader(VS);
  NewProgram.AttachFragmentShader(FS);
  NewProgram.Link;

  NonManagedDrawableImageThatDoesntGetItsCustomShaderResetToNilEveryFrame := TDrawableImage.Create('castle-data:/$$$TEMP$$$.png');
  NonManagedDrawableImageThatDoesntGetItsCustomShaderResetToNilEveryFrame.CustomShader := NewProgram;
end;

destructor TViewMain.Destroy;
begin
  //NonManagedDrawableImageThatDoesntGetItsCustomShaderResetToNilEveryFrame.CustomShader := nil;
  NonManagedDrawableImageThatDoesntGetItsCustomShaderResetToNilEveryFrame.Free;
  //NewProgram.Free; MEMORY LEAK WEEEEE :) OR JUST CRASHES
  inherited Destroy;
end;

procedure TViewMain.Start;
begin
  inherited;
end;

procedure TViewMain.Render;
begin
  inherited Render;
  NonManagedDrawableImageThatDoesntGetItsCustomShaderResetToNilEveryFrame.Draw(0, 0, 4999, 4999);
end;

procedure TViewMain.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  { This virtual method is executed every frame (many times per second). }
  Assert(LabelFps <> nil, 'If you remove LabelFps from the design, remember to remove also the assignment "LabelFps.Caption := ..." from code');
  LabelFps.Caption := NonManagedDrawableImageThatDoesntGetItsCustomShaderResetToNilEveryFrame.CustomShader.Name;
end;

function TViewMain.Press(const Event: TInputPressRelease): Boolean;
begin
  Result := inherited;
  if Result then Exit; // allow the ancestor to handle keys

  { This virtual method is executed when user presses
    a key, a mouse button, or touches a touch-screen.

    Note that each UI control has also events like OnPress and OnClick.
    These events can be used to handle the "press", if it should do something
    specific when used in that UI control.
    The TViewMain.Press method should be used to handle keys
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
