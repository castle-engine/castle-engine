{
  Copyright 2018-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Simple rendering using TCastleImageControl with a custom GLSL shader.

  Note that an alternative way of using custom shaders is to use a scene
  (like TCastle2DScene and TCastle2DSceneManager with a rectangle,
  see https://castle-engine.io/x3d_implementation_geometry2d.php ).
  And then you can assign custom shaders using nodes, see the example
  ../3d_rendering_processing/display_box_custom_shaders.lpr .

  This method is limited to rendering 2D images.
  It uses TCastleImageControl.CustomShader,
  that under the hood uses TGLImage.CustomShader.
  It requires manually creating TGLSLProgram instance and linking it. }

uses SysUtils,
  CastleWindow, CastleControls, CastleGLShaders, CastleTimeUtils,
  CastleFilesUtils, CastleUtils;

var
  Window: TCastleWindowCustom;
  Image: TCastleImageControl;
  Shader: TGLSLProgram;
  LifeTime: TFloatTime;

procedure ApplicationInitialize;
begin
  // simple black background underneath
  Window.Controls.InsertFront(TCastleSimpleBackground.Create(Application));

  Image := TCastleImageControl.Create(Application);
  Image.URL := ApplicationData('test_texture.png');
  Image.Stretch := true;
  Image.FullSize := true;
  Window.Controls.InsertFront(Image);
end;

procedure WindowOpen(Container: TUIContainer);
begin
  { Create TGLSLProgram instance.

    Note that the TGLSLProgram instance may only exist when OpenGL(ES) context
    is open. So it's cleanest to create / destroy TGLSLProgram in

    - Window.OnOpen / Window.OnClose events,
    - or overridden TUIControl.GLContextOpen / TUIControl.GLContextClose.

    This approach will also work reliably on mobile platforms,
    when the OpenGLES context may be lost and recreated in the middle
    of the application too.

    Note that Window.OnOpen occurs after Application.OnInitialize,
    so Image (TCastleImageControl) is already created.
  }

  Shader := TGLSLProgram.Create;
  Shader.AttachVertexShader(
    'attribute vec2 vertex;' + NL +
    'attribute vec2 tex_coord;' + NL +
    'uniform vec2 viewport_size;' + NL +
    'varying vec2 tex_coord_frag;' + NL +
    'void main(void)' + NL +
    '{' + NL +
    '  gl_Position = vec4(vertex * 2.0 / viewport_size - vec2(1.0), 0.0, 1.0);' + NL +
    '  tex_coord_frag = tex_coord;' + NL +
    '}'
  );
  Shader.AttachFragmentShader(
    '#ifdef GL_ES' + NL +
    'precision mediump float;' + NL +
    '#endif' + NL +
    'varying vec2 tex_coord_frag;' + NL +
    'uniform sampler2D texture;' + NL +
    'uniform float life_time;' + NL +
    'void main(void)' + NL +
    '{' + NL +
    '  gl_FragColor = texture2D(texture, tex_coord_frag);' + NL +

    // Uncomment to make simple edge detection
    '#define IMAGE_WIDTH 100' + NL +
    '#define IMAGE_HEIGHT 100' + NL +
    '  gl_FragColor +=' + NL +
    '    texture2D(texture, tex_coord_frag - vec2(0.0, 0.5/IMAGE_HEIGHT)) - texture2D(texture, tex_coord_frag + vec2(0.0, 0.5/IMAGE_HEIGHT)) +' + NL +
    '    texture2D(texture, tex_coord_frag - vec2(0.5/IMAGE_WIDTH, 0.0))  - texture2D(texture, tex_coord_frag + vec2(0.5/IMAGE_WIDTH, 0.0));' + NL +

    // Uncomment to animate rgb
    '  gl_FragColor.rgb = gl_FragColor.rgb * frac(life_time * 0.5);' + NL +

    '}'
  );
  Shader.Link;

  Image.CustomShader := Shader;
end;

procedure WindowClose(Container: TUIContainer);
begin
  FreeAndNil(Shader);
  Image.CustomShader := nil;
end;

procedure WindowUpdate(Container: TUIContainer);
begin
  LifeTime := LifeTime +  Container.Fps.SecondsPassed;
  Shader.Uniform('life_time', uaIgnore).SetValue(LifeTime);
end;

begin
  Window := TCastleWindowCustom.Create(Application);

  Application.OnInitialize := @ApplicationInitialize;
  Window.OnOpen := @WindowOpen;
  Window.OnClose := @WindowClose;
  Window.OnUpdate := @WindowUpdate;

  Window.OpenAndRun;
end.
