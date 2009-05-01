unit GLRenderState;

interface

type
  { Common knowledge about currently rendered OpenGL state.
    This is basically just a collection of global variables that
    for whatever reason have to be read/write in various distinct
    parts of the engine. If they don't fit elsewhere, they go here. }
  TGLRenderState = class
  public
    { Value > 0 means we're inside some stencil test (like for
      InShadow = @false pass of shadow volumes). }
    StencilTest: Cardinal;
  end;

var
  RenderState: TGLRenderState;

implementation

uses SysUtils;

initialization
  RenderState := TGLRenderState.Create;
finalization
  FreeAndNil(RenderState);
end.
