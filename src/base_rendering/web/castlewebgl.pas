{
  Copyright 2024-2024 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}
{ Access WebGL API from Castle Game Engine compiled in WebAssembly. }
unit CastleWebGL;

interface

procedure TestJobWeb;

implementation

uses SysUtils, JOB.Shared, JOB_Web, JOB.JS,
  CastleLog, CastleUtils;

procedure TestJobWeb;
var
  Canvas: IJSHTMLCanvasElement;
  GL: IJSWebGLRenderingContext;
  ContextWidth, ContextHeight: TGLSizeI;
begin
  Canvas := TJSHTMLCanvasElement.Cast(JSDocument.getElementById('castle-canvas'));

  GL := TJSWebGLRenderingContext.Cast(canvas.getContext('webgl'));
  if GL = nil then
    raise Exception.Create('Failed to load WebGL context from WASM');

  ContextWidth := GL.drawingBufferWidth;
  ContextHeight := GL.drawingBufferHeight;

  WritelnLog(Format('Wasm: Context Width x Height: %d x %d', [
    ContextWidth,
    ContextHeight
  ]));
  WritelnLog(Format('Wasm: Context basic information:' + NL +
    '  Version: %s' + NL +
    '  Shading Language Version: %s' + NL +
    '  Renderer: %s' + NL +
    '  Vendor: %s', [
    GL.GetParameter(TJSWebGLRenderingContext.SHADING_LANGUAGE_VERSION),
    GL.GetParameter(TJSWebGLRenderingContext.RENDERER),
    GL.GetParameter(TJSWebGLRenderingContext.VENDOR),
    GL.GetParameter(TJSWebGLRenderingContext.VERSION)
  ]));
end;

end.
