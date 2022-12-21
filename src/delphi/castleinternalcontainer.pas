{
  Copyright 2022-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Container class TCastleContainerEasy,
  that is easier to use than TCastleContainer.
  In particular it relies on

  - private OpenGL(ES) context handled by TGLContext descendants

  - with requirements expressed by public TGLContextRequirements.

  TODO: It is our long-term plan to merge TCastleContainerEasy capabilities
  into TCastleContainer. Thus, the implementation done here will be shared
  by TCastleWindow (all backends) and LCL TCastleControl,
  unifying them more.
  Right now, TCastleWindow and LCL TCastleControl do some things manually
  for historic purposes, e.g. they expose GL context requirements
  like TCastleWindow.DepthBits,
  and they expose callbacks like TCastleWindow.OnRender that we don't advise to use
  (use TUIState instead or TCastleUserInterface.OnRender to do rendering).
  Nowadays we know that

  - API of classes like TCastleWindow / TCastleControl should be simpler and just expose
    Container for everything reasonable.

  - Container also should delegate to TUIState and in turn to TCastleUserInterface
    for everything reasonable.

  TODO: Rename FContext:TGLContext here to not confuse with Container.Context:TRenderContext.
}
unit CastleInternalContainer;

{$I castleconf.inc}

interface

uses Classes, Generics.Collections,
  CastleRectangles, CastleImages,
  CastleUIControls, CastleInternalContextBase, CastleInternalContextWgl;

type
  TCastleContainerEasy = class(TCastleContainer)
  strict private
    FRequirements: TGLContextRequirements;
    FContext: TGLContextWGL;
    FGLInitialized: Boolean;
    { Copy of Requirements.DoubleBuffer when the context was created. }
    FEffectiveDoubleBuffer: Boolean;
    procedure MakeContextCurrent;
  protected
    { Adjust context parameters right before usage. }
    procedure AdjustContext(const AContext: TGLContextWGL); virtual;

    { Call these methods from final components that wrap TCastleContainerEasy,
      like TCastleControl, TCastleWindow. }
    procedure CreateContext;
    procedure DestroyContext;
    procedure DoRender;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GLInitialized: boolean; override;
    function SaveScreen(const SaveRect: TRectangle): TRGBImage; overload; override;

    { Context required parameters. }
    property Requirements: TGLContextRequirements read FRequirements;
  end;

  TCastleContainerEasyList = {$ifdef FPC}specialize{$endif} TObjectList<TCastleContainerEasy>;

implementation

uses SysUtils,
  CastleRenderContext, CastleGLUtils, CastleApplicationProperties, CastleGLImages,
  CastleGLVersion;

var
  { All TCastleContainerEasy instances created.

    We use this to share OpenGL contexts,
    as all OpenGL contexts in our engine must share OpenGL resources
    (our OnGLContextOpen and such callbacks depend on it,
    and it makes implementation much easier). }
  ContainersList: TCastleContainerEasyList;

  { Tracks how many containers on ContainersList have GL context initialized. }
  ContainersOpen: Cardinal;

constructor TCastleContainerEasy.Create(AOwner: TComponent);
begin
  inherited;

  FRequirements := TGLContextRequirements.Create(Self);
  FRequirements.Name := 'Requirements';
  FRequirements.SetSubComponent(true);

  FContext := TGLContextWGL.Create;
  FContext.WindowCaption := 'Castle'; // TODO: invented, check it is OK, with MultiSampling > 1
  FContext.WndClassName := 'Castle'; // TODO: invented, check it is OK, with MultiSampling > 1

  ContainersList.Add(Self);
end;

destructor TCastleContainerEasy.Destroy;
begin
  FreeAndNil(FContext);
  if ContainersList <> nil then
    ContainersList.Remove(Self);
  inherited;
end;

procedure TCastleContainerEasy.MakeContextCurrent;
begin
  RenderContext := Context;
  FContext.MakeCurrent;
end;

function TCastleContainerEasy.SaveScreen(const SaveRect: TRectangle): TRGBImage;

  { Color buffer where we draw, and from which it makes sense to grab pixels. }
  function SaveScreenBuffer: TColorBuffer;
  begin
    if FEffectiveDoubleBuffer then
      Result := cbBack
    else
      Result := cbFront;
  end;

begin
  MakeContextCurrent;
  EventBeforeRender;
  EventRender;
  Result := SaveScreen_NoFlush(Rect, SaveScreenBuffer);
end;

procedure TCastleContainerEasy.CreateContext;
begin
  if not FGLInitialized then
  begin
    FGLInitialized := true;

    AdjustContext(FContext);

    FContext.ContextCreate(FRequirements);

    FEffectiveDoubleBuffer := Requirements.DoubleBuffer;

    // initialize CGE OpenGL resources
    MakeContextCurrent;
    GLInformationInitialize;
    RenderContext.Viewport := Rect;
    ApplicationProperties._GLContextEarlyOpen;

    Inc(ContainersOpen);
    // Note that this will cause ApplicationProperties._GLContextOpen if necessary
    EventOpen(ContainersOpen);
    EventResize;
    Invalidate;
  end;
end;

procedure TCastleContainerEasy.AdjustContext(const AContext: TGLContextWGL);
begin
end;

procedure TCastleContainerEasy.DestroyContext;
begin
  if FGLInitialized then
  begin
    EventClose(ContainersOpen);
    Dec(ContainersOpen);
    FGLInitialized := false;
  end;
  inherited;
end;

function TCastleContainerEasy.GLInitialized: boolean;
begin
  Result := FGLInitialized;
end;

procedure TCastleContainerEasy.DoRender;
begin
  MakeContextCurrent;

  EventBeforeRender;
  Fps._RenderBegin;
  try
    EventRender;
    if GLVersion.BuggySwapNonStandardViewport then
      RenderContext.Viewport := Rect;
    FContext.SwapBuffers;

    // Note that calling Invalidate from RenderContext is not allowed,
    // it doesn't play OK with LCL or VCL.
    // if AutoRedisplay then Invalidate;
  finally Fps._RenderEnd end;
end;

initialization
  ContainersList := TCastleContainerEasyList.Create(false);
  //TODO: InitializeClipboard;
  //TODO: OnMainContainer := @TCastleControl(nil).GetMainContainer;
finalization
  //TODO: OnMainContainer := nil;
  FreeAndNil(ContainersList);
end.
