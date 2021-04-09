{
  Copyright 2013-2020 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Window with controls for easy navigation on touch interfaces. }
unit CastleWindowTouch;

{$I castleconf.inc}

interface

uses Classes, CastleWindow, CastleControls, CastleCameras,
  CastleUIControls, CastleViewport;

type
  TTouchInterface = CastleViewport.TTouchInterface;

  { Full-featured window for rendering (see @link(TCastleWindow))
    with optional touch controls, to provide a 3D navigation comfortable
    on touch devices (phones, tablets and such).

    In addition to all the goodies of the @link(TCastleWindow) functionality,
    this class can additionally manage one or two TCastleTouchControl instances.
    They will be automatically positioned in the bottom-left
    and bottom-right corners of the screen,
    and will allow the user to navigate using the default @link(TCastleViewport.Navigation SceneManager.Navigation).
    In the simplest case, just set @link(AutomaticTouchInterface) to @true,
    and the touch controls will automatically adjust to the current
    navigation type of the camera (examine, walk, fly...).

    @deprecated Instead of using this automatic composition, it is trivial
    (and much more flexible) to use TCastleWindowBase,
    add there TCastleViewport and TCastleTouchNavigation manually.
  }
  TCastleWindowTouch = class(TCastleWindow)
  strict private
    FTouchNavigation: TCastleTouchNavigation;
    function GetTouchInterface: TTouchInterface;
    function GetAutomaticTouchInterface: Boolean;
    function GetAutomaticWalkTouchCtl: TTouchInterface;
    function GetAutomaticExamineTouchCtl: TTouchInterface;
    procedure SetTouchInterface(const Value: TTouchInterface);
    procedure SetAutomaticTouchInterface(const Value: Boolean);
    procedure SetAutomaticWalkTouchCtl(const Value: TTouchInterface);
    procedure SetAutomaticExamineTouchCtl(const Value: TTouchInterface);
  public
    const
      DefaultAutomaticWalkTouchCtl = TCastleTouchNavigation.DefaultAutoWalkTouchInterface;
      DefaultAutomaticExamineTouchCtl = TCastleTouchNavigation.DefaultAutoExamineTouchInterface;
    constructor Create(AOwner: TComponent); override;
    property TouchInterface: TTouchInterface
      read GetTouchInterface write SetTouchInterface default tiNone;
    property AutomaticTouchInterface: Boolean
      read GetAutomaticTouchInterface write SetAutomaticTouchInterface
      default false;
    property AutomaticWalkTouchCtl: TTouchInterface
      read GetAutomaticWalkTouchCtl write SetAutomaticWalkTouchCtl
      default DefaultAutomaticWalkTouchCtl;
    property AutomaticExamineTouchCtl: TTouchInterface
      read GetAutomaticExamineTouchCtl write SetAutomaticExamineTouchCtl
      default DefaultAutomaticExamineTouchCtl;
  end deprecated 'use TCastleWindowBase and add TCastleTouchNavigation (and maybe set ControlMouseDragMode=true)';

const
  etciNone = tiNone deprecated;
  etciCtlWalkCtlRotate = tiWalkRotate deprecated;
  etciCtlWalkDragRotate = tiWalk deprecated;
  etciCtlFlyCtlWalkDragRotate =  tiFlyWalk deprecated;
  etciCtlPanXYDragRotate = tiPan deprecated;

  tiCtlWalkCtlRotate = tiWalk deprecated;
  tiCtlWalkDragRotate = tiWalkRotate deprecated;
  tiCtlFlyCtlWalkDragRotate = tiFlyWalk deprecated;
  tiCtlPanXYDragRotate = tiPan deprecated;

  tiNone = CastleViewport.tiNone;

implementation

uses SysUtils, CastleUtils;

{ TCastleWindowTouch --------------------------------------------------------- }

constructor TCastleWindowTouch.Create(AOwner: TComponent);
begin
  inherited;
  FTouchNavigation := TCastleTouchNavigation.Create(Self);
  FTouchNavigation.Viewport := SceneManager;
  FTouchNavigation.ControlMouseDragMode := true; // for backward compat
  FTouchNavigation.FullSize := true;
  SceneManager.InsertFront(FTouchNavigation);
end;

function TCastleWindowTouch.GetTouchInterface: TTouchInterface;
begin
  Result := FTouchNavigation.TouchInterface;
end;

function TCastleWindowTouch.GetAutomaticTouchInterface: Boolean;
begin
  Result := FTouchNavigation.AutoTouchInterface;
end;

function TCastleWindowTouch.GetAutomaticWalkTouchCtl: TTouchInterface;
begin
  Result := FTouchNavigation.AutoWalkTouchInterface;
end;

function TCastleWindowTouch.GetAutomaticExamineTouchCtl: TTouchInterface;
begin
  Result := FTouchNavigation.AutoExamineTouchInterface;
end;

procedure TCastleWindowTouch.SetTouchInterface(const Value: TTouchInterface);
begin
  FTouchNavigation.TouchInterface := Value;
end;

procedure TCastleWindowTouch.SetAutomaticTouchInterface(const Value: Boolean);
begin
  FTouchNavigation.AutoTouchInterface := Value;
end;

procedure TCastleWindowTouch.SetAutomaticWalkTouchCtl(const Value: TTouchInterface);
begin
  FTouchNavigation.AutoWalkTouchInterface := Value;
end;

procedure TCastleWindowTouch.SetAutomaticExamineTouchCtl(const Value: TTouchInterface);
begin
  FTouchNavigation.AutoExamineTouchInterface := Value;
end;

end.
