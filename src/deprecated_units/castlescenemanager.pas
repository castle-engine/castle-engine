{
  Copyright 2009-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Scene manager (TCastleSceneManager) and viewport (TCastleViewport) classes. }
unit CastleSceneManager deprecated 'use CastleViewport';

{$I castleconf.inc}

interface

uses SysUtils,
  CastleViewport, CastleScene, CastleInputs;

type
  TCastleViewport             = CastleViewport.TCastleViewport;
  TCastleAbstractViewport     = CastleViewport.TCastleViewport;
  {$warnings off} // only to keep deprecated working
  TCastleSceneManager         = CastleViewport.TCastleSceneManager;
  TCastleAbstractViewportList = CastleViewport.TCastleViewportList;
  {$warnings on}
  TRender3DEvent              = CastleViewport.TRenderOnePassEvent;
  TProjectionEvent            = CastleViewport.TProjectionEvent;
  TUseHeadlight               = CastleScene.TUseHeadlight;

  EViewportSceneManagerMissing = class(Exception)
  end deprecated 'this is never raised anymore';

const
  hlOn        = CastleScene.hlOn;
  hlOff       = CastleScene.hlOff;
  hlMainScene = CastleScene.hlMainScene;

function GetInput_Interact: TInputShortcut;
procedure SetInput_Interact(const Value: TInputShortcut);
property Input_Interact: TInputShortcut read GetInput_Interact write SetInput_Interact;

implementation

function GetInput_Interact: TInputShortcut;
begin
  Result := CastleViewport.Input_Interact;
end;

procedure SetInput_Interact(const Value: TInputShortcut);
begin
  CastleViewport.Input_Interact := Value;
end;

end.
