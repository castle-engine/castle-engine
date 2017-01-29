{
  Copyright 2008-2017 Michalis Kamburelis.

  This file is part of "the rift".

  "the rift" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "the rift" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "the rift"; if not, write to the Free Software
  Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA

  ----------------------------------------------------------------------------
}

{ }
unit RiftLoadable;

interface

uses X3DNodes;

type
  { Base class for anything that needs some time-consuming load/unload
    functionality. }
  TLoadable = class
  private
    FLoaded: boolean;
  protected
    procedure LoadInternal(const BaseLights: TLightInstancesList); virtual;
    procedure UnLoadInternal; virtual;
  public
    destructor Destroy; override;

    { This will do the time-consuming work of initializing this object,
      including loading it's VRML scenes, animations etc.

      This requires that game OpenGL context is already initialized
      (and should be kept initialized until UnLoad call).

      They are silently NO-OP when they are already loaded/unloaded.

      UnLoad will automatically be called when destructing.

      @italic(Descendants implementors:) override LoadInternal,
      UnLoadInternal, where you know that you really should load/unload.
      UnLoadInternal must be implemented such that it works even on partially
      loaded state (as it may be used in various finalization steps).

      @groupBegin }
    procedure Load(const BaseLights: TLightInstancesList);
    procedure UnLoad;
    property Loaded: boolean read FLoaded;
    { @groupEnd }

    { How many times Load will call Progress.Step.
      This class returns 1, each descendant should add inherited to
      it's own value . So you can be sure that this is always >= 1. }
    function LoadSteps: Cardinal; virtual;
  end;

implementation

uses CastleProgress;

destructor TLoadable.Destroy;
begin
  UnLoad;
  inherited;
end;

procedure TLoadable.LoadInternal(const BaseLights: TLightInstancesList);
begin
end;

procedure TLoadable.UnLoadInternal;
begin
end;

procedure TLoadable.Load(const BaseLights: TLightInstancesList);
begin
  if Loaded then
  begin
    Progress.Step(LoadSteps);
    Exit;
  end;

  Progress.Step;
  try
    LoadInternal(BaseLights);
  except
    UnLoad;
    raise;
  end;

  FLoaded := true;
end;

procedure TLoadable.UnLoad;
begin
  if not Loaded then Exit;

  UnLoadInternal;

  FLoaded := false;
end;

function TLoadable.LoadSteps: Cardinal;
begin
  Result := 1;
end;

end.
