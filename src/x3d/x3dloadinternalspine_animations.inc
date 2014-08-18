{
  Copyright 2014-2014 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Spine animations. }

{$ifdef read_interface}
  TAnimation = class
    Name: string;
    BoneTimelines: TBoneTimelineList;
    constructor Create;
    destructor Destroy; override;
    //procedure Parse(const Json: TJSONObject);
  end;

  TAnimationList = class(specialize TFPGObjectList<TAnimation>)
  end;
{$endif}

{$ifdef read_implementation}
constructor TAnimation.Create;
begin
  inherited;
  BoneTimelines := TBoneTimelineList.Create;
end;

destructor TAnimation.Destroy;
begin
  FreeAndNil(BoneTimelines);
  inherited;
end;
{$endif}
