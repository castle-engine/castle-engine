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

{ Spine bone timelines. }

{$ifdef read_interface}
  TBoneTimeline = class abstract
    Bone: TBone;
    Time: TSingleList;
    // TODO: curve not supported yet
    constructor Create;
    destructor Destroy; override;
  end;

  TBoneTimelineVector2 = class(TBoneTimeline)
    { Position or scale values on the timeline.
      This always has the same length as @link(Time) list. }
    Vectors: TVector2SingleList;
    constructor Create;
    destructor Destroy; override;
  end;

  TBoneTimelineRotate = class(TBoneTimeline)
    { Angle values on the timeline.
      This always has the same length as @link(Time) list. }
    Angles: TSingleList;
    constructor Create;
    destructor Destroy; override;
  end;

  TBoneTimelineList = class(specialize TFPGObjectList<TBoneTimeline>)
  end;
{$endif}

{$ifdef read_implementation}
constructor TBoneTimeline.Create;
begin
  inherited;
  Time := TSingleList.Create;
end;

destructor TBoneTimeline.Destroy;
begin
  FreeAndNil(Time);
  inherited;
end;

constructor TBoneTimelineVector2.Create;
begin
  inherited;
  Vectors := TVector2SingleList.Create;
end;

destructor TBoneTimelineVector2.Destroy;
begin
  FreeAndNil(Vectors);
  inherited;
end;

constructor TBoneTimelineRotate.Create;
begin
  inherited;
  Angles := TSingleList.Create;
end;

destructor TBoneTimelineRotate.Destroy;
begin
  FreeAndNil(Angles);
  inherited;
end;
{$endif}
