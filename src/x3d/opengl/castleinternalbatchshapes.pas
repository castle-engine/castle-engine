{
  Copyright 2019-2019 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Batch shapes (combine multiple shapes into one) (TBatchShapes). }
unit CastleInternalBatchShapes;

{$I castleconf.inc}

interface

uses CastleSceneInternalShape, CastleShapes;

type
  TBatchShapes = class
  strict private
    FCollected: TShapeList;
  public
    constructor Create;
    destructor Destroy; override;

    { Merge given shape into the @link(Collected) shapes.
      During this, the shape may merge with another shape into a single, larger
      shape. Returns @true if the shape was added to @link(Collected),
      otherwise it was not, and should be rendered by the caller immediately
      without the help of batching. }
    function Collect(const Shape: TGLShape): Boolean;

    { Currently collected shapes by @link(Collect). }
    property Collected: TShapeList read FCollected;

    { Release all shapes and clear the @link(Collected) list. }
    procedure FreeCollected;
  end;

implementation

uses SysUtils;

constructor TBatchShapes.Create;
begin
  inherited;
  FCollected := TShapeList.Create;
  FCollected.OwnsObjects := true;
end;

destructor TBatchShapes.Destroy;
begin
  FreeAndNil(FCollected);
  inherited;
end;

function TBatchShapes.Collect(const Shape: TGLShape): Boolean;
begin
  Result := false;
end;

procedure TBatchShapes.FreeCollected;
begin
  FCollected.Clear;
  // TODO
end;

end.
