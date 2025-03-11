{%MainUnit castletransform.pas}
{
  Copyright 2017-2025 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}

//type
  { Reference another TCastleTransform instance, to render one TCastleTransform
    multiple times within the same viewport. }
  TCastleTransformReference = class(TCastleTransform)
  strict private
    FReference, FRevertReference: TCastleTransform;
    FReferenceObserver, FRevertReferenceObserver: TFreeNotificationObserver;
    FReferenceWrapper: TCastleTransform;
    procedure SetReference(const Value: TCastleTransform);
    procedure SetRevertReference(const Value: TCastleTransform);
    procedure ReferenceFreeNotification(const Sender: TFreeNotificationObserver);
    procedure RevertReferenceFreeNotification(const Sender: TFreeNotificationObserver);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function PropertySections(const PropertyName: String): TPropertySections; override;
  published
    { Reference another TCastleTransform instance, rendering and processing
      it as our child. This makes the referenced instance being visible
      multiple times within the same viewport.

      The referenced instance doesn't have to be present itself in any
      viewport. But if it is present in some viewport,
      it has to be the same viewport as our own.
      In other words, you cannot use the same TCastleTransform instance
      in 2 different viewports.

      To be more precise, you actually cannot use the same TCastleTransform instance
      in 2 different TCastleAbstractRootTransform instances
      (see @link(TCastleTransform.World)).
      When 2 viewports share the same @code(Items) then (and only then)
      the same TCastleTransform instance are present in multiple viewports.
      See https://castle-engine.io/multiple_viewports_to_display_one_world . }
    property Reference: TCastleTransform read FReference write SetReference;

    { Not displayed, used only for the "Revert To Referenced Transform"
      feature in the editor. }
    property RevertReference: TCastleTransform read FRevertReference write SetRevertReference;
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TCastleTransformReference ----------------------------------------------------- }

constructor TCastleTransformReference.Create(AOwner: TComponent);
begin
  inherited;
  FReferenceObserver := TFreeNotificationObserver.Create(Self);
  FReferenceObserver.OnFreeNotification :=
    {$ifdef FPC}@{$endif}ReferenceFreeNotification;

  FRevertReferenceObserver := TFreeNotificationObserver.Create(Self);
  FRevertReferenceObserver.OnFreeNotification :=
    {$ifdef FPC}@{$endif}RevertReferenceFreeNotification;

  { FReference will be child of FReferenceWrapper, not directly of us.
    This way we avoid serializing it easily. }
  FReferenceWrapper := TCastleTransform.Create(Self);
  FReferenceWrapper.SetTransient;
  { This means that TCastleTransformReference will not modify children's Parent,
    which in turn means it will nicely work in CGE editor (both the referenced
    transform directly, and the one wrapped in TCastleTransformReference). }
  FReferenceWrapper.MeaningfulParent := false;
  Add(FReferenceWrapper);
end;

destructor TCastleTransformReference.Destroy;
begin
  inherited;
end;

procedure TCastleTransformReference.ReferenceFreeNotification(const Sender: TFreeNotificationObserver);
begin
  FReference := nil;
end;

procedure TCastleTransformReference.RevertReferenceFreeNotification(const Sender: TFreeNotificationObserver);
begin
  FRevertReference := nil;
end;

procedure TCastleTransformReference.SetReference(const Value: TCastleTransform);

  function HasChild(const Parent, Child: TCastleTransform): Boolean;
  var
    I: Integer;
  begin
    if Parent = Child then
      Exit(true);

    for I := 0 to Parent.Count - 1 do
      if HasChild(Parent[I], Child) then
        Exit(true);

    Result := false;
  end;

begin
  if FReference <> Value then
  begin
    { Check that the operation is valid }
    if Value <> nil then
    begin
      { Note that we check HasChild, not HasParent.
        HasParent would be faster, but would depend on FParent, and we don't want to depend on it
        when things may have multiple parents. }
      if HasChild(Value, Self) then
      begin
        if Value = Self then
          raise Exception.CreateFmt('TCastleTransformReference (%s) cannot have a Reference set to itself', [
            Name
          ])
        else
          raise Exception.CreateFmt('Cannot make a reference to %s (%s), as it is a parent of %s (TCastleTransformReference)', [
            Value.Name,
            Value.ClassName,
            Name
          ]);
      end;

      if (Value.World <> nil) and (World <> nil) and (Value.World <> World) then
        raise Exception.CreateFmt('Cannot make a reference to %s (%s), as it is in a different viewport than %s (TCastleTransformReference)', [
          Value.Name,
          Value.ClassName,
          Name
        ]);
    end;

    if FReference <> nil then
      FReferenceWrapper.Remove(FReference);
    FReference := Value;
    FReferenceObserver.Observed := FReference;
    if FReference <> nil then
      FReferenceWrapper.Add(FReference);
  end;
end;

procedure TCastleTransformReference.SetRevertReference(const Value: TCastleTransform);
begin
  if FRevertReference <> Value then
  begin
    FRevertReference := Value;
    FRevertReferenceObserver.Observed := FRevertReference;
  end;
end;

function TCastleTransformReference.PropertySections(
  const PropertyName: String): TPropertySections;
begin
  if PropertyName = 'Reference' then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

{$endif read_implementation}
