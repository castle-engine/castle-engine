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
  { How do we treat the transformation of @link(TCastleTransformReference.Reference)
    when rendering TCastleTransformReference. }
  TReferenceTransformation = (
    { Transformation of the referenced object
      (@link(TCastleTransformReference.Reference))
      is completely ignored when it is rendered inside @link(TCastleTransformReference).

      This means you can translate, rotate, scale the referenced object
      without it having any effect on how it is displayed inside
      @link(TCastleTransformReference).

      Note that we ignore only the @italic(direct) transformation of the
      referenced object. We never ignore transformations inside its children. }
    rtIgnoreTransform,

    { Translation (movement) of the referenced object
      (@link(TCastleTransformReference.Reference)) is ignored
      when it is rendered inside @link(TCastleTransformReference).

      The rest (rotation, scaling) of the transformation is applied.

      This means you can translate the referenced object
      without it having any effect on how it is displayed inside
      @link(TCastleTransformReference).
      However, rotating or scaling it will rotate/scale all the instances. }
    rtIgnoreTranslation,

    { Transformation of the referenced object
      (@link(TCastleTransformReference.Reference))
      is completely applied when it is rendered inside @link(TCastleTransformReference).

      This means that moving, rotating, scaling the referenced object
      will also move every instance of it inside @link(TCastleTransformReference).

      This was the default engine behavior in Castle Game Engine version
      7.0-alpha.3 before 2025-03-06. }
    rtDoNotIgnore
  );

  { Reference another TCastleTransform instance, to render one TCastleTransform
    multiple times within the same viewport. }
  TCastleTransformReference = class(TCastleTransform)
  strict private
    FReference: TCastleTransform;
    FReferenceObserver: TFreeNotificationObserver;
    FReferenceWrapper: TCastleTransform;
    FReferenceTransformation: TReferenceTransformation;
    { Last frame FReference.InverseTransform (that was used to calculate
      FReferenceWrapper transform).

      Only valid if LastReferenceInverseTransformValid.

      Using this is an optimization to avoid recalculating MatrixDecompose,
      sensible because in most cases and frames the FReference.InverseTransform
      stays equal. }
    LastReferenceInverseTransform: TMatrix4;
    LastReferenceInverseTransformValid: Boolean;
    procedure SetReference(const Value: TCastleTransform);
    procedure ReferenceFreeNotification(const Sender: TFreeNotificationObserver);
  protected
    procedure LocalRender(const Params: TRenderParams); override;
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

    { How do we treat the transformation of @link(TCastleTransformReference.Reference)
      when rendering TCastleTransformReference.

      @unorderedList(
        @item(By default this is @link(rtIgnoreTransform),
          which means that moving, rotating or scaling the referenced object has
          no effect on how it is displayed inside TCastleTransformReference referring
          to it.)
        @item(You can change it to @link(rtIgnoreTranslation)
          (moving referenced object has
          no effect, but rotating or scaling it will affect all instances).)
        @item(You can also change it to @link(rtDoNotIgnore)
          (moving, rotating, scaling the referenced object
          will also move every instance of it inside TCastleTransformReference).)
      )

      @seealso TReferenceTransformation }
    property ReferenceTransformation: TReferenceTransformation
      read FReferenceTransformation write FReferenceTransformation
      default rtIgnoreTransform;
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TCastleTransformReference ----------------------------------------------------- }

constructor TCastleTransformReference.Create(AOwner: TComponent);
begin
  inherited;
  FReferenceObserver := TFreeNotificationObserver.Create(Self);
  FReferenceObserver.OnFreeNotification := {$ifdef FPC}@{$endif}ReferenceFreeNotification;

  { FReference will be child of FReferenceWrapper, not directly of us.
    This way we avoid serializing it easily.
    Also this allows to implement ReferenceTransformation = rtIgnoreXxx,
    by modifying transformation of FReferenceWrapper in LocalRender. }

  FReferenceWrapper := TCastleTransform.Create(Self);
  FReferenceWrapper.SetTransient;
  { This means that FReferenceWrapper will not be set as FReference.Parent,
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

function TCastleTransformReference.PropertySections(
  const PropertyName: String): TPropertySections;
begin
  if ArrayContainsString(PropertyName, [
       'Reference', 'ReferenceTransformation'
     ]) then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

procedure TCastleTransformReference.LocalRender(const Params: TRenderParams);
var
  InvertRefTranslation: TVector3;
  InvertReftRotation: TVector4;
  InvertRefScale: TVector3;
  NewReferenceInverseTransformValid: Boolean;
  NewReferenceInverseTransform: TMatrix4;
begin
  NewReferenceInverseTransformValid := false;

  if FReference <> nil then
  begin
    Assert(FReferenceWrapper <> nil);
    case ReferenceTransformation of
      rtIgnoreTransform:
        begin
          NewReferenceInverseTransformValid := true;
          NewReferenceInverseTransform := FReference.InverseTransform;
          if (not LastReferenceInverseTransformValid) or
             (not TMatrix4.PerfectlyEquals(LastReferenceInverseTransform, NewReferenceInverseTransform)) then
          begin
            MatrixDecompose(NewReferenceInverseTransform,
              InvertRefTranslation, InvertReftRotation, InvertRefScale);
            LastReferenceInverseTransform := NewReferenceInverseTransform;
            FReferenceWrapper.Translation := InvertRefTranslation;
            FReferenceWrapper.Rotation := InvertReftRotation;
            FReferenceWrapper.Scale := InvertRefScale;
          end;
        end;
      rtIgnoreTranslation:
        begin
          FReferenceWrapper.Translation := -FReference.Translation;
          FReferenceWrapper.Rotation := TVector4.Zero;
          FReferenceWrapper.Scale := NoScale;
        end;
      else
        begin
          FReferenceWrapper.Translation := TVector3.Zero;
          FReferenceWrapper.Rotation := TVector4.Zero;
          FReferenceWrapper.Scale := NoScale;
        end;
    end;
  end;

  LastReferenceInverseTransformValid := NewReferenceInverseTransformValid;

  inherited;
end;

{$endif read_implementation}
