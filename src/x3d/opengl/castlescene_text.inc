{%MainUnit castlescene.pas}
{
  Copyright 2021-2021 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ TCastleText. }

{$ifdef read_interface}

type
  { Text (as TCastleTransform, inside TCastleViewport). }
  TCastleText = class(TCastleAbstractPrimitive)
  strict private
    FText: TStrings;
    FCaptionTranslate: Boolean;
    FDoubleSided: Boolean;
    TextNode: TTextNode;
    function GetCaption: String;
    procedure SetCaption(const Value: String);
    procedure TextChange(Sender: TObject);
    procedure SetText(const Value: TStrings);
    procedure SetDoubleSided(const Value: Boolean);
  protected
    procedure TranslateProperties(const TranslatePropertyEvent: TTranslatePropertyEvent); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function PropertySections(const PropertyName: String): TPropertySections; override;
  published
    { Text displayed, expressed by a simple String.
      This is just a shortcut to get/set @link(Text) as a single String.

      Use LineEnding or NL constant when setting this to indicate a newline.
      The two examples below are equivalent:

      @longCode(#
        // one way
        MyText.Text.Clear;
        MyText.Text.Add('First line');
        MyText.Text.Add('Second line');

        // alternative way to do the same
        MyText.Caption := 'First line' + LineEnding + 'Second line';
      #)
    }
    property Caption: String read GetCaption write SetCaption stored false;

    { Text displayed, with each line as a separate String.
      Setting this property merely copies the contents using TStrings.Assign. }
    property Text: TStrings read FText write SetText;

    { Should the @link(Caption) be localized (translated into other languages).
      Determines if the property is enumerated by @link(TCastleComponent.TranslateProperties),
      which affects the rest of localization routines. }
    property CaptionTranslate: Boolean read FCaptionTranslate write FCaptionTranslate default true;

    { Text is by default unlit. }
    property Material default pmUnlit;

    { When this is @true, the object is visible (and lit) from both sides.

      Note that the default for TCastleText is different than the default for most other primitives,
      like TCastleBox and TCastleSphere: by default text is double-sided,
      while most other primitives by default are not double-sided.
      Set this property to @false for more performance in case camera may be behind the text.
      The reason for different default is that text object doesn't have an obvious "interior",
      unlike a box of sphere,
      so you can more easily land behind the text (and get confused why is it not displayed).
      This follows X3D TTextNode.Solid default. }
    property DoubleSided: Boolean read FDoubleSided write SetDoubleSided default true;
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TCastleText ---------------------------------------------------------------- }

constructor TCastleText.Create(AOwner: TComponent);
begin
  inherited;

  TextNode := TTextNode.Create;
  ShapeNode.Geometry := TextNode;

  { default properties values }
  Material := pmUnlit;
  FText := TCastleStringList.Create;
  (FText as TCastleStringList).OnChange := {$ifdef CASTLE_OBJFPC}@{$endif} TextChange;
  FCaptionTranslate := true;
  FDoubleSided := true;
end;

destructor TCastleText.Destroy;
begin
  inherited;
end;

function TCastleText.PropertySections(const PropertyName: String): TPropertySections;
begin
  case PropertyName of
    'Caption', 'Text', 'DoubleSided':
      Result := [psBasic];
    else
      Result := inherited PropertySections(PropertyName);
  end;
end;

function TCastleText.GetCaption: String;
begin
  Result := TrimEndingNewline(Text.Text);
end;

procedure TCastleText.SetCaption(const Value: String);
begin
  Text.Text := Value;
end;

procedure TCastleText.TextChange(Sender: TObject);
begin
  TextNode.SetString(FText as TCastleStringList);
end;

procedure TCastleText.SetText(const Value: TStrings);
begin
  FText.Assign(Value);
end;

procedure TCastleText.TranslateProperties(
  const TranslatePropertyEvent: TTranslatePropertyEvent);
var
  S: String;
begin
  if CaptionTranslate and (Caption <> '') then
  begin
    S := Caption;
    TranslatePropertyEvent(Self, 'Caption', S);
    Caption := S;
  end;
end;

procedure TCastleText.SetDoubleSided(const Value: Boolean);
begin
  if FDoubleSided <> Value then
  begin
    FDoubleSided := Value;
    TextNode.Solid := not Value;
  end;
end;

{$endif read_implementation}
