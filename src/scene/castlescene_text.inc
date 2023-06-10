{%MainUnit castlescene.pas}
{
  Copyright 2021-2023 Michalis Kamburelis.

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
  { Text that is displayed and transformed as TCastleTransform,
    inside TCastleViewport, and can be manipulated in 3D.

    See @url(https://castle-engine.io/manual_text.php
    manual about displaying text and fonts).
    See @url(https://castle-engine.io/viewport_and_scenes#_text_tcastletext
    manual about TCastleText in a viewport). }
  TCastleText = class(TCastleAbstractPrimitive)
  strict private
    FText: TStrings;
    FCaptionTranslate: Boolean;
    FDoubleSided: Boolean;
    TextNode: TTextNode;
    FontStyleNode: TFontStyleNode;
    FAlignment: THorizontalPosition;
    FVerticalAlignment: TVerticalPosition;
    FSize, FSpacing: Single;
    FCustomFont: TCastleFont;
    FCustomFontObserver: TFreeNotificationObserver;
    function GetCaption: String;
    procedure SetCaption(const Value: String);
    procedure TextChange(Sender: TObject);
    procedure SetText(const Value: TStrings);
    procedure SetDoubleSided(const Value: Boolean);
    procedure SetAlignment(const Value: THorizontalPosition);
    procedure SetVerticalAlignment(const Value: TVerticalPosition);
    procedure SetSize(const Value: Single);
    procedure SetSpacing(const Value: Single);
    procedure SetCustomFont(const Value: TCastleFont);
    procedure CustomFontFreeNotification(const Sender: TFreeNotificationObserver);
    procedure CustomFontSizeChanged(Sender: TObject);
    procedure FontStyleFontChanged;
  protected
    procedure TranslateProperties(const TranslatePropertyEvent: TTranslatePropertyEvent); override;
    function GetInternalText: String; override;
    procedure SetInternalText(const Value: String); override;
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

    { Horizontal alignment of the text. }
    property Alignment: THorizontalPosition
      read FAlignment write SetAlignment default hpLeft;

    { Vertical alignment of the text. }
    property VerticalAlignment: TVerticalPosition
      read FVerticalAlignment write SetVerticalAlignment default vpBottom;

    { Size of the text, given as height of a single line of text.
      See https://www.web3d.org/specifications/X3Dv4Draft/ISO-IEC19775-1v4-CD/Part01/components/text.html#FontStyle
      for an illustrative image. }
    property Size: Single read FSize write SetSize {$ifdef FPC}default 1.0{$endif};

    { Spacing between lines of text, to make lines more distant (with values > 1) or more squeezed (with values < 1).
      See https://www.web3d.org/specifications/X3Dv4Draft/ISO-IEC19775-1v4-CD/Part01/components/text.html#FontStyle
      for an illustrative image. }
    property Spacing: Single read FSpacing write SetSpacing {$ifdef FPC}default 1.0{$endif};

    { Adjust the font. }
    property CustomFont: TCastleFont read FCustomFont write SetCustomFont;
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TCastleText ---------------------------------------------------------------- }

constructor TCastleText.Create(AOwner: TComponent);
begin
  inherited;

  TextNode := TTextNode.Create;
  ShapeNode.Geometry := TextNode;

  FontStyleNode := TFontStyleNode.Create;
  FontStyleNode.Family := ffSans; // not modifiable now, as we may disable loading by default serif/typewriter fonts in X3D
  FontStyleNode.Justify := fjBegin;
  FontStyleNode.JustifyMinor := fjEnd;
  TextNode.FontStyle := FontStyleNode;

  { default properties values }
  Material := pmUnlit;
  FText := TCastleStringList.Create;
  (FText as TCastleStringList).OnChange := {$ifdef FPC}@{$endif} TextChange;
  FCaptionTranslate := true;
  FDoubleSided := true;
  FSize := 1;
  FSpacing := 1;
  FAlignment := hpLeft;
  FVerticalAlignment := vpBottom;

  FCustomFontObserver := TFreeNotificationObserver.Create(Self);
  FCustomFontObserver.OnFreeNotification := {$ifdef FPC}@{$endif}CustomFontFreeNotification;
end;

destructor TCastleText.Destroy;
begin
  { Since our instance will become invalid soon,
    FCustomFont should not notify us anymore.
    Testcase: examples/localization/gettext/ }
  if FCustomFont <> nil then
    FCustomFont.RemoveFontSizeChangeNotification(
      {$ifdef FPC}@{$endif}CustomFontSizeChanged);

  FreeAndNil(FText);
  inherited;
end;

function TCastleText.PropertySections(const PropertyName: String): TPropertySections;
begin
  if (PropertyName = 'Caption') or
     (PropertyName = 'Text') or
     (PropertyName = 'DoubleSided') or
     (PropertyName = 'Size') or
     (PropertyName = 'Spacing') or
     (PropertyName = 'Alignment') or
     (PropertyName = 'VerticalAlignment') or
     (PropertyName = 'CustomFont') then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
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
  TextNode.SetText(FText as TCastleStringList);
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

procedure TCastleText.SetAlignment(const Value: THorizontalPosition);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    case Value of
      hpLeft  : FontStyleNode.Justify := fjBegin;
      hpMiddle: FontStyleNode.Justify := fjMiddle;
      hpRight : FontStyleNode.Justify := fjEnd;
      {$ifndef COMPILER_CASE_ANALYSIS}
      else raise EInternalError.Create('TCastleText.SetAlignment:Value?');
      {$endif}
    end;
  end;
end;

procedure TCastleText.SetVerticalAlignment(const Value: TVerticalPosition);
begin
  if FVerticalAlignment <> Value then
  begin
    FVerticalAlignment := Value;
    case Value of
      vpBottom: FontStyleNode.JustifyMinor := fjEnd;
      vpMiddle: FontStyleNode.JustifyMinor := fjMiddle;
      vpTop   : FontStyleNode.JustifyMinor := fjBegin;
      { Note that achieving fjFirst is not possible, and that is deliberate.
        I think fjFirst is a needless complication.
        It is much more important to have API similar to TCastleLabel. }
      {$ifndef COMPILER_CASE_ANALYSIS}
      else raise EInternalError.Create('TCastleText.SetVerticalAlignment:Value?');
      {$endif}
    end;
  end;
end;

procedure TCastleText.SetSize(const Value: Single);
begin
  if FSize <> Value then
  begin
    FSize := Value;
    FontStyleNode.Size := Value;
  end;
end;

procedure TCastleText.SetSpacing(const Value: Single);
begin
  if FSpacing <> Value then
  begin
    FSpacing := Value;
    FontStyleNode.Spacing := Value;
  end;
end;

function TCastleText.GetInternalText: String;
begin
  Result := Caption;
end;

procedure TCastleText.SetInternalText(const Value: String);
begin
  Caption := Value;
end;

procedure TCastleText.CustomFontFreeNotification(const Sender: TFreeNotificationObserver);
begin
  FCustomFont := nil;
end;

procedure TCastleText.CustomFontSizeChanged(Sender: TObject);
begin
  { When CustomFont properties, like OptimalSize, changed -- rebuild also displayed font texture. }
  FontStyleFontChanged;
end;

procedure TCastleText.FontStyleFontChanged;
var
  I: Integer;
  Shape: TShape;
begin
  // TextNode.FontChanged; // no need to, TShape.Changed will call it, also freeing TextProxy
  for I := 0 to TShapeTree.AssociatedShapesCount(TextNode) - 1 do
  begin
    Shape := TShapeTree.AssociatedShape(TextNode, I) as TShape;
    Shape.Changed(false, [chFontStyleFontChanged]);
  end;
end;

procedure TCastleText.SetCustomFont(const Value: TCastleFont);
begin
  if FCustomFont <> Value then
  begin
    if FCustomFont <> nil then
      FCustomFont.RemoveFontSizeChangeNotification(
        {$ifdef FPC}@{$endif}CustomFontSizeChanged);
    FCustomFont := Value;
    if FCustomFont <> nil then
      FCustomFont.AddFontSizeChangeNotification(
        {$ifdef FPC}@{$endif}CustomFontSizeChanged);

    FCustomFontObserver.Observed := Value;

    // modify X3D nodes
    FontStyleNode.CustomFont := Value;
    FontStyleFontChanged;
  end;
end;

{$endif read_implementation}
