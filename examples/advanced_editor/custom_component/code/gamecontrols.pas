{
  Copyright 2020-2022 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Game-specific components, that are also used by custom editor for this game. }
unit GameControls;

interface

uses Classes,
  CastleClassUtils, CastleUIControls, CastleControls, CastleGLImages;

type
  TImageGrid = class(TCastleUserInterface)
  strict private
    FRows, FColumns: Integer;
    FUrl: String;
    { While it would be possible to render the grid using multiple TCastleImageControl
      instances (see the version from
      https://github.com/castle-engine/castle-engine/commit/d2b20a608b01f87ff4e41db038393e7bbb3e90bb#diff-1d471baf18007e094eb060eec1873c36fc385ce7cea2af50b328db5059f39da3 )
      it is much more efficient to use one TDrawableImage instance
      with RepeatS = RepeatT = true. }
    FImage: TDrawableImage;
    procedure SetRows(const Value: Integer);
    procedure SetColumns(const Value: Integer);
    procedure SetUrl(const Value: String);
  protected
    procedure PreferredSize(var PreferredWidth, PreferredHeight: Single); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Render; override;
    function PropertySections(const PropertyName: String): TPropertySections; override;
    procedure EditorAllowResize(
      out ResizeWidth, ResizeHeight: Boolean; out Reason: String); override;
  published
    property Rows: Integer read FRows write SetRows default 1;
    property Columns: Integer read FColumns write SetColumns default 1;
    property Url: String read FUrl write SetUrl;
  end;

implementation

uses SysUtils,
  CastleComponentSerialize, CastleImages, CastleRectangles, CastleStringUtils,
  CastleUtils, CastleLog, CastleUriUtils
  { Use CastlePropEdits, and thus LCL and castle_components, only when part of the editor. }
  {$ifdef CASTLE_DESIGN_MODE} , PropEdits, ComponentEditors, CastlePropEdits {$endif};

{ TImageGrid ----------------------------------------------------------------- }

constructor TImageGrid.Create(AOwner: TComponent);
var
  DummyInitialImage: TGrayscaleImage;
begin
  inherited;
  FRows := 1;
  FColumns := 1;

  DummyInitialImage := TGrayscaleImage.Create(100, 100);
  DummyInitialImage.Clear(255); // white

  FImage := TDrawableImage.Create(DummyInitialImage, true, true);
  FImage.RepeatS := true;
  FImage.RepeatT := true;
end;

destructor TImageGrid.Destroy;
begin
  FreeAndNil(FImage);
  inherited;
end;

procedure TImageGrid.Render;
begin
  inherited;
  FImage.Draw(RenderRect, FloatRectangle(0, 0, Columns * FImage.Width, Rows * FImage.Height));
end;

function TImageGrid.PropertySections(const PropertyName: String): TPropertySections;
begin
  if (PropertyName = 'Rows') or
     (PropertyName = 'Columns') or
     (PropertyName = 'Url') then
    Result := [psBasic]
  else
    Result := inherited PropertySections(PropertyName);
end;

procedure TImageGrid.PreferredSize(var PreferredWidth, PreferredHeight: Single);
begin
  inherited;
  PreferredWidth := UIScale * FImage.Width * Columns;
  PreferredHeight := UIScale * FImage.Height * Rows;
end;

procedure TImageGrid.EditorAllowResize(
  out ResizeWidth, ResizeHeight: Boolean; out Reason: String);
begin
  inherited;
  ResizeWidth := false;
  ResizeHeight := false;
  Reason := SAppendPart(Reason, NL, 'TImageGrid always automatically adjusts to the required size, based on image size and Rows/Columns properties.');
end;

procedure TImageGrid.SetRows(const Value: Integer);
begin
  if FRows <> Value then
  begin
    FRows := Value;
    VisibleChange([chRectangle]); // redraw control, size changed
  end;
end;

procedure TImageGrid.SetColumns(const Value: Integer);
begin
  if FColumns <> Value then
  begin
    FColumns := Value;
    VisibleChange([chRectangle]); // redraw control, size changed
  end;
end;

procedure TImageGrid.SetUrl(const Value: String);
begin
  if FUrl <> Value then
  begin
    FUrl := Value;
    try
      FImage.Url := Value;
    except
      { If loading file failed, and we're inside CGE editor,
        merely report a warning. This allows deserializing in CGE editor
        designs with broken URLs. }
      on E: Exception do
      begin
        if CastleDesignMode then
        begin
          WritelnWarning('TImageGrid', 'Failed to load image "%s": %s',
            [UriDisplay(Value), ExceptMessage(E)]);
        end else
          raise;
      end;
    end;
    VisibleChange([chRectangle]); // redraw control, maybe even size changed
  end;
end;

{ TImageGridEditor ----------------------------------------------------------- }

{$ifdef CASTLE_DESIGN_MODE}
type
  { Editor for TImageGrid. }
  TImageGridEditor = class(TComponentEditor)
  public
    function GetVerbCount: Integer; override;
    function GetVerb(Index: Integer): string; override;
    procedure ExecuteVerb(Index: Integer); override;
  end;

function TImageGridEditor.GetVerbCount: Integer;
begin
  Result := (inherited GetVerbCount) + 1;
end;

function TImageGridEditor.GetVerb(Index: Integer): string;
var
  InheritedCount: Integer;
begin
  InheritedCount := inherited GetVerbCount;
  if Index < InheritedCount then
    Result := inherited GetVerb(Index)
  else
  if Index = InheritedCount then
  begin
    Result := 'Reload Image File';
  end else
    Result := '';
end;

procedure TImageGridEditor.ExecuteVerb(Index: Integer);
var
  InheritedCount: Integer;
  SavedUrl: String;
begin
  InheritedCount := inherited GetVerbCount;
  if Index < InheritedCount then
    inherited ExecuteVerb(Index)
  else
  if Index = InheritedCount then
  begin
    SavedUrl := (Component as TImageGrid).Url;
    (Component as TImageGrid).Url := '';
    (Component as TImageGrid).Url := SavedUrl;
    // Note: GetDesigner.Modified is not necessary here, as reopening this design would reload referenced URL too
  end else
    WritelnWarning('TImageGridEditor.ExecuteVerb invalid verb index: %d', [Index]);
end;
{$endif}

initialization
  RegisterSerializableComponent(TImageGrid, 'Image Grid');
  {$ifdef CASTLE_DESIGN_MODE}
  RegisterPropertyEditor(TypeInfo(AnsiString), TImageGrid, 'Url', TImageUrlPropertyEditor);
  RegisterComponentEditor(TImageGrid, TImageGridEditor);
  {$endif}
end.
