{
  Copyright 2020-2020 Michalis Kamburelis.

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
  CastleClassUtils, CastleUIControls, CastleControls;

type
  TImageGrid = class(TCastleUserInterface)
  strict private
    FRows, FColumns: Integer;
    FURL: String;
    FImages: array of TCastleImageControl;
    procedure RebuildChildren;
    procedure SetRows(const Value: Integer);
    procedure SetColumns(const Value: Integer);
    procedure SetURL(const Value: String);
  protected
    procedure PreferredSize(var PreferredWidth, PreferredHeight: Single); override;
  public
    constructor Create(AOwner: TComponent); override;
    function PropertySection(const PropertyName: String): TPropertySection; override;
  published
    property Rows: Integer read FRows write SetRows default 1;
    property Columns: Integer read FColumns write SetColumns default 1;
    property URL: String read FURL write SetURL;
  end;

implementation

uses SysUtils,
  CastleComponentSerialize;

constructor TImageGrid.Create(AOwner: TComponent);
begin
  inherited;
  FRows := 1;
  FColumns := 1;
end;

function TImageGrid.PropertySection(const PropertyName: String): TPropertySection;
begin
  case PropertyName of
    'Rows', 'Columns', 'URL':
      Result := psBasic;
    else
      Result := inherited PropertySection(PropertyName);
  end;
end;

procedure TImageGrid.PreferredSize(var PreferredWidth, PreferredHeight: Single);
var
  FirstImage: TCastleImageControl;
begin
  if Length(FImages) <> 0 then
  begin
    FirstImage := FImages[0];
    PreferredWidth := UIScale * FirstImage.EffectiveWidth * Columns;
    PreferredHeight := UIScale * FirstImage.EffectiveHeight * Rows;
  end;
end;

procedure TImageGrid.RebuildChildren;
var
  I, J: Integer;
  NewImage: TCastleImageControl;
begin
  for I := 0 to Length(FImages) - 1 do
    FreeAndNil(FImages[I]);

  SetLength(FImages, FRows * FColumns);
  for I := 0 to Rows - 1 do
    for J := 0 to Columns - 1 do
    begin
      NewImage := TCastleImageControl.Create(Self);
      NewImage.SetTransient; // do not show this child in editor, do not serialize
      NewImage.URL := URL;
      NewImage.Anchor(hpLeft, J * NewImage.EffectiveWidth);
      NewImage.Anchor(vpBottom, I * NewImage.EffectiveHeight);
      FImages[I * Columns + J] := NewImage;
      InsertFront(NewImage);
    end;
end;

procedure TImageGrid.SetRows(const Value: Integer);
begin
  if FRows <> Value then
  begin
    FRows := Value;
    RebuildChildren;
  end;
end;

procedure TImageGrid.SetColumns(const Value: Integer);
begin
  if FColumns <> Value then
  begin
    FColumns := Value;
    RebuildChildren;
  end;
end;

procedure TImageGrid.SetURL(const Value: String);
begin
  if FURL <> Value then
  begin
    FURL := Value;
    RebuildChildren;
  end;
end;

initialization
  RegisterSerializableComponent(TImageGrid, 'Image Grid');
end.
