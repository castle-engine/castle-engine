{%MainUnit castlecontrols.pas}
{
  Copyright 2010-2018 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}

  { Panel or a toolbar control.
    Use as a background for other controls like buttons.
    You can add vertical separators to separate groups of controls on the panel. }
  TCastlePanel = class(TCastleUserInterface)
  strict private
    FVerticalSeparators: TSingleList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Render; override;

    { Separator lines drawn on panel. Useful if you want to visually separate
      groups of contols (like a groups of buttons when you use
      this panel as a toolbar).

      Values are the horizontal positions of the separators (with respect
      to this panel @link(Left)). Width of the separator is in SeparatorSize. }
    property VerticalSeparators: TSingleList read FVerticalSeparators;
    class function SeparatorSize: Single;
  end deprecated 'use TCastleHorizontalGroup';

{$endif read_interface}

{$ifdef read_implementation}

{ TCastlePanel ------------------------------------------------------------------ }

constructor TCastlePanel.Create(AOwner: TComponent);
begin
  inherited;
  FVerticalSeparators := TSingleList.Create;
end;

destructor TCastlePanel.Destroy;
begin
  FreeAndNil(FVerticalSeparators);
  inherited;
end;

procedure TCastlePanel.Render;
const
  SeparatorMargin = 8;
var
  I: Integer;
  SeparatorMarginScaled: Single;
  SR: TFloatRectangle;
begin
  inherited;

  SR := RenderRect;
  Theme.Draw(SR, tiPanel, UIScale);

  SeparatorMarginScaled := SeparatorMargin * UIScale;
  if SR.Height <= 2 * SeparatorMarginScaled then
    Exit; // no space
  for I := 0 to VerticalSeparators.Count - 1 do
    Theme.Draw(FloatRectangle(
      SR.Left + VerticalSeparators[I] * UIScale,
      SR.Bottom + SeparatorMarginScaled,
      Theme.ImagesPersistent[tiPanelSeparator].Width, SR.Height - 2 * SeparatorMarginScaled),
      tiPanelSeparator, UIScale);
end;

class function TCastlePanel.SeparatorSize: Single;
begin
  Result := 2;
end;

{$endif read_implementation}
