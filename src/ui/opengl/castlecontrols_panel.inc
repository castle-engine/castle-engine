{
  Copyright 2010-2017 Michalis Kamburelis.

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
  TCastlePanel = class(TUIControlSizeable)
  strict private
    FVerticalSeparators: TCardinalList;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Render; override;

    { Separator lines drawn on panel. Useful if you want to visually separate
      groups of contols (like a groups of buttons when you use
      this panel as a toolbar).

      Values are the horizontal positions of the separators (with respect
      to this panel @link(Left)). Width of the separator is in SeparatorSize. }
    property VerticalSeparators: TCardinalList read FVerticalSeparators;
    class function SeparatorSize: Cardinal;
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TCastlePanel ------------------------------------------------------------------ }

constructor TCastlePanel.Create(AOwner: TComponent);
begin
  inherited;
  FVerticalSeparators := TCardinalList.Create;
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
  I, SeparatorMarginScaled: Integer;
  SR: TRectangle;
begin
  inherited;

  SR := ScreenRect;
  Theme.Draw(SR, tiPanel, UIScale);

  SeparatorMarginScaled := Round(SeparatorMargin * UIScale);
  if SR.Height <= 2 * SeparatorMarginScaled then
    Exit; // no space
  for I := 0 to VerticalSeparators.Count - 1 do
    Theme.Draw(Rectangle(
      SR.Left + Round(VerticalSeparators[I] * UIScale),
      SR.Bottom + SeparatorMarginScaled,
      Theme.Images[tiPanelSeparator].Width, SR.Height - 2 * SeparatorMarginScaled),
      tiPanelSeparator, UIScale);
end;

class function TCastlePanel.SeparatorSize: Cardinal;
begin
  Result := 2;
end;

{$endif read_implementation}
