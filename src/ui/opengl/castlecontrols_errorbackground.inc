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

  { Error background, for various error handlers.
    This is notably used by the CastleWindow exception handler. }
  TErrorBackground = class(TUIControl)
    procedure Render; override;
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TErrorBackground ------------------------------------------------------ }

procedure TErrorBackground.Render;
begin
  inherited;
  Theme.Draw(ParentRect, tiErrorBackground, UIScale);
end;

{$endif read_implementation}
