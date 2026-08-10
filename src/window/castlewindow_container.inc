{
  Copyright 2001-2026 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Container used by TCastleWindow.
  This whole file is only included in implementation of CastleWindow,
  as TWindowContainer is internal in this unit. }

{ TWindowContainer ----------------------------------------------------------- }

type
  { Container suitable to be used in TCastleWindow.
    Never create instances of this, just create TCastleWindow,
    and use container of it (in @link(TCastleWindow.Container)). }
  TWindowContainer = class(TCastleContainer)
  private
    Parent: TCastleWindow;
  public
    constructor Create(AParent: TCastleWindow); reintroduce;

    procedure Invalidate; override;
    function GLInitialized: boolean; override;
    function PixelsWidth: Integer; override;
    function PixelsHeight: Integer; override;
    function PixelsRect: TRectangle; override;
    function Focused: boolean; override;
    procedure SetInternalCursor(const Value: TMouseCursor); override;
    function SaveScreen(const SaveRect: TRectangle): TRGBImage; overload; override;
    procedure SystemSetMousePosition(const Value: TVector2); override;
    function SettingMousePositionCausesMotion: Boolean; override;
  end;

constructor TWindowContainer.Create(AParent: TCastleWindow);
begin
  inherited Create(nil);
  Parent := AParent;
end;

procedure TWindowContainer.Invalidate;
begin
  Parent.Invalidate;
end;

function TWindowContainer.GLInitialized: boolean;
begin
  Result := Parent.GLInitialized;
end;

function TWindowContainer.PixelsWidth: Integer;
begin
  Result := Parent.Width;
end;

function TWindowContainer.PixelsHeight: Integer;
begin
  Result := Parent.Height;
end;

function TWindowContainer.PixelsRect: TRectangle;
begin
  Result := Parent.Rect;
end;

function TWindowContainer.Focused: boolean;
begin
  Result := Parent.Focused;
end;

procedure TWindowContainer.SetInternalCursor(const Value: TMouseCursor);
begin
  Parent.InternalCursor := Value;
end;

function TWindowContainer.SaveScreen(const SaveRect: TRectangle): TRGBImage;
begin
  { In theory, the single-buffer
    case could be optimized (do not redraw if not needed, that is:
    if not invalidated), but it's not worth the complication since noone uses
    single-buffer for normal applications... And also, there is no reliable
    way to capture screen contents in case of single-buffer. }

  EventBeforeRender;
  EventRender;
  Result := SaveScreen_NoFlush(SaveRect, Parent.SaveScreenBuffer);
end;

procedure TWindowContainer.SystemSetMousePosition(const Value: TVector2);
begin
  Parent.SystemSetMousePosition(Value);
end;

