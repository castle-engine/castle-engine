{%MainUnit castlevectors.pas}
{
  Copyright 2019-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ TBorder. }

{$ifdef read_interface}

type
  { Configurable border size for @link(TCastleUserInterface.Border). }
  TBorder = class(TPersistent)
  strict private
    FTop, FRight, FBottom, FLeft, FAllSides: Single;
    FOnChange: TNotifyEvent;
    procedure SetAllSides(const AValue: Single);
    procedure SetBottom(const AValue: Single);
    procedure SetLeft(const AValue: Single);
    procedure SetRight(const AValue: Single);
    procedure SetTop(const AValue: Single);
  public
    constructor Create(const AOnChange: TNotifyEvent);
    procedure Assign(Source: TPersistent); override;

    { Total top border (Top + AllSides). }
    function TotalTop: Single;
    { Total right border (Right + AllSides). }
    function TotalRight: Single;
    { Total bottom border (Bottom + AllSides). }
    function TotalBottom: Single;
    { Total left border (Left + AllSides). }
    function TotalLeft: Single;

    { Total horizontal border (TotalLeft + TotalRight). }
    function TotalWidth: Single;
    { Total vertical border (TotalTop + TotalBottom). }
    function TotalHeight: Single;

    { Anything not zero? }
    function Exists: Boolean;
    function HasDefaultValue: Boolean;
  published
    property Top: Single      read FTop      write SetTop      {$ifdef FPC} default 0 {$endif};
    property Right: Single    read FRight    write SetRight    {$ifdef FPC} default 0 {$endif};
    property Bottom: Single   read FBottom   write SetBottom   {$ifdef FPC} default 0 {$endif};
    property Left: Single     read FLeft     write SetLeft     {$ifdef FPC} default 0 {$endif};
    property AllSides: Single read FAllSides write SetAllSides {$ifdef FPC} default 0 {$endif};
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TBorder -------------------------------------------------------------------- }

constructor TBorder.Create(const AOnChange: TNotifyEvent);
begin
  inherited Create;
  FOnChange := AOnChange;
end;

procedure TBorder.SetAllSides(const AValue: Single);
begin
  if FAllSides = AValue then Exit;
  FAllSides := AValue;
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TBorder.SetBottom(const AValue: Single);
begin
  if FBottom = AValue then Exit;
  FBottom := AValue;
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TBorder.SetLeft(const AValue: Single);
begin
  if FLeft = AValue then Exit;
  FLeft := AValue;
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TBorder.SetRight(const AValue: Single);
begin
  if FRight = AValue then Exit;
  FRight := AValue;
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TBorder.SetTop(const AValue: Single);
begin
  if FTop = AValue then Exit;
  FTop := AValue;
  if Assigned(FOnChange) then FOnChange(Self);
end;

function TBorder.TotalTop: Single;
begin
  Result := FAllSides + FTop;
end;

function TBorder.TotalRight: Single;
begin
  Result := FAllSides + FRight;
end;

function TBorder.TotalBottom: Single;
begin
  Result := FAllSides + FBottom;
end;

function TBorder.TotalLeft: Single;
begin
  Result := FAllSides + FLeft;
end;

function TBorder.TotalWidth: Single;
begin
  Result := 2 * FAllSides + FRight + FLeft;
end;

function TBorder.TotalHeight: Single;
begin
  Result := 2 * FAllSides + FTop + FBottom;
end;

function TBorder.Exists: Boolean;
begin
  Result := (FAllSides <> 0) or
    (FTop <> 0) or
    (FRight <> 0) or
    (FBottom <> 0) or
    (FLeft <> 0);
end;

function TBorder.HasDefaultValue: Boolean;
begin
  { For now, TBorder is by default always equal zero.
    So for now, this method is equal to "not Exists".
    (This may change some day if we'll use TBorder as subcomponent
    and will want to customize it's default value in parent.) }
  Result := not Exists;
end;

procedure TBorder.Assign(Source: TPersistent);
var
  SourceBorder: TBorder;
begin
  if Source is TBorder then
  begin
    SourceBorder := TBorder(Source);
    Top      := SourceBorder.Top;
    Right    := SourceBorder.Right;
    Bottom   := SourceBorder.Bottom;
    Left     := SourceBorder.Left;
    AllSides := SourceBorder.AllSides;
  end else
    { Since TBorder is a direct TPersistent descendant,
      it calls inherited ONLY when it cannot handle Source class. }
    inherited Assign(Source);
end;

{$endif read_implementation}
