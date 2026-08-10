{
  Copyright 2026-2026 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ View to choose song to play (@link(TViewChooseSong)). }
unit GameViewChooseSong;

interface

uses Classes,
  CastleVectors, CastleComponentSerialize,
  CastleUIControls, CastleControls, CastleKeysMouse;

type
  { View to choose song to play. }
  TViewChooseSong = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    LabelFps: TCastleLabel;
    SongsGroup: TCastleVerticalGroup;
    SongRowFactory: TCastleComponentFactory;
    GroupInSafeBorders: TCastleUserInterface;
  private
    procedure SongRowClick(Sender: TObject);
    procedure SafeBorderChanged(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Stop; override;
    procedure Resize; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
  end;

var
  ViewChooseSong: TViewChooseSong;

implementation

uses SysUtils,
  CastleLog,
  GameSongs, GameViewPlaySong;

{ TViewChooseSong ----------------------------------------------------------------- }

constructor TViewChooseSong.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewchoosesong.castle-user-interface';
end;

type
  TSongRowDesign = class(TPersistent)
  published
    LabelArtist, LabelTitle: TCastleLabel;
    ImageCover: TCastleImageControl;
  end;

procedure TViewChooseSong.Start;

  procedure AddSongUi(const SongIndex: Integer; const Song: TSong);
  var
    SongRow: TCastleButton;
    SongRowDesign: TSongRowDesign;
  begin
    SongRowDesign := TSongRowDesign.Create;
    try
      SongRow := SongRowFactory.ComponentLoad(Self, SongRowDesign) as TCastleButton;
      SongRow.OnClick := {$ifdef FPC}@{$endif} SongRowClick;
      SongRow.Tag := SongIndex;
      SongRowDesign.LabelArtist.Caption := Song.Artist;
      SongRowDesign.LabelTitle.Caption := Song.Title;
      SongRowDesign.ImageCover.URL := Song.UrlArt;
      SongsGroup.InsertFront(SongRow);
    finally FreeAndNil(SongRowDesign) end;
  end;

var
  SongIndex: Integer;
begin
  inherited;
  for SongIndex := 0 to Songs.Count - 1 do
    AddSongUi(SongIndex, Songs[SongIndex]);
  GroupInSafeBorders.Border.Assign(Container.SafeBorder);
  Container.OnSafeBorderChanged := {$ifdef FPC}@{$endif} SafeBorderChanged;
end;

procedure TViewChooseSong.Stop;
begin
  Container.OnSafeBorderChanged := nil;
  inherited;
end;

procedure TViewChooseSong.SafeBorderChanged(Sender: TObject);
begin
  GroupInSafeBorders.Border.Assign(Container.SafeBorder);
  //WriteLnLog('Safe border changed: %s', [Container.SafeBorder.ToString]);
end;

procedure TViewChooseSong.Resize;
var
  C: TCastleUserInterface;
begin
  inherited;
  // adjust width of group items, to match screen
  for C in SongsGroup do
    if C is TCastleButton then
      TCastleButton(C).Width := Container.UnscaledWidth - SongsGroup.Padding * 2;
end;

procedure TViewChooseSong.SongRowClick(Sender: TObject);
var
  SongIndex: Integer;
begin
  SongIndex := (Sender as TCastleButton).Tag;
  Assert((SongIndex >= 0) and (SongIndex < Songs.Count));
  ViewPlaySong.Song := Songs[SongIndex];
  Container.View := ViewPlaySong;
end;

procedure TViewChooseSong.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  { This virtual method is executed every frame (many times per second). }
  Assert(LabelFps <> nil, 'If you remove LabelFps from the design, remember to remove also the assignment "LabelFps.Caption := ..." from code');
  LabelFps.Caption := 'FPS: ' + Container.Fps.ToString;
end;

end.
