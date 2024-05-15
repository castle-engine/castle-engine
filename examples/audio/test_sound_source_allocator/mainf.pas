unit mainf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Spin,
  StdCtrls, Buttons, CastleSoundEngine, ExtCtrls, Grids, EditBtn;

type
  { TMain }

  TMain = class(TForm)
    ApplicationProperties1: TApplicationProperties;
    ButtonAllocateAndPlay: TButton;
    ButtonApplyAllocatorLimits: TButton;
    ButtonApplyAllocatorLimitsDefault: TButton;
    CheckKeepRefreshingUsed: TCheckBox;
    CheckBoxPlayLooping: TCheckBox;
    FileNameEditSound: TFileNameEdit;
    LabelDescription: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    LabelTitle: TLabel;
    LabelMaxAllocatedSources: TLabel;
    LabelMinAllocatedSources: TLabel;
    LabelSourceImportance: TLabel;
    ListAllocatedSources: TListBox;
    ListUsedSources1: TListBox;
    PanelSourcePlaying: TPanel;
    PanelAllocatorLimits: TPanel;
    PanelLists: TPanel;
    SpinEditSourceImportance: TSpinEdit;
    SpinEditMaxAllocatedSources: TSpinEdit;
    SpinEditMinAllocatedSources: TSpinEdit;
    TimerToRefreshUsedSounds: TTimer;
    TimerToDisplaySounds: TTimer;
    procedure ButtonAllocateAndPlayClick(Sender: TObject);
    procedure ButtonApplyAllocatorLimitsClick(Sender: TObject);
    procedure ButtonApplyAllocatorLimitsDefaultClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TimerToDisplaySoundsTimer(Sender: TObject);
    procedure TimerToRefreshUsedSoundsTimer(Sender: TObject);
  public
    { public declarations }
  end;

var
  Main: TMain;

implementation

uses CastleVectors, CastleUtils, CastleStringUtils, CastleApplicationProperties,
  CastleLCLUtils;

procedure TMain.FormCreate(Sender: TObject);
begin
  ButtonApplyAllocatorLimitsDefault.Caption:= Format('Set Min / Max allocated sources to Defaults (%d / %d)', [
    TSoundAllocator.DefaultMinAllocatedSources,
    TSoundAllocator.DefaultMaxAllocatedSources
  ]);
  SoundEngine.MinAllocatedSources := SpinEditMinAllocatedSources.Value;
  SoundEngine.MaxAllocatedSources := SpinEditMaxAllocatedSources.Value;
  SoundEngine.ContextOpen;
  TimerToDisplaySounds.Enabled := true;
  FileFiltersToDialog(LoadSound_FileFilters, FileNameEditSound);
end;

procedure TMain.ButtonApplyAllocatorLimitsClick(Sender: TObject);
begin
  SoundEngine.MinAllocatedSources := SpinEditMinAllocatedSources.Value;
  SoundEngine.MaxAllocatedSources := SpinEditMaxAllocatedSources.Value;
end;

procedure TMain.ButtonApplyAllocatorLimitsDefaultClick(Sender: TObject);
begin
  SpinEditMinAllocatedSources.Value := TSoundAllocator.DefaultMinAllocatedSources;
  SpinEditMaxAllocatedSources.Value := TSoundAllocator.DefaultMaxAllocatedSources;
  ButtonApplyAllocatorLimitsClick(nil);
end;

procedure TMain.ButtonAllocateAndPlayClick(Sender: TObject);
var
  Sound: TCastleSound;
  PlayingSound: TCastlePlayingSound;
begin
  Sound := TCastleSound.Create(Self);
  Sound.Url := FileNameEditSound.FileName;
  Sound.Priority := SpinEditSourceImportance.Value; // TODO: should be in 0..1 range

  PlayingSound := TCastlePlayingSound.Create(Self);
  PlayingSound.Sound := Sound;
  PlayingSound.Loop := CheckBoxPlayLooping.Checked;
  PlayingSound.FreeOnStop := true;

  SoundEngine.Play(PlayingSound);
end;

procedure TMain.TimerToDisplaySoundsTimer(Sender: TObject);
var
  I: Integer;
  S: string;
begin
  ListAllocatedSources.Clear;
  // SoundEngine.AllocatedSources will be nil if sound backend initialization failed
  if SoundEngine.InternalAllocatedSources <> nil then
    for I := 0 to SoundEngine.InternalAllocatedSources.Count - 1 do
    begin
      S := Format('%d: Sound source used: %5s', [
        I,
        BoolToStr(SoundEngine.InternalAllocatedSources[I].Used, true)
      ]);
      ListAllocatedSources.Items.Append(S);
    end;
end;

procedure TMain.TimerToRefreshUsedSoundsTimer(Sender: TObject);
begin
  if CheckKeepRefreshingUsed.Checked then
    { calling TCastleApplicationProperties._Update will cause
      updating used sound sources in SoundEngine. }
    ApplicationProperties._Update;
end;

initialization
  {$I mainf.lrs}
end.

