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
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TimerToDisplaySoundsTimer(Sender: TObject);
    procedure TimerToRefreshUsedSoundsTimer(Sender: TObject);
  private
    procedure SourceRelease(Sender: TSound);
  public
    { public declarations }
  end;

var
  Main: TMain;

implementation

uses CastleVectors, CastleUtils, CastleStringUtils, CastleApplicationProperties,
  CastleLCLUtils;

type
  { Data associated with sounds.

    TODO: we use TComponent freeing mechanism to keep all the TSoundData
    instances cleaned, because we cannot depend that TSound.OnRelease
    is reliably called always, even when TSound is destroyed.
    But this is unoptimal, it would be better to make OnRelease more reliable. }

  TSoundData = class(TComponent)
  public
    Buffer: TSoundBuffer;
    FileName: string;
    StartedTime: TTime;
  end;

procedure TMain.FormCreate(Sender: TObject);
begin
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

procedure TMain.ButtonAllocateAndPlayClick(Sender: TObject);
var
  UsedSource: TSound;
  UserData: TSoundData;
  Buffer: TSoundBuffer;
  PlayParameters: TSoundParameters;
begin
  Buffer := SoundEngine.LoadBuffer(FileNameEditSound.FileName);

  PlayParameters := TSoundParameters.Create;
  try
    PlayParameters.Buffer := Buffer;
    PlayParameters.Importance := SpinEditSourceImportance.Value;
    PlayParameters.Spatial := false;
    PlayParameters.Looping := CheckBoxPlayLooping.Checked;
    UsedSource := SoundEngine.PlaySound(PlayParameters);
  finally FreeAndNil(PlayParameters) end;

  if UsedSource <> nil then
  begin
    // Free previous UserData,
    // as (it seems) the Release is sometimes not called
    // (TODO: OnRelease should be more reliable in this case too)
    UsedSource.UserData.Free;
    UsedSource.UserData := nil;

    UserData := TSoundData.Create(Self);
    UserData.FileName := FileNameEditSound.FileName;
    UserData.Buffer := Buffer;
    UserData.StartedTime := Now;

    UsedSource.UserData := UserData;
  end;
end;

procedure TMain.FormDestroy(Sender: TObject);
var
  I: Integer;
  SoundData: TSoundData;
begin
  if SoundEngine.InternalAllocatedSources <> nil then
    for I := 0 to SoundEngine.InternalAllocatedSources.Count - 1 do
    begin
      SoundData := TSoundData(SoundEngine.InternalAllocatedSources[I].UserData);
      if SoundData <> nil then
      begin
        { free the UserData, that keeps our TSoundData references }
        SoundEngine.FreeBuffer(SoundData.Buffer);
        SoundEngine.InternalAllocatedSources[I].UserData.Free;
        SoundEngine.InternalAllocatedSources[I].UserData := nil;
      end;
    end;

  SoundEngine.ContextClose;
end;

procedure TMain.SourceRelease(Sender: TSound);
begin
  Assert(Sender.UserData <> nil);
  SoundEngine.FreeBuffer(TSoundData(Sender.UserData).Buffer);
  Sender.UserData.Free;
  Sender.UserData := nil;
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
      S := Format('%d: Sound source used: %5s',
        [ I,
          BoolToStr(SoundEngine.InternalAllocatedSources[I].Used, true) ]);
      if SoundEngine.InternalAllocatedSources[I].Used then
        S += Format(', started on %s, priority: %f, filename: %s',
          [ FormatDateTime('tt', TSoundData(
              SoundEngine.InternalAllocatedSources[I].UserData).StartedTime),
            SoundEngine.InternalAllocatedSources[I].Priority,
            TSoundData(SoundEngine.InternalAllocatedSources[I].
              UserData).FileName
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

