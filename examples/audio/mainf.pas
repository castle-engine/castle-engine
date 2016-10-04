unit mainf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Spin,
  StdCtrls, Buttons, CastleSoundEngine, ExtCtrls, Grids, EditBtn;

type
  { TMain }

  TMain = class(TForm)
    ButtonRefreshUsed: TButton;
    ButtonAllocateAndPlay: TButton;
    ButtonApplyAllocatorLimits: TButton;
    CheckBoxPlayLooping: TCheckBox;
    FileNameEditSound: TFileNameEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    LabelMaxAllocatedSources: TLabel;
    LabelMinAllocatedSources: TLabel;
    LabelSourceImportance: TLabel;
    ListAllocatedSources: TListBox;
    ListUsedSources1: TListBox;
    Memo1: TMemo;
    PanelSourcePlaying: TPanel;
    PanelAllocatorLimits: TPanel;
    PanelLists: TPanel;
    SpinEditSourceImportance: TSpinEdit;
    SpinEditMaxAllocatedSources: TSpinEdit;
    SpinEditMinAllocatedSources: TSpinEdit;
    Timer1: TTimer;
    procedure ButtonAllocateAndPlayClick(Sender: TObject);
    procedure ButtonApplyAllocatorLimitsClick(Sender: TObject);
    procedure ButtonRefreshUsedClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    procedure SourceRelease(Sender: TSound);
  public
    { public declarations }
  end;

var
  Main: TMain;

implementation

uses CastleVectors, CastleOpenAL, CastleUtils, CastleStringUtils;

{ TMain }

type
  TSoundData = class
    Buffer: TSoundBuffer;
    FileName: string;
    StartedTime: TTime;
  end;

procedure TMain.ButtonRefreshUsedClick(Sender: TObject);
begin
  SoundEngine.Refresh;
end;

procedure TMain.FormCreate(Sender: TObject);
begin
  SoundEngine.MinAllocatedSources := SpinEditMinAllocatedSources.Value;
  SoundEngine.MaxAllocatedSources := SpinEditMaxAllocatedSources.Value;
  SoundEngine.ALContextOpen;
  Timer1.Enabled := true;
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
begin
  Buffer := SoundEngine.LoadBuffer(FileNameEditSound.FileName);
  UsedSource := SoundEngine.AllocateSound(SpinEditSourceImportance.Value);

  if UsedSource <> nil then
  begin
    UserData := TSoundData.Create;
    UserData.FileName := FileNameEditSound.FileName;
    UserData.Buffer := Buffer;
    UserData.StartedTime := Now;

    UsedSource.UserData := UserData;

    UsedSource.Buffer := UserData.Buffer;
    UsedSource.Relative := true;
    UsedSource.Position := ZeroVector3Single;
    UsedSource.Looping := CheckBoxPlayLooping.Checked;

    alSourcePlay(UsedSource.ALSource);
  end;
end;

procedure TMain.FormDestroy(Sender: TObject);
begin
  SoundEngine.ALContextClose;
end;

procedure TMain.SourceRelease(Sender: TSound);
begin
  Assert(Sender.UserData <> nil);
  alDeleteBuffers(1, @TSoundData(Sender.UserData).Buffer);
  Sender.UserData.Free;
  Sender.UserData := nil;
end;

procedure TMain.Timer1Timer(Sender: TObject);
var
  I: Integer;
  S: string;
begin
  ListAllocatedSources.Clear;
  for I := 0 to SoundEngine.AllocatedSources.Count - 1 do
  begin
    S := Format('%d: AL source: %4d, used: %5s',
      [ I,
        SoundEngine.AllocatedSources[I].ALSource,
        BoolToStr(SoundEngine.AllocatedSources[I].Used, true) ]);
    if SoundEngine.AllocatedSources[I].Used then
      S += Format(', started on %s, importance: %d, filename: %s',
        [ FormatDateTime('tt', TSoundData(
            SoundEngine.AllocatedSources[I].UserData).StartedTime),
          SoundEngine.AllocatedSources[I].Importance,
          TSoundData(SoundEngine.AllocatedSources[I].
            UserData).FileName
        ]);
    ListAllocatedSources.Items.Append(S);
  end;
end;

initialization
  {$I mainf.lrs}
end.

