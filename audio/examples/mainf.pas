unit mainf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Spin,
  StdCtrls, Buttons, ALSoundAllocator, ExtCtrls, Grids, EditBtn;

type
  { TMain }

  TMain = class(TForm)
    ButtonAllocateAndPlay: TButton;
    ButtonApplyAllocatorLimits: TButton;
    ButtonRecreateAllocator: TButton;
    CheckBoxPlayLooping: TCheckBox;
    FileNameEditSound: TFileNameEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    LabelMaxAllocatedSounds: TLabel;
    LabelMinAllocatedSounds: TLabel;
    LabelSoundImportance: TLabel;
    ListAllocatedSounds: TListBox;
    ListUsedSounds1: TListBox;
    Memo1: TMemo;
    PanelSoundPlaying: TPanel;
    PanelAllocatorLimits: TPanel;
    PanelLists: TPanel;
    SpinEditSoundImportance: TSpinEdit;
    SpinEditMaxAllocatedSounds: TSpinEdit;
    SpinEditMinAllocatedSounds: TSpinEdit;
    Timer1: TTimer;
    procedure ButtonAllocateAndPlayClick(Sender: TObject);
    procedure ButtonApplyAllocatorLimitsClick(Sender: TObject);
    procedure ButtonRecreateAllocatorClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    SoundAllocator: TALSoundAllocator;
    procedure SoundUsingEnd(Sender: TALAllocatedSound);
  public
    { public declarations }
  end; 

var
  Main: TMain;

implementation

uses VectorMath, ALUtils, OpenAL, KambiStringUtils;

{ TMain }

type
  TALAllocatedSoundData = class
    ALBuffer: TALuint;
    FileName: string;
    StartedTime: TTime;
  end;

procedure TMain.ButtonRecreateAllocatorClick(Sender: TObject);
begin
  FreeAndNil(SoundAllocator);
  SoundAllocator := TALSoundAllocator.Create(
    SpinEditMinAllocatedSounds.Value,
    SpinEditMaxAllocatedSounds.Value);
end;

procedure TMain.FormCreate(Sender: TObject);
begin
  BeginAL(false);
  SoundAllocator := TALSoundAllocator.Create(
    SpinEditMinAllocatedSounds.Value,
    SpinEditMaxAllocatedSounds.Value);
  Timer1.Enabled := true;
end;

procedure TMain.ButtonApplyAllocatorLimitsClick(Sender: TObject);
begin
  SoundAllocator.MinAllocatedSounds := SpinEditMinAllocatedSounds.Value;
  SoundAllocator.MaxAllocatedSounds := SpinEditMaxAllocatedSounds.Value;
end;

procedure TMain.ButtonAllocateAndPlayClick(Sender: TObject);
var
  UsedSound: TALAllocatedSound;
  UserData: TALAllocatedSoundData;
begin
  UsedSound := SoundAllocator.AllocateSound(SpinEditSoundImportance.Value);

  if UsedSound <> nil then
  begin
    UserData := TALAllocatedSoundData.Create;
    UserData.FileName := FileNameEditSound.FileName;
    UserData.ALBuffer :=
      TALSoundWAV.alCreateBufferDataFromFile(UserData.FileName);
    UserData.StartedTime := Now;
      
    UsedSound.UserData := UserData;

    alSourcei(UsedSound.ALSound, AL_BUFFER, UserData.ALBuffer);
    alSourcei(UsedSound.ALSound, AL_SOURCE_RELATIVE, AL_TRUE);
    alSourceVector3f(UsedSound.ALSound, AL_POSITION,
      Vector3Single(0, 0, 0.1));
    if CheckBoxPlayLooping.Checked then
      alSourcei(UsedSound.ALSound, AL_LOOPING, AL_TRUE) else
      alSourcei(UsedSound.ALSound, AL_LOOPING, AL_FALSE);

    alSourcePlay(UsedSound.ALSound);
  end;
end;

procedure TMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(SoundAllocator);
  EndAL;
end;

procedure TMain.SoundUsingEnd(Sender: TALAllocatedSound);
begin
  Assert(Sender.UserData <> nil);
  alDeleteBuffers(1, @TALAllocatedSoundData(Sender.UserData).ALBuffer);
  Sender.UserData.Free;
  Sender.UserData := nil;
end;

procedure TMain.Timer1Timer(Sender: TObject);
var
  I: Integer;
  S: string;
begin
  if SoundAllocator <> nil then
  begin
    ListAllocatedSounds.Clear;
    for I := 0 to SoundAllocator.AllocatedSounds.High do
    begin
      S := Format('%d: AL sound: %4d, used: %5s',
        [ I,
          SoundAllocator.AllocatedSounds[I].ALSound,
          BoolToStrYesNo[SoundAllocator.AllocatedSounds[I].Used] ]);
      if SoundAllocator.AllocatedSounds[I].Used then
        S += Format(', started on %s, importance: %d, filename: %s',
          [ FormatDateTime('tt',
              TALAllocatedSoundData(
                SoundAllocator.AllocatedSounds[I].UserData).StartedTime),
            SoundAllocator.AllocatedSounds[I].Importance,
            TALAllocatedSoundData(SoundAllocator.AllocatedSounds[I].
              UserData).FileName
          ]);
      ListAllocatedSounds.Items.Append(S);
    end;
  end;
end;

initialization
  {$I mainf.lrs}
end.

