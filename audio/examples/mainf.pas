unit mainf;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, Spin,
  StdCtrls, Buttons, ALSourceAllocator, ExtCtrls, Grids, EditBtn;

type
  { TMain }

  TMain = class(TForm)
    ButtonRefreshUsed: TButton;
    ButtonAllocateAndPlay: TButton;
    ButtonApplyAllocatorLimits: TButton;
    ButtonRecreateAllocator: TButton;
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
    procedure ButtonRecreateAllocatorClick(Sender: TObject);
    procedure ButtonRefreshUsedClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    SourceAllocator: TALSourceAllocator;
    procedure SourceUsingEnd(Sender: TALAllocatedSource);
  public
    { public declarations }
  end; 

var
  Main: TMain;

implementation

uses VectorMath, ALUtils, OpenAL, KambiUtils, KambiStringUtils;

{ TMain }

type
  TALAllocatedSourceData = class
    ALBuffer: TALuint;
    FileName: string;
    StartedTime: TTime;
  end;

procedure TMain.ButtonRecreateAllocatorClick(Sender: TObject);
begin
  FreeAndNil(SourceAllocator);
  SourceAllocator := TALSourceAllocator.Create(
    SpinEditMinAllocatedSources.Value,
    SpinEditMaxAllocatedSources.Value);
end;

procedure TMain.ButtonRefreshUsedClick(Sender: TObject);
begin
  SourceAllocator.RefreshUsed;
end;

procedure TMain.FormCreate(Sender: TObject);
begin
  try
    BeginAL(false);
  except
    on E: EOpenALError do
    begin
      MessageDlg('Exception ' + E.ClassName + ':' + NL + E.Message,
        mtError, [mbOK], 0);
      { Re-raise, to end the program and print this on console }
      raise;
    end;
  end;
  SourceAllocator := TALSourceAllocator.Create(
    SpinEditMinAllocatedSources.Value,
    SpinEditMaxAllocatedSources.Value);
  Timer1.Enabled := true;
end;

procedure TMain.ButtonApplyAllocatorLimitsClick(Sender: TObject);
begin
  SourceAllocator.MinAllocatedSources := SpinEditMinAllocatedSources.Value;
  SourceAllocator.MaxAllocatedSources := SpinEditMaxAllocatedSources.Value;
end;

procedure TMain.ButtonAllocateAndPlayClick(Sender: TObject);
var
  UsedSource: TALAllocatedSource;
  UserData: TALAllocatedSourceData;
begin
  UsedSource := SourceAllocator.AllocateSource(SpinEditSourceImportance.Value);

  if UsedSource <> nil then
  begin
    UserData := TALAllocatedSourceData.Create;
    UserData.FileName := FileNameEditSound.FileName;
    UserData.ALBuffer :=
      TALSoundFile.alCreateBufferDataFromFile(UserData.FileName);
    UserData.StartedTime := Now;
      
    UsedSource.UserData := UserData;

    alSourcei(UsedSource.ALSource, AL_BUFFER, UserData.ALBuffer);
    alSourcei(UsedSource.ALSource, AL_SOURCE_RELATIVE, AL_TRUE);
    alSourceVector3f(UsedSource.ALSource, AL_POSITION, Vector3Single(0, 0, 0));
    if CheckBoxPlayLooping.Checked then
      alSourcei(UsedSource.ALSource, AL_LOOPING, AL_TRUE) else
      alSourcei(UsedSource.ALSource, AL_LOOPING, AL_FALSE);

    alSourcePlay(UsedSource.ALSource);
  end;
end;

procedure TMain.FormDestroy(Sender: TObject);
begin
  FreeAndNil(SourceAllocator);
  EndAL;
end;

procedure TMain.SourceUsingEnd(Sender: TALAllocatedSource);
begin
  Assert(Sender.UserData <> nil);
  alDeleteBuffers(1, @TALAllocatedSourceData(Sender.UserData).ALBuffer);
  Sender.UserData.Free;
  Sender.UserData := nil;
end;

procedure TMain.Timer1Timer(Sender: TObject);
var
  I: Integer;
  S: string;
begin
  if SourceAllocator <> nil then
  begin
    ListAllocatedSources.Clear;
    for I := 0 to SourceAllocator.AllocatedSources.High do
    begin
      S := Format('%d: AL source: %4d, used: %5s',
        [ I,
          SourceAllocator.AllocatedSources[I].ALSource,
          BoolToStrYesNo[SourceAllocator.AllocatedSources[I].Used] ]);
      if SourceAllocator.AllocatedSources[I].Used then
        S += Format(', started on %s, importance: %d, filename: %s',
          [ FormatDateTime('tt',
              TALAllocatedSourceData(
                SourceAllocator.AllocatedSources[I].UserData).StartedTime),
            SourceAllocator.AllocatedSources[I].Importance,
            TALAllocatedSourceData(SourceAllocator.AllocatedSources[I].
              UserData).FileName
          ]);
      ListAllocatedSources.Items.Append(S);
    end;
  end;
end;

initialization
  {$I mainf.lrs}
end.

