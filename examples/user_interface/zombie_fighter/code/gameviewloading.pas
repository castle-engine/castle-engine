{
  Copyright 2016-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ View with sample loading. }
unit GameViewLoading;

interface

uses Classes,
  CastleControls, CastleUIControls;

type
  { Loading view.
    This is an example how to show a loading progress using TViewLoading.

    As an example, it assumes that your "loading" consists of
    - one call to DoLoadSomething1
    - one call to DoLoadSomething2
    - 17 calls to DoLoadSomethingSmall
  }
  TViewLoading = class(TCastleView)
  published
    { Components designed using CGE editor.
      These fields will be automatically initialized at Start. }
    LabelPercent: TCastleLabel;
  strict private
    const
      FakeLoadingAdditionalStepsCount = 17;
    var
      { Variable that simulates loading progress,
        we will grow it from 0 to FakeLoadingAdditionalStepsCount during loading. }
      FakeLoadingAdditionalSteps: Cardinal;
    procedure UpdateProgress(const Progress: Single);
    procedure DoLoadSomething1(Sender: TObject);
    procedure DoLoadSomething2(Sender: TObject);
    procedure DoLoadSomethingSmall(Sender: TObject);
    procedure DoLoadingFinish(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
  end;

var
  ViewLoading: TViewLoading;

implementation

uses SysUtils,
  CastleColors, CastleWindow, CastleFilesUtils, CastleApplicationProperties,
  CastleUtils, CastleComponentSerialize, CastleTimeUtils,
  GameViewPlay;

{ TViewLoading ------------------------------------------------------------- }

constructor TViewLoading.Create(AOwner: TComponent);
begin
  inherited;
  DesignUrl := 'castle-data:/gameviewloading.castle-user-interface';
end;

procedure TViewLoading.Start;
begin
  inherited;

  FakeLoadingAdditionalSteps := 0;
  UpdateProgress(0);
  WaitForRenderAndCall({$ifdef FPC}@{$endif}DoLoadSomething1);
end;

procedure TViewLoading.UpdateProgress(const Progress: Single);
begin
  LabelPercent.Caption := IntToStr(Round(100 * Progress)) + '%';
end;

{$ifdef WASI}
{ WebAssembly will crash when we call standard Sleep.
  So we implement our own sleep below in a stupid way: just "busy waiting"
  until the time passes.

  DO NOT USE THIS IN YOUR OWN APPLICATIONS.

  This is used in this example just to "fake" that we're doing something
  time-consuming, to show that you can do something useful and show progress.
  This is the purpose of "Sleep" in this application (both on web and non-web).

  It is not good to be used in real applications, because

  - This "Sleep" implementation, with "busy waiting",
    is uselessly consuming CPU time.
    The "busy waiting" is a bad way to sleep, consuming CPU time doing nothing.

  - Even the proper "Sleep" on non-web platforms is useless in real games.
    It hangs the process, doing nothing, which is something you should never
    do. Instead always finish what you want to do as quick as possible,
    and adjust to passing time by accounting for SecondsPassed in the Update
    methods.
    See https://castle-engine.io/view_events .
}
procedure Sleep(const Milliseconds: Cardinal);
var
  TimerStart: TTimerResult;
begin
  TimerStart := Timer;
  while TimerStart.ElapsedTime < Milliseconds / 1000 do
    { nothing };
end;
{$endif}

procedure TViewLoading.DoLoadSomething1(Sender: TObject);
begin
  { Fake loading something big, one time. Just do something time-consuming there. }
  Sleep(100);

  UpdateProgress(0.25);
  WaitForRenderAndCall({$ifdef FPC}@{$endif}DoLoadSomething2);
end;

procedure TViewLoading.DoLoadSomething2(Sender: TObject);
begin
  { Fake loading something big, one time. Just do something time-consuming there. }
  Sleep(100);

  UpdateProgress(0.5);
  WaitForRenderAndCall({$ifdef FPC}@{$endif}DoLoadSomethingSmall);
end;

procedure TViewLoading.DoLoadSomethingSmall(Sender: TObject);
begin
  { Fake loading something small, 17 times (FakeLoadingAdditionalStepsCount).
    Just do something time-consuming there. }
  Sleep(5);

  Inc(FakeLoadingAdditionalSteps);
  UpdateProgress(0.5 + 0.5 * FakeLoadingAdditionalSteps / FakeLoadingAdditionalStepsCount);
  if FakeLoadingAdditionalSteps = FakeLoadingAdditionalStepsCount then
    { Finished loading. Using WaitForRenderAndCall(@DoFinish)
      means that user can see the value "100%", otherwise it would never get drawn,
      and the last loading frame would always show "97%". }
    WaitForRenderAndCall({$ifdef FPC}@{$endif}DoLoadingFinish)
  else
    WaitForRenderAndCall({$ifdef FPC}@{$endif}DoLoadSomethingSmall); // call this again, to load next step
end;

procedure TViewLoading.DoLoadingFinish(Sender: TObject);
begin
  { Finished loading, go to ViewPlay }
  Container.View := ViewPlay;
end;

end.
