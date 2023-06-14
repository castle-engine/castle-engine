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

uses Classes, CastleControls, CastleUIControls;

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
      LoadingFinished: Boolean;
    procedure UpdateProgress(const Progress: Single);
    procedure DoLoadSomething1(Sender: TObject);
    procedure DoLoadSomething2(Sender: TObject);
    procedure DoLoadSomethingSmall(Sender: TObject);
    procedure DoLoadingFinish(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Start; override;
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean); override;
  end;

implementation

uses SysUtils,
  CastleColors, CastleWindow, CastleFilesUtils, CastleApplicationProperties,
  CastleUtils, CastleComponentSerialize,
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

  LoadingFinished := false;
  FakeLoadingAdditionalSteps := 0;
  UpdateProgress(0);
  WaitForRenderAndCall({$ifdef FPC}@{$endif}DoLoadSomething1);
end;

procedure TViewLoading.UpdateProgress(const Progress: Single);
begin
  LabelPercent.Caption := IntToStr(Round(100 * Progress)) + '%';
end;

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
  { Finished loading, go to ViewPlay.

    In case of using CreateUntilStopped, you cannot change view
    inside a WaitForRenderAndCall callback.
    So we need additional flag LoadingFinished to mark this view,
    that is handled in Update.
    This is more straightforward in version without CreateUntilStopped,
    in examples/zombie_fighter/ . }
  LoadingFinished := true;
end;

procedure TViewLoading.Update(const SecondsPassed: Single; var HandleInput: Boolean);
begin
  inherited;
  if LoadingFinished then
    Container.View := TViewPlay.CreateUntilStopped;
end;

end.
