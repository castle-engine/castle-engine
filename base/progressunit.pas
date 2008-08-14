{
  Copyright 2002-2008 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; you can redistribute it and/or modify
  it under the terms of the GNU General Public License as published by
  the Free Software Foundation; either version 2 of the License, or
  (at your option) any later version.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU General Public License for more details.

  You should have received a copy of the GNU General Public License
  along with "Kambi VRML game engine"; if not, write to the Free Software
  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
}

{ @abstract(This unit implements functionality (but not the user interface)
  of a "progress bar".)

  "Progress bar" is something that can be shown to user to indicate progress
  of some lenghty operation. The simplest example of use is

  @longcode(#
Progress.UserInterface := ... some TProgressUserInterface object ...;
...
Progress.Init(100, 'Doing something time-consuming, please wait');
try
 for i := 1 to 100 do
 begin
  ... do something ...
  Progress.Step;
 end;
finally Progress.Fini; end;
  #)

  (Embedding code in "try ... finally ... end" is not required,
  but is strongly suggested. Rule of thumb says to always call
  Progress.Fini when you called Progress.Init.)

  And remember to call (usually somewhere at the beginning of your
  program) some procedure to set Progress.UserInterface ---
  e.g. set this to ProgressConsoleInterface from ProgressConsole unit
  to have progress bar displayed on console (StdErr, to be more precise).

  This unit by itself does not provide any interface to show such progress
  bar. Instead, you must assign Progress.UserInterface property
  to some object implementing such user interface. E.g. I have units
  @unorderedList(
    @item(ProgressGL --- show progress bar in OpenGL window)
    @item(ProgressConsole --- show progress bar on StdErr)
    @item(ProgressVideo --- show progress bar on console using Video unit)
    @item(ProgressF --- show progress using Delphi form)
  )

  This way any unit that implements some lengthy operation can call
  appropriate functions of @link(Progress) object, and final program
  can choose how it wants to show that progress to user (in console ?
  in OpenGL window ? etc.). E.g. in unit @link(Images) my function ResizeImage
  calls Progress.Init, Progress.Step and Progress.Fini when resizing
  an image (because it may take a while when resized image is really big).
  So ResizeImage makes "progress of operation" available.
  But ResizeImage does not require that this progress will be shown
  in any particular way --- you can show progress of ResizeImage in console,
  or in OpenGL window, or in Delphi VCL form etc.

  So this unit implements only the base functionality of "progress bar",
  not specific to any interface. One important non-obvious functionality
  of class TProgress is that even if you're calling @link(TProgress.Step)
  very often, @link(TProgressUserInterface.Update) is @italic(not) called too often.
  This means that you can set @link(TProgress.Max) to very large value
  and you don't have to worry that @link(TProgressUserInterface.Update) will
  be called too often. See documenatation of @link(TProgress.UpdatePart)
  and @link(TProgress.UpdateTicks) for more info.

  This unit creates one object of class @link(TProgress) : @link(Progress).
  But it is possible to create other objects of this class.
  So you can use more than one @link(TProgress) object at once, if you need
  (e.g. from different threads, each @link(TProgress) object displaying
  it's state in a separate window).
}

unit ProgressUnit;

{ Define this only for testing }
{ $define TESTING_PROGRESS_DELAY}

interface

uses
  SysUtils, KambiUtils, KambiTimeUtils;

const
  { }
  DefaultUpdatePart = {$ifdef TESTING_PROGRESS_DELAY} 100000000 {$else} 100 {$endif};
  DefaultUpdateTicks = {$ifdef TESTING_PROGRESS_DELAY} 0 {$else} 250 {$endif};

type
  TProgress = class;

  TProgressUserInterface = class
  public
    { show progress bar }
    procedure Init(Progress: TProgress); virtual; abstract;
    { update progress bar (because Progress.Position changed) }
    procedure Update(Progress: TProgress); virtual; abstract;
    { hide progress bar }
    procedure Fini(Progress: TProgress); virtual; abstract;
  end;

  TProgress = class
  private
    FUserInterface: TProgressUserInterface;
    FUpdatePart: Cardinal;
    FUpdateTicks: TMilisecTime;

    FMax, FPosition: Cardinal;
    { Variables below meaningfull only if Active.

      When UserInterfaceDelayed, this is the time and position (always 0)
      of the TProgress.Init call.
      When not UserInterfaceDelayed, this is the time and position
      of the last TProgress.Init or TProgress.Update call. }
    LastUpdatePos: Cardinal;
    LastUpdateTick: TMilisecTime;
    UserInterfaceDelayed: boolean;

    FTitle: string;
    FActive: boolean;
    FUseDescribePosition: boolean;
  public
    property UserInterface: TProgressUserInterface
      read FUserInterface write FUserInterface;

    { Position musi sie zmienic o (1/UpdatePart) * Max i jednoczesnie
      musi uplynac UpdateTicks od ostatniego wywolania OnUpdateProgress abysmy
      wywolali je ponownie. W ten sposob mozna bardzo gesto generowac wywolania
      Progress.Step i nie ma strachu, nie bedziemy tracic masy czasu na robienie
      co chwile update'a progressu. }
    property UpdatePart: Cardinal read FUpdatePart write FUpdatePart
      default DefaultUpdatePart;
    property UpdateTicks: TMilisecTime read FUpdateTicks write FUpdateTicks
      default DefaultUpdateTicks;

    property Position: Cardinal read FPosition;
    property Max: Cardinal read FMax;
    property Title: string read FTitle;

    { Active = true means that we are between Init and Fini call.
      Init changes Active to true, Fini changes Active to false. }
    property Active: boolean read FActive;

    { This function returns something like Format('(%d / %d)', [Position, Max]).
      In other words, this is some text that describes current value of
      Position and Max. It may be shown to the user, see also TitleWithPosition
      and UseDescribePosition properties. }
    function DescribePosition: string;

    { This should be used by UserInterface to determine whether to show
      somewhere DescribePosition value. }
    property UseDescribePosition: boolean
      read FUseDescribePosition write FUseDescribePosition default true;

    { The intension is to return Title glued with DescribePosition,
      or possibly ended with '...' (3 dots).
      More precisely: if UseDescribePosition then
      it returns Title glued with DescribePosition.
      Else, if AddDots, it returns Title glued with '...'.
      Else it returns just Title. }
    function TitleWithPosition(const AddDots: boolean): string;

    { Start the progress bar.
      You can call Init only when Active = false.
      Init initializes Max, Title, sets Position to 0 and changes
      Active to true.

      UserInterface must be initialized (non-nil) when calling
      Init, and you cannot change UserInterface when progress is Active
      (i.e. before you call Fini).

      If DelayUserInterface is set to @true, a very useful optimization
      is performed: TProgress.Init will not
      immediately result in TProgressUserInterface.Init call.
      Instead, actual initialization of the interface will be delayed
      until some TProgress.Update, when UpdateTicks time will pass.

      The advantage of DelayUserInterface is that if
      an operation will take a very short time, we will not waste
      time on possibly lengthy initialization of the progress bar
      interface. For example, ProgressGL has to capture OpenGL screen
      at the initialization, which takes a noticeable fraction of second
      by itself. So it's not sensible to init ProgressGL if an entire
      operation between Progress.Init and Fini will take only 0.001 of second..

      The only downside of DelayUserInterface is that it's not applicable
      to an operations with very few steps (e.g. 1) that may take a long time.
      If a time between Init and the first Update or Fini is really large,
      the progress bar will not be visible. }
    procedure Init(AMax: Cardinal; const ATitle: string;
      const DelayUserInterface: boolean = false);

    { Step increments Position by StepSize.
      Use only when Active (i.e. between Init .. Fini calls).

      Position always stays <= Max (so you can depend on this
      when implementaing TProgressUserInterface).
      But it is completely legal to try to raise Position above
      Max by calling Step, i.e. Step works like
        Position := Position +  StepSize;
        if Position > Max then Position := Max;

      (this is usefull when given Max was only an approximation of needed
      steps). }
    procedure Step(StepSize: Cardinal = 1);

    { You can call Fini only when Active = true.
      Fini changes Active to false.

      Note that it's perfectly legal to call Fini before Position
      reaches Max (it's sensible e.g. when you're allowing user to break
      some lenghty operation, or when Max was only an approximation
      of steps needed). }
    procedure Fini;

    constructor Create;
  end;

var
  { Created in initialization, freed in finalization. }
  Progress: TProgress;

type
  TProgressNullInterface = class(TProgressUserInterface)
  public
    procedure Init(Progress: TProgress); override;
    procedure Update(Progress: TProgress); override;
    procedure Fini(Progress: TProgress); override;
  end;

var
  { A special TProgressUserInterface instance, that simply does nothing.

    Set Progress.UserInterface to this, and then
    progress Init/Update/Fini will work --- but will not be displayed
    anywhere.

    Created in initialization, freed in finalization. }
  ProgressNullInterface :TProgressNullInterface;

implementation

function TProgress.DescribePosition: string;
begin
 Result := Format('(%d / %d)', [Position, Max]);
end;

function TProgress.TitleWithPosition(const AddDots: boolean): string;
begin
  if UseDescribePosition then
    Result := Title + ' ' + DescribePosition else
  if AddDots then
    Result := Title + ' ...' else
    Result := Title;
end;

procedure TProgress.Init(AMax: Cardinal; const ATitle: string;
  const DelayUserInterface: boolean);
begin
 Check(not Active, 'TProgress.Init error: progress is already active');
 FActive := true;

 Check(UserInterface <> nil,
   'TProgress.Init error: UserInterface not initialized');

 FPosition := 0;

 { Max(AMax, 1) secures us against AMax <= 0 values.

   (Otherwise, it would have to be secured at many places when calling
   Progress.Init, as sometimes AMax <= 0 values values can naturally
   occur. Consider e.g. building octree, when the VRML scene turns out
   to be empty.)

   The idea is that AMax <= 0 means that actually operation is already
   finished. So we'll set Max to 1 (to allow UserInterface to display it,
   since user interface can display only Max >= 1 values)
   and we'll do Step(1) immediately at the end of TProgress.Init,
   to show to user that operation is already done. }
 FMax := KambiUtils.Max(AMax, 1);

 FTitle := ATitle;

 { Calling UserInterface.Init updates LastUpdatePos and LastUpdateTick,
   just like calling UserInterface.Update. }
 LastUpdatePos := FPosition;
 LastUpdateTick := GetTickCount;

 UserInterfaceDelayed := DelayUserInterface;

 if not UserInterfaceDelayed then
 try
   UserInterface.Init(Self);
 except
   { In case of problems within UserInterface.Init, call Fini
     to change our state to not Active. }
   Fini;
   raise;
 end;

 { This means that AMax < Max(AMax, 1), in other words: AMax <= 0.
   Then show to user that this operation actually finished. }
 try
   if AMax < Max then Step;
 except
   { In case of problems within UserInterface.Init, call Fini
     to change our state to not Active. }
   Fini;
   raise;
 end;
end;

procedure TProgress.Step(StepSize: Cardinal);
begin
 Assert(Active, 'TProgress.Step error: progress is not active');

 FPosition := FPosition + StepSize;
 if Position > Max then FPosition := Max;

 if UserInterfaceDelayed then
 begin
   { Either actually init user interface, or resign from calling
     UserInterface.Update. }
   if TimeTickDiff(LastUpdateTick, GetTickCount) > UpdateTicks then
   begin
     UserInterface.Init(Self);
     UserInterfaceDelayed := false;
   end else
     Exit;
 end;

 if ((Position - LastUpdatePos) / Max > 1 / UpdatePart) and
    (TimeTickDiff(LastUpdateTick, GetTickCount) > UpdateTicks) then
 begin
  LastUpdatePos := FPosition;
  LastUpdateTick := GetTickCount;
  UserInterface.Update(Self);

  {$ifdef TESTING_PROGRESS_DELAY}
  Sleep(10);
  {$endif}
 end;
end;

procedure TProgress.Fini;
begin
 Check(Active, 'TProgress.Fini error: progress is not active');
 FActive := false;

 if not UserInterfaceDelayed then
 begin
   { update to reflect the current state of Position, if needed.
     Note that this does NOT mean that at the end Position is = Max.
     Noone ever guarantees that -- you can call Fini before Position
     reaches Max. }
   if LastUpdatePos < Position then
     UserInterface.Update(Self);

   UserInterface.Fini(Self);
 end;
end;

constructor TProgress.Create;
begin
 inherited;
 UpdatePart := DefaultUpdatePart;
 UpdateTicks := DefaultUpdateTicks;
 FActive := false;
 FUseDescribePosition := true;
end;

{ TProgressNullInterface ----------------------------------------------------- }

procedure TProgressNullInterface.Init(Progress: TProgress);
begin
end;

procedure TProgressNullInterface.Update(Progress: TProgress);
begin
end;

procedure TProgressNullInterface.Fini(Progress: TProgress);
begin
end;

{ initialization / finalization ---------------------------------------------- }

initialization
 Progress := TProgress.Create;
 ProgressNullInterface := TProgressNullInterface.Create;
finalization
 FreeAndNil(Progress);
 FreeAndNil(ProgressNullInterface);
end.
