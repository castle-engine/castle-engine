{
  This is based on CustApp unit from FPC
  but modified for Castle Game Engine needs.

  -------------------------------------------------------------------------

  Original header:

    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by the Free Pascal development team

    CustomApplication class.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  -------------------------------------------------------------------------

  Castle Game Engine version:

  This is a minimal subset of TCustomApplication class.
  Serves as a base for TCastleApplication in CastleWindow unit.
  We cut many things from FPC version,

  1. To make the process of maintaining this Delphi-only unit easier.

  2. Because we have in many cases equivalent things that we advise instead.
    (This advice applies to both FPC and Delphi, but with FPC we want
    to use original TCustomApplication from FPC.)
    This concerns:

    - Logging: we advise using @link(CastleLog) unit.

    - Command-line option parsing: use @link(CastleParameters).

    - ExeName: use our global @link(ExeName).

    - Environment variables: unsure why TCustomApplication has this at all?
      Just use SysUtils.GetEnvironmentVariable, already in both FPC and Delphi
      and cross-platform.

    - Application title: we have instead TCastleWindow.Caption
      and TCastleApplicationProperties.Caption.

    - Single instance: potentially useful, but significant complication
      to maintain. If you need this, we recommend you use standard
      VCL / FMX + TCastleControl for now, and setup single-instance
      using VCL / FMX ways.
}

{ Base application class (@link(TCustomApplication)), ancestor for
  @link(TCastleApplication). }
unit CustApp;

{$I castleconf.inc}

Interface

uses SysUtils, Classes, CastleUtils;

type
  TExceptionEvent = procedure (Sender: TObject; E: Exception) of object;

  { Base application class, ancestor for @link(TCastleApplication). }
  TCustomApplication = class(TComponent)
  private
    FOnException: TExceptionEvent;
    FTerminated : Boolean;
    FStopOnException : Boolean;
    FExceptionExitCode : Integer;
  protected
    Procedure DoRun; Virtual;
    procedure DoLog(EventType: TEventType; const Msg: String); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    procedure HandleException(Sender: TObject); virtual;
    procedure Initialize; virtual;
    procedure Run;
    procedure ShowException(E: Exception);virtual;
    procedure Terminate; overload; virtual;
    procedure Terminate(AExitCode : Integer); overload; virtual;
    property Terminated: Boolean read FTerminated;
    property OnException: TExceptionEvent read FOnException write FOnException;
    property StopOnException : Boolean Read FStopOnException Write FStopOnException;
    property ExceptionExitCode : Integer Read FExceptionExitCode Write FExceptionExitCode;
  end;

implementation

{ TCustomApplication }

procedure TCustomApplication.DoLog(EventType: TEventType; const Msg: String);
begin
  { Does nothing in this class, and also nothing calls it.

    We define this only for compatibility with FPC TCustomApplication
    that exposes also public methods like Log, that call DoLog.
    We don't advise this in CGE apps (use our WriteLnLog from CastleLog
    instead) so in Delphi version, we don't expose Log.
  }
end;

procedure TCustomApplication.DoRun;
begin
  // Override in descendent classes.
end;

constructor TCustomApplication.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FStopOnException := false;
end;

procedure TCustomApplication.HandleException(Sender: TObject);
begin
  if not (ExceptObject is Exception) then
    SysUtils.ShowException(ExceptObject, ExceptAddr)
  else
  begin
    if not Assigned(FOnException) then
      ShowException(Exception(ExceptObject))
    else
      FOnException(Sender, Exception(ExceptObject));
  end;
  if FStopOnException then
    Terminate(ExceptionExitCode);
end;

procedure TCustomApplication.Initialize;
begin
  FTerminated := false;
end;

procedure TCustomApplication.Run;
begin
  repeat
    try
      DoRun;
    except
      HandleException(Self);
    end;
  until FTerminated;
end;

procedure TCustomApplication.ShowException(E: Exception);
begin
  SysUtils.ShowException(E, ExceptAddr);
end;

procedure TCustomApplication.Terminate;
begin
  Terminate(ExitCode);
end;

procedure TCustomApplication.Terminate(AExitCode: Integer);
begin
  FTerminated := true;
  ExitCode := AExitCode;
end;

end.
