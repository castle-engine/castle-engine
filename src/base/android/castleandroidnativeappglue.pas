(*
 * Copyright (C) 2010 The Android Open Source Project
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
  *)

unit CastleAndroidNativeAppGlue;

{$I castleconf.inc}

interface

uses
  { Using CThreads is necessary, otherwise we cannot use FPC threads
    (TThread.Create simply terminates the process).

    And we really want to use FPC threads. Do not directly use non-FPC
    threads, e.g. do not create thread using pthread_create like original
    native_android_app_glue code from Google says.
    Weird things can happen
    if you use external threads (created by pthreads) without any FPC knowledge.
    See http://wiki.freepascal.org/Multithreaded_Application_Tutorial#External_threads .
    The symptoms are random segfaults, e.g. after checking MainWindow.Closed
    the drawing_toy started making segfaults at fpc_popaddrstack is various
    code places.

    Using CThreads (that actually wraps pthread threads underneath) makes
    code simple and stable.

    CThreads *must* be the 1st unit on "uses" clause here,
    and CastleAndroidNativeAppGlue *must* be the 1st unit on
    "uses" clause of your Android library. }
  CThreads,

  ctypes,baseunix,unixtype,
  CastleAndroidInternalConfiguration, CastleAndroidInternalLooper, CastleAndroidInternalLog,
  CastleAndroidInternalInput, CastleAndroidInternalRect, CastleAndroidInternalNativeWindow,
  CastleAndroidInternalNativeActivity;

(**
 * The native activity interface provided by <android/native_activity.h>
 * is based on a set of application-provided callbacks that will be called
 * by the Activity's main thread when certain events occur.
 *
 * This means that each one of this callbacks _should_ _not_ block, or they
 * risk having the system force-close the application. This programming
 * model is direct, lightweight, but constraining.
 *
 * The 'threaded_native_app' static library is used to provide a different
 * execution model where the application can implement its own main event
 * loop in a different thread instead. Here's how it works:
 *
 * 1/ The application must provide a function named "android_main()" that
 *    will be called when the activity is created, in a new thread that is
 *    distinct from the activity's main thread.
 *
 * 2/ android_main() receives a pointer to a valid "android_app" structure
 *    that contains references to other important objects, e.g. the
 *    ANativeActivity obejct instance the application is running in.
 *
 * 3/ the "android_app" object holds an ALooper instance that already
 *    listens to two important things:
 *
 *      - activity lifecycle events (e.g. "pause", "resume"). See APP_CMD_XXX
 *        declarations below.
 *
 *      - input events coming from the AInputQueue attached to the activity.
 *
 *    Each of these correspond to an ALooper identifier returned by
 *    ALooper_pollOnce with values of LOOPER_ID_MAIN and LOOPER_ID_INPUT,
 *    respectively.
 *
 *    Your application can use the same ALooper to listen to additional
 *    file-descriptors.  They can either be callback based, or with return
 *    identifiers starting with LOOPER_ID_USER.
 *
 * 4/ Whenever you receive a LOOPER_ID_MAIN or LOOPER_ID_INPUT event,
 *    the returned data will point to an android_poll_source structure.  You
 *    can call the process() function on it, and fill in android_app->onAppCmd
 *    and android_app->onInputEvent to be called for your own processing
 *    of the event.
 *
 *    Alternatively, you can call the low-level functions to read and process
 *    the data directly...  look at the process_cmd() and process_input()
 *    implementations in the glue to see how to do this.
 *
 * See the sample named "native-activity" that comes with the NDK with a
 * full usage example.  Also look at the JavaDoc of NativeActivity.
  *)

(**
 * Data associated with an ALooper fd that will be returned as the "outData"
 * when that source has data ready.
  *)

type
  Pandroid_poll_source = ^android_poll_source;
  Pandroid_app = ^Tandroid_app;
  android_poll_source = packed record
    // The identifier of this source.  May be LOOPER_ID_MAIN or
    // LOOPER_ID_INPUT.
    id : cint32;
    // The android_app this ident is associated with.
    app : Pandroid_app;
    // Function to call to perform the standard processing of data from
    // this source.
    process : procedure(app: Pandroid_app; source: Pandroid_poll_source);
  end;

(**
 * This is the interface for the standard glue code of a threaded
 * application.  In this model, the application's code is running
 * in its own thread separate from the main thread of the process.
 * It is not required that this thread be associated with the Java
 * VM, although it will need to be in order to make JNI calls any
 * Java objects.
  *)
  Tandroid_app = packed record
    // The application can place a pointer to its own state object
    // here if it likes.
    userData : Pointer;
    // Fill this in with the function to process main app commands (APP_CMD_*)
    onAppCmd : procedure(app: Pandroid_app; cmd: Integer) of object;
    // Fill this in with the function to process input events.  At this point
    // the event has already been pre-dispatched, and it will be finished upon
    // return. Return if you have handled the event, 0 for any default
    // dispatching.
    onInputEvent : function(app: Pandroid_app; event: PAInputEvent): boolean of object;
    // The ANativeActivity object instance that this app is running in.
    activity : PANativeActivity;
    // The current configuration the app is running in.
    config : PAConfiguration;
    // This is the last instance's saved state, as provided at creation time.
    // It is NULL if there was no state.  You can use this as you need; the
    // memory will remain around until you call android_app_exec_cmd() for
    // APP_CMD_RESUME, at which point it will be freed and savedState set to NULL.
    // These variables should only be changed when processing a APP_CMD_SAVE_STATE,
    // at which point they will be initialized to NULL and you can malloc your
    // state and place the information here.  In that case the memory will be
    // freed for you later.
    savedState : Pointer;
    savedStateSize : csize_t;
    // The ALooper associated with the app's thread.
    looper : PALooper;
    // When non-NULL, this is the input queue from which the app will
    // receive user input events.
    inputQueue : PAInputQueue;
    // When non-NULL, this is the window surface that the app can draw in.
    window : PANativeWindow;
    // Current content rectangle of the window; this is the area where the
    // window's content should be placed to be seen by the user.
    contentRect : ARect;
    // Current state of the app's activity.  May be either APP_CMD_START,
    // APP_CMD_RESUME, APP_CMD_PAUSE, or APP_CMD_STOP; see below.
    activityState : cint;
    // This is non-zero when the application's NativeActivity is being
    // destroyed and waiting for the app thread to complete.
    destroyRequested : cint;
    // -------------------------------------------------
    // Below are "private" implementation of the glue code.
    mutex : pthread_mutex_t;
    cond : pthread_cond_t;
    msgread : cint;
    msgwrite : cint;
    cmdPollSource : android_poll_source;
    inputPollSource : android_poll_source;
    running : cint;
    stateSaved : cint;
    destroyed : cint;
    redrawNeeded : cint;
    pendingInputQueue : PAInputQueue;
    pendingWindow : PANativeWindow;
    pendingContentRect : ARect;
  end;

const
  (**
     * Looper data ID of commands coming from the app's main thread, which
     * is returned as an identifier from ALooper_pollOnce().  The data for this
     * identifier is a pointer to an android_poll_source structure.
     * These can be retrieved and processed with android_app_read_cmd()
     * and android_app_exec_cmd().
      *)
  LOOPER_ID_MAIN = 1;
  (**
     * Looper data ID of events coming from the AInputQueue of the
     * application's window, which is returned as an identifier from
     * ALooper_pollOnce().  The data for this identifier is a pointer to an
     * android_poll_source structure.  These can be read via the inputQueue
     * object of android_app.
      *)
  LOOPER_ID_INPUT = 2;
  (**
     * Start of user-defined ALooper identifiers.
      *)
  LOOPER_ID_USER = 3;

const
  (**
     * Command from main thread: the AInputQueue has changed.  Upon processing
     * this command, android_app->inputQueue will be updated to the new queue
     * (or NULL).
      *)
  APP_CMD_INPUT_CHANGED = 0;
  (**
     * Command from main thread: a new ANativeWindow is ready for use.  Upon
     * receiving this command, android_app->window will contain the new window
     * surface.
      *)
  APP_CMD_INIT_WINDOW = 1;
  (**
     * Command from main thread: the existing ANativeWindow needs to be
     * terminated.  Upon receiving this command, android_app->window still
     * contains the existing window; after calling android_app_exec_cmd
     * it will be set to NULL.
      *)
  APP_CMD_TERM_WINDOW = 2;
  (**
     * Command from main thread: the current ANativeWindow has been resized.
     * Please redraw with its new size.
      *)
  APP_CMD_WINDOW_RESIZED = 3;
  (**
     * Command from main thread: the system needs that the current ANativeWindow
     * be redrawn.  You should redraw the window before handing this to
     * android_app_exec_cmd() in order to avoid transient drawing glitches.
      *)
  APP_CMD_WINDOW_REDRAW_NEEDED = 4;
  (**
     * Command from main thread: the content area of the window has changed,
     * such as from the soft input window being shown or hidden.  You can
     * find the new content rect in android_app::contentRect.
      *)
  APP_CMD_CONTENT_RECT_CHANGED = 5;
  (**
     * Command from main thread: the app's activity window has gained
     * input focus.
      *)
  APP_CMD_GAINED_FOCUS = 6;
  (**
     * Command from main thread: the app's activity window has lost
     * input focus.
      *)
  APP_CMD_LOST_FOCUS = 7;
  (**
     * Command from main thread: the current device configuration has changed.
      *)
  APP_CMD_CONFIG_CHANGED = 8;
  (**
     * Command from main thread: the system is running low on memory.
     * Try to reduce your memory use.
      *)
  APP_CMD_LOW_MEMORY = 9;
  (**
     * Command from main thread: the app's activity has been started.
      *)
  APP_CMD_START = 10;
  (**
     * Command from main thread: the app's activity has been resumed.
      *)
  APP_CMD_RESUME = 11;
  (**
     * Command from main thread: the app should generate a new saved state
     * for itself, to restore from later if needed.  If you have saved state,
     * allocate it with malloc and place it in android_app.savedState with
     * the size in android_app.savedStateSize.  The will be freed for you
     * later.
      *)
  APP_CMD_SAVE_STATE = 12;
  (**
     * Command from main thread: the app's activity has been paused.
      *)
  APP_CMD_PAUSE = 13;
  (**
     * Command from main thread: the app's activity has been stopped.
      *)
  APP_CMD_STOP = 14;
  (**
     * Command from main thread: the app's activity is being destroyed,
     * and waiting for the app thread to clean up and exit before proceeding.
      *)
  APP_CMD_DESTROY = 15;

type
  TAndroidMainFunction = procedure (App: PAndroid_App);
var
  AndroidMain: TAndroidMainFunction;

{ This function is exported from the library. It implements the activity,
  passing the control to AndroidMain callback.

  Note that this function *can* be called many times.
  The activity may be destroyed and then created again.
  The actual process *may* stay running.
  (to test, try "Developer Options -> Always Destroy Activity"). }
procedure ANativeActivity_onCreate(activity: PANativeActivity; savedState: Pointer; savedStateSize: csize_t); cdecl;

{ Allocate memory for saved state. }
function AllocateSavedState(const Size: csize_t): Pointer;

var
  { Singleton instance of PAndroid_App, initialized right before AndroidMain is called. }
  AndroidMainApp: PAndroid_App;

implementation

uses SysUtils, Classes, CastleUtils;

{ Use Libc memory allocation routines where needed. It seems savedState
  must be managed using them, onSaveInstanceState in CastleAndroidInternalNativeActivity
  is documented like that. }
const
  CLibName = 'c';
Function LibcMalloc (Size : ptruint) : Pointer; cdecl; external CLibName name 'malloc';
Procedure LibcFree (P : pointer); cdecl; external CLibName name 'free';

type
  ppthread_t = ^pthread_t;
  ppthread_mutex_t = ^pthread_mutex_t;
  ppthread_cond_t = ^pthread_cond_t;
  ppthread_mutexattr_t = ^pthread_mutexattr_t;
  ppthread_condattr_t = ^pthread_condattr_t;

function pthread_mutex_init(__mutex:ppthread_mutex_t; __mutex_attr:ppthread_mutexattr_t):CInt32;cdecl;external 'libc.so';
function pthread_mutex_destroy(__mutex:ppthread_mutex_t):CInt32;cdecl;external 'libc.so';
function pthread_mutex_lock(__mutex: ppthread_mutex_t):CInt32;cdecl;external 'libc.so';
function pthread_mutex_unlock(__mutex: ppthread_mutex_t):CInt32;cdecl;external 'libc.so';
function pthread_cond_init(__cond:ppthread_cond_t; __cond_attr:ppthread_condattr_t):CInt32;cdecl;external 'libc.so';
function pthread_cond_destroy(__cond:ppthread_cond_t):CInt32;cdecl;external 'libc.so';
function pthread_cond_signal(__cond:ppthread_cond_t):CInt32;cdecl;external 'libc.so';
function pthread_cond_broadcast(__cond:ppthread_cond_t):CInt32;cdecl;external 'libc.so';
function pthread_cond_wait(__cond:ppthread_cond_t; __mutex:ppthread_mutex_t):CInt32;cdecl;external 'libc.so';

procedure FreeSavedState(android_app: Pandroid_app);
begin
   pthread_mutex_lock(@android_app^.mutex);
   if android_app^.savedState <> Nil then
   begin
      LibcFree(android_app^.savedState);
      android_app^.savedState := nil;
      android_app^.savedStateSize := 0;
   end;
   pthread_mutex_unlock(@android_app^.mutex);
end;

(**
 * Call when ALooper_pollAll() returns LOOPER_ID_MAIN, reading the next
 * app command message.
  *)
function android_app_read_cmd(android_app: Pandroid_app): cint8;
var cmd: cint8;
begin
    result := -1;
    if fpread(android_app^.msgread, @cmd, sizeof(cmd)) = sizeof(cmd) then
    begin
        case cmd of
           APP_CMD_SAVE_STATE:
               FreeSavedState(android_app);
        end;
        result := cmd;
    end
    else
        AndroidLog(alError, 'NativeAppGlue: No data on command pipe');
end;

(**
 * Call with the command returned by android_app_read_cmd() to do the
 * initial pre-processing of the given command.  You can perform your own
 * actions for the command after calling this function.
  *)
procedure android_app_pre_exec_cmd(android_app: Pandroid_app; cmd: cint8);
begin
    case cmd of
        APP_CMD_INPUT_CHANGED:
            begin
               AndroidLog(alInfo,'NativeAppGlue: APP_CMD_INPUT_CHANGED');
               pthread_mutex_lock(@android_app^.mutex);
               if android_app^.inputQueue <> nil then
                   AInputQueue_detachLooper(android_app^.inputQueue);
               android_app^.inputQueue := android_app^.pendingInputQueue;
               if android_app^.inputQueue <> nil then
               begin
                   AndroidLog(alInfo,'NativeAppGlue: Attaching input queue to looper');
                   AInputQueue_attachLooper(android_app^.inputQueue,
                           android_app^.looper, LOOPER_ID_INPUT, nil,
                           @android_app^.inputPollSource);
               end;
               pthread_cond_broadcast(@android_app^.cond);
               pthread_mutex_unlock(@android_app^.mutex);
            end;

        APP_CMD_INIT_WINDOW:
            begin
               AndroidLog(alInfo,'NativeAppGlue: APP_CMD_INIT_WINDOW');
               pthread_mutex_lock(@android_app^.mutex);
               android_app^.window := android_app^.pendingWindow;
               pthread_cond_broadcast(@android_app^.cond);
               pthread_mutex_unlock(@android_app^.mutex);
            end;

        APP_CMD_TERM_WINDOW:
            begin
               AndroidLog(alInfo,'NativeAppGlue: APP_CMD_TERM_WINDOW');
               pthread_mutex_lock(@android_app^.mutex);
               android_app^.window := nil;
               pthread_cond_broadcast(@android_app^.cond);
               pthread_mutex_unlock(@android_app^.mutex);
            end;

        APP_CMD_RESUME,
        APP_CMD_START,
        APP_CMD_PAUSE,
        APP_CMD_STOP:
            begin
               // AndroidLog(alInfo,'NativeAppGlue: activityState:=%d', [cmd]);
               pthread_mutex_lock(@android_app^.mutex);
               android_app^.activityState := cmd;
               pthread_cond_broadcast(@android_app^.cond);
               pthread_mutex_unlock(@android_app^.mutex);
            end;

        APP_CMD_CONFIG_CHANGED:
            begin
               AndroidLog(alInfo,'NativeAppGlue: APP_CMD_CONFIG_CHANGED');
               AConfiguration_fromAssetManager(android_app^.config,
                       android_app^.activity^.assetManager);
            end;

        APP_CMD_DESTROY:
            begin
               AndroidLog(alInfo,'NativeAppGlue: APP_CMD_DESTROY');
               android_app^.destroyRequested := 1;
            end;
    end;
end;

(**
 * Call with the command returned by android_app_read_cmd() to do the
 * final post-processing of the given command.  You must have done your own
 * actions for the command before calling this function.
  *)
procedure android_app_post_exec_cmd(android_app: Pandroid_app; cmd: cint8);
begin
    case cmd of
        APP_CMD_TERM_WINDOW:
            begin
               AndroidLog(alInfo,'NativeAppGlue: APP_CMD_TERM_WINDOW');
               pthread_mutex_lock(@android_app^.mutex);
               android_app^.window := nil;
               pthread_cond_broadcast(@android_app^.cond);
               pthread_mutex_unlock(@android_app^.mutex);
            end;

        APP_CMD_SAVE_STATE:
            begin
               AndroidLog(alInfo,'NativeAppGlue: APP_CMD_SAVE_STATE');
               pthread_mutex_lock(@android_app^.mutex);
               android_app^.stateSaved := 1;
               pthread_cond_broadcast(@android_app^.cond);
               pthread_mutex_unlock(@android_app^.mutex);
            end;

        APP_CMD_RESUME:
            FreeSavedState(android_app);
    end;
end;

procedure android_app_destroy(android_app: Pandroid_app);
begin
    AndroidLog(alInfo, 'NativeAppGlue: android_app_destroy');
    FreeSavedState(android_app);
    pthread_mutex_lock(@android_app^.mutex);
    if (android_app^.inputQueue <> nil) then
        AInputQueue_detachLooper(android_app^.inputQueue);
    AConfiguration_delete(android_app^.config);
    android_app^.destroyed := 1;
    pthread_cond_broadcast(@android_app^.cond);
    pthread_mutex_unlock(@android_app^.mutex);
    // Can't touch android_app object after this.
end;

procedure process_input(app: Pandroid_app; source: Pandroid_poll_source);
var
  Event: PAInputEvent;
  Handled, Processed: boolean;
begin
  { Original native_app_glue was using if and processing only 1 event.
    Fixed to avoid ANRs following
    https://developer.nvidia.com/content/nativeactivity-input-crashes-and-anrs-simple-fix-dangerous-bug }
  Event := nil;
  Processed := false;
  while AInputQueue_getEvent(app^.inputQueue, @event) >= 0 do
  begin
    // AndroidLog(alInfo,'NativeAppGlue: New input event: type:=%d',[AInputEvent_getType(event)]);
    if AInputQueue_preDispatchEvent(app^.inputQueue, event) <> 0 then
      Continue;
    Handled := false;
    if Assigned(app^.onInputEvent) then
      Handled := App^.onInputEvent(app, event);
    AInputQueue_finishEvent(app^.inputQueue, event, handled);
    Processed := true;
  end;
  if not Processed then
    AndroidLog(alError, 'NativeAppGlue: Failure reading next input event');
end;

procedure process_cmd(app: Pandroid_app; source: Pandroid_poll_source);
var cmd: cint8;
begin
    cmd := android_app_read_cmd(app);
    android_app_pre_exec_cmd(app, cmd);
    if (app^.onAppCmd <> nil) then app^.onAppCmd(app, cmd);
    android_app_post_exec_cmd(app, cmd);
end;

type
  TAndroidAppThread = class(TThread)
    Android_app: PAndroid_app;
    constructor Create(AnAndroid_app: PAndroid_app; const CreateSuspended: boolean);
    procedure Execute; override;
  end;

constructor TAndroidAppThread.Create(AnAndroid_app: PAndroid_app;
  const CreateSuspended: boolean);
begin
  Android_app := AnAndroid_app;
  inherited Create(CreateSuspended);
end;

procedure TAndroidAppThread.Execute;
var
  looper: PALooper;
begin
  try
    AndroidLog(alInfo, 'NativeAppGlue: TAndroidAppThread.Execute');

    android_app^.config := AConfiguration_new();
    AConfiguration_fromAssetManager(android_app^.config, android_app^.activity^.assetManager);

    android_app^.cmdPollSource.id := LOOPER_ID_MAIN;
    android_app^.cmdPollSource.app := android_app;
    android_app^.cmdPollSource.process := @process_cmd;
    android_app^.inputPollSource.id := LOOPER_ID_INPUT;
    android_app^.inputPollSource.app := android_app;
    android_app^.inputPollSource.process := @process_input;

    looper := ALooper_prepare(ALOOPER_PREPARE_ALLOW_NON_CALLBACKS);
    ALooper_addFd(looper, android_app^.msgread, LOOPER_ID_MAIN, ALOOPER_EVENT_INPUT, nil,
            @android_app^.cmdPollSource);
    android_app^.looper := looper;

    pthread_mutex_lock(@android_app^.mutex);
    android_app^.running := 1;
    pthread_cond_broadcast(@android_app^.cond);
    pthread_mutex_unlock(@android_app^.mutex);

    { Looks like this has to be in it's own try..except handler,
      other a crash can occur when catching exceptions from inside AndroidMain.
      Not really known why.
      Reproducible by drawing_toy with deliberately crashing DrawCore.
      (Possibly not needed anymore, since using TAndroidAppThread?). }
    try
      AndroidMainApp := android_app;
      AndroidMain(android_app);
    except
      on E: TObject do
        AndroidLog(alError, 'NativeAppGlue: AndroidMain exited with exception: ' + ExceptMessage(E));
    end;

    android_app_destroy(android_app);
  except
    on E: TObject do
      AndroidLog(alError, 'NativeAppGlue: TAndroidAppThread.Execute exited with exception: ' + ExceptMessage(E));
  end;
end;

// --------------------------------------------------------------------
// Native activity interaction (called from main thread)
// --------------------------------------------------------------------

function AllocateSavedState(const Size: csize_t): Pointer;
begin
  Result := LibcMalloc(Size);
end;

function android_app_create(activity: PANativeActivity; savedState: Pointer; savedStateSize: csize_t): Pandroid_app;
var
    msgpipe: array[0..1] of cint;
    android_app: Pandroid_app;
    Thread: TAndroidAppThread;
begin
    android_app := Pandroid_app(LibcMalloc(sizeof(Tandroid_app)));
    fillchar(android_app^, sizeof(tandroid_app), 0);
    android_app^.activity := activity;

    pthread_mutex_init(@android_app^.mutex, nil);
    pthread_cond_init(@android_app^.cond, nil);

    if (savedState <> nil) then
    begin
        AndroidLog(alInfo, Format('NativeAppGlue: Got state to load, size %d', [savedStateSize]));
        android_app^.savedState := AllocateSavedState(savedStateSize);
        android_app^.savedStateSize := savedStateSize;
        move(pbyte(savedState)^, pbyte(android_app^.savedState)^, savedStateSize);
    end;

    if FpPipe(msgpipe) <> 0 then
        AndroidLog(alError, 'NativeAppGlue: Could not create pipe');

    android_app^.msgread := msgpipe[0];
    android_app^.msgwrite := msgpipe[1];

    Thread := TAndroidAppThread.Create(Android_app, false);
    Thread.FreeOnTerminate := true;

    // Wait for thread to start.
    pthread_mutex_lock(@android_app^.mutex);
    while android_app^.running = 0 do
        pthread_cond_wait(@android_app^.cond, @android_app^.mutex);
    pthread_mutex_unlock(@android_app^.mutex);

    result := android_app;
end;

procedure android_app_write_cmd(android_app: Pandroid_app; cmd: cint8);
begin
    if fpwrite(android_app^.msgwrite, cmd, sizeof(cmd)) <> sizeof(cmd) then
        AndroidLog(alError, 'NativeAppGlue: Failure writing android_app cmd');
end;

procedure android_app_set_input(android_app: Pandroid_app; inputQueue: PAInputQueue);
begin
    pthread_mutex_lock(@android_app^.mutex);
    android_app^.pendingInputQueue := inputQueue;
    android_app_write_cmd(android_app, APP_CMD_INPUT_CHANGED);
    while (android_app^.inputQueue <> android_app^.pendingInputQueue) do
        pthread_cond_wait(@android_app^.cond, @android_app^.mutex);
    pthread_mutex_unlock(@android_app^.mutex);
end;

procedure android_app_set_window(android_app: Pandroid_app; window: PANativeWindow);
begin
    pthread_mutex_lock(@android_app^.mutex);
    if (android_app^.pendingWindow <> nil) then
        android_app_write_cmd(android_app, APP_CMD_TERM_WINDOW);

    android_app^.pendingWindow := window;
    if (window <> nil) then
        android_app_write_cmd(android_app, APP_CMD_INIT_WINDOW);

    while (android_app^.window <> android_app^.pendingWindow) do
        pthread_cond_wait(@android_app^.cond, @android_app^.mutex);

    pthread_mutex_unlock(@android_app^.mutex);
end;

procedure android_app_set_activity_state(android_app: Pandroid_app; cmd: cint8);
begin
    // AndroidLog(alInfo,' NativeAppGlue: Setting activity state to %d', [cmd]);
    pthread_mutex_lock(@android_app^.mutex);
    android_app_write_cmd(android_app, cmd);
    while (android_app^.activityState <> cmd) do
        pthread_cond_wait(@android_app^.cond, @android_app^.mutex);

    pthread_mutex_unlock(@android_app^.mutex);
end;

procedure android_app_free(android_app: Pandroid_app);
begin
    pthread_mutex_lock(@android_app^.mutex);
    android_app_write_cmd(android_app, APP_CMD_DESTROY);
    while android_app^.destroyed = 0 do
        pthread_cond_wait(@android_app^.cond, @android_app^.mutex);

    pthread_mutex_unlock(@android_app^.mutex);

    fpclose(android_app^.msgread);
    fpclose(android_app^.msgwrite);
    pthread_cond_destroy(@android_app^.cond);
    pthread_mutex_destroy(@android_app^.mutex);
    LibcFree(android_app);
end;

procedure onDestroy(activity: PANativeActivity); cdecl;
begin
  try
    AndroidLog(alInfo, 'NativeAppGlue: onDestroy');
    android_app_free(Pandroid_app(activity^.instance));
  except
    on E: TObject do
      AndroidLog(alError, 'NativeAppGlue: onDestroy exited with exception: ' + ExceptMessage(E));
  end;
end;

procedure onStart(activity: PANativeActivity); cdecl;
begin
  try
    AndroidLog(alInfo, 'NativeAppGlue: onStart');
    android_app_set_activity_state(Pandroid_app(activity^.instance), APP_CMD_START);
  except
    on E: TObject do
      AndroidLog(alError, 'NativeAppGlue: onStart exited with exception: ' + ExceptMessage(E));
  end;
end;

procedure onResume(activity: PANativeActivity); cdecl;
begin
  try
    AndroidLog(alInfo, 'NativeAppGlue: onResume');
    android_app_set_activity_state(Pandroid_app(activity^.instance), APP_CMD_RESUME);
  except
    on E: TObject do
      AndroidLog(alError, 'NativeAppGlue: onResume exited with exception: ' + ExceptMessage(E));
  end;
end;

function onSaveInstanceState(activity: PANativeActivity; outLen: pcsize_t): Pointer; cdecl;
var android_app: Pandroid_app;
    savedState: pointer;
begin
  result := nil;
  outLen^ := 0;

  try
    android_app := activity^.instance;
    savedState := nil;

    AndroidLog(alInfo, 'NativeAppGlue: onSaveInstanceState');
    pthread_mutex_lock(@android_app^.mutex);
    android_app^.stateSaved := 0;
    android_app_write_cmd(android_app, APP_CMD_SAVE_STATE);
    while android_app^.stateSaved = 0 do
        pthread_cond_wait(@android_app^.cond, @android_app^.mutex);

    if android_app^.savedState <> nil then
    begin
        savedState := android_app^.savedState;
        outLen^ := android_app^.savedStateSize;
        android_app^.savedState := nil;
        android_app^.savedStateSize := 0;
        AndroidLog(alInfo, Format('NativeAppGlue: Got state to save, size %d', [outLen^]));
    end;

    pthread_mutex_unlock(@android_app^.mutex);

    result := savedState;
  except
    on E: TObject do
      AndroidLog(alError, 'NativeAppGlue: onSaveInstanceState exited with exception: ' + ExceptMessage(E));
  end;
end;

procedure onPause(activity: PANativeActivity); cdecl;
begin
  try
    AndroidLog(alInfo, 'NativeAppGlue: onPause');
    android_app_set_activity_state(Pandroid_app(activity^.instance), APP_CMD_PAUSE);
  except
    on E: TObject do
      AndroidLog(alError, 'NativeAppGlue: onStop exited with exception: ' + ExceptMessage(E));
  end;
end;

procedure onStop(activity: PANativeActivity); cdecl;
begin
  try
    AndroidLog(alInfo, 'NativeAppGlue: onStop');
    android_app_set_activity_state(Pandroid_app(activity^.instance), APP_CMD_STOP);
  except
    on E: TObject do
      AndroidLog(alError, 'NativeAppGlue: onStop exited with exception: ' + ExceptMessage(E));
  end;
end;

procedure onConfigurationChanged(activity: PANativeActivity); cdecl;
var android_app: Pandroid_app;
begin
  try
    android_app := activity^.instance;
    AndroidLog(alInfo, 'NativeAppGlue: onConfigurationChanged');
    android_app_write_cmd(android_app, APP_CMD_CONFIG_CHANGED);
  except
    on E: TObject do
      AndroidLog(alError, 'NativeAppGlue: onConfigurationChanged exited with exception: ' + ExceptMessage(E));
  end;
end;

procedure onLowMemory(activity: PANativeActivity); cdecl;
var android_app: Pandroid_app;
begin
  try
    android_app := activity^.instance;
    AndroidLog(alInfo, 'NativeAppGlue: onLowMemory');
    android_app_write_cmd(android_app, APP_CMD_LOW_MEMORY);
  except
    on E: TObject do
      AndroidLog(alError, 'NativeAppGlue: onLowMemory exited with exception: ' + ExceptMessage(E));
  end;
end;

procedure onWindowFocusChanged(activity: PANativeActivity; focused: cint); cdecl;
begin
  try
    AndroidLog(alInfo, 'NativeAppGlue: onWindowFocusChanged');

    if focused <> 0 then
       android_app_write_cmd(activity^.instance, APP_CMD_GAINED_FOCUS)
    else
       android_app_write_cmd(activity^.instance, APP_CMD_LOST_FOCUS);
  except
    on E: TObject do
      AndroidLog(alError, 'NativeAppGlue: onWindowFocusChanged exited with exception: ' + ExceptMessage(E));
  end;
end;

procedure onNativeWindowCreated(activity: PANativeActivity; window: PANativeWindow); cdecl;
begin
  try
    AndroidLog(alInfo, 'NativeAppGlue: onNativeWindowCreated');
    android_app_set_window(activity^.instance, window);
  except
    on E: TObject do
      AndroidLog(alError, 'NativeAppGlue: onNativeWindowCreated exited with exception: ' + ExceptMessage(E));
  end;
end;

procedure onNativeWindowDestroyed(activity: PANativeActivity; window: PANativeWindow); cdecl;
begin
  try
    AndroidLog(alInfo, 'NativeAppGlue: onNativeWindowDestroyed');
    android_app_set_window(activity^.instance, nil);
  except
    on E: TObject do
      AndroidLog(alError, 'NativeAppGlue: onNativeWindowDestroyed exited with exception: ' + ExceptMessage(E));
  end;
end;

procedure onInputQueueCreated(activity: PANativeActivity; queue: PAInputQueue); cdecl;
begin
  try
    AndroidLog(alInfo, 'NativeAppGlue: onInputQueueCreated');
    android_app_set_input(activity^.instance, queue);
  except
    on E: TObject do
      AndroidLog(alError, 'NativeAppGlue: onInputQueueCreated exited with exception: ' + ExceptMessage(E));
  end;
end;

procedure onInputQueueDestroyed(activity: PANativeActivity; queue: PAInputQueue); cdecl;
begin
  try
    AndroidLog(alInfo, 'NativeAppGlue: onInputQueueDestroyed');
    android_app_set_input(activity^.instance, nil);
  except
    on E: TObject do
      AndroidLog(alError, 'NativeAppGlue: onInputQueueDestroyed exited with exception: ' + ExceptMessage(E));
  end;
end;

procedure ANativeActivity_onCreate(activity: PANativeActivity; savedState: Pointer; savedStateSize: csize_t); cdecl;
begin
  try
    AndroidLog(alInfo, 'NativeAppGlue: ANativeActivity_onCreate called, creating activity');

    activity^.callbacks^.onDestroy := @onDestroy;
    activity^.callbacks^.onStart := @onStart;
    activity^.callbacks^.onStop := @onStop;
    activity^.callbacks^.onPause := @onPause;
    activity^.callbacks^.onResume := @onResume;

    activity^.callbacks^.onNativeWindowCreated := @onNativeWindowCreated;
    activity^.callbacks^.onNativeWindowDestroyed := @onNativeWindowDestroyed;

    activity^.callbacks^.onSaveInstanceState := @onSaveInstanceState;
    activity^.callbacks^.onConfigurationChanged := @onConfigurationChanged;
    activity^.callbacks^.onLowMemory := @onLowMemory;
    activity^.callbacks^.onWindowFocusChanged := @onWindowFocusChanged;
    activity^.callbacks^.onInputQueueCreated := @onInputQueueCreated;
    activity^.callbacks^.onInputQueueDestroyed := @onInputQueueDestroyed;

    activity^.instance := android_app_create(activity, savedState, savedStateSize);
  except
    on E: TObject do
      AndroidLog(alError, 'NativeAppGlue: ANativeActivity_onCreate exited with exception: ' + ExceptMessage(E));
  end;
end;

end.
