{
    This file is part of the Free Pascal run time library.
    Copyright (c) 2003 by the Free Pascal development team

    CustomApplication class.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}

{$I castleconf.inc} // hide useless Delphi hints/warnings

{$h+}
unit CustApp;

Interface

uses SysUtils,Classes, CastleUtils, singleinstance;

Type
  TStringArray = Array of string;
  TExceptionEvent = Procedure (Sender : TObject; E : Exception) Of Object;
  TEventLogTypes = Set of TEventType;

  { TCustomApplication }

  TCustomApplication = Class(TComponent)
  Private
    FEventLogFilter: TEventLogTypes;
    FOnException: TExceptionEvent;
    FSingleInstance: TBaseSingleInstance;
    FSingleInstanceClass: TBaseSingleInstanceClass; // set before FSingleInstance is created
    FSingleInstanceEnabled: Boolean; // set before Initialize is called
    FTerminated : Boolean;
    FHelpFile,
    FTitle : String;
    FOptionChar : Char;
    FCaseSensitiveOptions : Boolean;
    FStopOnException : Boolean;
    FExceptionExitCode : Integer;
    function GetEnvironmentVar(VarName : String): String;
    function GetExeName: string;
    Function GetLocation : String;
    function GetSingleInstance: TBaseSingleInstance;
    procedure SetSingleInstanceClass(
      const ASingleInstanceClass: TBaseSingleInstanceClass);
    function GetTitle: string;
  Protected
    function GetOptionAtIndex(AIndex: Integer; IsLong: Boolean): String;
    procedure SetTitle(const AValue: String); Virtual;
    Function GetConsoleApplication : boolean; Virtual;
    Procedure DoRun; Virtual;
    Function GetParams(Index : Integer) : String;virtual;
    function GetParamCount: Integer;Virtual;
    Procedure DoLog(EventType : TEventType; const Msg : String);  virtual;
  Public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    // Some Delphi methods.
    procedure HandleException(Sender: TObject); virtual;
    procedure Initialize; virtual;
    procedure Run;
    procedure ShowException(E: Exception);virtual;
    procedure Terminate; overload; virtual;
    procedure Terminate(AExitCode : Integer); overload; virtual;
    // Extra methods.
    function FindOptionIndex(Const S : String; Var Longopt : Boolean; StartAt : Integer = -1) : Integer;
    Function GetOptionValue(Const S : String) : String; overload;
    Function GetOptionValue(Const C: Char; Const S : String) : String; overload;
    Function GetOptionValues(Const C: Char; Const S : String) : TStringArray;
    Function HasOption(Const S : String) : Boolean; overload;
    Function HasOption(Const C : Char; Const S : String) : Boolean; overload;
    Function CheckOptions(Const ShortOptions : String; Const Longopts : TStrings; Opts,NonOpts : TStrings; AllErrors : Boolean = False) : String; overload;
    Function CheckOptions(Const ShortOptions : String; Const Longopts : Array of string; Opts,NonOpts : TStrings; AllErrors : Boolean = False) : String; overload;
    Function CheckOptions(Const ShortOptions : String; Const Longopts : TStrings; AllErrors : Boolean = False) : String; overload;
    Function CheckOptions(Const ShortOptions : String; Const LongOpts : Array of string; AllErrors : Boolean = False) : String; overload;
    Function CheckOptions(Const ShortOptions : String; Const LongOpts : String; AllErrors : Boolean = False) : String; overload;
    Function GetNonOptions(Const ShortOptions : String; Const Longopts : Array of string) : TStringArray; overload;
    Procedure GetNonOptions(Const ShortOptions : String; Const Longopts : Array of string; NonOptions : TStrings); overload;
    Procedure GetEnvironmentList(List : TStrings;NamesOnly : Boolean); overload;
    Procedure GetEnvironmentList(List : TStrings); overload;
    Procedure Log(EventType : TEventType; const Msg : String); overload;
    Procedure Log(EventType : TEventType; const Fmt : String; const Args : array of const); overload;
    // Delphi properties
    property ExeName: string read GetExeName;
    property HelpFile: string read FHelpFile write FHelpFile;
    property Terminated: Boolean read FTerminated;
    property Title: string read FTitle write SetTitle;
    property OnException: TExceptionEvent read FOnException write FOnException;
    // Extra properties
    Property ConsoleApplication : Boolean Read GetConsoleApplication;
    Property Location : String Read GetLocation;
    Property Params [Index : integer] : String Read GetParams;
    Property ParamCount : Integer Read GetParamCount;
    Property EnvironmentVariable[envName : String] : String Read GetEnvironmentVar;
    Property OptionChar : Char Read FoptionChar Write FOptionChar;
    Property CaseSensitiveOptions : Boolean Read FCaseSensitiveOptions Write FCaseSensitiveOptions;
    Property StopOnException : Boolean Read FStopOnException Write FStopOnException;
    Property ExceptionExitCode : Integer Read FExceptionExitCode Write FExceptionExitCode;
    Property EventLogFilter : TEventLogTypes Read FEventLogFilter Write FEventLogFilter;
    Property SingleInstance: TBaseSingleInstance read GetSingleInstance;
    Property SingleInstanceClass: TBaseSingleInstanceClass read FSingleInstanceClass write SetSingleInstanceClass;
    Property SingleInstanceEnabled: Boolean read FSingleInstanceEnabled write FSingleInstanceEnabled;
  end;

var CustomApplication : TCustomApplication = nil;

Implementation

{$ifdef darwin}
uses
  MacOSAll;
{$endif}

{ TCustomApplication }

function TCustomApplication.GetExeName: string;
{$if defined(darwin)}
var
  mainBundle: CFBundleRef;
  executableUrl: CFURLRef;
  executableFSPath: CFStringRef;
  utf16len: ptrint;
  error: boolean;
begin
  error:=false;
  { Get main bundle. This even works most of the time for command line
    applications
  }
  mainbundle:=CFBundleGetMainBundle;
  if assigned(mainbundle) then
    begin
      { get the URL pointing to the executable of the bundle }
      executableUrl:=CFBundleCopyExecutableURL(mainBundle);
      if assigned(executableUrl) then
        begin
          { convert the url to a POSIX path }
          executableFSPath:=CFURLCopyFileSystemPath(executableUrl,kCFURLPOSIXPathStyle);
          CFRelease(executableUrl);
          { convert to UTF-8 -- this is not really clean since in theory the
            ansi-encoding could be different, but
              a) all file i/o routines on Darwin expect utf-8-encoded strings
              b) there is no easy way to convert the Unix LANG encoding
                 setting to an equivalent CoreFoundation encoding
          }
          utf16len:=CFStringGetLength(executableFSPath);
          // +1 for extra terminating #0 in the worst case, so the pos below
          // will always find the #0
          setlength(result,utf16len*3+1);
          if CFStringGetCString(executableFSPath,@result[1],length(result),kCFStringEncodingUTF8) then
            { truncate to actual length, #0 cannot appear in a file path }
            setlength(result,pos(#0,result)-1)
          else
            error:=true;
          CFRelease(executableFSPath);
        end
      else
        error:=true;
    end
  else
    error:=true;
  if error then
    { can't do better than this }
    Result:=Paramstr(0);
end;
{$else darwin}
begin
  Result:=Paramstr(0);
end;
{$endif darwin}

Procedure SysGetEnvironmentList(List : TStrings;NamesOnly : Boolean);
{var
   s : string;
   i,l,j,count : Integer;}
begin
  raise Exception.Create('Not implemented!');
  {count:=GetEnvironmentVariableCount;
  if count>0 then
    for j:=1 to count  do
     begin
       s:=GetEnvironmentString(j);
       l:=Length(s);
       If NamesOnly then
          begin
            I:=pos('=',s);
            If (I>0) then
              S:=Copy(S,1,I-1);
          end;
       List.Add(S);
    end;}
end;

function TCustomApplication.GetEnvironmentVar(VarName : String): String;
begin
  Result:=GetEnvironmentVariable(VarName);
end;

procedure TCustomApplication.GetEnvironmentList(List: TStrings;
  NamesOnly: Boolean);

begin
  // Routine must be in custapp.inc
  SysGetEnvironmentList(List,NamesOnly);
end;

procedure TCustomApplication.GetEnvironmentList(List: TStrings);

begin
  GetEnvironmentList(List,False);
end;

function TCustomApplication.GetLocation: String;
begin
  Result:=ExtractFilePath(GetExeName);
end;

function TCustomApplication.GetParamCount: Integer;
begin
   Result:=System.ParamCount;
end;

function TCustomApplication.GetTitle: string;
begin
  Result:=FTitle;
end;

function TCustomApplication.GetParams(Index: Integer): String;
begin
  Result:=ParamStr(Index);
end;

function TCustomApplication.GetSingleInstance: TBaseSingleInstance;
begin
  if FSingleInstance = nil then
    begin
    if FSingleInstanceClass=Nil then
      Raise ESingleInstance.Create('No single instance provider class set! Include a single-instance class unit such as advsingleinstance');
    FSingleInstance := FSingleInstanceClass.Create(Self);
    end;
  Result := FSingleInstance;
end;

procedure TCustomApplication.SetTitle(const AValue: String);
begin
  FTitle:=AValue;
end;

function TCustomApplication.GetConsoleApplication: boolean;
begin
  Result:=IsConsole;
end;

procedure TCustomApplication.DoRun;
begin
  if Assigned(FSingleInstance) then
    if FSingleInstance.IsServer then
      FSingleInstance.ServerCheckMessages;

  // Override in descendent classes.
end;

procedure TCustomApplication.DoLog(EventType: TEventType; const Msg: String);

begin
  // Do nothing, override in descendants
end;

procedure TCustomApplication.Log(EventType: TEventType; const Msg: String);

begin
  If (FEventLogFilter=[]) or (EventType in FEventLogFilter) then
    DoLog(EventType,Msg);
end;

procedure TCustomApplication.Log(EventType: TEventType; const Fmt: String;
  const Args: array of const);
begin
  try
    Log(EventType, Format(Fmt, Args));
  except
    On E : Exception do
      Log(etError,Format('Error formatting message "%s" with %d arguments: %s',[Fmt,Length(Args),E.Message]));
  end
end;

constructor TCustomApplication.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOptionChar:='-';
  FCaseSensitiveOptions:=True;
  FStopOnException:=False;
  FSingleInstanceClass := DefaultSingleInstanceClass;
end;

destructor TCustomApplication.Destroy;
begin
  inherited Destroy;
end;

procedure TCustomApplication.HandleException(Sender: TObject);
begin
  If Not (ExceptObject is Exception) then
    SysUtils.showexception(ExceptObject,ExceptAddr)
  else
    begin
    If Not Assigned(FOnexception) then
      ShowException(Exception(ExceptObject))
    else
      FOnException(Sender,Exception(ExceptObject));
    end;
  If FStopOnException then
    Terminate(ExceptionExitCode);
end;


procedure TCustomApplication.Initialize;
begin
  FTerminated:=False;
  if FSingleInstanceEnabled then
  begin
    case SingleInstance.Start of
      siClient:
      begin
        SingleInstance.ClientPostParams;
        FTerminated:=True;
      end;
      siNotResponding:
        FTerminated:=True;
    end;
  end;
end;

procedure TCustomApplication.Run;

begin
  Repeat
    Try
      DoRun;
    except
      HandleException(Self);
    end;
  Until FTerminated;
end;

procedure TCustomApplication.SetSingleInstanceClass(
  const ASingleInstanceClass: TBaseSingleInstanceClass);
begin
  Assert((FSingleInstance = nil) and (ASingleInstanceClass <> nil));
  FSingleInstanceClass := ASingleInstanceClass;
end;

procedure TCustomApplication.ShowException(E: Exception);

begin
  Sysutils.ShowException(E,ExceptAddr)
end;

procedure TCustomApplication.Terminate;
begin
  Terminate(ExitCode);
end;

procedure TCustomApplication.Terminate(AExitCode : Integer) ;

begin
  FTerminated:=True;
  ExitCode:=AExitCode;
end;

function TCustomApplication.GetOptionAtIndex(AIndex : Integer; IsLong: Boolean): String;

Var
  P : Integer;
  O : String;

begin
  Result:='';
  If (AIndex=-1) then
    Exit;
  If IsLong then
    begin // Long options have form --option=value
    O:=Params[AIndex];
    P:=Pos('=',O);
   If (P=0) then
      P:=Length(O);
    Delete(O,1,P);
    Result:=O;
    end
  else
    begin // short options have form '-o value'
    If (AIndex<ParamCount) then
      if (Copy(Params[AIndex+1],1,1)<>OptionChar) then
        Result:=Params[AIndex+1];
    end;
  end;


function TCustomApplication.GetOptionValue(const S: String): String;
begin
  Result:=GetoptionValue(#255,S);
end;

function TCustomApplication.GetOptionValue(const C: Char; const S: String
  ): String;

Var
  B : Boolean;
  I : integer;

begin
  Result:='';
  I:=FindOptionIndex(C,B);
  If (I=-1) then
    I:=FindOptionIndex(S,B);
  If I<>-1 then
    Result:=GetOptionAtIndex(I,B);
end;

function TCustomApplication.GetOptionValues(const C: Char; const S: String): TStringArray;

Var
  I,Cnt : Integer;
  B : Boolean;

begin
  SetLength(Result,ParamCount);
  Cnt:=0;
  I:=-1;
  Repeat
    I:=FindOptionIndex(C,B,I);
    If I<>-1 then
      begin
      Inc(Cnt);
      Dec(I);
      end;
  Until I=-1;
  Repeat
    I:=FindOptionIndex(S,B,I);
    If I<>-1 then
      begin
      Inc(Cnt);
      Dec(I);
      end;
  Until I=-1;
  SetLength(Result,Cnt);
  Cnt:=0;
  I:=-1;
  Repeat
    I:=FindOptionIndex(C,B,I);
    If (I<>-1) then
      begin
      Result[Cnt]:=GetOptionAtIndex(I,False);
      Inc(Cnt);
      Dec(i);
      end;
  Until (I=-1);
  I:=-1;
  Repeat
    I:=FindOptionIndex(S,B,I);
    If I<>-1 then
      begin
      Result[Cnt]:=GetOptionAtIndex(I,True);
      Inc(Cnt);
      Dec(i);
      end;
  Until (I=-1);
end;

function TCustomApplication.HasOption(const S: String): Boolean;

Var
  B : Boolean;

begin
  Result:=FindOptionIndex(S,B)<>-1;
end;

function TCustomApplication.FindOptionIndex(const S: String;
  var Longopt: Boolean; StartAt : Integer = -1): Integer;

Var
  SO,O : String;
  I,P : Integer;

begin
  If Not CaseSensitiveOptions then
    SO:=UpperCase(S)
  else
    SO:=S;
  Result:=-1;
  I:=StartAt;
  if (I=-1) then
    I:=ParamCount;
  While (Result=-1) and (I>0) do
    begin
    O:=Params[i];
    // - must be seen as an option value
    If (Length(O)>1) and (O[1]=FOptionChar) then
      begin
      Delete(O,1,1);
      LongOpt:=(Length(O)>0) and (O[1]=FOptionChar);
      If LongOpt then
        begin
        Delete(O,1,1);
        P:=Pos('=',O);
        If (P<>0) then
          O:=Copy(O,1,P-1);
        end;
      If Not CaseSensitiveOptions then
        O:=UpperCase(O);
      If (O=SO) then
        Result:=i;
      end;
    Dec(i);
    end;
end;

function TCustomApplication.HasOption(const C: Char; const S: String): Boolean;

Var
  B : Boolean;

begin
  Result:=(FindOptionIndex(C,B)<>-1) or (FindOptionIndex(S,B)<>-1);
end;


function TCustomApplication.CheckOptions(const ShortOptions: String;
  const Longopts: TStrings; AllErrors: Boolean): String;

begin
  Result:=CheckOptions(ShortOptions,LongOpts,Nil,Nil,AllErrors);
end;

ResourceString
  SErrInvalidOption = 'Invalid option at position %d: "%s"';
  SErrNoOptionAllowed = 'Option at position %d does not allow an argument: %s';
  SErrOptionNeeded = 'Option at position %d needs an argument : %s';

function TCustomApplication.CheckOptions(const ShortOptions: String;
  const Longopts: TStrings; Opts, NonOpts: TStrings; AllErrors: Boolean
  ): String;

Var
  I,J,L,P : Integer;
  O,OV,SO : String;
  UsedArg,HaveArg : Boolean;

  Function FindLongOpt(S : String) : boolean;

  Var
    I : integer;

  begin
    Result:=Assigned(LongOpts);
    if Not Result then
      exit;
    If CaseSensitiveOptions then
      begin
      I:=LongOpts.Count-1;
      While (I>=0) and (LongOpts[i]<>S) do
        Dec(i);
      end
    else
      begin
      S:=UpperCase(S);
      I:=LongOpts.Count-1;
      While (I>=0) and (UpperCase(LongOpts[i])<>S) do
        Dec(i);
      end;
    Result:=(I<>-1);
  end;

  Procedure AddToResult(Const Msg : string);

  begin
    If (Result<>'') then
      Result:=Result+sLineBreak;
    Result:=Result+Msg;
  end;

begin
  If CaseSensitiveOptions then
    SO:=Shortoptions
  else
    SO:=LowerCase(Shortoptions);
  Result:='';
  I:=1;
  While (I<=ParamCount) and ((Result='') or AllErrors) do
    begin
    O:=Paramstr(I);
    If (Length(O)=0) or (O[1]<>FOptionChar) then
      begin
      If Assigned(NonOpts) then
        NonOpts.Add(O);
      end
    else
      begin
      If (Length(O)<2) then
        AddToResult(Format(SErrInvalidOption,[i,O]))
      else
        begin
        HaveArg:=False;
        OV:='';
        // Long option ?
        If (O[2]=FOptionChar) then
          begin
          Delete(O,1,2);
          J:=Pos('=',O);
          If J<>0 then
            begin
            HaveArg:=true;
            OV:=O;
            Delete(OV,1,J);
            O:=Copy(O,1,J-1);
            end;
          // Switch Option
          If FindLongopt(O) then
            begin
            If HaveArg then
              AddToResult(Format(SErrNoOptionAllowed,[I,O]));
            end
          else
            begin // Required argument
            If FindLongOpt(O+':') then
              begin
              If Not HaveArg then
                AddToResult(Format(SErrOptionNeeded,[I,O]));
              end
            else
              begin // Optional Argument.
              If not FindLongOpt(O+'::') then
                AddToResult(Format(SErrInvalidOption,[I,O]));
              end;
            end;
          end
        else // Short Option.
          begin
          HaveArg:=(I<ParamCount) and (Length(ParamStr(I+1))>0) and (ParamStr(I+1)[1]<>FOptionChar);
          UsedArg:=False;
          If Not CaseSensitiveOptions then
            O:=LowerCase(O);
          L:=Length(O);
          J:=2;
          While ((Result='') or AllErrors) and (J<=L) do
            begin
            P:=Pos(O[J],SO);
            If (P=0) or (O[j]=':') then
              AddToResult(Format(SErrInvalidOption,[I,O[J]]))
            else
              begin
              If (P<Length(SO)) and (SO[P+1]=':') then
                begin
                // Required argument
                If ((P+1)=Length(SO)) or (SO[P+2]<>':') Then
                  If (J<L) or not haveArg then // Must be last in multi-opt !!
                    AddToResult(Format(SErrOptionNeeded,[I,O[J]]));
                O:=O[j]; // O is added to arguments.
                UsedArg:=True;
                end;
              end;
            Inc(J);
            end;
          HaveArg:=HaveArg and UsedArg;
          If HaveArg then
            begin
            Inc(I); // Skip argument.
            OV:=Paramstr(I);
            end;
          end;
        If HaveArg and ((Result='') or AllErrors) then
          If Assigned(Opts) then
            Opts.Add(O+'='+OV);
        end;
      end;
    Inc(I);
    end;
end;

function TCustomApplication.CheckOptions(const ShortOptions: String;
  const Longopts: array of string; Opts, NonOpts: TStrings; AllErrors: Boolean
  ): String;
Var
  L : TStringList;
  I : Integer;

begin
  L:=TStringList.Create;
  Try
    For I:=0 to High(LongOpts) do
      L.Add(LongOpts[i]);
    Result:=CheckOptions(ShortOptions,L,Opts,NonOpts,AllErrors);
  Finally
    L.Free;
  end;
end;

function TCustomApplication.CheckOptions(const ShortOptions: String;
  const LongOpts: array of string; AllErrors: Boolean): String;

Var
  L : TStringList;
  I : Integer;

begin
  L:=TStringList.Create;
  Try
    For I:=0 to High(LongOpts) do
      L.Add(LongOpts[i]);
    Result:=CheckOptions(ShortOptions,L,AllErrors);
  Finally
    L.Free;
  end;
end;

function TCustomApplication.CheckOptions(const ShortOptions: String;
  const LongOpts: String; AllErrors: Boolean): String;

Const
  SepChars = ' '#10#13#9;

Var
  L : TStringList;
  Len,I,J : Integer;

begin
  L:=TStringList.Create;
  Try
    I:=1;
    Len:=Length(LongOpts);
    While I<=Len do
      begin
      While Isdelimiter(SepChars,LongOpts,I) do
        Inc(I);
      J:=I;
      While (J<=Len) and Not IsDelimiter(SepChars,LongOpts,J) do
        Inc(J);
      If (I<=J) then
        L.Add(Copy(LongOpts,I,(J-I)));
      I:=J+1;
      end;
    Result:=CheckOptions(Shortoptions,L,AllErrors);
  Finally
    L.Free;
  end;
end;

function TCustomApplication.GetNonOptions(const ShortOptions: String;
  const Longopts: array of string): TStringArray;

Var
  NO : TStrings;
  I : Integer;

begin
  No:=TStringList.Create;
  try
    GetNonOptions(ShortOptions,LongOpts,No);
    SetLength(Result,NO.Count);
    For I:=0 to NO.Count-1 do
      Result[I]:=NO[i];
  finally
    NO.Free;
  end;
end;

procedure TCustomApplication.GetNonOptions(const ShortOptions: String;
  const Longopts: array of string; NonOptions: TStrings);

Var
  S : String;

begin
  S:=CheckOptions(ShortOptions,LongOpts,Nil,NonOptions,true);
  if (S<>'') then
    Raise EListError.Create(S);
end;

end.
