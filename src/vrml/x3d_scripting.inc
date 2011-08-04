{
  Copyright 2002-2011 Michalis Kamburelis.

  This file is part of "Kambi VRML game engine".

  "Kambi VRML game engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Kambi VRML game engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}
  { }
  TScriptType = (stCompiled, stKambiScript);

  TNodeX3DScriptNode = class(TNodeX3DChildNode, INodeX3DURLObject)
  private
    FInitialized: boolean;
    procedure SetInitialized(const Value: boolean);
  private
    FInitializedFullUrl: string;
    FInitializedScriptType: TScriptType;

    CompiledScriptContents: TStringList;

    KambiScriptProgram: TKamScriptProgram;
    KambiScriptVariables: TKamScriptVRMLValueList;

    procedure EventUrlReceive(
      Event: TVRMLEvent; Value: TVRMLField; const Time: TVRMLTime);
  protected
    procedure ExecuteScript(const FunctionName: string;
      ReceivedValue: TVRMLField);
  public
    procedure CreateNode; override;
    destructor Destroy; override;

    private FFdUrl: TMFString;
    public property FdUrl: TMFString read FFdUrl;

    { Initialize / shutdown of the script. Read this to know current
      script state, write this is to actually initialize/shutdown if needed.

      Note that the script node doesn't initialize itself at any point.
      Reason: there's no comfortable moment to do this: 1. in constructor
      some routes and such are not initialized yet, and we should initialize
      script after whole VRML graph is loaded. 2. tricks with BeforeTraverse,
      like used for TNodeInline, are not Ok, since scripts must work
      even when they are in the inactive part of VRML graph.

      If you want scripts to work, you have to initialize them yourself.
      If you use TVRMLScene, this will be done for you automatically
      when you change TVRMLScene.ProcessEvents, so it's not a problem in
      practice. }
    property Initialized: boolean read FInitialized write SetInitialized
      default false;

    { Clear the memory when the last events were generated from this script.

      @seealso TVRMLRoute.ResetLastEventTime
        TKamScriptVRMLValuesList.ResetLastEventTimes }
    procedure ResetLastEventTimes;
  end;

  TNodeScript = class(TNodeX3DScriptNode)
  private
    procedure IDeclEventReceive(
      Event: TVRMLEvent; Value: TVRMLField; const Time: TVRMLTime);
  public
    procedure CreateNode; override;
    class function ClassNodeTypeName: string; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFdDirectOutput: TSFBool;
    public property FdDirectOutput: TSFBool read FFdDirectOutput;

    private FFdMustEvaluate: TSFBool;
    public property FdMustEvaluate: TSFBool read FFdMustEvaluate;

    procedure PostAddInterfaceDeclaration(IDecl: TVRMLInterfaceDeclaration); override;
  end;

{$endif read_interface}

{$ifdef read_implementation}
procedure TNodeX3DScriptNode.CreateNode;
begin
  inherited;

  FFdUrl := TMFString.Create(Self, 'url', []);
  Fields.Add(FFdUrl);
  Fdurl.OnReceive.Add(@EventUrlReceive);
  { X3D specification comment: [URI] }

  DefaultContainerField := 'children';

  { VRML 97 says this is RestrictedAccessTypes.
    X3D (both the general spec, and X3D classic encoding grammar,
    so it's not by accident) allows here all access types, including inputOutput.
    Some x3d-public mails confirm this was intentionally limited
    (easier definition) in VRML 97, and intentionally improved in X3D. }
  HasInterfaceDeclarations := AllAccessTypes;

  CDataField := FdUrl;
end;

destructor TNodeX3DScriptNode.Destroy;
begin
  { shutdown by directly setting FInitialized to false,
    we don't want to call ExecuteScript at this point (as VRML graph
    is partially destroyed here). }
  FInitialized := false;

  FreeAndNil(CompiledScriptContents);

  FreeAndNil(KambiScriptProgram);
  FreeAndNil(KambiScriptVariables);

  inherited;
end;

procedure TNodeX3DScriptNode.EventUrlReceive(
  Event: TVRMLEvent; Value: TVRMLField; const Time: TVRMLTime);
begin
  { Simply shutdown + initialize from new URL }
  Initialized := false;
  Initialized := true;
end;

procedure TNodeX3DScriptNode.ExecuteScript(const FunctionName: string;
  ReceivedValue: TVRMLField);
var
  I: Integer;
  ParameterValue, ParameterTime: TKamScriptValue;
begin
  if Initialized and
     (Scene <> nil) then
  begin
    { Disable this by default, as it really trashes the log with a lot
      of messages in case script is used continously (with TimeSensor or such).
    if Log then
      WritelnLog('VRML Script', 'Executing "' + FunctionName + '"');
    }

    case FInitializedScriptType of
      stCompiled:
        begin
          I := CompiledScriptContents.IndexOfName(FunctionName);
          if I <> -1 then
            Scene.ExecuteCompiledScript(CompiledScriptContents.ValueFromIndex[I], ReceivedValue) else
          if CompiledScriptContents.IndexOf(FunctionName) <> -1 then
            { If FunctionName exists, without any value, then just execute
              the handler of the same name. }
            Scene.ExecuteCompiledScript(FunctionName, ReceivedValue);
        end;
      stKambiScript:
        begin
          { ReceivedValue may be nil for initialize/shutdown calls. }
          if ReceivedValue <> nil then
          begin
            ParameterValue := VRMLKamScriptCreateValue(ReceivedValue);
            VRMLKamScriptBeforeExecute(ParameterValue, ReceivedValue);
          end else
            ParameterValue := nil;
          ParameterTime := TKamScriptFloat.Create(true, Scene.GetTime.Seconds);
          try
            KambiScriptVariables.BeforeExecute;
            try
              if ParameterValue <> nil then
                KambiScriptProgram.ExecuteFunction(FunctionName,
                  [ ParameterValue, ParameterTime ], true) else
                KambiScriptProgram.ExecuteFunction(FunctionName,
                  [ ParameterTime ], true);
            except
              on E: EKamScriptError do
              begin
                OnWarning(wtMajor, 'VRML/X3D', 'Error when executing KambiScript: ' + E.Message);
                Exit;
              end;
            end;

            { VRMLKamScriptAfterExecute(ParameterValue, ...) is not called,
              we don't want
              to propagate changes to parameter in script to ReceivedValue.
              ReceivedValue should not be even changed by event handler.
              Actually, ParameterValue could be constant in script
              (not Writeable), but we allow it to be writeable now, for local
              changes by user (may be comfortable sometimes). }

            KambiScriptVariables.AfterExecute;
          finally
            FreeAndNil(ParameterValue);
            FreeAndNil(ParameterTime)
          end;
        end;
      else raise EInternalError.Create('Unknown script type initialized');
    end;
  end;
end;

procedure TNodeX3DScriptNode.SetInitialized(const Value: boolean);

  { @raises(EKamScriptSyntaxError in case of program parsing error
      (we just let through EKamScriptSyntaxError from ParseProgram).) }
  procedure KambiScriptInitialize(const ProgramContent: string);
  var
    I: Integer;
  begin
    FreeAndNil(KambiScriptProgram);
    FreeAndNil(KambiScriptVariables);

    KambiScriptVariables := TKamScriptVRMLValueList.Create(true);

    { Add fields and output events (coming from interface decl,
      not normal Script fields/events) to KambiScriptVariables. }

    for I := 0 to Fields.Count - 1 do
      if Fields[I].ParentInterfaceDeclaration <> nil then
        KambiScriptVariables.Add(Fields[I]);

    for I := 0 to Events.Count - 1 do
      if (not Events[I].InEvent) and
         (Events[I].ParentInterfaceDeclaration <> nil) then
        KambiScriptVariables.Add(Events[I]);

    KambiScriptProgram := ParseProgram(ProgramContent, KambiScriptVariables);
    KambiScriptProgram.Environment.WWWBasePath := WWWBasePath;
  end;

  function LoadUrl(Url: string; var FailedUrlsLog: string): boolean;
  var
    Protocol, PotentialFullUrl, ScriptContents: string;
  begin
    Result := false;
    Protocol := UrlProtocol(Url);

    if Protocol = 'compiled' then
    begin
      FreeAndNil(CompiledScriptContents);
      CompiledScriptContents := TStringList.Create;
      CompiledScriptContents.Text := UrlDeleteProtocol(Url);

      Result := true;
      FInitializedScriptType := stCompiled;
      FInitializedFullUrl := Url;
    end else

    if Protocol = 'kambiscript' then
    begin
      try
        KambiScriptInitialize(UrlDeleteProtocol(Url));

        Result := true;
        FInitializedScriptType := stKambiScript;
        FInitializedFullUrl := Url;
      except
        { This catches exceptions from KambiScriptInitialize }
        on E: EKamScriptSyntaxError do
          OnWarning(wtMajor, 'VRML/X3D', Format(
            'Exception %s when parsing KambiScript: %s',
              [E.ClassName, E.Message ]));
      end;
    end else

    if (Protocol = '') and
       SameText(ExtractFileExt(Url), '.kscript') then
    begin
      PotentialFullUrl := PathFromWWWBasePath(Url);
      try
        ScriptContents := FileToString(PotentialFullUrl);

        KambiScriptInitialize(ScriptContents);

        Result := true;
        FInitializedScriptType := stKambiScript;
        FInitializedFullUrl := PotentialFullUrl;
      except
        { This catches exceptions from FileToString,
          and exceptions from KambiScriptInitialize
          (EKamScriptSyntaxError only). }
        on E: Exception do
          OnWarning(wtMinor, 'VRML/X3D', Format(
            'Exception %s when loading Script from URL "%s": %s',
              [E.ClassName, PotentialFullUrl, E.Message ]));
      end;
    end else

    begin
      if FailedUrlsLog <> '' then
        FailedUrlsLog += ', ';
      if Protocol <> '' then
        FailedUrlsLog += Format('protocol "%s" not handled', [Protocol]) else
        FailedUrlsLog += Format('URL "%s" not handled', [Url]);
    end;

    { TODO: checks for Protocol <> '', above and below, are done
      to avoid showing full Script content in case of inline scripts.
      We currently assume that Protocol <> '' then we have inline script
      content --- but this is bad in the long run, as URLs to files
      may also have protocol (http, file etc.). We just don't handle
      URLs to files right now. }

    if Result and Log then
    begin
      if Protocol <> '' then
        WritelnLog('VRML Script', Format('Loaded from protocol "%s"', [Protocol])) else
        WritelnLog('VRML Script', Format('Loaded from URL "%s"', [Url]));
    end;
  end;

var
  ValidUrlFound: boolean;
  I: Integer;
  FailedUrlsLog: string;
begin
  if FInitialized <> Value then
  begin
    if Value then
    begin
      ValidUrlFound := false;
      FailedUrlsLog := '';

      for I := 0 to FdUrl.Items.Count - 1 do
      begin
        if LoadUrl(FdUrl.Items[I], FailedUrlsLog) then
        begin
          ValidUrlFound := true;
          Break;
        end;
      end;

      if ValidUrlFound then
      begin
        FInitialized := Value;
        ExecuteScript('initialize', nil);
      end else
      begin
        if FailedUrlsLog <> '' then
          FailedUrlsLog := ' (' + FailedUrlsLog + ')';
        OnWarning(wtMinor, 'VRML/X3D', 'Script node ignored: no supported protocol found' + FailedUrlsLog);
      end;
    end else
    begin
      ExecuteScript('shutdown', nil);
      FInitialized := Value;
    end;
  end;
end;

procedure TNodeX3DScriptNode.ResetLastEventTimes;
begin
  if Initialized and (FInitializedScriptType = stKambiScript) then
    KambiScriptVariables.ResetLastEventTimes;
end;

procedure TNodeScript.CreateNode;
begin
  inherited;

  FFdDirectOutput := TSFBool.Create(Self, 'directOutput', false);
  FFdDirectOutput.Exposed := false;
  Fields.Add(FFdDirectOutput);

  FFdMustEvaluate := TSFBool.Create(Self, 'mustEvaluate', false);
  FFdMustEvaluate.Exposed := false;
  Fields.Add(FFdMustEvaluate);

  DefaultContainerField := 'children';
end;

class function TNodeScript.ClassNodeTypeName: string;
begin
  Result := 'Script';
end;

class function TNodeScript.URNMatching(const URN: string): boolean;
begin
  Result := (inherited URNMatching(URN)) or
    (URN = URNVRML97Nodes + ClassNodeTypeName) or
    (URN = URNX3DNodes + ClassNodeTypeName);
end;

procedure TNodeScript.PostAddInterfaceDeclaration(
  IDecl: TVRMLInterfaceDeclaration);
begin
  inherited;

  if (IDecl.Event <> nil) and IDecl.Event.InEvent then
    IDecl.Event.OnReceive.Add(@IDeclEventReceive) else
  if (IDecl.Field <> nil) and IDecl.Field.Exposed then
    IDecl.Field.OnReceive.Add(@IDeclEventReceive);
end;

procedure TNodeScript.IDeclEventReceive(
  Event: TVRMLEvent; Value: TVRMLField; const Time: TVRMLTime);
var
  FunctionName: string;
begin
  if Event.ParentExposedField <> nil then
    FunctionName := Event.ParentExposedField.Name else
    FunctionName := Event.Name;
  ExecuteScript(FunctionName, Value);
end;

procedure RegisterScriptingNodes;
begin
  NodesManager.RegisterNodeClasses([
    TNodeScript
  ]);
end;

{$endif read_implementation}
