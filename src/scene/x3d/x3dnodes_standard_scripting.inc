{%MainUnit x3dnodes.pas}
{
  Copyright 2002-2023 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{$ifdef read_interface}

  { Abstract node type for all scripting nodes. }
  TAbstractScriptNode = class(TAbstractChildNode)
  strict private
    type
      TScriptType = (stCompiled, stCastleScript);
    var
      FInitialized: boolean;
      FInitializedScriptType: TScriptType;

      CompiledScriptContents: TStringList;

      CastleScriptProgram: TCasScriptProgram;
      CastleScriptVariables: TCasScriptX3DValueList;

    procedure SetInitialized(const Value: boolean);
    procedure EventUrlReceive(
      const Event: TX3DEvent; const Value: TX3DField; const Time: TX3DTime);
  protected
    procedure ExecuteScript(const FunctionName: string;
      ReceivedValue: TX3DField; const Time: TX3DTime);
  public
    constructor Create(const AX3DName: String = ''; const ABaseUrl: String = ''); override;
    destructor Destroy; override;

    { Initialize / shutdown of the script. Read this to know current
      script state, write this is to actually initialize/shutdown if needed.

      Note that the script node doesn't initialize itself at any point.
      Reason: there's no comfortable moment to do this: 1. in constructor
      some routes and such are not initialized yet, and we should initialize
      script after whole VRML graph is loaded. 2. tricks with BeforeTraverse,
      like used for TInlineNode, are not Ok, since scripts must work
      even when they are in the inactive part of VRML graph.

      If you want scripts to work, you have to initialize them yourself.
      If you use TCastleSceneCore, this will be done for you automatically
      when you change TCastleSceneCore.ProcessEvents, so it's not a problem in
      practice. }
    property Initialized: boolean read FInitialized write SetInitialized
      default false;

    { Clear the memory when the last events were generated from this script.

      @seealso TX3DRoute.ResetLastEventTime
        TCasScriptVRMLValuesList.ResetLastEventTimes }
    procedure ResetLastEventTimes;

  {$I auto_generated_node_helpers/x3dnodes_x3dscriptnode.inc}
  end;

  { Program behavior in a scene using scripts. }
  TScriptNode = class(TAbstractScriptNode)
  strict private
    procedure IDeclEventReceive(
      const Event: TX3DEvent; const Value: TX3DField; const Time: TX3DTime);
  public
    procedure PostAddInterfaceDeclaration(IDecl: TX3DInterfaceDeclaration); override;
  {$I auto_generated_node_helpers/x3dnodes_script.inc}
  end;

{$endif read_interface}

{$ifdef read_implementation}

{ TAbstractScriptNode -------------------------------------------------------- }

constructor TAbstractScriptNode.Create(const AX3DName, ABaseUrl: String);
begin
  inherited;
  AddFunctionality(TUrlFunctionality.Create(Self));
  Fdurl.AddNotification({$ifdef FPC}@{$endif} EventUrlReceive);

  { VRML 97 says this is RestrictedAccessTypes.
    X3D (both the general spec, and X3D classic encoding grammar,
    so it's not by accident) allows here all access types, including inputOutput.
    Some x3d-public mails confirm this was intentionally limited
    (easier definition) in VRML 97, and intentionally improved in X3D. }
  HasInterfaceDeclarations := AllAccessTypes;

  CDataField := FdUrl;
end;

destructor TAbstractScriptNode.Destroy;
begin
  { shutdown by directly setting FInitialized to false,
    we don't want to call ExecuteScript at this point (as VRML graph
    is partially destroyed here). }
  FInitialized := false;

  FreeAndNil(CompiledScriptContents);
  FreeAndNil(CastleScriptProgram);
  FreeAndNil(CastleScriptVariables);
  inherited;
end;

procedure TAbstractScriptNode.EventUrlReceive(
  const Event: TX3DEvent; const Value: TX3DField; const Time: TX3DTime);
begin
  { Simply shutdown + initialize from new URL }
  Initialized := false;
  Initialized := true;
end;

procedure TAbstractScriptNode.ExecuteScript(const FunctionName: string;
  ReceivedValue: TX3DField; const Time: TX3DTime);
var
  I: Integer;
  ParameterValue, ParameterTime: TCasScriptValue;
begin
  if Initialized and (Scene <> nil) then
  begin
    { Disable this by default, as it really trashes the log with a lot
      of messages in case script is used continuously (with TimeSensor or such).
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
      stCastleScript:
        begin
          { ReceivedValue may be nil for initialize/shutdown calls. }
          if ReceivedValue <> nil then
          begin
            ParameterValue := X3DCasScriptCreateValue(ReceivedValue);
            X3DCasScriptBeforeExecute(ParameterValue, ReceivedValue);
          end else
            ParameterValue := nil;
          { Note: it's not really important whether below we'd use
            Scene.Time or Time.Seconds. }
          ParameterTime := TCasScriptFloat.Create(true, Scene.Time);
          try
            CastleScriptVariables.BeforeExecute;
            try
              if ParameterValue <> nil then
                CastleScriptProgram.ExecuteFunction(FunctionName,
                  [ ParameterValue, ParameterTime ], true) else
                CastleScriptProgram.ExecuteFunction(FunctionName,
                  [ ParameterTime ], true);
            except
              on E: ECasScriptError do
              begin
                WritelnWarning('X3D', 'Error when executing CastleScript: ' + E.Message);
                Exit;
              end;
            end;

            { X3DCasScriptAfterExecute(ParameterValue, ...) is not called,
              we don't want
              to propagate changes to parameter in script to ReceivedValue.
              ReceivedValue should not be even changed by event handler.
              Actually, ParameterValue could be constant in script
              (not Writeable), but we allow it to be writeable now, for local
              changes by user (may be comfortable sometimes). }

            CastleScriptVariables.AfterExecute(Time);
          finally
            FreeAndNil(ParameterValue);
            FreeAndNil(ParameterTime)
          end;
        end;
      {$ifndef COMPILER_CASE_ANALYSIS}
      else raise EInternalError.Create('Unknown script type initialized');
      {$endif}
    end;
  end;
end;

procedure TAbstractScriptNode.SetInitialized(const Value: boolean);

  { @raises(ECasScriptSyntaxError in case of program parsing error
      (we just let through ECasScriptSyntaxError from ParseProgram).) }
  procedure CastleScriptInitialize(const ProgramContent: string);
  var
    I: Integer;
  begin
    FreeAndNil(CastleScriptProgram);
    FreeAndNil(CastleScriptVariables);

    CastleScriptVariables := TCasScriptX3DValueList.Create(true);

    { Add fields and output events (coming from interface decl,
      not normal Script fields/events) to CastleScriptVariables. }

    for I := 0 to FieldsCount - 1 do
      if Fields[I].ParentInterfaceDeclaration <> nil then
        CastleScriptVariables.Add(Fields[I]);

    for I := 0 to EventsCount - 1 do
      if (not Events[I].InEvent) and
         (Events[I].ParentInterfaceDeclaration <> nil) then
        CastleScriptVariables.Add(Events[I]);

    CastleScriptProgram := ParseProgram(ProgramContent, CastleScriptVariables);
    CastleScriptProgram.Environment.BaseUrl := BaseUrl;
  end;

  function LoadUrl(Url: string; var FailedUrlsLog: string): boolean;
  var
    ScriptContents, MimeType: string;
    ScriptContentsStream: TStream;
  begin
    Result := false;
    try
      ScriptContentsStream := Download(URL, [], MimeType);
      try

        if (MimeType = 'application/javascript') or // MIME official in RFC
           (MimeType = 'text/javascript') or // MIME common in HTML
           (MimeType = 'application/x-javascript') or // MIME  commonly returned by http servers
           (MimeType = 'text/ecmascript') or // MIME suggested by SVG
           (MimeType = 'text/plain') then // MIME accepted just in case, discouraged
           { see about MIMEs:
             http://stackoverflow.com/questions/189850/what-is-the-javascript-mime-type-what-belongs-in-the-type-attribute-of-a-script
             http://annevankesteren.nl/2005/02/javascript-mime-type }
        begin
          WritelnWarning('X3D', Format(
            'ECMAScript is not yet implemented, ignoring URL "%s"',
            [URIDisplay(URL)]));
        end else

        if MimeType = 'text/x-castlescript' then
        begin
          ScriptContents := ReadGrowingStreamToString(ScriptContentsStream);
          CastleScriptInitialize(ScriptContents);

          Result := true;
          FInitializedScriptType := stCastleScript;
        end else

        if MimeType = 'text/x-castle-compiled' then
        begin
          ScriptContents := ReadGrowingStreamToString(ScriptContentsStream);

          FreeAndNil(CompiledScriptContents);
          CompiledScriptContents := TStringList.Create;
          CompiledScriptContents.Text := ScriptContents;

          Result := true;
          FInitializedScriptType := stCompiled;
        end else

        begin
          WritelnWarning('X3D', Format(
            'Script MIME type not recognized in URL "%s": %s',
            [URIDisplay(URL), MimeType]));
        end;

      finally FreeAndNil(ScriptContentsStream) end;
    except
      { Catch exceptions from Download and CastleScriptInitialize }
      on E: Exception do
        WritelnWarning('X3D', Format(
          'Exception %s when loading Script from URL "%s": %s',
            [E.ClassName, URIDisplay(URL), E.Message ]));
    end;

    // if Result and Log and LogScript then
    //   WritelnLog('Script', Format('Loaded from URL "%s"', [URIDisplay(URL)]));
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
        if LoadUrl(PathFromBaseUrl(FdUrl.Items[I]), FailedUrlsLog) then
        begin
          ValidUrlFound := true;
          Break;
        end;
      end;

      if ValidUrlFound then
      begin
        FInitialized := Value;
        ExecuteScript('initialize', nil, Scene.NextEventTime);
      end else
      begin
        if FailedUrlsLog <> '' then
          FailedUrlsLog := ' (' + FailedUrlsLog + ')';
        WritelnWarning('X3D', 'Script node ignored: no supported protocol found' + FailedUrlsLog);
      end;
    end else
    begin
      ExecuteScript('shutdown', nil, Scene.NextEventTime);
      FInitialized := Value;
    end;
  end;
end;

procedure TAbstractScriptNode.ResetLastEventTimes;
begin
  if Initialized and (FInitializedScriptType = stCastleScript) then
    CastleScriptVariables.ResetLastEventTimes;
end;

{ TScriptNode ---------------------------------------------------------------- }

procedure TScriptNode.PostAddInterfaceDeclaration(
  IDecl: TX3DInterfaceDeclaration);
begin
  inherited;

  if (IDecl.Event <> nil) and IDecl.Event.InEvent then
    IDecl.Event.AddNotification({$ifdef FPC}@{$endif} IDeclEventReceive) else
  if (IDecl.Field <> nil) and IDecl.Field.Exposed then
    IDecl.Field.AddNotification({$ifdef FPC}@{$endif} IDeclEventReceive);
end;

procedure TScriptNode.IDeclEventReceive(
  const Event: TX3DEvent; const Value: TX3DField; const Time: TX3DTime);
var
  FunctionName: string;
begin
  if Event.ParentExposedField <> nil then
    FunctionName := Event.ParentExposedField.X3DName
  else
    FunctionName := Event.X3DName;
  ExecuteScript(FunctionName, Value, Time);
end;

procedure RegisterScriptingNodes;
begin
  NodesManager.RegisterNodeClasses([
    TScriptNode
  ]);
end;

{$endif read_implementation}
