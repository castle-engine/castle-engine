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

{$ifdef read_interface}
  { }
  TScriptType = (stCompiled, stKambiScript);

  TNodeX3DScriptNode = class(TNodeX3DChildNode, INodeX3DURLObject)
  private
    FInitialized: boolean;
    procedure SetInitialized(const Value: boolean);
    FInitializedFullUrl: string;
    FInitializedScriptType: TScriptType;

    CompiledScriptContents: TStringList;

    KambiScriptProgram: TKamScriptProgram;
    KambiScriptVariables: TKamScriptVRMLValuesList;

    procedure EventUrlReceive(
      Event: TVRMLEvent; Value: TVRMLField; const Time: TKamTime);
  protected
    procedure ExecuteScript(const FunctionName: string;
      ReceivedValue: TVRMLField);
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    destructor Destroy; override;

    private FFdurl: TMFString;
    public property Fdurl: TMFString read FFdurl;

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
      Event: TVRMLEvent; Value: TVRMLField; const Time: TKamTime);
  public
    constructor Create(const ANodeName: string; const AWWWBasePath: string); override;
    class function ClassNodeTypeName: string; override;
    function SuggestedVRMLVersion(
      out VerMajor, VerMinor, SuggestionPriority: Integer): boolean; override;
    class function URNMatching(const URN: string): boolean; override;

    private FFddirectOutput: TSFBool;
    public property FddirectOutput: TSFBool read FFddirectOutput;

    private FFdmustEvaluate: TSFBool;
    public property FdmustEvaluate: TSFBool read FFdmustEvaluate;

    procedure PostAddInterfaceDeclaration(IDecl: TVRMLInterfaceDeclaration); override;
  end;

{$endif read_interface}

{$ifdef read_implementation}
constructor TNodeX3DScriptNode.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  { We register to Fdurl.EventOut, for the reasons see x3d_networking.inc
    in TNodeInline.Create. }

  FFdurl := TMFString.Create(Self, 'url', []);
  Fields.Add(FFdurl);
  Fdurl.EventOut.OnReceive.AppendItem(@EventUrlReceive);
  { X3D specification comment: [URI] }

  DefaultContainerField := 'children';
end;

destructor TNodeX3DScriptNode.Destroy;
begin
  { shutdown by directly setting FInitialized to false,
    we don't want to call ExecuteScript at this point (as VRML graph
    is partially destroyed here). }
  FInitialized := false;

  FreeAndNil(CompiledScriptContents);

  FreeAndNil(KambiScriptProgram);
  FreeWithContentsAndNil(KambiScriptVariables);

  inherited;
end;

procedure TNodeX3DScriptNode.EventUrlReceive(
  Event: TVRMLEvent; Value: TVRMLField; const Time: TKamTime);
begin
  { Simply shutdown + initialize from new URL }
  Initialized := false;
  Initialized := true;
end;

procedure TNodeX3DScriptNode.ExecuteScript(const FunctionName: string;
  ReceivedValue: TVRMLField);

  procedure ExecuteCompiledHandler(const HandlerName: string);
  var
    I: Integer;
    HandlerInfos: TDynCompiledScriptHandlerInfoArray;
  begin
    HandlerInfos := TVRMLScene(ParentEventsProcessor).CompiledScriptHandlers;
    for I := 0 to HandlerInfos.Count - 1 do
      if HandlerInfos.Items[I].Name = HandlerName then
      begin
        HandlerInfos.Items[I].Handler(ReceivedValue,
          TVRMLScene(ParentEventsProcessor).WorldTime);
        Break;
      end;
  end;

var
  I: Integer;
  ParameterValue, ParameterTime: TKamScriptValue;
begin
  if Initialized and
     (ParentEventsProcessor <> nil) and
     (ParentEventsProcessor is TVRMLScene) then
  begin
    if Log then
      WritelnLog('VRML Script', 'Executing "' + FunctionName + '"');

    case FInitializedScriptType of
      stCompiled:
        begin
          I := CompiledScriptContents.IndexOfName(FunctionName);
          if I <> -1 then
            ExecuteCompiledHandler(CompiledScriptContents.ValueFromIndex[I]) else
          if CompiledScriptContents.IndexOf(FunctionName) <> -1 then
            { If FunctionName exists, without any value, then just execute
              the handler of the same name. }
            ExecuteCompiledHandler(FunctionName);
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
          ParameterTime := TKamScriptFloat.Create(true,
            TVRMLScene(ParentEventsProcessor).WorldTime);
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
                VRMLNonFatalError('Error when executing KambiScript: ' + E.Message);
                Exit;
              end;
            end;

            { VRMLKamScriptAfterExecute(ParameterValue, ...) is not called,
              we don't want
              to propagate changes to parameter in script to ReceivedValue.
              ReceivedValue should not be even changed by event handler.
              Actually, ParameterValue should be constant in script. }

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
    FreeWithContentsAndNil(KambiScriptVariables);

    KambiScriptVariables := TKamScriptVRMLValuesList.Create;

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
  end;

  function LoadUrl(Url: string): boolean;
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
          VRMLNonFatalError(Format(
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
          VRMLNonFatalError(Format(
            'Exception %s when loading Script from URL "%s": %s',
              [E.ClassName, PotentialFullUrl, E.Message ]));
      end;
    end else

    begin
      if Protocol <> '' then
        VRMLNonFatalError(Format('Script protocol "%s" not handled', [Protocol])) else
        VRMLNonFatalError(Format('Script from URL "%s" not handled', [Url]));
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
begin
  if FInitialized <> Value then
  begin
    if Value then
    begin
      ValidUrlFound := false;

      for I := 0 to FdUrl.Items.Count - 1 do
      begin
        if LoadUrl(FdUrl.Items[I]) then
        begin
          ValidUrlFound := true;
          Break;
        end;
      end;

      if (not ValidUrlFound) and CDataExists then
      begin
        if LoadUrl(CData) then
          ValidUrlFound := true;
      end;

      if ValidUrlFound then
      begin
        FInitialized := Value;
        ExecuteScript('initialize', nil);
      end else
      begin
        VRMLNonFatalError('Script node ignored: no supported protocol found');
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

constructor TNodeScript.Create(const ANodeName: string;
  const AWWWBasePath: string);
begin
  inherited;

  FFddirectOutput := TSFBool.Create(Self, 'directOutput', false);
  FFddirectOutput.Exposed := false;
  Fields.Add(FFddirectOutput);

  FFdmustEvaluate := TSFBool.Create(Self, 'mustEvaluate', false);
  FFdmustEvaluate.Exposed := false;
  Fields.Add(FFdmustEvaluate);

  DefaultContainerField := 'children';

  { VRML 97 says this is RestrictedAccessTypes.
    But X3D, (both the general spec, and X3D classic encoding grammar,
    so it's not by accident) allows here all access types,
    including inputOutput. }
  HasInterfaceDeclarations := AllAccessTypes;

  CDataAllowed := true;
end;

class function TNodeScript.ClassNodeTypeName: string;
begin
  Result := 'Script';
end;

function TNodeScript.SuggestedVRMLVersion(
  out VerMajor, VerMinor, SuggestionPriority: Integer): boolean;
begin
  Result := inherited SuggestedVRMLVersion(VerMajor, VerMinor, SuggestionPriority);
  { Since VRML 2.0 }
  AndSuggestedVRMLVersion(Result, VerMajor, VerMinor, SuggestionPriority,
    true, 2, 0, 1000);
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
    IDecl.Event.OnReceive.AppendItem(@IDeclEventReceive) else
  if (IDecl.Field <> nil) and IDecl.Field.Exposed then
    { We register for EventOut, to get IDeclEventReceive with new
      value of the event. }
    IDecl.Field.EventOut.OnReceive.AppendItem(@IDeclEventReceive);
end;

procedure TNodeScript.IDeclEventReceive(
  Event: TVRMLEvent; Value: TVRMLField; const Time: TKamTime);
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
