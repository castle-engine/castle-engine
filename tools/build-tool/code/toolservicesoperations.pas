{
  Copyright 2021-2021 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.md,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Services (on Android and iOS) operations,
  to be done when actually generating project using them. }
unit ToolServicesOperations;

interface

uses SysUtils, Generics.Collections, DOM,
  CastleUtils, CastleStringUtils,
  ToolServices, ToolProject;

type
  TServiceManifest = class
  strict private
    FService: TService;
  public
    type
      TServiceCopy = class
        Source, Target: String;
      end;
      TServiceCopyList = specialize TObjectList<TServiceCopy>;
    var
      PackageOperations: TServiceCopyList;

      { Minimum value of the project's Android min_sdk_version
        required by this service, or 0 if the service has no such requirement.
        Read from min_min_sdk_version attribute of <service>. }
      MinMinSdkVersion: Cardinal;

    constructor Create(const AService: TService);
    destructor Destroy; override;
    procedure Load(const UrlServicePath: String);

    { Call all package operations for this service.
      OutputPath is passed from PackageServices. }
    procedure Package(const Project: TCastleProject; const OutputPath: String);

    property Service: TService read FService;
  end;

  TServiceManifestList = specialize TObjectList<TServiceManifest>;

{ Load CastleEngineService.xml of all services.
  The result must be freed by the caller.

  UrlServicesPath must be an absolute URL (may, but doesn't have to, end with /)
  pointing to the CGE read-only files of services. }
function LoadServices(const Services: TServiceList;
  const UrlServicesPath: String): TServiceManifestList;

{ Call all package operations for all services,
  realizing the instructions from service CastleEngineService.xml.

  ServiceManifests should come from LoadServices.

  OutputPath must be an absolute path (may, but doesn't have to, end with PathDelim)
  pointing to the writeable files where service templates are unpacked now. }
procedure PackageServices(const Project: TCastleProject;
  const ServiceManifests: TServiceManifestList; const OutputPath: String);

{ Raise the project's Android min SDK version, if any service requires it
  (has min_min_sdk_version in CastleEngineService.xml).
  Makes an exception if the resulting min SDK version would be larger
  than target SDK version.

  Must be called before generating files from templates
  (that use ANDROID_MIN_SDK_VERSION macro). }
procedure ApplyServicesAndroidMinSdkVersion(const Project: TCastleProject;
  const ServiceManifests: TServiceManifestList);

implementation

uses Classes, XMLRead,
  CastleXmlUtils, CastleUriUtils, CastleFilesUtils,
  ToolUtils, ToolCommonUtils;

{ TServiceManifest ----------------------------------------------------------- }

constructor TServiceManifest.Create(const AService: TService);
begin
  inherited Create;
  FService := AService;
  PackageOperations := TServiceCopyList.Create;
end;

destructor TServiceManifest.Destroy;
begin
  FreeAndNil(PackageOperations);
  inherited;
end;

procedure TServiceManifest.Load(const UrlServicePath: String);
var
  UrlServiceManifest: String;
  Doc: TXMLDocument;
  PackageElement: TDOMElement;
  I: TXMLElementIterator;
  CopyOperation: TServiceCopy;
begin
  PackageOperations.Clear;
  MinMinSdkVersion := 0;

  UrlServiceManifest := CombineURI(URIIncludeSlash(UrlServicePath), 'CastleEngineService.xml');
  if URIFileExists(UrlServiceManifest) then
  begin
    Doc := UrlReadXml(UrlServiceManifest);
    try
      if Doc.DocumentElement.TagName8 <> 'service' then
        raise Exception.CreateFmt('Root element of CastleEngineService.xml must be <service>, not "%s"', [
          Doc.DocumentElement.TagName8
        ]);
      MinMinSdkVersion := Doc.DocumentElement.AttributeCardinalDef('min_min_sdk_version', 0);
      PackageElement := Doc.DocumentElement.Child('package', false);
      if PackageElement <> nil then
      begin
        I := PackageElement.ChildrenIterator;
        try
          while I.GetNext do
          begin
            if I.Current.TagName8 <> 'copy' then
              raise Exception.CreateFmt('Children of <package> must be <copy>, not "%s"', [
                I.Current.TagName8
              ]);
            CopyOperation := TServiceCopy.Create;
            CopyOperation.Source := I.Current.AttributeString('source');
            CopyOperation.Target := I.Current.AttributeString('target');
            PackageOperations.Add(CopyOperation);
          end;
        finally FreeAndNil(I) end;
      end;
    finally FreeAndNil(Doc) end;
  end;
end;

procedure TServiceManifest.Package(const Project: TCastleProject; const OutputPath: String);
var
  C: TServiceCopy;
  Source, Target: String;
begin
  for C in PackageOperations do
  begin
    (* Copy.Source may be something like '${IOS.FMOD.LIBRARY_PATH}' and the macro
      may expand to an absolute path, or a path relative to the project. *)
    Source := CombinePaths(Project.Path, Project.ReplaceMacros(C.Source));

    if IsPathAbsolute(C.Target) then
      raise Exception.CreateFmt('Target to copy package file in CastleEngineService.xml is an absolute path, it should be relative to output: "%s"', [
        C.Target
      ]);
    Target := CombinePaths(OutputPath, C.Target);
    if not IsPathAbsolute(Target) then
      raise Exception.CreateFmt('Target to copy package file is not an absolute path: "%s"', [
        Target
      ]);
    if RegularFileExists(Target) then
      raise Exception.CreateFmt('Service file "%s" already exists, specified by multiple services: %s', [
        Target
      ]);

    // all checks passed, do the actual file operations (create dir, copy file)
    SmartCopyFile(Source, Target);
    WritelnVerbose(Format('Packaging service %s: Copied %s => %s', [
      FService.Name,
      Source,
      Target
    ]));
  end;
end;

{ routines ------------------------------------------------------------------- }

function LoadServices(const Services: TServiceList;
  const UrlServicesPath: String): TServiceManifestList;
var
  Service: TService;
  ServiceManifest: TServiceManifest;
begin
  Result := TServiceManifestList.Create(true);
  try
    for Service in Services do
    begin
      ServiceManifest := TServiceManifest.Create(Service);
      Result.Add(ServiceManifest);
      ServiceManifest.Load(CombineURI(URIIncludeSlash(UrlServicesPath), Service.Name));
    end;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

procedure PackageServices(const Project: TCastleProject;
  const ServiceManifests: TServiceManifestList; const OutputPath: String);
var
  ServiceManifest: TServiceManifest;
begin
  for ServiceManifest in ServiceManifests do
    ServiceManifest.Package(Project, OutputPath);
end;

procedure ApplyServicesAndroidMinSdkVersion(const Project: TCastleProject;
  const ServiceManifests: TServiceManifestList);
var
  ServiceManifest: TServiceManifest;
begin
  for ServiceManifest in ServiceManifests do
    if ServiceManifest.MinMinSdkVersion > Project.AndroidMinSdkVersion then
    begin
      Writeln(Format('Bumping project''s Android min SDK version to %d (from %d) because service "%s" requires it', [
        ServiceManifest.MinMinSdkVersion,
        Project.AndroidMinSdkVersion,
        ServiceManifest.Service.Name
      ]));
      Project.AndroidMinSdkVersionFromServices := ServiceManifest.MinMinSdkVersion;
    end;

  if Project.AndroidMinSdkVersion > Project.AndroidTargetSdkVersion then
    raise Exception.CreateFmt('Android min SDK version %d (required by services) is larger than target_sdk_version %d, this is incorrect. Increase target_sdk_version in CastleEngineManifest.xml.', [
      Project.AndroidMinSdkVersion,
      Project.AndroidTargetSdkVersion
    ]);
end;

end.
