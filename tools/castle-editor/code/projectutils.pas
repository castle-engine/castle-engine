unit ProjectUtils;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

procedure ProjectOpen(const ManifestUrl: string);

implementation

uses CastleURIUtils;

procedure ProjectOpen(const ManifestUrl: string);
begin
  // Validate
  if not URIFileExists(ManifestUrl) then
    raise Exception.CreateFmt('Cannot find CastleEngineManifest.xml at this location: "%s". Invalid project opened.',
      [ManifestUrl]);

  // TODO
end;

end.

