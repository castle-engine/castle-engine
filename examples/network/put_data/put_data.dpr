program put_data;

{ Send PUT request with JSON body.
  Note: to send non-trivial JSON, better create it with proper JSON library
  like FpJson. }

{$apptype console}

uses
  {$ifdef UNIX} CThreads, {$endif} // necessary to have asynchronous downloading on Unix
  SysUtils,
  {$ifdef FPC} OpenSSLSockets, {$endif} // support HTTPS
  CastleUtils, CastleDownload, CastleClassUtils;

var
  D: TCastleDownload;
begin
  D := TCastleDownload.Create(nil);
  try
    D.Url := 'https://castle-engine.io/miscella/test_put.php';
    D.HttpMethod := hmPut;
    WriteStr(D.HttpRequestBody, '{ "test": "value" }');
    D.Start;
    D.WaitForFinish;
    case D.Status of
      dsError: Writeln('Error: ', D.ErrorMessage);
      dsSuccess: Writeln('Success');
      else raise EInternalError.Create('Unexpected status');
    end;
    Writeln('Response code: ', D.HttpResponseCode);
    Writeln('Response body: ', ReadGrowingStreamToDefaultString(D.Contents));
  finally FreeAndNil(D) end;
end.
