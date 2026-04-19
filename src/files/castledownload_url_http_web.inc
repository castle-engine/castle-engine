{%MainUnit castledownload.pas}
{
  Copyright 2026-2026 Michalis Kamburelis.

  This file is part of "Castle Game Engine".

  "Castle Game Engine" is free software; see the file COPYING.txt,
  included in this distribution, for details about the copyright.

  "Castle Game Engine" is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

  ----------------------------------------------------------------------------
}

{ Handle http, https URLs using XMLHttpRequest. }

{$ifdef WASI}

// Hide: Warning: Implicit string type conversion from "AnsiString" to "UnicodeString"
// (looks like it can occur with 3 different codes???)
{$warn 4101 off}
{$warn 4104 off}
{$warn 4105 off}

{$ifdef read_implementation_uses}
Job.Js, CastleInternalJobWeb,
{$endif read_implementation_uses}

{$ifdef read_implementation}

{ THttpWebReader ------------------------------------------------------------- }

type
  { TUrlAsynchronousReader descendant that implements http(s) downloading
    on web using XMLHttpRequest. }
  THttpWebReader = class(TUrlAsynchronousReader)
  strict private
    Xhr: IJSXMLHttpRequest;
    function HandleLoad(Event: IJSEvent): Variant;
    function HandleError(Event: IJSEvent): Variant;
    function HandleProgress(Event: IJSEvent): Variant;
  public
    procedure Start; override;
    destructor Destroy; override;
  end;

procedure THttpWebReader.Start;
const
  HttpMethodNames: array[THttpMethod] of String = (
    'GET', 'POST', 'PUT', 'DELETE', 'OPTIONS', 'HEAD'
  );

  { Encode HttpPostData as application/x-www-form-urlencoded string. }
  function EncodeFormData: String;
  var
    I: Integer;
    Key, Value: String;
    EqPos: Integer;
  begin
    Result := '';
    for I := 0 to HttpPostData.Count - 1 do
    begin
      if I > 0 then
        Result := Result + '&';
      EqPos := Pos('=', HttpPostData[I]);
      if EqPos > 0 then
      begin
        Key := Copy(HttpPostData[I], 1, EqPos - 1);
        Value := SEnding(HttpPostData[I], EqPos + 1);
      end else
      begin
        Key := HttpPostData[I];
        Value := '';
      end;
      Result := Result +
        InternalUrlEncodeForm(Key) + '=' + InternalUrlEncodeForm(Value);
    end;
  end;

  function UseHttpRequestBody: Boolean;
  begin
    Result := (HttpRequestBody <> nil) and (HttpRequestBody.Size > 0);
  end;

var
  I: Integer;
  RequestBodyBuffer: IJSArrayBuffer;
begin
  inherited;
  if not CheckNetworkAccess then
    Exit;

  CheckFileAccessSafe(Url);
  WritelnLog('Network', 'Download using XMLHttpRequest started for "%s"', [
    UriDisplay(Url)
  ]);

  MimeType := UriMimeType(Url);
  FinalUrl := Url;

  Xhr := TJSXMLHttpRequest.Create;
  Xhr.ResponseType := 'arraybuffer';

  { Register event listeners before calling open/send }
  Xhr.OnLoad := @HandleLoad;
  Xhr.OnError := @HandleError;
  Xhr.OnProgress := @HandleProgress;

  { Open the request }
  Xhr.Open(HttpMethodNames[HttpMethod], Url);

  { Set custom HTTP headers }
  if HttpHeadersKeys <> nil then
  begin
    Assert((HttpHeadersKeys <> nil) = (HttpHeadersValues <> nil));
    Assert(HttpHeadersKeys.Count = HttpHeadersValues.Count);
    for I := 0 to HttpHeadersKeys.Count - 1 do
      Xhr.SetRequestHeader(HttpHeadersKeys[I], HttpHeadersValues[I]);
  end;

  { Send the request with appropriate body }
  if UseHttpRequestBody then
  begin
    RequestBodyBuffer := TJSArrayBuffer.Create(HttpRequestBody.Size);
    RequestBodyBuffer.CopyFromMemory(HttpRequestBody.Memory, HttpRequestBody.Size);
    Xhr.Send(RequestBodyBuffer);
  end else
  if (HttpMethod = hmPost) and (HttpPostData <> nil) and (HttpPostData.Count > 0) then
  begin
    Xhr.SetRequestHeader('Content-Type', 'application/x-www-form-urlencoded');
    Xhr.Send(EncodeFormData);
  end else
    Xhr.Send;

  Contents := TMemoryStream.Create;

  HttpResponseHeaders := TStringList.Create;
  HttpResponseHeaders.NameValueSeparator := ':';
end;

function THttpWebReader.HandleLoad(Event: IJSEvent): Variant;
var
  ResponseBuffer: IJSArrayBuffer;
begin
  Result := true;

  { Read HTTP status code }
  HttpResponseCode := Xhr.Status;

  { Parse response headers }
  HttpResponseHeaders.Text := Xhr.GetAllResponseHeaders;

  MimeType := ContentTypeToMimeType(HttpResponseHeaders.Values['Content-Type']);

  { Read response body to Contents stream }
  if Xhr.ResponseType = 'arraybuffer' then
  begin
    ResponseBuffer := TJSArrayBuffer.Cast(Xhr.Response);
    if ResponseBuffer <> nil then
    begin
      TMemoryStream(Contents).Size := ResponseBuffer.ByteLength;
      if ResponseBuffer.ByteLength > 0 then
        ResponseBuffer.CopyToMemory(TMemoryStream(Contents).Memory, TMemoryStream(Contents).Size);
      Contents.Position := 0;
      WritelnLog('Network', 'Response is arraybuffer, size %d bytes', [Contents.Size]);
    end;
  end else
  begin
    Status := dsError;
    ErrorMessage := Format('Unexpected response type "%s"', [
      Xhr.ResponseType
    ]);
    WritelnLog('Network', ErrorMessage);
    Exit;
  end;

  DownloadedBytes := Contents.Size;
  TotalBytes := Contents.Size;
  FinalUrl := Xhr.ResponseURL;

  if (HttpResponseCode >= 200) and (HttpResponseCode < 400) then
  begin
    Status := dsSuccess;
    WritelnLog('Network', 'Successfully downloaded "%s", MIME type "%s", HTTP response code %d', [
      UriDisplay(Url),
      MimeType,
      HttpResponseCode
    ]);
  end else
  begin
    Status := dsError;
    ErrorMessage := Format('HTTP request failed for "%s" with status %d', [
      UriDisplay(Url),
      HttpResponseCode
    ]);
    WritelnLog('Network', ErrorMessage);
  end;
end;

function THttpWebReader.HandleError(Event: IJSEvent): Variant;
begin
  Result := true;
  Status := dsError;
  ErrorMessage := Format('Network error when downloading "%s"', [
    UriDisplay(Url)
  ]);
  WritelnLog('Network', ErrorMessage);
end;

function THttpWebReader.HandleProgress(Event: IJSEvent): Variant;
var
  ProgressEvent: IJSProgressEvent;
begin
  Result := true;
  ProgressEvent := TJSProgressEvent.Cast(Event);
  { Note that we Round the Loaded / Total values. In normal usage, they are
    bytes, so should be integers, see
    https://developer.mozilla.org/en-US/docs/Web/API/ProgressEvent .
    They can have fractions only in custom usage (which we don't do)
    if something sets custom progress. }
  DownloadedBytes := Round(ProgressEvent.Loaded);
  if ProgressEvent.LengthComputable then
    TotalBytes := Round(ProgressEvent.Total);
end;

destructor THttpWebReader.Destroy;
begin
  if (Xhr <> nil) and (Status = dsDownloading) then
    Xhr.Abort;
  inherited;
end;

{$endif read_implementation}

{$endif WASI}
