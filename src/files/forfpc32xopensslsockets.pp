{$IFNDEF FPC_DOTTEDUNITS}
unit ForFpc32xOpenSslSockets;
{$ENDIF FPC_DOTTEDUNITS}

{$mode objfpc}{$H+}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.Classes, System.SysUtils, System.Net.Sockets, System.Net.Ssockets, System.Net.Sslsockets, System.Net.Sslbase, Api.Openssl, System.Net.Fpopenssl;
{$ELSE FPC_DOTTEDUNITS}
uses
  Classes, SysUtils, sockets, ssockets, sslsockets, sslbase, ForFpc32xopenssl, ForFpc32xfpopenssl;
{$ENDIF FPC_DOTTEDUNITS}

Type

  { TOpenSSLSocketHandler }

  TOpenSSLSocketHandler = Class(TSSLSocketHandler)
  Private
    FSSL: TSSL;
    FCTX : TSSLContext;
    FSSLLastErrorString: string;
    FSSLLastError : Integer;
  Protected
    procedure SetSSLLastErrorString(AValue: string);
    Function FetchErrorInfo: Boolean;
    function CheckSSL(SSLResult: Integer): Boolean;
    function CheckSSL(SSLResult: Pointer): Boolean;
    function CreateSSLContext(AType: TSSLType): TSSLContext; virtual;
    function InitContext(NeedCertificate: Boolean): Boolean; virtual;
    function DoneContext: Boolean; virtual;
    function InitSslKeys: boolean;virtual;
    Function GetLastSSLErrorString : String; // CGE modification to adjust to FPC32: override;
    Function GetLastSSLErrorCode : Integer; // CGE modification to adjust to FPC32: override;
  Public
    Constructor create; override;
    destructor destroy; override;
    function CreateCertGenerator: TX509Certificate; override;
    function Connect : Boolean; override;
    function Close : Boolean; override;
    function Accept : Boolean; override;
    function Shutdown(BiDirectional : Boolean): boolean; override;
    function Send(Const Buffer; Count: Integer): Integer; override;
    function Recv(Const Buffer; Count: Integer): Integer; override;
    function BytesAvailable: Integer; override;
    // Result of last CheckSSL call.
    Function SSLLastError: integer;
    property SSLLastErrorString: string read FSSLLastErrorString write SetSSLLastErrorString;
    property SSL: TSSL read FSSL; // allow more lower level info and control
  end;

implementation

{ TSocketHandler }
Resourcestring
  SErrNoLibraryInit = 'Could not initialize OpenSSL library';
  SErrCouldNotCreateSelfSignedCertificate = 'Failed to create self-signed certificate';
  SErrCouldNotInitSSLKeys = 'Failed to initialize SSL keys';

Procedure MaybeInitSSLInterface;

begin
  if not IsSSLloaded then
    if not InitSSLInterface then
      Raise EInOutError.Create(SErrNoLibraryInit);
end;

function TopenSSLSocketHandler.CreateCertGenerator: TX509Certificate;
begin
  Result:=TOpenSSLX509Certificate.Create;
end;

function TOpenSSLSocketHandler.CreateSSLContext(AType: TSSLType): TSSLContext;
begin
  Result := TSSLContext.Create(AType);
end;

procedure TOpenSSLSocketHandler.SetSSLLastErrorString(AValue: string);
begin
  if FSSLLastErrorString=AValue then Exit;
  FSSLLastErrorString:=AValue;
end;

function TOpenSSLSocketHandler.Connect: Boolean;
begin
  Result:=Inherited Connect;
  Result := Result and InitContext(False);
  if Result then
    begin
    Result:=CheckSSL(FSSL.SetFD(Socket.Handle));
    if Result then
     begin
     if SendHostAsSNI  and (Socket is TInetSocket) then
       FSSL.Ctrl(SSL_CTRL_SET_TLSEXT_HOSTNAME,TLSEXT_NAMETYPE_host_name,PAnsiChar(AnsiString((Socket as TInetSocket).Host)));
     if VerifyPeerCert and (Socket is TInetSocket) then
       FSSL.Set1Host((Socket as TInetSocket).Host);
     Result:=CheckSSL(FSSL.Connect);
     //if Result and VerifyPeerCert then
     //  Result:=(FSSL.VerifyResult<>0) or (not DoVerifyCert);
     if Result then
       Result:= DoVerifyCert;
     if Result then
       SetSSLActive(True);
     end;
    end;
end;

function TOpenSSLSocketHandler.Close: Boolean;
begin
  Result:=Shutdown(False);
end;

Function TOpenSSLSocketHandler.FetchErrorInfo : Boolean;

var
  S : AnsiString;

begin
  FSSLLastErrorString:='';
  FSSLLastError:=ErrGetError;
  ErrClearError;
  Result:=(FSSLLastError<>0);
  if Result then
    begin
    S:=StringOfChar(#0,256);
    ErrErrorString(FSSLLastError,S,256);
    FSSLLastErrorString:=s;
    end;
end;

function TOpenSSLSocketHandler.CheckSSL(SSLResult : Integer) : Boolean;

begin
  Result:=SSLResult>=1;
  if Not Result then
     begin
     FSSLLastError:=SSLResult;
     FetchErrorInfo;
     end;
end;

function TOpenSSLSocketHandler.CheckSSL(SSLResult: Pointer): Boolean;
begin
  Result:=(SSLResult<>Nil);
  if not Result then
    Result:=FetchErrorInfo;
end;

function TOpenSSLSocketHandler.DoneContext: Boolean;

begin
  FreeAndNil(FSSL);
  FreeAndNil(FCTX);
  ErrRemoveState(0);
  SetSSLActive(False);
  Result:=True;
end;

Function HandleSSLPwd(buf : PAnsiChar; len:Integer; flags:Integer; UD : Pointer):Integer; cdecl;

var
  Pwd: AnsiString;
  H :  TOpenSSLSocketHandler;

begin
  if Not Assigned(UD) then
    PWD:=''
  else
    begin
    H:=TOpenSSLSocketHandler(UD);
    Pwd:=H.CertificateData.KeyPassword;
    end;
  if (len<Length(Pwd)+1) then
    SetLength(Pwd,len-1);
  pwd:=pwd+#0;
  Result:=Length(Pwd);
  Move(Pointer(Pwd)^,Buf^,Result);
end;

function TOpenSSLSocketHandler.InitSslKeys: boolean;

begin
  Result:=(FCTX<>Nil);
  if not Result then
    Exit;
  if not CertificateData.Certificate.Empty then
    Result:=CheckSSL(FCTX.UseCertificate(CertificateData.Certificate));
  if Result and not CertificateData.PrivateKey.Empty then
    Result:=CheckSSL(FCTX.UsePrivateKey(CertificateData.PrivateKey));
  // CGE modification to adjust to FPC32:
  // if Result and ((CertificateData.CertCA.FileName<>'') or (CertificateData.TrustedCertsDir<>'')) then
  //   Result:=CheckSSL(FCTX.LoadVerifyLocations(CertificateData.CertCA.FileName,CertificateData.TrustedCertsDir));
  if Result and not CertificateData.PFX.Empty then
    Result:=CheckSSL(FCTX.LoadPFX(CertificateData.PFX,CertificateData.KeyPassword));
end;

function TOpenSSLSocketHandler.GetLastSSLErrorString: String;
begin
  Result:=FSSLLastErrorString;
end;

function TOpenSSLSocketHandler.GetLastSSLErrorCode: Integer;
begin
  Result:=FSSLLastError;
end;

constructor TOpenSSLSocketHandler.create;
begin
  inherited create;
  MaybeInitSSLInterface;
end;

destructor TOpenSSLSocketHandler.destroy;
begin
  FreeAndNil(FCTX);
  FreeAndNil(FSSL);
  inherited destroy;
end;

function TOpenSSLSocketHandler.InitContext(NeedCertificate:Boolean): Boolean;

Const
  VO : Array[Boolean] of Integer = (SSL_VERIFY_NONE,SSL_VERIFY_PEER);

var
  s: AnsiString;

begin
  Result:=DoneContext;
  if Not Result then
    Exit;
  try
    FCTX:=CreateSSLContext(SSLType);
  Except
    CheckSSL(Nil);
    raise;
  end;
  S:=CertificateData.CipherList;
  FCTX.SetCipherList(S);
  FCTX.SetVerify(VO[VerifypeerCert],Nil);
  FCTX.SetDefaultPasswdCb(@HandleSSLPwd);
  FCTX.SetDefaultPasswdCbUserdata(self);
  If NeedCertificate and CertificateData.NeedCertificateData  then
    if Not CreateSelfSignedCertificate then
      begin
      DoneContext;
      raise ESSL.Create(SErrCouldNotCreateSelfSignedCertificate);
      end;
   if Not InitSSLKeys then
     begin
     DoneContext;
     raise ESSL.Create(SErrCouldNotInitSSLKeys);
     end;
   try
     FSSL:=TSSL.Create(FCTX);
     Result:=True;
   Except
     CheckSSL(Nil);
     DoneContext;
     raise;
   end;
end;

function TOpenSSLSocketHandler.Accept: Boolean;

begin
  Result:=InitContext(True);
  if Result then
    begin
    Result:=CheckSSL(FSSL.setfd(Socket.Handle));
    if Result then
      Result:=CheckSSL(FSSL.Accept);
    end;
  SetSSLActive(Result);
end;


function TOpenSSLSocketHandler.Shutdown(BiDirectional : Boolean): boolean;

var
  r : integer;

begin
  Result:=assigned(FSsl);
  if Result then
    If Not BiDirectional then
      Result:=CheckSSL(FSSL.Shutdown)
    else
      begin
      r:=FSSL.Shutdown;
      if r<>0 then
        Result:=CheckSSL(r)
      else
        begin
        Result:=fpShutdown(Socket.Handle,1)=0;
        if Result then
          Result:=CheckSSL(FSsl.Shutdown);
        end
      end;
  If Result then
    Result:=DoneContext;
end;

function TOpenSSLSocketHandler.Send(Const Buffer; Count: Integer): Integer;
var
  e: integer;
begin
  FLastError:=0;
  FSSLLastError := 0;
  FSSLLastErrorString:='';
  repeat
    Result:=FSsl.Write(@Buffer,Count);
    e:=FSsl.GetError(Result);
  until Not (e in [SSL_ERROR_WANT_READ,SSL_ERROR_WANT_WRITE]);
  if (E=SSL_ERROR_ZERO_RETURN) then
    Result:=0
  else if (e<>0) then
    begin
    FSSLLastError:=e;
    if e=SSL_ERROR_SYSCALL then
      FLastError:=socketerror;
    end;
end;

function TOpenSSLSocketHandler.Recv(Const Buffer; Count: Integer): Integer;

var
  e: integer;
begin
  FLastError:=0;
  FSSLLastError:=0;
  FSSLLastErrorString:= '';
  repeat
    Result:=FSSL.Read(@Buffer ,Count);
    e:=FSSL.GetError(Result);
    if (e=SSL_ERROR_WANT_READ) and (Socket.IOTimeout>0) then
      e:=SSL_ERROR_ZERO_RETURN;
  until Not (e in [SSL_ERROR_WANT_READ,SSL_ERROR_WANT_WRITE]);
  if (E=SSL_ERROR_ZERO_RETURN) then
    Result:=0
  else if (e<>0) then
    begin
    FSSLLastError:=e;
    if e=SSL_ERROR_SYSCALL then
      FLastError:=socketerror;
    end;
end;

function TOpenSSLSocketHandler.BytesAvailable: Integer;
begin
  Result:= FSSL.Pending;
end;

Function TOpenSSLSocketHandler.SSLLastError: integer;
begin
  Result:=FSSLLastError;
end;

initialization
  TSSLSocketHandler.SetDefaultHandlerClass(TOpenSSLSocketHandler);
end.

