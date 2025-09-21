{
    This file is part of the Free Component Library (FCL)
    Copyright (c) 1999-2000 by the Free Pascal development team

    Small OOP wrapper around OpenSSL unit.

    See the file COPYING.FPC, included in this distribution,
    for details about the copyright.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.

 **********************************************************************}
{$IFNDEF FPC_DOTTEDUNITS}
unit CastleInternalForFpc32xfpopenssl;
{$ENDIF FPC_DOTTEDUNITS}

{$mode objfpc}{$H+}
{.$DEFINE DUMPCERT}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses
  System.Classes, System.SysUtils, System.Net.Sslbase, Api.Openssl, System.CTypes;
{$ELSE FPC_DOTTEDUNITS}
uses
  Classes, SysUtils, sslbase, CastleInternalForFpc32xopenssl, ctypes;
{$ENDIF FPC_DOTTEDUNITS}

{$IFDEF DUMPCERT}
Const
  DumpCertFile = 'x509.txt';
{$ENDIF}

Type
  { TSSLContext }

  TSSLContext = Class;
  TRTlsExtCtx = record
    CTX: TSSLContext;
    domains: array of string; // SSL Certificate with one or more alternative names (SAN)
  end;
  TTlsExtCtx = array of TRTlsExtCtx;
  PTlsExtCtx = ^TTlsExtCtx;

  TSSLContext = Class(TObject)
  private
    FCTX: PSSL_CTX;
    function UsePrivateKey(pkey: SslPtr): cInt;
    function UsePrivateKeyASN1(pk: cInt; d: String; len: cLong): cInt;
    function UsePrivateKeyASN1(pk: cInt; d: TBytes; len: cLong): cInt;
    function UsePrivateKeyFile(const Afile: String; Atype: cInt): cInt;
  Public
    Constructor Create(AContext : PSSL_CTX = Nil); overload;
    Constructor Create(AType : TSSLType); overload;
    Destructor Destroy; override;
    Function SetCipherList(Var ACipherList : AnsiString) : Integer;
    procedure SetVerify(mode: Integer; arg2: TSSLCTXVerifyCallback);
    procedure SetDefaultPasswdCb(cb: PPasswdCb);
    procedure SetDefaultPasswdCbUserdata(u: SslPtr);
    Function UsePrivateKey(Data : TSSLData) : cint;
    // Use certificate.
    Function UseCertificate(Data : TSSLData) : cint;
    function UseCertificateASN1(len: cLong; d: String):cInt; overload; deprecated 'use TBytes overload';
    function UseCertificateASN1(len: cLong; buf: TBytes):cInt; overload;
    function UseCertificateFile(const Afile: String; Atype: cInt):cInt;
    function UseCertificateChainFile(const Afile: PAnsiChar):cInt;
    function UseCertificate(x: SslPtr):cInt;
    function LoadVerifyLocations(const CAfile: String; const CApath: String):cInt;
    function LoadPFX(Const S,APassword : AnsiString) : cint; deprecated 'use TBytes overload';
    function LoadPFX(Const Buf : TBytes;APassword : AnsiString) : cint;
    function LoadPFX(Data : TSSLData; Const APAssword : Ansistring) : cint;
    function SetOptions(AOptions: cLong): cLong;
    procedure SetTlsextServernameCallback(cb: PCallbackCb);
    procedure SetTlsextServernameArg(ATlsextcbp: SslPtr);
    procedure ActivateServerSNI(ATlsextcbp: TTlsExtCtx);
    procedure SetEcdhAuto(const onoff: boolean);
    Property CTX: PSSL_CTX Read FCTX;
  end;

  TSSL = Class(TObject)
  Public
    FSSL : PSSL;
  Public
    Constructor Create(ASSL : PSSL = Nil);
    Constructor Create(AContext : TSSLContext);
    destructor Destroy; override;
    function SetFd(fd: cInt):cInt;
    function Accept : cInt;
    function Connect : cInt;
    function Shutdown : cInt;
    function Read(buf: SslPtr; num: cInt):cInt;
    function Peek(buf: SslPtr; num: cInt):cInt;
    function Write(buf: SslPtr; num: cInt):cInt;
    Function PeerCertificate : PX509;
    function Ctrl(cmd: cInt; larg: clong; parg: Pointer): cInt;
    function Pending:cInt;
    Function GetError(AResult :cint) : cint;
    function GetCurrentCipher :SslPtr;
    function Version: String;
    function PeerName: string;
    function PeerNameHash: cardinal;
    function PeerSubject : String;
    Function PeerIssuer : String;
    Function PeerSerialNo : Integer;
    Function PeerFingerprint(const name: string = 'MD5') : String;
    Function CertInfo : String;
    function CipherName: string;
    function CipherBits: integer;
    function CipherAlgBits: integer;
    Function VerifyResult : Integer;
    function Set1Host(const hostname: string): Integer;
    Property SSL: PSSL Read FSSL;
  end;


  TOpenSSLX509Certificate = Class (TX509Certificate)
  Protected
    function CreateKey: PEVP_PKEY; virtual;
    procedure SetNameData(x: PX509); virtual;
    procedure SetTimes(x: PX509); virtual;
  Public
    Function CreateCertificateAndKey : TCertAndKey; override;
  end;

  ESSL = Class(Exception);

Function BioToString(B : PBIO; FreeBIO : Boolean = False) : AnsiString;

implementation

{$IFDEF FPC_DOTTEDUNITS}
uses System.DateUtils;
{$ELSE FPC_DOTTEDUNITS}
uses dateutils;
{$ENDIF FPC_DOTTEDUNITS}

Resourcestring
  SErrCountNotGetContext = 'Failed to create SSL Context';
  SErrFailedToCreateSSL = 'Failed to create SSL';


Function BioToString(B : PBIO; FreeBIO : Boolean = False) : AnsiString;

Var
  L,RL : Integer;
begin
  l:=bioctrlpending(B);
  Result:=StringOfChar(#0,l);
  RL:=BioRead(B,Result,L);
  if (RL>0) then
    SetLength(Result,RL)
  else
    SetLength(Result,0);
  if FreeBio then
    BioFreeAll(B);
end;

Function BioToTBytes(B : PBIO; FreeBIO : Boolean = False) : TBytes;

Var
  L,RL : Integer;
begin
  l:=bioctrlpending(B);
  SetLength(Result,l);
  FillChar(Result[0],L,0);
  RL:=BioRead(B,Result,L);
  if (RL>0) then
    SetLength(Result,RL)
  else
    SetLength(Result,0);
  if FreeBio then
    BioFreeAll(B);
end;

function SelectSNIContextCallback(ASSL: TSSL; ad: integer; arg: TTlsExtCtx): integer; cdecl;
var
  sHostName: string;
  o, i, f: integer;
begin
  sHostName := SSLGetServername(ASSL, TLSEXT_NAMETYPE_host_name);
  if (sHostName <> '') and (length(arg) > 0) then
  begin
    f := -1;
    for o:=0 to length(arg)-1 do
    begin
      for i:=0 to length(arg[o].domains)-1 do
        if sHostName = arg[o].domains[i] then
        begin
          f := o;
          break;
        end;
      if f <> -1 then break
    end;
    if f = -1 then
      result := SSL_TLSEXT_ERR_NOACK
    else if f > 1 then // first one should be the main certificate
      SslSetSslCtx(ASSL, arg[f].CTX);
  end;
  result := SSL_TLSEXT_ERR_OK;
end;

{ TOpenSSLX509Certificate }


procedure TOpenSSLX509Certificate.SetNameData(x: PX509);

Var
  ND : PX509_NAME;
  S : AnsiString;

  Procedure SetEntry(aCode,aValue : AnsiString);

  begin
    if (AValue<>'') then
      X509NameAddEntryByTxt(ND, aCode, $1001, aValue, -1, -1, 0);
  end;

begin
  ND:=X509GetSubjectName(x);
  S:=Country;
  if S='' then
    S:='BE';
  SetEntry('C',S);
  S:=HostName;
  if S='' then
    S:='localhost';
  SetEntry('CN',S);
  SetEntry('O',Organization);
  x509SetIssuerName(x,ND);
end;

Procedure TOpenSSLX509Certificate.SetTimes(x : PX509);

var
  Utc : PASN1_UTCTIME;

begin
  Utc:=Asn1UtctimeNew;
  try
    ASN1UtcTimeSetString(Utc,PAnsiChar(FormatDateTime('YYMMDDHHNNSS"Z"',ValidFrom)));
    X509SetNotBefore(x, Utc);
    ASN1UtcTimeSetString(Utc,PAnsiChar(FormatDateTime('YYMMDDHHNNSS"Z"',ValidTo)));
    X509SetNotAfter(x,Utc);
  finally
    Asn1UtctimeFree(Utc);
  end;
end;


function TOpenSSLX509Certificate.CreateKey : PEVP_PKEY;

Var
  rsa: PRSA;

begin
  Result:=EvpPkeynew;
  rsa:=RsaGenerateKey(KeySize,$10001,nil,nil);
  EvpPkeyAssign(Result,EVP_PKEY_RSA,rsa);
end;

function TOpenSSLX509Certificate.CreateCertificateAndKey: TCertAndKey;

var
  pk: PEVP_PKEY;
  x: PX509;
  b: PBIO;
{$IFDEF DUMPCERT}
  s : string;
{$ENDIF}

begin
  SetLength(Result.Certificate,0);
  SetLength(Result.PrivateKey,0);
  pk := nil;
  x := X509New;
  try
    X509SetVersion(x, Version);
    Asn1IntegerSet(X509getSerialNumber(x), GetRealSerial);
    SetTimes(X);
    pk:=CreateKey;
    X509SetPubkey(x, pk);
    SetNameData(x);
    x509Sign(x,pk,EvpGetDigestByName('SHA1'));
    // Certificate
    b := BioNew(BioSMem);
    i2dX509Bio(b, x);
    Result.Certificate:=BioToTbytes(B,True);
    // Private key
    b := BioNew(BioSMem);
    i2dPrivatekeyBio(b, pk);
    Result.PrivateKey:=BioToTbytes(B,True);
{$IFDEF DUMPCERT}
    b := BioNew(BioSMem);
    PEM_write_bio_X509(b,x);
    S:=BioToString(B,True);
    With TStringList.Create do
      try
        Add(S);
        SaveToFile(IncludeTrailingPathDelimiter(GetTempDir)+DumpCertFile);
      finally
        Free;
      end;
{$ENDIF}
  finally
    X509free(x);
    EvpPkeyFree(pk);
  end;
end;

{ TSSLContext }

Constructor TSSLContext.Create(AContext: PSSL_CTX);
begin
  FCTX:=AContext
end;

Constructor TSSLContext.Create(AType: TSSLType);

Var
  C : PSSL_CTX;

begin
  C := nil;
  Case AType of
    stAny:
      begin
        if Assigned(SslTLSMethod) then
          C := SslCtxNew(SslTLSMethod)
        else
          C := SslCtxNew(SslMethodV23);
      end;
    stSSLv2: C := SslCtxNew(SslMethodV2);
    stSSLv3: C := SslCtxNew(SslMethodV3);
    stTLSv1: C := SslCtxNew(SslMethodTLSV1);
    stTLSv1_1: C := SslCtxNew(SslMethodTLSV1_1);
    stTLSv1_2: C := SslCtxNew(SslMethodTLSV1_2);
  end;
  if (C=Nil) then
     Raise ESSL.Create(SErrCountNotGetContext);
  Create(C);
end;

Destructor TSSLContext.Destroy;
begin
  SslCtxFree(FCTX);
  inherited Destroy;
end;

Function TSSLContext.SetCipherList(Var ACipherList: AnsiString): Integer;

begin
  Result:=SSLCTxSetCipherList(FCTX,ACipherList);
end;

procedure TSSLContext.SetVerify(mode: Integer; arg2: TSSLCTXVerifyCallback);
begin
  SslCtxSetVerify(FCtx,Mode,arg2);
end;

procedure TSSLContext.SetDefaultPasswdCb(cb: PPasswdCb);
begin
  SslCtxSetDefaultPasswdCb(Fctx,cb)
end;

procedure TSSLContext.SetDefaultPasswdCbUserdata(u: SslPtr);
begin
  SslCtxSetDefaultPasswdCbUserdata(FCTX,u);
end;

function TSSLContext.UsePrivateKey(pkey: SslPtr):cInt;
begin
  Result:=SslCtxUsePrivateKey(FCTX,pkey);
end;

function TSSLContext.UsePrivateKeyASN1(pk: cInt; d: String; len: cLong):cInt;
begin
  Result:=SslCtxUsePrivateKeyASN1(pk,FCtx,d,len);
end;

function TSSLContext.UsePrivateKeyASN1(pk: cInt; d: TBytes; len: cLong): cInt;
begin
  Result:=SslCtxUsePrivateKeyASN1(pk,FCtx,d,len);
end;

function TSSLContext.UsePrivateKeyFile(const Afile: String; Atype: cInt):cInt;
begin
  Result:=SslCtxUsePrivateKeyFile(FCTX,AFile,AType);
end;


Function TSSLContext.UsePrivateKey(Data: TSSLData): cint;

Var
  FN : String;
  l : integer;

begin
  Result:=-1;
  L:=Length(Data.Value);
  If (l<>0) then
    Result:=UsePrivateKeyASN1(EVP_PKEY_RSA,Data.Value,L)
  else if (Data.FileName<>'') then
    begin
    FN:=Data.FileName;
    Result:=UsePrivateKeyFile(FN,SSL_FILETYPE_PEM);
    if (Result<>1) then
      Result:=UsePrivateKeyFile(FN,SSL_FILETYPE_ASN1);
    end;
end;

Function TSSLContext.UseCertificate(Data: TSSLData): cint;

Var
  l : integer;
  FN : String;

begin
  Result:=-1;
  L:=Length(Data.Value);
  if (L<>0) then
    Result:=UseCertificateASN1(length(Data.Value),Data.Value)
  else if (Data.FileName<>'') then
    begin
    FN:=Data.FileName;
    Result:=UseCertificateChainFile(PAnsiChar(FN));
    if Result<>1 then
       begin
       Result:=UseCertificateFile(FN,SSL_FILETYPE_PEM);
       if (Result<>1) then
         Result:=UseCertificateFile(FN,SSL_FILETYPE_ASN1);
       end;
    end
end;

function TSSLContext.UseCertificateASN1(len: cLong; d: String): cInt;
begin
  Result:=sslctxUseCertificateASN1(FCTX,len,d);
end;

function TSSLContext.UseCertificateASN1(len: cLong; buf: TBytes): cInt;
begin
  Result:=sslctxUseCertificateASN1(FCTX,len,Buf);
end;

function TSSLContext.UseCertificateFile(const Afile: String; Atype: cInt): cInt;
begin
  Result:=sslctxUseCertificateFile(FCTX,Afile,Atype);
end;

function TSSLContext.UseCertificateChainFile(const Afile: PAnsiChar): cInt;
begin
  Result:=sslctxUseCertificateChainFile(FCTX,Afile);
end;

function TSSLContext.UseCertificate(x: SslPtr): cInt;
begin
  Result:=SSLCTXusecertificate(FCTX,X);
end;

function TSSLContext.LoadVerifyLocations(const CAfile: String; const CApath: String): cInt;
begin
  Result:=SslCtxLoadVerifyLocations(FCTX,CAfile,CApath);
end;

function TSSLContext.LoadPFX(Const S, APassword: AnsiString): cint;

var
  Buf : TBytes;

begin
  SetLength(Buf,Length(S));
  Move(S[1],Buf[0],Length(S));
  Result:=LoadPFX(Buf,APAssword);
end;

function TSSLContext.LoadPFX(const Buf: TBytes; APassword: AnsiString): cint;

var
  b: PBIO;
  p12,c,pk,ca: SslPtr;

begin
  Result:=-1;
  c:=nil;
  pk:=nil;
  ca:=nil;
  p12:=Nil;
  b:=BioNew(BioSMem);
  try
    BioWrite(b,Buf,Length(Buf));
    p12:=d2iPKCS12bio(b,nil);
    if Assigned(p12) then
      if PKCS12parse(p12,APassword,pk,c,ca)>0 then
        begin
        Result:=UseCertificate(c);
        if (Result>0) then
          Result:=UsePrivateKey(pk);
        end;
  finally
    if pk<>Nil then
      EvpPkeyFree(pk);
    if c<>nil then
      X509free(c);
//  SkX509PopFree(ca,_X509Free);
    if p12<>Nil then
      PKCS12free(p12);
    BioFreeAll(b);
  end;
end;

function TSSLContext.LoadPFX(Data: TSSLData; Const APAssword : Ansistring): cint;

Var
  B : TBytes;

begin
  Result:=-1;
  try
    if (Length(Data.Value)<>0) then
      B:=Data.Value
    else
      With TFileStream.Create(Data.FileName,fmOpenRead or fmShareDenyNone) do
        Try
          SetLength(B,Size);
          ReadBuffer(B[0],Size);
        finally
          Free;
        end;
    Result:=LoadPFX(B,APassword);
  except
    // Silently ignore
    Exit;
  end;
end;

function TSSLContext.SetOptions(AOptions: cLong): cLong;
begin
  result := SslCtxCtrl(FCTX, SSL_CTRL_OPTIONS, AOptions, nil);
end;

procedure TSSLContext.SetTlsextServernameCallback(cb: PCallbackCb);
begin
  SslCtxCallbackCtrl(FCTX, SSL_CTRL_SET_TLSEXT_SERVERNAME_CB, cb);
end;

procedure TSSLContext.SetTlsextServernameArg(ATlsextcbp: SslPtr);
begin
  SslCtxCtrl(FCTX, SSL_CTRL_SET_TLSEXT_SERVERNAME_ARG, 0, ATlsextcbp);
end;

procedure TSSLContext.ActivateServerSNI(ATlsextcbp: TTlsExtCtx);
begin
  SetTlsextServernameCallback(@SelectSNIContextCallback);
  SetTlsextServernameArg(Pointer(ATlsextcbp));
end;

procedure TSSLContext.SetEcdhAuto(const onoff: boolean);
var larg: clong;
begin
  if onoff then
    larg := 1
  else
    larg := 0;
  SslCtxCtrl(FCTX, SSL_CTRL_SET_ECDH_AUTO, larg, nil);
end;

{ TSSL }

Constructor TSSL.Create(ASSL: PSSL);
begin
  FSSL:=ASSL;
end;

Constructor TSSL.Create(AContext: TSSLContext);
begin
  FSSL:=Nil;
  if Assigned(AContext) and Assigned(AContext.CTX) then
    FSSL:=sslNew(AContext.CTX);
  If (FSSL=Nil) then
    Raise ESSL.Create(SErrFailedToCreateSSL)
end;

destructor TSSL.Destroy;
begin
  sslfree(FSSL);
  inherited Destroy;
end;

function TSSL.Ctrl(cmd: cInt; larg: clong; parg: Pointer): cInt;

begin
  Result:=sslCtrl(fSSL,cmd,larg,parg);
end;

function TSSL.SetFd(fd: cInt): cInt;
begin
  Result:=sslSetFD(fSSL,fd);
end;

function TSSL.Accept: cInt;
begin
  Result:=sslAccept(fSSL);
end;

function TSSL.Connect: cInt;
begin
  Result:=sslConnect(fSSL);
end;

function TSSL.Shutdown: cInt;
begin
  try
    Result:=sslShutDown(fSSL);
  except
    // Sometimes, SSL gives an error when the connection is lost
  end;
end;

function TSSL.Read(buf: SslPtr; num: cInt): cInt;
begin
  Result:=sslRead(FSSL,buf,num);
end;

function TSSL.Peek(buf: SslPtr; num: cInt): cInt;
begin
  Result:=sslPeek(FSSL,buf,num);
end;

function TSSL.Write(buf: SslPtr; num: cInt): cInt;
begin
  Result:=sslWrite(FSSL,buf,num);
end;

Function TSSL.PeerCertificate: PX509;
begin
  Result:=sslGetPeercertificate(FSSL);
end;

function TSSL.Pending: cInt;
begin
  Result:=sslPending(FSSL);
end;

Function TSSL.GetError(AResult: cint): cint;
begin
  Result:=SslGetError(FSsl,AResult);
end;

function TSSL.GetCurrentCipher: SslPtr;
begin
  Result:=SSLGetCurrentCipher(FSSL);
end;

function TSSL.Version: String;
begin
  Result:=SSlGetVersion(FSsl);
end;

function TSSL.PeerName: string;
var
  s : ansistring;
  p : Integer;
begin
  Result:='';
  S:=PeerSubject;
  P:=Pos('/CN=', S);
  if (P>0) then
    begin
    Delete(S,1,P+3);
    P:=Pos('/',S);
    if (P>0) then
      Result:=Copy(S,1,P-1)
    else
      Result := S;
    end
end;

function TSSL.PeerNameHash: cardinal;
var
  C : PX509;
begin
  Result:=0;
  c:=PeerCertificate;
  if (C=Nil) then
    exit;
  try
    Result:=X509NameHash(X509GetSubjectName(C));
  finally
    X509Free(C);
  end;
end;

function TSSL.PeerSubject: String;
var
  c : PX509;
  s : ansistring;

begin
  Result:='';
  S:='';
  c:=PeerCertificate;
  if Assigned(c) then
    try
      setlength(s, 4096);
      Result:=X509NameOneline(X509GetSubjectName(c),s,Length(s));
    finally
      X509Free(c);
    end;
end;

Function TSSL.PeerIssuer: String;

var
  C: PX509;
  S: ansistring;

begin
  Result:='';
  C:=PeerCertificate;
  if (C=Nil) then
    Exit;
  try
    S:=StringOfChar(#0,4096);
    Result:=X509NameOneline(X509GetIssuerName(C),S,4096);
  finally
    X509Free(C);
  end;
end;

Function TSSL.PeerSerialNo: Integer;
var
  C : PX509;
  SN : PASN1_INTEGER;

begin
  Result:=-1;
  C:=PeerCertificate;
  if (C=Nil) then
    exit;
  try
    SN:=X509GetSerialNumber(C);
    Result:=Asn1IntegerGet(SN);
  finally
    X509Free(C);
  end;
end;

Function TSSL.PeerFingerprint(const name: string): String;
var
  C : PX509;
  L : integer;

begin
  Result:='';
  C:=PeerCertificate;
  if (C=Nil) then
    Exit;
  try
    Result:=StringOfChar(#0,EVP_MAX_MD_SIZE);
    L:=0;
    X509Digest(C,EvpGetDigestByName(name),Result,L);
    SetLength(Result,L);
  finally
    X509Free(C);
  end;
end;

Function TSSL.CertInfo: String;
var
  C : PX509;
  B : PBIO;

begin
  Result:='';
  C:=PeerCertificate;
  if (C=Nil)  then
    Exit;
  try
    B:=BioNew(BioSMem);
    try
      X509Print(B,C);
      Result:=BioToString(B);
    finally
      BioFreeAll(B);
    end;
  finally
    X509Free(C);
  end;
end;

function TSSL.CipherName: string;
begin
  Result:=SslCipherGetName(GetCurrentCipher);
end;

function TSSL.CipherBits: integer;

var
  x: integer;

begin
  x:=0;
  Result:=SSLCipherGetBits(GetCurrentCipher,x);
end;

function TSSL.CipherAlgBits: integer;

begin
  Result:=0;
  SSLCipherGetBits(GetCurrentCipher,Result);
end;

Function TSSL.VerifyResult: Integer;

begin
  Result:=SslGetVerifyResult(FSsl);
end;

function TSSL.Set1Host(const hostname: string): Integer;
begin
  Result := SslSet1Host(FSsl, hostname);
end;

end.

