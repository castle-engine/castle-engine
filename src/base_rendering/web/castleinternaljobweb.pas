{$IFNDEF FPC_DOTTEDUNITS}
Unit CastleInternalJobWeb;
{$ENDIF FPC_DOTTEDUNITS}

{$MODE ObjFPC}
{$H+}

interface

{$IFDEF FPC_DOTTEDUNITS}
uses System.SysUtils, Wasm.Job.Js;
{$ELSE FPC_DOTTEDUNITS}
uses SysUtils, Job.JS;
{$ENDIF FPC_DOTTEDUNITS}

{
  Automatically generated file by TWebIDLToPasWasmJob on 2025-02-27 06:54:56
  
  Used command-line options: 
  --input=castleinternaljobweb.webidl
  --outputformat=wasmjob
  --typealiases=@alias_webidl_types_in_other_units.txt
  --globals=@global_webidl_vars.txt
  --unitname=CastleInternalJobWeb
  --output=../castleinternaljobweb.pas
  --optionsinheader
  
  Command-line options translated to: 
  
  Keyword prefix: 
  Keyword suffix: _
  Class prefix: TJS
  Class suffix: 
  Field prefix: F
  Getter prefix: _Get
  Setter prefix: _Set
  WebIDL version: v2
  Type aliases:
  Object=IJSObject
  Set=IJSSet
  Map=IJSMap
  Function=IJSFunction
  Date=IJSDate
  RegExp=IJSRegExp
  String=IJSString
  Array=IJSArray
  ArrayBuffer=IJSArrayBuffer
  TypedArray=IJSTypedArray
  BufferSource=IJSBufferSource
  DataView=IJSDataView
  JSON=IJSJSON
  Error=IJSError
  TextEncoder=IJSTextEncode
  TextDecoder=IJSTextDecoder
  
  Dictionary class parent: 
  Base Options: [AddOptionsToHeader,ExpandUnionTypeArgs,DictionaryAsClass]
}
Type
  // Forward class definitions
  IJSAnimationFrameProvider = interface;
  TJSAnimationFrameProvider = class;
  IJSCSSStyleDeclaration = interface;
  TJSCSSStyleDeclaration = class;
  IJSContentSecurityPolicy = interface;
  TJSContentSecurityPolicy = class;
  IJSPrincipal = interface;
  TJSPrincipal = class;
  IJSWindowProxy = interface;
  TJSWindowProxy = class;
  IJSnsISupports = interface;
  TJSnsISupports = class;
  IJSURI = interface;
  TJSURI = class;
  IJSnsIDocShell = interface;
  TJSnsIDocShell = class;
  IJSnsILoadGroup = interface;
  TJSnsILoadGroup = class;
  IJSnsIReferrerInfo = interface;
  TJSnsIReferrerInfo = class;
  IJSnsICookieJarSettings = interface;
  TJSnsICookieJarSettings = class;
  IJSnsIPermissionDelegateHandler = interface;
  TJSnsIPermissionDelegateHandler = class;
  IJSXULCommandDispatcher = interface;
  TJSXULCommandDispatcher = class;
  IJSDocument = interface;
  TJSDocument = class;
  IJSDOMRect = interface;
  TJSDOMRect = class;
  IJSDOMRectReadOnly = interface;
  TJSDOMRectReadOnly = class;
  IJSnsIScreen = interface;
  TJSnsIScreen = class;
  IJSElement = interface;
  TJSElement = class;
  IJSElementCSSInlineStyle = interface;
  TJSElementCSSInlineStyle = class;
  IJSGlobalEventHandlers = interface;
  TJSGlobalEventHandlers = class;
  IJSWindowEventHandlers = interface;
  TJSWindowEventHandlers = class;
  IJSOnErrorEventHandlerForNodes = interface;
  TJSOnErrorEventHandlerForNodes = class;
  IJSOnErrorEventHandlerForWindow = interface;
  TJSOnErrorEventHandlerForWindow = class;
  IJSEventTarget = interface;
  TJSEventTarget = class;
  IJSEvent = interface;
  TJSEvent = class;
  IJSHTMLCanvasElement = interface;
  TJSHTMLCanvasElement = class;
  IJSHTMLDivElement = interface;
  TJSHTMLDivElement = class;
  IJSHTMLElement = interface;
  TJSHTMLElement = class;
  IJSimgINotificationObserver = interface;
  TJSimgINotificationObserver = class;
  IJSimgIRequest = interface;
  TJSimgIRequest = class;
  IJSnsIStreamListener = interface;
  TJSnsIStreamListener = class;
  IJSHTMLImageElement = interface;
  TJSHTMLImageElement = class;
  IJSMozImageLoadingContent = interface;
  TJSMozImageLoadingContent = class;
  IJSHTMLMediaElement = interface;
  TJSHTMLMediaElement = class;
  IJSHTMLSpanElement = interface;
  TJSHTMLSpanElement = class;
  IJSHTMLVideoElement = interface;
  TJSHTMLVideoElement = class;
  IJSImageBitmap = interface;
  TJSImageBitmap = class;
  IJSImageData = interface;
  TJSImageData = class;
  IJSKeyboardEvent = interface;
  TJSKeyboardEvent = class;
  IJSMouseEvent = interface;
  TJSMouseEvent = class;
  IJSNode = interface;
  TJSNode = class;
  IJSNonElementParentNode = interface;
  TJSNonElementParentNode = class;
  IJSOffscreenCanvas = interface;
  TJSOffscreenCanvas = class;
  IJSPointerEvent = interface;
  TJSPointerEvent = class;
  IJSUIEvent = interface;
  TJSUIEvent = class;
  IJSVideoFrame = interface;
  TJSVideoFrame = class;
  IJSWebGLSampler = interface;
  TJSWebGLSampler = class;
  IJSWebGLSync = interface;
  TJSWebGLSync = class;
  IJSWebGLTransformFeedback = interface;
  TJSWebGLTransformFeedback = class;
  IJSWebGL2RenderingContext = interface;
  TJSWebGL2RenderingContext = class;
  IJSWebGL2RenderingContextBase = interface;
  TJSWebGL2RenderingContextBase = class;
  IJSEXT_color_buffer_float = interface;
  TJSEXT_color_buffer_float = class;
  IJSOVR_multiview2 = interface;
  TJSOVR_multiview2 = class;
  IJSWebGLContextEvent = interface;
  TJSWebGLContextEvent = class;
  IJSWebGLBuffer = interface;
  TJSWebGLBuffer = class;
  IJSWebGLFramebuffer = interface;
  TJSWebGLFramebuffer = class;
  IJSWebGLProgram = interface;
  TJSWebGLProgram = class;
  IJSWebGLRenderbuffer = interface;
  TJSWebGLRenderbuffer = class;
  IJSWebGLShader = interface;
  TJSWebGLShader = class;
  IJSWebGLTexture = interface;
  TJSWebGLTexture = class;
  IJSWebGLUniformLocation = interface;
  TJSWebGLUniformLocation = class;
  IJSWebGLVertexArrayObject = interface;
  TJSWebGLVertexArrayObject = class;
  IJSWebGLActiveInfo = interface;
  TJSWebGLActiveInfo = class;
  IJSWebGLShaderPrecisionFormat = interface;
  TJSWebGLShaderPrecisionFormat = class;
  IJSWebGLRenderingContextBase = interface;
  TJSWebGLRenderingContextBase = class;
  IJSWebGLRenderingContext = interface;
  TJSWebGLRenderingContext = class;
  IJSEXT_texture_compression_bptc = interface;
  TJSEXT_texture_compression_bptc = class;
  IJSEXT_texture_compression_rgtc = interface;
  TJSEXT_texture_compression_rgtc = class;
  IJSEXT_texture_norm16 = interface;
  TJSEXT_texture_norm16 = class;
  IJSWEBGL_compressed_texture_s3tc = interface;
  TJSWEBGL_compressed_texture_s3tc = class;
  IJSWEBGL_compressed_texture_s3tc_srgb = interface;
  TJSWEBGL_compressed_texture_s3tc_srgb = class;
  IJSWEBGL_compressed_texture_astc = interface;
  TJSWEBGL_compressed_texture_astc = class;
  IJSWEBGL_compressed_texture_etc = interface;
  TJSWEBGL_compressed_texture_etc = class;
  IJSWEBGL_compressed_texture_etc1 = interface;
  TJSWEBGL_compressed_texture_etc1 = class;
  IJSWEBGL_compressed_texture_pvrtc = interface;
  TJSWEBGL_compressed_texture_pvrtc = class;
  IJSWEBGL_debug_renderer_info = interface;
  TJSWEBGL_debug_renderer_info = class;
  IJSWEBGL_debug_shaders = interface;
  TJSWEBGL_debug_shaders = class;
  IJSWEBGL_depth_texture = interface;
  TJSWEBGL_depth_texture = class;
  IJSOES_element_index_uint = interface;
  TJSOES_element_index_uint = class;
  IJSEXT_frag_depth = interface;
  TJSEXT_frag_depth = class;
  IJSWEBGL_lose_context = interface;
  TJSWEBGL_lose_context = class;
  IJSEXT_texture_filter_anisotropic = interface;
  TJSEXT_texture_filter_anisotropic = class;
  IJSEXT_sRGB = interface;
  TJSEXT_sRGB = class;
  IJSOES_standard_derivatives = interface;
  TJSOES_standard_derivatives = class;
  IJSOES_texture_float = interface;
  TJSOES_texture_float = class;
  IJSWEBGL_draw_buffers = interface;
  TJSWEBGL_draw_buffers = class;
  IJSOES_texture_float_linear = interface;
  TJSOES_texture_float_linear = class;
  IJSEXT_shader_texture_lod = interface;
  TJSEXT_shader_texture_lod = class;
  IJSOES_texture_half_float = interface;
  TJSOES_texture_half_float = class;
  IJSOES_texture_half_float_linear = interface;
  TJSOES_texture_half_float_linear = class;
  IJSWEBGL_color_buffer_float = interface;
  TJSWEBGL_color_buffer_float = class;
  IJSEXT_color_buffer_half_float = interface;
  TJSEXT_color_buffer_half_float = class;
  IJSOES_vertex_array_object = interface;
  TJSOES_vertex_array_object = class;
  IJSANGLE_instanced_arrays = interface;
  TJSANGLE_instanced_arrays = class;
  IJSEXT_blend_minmax = interface;
  TJSEXT_blend_minmax = class;
  IJSWebGLQuery = interface;
  TJSWebGLQuery = class;
  IJSEXT_disjoint_timer_query = interface;
  TJSEXT_disjoint_timer_query = class;
  IJSMOZ_debug = interface;
  TJSMOZ_debug = class;
  IJSEXT_float_blend = interface;
  TJSEXT_float_blend = class;
  IJSOES_fbo_render_mipmap = interface;
  TJSOES_fbo_render_mipmap = class;
  IJSWEBGL_explicit_present = interface;
  TJSWEBGL_explicit_present = class;
  IJSOES_draw_buffers_indexed = interface;
  TJSOES_draw_buffers_indexed = class;
  IJSWEBGL_provoking_vertex = interface;
  TJSWEBGL_provoking_vertex = class;
  IJSEXT_depth_clamp = interface;
  TJSEXT_depth_clamp = class;
  IJSnsIBrowserDOMWindow = interface;
  TJSnsIBrowserDOMWindow = class;
  IJSXULControllers = interface;
  TJSXULControllers = class;
  IJSnsIDOMWindowUtils = interface;
  TJSnsIDOMWindowUtils = class;
  IJSnsIPrintSettings = interface;
  TJSnsIPrintSettings = class;
  IJSWindow = interface;
  TJSWindow = class;
  IJSDOMRectInit = interface;
  TJSDOMRectInit = class;
  IJSEventListenerOptions = interface;
  TJSEventListenerOptions = class;
  IJSAddEventListenerOptions = interface;
  TJSAddEventListenerOptions = class;
  IJSEventInit = interface;
  TJSEventInit = class;
  IJSKeyboardEventInit = interface;
  TJSKeyboardEventInit = class;
  IJSMouseEventInit = interface;
  TJSMouseEventInit = class;
  IJSImageEncodeOptions = interface;
  TJSImageEncodeOptions = class;
  IJSPointerEventInit = interface;
  TJSPointerEventInit = class;
  IJSUIEventInit = interface;
  TJSUIEventInit = class;
  IJSEventModifierInit = interface;
  TJSEventModifierInit = class;
  IJSVideoFrameInit = interface;
  TJSVideoFrameInit = class;
  IJSVideoFrameBufferInit = interface;
  TJSVideoFrameBufferInit = class;
  IJSVideoFrameCopyToOptions = interface;
  TJSVideoFrameCopyToOptions = class;
  IJSPlaneLayout = interface;
  TJSPlaneLayout = class;
  IJSWebGLContextEventInit = interface;
  TJSWebGLContextEventInit = class;
  IJSWebGLContextAttributes = interface;
  TJSWebGLContextAttributes = class;
  TVisibilityState = UnicodeString;
  TOffscreenRenderingContextId = UnicodeString;
  TAlphaOption = UnicodeString;
  TVideoPixelFormat = UnicodeString;
  TWebGLPowerPreference = UnicodeString;
  TPredefinedColorSpace = UnicodeString;
  // Union of Event, DOMString
  TOnErrorEventHandlerNonNull_event_Type = Variant;
  // Union of HTMLCanvasElement, OffscreenCanvas
  TCanvasSource = Variant;
  // Union of OffscreenCanvasRenderingContext2D, ImageBitmapRenderingContext, WebGLRenderingContext, WebGL2RenderingContext, GPUCanvasContext
  TOffscreenRenderingContext = Variant;
  TDOMTimeStamp = QWord;
  TEpochTimeStamp = QWord;
  TDOMHighResTimeStamp = Double;
  TGLint64 = Int64;
  TGLuint64 = QWord;
  // Union of Uint32Array, sequence
  TUint32List = Variant;
  TGLenum = Cardinal;
  TGLboolean = Boolean;
  TGLbitfield = Cardinal;
  TGLbyte = ShortInt;
  TGLshort = SmallInt;
  TGLint = LongInt;
  TGLsizei = LongInt;
  TGLintptr = Int64;
  TGLsizeiptr = Int64;
  TGLubyte = Byte;
  TGLushort = Word;
  TGLuint = Cardinal;
  TGLfloat = Single;
  TGLclampf = Single;
  TGLuint64EXT = QWord;
  // Union of Float32Array, sequence
  TFloat32List = Variant;
  // Union of Int32Array, sequence
  TInt32List = Variant;
  TFrameRequestCallback = procedure (time: TDOMHighResTimeStamp) of object;
  TEventHandlerNonNull = function (event: IJSEvent): Variant of object;
  TEventHandler = TEventHandlerNonNull;
  TOnBeforeUnloadEventHandlerNonNull = function (event: IJSEvent): UnicodeString of object;
  TOnBeforeUnloadEventHandler = TOnBeforeUnloadEventHandlerNonNull;
  TOnErrorEventHandlerNonNull = function (const event: TOnErrorEventHandlerNonNull_event_Type; const source: UTF8String; lineno: Cardinal; column: Cardinal; const error: Variant): Variant of object;
  TOnErrorEventHandler = TOnErrorEventHandlerNonNull;
  TEventListener = function (event: IJSEvent): Boolean of object;
  
  { --------------------------------------------------------------------
    TJSDOMRectInit
    --------------------------------------------------------------------}
  
  TJSDOMRectInitRec = record
    x: Double;
    y: Double;
    width: Double;
    height: Double;
  end;
  
  IJSDOMRectInit = interface(IJSObject)
    ['{EA9EDD00-0E42-3C49-BC7F-2EDD319D5E04}']
    function _Getx: Double; 
    function _Gety: Double; 
    function _Getwidth: Double; 
    function _Getheight: Double; 
    procedure _Setx(const aValue: Double);
    procedure _Sety(const aValue: Double);
    procedure _Setwidth(const aValue: Double);
    procedure _Setheight(const aValue: Double);
    property x: Double read _Getx write _Setx;
    property y: Double read _Gety write _Sety;
    property width: Double read _Getwidth write _Setwidth;
    property height: Double read _Getheight write _Setheight;
  end;
  
  TJSDOMRectInit = class(TJSObject,IJSDOMRectInit)
  Private
  Protected
    function _Getx: Double; 
    function _Gety: Double; 
    function _Getwidth: Double; 
    function _Getheight: Double; 
    procedure _Setx(const aValue: Double);
    procedure _Sety(const aValue: Double);
    procedure _Setwidth(const aValue: Double);
    procedure _Setheight(const aValue: Double);
  Public
    constructor create(const aDict : TJSDOMRectInitRec); overload;
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSDOMRectInit;
    property x: Double read _Getx write _Setx;
    property y: Double read _Gety write _Sety;
    property width: Double read _Getwidth write _Setwidth;
    property height: Double read _Getheight write _Setheight;
  end;
  
  { --------------------------------------------------------------------
    TJSEventListenerOptions
    --------------------------------------------------------------------}
  
  TJSEventListenerOptionsRec = record
    capture: Boolean;
    mozSystemGroup: Boolean;
  end;
  
  IJSEventListenerOptions = interface(IJSObject)
    ['{AD98F9CB-0E73-3B09-ACE8-0F9870F2B117}']
    function _Getcapture: Boolean; 
    function _GetmozSystemGroup: Boolean; 
    procedure _Setcapture(const aValue: Boolean);
    procedure _SetmozSystemGroup(const aValue: Boolean);
    property capture: Boolean read _Getcapture write _Setcapture;
    property mozSystemGroup: Boolean read _GetmozSystemGroup write _SetmozSystemGroup;
  end;
  
  TJSEventListenerOptions = class(TJSObject,IJSEventListenerOptions)
  Private
  Protected
    function _Getcapture: Boolean; 
    function _GetmozSystemGroup: Boolean; 
    procedure _Setcapture(const aValue: Boolean);
    procedure _SetmozSystemGroup(const aValue: Boolean);
  Public
    constructor create(const aDict : TJSEventListenerOptionsRec); overload;
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSEventListenerOptions;
    property capture: Boolean read _Getcapture write _Setcapture;
    property mozSystemGroup: Boolean read _GetmozSystemGroup write _SetmozSystemGroup;
  end;
  
  { --------------------------------------------------------------------
    TJSEventInit
    --------------------------------------------------------------------}
  
  TJSEventInitRec = record
    bubbles: Boolean;
    cancelable: Boolean;
    composed: Boolean;
  end;
  
  IJSEventInit = interface(IJSObject)
    ['{743F6C7A-0AEF-38EB-BA94-5F17EB1EF670}']
    function _Getbubbles: Boolean; 
    function _Getcancelable: Boolean; 
    function _Getcomposed: Boolean; 
    procedure _Setbubbles(const aValue: Boolean);
    procedure _Setcancelable(const aValue: Boolean);
    procedure _Setcomposed(const aValue: Boolean);
    property bubbles: Boolean read _Getbubbles write _Setbubbles;
    property cancelable: Boolean read _Getcancelable write _Setcancelable;
    property composed: Boolean read _Getcomposed write _Setcomposed;
  end;
  
  TJSEventInit = class(TJSObject,IJSEventInit)
  Private
  Protected
    function _Getbubbles: Boolean; 
    function _Getcancelable: Boolean; 
    function _Getcomposed: Boolean; 
    procedure _Setbubbles(const aValue: Boolean);
    procedure _Setcancelable(const aValue: Boolean);
    procedure _Setcomposed(const aValue: Boolean);
  Public
    constructor create(const aDict : TJSEventInitRec); overload;
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSEventInit;
    property bubbles: Boolean read _Getbubbles write _Setbubbles;
    property cancelable: Boolean read _Getcancelable write _Setcancelable;
    property composed: Boolean read _Getcomposed write _Setcomposed;
  end;
  
  { --------------------------------------------------------------------
    TJSImageEncodeOptions
    --------------------------------------------------------------------}
  
  TJSImageEncodeOptionsRec = record
    type_: UnicodeString;
    quality: Double;
  end;
  
  IJSImageEncodeOptions = interface(IJSObject)
    ['{45F1632C-B7D9-35EA-8791-64CAD79CE33D}']
    function _Gettype_: UnicodeString; 
    function _Getquality: Double; 
    procedure _Settype_(const aValue: UnicodeString);
    procedure _Setquality(const aValue: Double);
    property type_: UnicodeString read _Gettype_ write _Settype_;
    property quality: Double read _Getquality write _Setquality;
  end;
  
  TJSImageEncodeOptions = class(TJSObject,IJSImageEncodeOptions)
  Private
  Protected
    function _Gettype_: UnicodeString; 
    function _Getquality: Double; 
    procedure _Settype_(const aValue: UnicodeString);
    procedure _Setquality(const aValue: Double);
  Public
    constructor create(const aDict : TJSImageEncodeOptionsRec); overload;
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSImageEncodeOptions;
    property type_: UnicodeString read _Gettype_ write _Settype_;
    property quality: Double read _Getquality write _Setquality;
  end;
  
  { --------------------------------------------------------------------
    TJSVideoFrameInit
    --------------------------------------------------------------------}
  
  TJSVideoFrameInitRec = record
    duration: QWord;
    timestamp: Int64;
    alpha: TAlphaOption;
    displayWidth: Cardinal;
    displayHeight: Cardinal;
  end;
  
  IJSVideoFrameInit = interface(IJSObject)
    ['{2CB58967-CA42-3BBB-977A-BD5E644C7E51}']
    function _Getduration: QWord; 
    function _Gettimestamp: Int64; 
    function _Getalpha: TAlphaOption; 
    function _GetdisplayWidth: Cardinal; 
    function _GetdisplayHeight: Cardinal; 
    procedure _Setduration(const aValue: QWord);
    procedure _Settimestamp(const aValue: Int64);
    procedure _Setalpha(const aValue: TAlphaOption);
    procedure _SetdisplayWidth(const aValue: Cardinal);
    procedure _SetdisplayHeight(const aValue: Cardinal);
    property duration: QWord read _Getduration write _Setduration;
    property timestamp: Int64 read _Gettimestamp write _Settimestamp;
    property alpha: TAlphaOption read _Getalpha write _Setalpha;
    property displayWidth: Cardinal read _GetdisplayWidth write _SetdisplayWidth;
    property displayHeight: Cardinal read _GetdisplayHeight write _SetdisplayHeight;
  end;
  
  TJSVideoFrameInit = class(TJSObject,IJSVideoFrameInit)
  Private
  Protected
    function _Getduration: QWord; 
    function _Gettimestamp: Int64; 
    function _Getalpha: TAlphaOption; 
    function _GetdisplayWidth: Cardinal; 
    function _GetdisplayHeight: Cardinal; 
    procedure _Setduration(const aValue: QWord);
    procedure _Settimestamp(const aValue: Int64);
    procedure _Setalpha(const aValue: TAlphaOption);
    procedure _SetdisplayWidth(const aValue: Cardinal);
    procedure _SetdisplayHeight(const aValue: Cardinal);
  Public
    constructor create(const aDict : TJSVideoFrameInitRec); overload;
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSVideoFrameInit;
    property duration: QWord read _Getduration write _Setduration;
    property timestamp: Int64 read _Gettimestamp write _Settimestamp;
    property alpha: TAlphaOption read _Getalpha write _Setalpha;
    property displayWidth: Cardinal read _GetdisplayWidth write _SetdisplayWidth;
    property displayHeight: Cardinal read _GetdisplayHeight write _SetdisplayHeight;
  end;
  
  { --------------------------------------------------------------------
    TJSVideoFrameBufferInit
    --------------------------------------------------------------------}
  
  TJSPlaneLayoutDynArray = IJSArray; // array of PlaneLayout
  TJSVideoFrameBufferInitRec = record
    format: TVideoPixelFormat; // required
    codedWidth: Cardinal;
    codedHeight: Cardinal;
    timestamp: Int64;
    duration: QWord;
    layout: TJSPlaneLayoutDynArray;
    displayWidth: Cardinal;
    displayHeight: Cardinal;
  end;
  
  IJSVideoFrameBufferInit = interface(IJSObject)
    ['{8F6A7F42-C768-3312-91CD-57A879945EE2}']
    function _Getformat: TVideoPixelFormat; 
    function _GetcodedWidth: Cardinal; 
    function _GetcodedHeight: Cardinal; 
    function _Gettimestamp: Int64; 
    function _Getduration: QWord; 
    function _Getlayout: TJSPlaneLayoutDynArray; 
    function _GetdisplayWidth: Cardinal; 
    function _GetdisplayHeight: Cardinal; 
    procedure _Setformat(const aValue: TVideoPixelFormat);
    procedure _SetcodedWidth(const aValue: Cardinal);
    procedure _SetcodedHeight(const aValue: Cardinal);
    procedure _Settimestamp(const aValue: Int64);
    procedure _Setduration(const aValue: QWord);
    procedure _Setlayout(const aValue: TJSPlaneLayoutDynArray);
    procedure _SetdisplayWidth(const aValue: Cardinal);
    procedure _SetdisplayHeight(const aValue: Cardinal);
    property format: TVideoPixelFormat read _Getformat write _Setformat; // required
    property codedWidth: Cardinal read _GetcodedWidth write _SetcodedWidth;
    property codedHeight: Cardinal read _GetcodedHeight write _SetcodedHeight;
    property timestamp: Int64 read _Gettimestamp write _Settimestamp;
    property duration: QWord read _Getduration write _Setduration;
    property layout: TJSPlaneLayoutDynArray read _Getlayout write _Setlayout;
    property displayWidth: Cardinal read _GetdisplayWidth write _SetdisplayWidth;
    property displayHeight: Cardinal read _GetdisplayHeight write _SetdisplayHeight;
  end;
  
  TJSVideoFrameBufferInit = class(TJSObject,IJSVideoFrameBufferInit)
  Private
  Protected
    function _Getformat: TVideoPixelFormat; 
    function _GetcodedWidth: Cardinal; 
    function _GetcodedHeight: Cardinal; 
    function _Gettimestamp: Int64; 
    function _Getduration: QWord; 
    function _Getlayout: TJSPlaneLayoutDynArray; 
    function _GetdisplayWidth: Cardinal; 
    function _GetdisplayHeight: Cardinal; 
    procedure _Setformat(const aValue: TVideoPixelFormat);
    procedure _SetcodedWidth(const aValue: Cardinal);
    procedure _SetcodedHeight(const aValue: Cardinal);
    procedure _Settimestamp(const aValue: Int64);
    procedure _Setduration(const aValue: QWord);
    procedure _Setlayout(const aValue: TJSPlaneLayoutDynArray);
    procedure _SetdisplayWidth(const aValue: Cardinal);
    procedure _SetdisplayHeight(const aValue: Cardinal);
  Public
    constructor create(const aDict : TJSVideoFrameBufferInitRec); overload;
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSVideoFrameBufferInit;
    property format: TVideoPixelFormat read _Getformat write _Setformat; // required
    property codedWidth: Cardinal read _GetcodedWidth write _SetcodedWidth;
    property codedHeight: Cardinal read _GetcodedHeight write _SetcodedHeight;
    property timestamp: Int64 read _Gettimestamp write _Settimestamp;
    property duration: QWord read _Getduration write _Setduration;
    property layout: TJSPlaneLayoutDynArray read _Getlayout write _Setlayout;
    property displayWidth: Cardinal read _GetdisplayWidth write _SetdisplayWidth;
    property displayHeight: Cardinal read _GetdisplayHeight write _SetdisplayHeight;
  end;
  
  { --------------------------------------------------------------------
    TJSVideoFrameCopyToOptions
    --------------------------------------------------------------------}
  
  TJSVideoFrameCopyToOptionsRec = record
    layout: TJSPlaneLayoutDynArray;
    format: TVideoPixelFormat;
    colorSpace: TPredefinedColorSpace;
  end;
  
  IJSVideoFrameCopyToOptions = interface(IJSObject)
    ['{C4499704-1B01-354A-8DBE-98F362470AEB}']
    function _Getlayout: TJSPlaneLayoutDynArray; 
    function _Getformat: TVideoPixelFormat; 
    function _GetcolorSpace: TPredefinedColorSpace; 
    procedure _Setlayout(const aValue: TJSPlaneLayoutDynArray);
    procedure _Setformat(const aValue: TVideoPixelFormat);
    procedure _SetcolorSpace(const aValue: TPredefinedColorSpace);
    property layout: TJSPlaneLayoutDynArray read _Getlayout write _Setlayout;
    property format: TVideoPixelFormat read _Getformat write _Setformat;
    property colorSpace: TPredefinedColorSpace read _GetcolorSpace write _SetcolorSpace;
  end;
  
  TJSVideoFrameCopyToOptions = class(TJSObject,IJSVideoFrameCopyToOptions)
  Private
  Protected
    function _Getlayout: TJSPlaneLayoutDynArray; 
    function _Getformat: TVideoPixelFormat; 
    function _GetcolorSpace: TPredefinedColorSpace; 
    procedure _Setlayout(const aValue: TJSPlaneLayoutDynArray);
    procedure _Setformat(const aValue: TVideoPixelFormat);
    procedure _SetcolorSpace(const aValue: TPredefinedColorSpace);
  Public
    constructor create(const aDict : TJSVideoFrameCopyToOptionsRec); overload;
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSVideoFrameCopyToOptions;
    property layout: TJSPlaneLayoutDynArray read _Getlayout write _Setlayout;
    property format: TVideoPixelFormat read _Getformat write _Setformat;
    property colorSpace: TPredefinedColorSpace read _GetcolorSpace write _SetcolorSpace;
  end;
  
  { --------------------------------------------------------------------
    TJSPlaneLayout
    --------------------------------------------------------------------}
  
  TJSPlaneLayoutRec = record
    offset: Cardinal;
    stride: Cardinal;
  end;
  
  IJSPlaneLayout = interface(IJSObject)
    ['{3EE08CD3-8E85-3C5F-8386-86AD3028CB40}']
    function _Getoffset: Cardinal; 
    function _Getstride: Cardinal; 
    procedure _Setoffset(const aValue: Cardinal);
    procedure _Setstride(const aValue: Cardinal);
    property offset: Cardinal read _Getoffset write _Setoffset;
    property stride: Cardinal read _Getstride write _Setstride;
  end;
  
  TJSPlaneLayout = class(TJSObject,IJSPlaneLayout)
  Private
  Protected
    function _Getoffset: Cardinal; 
    function _Getstride: Cardinal; 
    procedure _Setoffset(const aValue: Cardinal);
    procedure _Setstride(const aValue: Cardinal);
  Public
    constructor create(const aDict : TJSPlaneLayoutRec); overload;
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSPlaneLayout;
    property offset: Cardinal read _Getoffset write _Setoffset;
    property stride: Cardinal read _Getstride write _Setstride;
  end;
  
  { --------------------------------------------------------------------
    TJSWebGLContextAttributes
    --------------------------------------------------------------------}
  
  TJSWebGLContextAttributesRec = record
    alpha: TGLboolean;
    depth: TGLboolean;
    stencil: TGLboolean;
    antialias: TGLboolean;
    premultipliedAlpha: TGLboolean;
    preserveDrawingBuffer: TGLboolean;
    failIfMajorPerformanceCaveat: TGLboolean;
    powerPreference: TWebGLPowerPreference;
    forceSoftwareRendering: TGLboolean;
    xrCompatible: Boolean;
  end;
  
  IJSWebGLContextAttributes = interface(IJSObject)
    ['{3A387E00-3C5B-3F11-97C7-40243B6E58BB}']
    function _Getalpha: TGLboolean; 
    function _Getdepth: TGLboolean; 
    function _Getstencil: TGLboolean; 
    function _Getantialias: TGLboolean; 
    function _GetpremultipliedAlpha: TGLboolean; 
    function _GetpreserveDrawingBuffer: TGLboolean; 
    function _GetfailIfMajorPerformanceCaveat: TGLboolean; 
    function _GetpowerPreference: TWebGLPowerPreference; 
    function _GetforceSoftwareRendering: TGLboolean; 
    function _GetxrCompatible: Boolean; 
    procedure _Setalpha(const aValue: TGLboolean);
    procedure _Setdepth(const aValue: TGLboolean);
    procedure _Setstencil(const aValue: TGLboolean);
    procedure _Setantialias(const aValue: TGLboolean);
    procedure _SetpremultipliedAlpha(const aValue: TGLboolean);
    procedure _SetpreserveDrawingBuffer(const aValue: TGLboolean);
    procedure _SetfailIfMajorPerformanceCaveat(const aValue: TGLboolean);
    procedure _SetpowerPreference(const aValue: TWebGLPowerPreference);
    procedure _SetforceSoftwareRendering(const aValue: TGLboolean);
    procedure _SetxrCompatible(const aValue: Boolean);
    property alpha: TGLboolean read _Getalpha write _Setalpha;
    property depth: TGLboolean read _Getdepth write _Setdepth;
    property stencil: TGLboolean read _Getstencil write _Setstencil;
    property antialias: TGLboolean read _Getantialias write _Setantialias;
    property premultipliedAlpha: TGLboolean read _GetpremultipliedAlpha write _SetpremultipliedAlpha;
    property preserveDrawingBuffer: TGLboolean read _GetpreserveDrawingBuffer write _SetpreserveDrawingBuffer;
    property failIfMajorPerformanceCaveat: TGLboolean read _GetfailIfMajorPerformanceCaveat write _SetfailIfMajorPerformanceCaveat;
    property powerPreference: TWebGLPowerPreference read _GetpowerPreference write _SetpowerPreference;
    property forceSoftwareRendering: TGLboolean read _GetforceSoftwareRendering write _SetforceSoftwareRendering;
    property xrCompatible: Boolean read _GetxrCompatible write _SetxrCompatible;
  end;
  
  TJSWebGLContextAttributes = class(TJSObject,IJSWebGLContextAttributes)
  Private
  Protected
    function _Getalpha: TGLboolean; 
    function _Getdepth: TGLboolean; 
    function _Getstencil: TGLboolean; 
    function _Getantialias: TGLboolean; 
    function _GetpremultipliedAlpha: TGLboolean; 
    function _GetpreserveDrawingBuffer: TGLboolean; 
    function _GetfailIfMajorPerformanceCaveat: TGLboolean; 
    function _GetpowerPreference: TWebGLPowerPreference; 
    function _GetforceSoftwareRendering: TGLboolean; 
    function _GetxrCompatible: Boolean; 
    procedure _Setalpha(const aValue: TGLboolean);
    procedure _Setdepth(const aValue: TGLboolean);
    procedure _Setstencil(const aValue: TGLboolean);
    procedure _Setantialias(const aValue: TGLboolean);
    procedure _SetpremultipliedAlpha(const aValue: TGLboolean);
    procedure _SetpreserveDrawingBuffer(const aValue: TGLboolean);
    procedure _SetfailIfMajorPerformanceCaveat(const aValue: TGLboolean);
    procedure _SetpowerPreference(const aValue: TWebGLPowerPreference);
    procedure _SetforceSoftwareRendering(const aValue: TGLboolean);
    procedure _SetxrCompatible(const aValue: Boolean);
  Public
    constructor create(const aDict : TJSWebGLContextAttributesRec); overload;
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSWebGLContextAttributes;
    property alpha: TGLboolean read _Getalpha write _Setalpha;
    property depth: TGLboolean read _Getdepth write _Setdepth;
    property stencil: TGLboolean read _Getstencil write _Setstencil;
    property antialias: TGLboolean read _Getantialias write _Setantialias;
    property premultipliedAlpha: TGLboolean read _GetpremultipliedAlpha write _SetpremultipliedAlpha;
    property preserveDrawingBuffer: TGLboolean read _GetpreserveDrawingBuffer write _SetpreserveDrawingBuffer;
    property failIfMajorPerformanceCaveat: TGLboolean read _GetfailIfMajorPerformanceCaveat write _SetfailIfMajorPerformanceCaveat;
    property powerPreference: TWebGLPowerPreference read _GetpowerPreference write _SetpowerPreference;
    property forceSoftwareRendering: TGLboolean read _GetforceSoftwareRendering write _SetforceSoftwareRendering;
    property xrCompatible: Boolean read _GetxrCompatible write _SetxrCompatible;
  end;
  
  { --------------------------------------------------------------------
    TJSAddEventListenerOptions
    --------------------------------------------------------------------}
  
  TJSAddEventListenerOptionsRec = record
    passive: Boolean;
    once: Boolean;
    capture: Boolean;
    mozSystemGroup: Boolean;
  end;
  
  IJSAddEventListenerOptions = interface(IJSEventListenerOptions)
    ['{23EE101A-4D93-3212-A7ED-3DF8D05E8DCE}']
    function _Getpassive: Boolean; 
    function _Getonce: Boolean; 
    procedure _Setpassive(const aValue: Boolean);
    procedure _Setonce(const aValue: Boolean);
    property passive: Boolean read _Getpassive write _Setpassive;
    property once: Boolean read _Getonce write _Setonce;
  end;
  
  TJSAddEventListenerOptions = class(TJSEventListenerOptions,IJSAddEventListenerOptions)
  Private
  Protected
    function _Getpassive: Boolean; 
    function _Getonce: Boolean; 
    procedure _Setpassive(const aValue: Boolean);
    procedure _Setonce(const aValue: Boolean);
  Public
    constructor create(const aDict : TJSAddEventListenerOptionsRec); overload;
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSAddEventListenerOptions;
    property passive: Boolean read _Getpassive write _Setpassive;
    property once: Boolean read _Getonce write _Setonce;
  end;
  
  { --------------------------------------------------------------------
    TJSUIEventInit
    --------------------------------------------------------------------}
  
  TJSUIEventInitRec = record
    view: TJSWindow;
    detail: LongInt;
    bubbles: Boolean;
    cancelable: Boolean;
    composed: Boolean;
  end;
  
  IJSUIEventInit = interface(IJSEventInit)
    ['{7A60C9B8-69FA-3FC6-BDA8-E8AD03E09E8A}']
    function _Getview: IJSWindow; 
    function _Getdetail: LongInt; 
    procedure _Setview(const aValue: IJSWindow);
    procedure _Setdetail(const aValue: LongInt);
    property view: IJSWindow read _Getview write _Setview;
    property detail: LongInt read _Getdetail write _Setdetail;
  end;
  
  TJSUIEventInit = class(TJSEventInit,IJSUIEventInit)
  Private
  Protected
    function _Getview: IJSWindow; 
    function _Getdetail: LongInt; 
    procedure _Setview(const aValue: IJSWindow);
    procedure _Setdetail(const aValue: LongInt);
  Public
    constructor create(const aDict : TJSUIEventInitRec); overload;
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSUIEventInit;
    property view: IJSWindow read _Getview write _Setview;
    property detail: LongInt read _Getdetail write _Setdetail;
  end;
  
  { --------------------------------------------------------------------
    TJSWebGLContextEventInit
    --------------------------------------------------------------------}
  
  TJSWebGLContextEventInitRec = record
    statusMessage: UnicodeString;
    bubbles: Boolean;
    cancelable: Boolean;
    composed: Boolean;
  end;
  
  IJSWebGLContextEventInit = interface(IJSEventInit)
    ['{DA4E12E7-D4C8-3F26-A90B-1A3D966E3EAA}']
    function _GetstatusMessage: UnicodeString; 
    procedure _SetstatusMessage(const aValue: UnicodeString);
    property statusMessage: UnicodeString read _GetstatusMessage write _SetstatusMessage;
  end;
  
  TJSWebGLContextEventInit = class(TJSEventInit,IJSWebGLContextEventInit)
  Private
  Protected
    function _GetstatusMessage: UnicodeString; 
    procedure _SetstatusMessage(const aValue: UnicodeString);
  Public
    constructor create(const aDict : TJSWebGLContextEventInitRec); overload;
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSWebGLContextEventInit;
    property statusMessage: UnicodeString read _GetstatusMessage write _SetstatusMessage;
  end;
  
  { --------------------------------------------------------------------
    TJSEventModifierInit
    --------------------------------------------------------------------}
  
  TJSEventModifierInitRec = record
    ctrlKey: Boolean;
    shiftKey: Boolean;
    altKey: Boolean;
    metaKey: Boolean;
    modifierAltGraph: Boolean;
    modifierCapsLock: Boolean;
    modifierFn: Boolean;
    modifierFnLock: Boolean;
    modifierNumLock: Boolean;
    modifierOS: Boolean;
    modifierScrollLock: Boolean;
    modifierSymbol: Boolean;
    modifierSymbolLock: Boolean;
    view: TJSWindow;
    detail: LongInt;
    bubbles: Boolean;
    cancelable: Boolean;
    composed: Boolean;
  end;
  
  IJSEventModifierInit = interface(IJSUIEventInit)
    ['{2CEC0442-6DC0-3932-9972-C79F1BE13412}']
    function _GetctrlKey: Boolean; 
    function _GetshiftKey: Boolean; 
    function _GetaltKey: Boolean; 
    function _GetmetaKey: Boolean; 
    function _GetmodifierAltGraph: Boolean; 
    function _GetmodifierCapsLock: Boolean; 
    function _GetmodifierFn: Boolean; 
    function _GetmodifierFnLock: Boolean; 
    function _GetmodifierNumLock: Boolean; 
    function _GetmodifierOS: Boolean; 
    function _GetmodifierScrollLock: Boolean; 
    function _GetmodifierSymbol: Boolean; 
    function _GetmodifierSymbolLock: Boolean; 
    procedure _SetctrlKey(const aValue: Boolean);
    procedure _SetshiftKey(const aValue: Boolean);
    procedure _SetaltKey(const aValue: Boolean);
    procedure _SetmetaKey(const aValue: Boolean);
    procedure _SetmodifierAltGraph(const aValue: Boolean);
    procedure _SetmodifierCapsLock(const aValue: Boolean);
    procedure _SetmodifierFn(const aValue: Boolean);
    procedure _SetmodifierFnLock(const aValue: Boolean);
    procedure _SetmodifierNumLock(const aValue: Boolean);
    procedure _SetmodifierOS(const aValue: Boolean);
    procedure _SetmodifierScrollLock(const aValue: Boolean);
    procedure _SetmodifierSymbol(const aValue: Boolean);
    procedure _SetmodifierSymbolLock(const aValue: Boolean);
    property ctrlKey: Boolean read _GetctrlKey write _SetctrlKey;
    property shiftKey: Boolean read _GetshiftKey write _SetshiftKey;
    property altKey: Boolean read _GetaltKey write _SetaltKey;
    property metaKey: Boolean read _GetmetaKey write _SetmetaKey;
    property modifierAltGraph: Boolean read _GetmodifierAltGraph write _SetmodifierAltGraph;
    property modifierCapsLock: Boolean read _GetmodifierCapsLock write _SetmodifierCapsLock;
    property modifierFn: Boolean read _GetmodifierFn write _SetmodifierFn;
    property modifierFnLock: Boolean read _GetmodifierFnLock write _SetmodifierFnLock;
    property modifierNumLock: Boolean read _GetmodifierNumLock write _SetmodifierNumLock;
    property modifierOS: Boolean read _GetmodifierOS write _SetmodifierOS;
    property modifierScrollLock: Boolean read _GetmodifierScrollLock write _SetmodifierScrollLock;
    property modifierSymbol: Boolean read _GetmodifierSymbol write _SetmodifierSymbol;
    property modifierSymbolLock: Boolean read _GetmodifierSymbolLock write _SetmodifierSymbolLock;
  end;
  
  TJSEventModifierInit = class(TJSUIEventInit,IJSEventModifierInit)
  Private
  Protected
    function _GetctrlKey: Boolean; 
    function _GetshiftKey: Boolean; 
    function _GetaltKey: Boolean; 
    function _GetmetaKey: Boolean; 
    function _GetmodifierAltGraph: Boolean; 
    function _GetmodifierCapsLock: Boolean; 
    function _GetmodifierFn: Boolean; 
    function _GetmodifierFnLock: Boolean; 
    function _GetmodifierNumLock: Boolean; 
    function _GetmodifierOS: Boolean; 
    function _GetmodifierScrollLock: Boolean; 
    function _GetmodifierSymbol: Boolean; 
    function _GetmodifierSymbolLock: Boolean; 
    procedure _SetctrlKey(const aValue: Boolean);
    procedure _SetshiftKey(const aValue: Boolean);
    procedure _SetaltKey(const aValue: Boolean);
    procedure _SetmetaKey(const aValue: Boolean);
    procedure _SetmodifierAltGraph(const aValue: Boolean);
    procedure _SetmodifierCapsLock(const aValue: Boolean);
    procedure _SetmodifierFn(const aValue: Boolean);
    procedure _SetmodifierFnLock(const aValue: Boolean);
    procedure _SetmodifierNumLock(const aValue: Boolean);
    procedure _SetmodifierOS(const aValue: Boolean);
    procedure _SetmodifierScrollLock(const aValue: Boolean);
    procedure _SetmodifierSymbol(const aValue: Boolean);
    procedure _SetmodifierSymbolLock(const aValue: Boolean);
  Public
    constructor create(const aDict : TJSEventModifierInitRec); overload;
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSEventModifierInit;
    property ctrlKey: Boolean read _GetctrlKey write _SetctrlKey;
    property shiftKey: Boolean read _GetshiftKey write _SetshiftKey;
    property altKey: Boolean read _GetaltKey write _SetaltKey;
    property metaKey: Boolean read _GetmetaKey write _SetmetaKey;
    property modifierAltGraph: Boolean read _GetmodifierAltGraph write _SetmodifierAltGraph;
    property modifierCapsLock: Boolean read _GetmodifierCapsLock write _SetmodifierCapsLock;
    property modifierFn: Boolean read _GetmodifierFn write _SetmodifierFn;
    property modifierFnLock: Boolean read _GetmodifierFnLock write _SetmodifierFnLock;
    property modifierNumLock: Boolean read _GetmodifierNumLock write _SetmodifierNumLock;
    property modifierOS: Boolean read _GetmodifierOS write _SetmodifierOS;
    property modifierScrollLock: Boolean read _GetmodifierScrollLock write _SetmodifierScrollLock;
    property modifierSymbol: Boolean read _GetmodifierSymbol write _SetmodifierSymbol;
    property modifierSymbolLock: Boolean read _GetmodifierSymbolLock write _SetmodifierSymbolLock;
  end;
  
  { --------------------------------------------------------------------
    TJSKeyboardEventInit
    --------------------------------------------------------------------}
  
  TJSKeyboardEventInitRec = record
    key: UnicodeString;
    code: UnicodeString;
    location: Cardinal;
    repeat_: Boolean;
    isComposing: Boolean;
    charCode: Cardinal;
    keyCode: Cardinal;
    which: Cardinal;
    ctrlKey: Boolean;
    shiftKey: Boolean;
    altKey: Boolean;
    metaKey: Boolean;
    modifierAltGraph: Boolean;
    modifierCapsLock: Boolean;
    modifierFn: Boolean;
    modifierFnLock: Boolean;
    modifierNumLock: Boolean;
    modifierOS: Boolean;
    modifierScrollLock: Boolean;
    modifierSymbol: Boolean;
    modifierSymbolLock: Boolean;
    view: TJSWindow;
    detail: LongInt;
    bubbles: Boolean;
    cancelable: Boolean;
    composed: Boolean;
  end;
  
  IJSKeyboardEventInit = interface(IJSEventModifierInit)
    ['{40F8318E-16C5-3E3D-8CFE-37F939DF7424}']
    function _Getkey: UnicodeString; 
    function _Getcode: UnicodeString; 
    function _Getlocation: Cardinal; 
    function _Getrepeat_: Boolean; 
    function _GetisComposing: Boolean; 
    function _GetcharCode: Cardinal; 
    function _GetkeyCode: Cardinal; 
    function _Getwhich: Cardinal; 
    procedure _Setkey(const aValue: UnicodeString);
    procedure _Setcode(const aValue: UnicodeString);
    procedure _Setlocation(const aValue: Cardinal);
    procedure _Setrepeat_(const aValue: Boolean);
    procedure _SetisComposing(const aValue: Boolean);
    procedure _SetcharCode(const aValue: Cardinal);
    procedure _SetkeyCode(const aValue: Cardinal);
    procedure _Setwhich(const aValue: Cardinal);
    property key: UnicodeString read _Getkey write _Setkey;
    property code: UnicodeString read _Getcode write _Setcode;
    property location: Cardinal read _Getlocation write _Setlocation;
    property repeat_: Boolean read _Getrepeat_ write _Setrepeat_;
    property isComposing: Boolean read _GetisComposing write _SetisComposing;
    property charCode: Cardinal read _GetcharCode write _SetcharCode;
    property keyCode: Cardinal read _GetkeyCode write _SetkeyCode;
    property which: Cardinal read _Getwhich write _Setwhich;
  end;
  
  TJSKeyboardEventInit = class(TJSEventModifierInit,IJSKeyboardEventInit)
  Private
  Protected
    function _Getkey: UnicodeString; 
    function _Getcode: UnicodeString; 
    function _Getlocation: Cardinal; 
    function _Getrepeat_: Boolean; 
    function _GetisComposing: Boolean; 
    function _GetcharCode: Cardinal; 
    function _GetkeyCode: Cardinal; 
    function _Getwhich: Cardinal; 
    procedure _Setkey(const aValue: UnicodeString);
    procedure _Setcode(const aValue: UnicodeString);
    procedure _Setlocation(const aValue: Cardinal);
    procedure _Setrepeat_(const aValue: Boolean);
    procedure _SetisComposing(const aValue: Boolean);
    procedure _SetcharCode(const aValue: Cardinal);
    procedure _SetkeyCode(const aValue: Cardinal);
    procedure _Setwhich(const aValue: Cardinal);
  Public
    constructor create(const aDict : TJSKeyboardEventInitRec); overload;
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSKeyboardEventInit;
    property key: UnicodeString read _Getkey write _Setkey;
    property code: UnicodeString read _Getcode write _Setcode;
    property location: Cardinal read _Getlocation write _Setlocation;
    property repeat_: Boolean read _Getrepeat_ write _Setrepeat_;
    property isComposing: Boolean read _GetisComposing write _SetisComposing;
    property charCode: Cardinal read _GetcharCode write _SetcharCode;
    property keyCode: Cardinal read _GetkeyCode write _SetkeyCode;
    property which: Cardinal read _Getwhich write _Setwhich;
  end;
  
  { --------------------------------------------------------------------
    TJSMouseEventInit
    --------------------------------------------------------------------}
  
  TJSMouseEventInitRec = record
    screenX: Double;
    screenY: Double;
    clientX: Double;
    clientY: Double;
    button: SmallInt;
    buttons: Word;
    relatedTarget: TJSEventTarget;
    movementX: LongInt;
    movementY: LongInt;
    ctrlKey: Boolean;
    shiftKey: Boolean;
    altKey: Boolean;
    metaKey: Boolean;
    modifierAltGraph: Boolean;
    modifierCapsLock: Boolean;
    modifierFn: Boolean;
    modifierFnLock: Boolean;
    modifierNumLock: Boolean;
    modifierOS: Boolean;
    modifierScrollLock: Boolean;
    modifierSymbol: Boolean;
    modifierSymbolLock: Boolean;
    view: TJSWindow;
    detail: LongInt;
    bubbles: Boolean;
    cancelable: Boolean;
    composed: Boolean;
  end;
  
  IJSMouseEventInit = interface(IJSEventModifierInit)
    ['{AA62FC06-A487-33BE-81F6-2D34E48E438D}']
    function _GetscreenX: Double; 
    function _GetscreenY: Double; 
    function _GetclientX: Double; 
    function _GetclientY: Double; 
    function _Getbutton: SmallInt; 
    function _Getbuttons: Word; 
    function _GetrelatedTarget: IJSEventTarget; 
    function _GetmovementX: LongInt; 
    function _GetmovementY: LongInt; 
    procedure _SetscreenX(const aValue: Double);
    procedure _SetscreenY(const aValue: Double);
    procedure _SetclientX(const aValue: Double);
    procedure _SetclientY(const aValue: Double);
    procedure _Setbutton(const aValue: SmallInt);
    procedure _Setbuttons(const aValue: Word);
    procedure _SetrelatedTarget(const aValue: IJSEventTarget);
    procedure _SetmovementX(const aValue: LongInt);
    procedure _SetmovementY(const aValue: LongInt);
    property screenX: Double read _GetscreenX write _SetscreenX;
    property screenY: Double read _GetscreenY write _SetscreenY;
    property clientX: Double read _GetclientX write _SetclientX;
    property clientY: Double read _GetclientY write _SetclientY;
    property button: SmallInt read _Getbutton write _Setbutton;
    property buttons: Word read _Getbuttons write _Setbuttons;
    property relatedTarget: IJSEventTarget read _GetrelatedTarget write _SetrelatedTarget;
    property movementX: LongInt read _GetmovementX write _SetmovementX;
    property movementY: LongInt read _GetmovementY write _SetmovementY;
  end;
  
  TJSMouseEventInit = class(TJSEventModifierInit,IJSMouseEventInit)
  Private
  Protected
    function _GetscreenX: Double; 
    function _GetscreenY: Double; 
    function _GetclientX: Double; 
    function _GetclientY: Double; 
    function _Getbutton: SmallInt; 
    function _Getbuttons: Word; 
    function _GetrelatedTarget: IJSEventTarget; 
    function _GetmovementX: LongInt; 
    function _GetmovementY: LongInt; 
    procedure _SetscreenX(const aValue: Double);
    procedure _SetscreenY(const aValue: Double);
    procedure _SetclientX(const aValue: Double);
    procedure _SetclientY(const aValue: Double);
    procedure _Setbutton(const aValue: SmallInt);
    procedure _Setbuttons(const aValue: Word);
    procedure _SetrelatedTarget(const aValue: IJSEventTarget);
    procedure _SetmovementX(const aValue: LongInt);
    procedure _SetmovementY(const aValue: LongInt);
  Public
    constructor create(const aDict : TJSMouseEventInitRec); overload;
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSMouseEventInit;
    property screenX: Double read _GetscreenX write _SetscreenX;
    property screenY: Double read _GetscreenY write _SetscreenY;
    property clientX: Double read _GetclientX write _SetclientX;
    property clientY: Double read _GetclientY write _SetclientY;
    property button: SmallInt read _Getbutton write _Setbutton;
    property buttons: Word read _Getbuttons write _Setbuttons;
    property relatedTarget: IJSEventTarget read _GetrelatedTarget write _SetrelatedTarget;
    property movementX: LongInt read _GetmovementX write _SetmovementX;
    property movementY: LongInt read _GetmovementY write _SetmovementY;
  end;
  
  { --------------------------------------------------------------------
    TJSPointerEventInit
    --------------------------------------------------------------------}
  
  TJSPointerEventDynArray = IJSArray; // array of PointerEvent
  TJSPointerEventInitRec = record
    pointerId: LongInt;
    width: Double;
    height: Double;
    pressure: Single;
    tangentialPressure: Single;
    tiltX: LongInt;
    tiltY: LongInt;
    twist: LongInt;
    altitudeAngle: Double;
    azimuthAngle: Double;
    pointerType: UnicodeString;
    isPrimary: Boolean;
    coalescedEvents: TJSPointerEventDynArray;
    predictedEvents: TJSPointerEventDynArray;
    screenX: Double;
    screenY: Double;
    clientX: Double;
    clientY: Double;
    button: SmallInt;
    buttons: Word;
    relatedTarget: TJSEventTarget;
    movementX: LongInt;
    movementY: LongInt;
    ctrlKey: Boolean;
    shiftKey: Boolean;
    altKey: Boolean;
    metaKey: Boolean;
    modifierAltGraph: Boolean;
    modifierCapsLock: Boolean;
    modifierFn: Boolean;
    modifierFnLock: Boolean;
    modifierNumLock: Boolean;
    modifierOS: Boolean;
    modifierScrollLock: Boolean;
    modifierSymbol: Boolean;
    modifierSymbolLock: Boolean;
    view: TJSWindow;
    detail: LongInt;
    bubbles: Boolean;
    cancelable: Boolean;
    composed: Boolean;
  end;
  
  IJSPointerEventInit = interface(IJSMouseEventInit)
    ['{BAD619C0-3444-3B5C-B079-B975554E00D0}']
    function _GetpointerId: LongInt; 
    function _Getwidth: Double; 
    function _Getheight: Double; 
    function _Getpressure: Single; 
    function _GettangentialPressure: Single; 
    function _GettiltX: LongInt; 
    function _GettiltY: LongInt; 
    function _Gettwist: LongInt; 
    function _GetaltitudeAngle: Double; 
    function _GetazimuthAngle: Double; 
    function _GetpointerType: UnicodeString; 
    function _GetisPrimary: Boolean; 
    function _GetcoalescedEvents: TJSPointerEventDynArray; 
    function _GetpredictedEvents: TJSPointerEventDynArray; 
    procedure _SetpointerId(const aValue: LongInt);
    procedure _Setwidth(const aValue: Double);
    procedure _Setheight(const aValue: Double);
    procedure _Setpressure(const aValue: Single);
    procedure _SettangentialPressure(const aValue: Single);
    procedure _SettiltX(const aValue: LongInt);
    procedure _SettiltY(const aValue: LongInt);
    procedure _Settwist(const aValue: LongInt);
    procedure _SetaltitudeAngle(const aValue: Double);
    procedure _SetazimuthAngle(const aValue: Double);
    procedure _SetpointerType(const aValue: UnicodeString);
    procedure _SetisPrimary(const aValue: Boolean);
    procedure _SetcoalescedEvents(const aValue: TJSPointerEventDynArray);
    procedure _SetpredictedEvents(const aValue: TJSPointerEventDynArray);
    property pointerId: LongInt read _GetpointerId write _SetpointerId;
    property width: Double read _Getwidth write _Setwidth;
    property height: Double read _Getheight write _Setheight;
    property pressure: Single read _Getpressure write _Setpressure;
    property tangentialPressure: Single read _GettangentialPressure write _SettangentialPressure;
    property tiltX: LongInt read _GettiltX write _SettiltX;
    property tiltY: LongInt read _GettiltY write _SettiltY;
    property twist: LongInt read _Gettwist write _Settwist;
    property altitudeAngle: Double read _GetaltitudeAngle write _SetaltitudeAngle;
    property azimuthAngle: Double read _GetazimuthAngle write _SetazimuthAngle;
    property pointerType: UnicodeString read _GetpointerType write _SetpointerType;
    property isPrimary: Boolean read _GetisPrimary write _SetisPrimary;
    property coalescedEvents: TJSPointerEventDynArray read _GetcoalescedEvents write _SetcoalescedEvents;
    property predictedEvents: TJSPointerEventDynArray read _GetpredictedEvents write _SetpredictedEvents;
  end;
  
  TJSPointerEventInit = class(TJSMouseEventInit,IJSPointerEventInit)
  Private
  Protected
    function _GetpointerId: LongInt; 
    function _Getwidth: Double; 
    function _Getheight: Double; 
    function _Getpressure: Single; 
    function _GettangentialPressure: Single; 
    function _GettiltX: LongInt; 
    function _GettiltY: LongInt; 
    function _Gettwist: LongInt; 
    function _GetaltitudeAngle: Double; 
    function _GetazimuthAngle: Double; 
    function _GetpointerType: UnicodeString; 
    function _GetisPrimary: Boolean; 
    function _GetcoalescedEvents: TJSPointerEventDynArray; 
    function _GetpredictedEvents: TJSPointerEventDynArray; 
    procedure _SetpointerId(const aValue: LongInt);
    procedure _Setwidth(const aValue: Double);
    procedure _Setheight(const aValue: Double);
    procedure _Setpressure(const aValue: Single);
    procedure _SettangentialPressure(const aValue: Single);
    procedure _SettiltX(const aValue: LongInt);
    procedure _SettiltY(const aValue: LongInt);
    procedure _Settwist(const aValue: LongInt);
    procedure _SetaltitudeAngle(const aValue: Double);
    procedure _SetazimuthAngle(const aValue: Double);
    procedure _SetpointerType(const aValue: UnicodeString);
    procedure _SetisPrimary(const aValue: Boolean);
    procedure _SetcoalescedEvents(const aValue: TJSPointerEventDynArray);
    procedure _SetpredictedEvents(const aValue: TJSPointerEventDynArray);
  Public
    constructor create(const aDict : TJSPointerEventInitRec); overload;
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSPointerEventInit;
    property pointerId: LongInt read _GetpointerId write _SetpointerId;
    property width: Double read _Getwidth write _Setwidth;
    property height: Double read _Getheight write _Setheight;
    property pressure: Single read _Getpressure write _Setpressure;
    property tangentialPressure: Single read _GettangentialPressure write _SettangentialPressure;
    property tiltX: LongInt read _GettiltX write _SettiltX;
    property tiltY: LongInt read _GettiltY write _SettiltY;
    property twist: LongInt read _Gettwist write _Settwist;
    property altitudeAngle: Double read _GetaltitudeAngle write _SetaltitudeAngle;
    property azimuthAngle: Double read _GetazimuthAngle write _SetazimuthAngle;
    property pointerType: UnicodeString read _GetpointerType write _SetpointerType;
    property isPrimary: Boolean read _GetisPrimary write _SetisPrimary;
    property coalescedEvents: TJSPointerEventDynArray read _GetcoalescedEvents write _SetcoalescedEvents;
    property predictedEvents: TJSPointerEventDynArray read _GetpredictedEvents write _SetpredictedEvents;
  end;
  
  { --------------------------------------------------------------------
    TJSAnimationFrameProvider
    --------------------------------------------------------------------}
  
  IJSAnimationFrameProvider = interface(IJSObject)
    ['{7685FDC3-774D-3963-9FF2-0ED5389D80ED}']
    function requestAnimationFrame(const aCallback: TFrameRequestCallback): LongInt;
    procedure cancelAnimationFrame(aHandle: LongInt);
  end;
  
  TJSAnimationFrameProvider = class(TJSObject,IJSAnimationFrameProvider)
  Private
  Protected
  Public
    function requestAnimationFrame(const aCallback: TFrameRequestCallback): LongInt; overload;
    procedure cancelAnimationFrame(aHandle: LongInt); overload;
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSAnimationFrameProvider;
  end;
  
  { --------------------------------------------------------------------
    TJSCSSStyleDeclaration
    --------------------------------------------------------------------}
  
  IJSCSSStyleDeclaration = interface(IJSObject)
    ['{F6EBDCB3-BEBB-3837-B388-6847C13B52DD}']
    function _GetcssText: UTF8String; 
    function _Getlength_: Cardinal; 
    procedure _SetcssText(const aValue: UTF8String);
    function item(aIndex: Cardinal): UTF8String;
    function getPropertyValue(const aProperty_: UTF8String): UTF8String;
    function getPropertyPriority(const aProperty_: UTF8String): UTF8String;
    procedure setProperty(const aProperty_: UTF8String; const aValue: UTF8String; const aPriority: UTF8String);
    procedure setProperty(const aProperty_: UTF8String; const aValue: UTF8String);
    function removeProperty(const aProperty_: UTF8String): UTF8String;
    property cssText: UTF8String read _GetcssText write _SetcssText;
    property length_: Cardinal read _Getlength_;
  end;
  
  TJSCSSStyleDeclaration = class(TJSObject,IJSCSSStyleDeclaration)
  Private
  Protected
    function _GetcssText: UTF8String; 
    function _Getlength_: Cardinal; 
    procedure _SetcssText(const aValue: UTF8String);
  Public
    function item(aIndex: Cardinal): UTF8String; overload;
    function getPropertyValue(const aProperty_: UTF8String): UTF8String; overload;
    function getPropertyPriority(const aProperty_: UTF8String): UTF8String; overload;
    procedure setProperty(const aProperty_: UTF8String; const aValue: UTF8String; const aPriority: UTF8String); overload;
    procedure setProperty(const aProperty_: UTF8String; const aValue: UTF8String); overload;
    function removeProperty(const aProperty_: UTF8String): UTF8String; overload;
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSCSSStyleDeclaration;
    property cssText: UTF8String read _GetcssText write _SetcssText;
    property length_: Cardinal read _Getlength_;
  end;
  
  { --------------------------------------------------------------------
    TJSContentSecurityPolicy
    --------------------------------------------------------------------}
  
  IJSContentSecurityPolicy = interface(IJSObject)
    ['{E0F14358-2D99-30AE-B53A-19677BFCD90F}']
  end;
  
  TJSContentSecurityPolicy = class(TJSObject,IJSContentSecurityPolicy)
  Private
  Protected
  Public
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSContentSecurityPolicy;
  end;
  
  { --------------------------------------------------------------------
    TJSPrincipal
    --------------------------------------------------------------------}
  
  IJSPrincipal = interface(IJSObject)
    ['{33824E56-58B9-38D0-B869-8DD15390C552}']
  end;
  
  TJSPrincipal = class(TJSObject,IJSPrincipal)
  Private
  Protected
  Public
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSPrincipal;
  end;
  
  { --------------------------------------------------------------------
    TJSWindowProxy
    --------------------------------------------------------------------}
  
  IJSWindowProxy = interface(IJSObject)
    ['{EEE0FA86-59DA-3E5C-A83D-1A2372542131}']
  end;
  
  TJSWindowProxy = class(TJSObject,IJSWindowProxy)
  Private
  Protected
  Public
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSWindowProxy;
  end;
  
  { --------------------------------------------------------------------
    TJSnsISupports
    --------------------------------------------------------------------}
  
  IJSnsISupports = interface(IJSObject)
    ['{EEE16FF0-3EAA-398A-903D-1A2372542131}']
  end;
  
  TJSnsISupports = class(TJSObject,IJSnsISupports)
  Private
  Protected
  Public
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSnsISupports;
  end;
  
  { --------------------------------------------------------------------
    TJSURI
    --------------------------------------------------------------------}
  
  IJSURI = interface(IJSObject)
    ['{677FA928-3D1A-3372-9421-3194B5554269}']
  end;
  
  TJSURI = class(TJSObject,IJSURI)
  Private
  Protected
  Public
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSURI;
  end;
  
  { --------------------------------------------------------------------
    TJSnsIDocShell
    --------------------------------------------------------------------}
  
  IJSnsIDocShell = interface(IJSObject)
    ['{EEE16FEE-5839-3BB6-B43D-1A2372542131}']
  end;
  
  TJSnsIDocShell = class(TJSObject,IJSnsIDocShell)
  Private
  Protected
  Public
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSnsIDocShell;
  end;
  
  { --------------------------------------------------------------------
    TJSnsILoadGroup
    --------------------------------------------------------------------}
  
  IJSnsILoadGroup = interface(IJSObject)
    ['{E6D0CC5C-1971-3F00-A220-F0D81AA2A621}']
  end;
  
  TJSnsILoadGroup = class(TJSObject,IJSnsILoadGroup)
  Private
  Protected
  Public
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSnsILoadGroup;
  end;
  
  { --------------------------------------------------------------------
    TJSnsIReferrerInfo
    --------------------------------------------------------------------}
  
  IJSnsIReferrerInfo = interface(IJSObject)
    ['{AC6D32F7-0AAD-30F2-A5E1-4B03E0B13444}']
  end;
  
  TJSnsIReferrerInfo = class(TJSObject,IJSnsIReferrerInfo)
  Private
  Protected
  Public
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSnsIReferrerInfo;
  end;
  
  { --------------------------------------------------------------------
    TJSnsICookieJarSettings
    --------------------------------------------------------------------}
  
  IJSnsICookieJarSettings = interface(IJSObject)
    ['{3346BB52-F01F-3C27-81BD-9658712A80F0}']
  end;
  
  TJSnsICookieJarSettings = class(TJSObject,IJSnsICookieJarSettings)
  Private
  Protected
  Public
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSnsICookieJarSettings;
  end;
  
  { --------------------------------------------------------------------
    TJSnsIPermissionDelegateHandler
    --------------------------------------------------------------------}
  
  IJSnsIPermissionDelegateHandler = interface(IJSObject)
    ['{E64C3B0E-5583-330B-976E-C5C335C10FA8}']
  end;
  
  TJSnsIPermissionDelegateHandler = class(TJSObject,IJSnsIPermissionDelegateHandler)
  Private
  Protected
  Public
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSnsIPermissionDelegateHandler;
  end;
  
  { --------------------------------------------------------------------
    TJSXULCommandDispatcher
    --------------------------------------------------------------------}
  
  IJSXULCommandDispatcher = interface(IJSObject)
    ['{33454C5E-F00F-3CC4-8CBE-E8980E2260F0}']
  end;
  
  TJSXULCommandDispatcher = class(TJSObject,IJSXULCommandDispatcher)
  Private
  Protected
  Public
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSXULCommandDispatcher;
  end;
  
  { --------------------------------------------------------------------
    TJSDOMRectReadOnly
    --------------------------------------------------------------------}
  
  IJSDOMRectReadOnly = interface(IJSObject)
    ['{1945BAF0-30A9-3E11-8775-7872297173E5}']
    function _Getx: Double; 
    function _Gety: Double; 
    function _Getwidth: Double; 
    function _Getheight: Double; 
    function _Gettop: Double; 
    function _Getright: Double; 
    function _Getbottom: Double; 
    function _Getleft: Double; 
    function toJSON: IJSObject;
    property x: Double read _Getx;
    property y: Double read _Gety;
    property width: Double read _Getwidth;
    property height: Double read _Getheight;
    property top: Double read _Gettop;
    property right: Double read _Getright;
    property bottom: Double read _Getbottom;
    property left: Double read _Getleft;
  end;
  
  TJSDOMRectReadOnly = class(TJSObject,IJSDOMRectReadOnly)
  Private
  Protected
    function _Getx: Double; 
    function _Gety: Double; 
    function _Getwidth: Double; 
    function _Getheight: Double; 
    function _Gettop: Double; 
    function _Getright: Double; 
    function _Getbottom: Double; 
    function _Getleft: Double; 
  Public
    constructor Create(aX: Double; aY: Double; aWidth: Double; aHeight: Double); overload;
    constructor Create; overload;
    constructor Create(aX: Double); overload;
    constructor Create(aX: Double; aY: Double); overload;
    constructor Create(aX: Double; aY: Double; aWidth: Double); overload;
    function fromRect(const aOther: IJSDOMRectInit): IJSDOMRectReadOnly; overload;
    function fromRect: IJSDOMRectReadOnly; overload;
    function toJSON: IJSObject; overload;
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSDOMRectReadOnly;
    property x: Double read _Getx;
    property y: Double read _Gety;
    property width: Double read _Getwidth;
    property height: Double read _Getheight;
    property top: Double read _Gettop;
    property right: Double read _Getright;
    property bottom: Double read _Getbottom;
    property left: Double read _Getleft;
  end;
  
  { --------------------------------------------------------------------
    TJSnsIScreen
    --------------------------------------------------------------------}
  
  IJSnsIScreen = interface(IJSObject)
    ['{33832E53-F8FD-30F0-B869-8DD15390C552}']
  end;
  
  TJSnsIScreen = class(TJSObject,IJSnsIScreen)
  Private
  Protected
  Public
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSnsIScreen;
  end;
  
  { --------------------------------------------------------------------
    TJSElementCSSInlineStyle
    --------------------------------------------------------------------}
  
  IJSElementCSSInlineStyle = interface(IJSObject)
    ['{A68CE660-BA16-3C6A-855A-0663A9D0E1BF}']
    function _Getstyle: IJSCSSStyleDeclaration; 
    property style: IJSCSSStyleDeclaration read _Getstyle;
  end;
  
  TJSElementCSSInlineStyle = class(TJSObject,IJSElementCSSInlineStyle)
  Private
  Protected
    function _Getstyle: IJSCSSStyleDeclaration; 
  Public
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSElementCSSInlineStyle;
    property style: IJSCSSStyleDeclaration read _Getstyle;
  end;
  
  { --------------------------------------------------------------------
    TJSGlobalEventHandlers
    --------------------------------------------------------------------}
  
  IJSGlobalEventHandlers = interface(IJSObject)
    ['{60B8C1FB-8AA9-36D6-B262-C54EA1FB0C5E}']
    function _Getonabort: TEventHandler; 
    function _Getonblur: TEventHandler; 
    function _Getonfocus: TEventHandler; 
    function _Getoncancel: TEventHandler; 
    function _Getonauxclick: TEventHandler; 
    function _Getonbeforeinput: TEventHandler; 
    function _Getonbeforetoggle: TEventHandler; 
    function _Getoncanplay: TEventHandler; 
    function _Getoncanplaythrough: TEventHandler; 
    function _Getonchange: TEventHandler; 
    function _Getonclick: TEventHandler; 
    function _Getonclose: TEventHandler; 
    function _Getoncontentvisibilityautostatechange: TEventHandler; 
    function _Getoncontextlost: TEventHandler; 
    function _Getoncontextmenu: TEventHandler; 
    function _Getoncontextrestored: TEventHandler; 
    function _Getoncopy: TEventHandler; 
    function _Getoncuechange: TEventHandler; 
    function _Getoncut: TEventHandler; 
    function _Getondblclick: TEventHandler; 
    function _Getondrag: TEventHandler; 
    function _Getondragend: TEventHandler; 
    function _Getondragenter: TEventHandler; 
    function _Getondragexit: TEventHandler; 
    function _Getondragleave: TEventHandler; 
    function _Getondragover: TEventHandler; 
    function _Getondragstart: TEventHandler; 
    function _Getondrop: TEventHandler; 
    function _Getondurationchange: TEventHandler; 
    function _Getonemptied: TEventHandler; 
    function _Getonended: TEventHandler; 
    function _Getonformdata: TEventHandler; 
    function _Getoninput: TEventHandler; 
    function _Getoninvalid: TEventHandler; 
    function _Getonkeydown: TEventHandler; 
    function _Getonkeypress: TEventHandler; 
    function _Getonkeyup: TEventHandler; 
    function _Getonload: TEventHandler; 
    function _Getonloadeddata: TEventHandler; 
    function _Getonloadedmetadata: TEventHandler; 
    function _Getonloadstart: TEventHandler; 
    function _Getonmousedown: TEventHandler; 
    function _Getonmouseenter: TEventHandler; 
    function _Getonmouseleave: TEventHandler; 
    function _Getonmousemove: TEventHandler; 
    function _Getonmouseout: TEventHandler; 
    function _Getonmouseover: TEventHandler; 
    function _Getonmouseup: TEventHandler; 
    function _Getonwheel: TEventHandler; 
    function _Getonpaste: TEventHandler; 
    function _Getonpause: TEventHandler; 
    function _Getonplay: TEventHandler; 
    function _Getonplaying: TEventHandler; 
    function _Getonprogress: TEventHandler; 
    function _Getonratechange: TEventHandler; 
    function _Getonreset: TEventHandler; 
    function _Getonresize: TEventHandler; 
    function _Getonscroll: TEventHandler; 
    function _Getonscrollend: TEventHandler; 
    function _Getonsecuritypolicyviolation: TEventHandler; 
    function _Getonseeked: TEventHandler; 
    function _Getonseeking: TEventHandler; 
    function _Getonselect: TEventHandler; 
    function _Getonslotchange: TEventHandler; 
    function _Getonstalled: TEventHandler; 
    function _Getonsubmit: TEventHandler; 
    function _Getonsuspend: TEventHandler; 
    function _Getontimeupdate: TEventHandler; 
    function _Getonvolumechange: TEventHandler; 
    function _Getonwaiting: TEventHandler; 
    function _Getonselectstart: TEventHandler; 
    function _Getonselectionchange: TEventHandler; 
    function _Getontoggle: TEventHandler; 
    function _Getonpointercancel: TEventHandler; 
    function _Getonpointerdown: TEventHandler; 
    function _Getonpointerup: TEventHandler; 
    function _Getonpointermove: TEventHandler; 
    function _Getonpointerout: TEventHandler; 
    function _Getonpointerover: TEventHandler; 
    function _Getonpointerenter: TEventHandler; 
    function _Getonpointerleave: TEventHandler; 
    function _Getongotpointercapture: TEventHandler; 
    function _Getonlostpointercapture: TEventHandler; 
    function _Getonmozfullscreenchange: TEventHandler; 
    function _Getonmozfullscreenerror: TEventHandler; 
    function _Getonanimationcancel: TEventHandler; 
    function _Getonanimationend: TEventHandler; 
    function _Getonanimationiteration: TEventHandler; 
    function _Getonanimationstart: TEventHandler; 
    function _Getontransitioncancel: TEventHandler; 
    function _Getontransitionend: TEventHandler; 
    function _Getontransitionrun: TEventHandler; 
    function _Getontransitionstart: TEventHandler; 
    function _Getonwebkitanimationend: TEventHandler; 
    function _Getonwebkitanimationiteration: TEventHandler; 
    function _Getonwebkitanimationstart: TEventHandler; 
    function _Getonwebkittransitionend: TEventHandler; 
    procedure _Setonabort(const aValue: TEventHandler);
    procedure _Setonblur(const aValue: TEventHandler);
    procedure _Setonfocus(const aValue: TEventHandler);
    procedure _Setoncancel(const aValue: TEventHandler);
    procedure _Setonauxclick(const aValue: TEventHandler);
    procedure _Setonbeforeinput(const aValue: TEventHandler);
    procedure _Setonbeforetoggle(const aValue: TEventHandler);
    procedure _Setoncanplay(const aValue: TEventHandler);
    procedure _Setoncanplaythrough(const aValue: TEventHandler);
    procedure _Setonchange(const aValue: TEventHandler);
    procedure _Setonclick(const aValue: TEventHandler);
    procedure _Setonclose(const aValue: TEventHandler);
    procedure _Setoncontentvisibilityautostatechange(const aValue: TEventHandler);
    procedure _Setoncontextlost(const aValue: TEventHandler);
    procedure _Setoncontextmenu(const aValue: TEventHandler);
    procedure _Setoncontextrestored(const aValue: TEventHandler);
    procedure _Setoncopy(const aValue: TEventHandler);
    procedure _Setoncuechange(const aValue: TEventHandler);
    procedure _Setoncut(const aValue: TEventHandler);
    procedure _Setondblclick(const aValue: TEventHandler);
    procedure _Setondrag(const aValue: TEventHandler);
    procedure _Setondragend(const aValue: TEventHandler);
    procedure _Setondragenter(const aValue: TEventHandler);
    procedure _Setondragexit(const aValue: TEventHandler);
    procedure _Setondragleave(const aValue: TEventHandler);
    procedure _Setondragover(const aValue: TEventHandler);
    procedure _Setondragstart(const aValue: TEventHandler);
    procedure _Setondrop(const aValue: TEventHandler);
    procedure _Setondurationchange(const aValue: TEventHandler);
    procedure _Setonemptied(const aValue: TEventHandler);
    procedure _Setonended(const aValue: TEventHandler);
    procedure _Setonformdata(const aValue: TEventHandler);
    procedure _Setoninput(const aValue: TEventHandler);
    procedure _Setoninvalid(const aValue: TEventHandler);
    procedure _Setonkeydown(const aValue: TEventHandler);
    procedure _Setonkeypress(const aValue: TEventHandler);
    procedure _Setonkeyup(const aValue: TEventHandler);
    procedure _Setonload(const aValue: TEventHandler);
    procedure _Setonloadeddata(const aValue: TEventHandler);
    procedure _Setonloadedmetadata(const aValue: TEventHandler);
    procedure _Setonloadstart(const aValue: TEventHandler);
    procedure _Setonmousedown(const aValue: TEventHandler);
    procedure _Setonmouseenter(const aValue: TEventHandler);
    procedure _Setonmouseleave(const aValue: TEventHandler);
    procedure _Setonmousemove(const aValue: TEventHandler);
    procedure _Setonmouseout(const aValue: TEventHandler);
    procedure _Setonmouseover(const aValue: TEventHandler);
    procedure _Setonmouseup(const aValue: TEventHandler);
    procedure _Setonwheel(const aValue: TEventHandler);
    procedure _Setonpaste(const aValue: TEventHandler);
    procedure _Setonpause(const aValue: TEventHandler);
    procedure _Setonplay(const aValue: TEventHandler);
    procedure _Setonplaying(const aValue: TEventHandler);
    procedure _Setonprogress(const aValue: TEventHandler);
    procedure _Setonratechange(const aValue: TEventHandler);
    procedure _Setonreset(const aValue: TEventHandler);
    procedure _Setonresize(const aValue: TEventHandler);
    procedure _Setonscroll(const aValue: TEventHandler);
    procedure _Setonscrollend(const aValue: TEventHandler);
    procedure _Setonsecuritypolicyviolation(const aValue: TEventHandler);
    procedure _Setonseeked(const aValue: TEventHandler);
    procedure _Setonseeking(const aValue: TEventHandler);
    procedure _Setonselect(const aValue: TEventHandler);
    procedure _Setonslotchange(const aValue: TEventHandler);
    procedure _Setonstalled(const aValue: TEventHandler);
    procedure _Setonsubmit(const aValue: TEventHandler);
    procedure _Setonsuspend(const aValue: TEventHandler);
    procedure _Setontimeupdate(const aValue: TEventHandler);
    procedure _Setonvolumechange(const aValue: TEventHandler);
    procedure _Setonwaiting(const aValue: TEventHandler);
    procedure _Setonselectstart(const aValue: TEventHandler);
    procedure _Setonselectionchange(const aValue: TEventHandler);
    procedure _Setontoggle(const aValue: TEventHandler);
    procedure _Setonpointercancel(const aValue: TEventHandler);
    procedure _Setonpointerdown(const aValue: TEventHandler);
    procedure _Setonpointerup(const aValue: TEventHandler);
    procedure _Setonpointermove(const aValue: TEventHandler);
    procedure _Setonpointerout(const aValue: TEventHandler);
    procedure _Setonpointerover(const aValue: TEventHandler);
    procedure _Setonpointerenter(const aValue: TEventHandler);
    procedure _Setonpointerleave(const aValue: TEventHandler);
    procedure _Setongotpointercapture(const aValue: TEventHandler);
    procedure _Setonlostpointercapture(const aValue: TEventHandler);
    procedure _Setonmozfullscreenchange(const aValue: TEventHandler);
    procedure _Setonmozfullscreenerror(const aValue: TEventHandler);
    procedure _Setonanimationcancel(const aValue: TEventHandler);
    procedure _Setonanimationend(const aValue: TEventHandler);
    procedure _Setonanimationiteration(const aValue: TEventHandler);
    procedure _Setonanimationstart(const aValue: TEventHandler);
    procedure _Setontransitioncancel(const aValue: TEventHandler);
    procedure _Setontransitionend(const aValue: TEventHandler);
    procedure _Setontransitionrun(const aValue: TEventHandler);
    procedure _Setontransitionstart(const aValue: TEventHandler);
    procedure _Setonwebkitanimationend(const aValue: TEventHandler);
    procedure _Setonwebkitanimationiteration(const aValue: TEventHandler);
    procedure _Setonwebkitanimationstart(const aValue: TEventHandler);
    procedure _Setonwebkittransitionend(const aValue: TEventHandler);
    property onabort: TEventHandler read _Getonabort write _Setonabort;
    property onblur: TEventHandler read _Getonblur write _Setonblur;
    property onfocus: TEventHandler read _Getonfocus write _Setonfocus;
    property oncancel: TEventHandler read _Getoncancel write _Setoncancel;
    property onauxclick: TEventHandler read _Getonauxclick write _Setonauxclick;
    property onbeforeinput: TEventHandler read _Getonbeforeinput write _Setonbeforeinput;
    property onbeforetoggle: TEventHandler read _Getonbeforetoggle write _Setonbeforetoggle;
    property oncanplay: TEventHandler read _Getoncanplay write _Setoncanplay;
    property oncanplaythrough: TEventHandler read _Getoncanplaythrough write _Setoncanplaythrough;
    property onchange: TEventHandler read _Getonchange write _Setonchange;
    property onclick: TEventHandler read _Getonclick write _Setonclick;
    property onclose: TEventHandler read _Getonclose write _Setonclose;
    property oncontentvisibilityautostatechange: TEventHandler read _Getoncontentvisibilityautostatechange write _Setoncontentvisibilityautostatechange;
    property oncontextlost: TEventHandler read _Getoncontextlost write _Setoncontextlost;
    property oncontextmenu: TEventHandler read _Getoncontextmenu write _Setoncontextmenu;
    property oncontextrestored: TEventHandler read _Getoncontextrestored write _Setoncontextrestored;
    property oncopy: TEventHandler read _Getoncopy write _Setoncopy;
    property oncuechange: TEventHandler read _Getoncuechange write _Setoncuechange;
    property oncut: TEventHandler read _Getoncut write _Setoncut;
    property ondblclick: TEventHandler read _Getondblclick write _Setondblclick;
    property ondrag: TEventHandler read _Getondrag write _Setondrag;
    property ondragend: TEventHandler read _Getondragend write _Setondragend;
    property ondragenter: TEventHandler read _Getondragenter write _Setondragenter;
    property ondragexit: TEventHandler read _Getondragexit write _Setondragexit;
    property ondragleave: TEventHandler read _Getondragleave write _Setondragleave;
    property ondragover: TEventHandler read _Getondragover write _Setondragover;
    property ondragstart: TEventHandler read _Getondragstart write _Setondragstart;
    property ondrop: TEventHandler read _Getondrop write _Setondrop;
    property ondurationchange: TEventHandler read _Getondurationchange write _Setondurationchange;
    property onemptied: TEventHandler read _Getonemptied write _Setonemptied;
    property onended: TEventHandler read _Getonended write _Setonended;
    property onformdata: TEventHandler read _Getonformdata write _Setonformdata;
    property oninput: TEventHandler read _Getoninput write _Setoninput;
    property oninvalid: TEventHandler read _Getoninvalid write _Setoninvalid;
    property onkeydown: TEventHandler read _Getonkeydown write _Setonkeydown;
    property onkeypress: TEventHandler read _Getonkeypress write _Setonkeypress;
    property onkeyup: TEventHandler read _Getonkeyup write _Setonkeyup;
    property onload: TEventHandler read _Getonload write _Setonload;
    property onloadeddata: TEventHandler read _Getonloadeddata write _Setonloadeddata;
    property onloadedmetadata: TEventHandler read _Getonloadedmetadata write _Setonloadedmetadata;
    property onloadstart: TEventHandler read _Getonloadstart write _Setonloadstart;
    property onmousedown: TEventHandler read _Getonmousedown write _Setonmousedown;
    property onmouseenter: TEventHandler read _Getonmouseenter write _Setonmouseenter;
    property onmouseleave: TEventHandler read _Getonmouseleave write _Setonmouseleave;
    property onmousemove: TEventHandler read _Getonmousemove write _Setonmousemove;
    property onmouseout: TEventHandler read _Getonmouseout write _Setonmouseout;
    property onmouseover: TEventHandler read _Getonmouseover write _Setonmouseover;
    property onmouseup: TEventHandler read _Getonmouseup write _Setonmouseup;
    property onwheel: TEventHandler read _Getonwheel write _Setonwheel;
    property onpaste: TEventHandler read _Getonpaste write _Setonpaste;
    property onpause: TEventHandler read _Getonpause write _Setonpause;
    property onplay: TEventHandler read _Getonplay write _Setonplay;
    property onplaying: TEventHandler read _Getonplaying write _Setonplaying;
    property onprogress: TEventHandler read _Getonprogress write _Setonprogress;
    property onratechange: TEventHandler read _Getonratechange write _Setonratechange;
    property onreset: TEventHandler read _Getonreset write _Setonreset;
    property onresize: TEventHandler read _Getonresize write _Setonresize;
    property onscroll: TEventHandler read _Getonscroll write _Setonscroll;
    property onscrollend: TEventHandler read _Getonscrollend write _Setonscrollend;
    property onsecuritypolicyviolation: TEventHandler read _Getonsecuritypolicyviolation write _Setonsecuritypolicyviolation;
    property onseeked: TEventHandler read _Getonseeked write _Setonseeked;
    property onseeking: TEventHandler read _Getonseeking write _Setonseeking;
    property onselect: TEventHandler read _Getonselect write _Setonselect;
    property onslotchange: TEventHandler read _Getonslotchange write _Setonslotchange;
    property onstalled: TEventHandler read _Getonstalled write _Setonstalled;
    property onsubmit: TEventHandler read _Getonsubmit write _Setonsubmit;
    property onsuspend: TEventHandler read _Getonsuspend write _Setonsuspend;
    property ontimeupdate: TEventHandler read _Getontimeupdate write _Setontimeupdate;
    property onvolumechange: TEventHandler read _Getonvolumechange write _Setonvolumechange;
    property onwaiting: TEventHandler read _Getonwaiting write _Setonwaiting;
    property onselectstart: TEventHandler read _Getonselectstart write _Setonselectstart;
    property onselectionchange: TEventHandler read _Getonselectionchange write _Setonselectionchange;
    property ontoggle: TEventHandler read _Getontoggle write _Setontoggle;
    property onpointercancel: TEventHandler read _Getonpointercancel write _Setonpointercancel;
    property onpointerdown: TEventHandler read _Getonpointerdown write _Setonpointerdown;
    property onpointerup: TEventHandler read _Getonpointerup write _Setonpointerup;
    property onpointermove: TEventHandler read _Getonpointermove write _Setonpointermove;
    property onpointerout: TEventHandler read _Getonpointerout write _Setonpointerout;
    property onpointerover: TEventHandler read _Getonpointerover write _Setonpointerover;
    property onpointerenter: TEventHandler read _Getonpointerenter write _Setonpointerenter;
    property onpointerleave: TEventHandler read _Getonpointerleave write _Setonpointerleave;
    property ongotpointercapture: TEventHandler read _Getongotpointercapture write _Setongotpointercapture;
    property onlostpointercapture: TEventHandler read _Getonlostpointercapture write _Setonlostpointercapture;
    property onmozfullscreenchange: TEventHandler read _Getonmozfullscreenchange write _Setonmozfullscreenchange;
    property onmozfullscreenerror: TEventHandler read _Getonmozfullscreenerror write _Setonmozfullscreenerror;
    property onanimationcancel: TEventHandler read _Getonanimationcancel write _Setonanimationcancel;
    property onanimationend: TEventHandler read _Getonanimationend write _Setonanimationend;
    property onanimationiteration: TEventHandler read _Getonanimationiteration write _Setonanimationiteration;
    property onanimationstart: TEventHandler read _Getonanimationstart write _Setonanimationstart;
    property ontransitioncancel: TEventHandler read _Getontransitioncancel write _Setontransitioncancel;
    property ontransitionend: TEventHandler read _Getontransitionend write _Setontransitionend;
    property ontransitionrun: TEventHandler read _Getontransitionrun write _Setontransitionrun;
    property ontransitionstart: TEventHandler read _Getontransitionstart write _Setontransitionstart;
    property onwebkitanimationend: TEventHandler read _Getonwebkitanimationend write _Setonwebkitanimationend;
    property onwebkitanimationiteration: TEventHandler read _Getonwebkitanimationiteration write _Setonwebkitanimationiteration;
    property onwebkitanimationstart: TEventHandler read _Getonwebkitanimationstart write _Setonwebkitanimationstart;
    property onwebkittransitionend: TEventHandler read _Getonwebkittransitionend write _Setonwebkittransitionend;
  end;
  
  TJSGlobalEventHandlers = class(TJSObject,IJSGlobalEventHandlers)
  Private
  Protected
    function _Getonabort: TEventHandler; 
    function _Getonblur: TEventHandler; 
    function _Getonfocus: TEventHandler; 
    function _Getoncancel: TEventHandler; 
    function _Getonauxclick: TEventHandler; 
    function _Getonbeforeinput: TEventHandler; 
    function _Getonbeforetoggle: TEventHandler; 
    function _Getoncanplay: TEventHandler; 
    function _Getoncanplaythrough: TEventHandler; 
    function _Getonchange: TEventHandler; 
    function _Getonclick: TEventHandler; 
    function _Getonclose: TEventHandler; 
    function _Getoncontentvisibilityautostatechange: TEventHandler; 
    function _Getoncontextlost: TEventHandler; 
    function _Getoncontextmenu: TEventHandler; 
    function _Getoncontextrestored: TEventHandler; 
    function _Getoncopy: TEventHandler; 
    function _Getoncuechange: TEventHandler; 
    function _Getoncut: TEventHandler; 
    function _Getondblclick: TEventHandler; 
    function _Getondrag: TEventHandler; 
    function _Getondragend: TEventHandler; 
    function _Getondragenter: TEventHandler; 
    function _Getondragexit: TEventHandler; 
    function _Getondragleave: TEventHandler; 
    function _Getondragover: TEventHandler; 
    function _Getondragstart: TEventHandler; 
    function _Getondrop: TEventHandler; 
    function _Getondurationchange: TEventHandler; 
    function _Getonemptied: TEventHandler; 
    function _Getonended: TEventHandler; 
    function _Getonformdata: TEventHandler; 
    function _Getoninput: TEventHandler; 
    function _Getoninvalid: TEventHandler; 
    function _Getonkeydown: TEventHandler; 
    function _Getonkeypress: TEventHandler; 
    function _Getonkeyup: TEventHandler; 
    function _Getonload: TEventHandler; 
    function _Getonloadeddata: TEventHandler; 
    function _Getonloadedmetadata: TEventHandler; 
    function _Getonloadstart: TEventHandler; 
    function _Getonmousedown: TEventHandler; 
    function _Getonmouseenter: TEventHandler; 
    function _Getonmouseleave: TEventHandler; 
    function _Getonmousemove: TEventHandler; 
    function _Getonmouseout: TEventHandler; 
    function _Getonmouseover: TEventHandler; 
    function _Getonmouseup: TEventHandler; 
    function _Getonwheel: TEventHandler; 
    function _Getonpaste: TEventHandler; 
    function _Getonpause: TEventHandler; 
    function _Getonplay: TEventHandler; 
    function _Getonplaying: TEventHandler; 
    function _Getonprogress: TEventHandler; 
    function _Getonratechange: TEventHandler; 
    function _Getonreset: TEventHandler; 
    function _Getonresize: TEventHandler; 
    function _Getonscroll: TEventHandler; 
    function _Getonscrollend: TEventHandler; 
    function _Getonsecuritypolicyviolation: TEventHandler; 
    function _Getonseeked: TEventHandler; 
    function _Getonseeking: TEventHandler; 
    function _Getonselect: TEventHandler; 
    function _Getonslotchange: TEventHandler; 
    function _Getonstalled: TEventHandler; 
    function _Getonsubmit: TEventHandler; 
    function _Getonsuspend: TEventHandler; 
    function _Getontimeupdate: TEventHandler; 
    function _Getonvolumechange: TEventHandler; 
    function _Getonwaiting: TEventHandler; 
    function _Getonselectstart: TEventHandler; 
    function _Getonselectionchange: TEventHandler; 
    function _Getontoggle: TEventHandler; 
    function _Getonpointercancel: TEventHandler; 
    function _Getonpointerdown: TEventHandler; 
    function _Getonpointerup: TEventHandler; 
    function _Getonpointermove: TEventHandler; 
    function _Getonpointerout: TEventHandler; 
    function _Getonpointerover: TEventHandler; 
    function _Getonpointerenter: TEventHandler; 
    function _Getonpointerleave: TEventHandler; 
    function _Getongotpointercapture: TEventHandler; 
    function _Getonlostpointercapture: TEventHandler; 
    function _Getonmozfullscreenchange: TEventHandler; 
    function _Getonmozfullscreenerror: TEventHandler; 
    function _Getonanimationcancel: TEventHandler; 
    function _Getonanimationend: TEventHandler; 
    function _Getonanimationiteration: TEventHandler; 
    function _Getonanimationstart: TEventHandler; 
    function _Getontransitioncancel: TEventHandler; 
    function _Getontransitionend: TEventHandler; 
    function _Getontransitionrun: TEventHandler; 
    function _Getontransitionstart: TEventHandler; 
    function _Getonwebkitanimationend: TEventHandler; 
    function _Getonwebkitanimationiteration: TEventHandler; 
    function _Getonwebkitanimationstart: TEventHandler; 
    function _Getonwebkittransitionend: TEventHandler; 
    procedure _Setonabort(const aValue: TEventHandler);
    procedure _Setonblur(const aValue: TEventHandler);
    procedure _Setonfocus(const aValue: TEventHandler);
    procedure _Setoncancel(const aValue: TEventHandler);
    procedure _Setonauxclick(const aValue: TEventHandler);
    procedure _Setonbeforeinput(const aValue: TEventHandler);
    procedure _Setonbeforetoggle(const aValue: TEventHandler);
    procedure _Setoncanplay(const aValue: TEventHandler);
    procedure _Setoncanplaythrough(const aValue: TEventHandler);
    procedure _Setonchange(const aValue: TEventHandler);
    procedure _Setonclick(const aValue: TEventHandler);
    procedure _Setonclose(const aValue: TEventHandler);
    procedure _Setoncontentvisibilityautostatechange(const aValue: TEventHandler);
    procedure _Setoncontextlost(const aValue: TEventHandler);
    procedure _Setoncontextmenu(const aValue: TEventHandler);
    procedure _Setoncontextrestored(const aValue: TEventHandler);
    procedure _Setoncopy(const aValue: TEventHandler);
    procedure _Setoncuechange(const aValue: TEventHandler);
    procedure _Setoncut(const aValue: TEventHandler);
    procedure _Setondblclick(const aValue: TEventHandler);
    procedure _Setondrag(const aValue: TEventHandler);
    procedure _Setondragend(const aValue: TEventHandler);
    procedure _Setondragenter(const aValue: TEventHandler);
    procedure _Setondragexit(const aValue: TEventHandler);
    procedure _Setondragleave(const aValue: TEventHandler);
    procedure _Setondragover(const aValue: TEventHandler);
    procedure _Setondragstart(const aValue: TEventHandler);
    procedure _Setondrop(const aValue: TEventHandler);
    procedure _Setondurationchange(const aValue: TEventHandler);
    procedure _Setonemptied(const aValue: TEventHandler);
    procedure _Setonended(const aValue: TEventHandler);
    procedure _Setonformdata(const aValue: TEventHandler);
    procedure _Setoninput(const aValue: TEventHandler);
    procedure _Setoninvalid(const aValue: TEventHandler);
    procedure _Setonkeydown(const aValue: TEventHandler);
    procedure _Setonkeypress(const aValue: TEventHandler);
    procedure _Setonkeyup(const aValue: TEventHandler);
    procedure _Setonload(const aValue: TEventHandler);
    procedure _Setonloadeddata(const aValue: TEventHandler);
    procedure _Setonloadedmetadata(const aValue: TEventHandler);
    procedure _Setonloadstart(const aValue: TEventHandler);
    procedure _Setonmousedown(const aValue: TEventHandler);
    procedure _Setonmouseenter(const aValue: TEventHandler);
    procedure _Setonmouseleave(const aValue: TEventHandler);
    procedure _Setonmousemove(const aValue: TEventHandler);
    procedure _Setonmouseout(const aValue: TEventHandler);
    procedure _Setonmouseover(const aValue: TEventHandler);
    procedure _Setonmouseup(const aValue: TEventHandler);
    procedure _Setonwheel(const aValue: TEventHandler);
    procedure _Setonpaste(const aValue: TEventHandler);
    procedure _Setonpause(const aValue: TEventHandler);
    procedure _Setonplay(const aValue: TEventHandler);
    procedure _Setonplaying(const aValue: TEventHandler);
    procedure _Setonprogress(const aValue: TEventHandler);
    procedure _Setonratechange(const aValue: TEventHandler);
    procedure _Setonreset(const aValue: TEventHandler);
    procedure _Setonresize(const aValue: TEventHandler);
    procedure _Setonscroll(const aValue: TEventHandler);
    procedure _Setonscrollend(const aValue: TEventHandler);
    procedure _Setonsecuritypolicyviolation(const aValue: TEventHandler);
    procedure _Setonseeked(const aValue: TEventHandler);
    procedure _Setonseeking(const aValue: TEventHandler);
    procedure _Setonselect(const aValue: TEventHandler);
    procedure _Setonslotchange(const aValue: TEventHandler);
    procedure _Setonstalled(const aValue: TEventHandler);
    procedure _Setonsubmit(const aValue: TEventHandler);
    procedure _Setonsuspend(const aValue: TEventHandler);
    procedure _Setontimeupdate(const aValue: TEventHandler);
    procedure _Setonvolumechange(const aValue: TEventHandler);
    procedure _Setonwaiting(const aValue: TEventHandler);
    procedure _Setonselectstart(const aValue: TEventHandler);
    procedure _Setonselectionchange(const aValue: TEventHandler);
    procedure _Setontoggle(const aValue: TEventHandler);
    procedure _Setonpointercancel(const aValue: TEventHandler);
    procedure _Setonpointerdown(const aValue: TEventHandler);
    procedure _Setonpointerup(const aValue: TEventHandler);
    procedure _Setonpointermove(const aValue: TEventHandler);
    procedure _Setonpointerout(const aValue: TEventHandler);
    procedure _Setonpointerover(const aValue: TEventHandler);
    procedure _Setonpointerenter(const aValue: TEventHandler);
    procedure _Setonpointerleave(const aValue: TEventHandler);
    procedure _Setongotpointercapture(const aValue: TEventHandler);
    procedure _Setonlostpointercapture(const aValue: TEventHandler);
    procedure _Setonmozfullscreenchange(const aValue: TEventHandler);
    procedure _Setonmozfullscreenerror(const aValue: TEventHandler);
    procedure _Setonanimationcancel(const aValue: TEventHandler);
    procedure _Setonanimationend(const aValue: TEventHandler);
    procedure _Setonanimationiteration(const aValue: TEventHandler);
    procedure _Setonanimationstart(const aValue: TEventHandler);
    procedure _Setontransitioncancel(const aValue: TEventHandler);
    procedure _Setontransitionend(const aValue: TEventHandler);
    procedure _Setontransitionrun(const aValue: TEventHandler);
    procedure _Setontransitionstart(const aValue: TEventHandler);
    procedure _Setonwebkitanimationend(const aValue: TEventHandler);
    procedure _Setonwebkitanimationiteration(const aValue: TEventHandler);
    procedure _Setonwebkitanimationstart(const aValue: TEventHandler);
    procedure _Setonwebkittransitionend(const aValue: TEventHandler);
  Public
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSGlobalEventHandlers;
    property onabort: TEventHandler read _Getonabort write _Setonabort;
    property onblur: TEventHandler read _Getonblur write _Setonblur;
    property onfocus: TEventHandler read _Getonfocus write _Setonfocus;
    property oncancel: TEventHandler read _Getoncancel write _Setoncancel;
    property onauxclick: TEventHandler read _Getonauxclick write _Setonauxclick;
    property onbeforeinput: TEventHandler read _Getonbeforeinput write _Setonbeforeinput;
    property onbeforetoggle: TEventHandler read _Getonbeforetoggle write _Setonbeforetoggle;
    property oncanplay: TEventHandler read _Getoncanplay write _Setoncanplay;
    property oncanplaythrough: TEventHandler read _Getoncanplaythrough write _Setoncanplaythrough;
    property onchange: TEventHandler read _Getonchange write _Setonchange;
    property onclick: TEventHandler read _Getonclick write _Setonclick;
    property onclose: TEventHandler read _Getonclose write _Setonclose;
    property oncontentvisibilityautostatechange: TEventHandler read _Getoncontentvisibilityautostatechange write _Setoncontentvisibilityautostatechange;
    property oncontextlost: TEventHandler read _Getoncontextlost write _Setoncontextlost;
    property oncontextmenu: TEventHandler read _Getoncontextmenu write _Setoncontextmenu;
    property oncontextrestored: TEventHandler read _Getoncontextrestored write _Setoncontextrestored;
    property oncopy: TEventHandler read _Getoncopy write _Setoncopy;
    property oncuechange: TEventHandler read _Getoncuechange write _Setoncuechange;
    property oncut: TEventHandler read _Getoncut write _Setoncut;
    property ondblclick: TEventHandler read _Getondblclick write _Setondblclick;
    property ondrag: TEventHandler read _Getondrag write _Setondrag;
    property ondragend: TEventHandler read _Getondragend write _Setondragend;
    property ondragenter: TEventHandler read _Getondragenter write _Setondragenter;
    property ondragexit: TEventHandler read _Getondragexit write _Setondragexit;
    property ondragleave: TEventHandler read _Getondragleave write _Setondragleave;
    property ondragover: TEventHandler read _Getondragover write _Setondragover;
    property ondragstart: TEventHandler read _Getondragstart write _Setondragstart;
    property ondrop: TEventHandler read _Getondrop write _Setondrop;
    property ondurationchange: TEventHandler read _Getondurationchange write _Setondurationchange;
    property onemptied: TEventHandler read _Getonemptied write _Setonemptied;
    property onended: TEventHandler read _Getonended write _Setonended;
    property onformdata: TEventHandler read _Getonformdata write _Setonformdata;
    property oninput: TEventHandler read _Getoninput write _Setoninput;
    property oninvalid: TEventHandler read _Getoninvalid write _Setoninvalid;
    property onkeydown: TEventHandler read _Getonkeydown write _Setonkeydown;
    property onkeypress: TEventHandler read _Getonkeypress write _Setonkeypress;
    property onkeyup: TEventHandler read _Getonkeyup write _Setonkeyup;
    property onload: TEventHandler read _Getonload write _Setonload;
    property onloadeddata: TEventHandler read _Getonloadeddata write _Setonloadeddata;
    property onloadedmetadata: TEventHandler read _Getonloadedmetadata write _Setonloadedmetadata;
    property onloadstart: TEventHandler read _Getonloadstart write _Setonloadstart;
    property onmousedown: TEventHandler read _Getonmousedown write _Setonmousedown;
    property onmouseenter: TEventHandler read _Getonmouseenter write _Setonmouseenter;
    property onmouseleave: TEventHandler read _Getonmouseleave write _Setonmouseleave;
    property onmousemove: TEventHandler read _Getonmousemove write _Setonmousemove;
    property onmouseout: TEventHandler read _Getonmouseout write _Setonmouseout;
    property onmouseover: TEventHandler read _Getonmouseover write _Setonmouseover;
    property onmouseup: TEventHandler read _Getonmouseup write _Setonmouseup;
    property onwheel: TEventHandler read _Getonwheel write _Setonwheel;
    property onpaste: TEventHandler read _Getonpaste write _Setonpaste;
    property onpause: TEventHandler read _Getonpause write _Setonpause;
    property onplay: TEventHandler read _Getonplay write _Setonplay;
    property onplaying: TEventHandler read _Getonplaying write _Setonplaying;
    property onprogress: TEventHandler read _Getonprogress write _Setonprogress;
    property onratechange: TEventHandler read _Getonratechange write _Setonratechange;
    property onreset: TEventHandler read _Getonreset write _Setonreset;
    property onresize: TEventHandler read _Getonresize write _Setonresize;
    property onscroll: TEventHandler read _Getonscroll write _Setonscroll;
    property onscrollend: TEventHandler read _Getonscrollend write _Setonscrollend;
    property onsecuritypolicyviolation: TEventHandler read _Getonsecuritypolicyviolation write _Setonsecuritypolicyviolation;
    property onseeked: TEventHandler read _Getonseeked write _Setonseeked;
    property onseeking: TEventHandler read _Getonseeking write _Setonseeking;
    property onselect: TEventHandler read _Getonselect write _Setonselect;
    property onslotchange: TEventHandler read _Getonslotchange write _Setonslotchange;
    property onstalled: TEventHandler read _Getonstalled write _Setonstalled;
    property onsubmit: TEventHandler read _Getonsubmit write _Setonsubmit;
    property onsuspend: TEventHandler read _Getonsuspend write _Setonsuspend;
    property ontimeupdate: TEventHandler read _Getontimeupdate write _Setontimeupdate;
    property onvolumechange: TEventHandler read _Getonvolumechange write _Setonvolumechange;
    property onwaiting: TEventHandler read _Getonwaiting write _Setonwaiting;
    property onselectstart: TEventHandler read _Getonselectstart write _Setonselectstart;
    property onselectionchange: TEventHandler read _Getonselectionchange write _Setonselectionchange;
    property ontoggle: TEventHandler read _Getontoggle write _Setontoggle;
    property onpointercancel: TEventHandler read _Getonpointercancel write _Setonpointercancel;
    property onpointerdown: TEventHandler read _Getonpointerdown write _Setonpointerdown;
    property onpointerup: TEventHandler read _Getonpointerup write _Setonpointerup;
    property onpointermove: TEventHandler read _Getonpointermove write _Setonpointermove;
    property onpointerout: TEventHandler read _Getonpointerout write _Setonpointerout;
    property onpointerover: TEventHandler read _Getonpointerover write _Setonpointerover;
    property onpointerenter: TEventHandler read _Getonpointerenter write _Setonpointerenter;
    property onpointerleave: TEventHandler read _Getonpointerleave write _Setonpointerleave;
    property ongotpointercapture: TEventHandler read _Getongotpointercapture write _Setongotpointercapture;
    property onlostpointercapture: TEventHandler read _Getonlostpointercapture write _Setonlostpointercapture;
    property onmozfullscreenchange: TEventHandler read _Getonmozfullscreenchange write _Setonmozfullscreenchange;
    property onmozfullscreenerror: TEventHandler read _Getonmozfullscreenerror write _Setonmozfullscreenerror;
    property onanimationcancel: TEventHandler read _Getonanimationcancel write _Setonanimationcancel;
    property onanimationend: TEventHandler read _Getonanimationend write _Setonanimationend;
    property onanimationiteration: TEventHandler read _Getonanimationiteration write _Setonanimationiteration;
    property onanimationstart: TEventHandler read _Getonanimationstart write _Setonanimationstart;
    property ontransitioncancel: TEventHandler read _Getontransitioncancel write _Setontransitioncancel;
    property ontransitionend: TEventHandler read _Getontransitionend write _Setontransitionend;
    property ontransitionrun: TEventHandler read _Getontransitionrun write _Setontransitionrun;
    property ontransitionstart: TEventHandler read _Getontransitionstart write _Setontransitionstart;
    property onwebkitanimationend: TEventHandler read _Getonwebkitanimationend write _Setonwebkitanimationend;
    property onwebkitanimationiteration: TEventHandler read _Getonwebkitanimationiteration write _Setonwebkitanimationiteration;
    property onwebkitanimationstart: TEventHandler read _Getonwebkitanimationstart write _Setonwebkitanimationstart;
    property onwebkittransitionend: TEventHandler read _Getonwebkittransitionend write _Setonwebkittransitionend;
  end;
  
  { --------------------------------------------------------------------
    TJSWindowEventHandlers
    --------------------------------------------------------------------}
  
  IJSWindowEventHandlers = interface(IJSObject)
    ['{59259445-2E1D-3B8D-AD27-083C0E2E8A69}']
    function _Getonafterprint: TEventHandler; 
    function _Getonbeforeprint: TEventHandler; 
    function _Getonbeforeunload: TOnBeforeUnloadEventHandler; 
    function _Getonhashchange: TEventHandler; 
    function _Getonlanguagechange: TEventHandler; 
    function _Getonmessage: TEventHandler; 
    function _Getonmessageerror: TEventHandler; 
    function _Getonoffline: TEventHandler; 
    function _Getononline: TEventHandler; 
    function _Getonpagehide: TEventHandler; 
    function _Getonpageshow: TEventHandler; 
    function _Getonpopstate: TEventHandler; 
    function _Getonrejectionhandled: TEventHandler; 
    function _Getonstorage: TEventHandler; 
    function _Getonunhandledrejection: TEventHandler; 
    function _Getonunload: TEventHandler; 
    function _Getongamepadconnected: TEventHandler; 
    function _Getongamepaddisconnected: TEventHandler; 
    procedure _Setonafterprint(const aValue: TEventHandler);
    procedure _Setonbeforeprint(const aValue: TEventHandler);
    procedure _Setonbeforeunload(const aValue: TOnBeforeUnloadEventHandler);
    procedure _Setonhashchange(const aValue: TEventHandler);
    procedure _Setonlanguagechange(const aValue: TEventHandler);
    procedure _Setonmessage(const aValue: TEventHandler);
    procedure _Setonmessageerror(const aValue: TEventHandler);
    procedure _Setonoffline(const aValue: TEventHandler);
    procedure _Setononline(const aValue: TEventHandler);
    procedure _Setonpagehide(const aValue: TEventHandler);
    procedure _Setonpageshow(const aValue: TEventHandler);
    procedure _Setonpopstate(const aValue: TEventHandler);
    procedure _Setonrejectionhandled(const aValue: TEventHandler);
    procedure _Setonstorage(const aValue: TEventHandler);
    procedure _Setonunhandledrejection(const aValue: TEventHandler);
    procedure _Setonunload(const aValue: TEventHandler);
    procedure _Setongamepadconnected(const aValue: TEventHandler);
    procedure _Setongamepaddisconnected(const aValue: TEventHandler);
    property onafterprint: TEventHandler read _Getonafterprint write _Setonafterprint;
    property onbeforeprint: TEventHandler read _Getonbeforeprint write _Setonbeforeprint;
    property onbeforeunload: TOnBeforeUnloadEventHandler read _Getonbeforeunload write _Setonbeforeunload;
    property onhashchange: TEventHandler read _Getonhashchange write _Setonhashchange;
    property onlanguagechange: TEventHandler read _Getonlanguagechange write _Setonlanguagechange;
    property onmessage: TEventHandler read _Getonmessage write _Setonmessage;
    property onmessageerror: TEventHandler read _Getonmessageerror write _Setonmessageerror;
    property onoffline: TEventHandler read _Getonoffline write _Setonoffline;
    property ononline: TEventHandler read _Getononline write _Setononline;
    property onpagehide: TEventHandler read _Getonpagehide write _Setonpagehide;
    property onpageshow: TEventHandler read _Getonpageshow write _Setonpageshow;
    property onpopstate: TEventHandler read _Getonpopstate write _Setonpopstate;
    property onrejectionhandled: TEventHandler read _Getonrejectionhandled write _Setonrejectionhandled;
    property onstorage: TEventHandler read _Getonstorage write _Setonstorage;
    property onunhandledrejection: TEventHandler read _Getonunhandledrejection write _Setonunhandledrejection;
    property onunload: TEventHandler read _Getonunload write _Setonunload;
    property ongamepadconnected: TEventHandler read _Getongamepadconnected write _Setongamepadconnected;
    property ongamepaddisconnected: TEventHandler read _Getongamepaddisconnected write _Setongamepaddisconnected;
  end;
  
  TJSWindowEventHandlers = class(TJSObject,IJSWindowEventHandlers)
  Private
  Protected
    function _Getonafterprint: TEventHandler; 
    function _Getonbeforeprint: TEventHandler; 
    function _Getonbeforeunload: TOnBeforeUnloadEventHandler; 
    function _Getonhashchange: TEventHandler; 
    function _Getonlanguagechange: TEventHandler; 
    function _Getonmessage: TEventHandler; 
    function _Getonmessageerror: TEventHandler; 
    function _Getonoffline: TEventHandler; 
    function _Getononline: TEventHandler; 
    function _Getonpagehide: TEventHandler; 
    function _Getonpageshow: TEventHandler; 
    function _Getonpopstate: TEventHandler; 
    function _Getonrejectionhandled: TEventHandler; 
    function _Getonstorage: TEventHandler; 
    function _Getonunhandledrejection: TEventHandler; 
    function _Getonunload: TEventHandler; 
    function _Getongamepadconnected: TEventHandler; 
    function _Getongamepaddisconnected: TEventHandler; 
    procedure _Setonafterprint(const aValue: TEventHandler);
    procedure _Setonbeforeprint(const aValue: TEventHandler);
    procedure _Setonbeforeunload(const aValue: TOnBeforeUnloadEventHandler);
    procedure _Setonhashchange(const aValue: TEventHandler);
    procedure _Setonlanguagechange(const aValue: TEventHandler);
    procedure _Setonmessage(const aValue: TEventHandler);
    procedure _Setonmessageerror(const aValue: TEventHandler);
    procedure _Setonoffline(const aValue: TEventHandler);
    procedure _Setononline(const aValue: TEventHandler);
    procedure _Setonpagehide(const aValue: TEventHandler);
    procedure _Setonpageshow(const aValue: TEventHandler);
    procedure _Setonpopstate(const aValue: TEventHandler);
    procedure _Setonrejectionhandled(const aValue: TEventHandler);
    procedure _Setonstorage(const aValue: TEventHandler);
    procedure _Setonunhandledrejection(const aValue: TEventHandler);
    procedure _Setonunload(const aValue: TEventHandler);
    procedure _Setongamepadconnected(const aValue: TEventHandler);
    procedure _Setongamepaddisconnected(const aValue: TEventHandler);
  Public
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSWindowEventHandlers;
    property onafterprint: TEventHandler read _Getonafterprint write _Setonafterprint;
    property onbeforeprint: TEventHandler read _Getonbeforeprint write _Setonbeforeprint;
    property onbeforeunload: TOnBeforeUnloadEventHandler read _Getonbeforeunload write _Setonbeforeunload;
    property onhashchange: TEventHandler read _Getonhashchange write _Setonhashchange;
    property onlanguagechange: TEventHandler read _Getonlanguagechange write _Setonlanguagechange;
    property onmessage: TEventHandler read _Getonmessage write _Setonmessage;
    property onmessageerror: TEventHandler read _Getonmessageerror write _Setonmessageerror;
    property onoffline: TEventHandler read _Getonoffline write _Setonoffline;
    property ononline: TEventHandler read _Getononline write _Setononline;
    property onpagehide: TEventHandler read _Getonpagehide write _Setonpagehide;
    property onpageshow: TEventHandler read _Getonpageshow write _Setonpageshow;
    property onpopstate: TEventHandler read _Getonpopstate write _Setonpopstate;
    property onrejectionhandled: TEventHandler read _Getonrejectionhandled write _Setonrejectionhandled;
    property onstorage: TEventHandler read _Getonstorage write _Setonstorage;
    property onunhandledrejection: TEventHandler read _Getonunhandledrejection write _Setonunhandledrejection;
    property onunload: TEventHandler read _Getonunload write _Setonunload;
    property ongamepadconnected: TEventHandler read _Getongamepadconnected write _Setongamepadconnected;
    property ongamepaddisconnected: TEventHandler read _Getongamepaddisconnected write _Setongamepaddisconnected;
  end;
  
  { --------------------------------------------------------------------
    TJSOnErrorEventHandlerForNodes
    --------------------------------------------------------------------}
  
  IJSOnErrorEventHandlerForNodes = interface(IJSObject)
    ['{13D166E9-F70F-346C-B11C-1B956C0BBCB0}']
    function _Getonerror: TEventHandler; 
    procedure _Setonerror(const aValue: TEventHandler);
    property onerror: TEventHandler read _Getonerror write _Setonerror;
  end;
  
  TJSOnErrorEventHandlerForNodes = class(TJSObject,IJSOnErrorEventHandlerForNodes)
  Private
  Protected
    function _Getonerror: TEventHandler; 
    procedure _Setonerror(const aValue: TEventHandler);
  Public
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSOnErrorEventHandlerForNodes;
    property onerror: TEventHandler read _Getonerror write _Setonerror;
  end;
  
  { --------------------------------------------------------------------
    TJSOnErrorEventHandlerForWindow
    --------------------------------------------------------------------}
  
  IJSOnErrorEventHandlerForWindow = interface(IJSObject)
    ['{EF0A17DA-F3C5-35AE-80F9-08BD29C87E5B}']
    function _Getonerror: TOnErrorEventHandler; 
    procedure _Setonerror(const aValue: TOnErrorEventHandler);
    property onerror: TOnErrorEventHandler read _Getonerror write _Setonerror;
  end;
  
  TJSOnErrorEventHandlerForWindow = class(TJSObject,IJSOnErrorEventHandlerForWindow)
  Private
  Protected
    function _Getonerror: TOnErrorEventHandler; 
    procedure _Setonerror(const aValue: TOnErrorEventHandler);
  Public
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSOnErrorEventHandlerForWindow;
    property onerror: TOnErrorEventHandler read _Getonerror write _Setonerror;
  end;
  
  { --------------------------------------------------------------------
    TJSEventTarget
    --------------------------------------------------------------------}
  
  // Union of AddEventListenerOptions, boolean
  TEventTarget_addEventListener_options_Type = Variant;
  // Union of EventListenerOptions, boolean
  TEventTarget_removeEventListener_options_Type = Variant;
  IJSEventTarget = interface(IJSObject)
    ['{508F4552-EEB9-36D3-BF1A-151D53753BED}']
    procedure addEventListener(const aType_: UnicodeString; const aListener: TEventListener; const aOptions: IJSAddEventListenerOptions; aWantsUntrusted: Boolean);
    procedure addEventListener(const aType_: UnicodeString; const aListener: TEventListener);
    procedure addEventListener(const aType_: UnicodeString; const aListener: TEventListener; aOptions: Boolean; aWantsUntrusted: Boolean);
    procedure addEventListener(const aType_: UnicodeString; const aListener: TEventListener; aOptions: Boolean);
    procedure addEventListener(const aType_: UnicodeString; const aListener: TEventListener; const aOptions: IJSAddEventListenerOptions);
    procedure removeEventListener(const aType_: UnicodeString; const aListener: TEventListener; aOptions: Boolean);
    procedure removeEventListener(const aType_: UnicodeString; const aListener: TEventListener);
    procedure removeEventListener(const aType_: UnicodeString; const aListener: TEventListener; const aOptions: IJSEventListenerOptions);
    function dispatchEvent(aEvent: IJSEvent): Boolean;
  end;
  
  TJSEventTarget = class(TJSObject,IJSEventTarget)
  Private
  Protected
  Public
    constructor Create; overload;
    procedure addEventListener(const aType_: UnicodeString; const aListener: TEventListener; const aOptions: IJSAddEventListenerOptions; aWantsUntrusted: Boolean); overload;
    procedure addEventListener(const aType_: UnicodeString; const aListener: TEventListener); overload;
    procedure addEventListener(const aType_: UnicodeString; const aListener: TEventListener; aOptions: Boolean; aWantsUntrusted: Boolean); overload;
    procedure addEventListener(const aType_: UnicodeString; const aListener: TEventListener; aOptions: Boolean); overload;
    procedure addEventListener(const aType_: UnicodeString; const aListener: TEventListener; const aOptions: IJSAddEventListenerOptions); overload;
    procedure removeEventListener(const aType_: UnicodeString; const aListener: TEventListener; aOptions: Boolean); overload;
    procedure removeEventListener(const aType_: UnicodeString; const aListener: TEventListener); overload;
    procedure removeEventListener(const aType_: UnicodeString; const aListener: TEventListener; const aOptions: IJSEventListenerOptions); overload;
    function dispatchEvent(aEvent: IJSEvent): Boolean; overload;
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSEventTarget;
  end;
  
  { --------------------------------------------------------------------
    TJSEvent
    --------------------------------------------------------------------}
  
  TJSEventTargetDynArray = IJSArray; // array of EventTarget
  
  IJSEvent = interface(IJSObject)
    ['{AB32DD1E-5570-35E0-A83F-1CEAEC58476F}']
    function _Gettype_: UnicodeString; 
    function _Gettarget: IJSEventTarget; 
    function _GetcurrentTarget: IJSEventTarget; 
    function _GeteventPhase: Word; 
    function _Getbubbles: Boolean; 
    function _Getcancelable: Boolean; 
    function _GetreturnValue: Boolean; 
    function _GetdefaultPrevented: Boolean; 
    function _Getcomposed: Boolean; 
    function _GetisTrusted: Boolean; 
    function _GettimeStamp: TDOMHighResTimeStamp; 
    function _GetcancelBubble: Boolean; 
    function _GetoriginalTarget: IJSEventTarget; 
    function _GetexplicitOriginalTarget: IJSEventTarget; 
    procedure _SetreturnValue(const aValue: Boolean);
    procedure _SetcancelBubble(const aValue: Boolean);
    function composedPath: TJSEventTargetDynArray;
    procedure stopPropagation;
    procedure stopImmediatePropagation;
    procedure preventDefault;
    procedure initEvent(const aType_: UnicodeString; aBubbles: Boolean; aCancelable: Boolean);
    procedure initEvent(const aType_: UnicodeString);
    procedure initEvent(const aType_: UnicodeString; aBubbles: Boolean);
    property type_: UnicodeString read _Gettype_;
    property target: IJSEventTarget read _Gettarget;
    property currentTarget: IJSEventTarget read _GetcurrentTarget;
    property eventPhase: Word read _GeteventPhase;
    property bubbles: Boolean read _Getbubbles;
    property cancelable: Boolean read _Getcancelable;
    property returnValue: Boolean read _GetreturnValue write _SetreturnValue;
    property defaultPrevented: Boolean read _GetdefaultPrevented;
    property composed: Boolean read _Getcomposed;
    property isTrusted: Boolean read _GetisTrusted;
    property timeStamp: TDOMHighResTimeStamp read _GettimeStamp;
    property cancelBubble: Boolean read _GetcancelBubble write _SetcancelBubble;
    property originalTarget: IJSEventTarget read _GetoriginalTarget;
    property explicitOriginalTarget: IJSEventTarget read _GetexplicitOriginalTarget;
  end;
  
  TJSEvent = class(TJSObject,IJSEvent)
  Private
  Protected
    function _Gettype_: UnicodeString; 
    function _Gettarget: IJSEventTarget; 
    function _GetcurrentTarget: IJSEventTarget; 
    function _GeteventPhase: Word; 
    function _Getbubbles: Boolean; 
    function _Getcancelable: Boolean; 
    function _GetreturnValue: Boolean; 
    function _GetdefaultPrevented: Boolean; 
    function _Getcomposed: Boolean; 
    function _GetisTrusted: Boolean; 
    function _GettimeStamp: TDOMHighResTimeStamp; 
    function _GetcancelBubble: Boolean; 
    function _GetoriginalTarget: IJSEventTarget; 
    function _GetexplicitOriginalTarget: IJSEventTarget; 
    procedure _SetreturnValue(const aValue: Boolean);
    procedure _SetcancelBubble(const aValue: Boolean);
  Public
    Const
      NONE = 0;
      CAPTURING_PHASE = 1;
      AT_TARGET = 2;
      BUBBLING_PHASE = 3;
      ALT_MASK = $00000001;
      CONTROL_MASK = $00000002;
      SHIFT_MASK = $00000004;
      META_MASK = $00000008;
  Public
    constructor Create(const aType_: UnicodeString; const aEventInitDict: IJSEventInit); overload;
    constructor Create(const aType_: UnicodeString); overload;
    function composedPath: TJSEventTargetDynArray; overload;
    procedure stopPropagation; overload;
    procedure stopImmediatePropagation; overload;
    procedure preventDefault; overload;
    procedure initEvent(const aType_: UnicodeString; aBubbles: Boolean; aCancelable: Boolean); overload;
    procedure initEvent(const aType_: UnicodeString); overload;
    procedure initEvent(const aType_: UnicodeString; aBubbles: Boolean); overload;
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSEvent;
    property type_: UnicodeString read _Gettype_;
    property target: IJSEventTarget read _Gettarget;
    property currentTarget: IJSEventTarget read _GetcurrentTarget;
    property eventPhase: Word read _GeteventPhase;
    property bubbles: Boolean read _Getbubbles;
    property cancelable: Boolean read _Getcancelable;
    property returnValue: Boolean read _GetreturnValue write _SetreturnValue;
    property defaultPrevented: Boolean read _GetdefaultPrevented;
    property composed: Boolean read _Getcomposed;
    property isTrusted: Boolean read _GetisTrusted;
    property timeStamp: TDOMHighResTimeStamp read _GettimeStamp;
    property cancelBubble: Boolean read _GetcancelBubble write _SetcancelBubble;
    property originalTarget: IJSEventTarget read _GetoriginalTarget;
    property explicitOriginalTarget: IJSEventTarget read _GetexplicitOriginalTarget;
  end;
  
  { --------------------------------------------------------------------
    TJSimgINotificationObserver
    --------------------------------------------------------------------}
  
  IJSimgINotificationObserver = interface(IJSObject)
    ['{804BD3B3-27C0-39AC-A0EE-8EC1D6DBBD61}']
  end;
  
  TJSimgINotificationObserver = class(TJSObject,IJSimgINotificationObserver)
  Private
  Protected
  Public
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSimgINotificationObserver;
  end;
  
  { --------------------------------------------------------------------
    TJSimgIRequest
    --------------------------------------------------------------------}
  
  IJSimgIRequest = interface(IJSObject)
    ['{EEE11C67-DB51-35B9-943D-1A2372542131}']
  end;
  
  TJSimgIRequest = class(TJSObject,IJSimgIRequest)
  Private
  Protected
  Public
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSimgIRequest;
  end;
  
  { --------------------------------------------------------------------
    TJSnsIStreamListener
    --------------------------------------------------------------------}
  
  IJSnsIStreamListener = interface(IJSObject)
    ['{2B7FAA73-2682-300B-9709-981130F8698D}']
  end;
  
  TJSnsIStreamListener = class(TJSObject,IJSnsIStreamListener)
  Private
  Protected
  Public
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSnsIStreamListener;
  end;
  
  { --------------------------------------------------------------------
    TJSMozImageLoadingContent
    --------------------------------------------------------------------}
  
  IJSMozImageLoadingContent = interface(IJSObject)
    ['{E78C46AD-22F9-3955-A9ED-A5614E1AA513}']
  end;
  
  TJSMozImageLoadingContent = class(TJSObject,IJSMozImageLoadingContent)
  Private
  Protected
  Public
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSMozImageLoadingContent;
  end;
  
  { --------------------------------------------------------------------
    TJSImageBitmap
    --------------------------------------------------------------------}
  
  IJSImageBitmap = interface(IJSObject)
    ['{46D387E6-FE33-3229-BAF0-480EB6A31A3D}']
    function _Getwidth: Cardinal; 
    function _Getheight: Cardinal; 
    property width: Cardinal read _Getwidth;
    property height: Cardinal read _Getheight;
  end;
  
  TJSImageBitmap = class(TJSObject,IJSImageBitmap)
  Private
  Protected
    function _Getwidth: Cardinal; 
    function _Getheight: Cardinal; 
  Public
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSImageBitmap;
    property width: Cardinal read _Getwidth;
    property height: Cardinal read _Getheight;
  end;
  
  { --------------------------------------------------------------------
    TJSImageData
    --------------------------------------------------------------------}
  
  IJSImageData = interface(IJSObject)
    ['{D03D831F-35C5-3CC2-8276-EC263EF4EE49}']
    function _Getwidth: Cardinal; 
    function _Getheight: Cardinal; 
    function _Getdata: IJSUint8ClampedArray; 
    property width: Cardinal read _Getwidth;
    property height: Cardinal read _Getheight;
    property data: IJSUint8ClampedArray read _Getdata;
  end;
  
  TJSImageData = class(TJSObject,IJSImageData)
  Private
  Protected
    function _Getwidth: Cardinal; 
    function _Getheight: Cardinal; 
    function _Getdata: IJSUint8ClampedArray; 
  Public
    constructor Create(aSw: Cardinal; aSh: Cardinal); overload;
    constructor Create(aData: IJSUint8ClampedArray; aSw: Cardinal; aSh: Cardinal); overload;
    constructor Create(aData: IJSUint8ClampedArray; aSw: Cardinal); overload;
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSImageData;
    property width: Cardinal read _Getwidth;
    property height: Cardinal read _Getheight;
    property data: IJSUint8ClampedArray read _Getdata;
  end;
  
  { --------------------------------------------------------------------
    TJSNonElementParentNode
    --------------------------------------------------------------------}
  
  IJSNonElementParentNode = interface(IJSObject)
    ['{B9E6BE73-B984-34CC-BBBC-93D6C0C8121C}']
    function getElementById(const aElementId: UnicodeString): IJSElement;
  end;
  
  TJSNonElementParentNode = class(TJSObject,IJSNonElementParentNode)
  Private
  Protected
  Public
    function getElementById(const aElementId: UnicodeString): IJSElement; overload;
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSNonElementParentNode;
  end;
  
  { --------------------------------------------------------------------
    TJSVideoFrame
    --------------------------------------------------------------------}
  
  // Union of ArrayBufferView, ArrayBuffer
  TVideoFrame_copyTo_destination_Type = Variant;
  IJSVideoFrame = interface(IJSObject)
    ['{C16277DD-B0A8-36E5-8B54-E8B6538C778B}']
    function _Getformat: TVideoPixelFormat; 
    function _GetcodedWidth: Cardinal; 
    function _GetcodedHeight: Cardinal; 
    function _GetdisplayWidth: Cardinal; 
    function _GetdisplayHeight: Cardinal; 
    function _Getduration: QWord; 
    function _Gettimestamp: Int64; 
    function allocationSize(const aOptions: IJSVideoFrameCopyToOptions): Cardinal;
    function allocationSize: Cardinal;
    function copyTo(aDestination: IJSArrayBuffer; const aOptions: IJSVideoFrameCopyToOptions): IJSPromise; // Promise<sequence>
    function copyTo(aDestination: IJSArrayBufferView; const aOptions: IJSVideoFrameCopyToOptions): IJSPromise; // Promise<sequence>
    function copyTo(aDestination: IJSArrayBufferView): IJSPromise; // Promise<sequence>
    function copyTo(aDestination: IJSArrayBuffer): IJSPromise; // Promise<sequence>
    function clone: IJSVideoFrame;
    procedure close;
    property format: TVideoPixelFormat read _Getformat;
    property codedWidth: Cardinal read _GetcodedWidth;
    property codedHeight: Cardinal read _GetcodedHeight;
    property displayWidth: Cardinal read _GetdisplayWidth;
    property displayHeight: Cardinal read _GetdisplayHeight;
    property duration: QWord read _Getduration;
    property timestamp: Int64 read _Gettimestamp;
  end;
  
  TJSVideoFrame = class(TJSObject,IJSVideoFrame)
  Private
  Protected
    function _Getformat: TVideoPixelFormat; 
    function _GetcodedWidth: Cardinal; 
    function _GetcodedHeight: Cardinal; 
    function _GetdisplayWidth: Cardinal; 
    function _GetdisplayHeight: Cardinal; 
    function _Getduration: QWord; 
    function _Gettimestamp: Int64; 
  Public
    constructor Create(aImageElement: IJSHTMLImageElement; const aInit: IJSVideoFrameInit); overload;
    constructor Create(aImageElement: IJSHTMLImageElement); overload;
    constructor Create(aCanvasElement: IJSHTMLCanvasElement; const aInit: IJSVideoFrameInit); overload;
    constructor Create(aCanvasElement: IJSHTMLCanvasElement); overload;
    constructor Create(aVideoElement: IJSHTMLVideoElement; const aInit: IJSVideoFrameInit); overload;
    constructor Create(aVideoElement: IJSHTMLVideoElement); overload;
    constructor Create(aOffscreenCanvas: IJSOffscreenCanvas; const aInit: IJSVideoFrameInit); overload;
    constructor Create(aOffscreenCanvas: IJSOffscreenCanvas); overload;
    constructor Create(aImageBitmap: IJSImageBitmap; const aInit: IJSVideoFrameInit); overload;
    constructor Create(aImageBitmap: IJSImageBitmap); overload;
    constructor Create(aVideoFrame: IJSVideoFrame; const aInit: IJSVideoFrameInit); overload;
    constructor Create(aVideoFrame: IJSVideoFrame); overload;
    constructor Create(aBufferView: IJSArrayBufferView; const aInit: IJSVideoFrameBufferInit); overload;
    constructor Create(aBuffer: IJSArrayBuffer; const aInit: IJSVideoFrameBufferInit); overload;
    function allocationSize(const aOptions: IJSVideoFrameCopyToOptions): Cardinal; overload;
    function allocationSize: Cardinal; overload;
    function copyTo(aDestination: IJSArrayBuffer; const aOptions: IJSVideoFrameCopyToOptions): IJSPromise; overload; // Promise<sequence>
    function copyTo(aDestination: IJSArrayBufferView; const aOptions: IJSVideoFrameCopyToOptions): IJSPromise; overload; // Promise<sequence>
    function copyTo(aDestination: IJSArrayBufferView): IJSPromise; overload; // Promise<sequence>
    function copyTo(aDestination: IJSArrayBuffer): IJSPromise; overload; // Promise<sequence>
    function clone: IJSVideoFrame; overload;
    procedure close; overload;
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSVideoFrame;
    property format: TVideoPixelFormat read _Getformat;
    property codedWidth: Cardinal read _GetcodedWidth;
    property codedHeight: Cardinal read _GetcodedHeight;
    property displayWidth: Cardinal read _GetdisplayWidth;
    property displayHeight: Cardinal read _GetdisplayHeight;
    property duration: QWord read _Getduration;
    property timestamp: Int64 read _Gettimestamp;
  end;
  
  { --------------------------------------------------------------------
    TJSWebGLSampler
    --------------------------------------------------------------------}
  
  IJSWebGLSampler = interface(IJSObject)
    ['{E6D054BF-5700-3BE0-A260-F0D81AA2A621}']
  end;
  
  TJSWebGLSampler = class(TJSObject,IJSWebGLSampler)
  Private
  Protected
  Public
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSWebGLSampler;
  end;
  
  { --------------------------------------------------------------------
    TJSWebGLSync
    --------------------------------------------------------------------}
  
  IJSWebGLSync = interface(IJSObject)
    ['{3383B7B5-610A-3240-B869-8DD15390C552}']
  end;
  
  TJSWebGLSync = class(TJSObject,IJSWebGLSync)
  Private
  Protected
  Public
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSWebGLSync;
  end;
  
  { --------------------------------------------------------------------
    TJSWebGLTransformFeedback
    --------------------------------------------------------------------}
  
  IJSWebGLTransformFeedback = interface(IJSObject)
    ['{D660E8F5-D604-3A42-8FFE-03239D3DE360}']
  end;
  
  TJSWebGLTransformFeedback = class(TJSObject,IJSWebGLTransformFeedback)
  Private
  Protected
  Public
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSWebGLTransformFeedback;
  end;
  
  { --------------------------------------------------------------------
    TJSWebGL2RenderingContext
    --------------------------------------------------------------------}
  
  TUnicodeStringDynArray = IJSArray; // array of DOMString
  TJSWebGLShaderDynArray = IJSArray; // array of WebGLShader
  TGLfloatDynArray = IJSArray; // array of GLfloat
  TGLenumDynArray = IJSArray; // array of GLenum
  TGLintDynArray = IJSArray; // array of GLint
  TGLuintDynArray = IJSArray; // array of GLuint
  
  IJSWebGL2RenderingContext = interface(IJSObject)
    ['{9A4F888B-DEE7-3133-8675-4203EDB67B9A}']
    function _Getcanvas: TCanvasSource; 
    function _GetdrawingBufferWidth: TGLsizei; 
    function _GetdrawingBufferHeight: TGLsizei; 
    function _GetdrawingBufferColorSpace: TPredefinedColorSpace; 
    function _GetunpackColorSpace: TPredefinedColorSpace; 
    procedure _SetdrawingBufferColorSpace(const aValue: TPredefinedColorSpace);
    procedure _SetunpackColorSpace(const aValue: TPredefinedColorSpace);
    function getContextAttributes: IJSWebGLContextAttributes;
    function isContextLost: Boolean;
    function getSupportedExtensions: TUnicodeStringDynArray;
    function getExtension(const aName: UnicodeString): IJSObject;
    procedure activeTexture(aTexture: TGLenum);
    procedure attachShader(aProgram_: IJSWebGLProgram; aShader: IJSWebGLShader);
    procedure bindAttribLocation(aProgram_: IJSWebGLProgram; aIndex: TGLuint; const aName: UnicodeString);
    procedure bindBuffer(aTarget: TGLenum; aBuffer: IJSWebGLBuffer);
    procedure bindFramebuffer(aTarget: TGLenum; aFramebuffer: IJSWebGLFramebuffer);
    procedure bindRenderbuffer(aTarget: TGLenum; aRenderbuffer: IJSWebGLRenderbuffer);
    procedure bindTexture(aTarget: TGLenum; aTexture: IJSWebGLTexture);
    procedure blendColor(aRed: TGLfloat; aGreen: TGLfloat; aBlue: TGLfloat; aAlpha: TGLfloat);
    procedure blendEquation(aMode: TGLenum);
    procedure blendEquationSeparate(aModeRGB: TGLenum; aModeAlpha: TGLenum);
    procedure blendFunc(aSfactor: TGLenum; aDfactor: TGLenum);
    procedure blendFuncSeparate(aSrcRGB: TGLenum; aDstRGB: TGLenum; aSrcAlpha: TGLenum; aDstAlpha: TGLenum);
    function checkFramebufferStatus(aTarget: TGLenum): TGLenum;
    procedure clear(aMask: TGLbitfield);
    procedure clearColor(aRed: TGLfloat; aGreen: TGLfloat; aBlue: TGLfloat; aAlpha: TGLfloat);
    procedure clearDepth(aDepth: TGLclampf);
    procedure clearStencil(aS_: TGLint);
    procedure colorMask(aRed: TGLboolean; aGreen: TGLboolean; aBlue: TGLboolean; aAlpha: TGLboolean);
    procedure compileShader(aShader: IJSWebGLShader);
    procedure copyTexImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLenum; aX: TGLint; aY: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aBorder: TGLint);
    procedure copyTexSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aX: TGLint; aY: TGLint; aWidth: TGLsizei; aHeight: TGLsizei);
    function createBuffer: IJSWebGLBuffer;
    function createFramebuffer: IJSWebGLFramebuffer;
    function createProgram: IJSWebGLProgram;
    function createRenderbuffer: IJSWebGLRenderbuffer;
    function createShader(aType_: TGLenum): IJSWebGLShader;
    function createTexture: IJSWebGLTexture;
    procedure cullFace(aMode: TGLenum);
    procedure deleteBuffer(aBuffer: IJSWebGLBuffer);
    procedure deleteFramebuffer(aFramebuffer: IJSWebGLFramebuffer);
    procedure deleteProgram(aProgram_: IJSWebGLProgram);
    procedure deleteRenderbuffer(aRenderbuffer: IJSWebGLRenderbuffer);
    procedure deleteShader(aShader: IJSWebGLShader);
    procedure deleteTexture(aTexture: IJSWebGLTexture);
    procedure depthFunc(aFunc: TGLenum);
    procedure depthMask(aFlag: TGLboolean);
    procedure depthRange(aZNear: TGLclampf; aZFar: TGLclampf);
    procedure detachShader(aProgram_: IJSWebGLProgram; aShader: IJSWebGLShader);
    procedure disable(aCap: TGLenum);
    procedure disableVertexAttribArray(aIndex: TGLuint);
    procedure drawArrays(aMode: TGLenum; aFirst: TGLint; aCount: TGLsizei);
    procedure drawElements(aMode: TGLenum; aCount: TGLsizei; aType_: TGLenum; aOffset: TGLintptr);
    procedure enable(aCap: TGLenum);
    procedure enableVertexAttribArray(aIndex: TGLuint);
    procedure finish;
    procedure flush;
    procedure framebufferRenderbuffer(aTarget: TGLenum; aAttachment: TGLenum; aRenderbuffertarget: TGLenum; aRenderbuffer: IJSWebGLRenderbuffer);
    procedure framebufferTexture2D(aTarget: TGLenum; aAttachment: TGLenum; aTextarget: TGLenum; aTexture: IJSWebGLTexture; aLevel: TGLint);
    procedure frontFace(aMode: TGLenum);
    procedure generateMipmap(aTarget: TGLenum);
    function getActiveAttrib(aProgram_: IJSWebGLProgram; aIndex: TGLuint): IJSWebGLActiveInfo;
    function getActiveUniform(aProgram_: IJSWebGLProgram; aIndex: TGLuint): IJSWebGLActiveInfo;
    function getAttachedShaders(aProgram_: IJSWebGLProgram): TJSWebGLShaderDynArray;
    function getAttribLocation(aProgram_: IJSWebGLProgram; const aName: UnicodeString): TGLint;
    function getBufferParameter(aTarget: TGLenum; aPname: TGLenum): Variant;
    function getParameter(aPname: TGLenum): Variant;
    function getError: TGLenum;
    function getFramebufferAttachmentParameter(aTarget: TGLenum; aAttachment: TGLenum; aPname: TGLenum): Variant;
    function getProgramParameter(aProgram_: IJSWebGLProgram; aPname: TGLenum): Variant;
    function getProgramInfoLog(aProgram_: IJSWebGLProgram): UnicodeString;
    function getRenderbufferParameter(aTarget: TGLenum; aPname: TGLenum): Variant;
    function getShaderParameter(aShader: IJSWebGLShader; aPname: TGLenum): Variant;
    function getShaderPrecisionFormat(aShadertype: TGLenum; aPrecisiontype: TGLenum): IJSWebGLShaderPrecisionFormat;
    function getShaderInfoLog(aShader: IJSWebGLShader): UnicodeString;
    function getShaderSource(aShader: IJSWebGLShader): UnicodeString;
    function getTexParameter(aTarget: TGLenum; aPname: TGLenum): Variant;
    function getUniform(aProgram_: IJSWebGLProgram; aLocation: IJSWebGLUniformLocation): Variant;
    function getUniformLocation(aProgram_: IJSWebGLProgram; const aName: UnicodeString): IJSWebGLUniformLocation;
    function getVertexAttrib(aIndex: TGLuint; aPname: TGLenum): Variant;
    function getVertexAttribOffset(aIndex: TGLuint; aPname: TGLenum): TGLintptr;
    procedure hint(aTarget: TGLenum; aMode: TGLenum);
    function isBuffer(aBuffer: IJSWebGLBuffer): TGLboolean;
    function isEnabled(aCap: TGLenum): TGLboolean;
    function isFramebuffer(aFramebuffer: IJSWebGLFramebuffer): TGLboolean;
    function isProgram(aProgram_: IJSWebGLProgram): TGLboolean;
    function isRenderbuffer(aRenderbuffer: IJSWebGLRenderbuffer): TGLboolean;
    function isShader(aShader: IJSWebGLShader): TGLboolean;
    function isTexture(aTexture: IJSWebGLTexture): TGLboolean;
    procedure lineWidth(aWidth: TGLfloat);
    procedure linkProgram(aProgram_: IJSWebGLProgram);
    procedure pixelStorei(aPname: TGLenum; aParam: TGLint);
    procedure polygonOffset(aFactor: TGLfloat; aUnits: TGLfloat);
    procedure renderbufferStorage(aTarget: TGLenum; aInternalformat: TGLenum; aWidth: TGLsizei; aHeight: TGLsizei);
    procedure sampleCoverage(aValue: TGLclampf; aInvert: TGLboolean);
    procedure scissor(aX: TGLint; aY: TGLint; aWidth: TGLsizei; aHeight: TGLsizei);
    procedure shaderSource(aShader: IJSWebGLShader; const aSource: UnicodeString);
    procedure stencilFunc(aFunc: TGLenum; aRef: TGLint; aMask: TGLuint);
    procedure stencilFuncSeparate(aFace: TGLenum; aFunc: TGLenum; aRef: TGLint; aMask: TGLuint);
    procedure stencilMask(aMask: TGLuint);
    procedure stencilMaskSeparate(aFace: TGLenum; aMask: TGLuint);
    procedure stencilOp(aFail: TGLenum; aZfail: TGLenum; aZpass: TGLenum);
    procedure stencilOpSeparate(aFace: TGLenum; aFail: TGLenum; aZfail: TGLenum; aZpass: TGLenum);
    procedure texParameterf(aTarget: TGLenum; aPname: TGLenum; aParam: TGLfloat);
    procedure texParameteri(aTarget: TGLenum; aPname: TGLenum; aParam: TGLint);
    procedure uniform1f(aLocation: IJSWebGLUniformLocation; aX: TGLfloat);
    procedure uniform2f(aLocation: IJSWebGLUniformLocation; aX: TGLfloat; aY: TGLfloat);
    procedure uniform3f(aLocation: IJSWebGLUniformLocation; aX: TGLfloat; aY: TGLfloat; aZ: TGLfloat);
    procedure uniform4f(aLocation: IJSWebGLUniformLocation; aX: TGLfloat; aY: TGLfloat; aZ: TGLfloat; aW: TGLfloat);
    procedure uniform1i(aLocation: IJSWebGLUniformLocation; aX: TGLint);
    procedure uniform2i(aLocation: IJSWebGLUniformLocation; aX: TGLint; aY: TGLint);
    procedure uniform3i(aLocation: IJSWebGLUniformLocation; aX: TGLint; aY: TGLint; aZ: TGLint);
    procedure uniform4i(aLocation: IJSWebGLUniformLocation; aX: TGLint; aY: TGLint; aZ: TGLint; aW: TGLint);
    procedure useProgram(aProgram_: IJSWebGLProgram);
    procedure validateProgram(aProgram_: IJSWebGLProgram);
    procedure vertexAttrib1f(aIndx: TGLuint; aX: TGLfloat);
    procedure vertexAttrib1fv(aIndx: TGLuint; aValues: IJSFloat32Array);
    procedure vertexAttrib1fv(aIndx: TGLuint; const aValues: TGLfloatDynArray);
    procedure vertexAttrib2f(aIndx: TGLuint; aX: TGLfloat; aY: TGLfloat);
    procedure vertexAttrib2fv(aIndx: TGLuint; aValues: IJSFloat32Array);
    procedure vertexAttrib2fv(aIndx: TGLuint; const aValues: TGLfloatDynArray);
    procedure vertexAttrib3f(aIndx: TGLuint; aX: TGLfloat; aY: TGLfloat; aZ: TGLfloat);
    procedure vertexAttrib3fv(aIndx: TGLuint; aValues: IJSFloat32Array);
    procedure vertexAttrib3fv(aIndx: TGLuint; const aValues: TGLfloatDynArray);
    procedure vertexAttrib4f(aIndx: TGLuint; aX: TGLfloat; aY: TGLfloat; aZ: TGLfloat; aW: TGLfloat);
    procedure vertexAttrib4fv(aIndx: TGLuint; aValues: IJSFloat32Array);
    procedure vertexAttrib4fv(aIndx: TGLuint; const aValues: TGLfloatDynArray);
    procedure vertexAttribPointer(aIndx: TGLuint; aSize: TGLint; aType_: TGLenum; aNormalized: TGLboolean; aStride: TGLsizei; aOffset: TGLintptr);
    procedure viewport(aX: TGLint; aY: TGLint; aWidth: TGLsizei; aHeight: TGLsizei);
    function makeXRCompatible: IJSPromise; // Promise<undefined>
    procedure bufferData(aTarget: TGLenum; aSize: TGLsizeiptr; aUsage: TGLenum);
    procedure bufferData(aTarget: TGLenum; aSrcData: IJSArrayBuffer; aUsage: TGLenum);
    procedure bufferData(aTarget: TGLenum; aSrcData: IJSArrayBufferView; aUsage: TGLenum);
    procedure bufferSubData(aTarget: TGLenum; aOffset: TGLintptr; aSrcData: IJSArrayBuffer);
    procedure bufferSubData(aTarget: TGLenum; aOffset: TGLintptr; aSrcData: IJSArrayBufferView);
    procedure bufferData(aTarget: TGLenum; aSrcData: IJSArrayBufferView; aUsage: TGLenum; aSrcOffset: TGLuint; aLength_: TGLuint);
    procedure bufferData(aTarget: TGLenum; aSrcData: IJSArrayBufferView; aUsage: TGLenum; aSrcOffset: TGLuint);
    procedure bufferSubData(aTarget: TGLenum; aDstByteOffset: TGLintptr; aSrcData: IJSArrayBufferView; aSrcOffset: TGLuint; aLength_: TGLuint);
    procedure bufferSubData(aTarget: TGLenum; aDstByteOffset: TGLintptr; aSrcData: IJSArrayBufferView; aSrcOffset: TGLuint);
    procedure copyBufferSubData(aReadTarget: TGLenum; aWriteTarget: TGLenum; aReadOffset: TGLintptr; aWriteOffset: TGLintptr; aSize: TGLsizeiptr);
    procedure getBufferSubData(aTarget: TGLenum; aSrcByteOffset: TGLintptr; aDstData: IJSArrayBufferView; aDstOffset: TGLuint; aLength_: TGLuint);
    procedure getBufferSubData(aTarget: TGLenum; aSrcByteOffset: TGLintptr; aDstData: IJSArrayBufferView);
    procedure getBufferSubData(aTarget: TGLenum; aSrcByteOffset: TGLintptr; aDstData: IJSArrayBufferView; aDstOffset: TGLuint);
    procedure blitFramebuffer(aSrcX0: TGLint; aSrcY0: TGLint; aSrcX1: TGLint; aSrcY1: TGLint; aDstX0: TGLint; aDstY0: TGLint; aDstX1: TGLint; aDstY1: TGLint; aMask: TGLbitfield; aFilter: TGLenum);
    procedure framebufferTextureLayer(aTarget: TGLenum; aAttachment: TGLenum; aTexture: IJSWebGLTexture; aLevel: TGLint; aLayer: TGLint);
    procedure invalidateFramebuffer(aTarget: TGLenum; const aAttachments: TGLenumDynArray);
    procedure invalidateSubFramebuffer(aTarget: TGLenum; const aAttachments: TGLenumDynArray; aX: TGLint; aY: TGLint; aWidth: TGLsizei; aHeight: TGLsizei);
    procedure readBuffer(aSrc: TGLenum);
    function getInternalformatParameter(aTarget: TGLenum; aInternalformat: TGLenum; aPname: TGLenum): Variant;
    procedure renderbufferStorageMultisample(aTarget: TGLenum; aSamples: TGLsizei; aInternalformat: TGLenum; aWidth: TGLsizei; aHeight: TGLsizei);
    procedure texStorage2D(aTarget: TGLenum; aLevels: TGLsizei; aInternalformat: TGLenum; aWidth: TGLsizei; aHeight: TGLsizei);
    procedure texStorage3D(aTarget: TGLenum; aLevels: TGLsizei; aInternalformat: TGLenum; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei);
    procedure texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aPixels: IJSArrayBufferView);
    procedure texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSHTMLCanvasElement);
    procedure texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSHTMLImageElement);
    procedure texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSHTMLVideoElement);
    procedure texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSImageBitmap);
    procedure texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSImageData);
    procedure texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSOffscreenCanvas);
    procedure texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSVideoFrame);
    procedure texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aPixels: IJSArrayBufferView);
    procedure texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSHTMLCanvasElement);
    procedure texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSHTMLImageElement);
    procedure texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSHTMLVideoElement);
    procedure texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSImageBitmap);
    procedure texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSImageData);
    procedure texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSOffscreenCanvas);
    procedure texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSVideoFrame);
    procedure texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aPboOffset: TGLintptr);
    procedure texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSHTMLCanvasElement);
    procedure texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSHTMLImageElement);
    procedure texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSHTMLVideoElement);
    procedure texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSImageBitmap);
    procedure texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSImageData);
    procedure texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSOffscreenCanvas);
    procedure texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSVideoFrame);
    procedure texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aSrcData: IJSArrayBufferView; aSrcOffset: TGLuint);
    procedure texImage3D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aPboOffset: TGLintptr);
    procedure texImage3D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSHTMLCanvasElement);
    procedure texImage3D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSHTMLImageElement);
    procedure texImage3D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSHTMLVideoElement);
    procedure texImage3D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSImageBitmap);
    procedure texImage3D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSImageData);
    procedure texImage3D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSOffscreenCanvas);
    procedure texImage3D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSVideoFrame);
    procedure texImage3D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aSrcData: IJSArrayBufferView);
    procedure texImage3D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aSrcData: IJSArrayBufferView; aSrcOffset: TGLuint);
    procedure texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aPboOffset: TGLintptr);
    procedure texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aSource: IJSHTMLCanvasElement);
    procedure texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aSource: IJSHTMLImageElement);
    procedure texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aSource: IJSHTMLVideoElement);
    procedure texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aSource: IJSImageBitmap);
    procedure texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aSource: IJSImageData);
    procedure texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aSource: IJSOffscreenCanvas);
    procedure texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aSource: IJSVideoFrame);
    procedure texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aSrcData: IJSArrayBufferView; aSrcOffset: TGLuint);
    procedure texSubImage3D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aZoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aPboOffset: TGLintptr);
    procedure texSubImage3D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aZoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aSource: IJSHTMLCanvasElement);
    procedure texSubImage3D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aZoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aSource: IJSHTMLImageElement);
    procedure texSubImage3D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aZoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aSource: IJSHTMLVideoElement);
    procedure texSubImage3D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aZoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aSource: IJSImageBitmap);
    procedure texSubImage3D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aZoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aSource: IJSImageData);
    procedure texSubImage3D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aZoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aSource: IJSOffscreenCanvas);
    procedure texSubImage3D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aZoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aSource: IJSVideoFrame);
    procedure texSubImage3D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aZoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aSrcData: IJSArrayBufferView; aSrcOffset: TGLuint);
    procedure texSubImage3D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aZoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aSrcData: IJSArrayBufferView);
    procedure copyTexSubImage3D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aZoffset: TGLint; aX: TGLint; aY: TGLint; aWidth: TGLsizei; aHeight: TGLsizei);
    procedure compressedTexImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLenum; aWidth: TGLsizei; aHeight: TGLsizei; aBorder: TGLint; aImageSize: TGLsizei; aOffset: TGLintptr);
    procedure compressedTexImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLenum; aWidth: TGLsizei; aHeight: TGLsizei; aBorder: TGLint; aSrcData: IJSArrayBufferView; aSrcOffset: TGLuint; aSrcLengthOverride: TGLuint);
    procedure compressedTexImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLenum; aWidth: TGLsizei; aHeight: TGLsizei; aBorder: TGLint; aSrcData: IJSArrayBufferView);
    procedure compressedTexImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLenum; aWidth: TGLsizei; aHeight: TGLsizei; aBorder: TGLint; aSrcData: IJSArrayBufferView; aSrcOffset: TGLuint);
    procedure compressedTexImage3D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLenum; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aBorder: TGLint; aImageSize: TGLsizei; aOffset: TGLintptr);
    procedure compressedTexImage3D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLenum; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aBorder: TGLint; aSrcData: IJSArrayBufferView; aSrcOffset: TGLuint; aSrcLengthOverride: TGLuint);
    procedure compressedTexImage3D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLenum; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aBorder: TGLint; aSrcData: IJSArrayBufferView);
    procedure compressedTexImage3D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLenum; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aBorder: TGLint; aSrcData: IJSArrayBufferView; aSrcOffset: TGLuint);
    procedure compressedTexSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aFormat: TGLenum; aImageSize: TGLsizei; aOffset: TGLintptr);
    procedure compressedTexSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aFormat: TGLenum; aSrcData: IJSArrayBufferView; aSrcOffset: TGLuint; aSrcLengthOverride: TGLuint);
    procedure compressedTexSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aFormat: TGLenum; aSrcData: IJSArrayBufferView);
    procedure compressedTexSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aFormat: TGLenum; aSrcData: IJSArrayBufferView; aSrcOffset: TGLuint);
    procedure compressedTexSubImage3D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aZoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aFormat: TGLenum; aImageSize: TGLsizei; aOffset: TGLintptr);
    procedure compressedTexSubImage3D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aZoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aFormat: TGLenum; aSrcData: IJSArrayBufferView; aSrcOffset: TGLuint; aSrcLengthOverride: TGLuint);
    procedure compressedTexSubImage3D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aZoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aFormat: TGLenum; aSrcData: IJSArrayBufferView);
    procedure compressedTexSubImage3D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aZoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aFormat: TGLenum; aSrcData: IJSArrayBufferView; aSrcOffset: TGLuint);
    function getFragDataLocation(aProgram_: IJSWebGLProgram; const aName: UnicodeString): TGLint;
    procedure uniform1ui(aLocation: IJSWebGLUniformLocation; aV0: TGLuint);
    procedure uniform2ui(aLocation: IJSWebGLUniformLocation; aV0: TGLuint; aV1: TGLuint);
    procedure uniform3ui(aLocation: IJSWebGLUniformLocation; aV0: TGLuint; aV1: TGLuint; aV2: TGLuint);
    procedure uniform4ui(aLocation: IJSWebGLUniformLocation; aV0: TGLuint; aV1: TGLuint; aV2: TGLuint; aV3: TGLuint);
    procedure uniform1fv(aLocation: IJSWebGLUniformLocation; aData: IJSFloat32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint);
    procedure uniform1fv(aLocation: IJSWebGLUniformLocation; const aData: TGLfloatDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint);
    procedure uniform1fv(aLocation: IJSWebGLUniformLocation; const aData: TGLfloatDynArray);
    procedure uniform1fv(aLocation: IJSWebGLUniformLocation; aData: IJSFloat32Array);
    procedure uniform1fv(aLocation: IJSWebGLUniformLocation; const aData: TGLfloatDynArray; aSrcOffset: TGLuint);
    procedure uniform1fv(aLocation: IJSWebGLUniformLocation; aData: IJSFloat32Array; aSrcOffset: TGLuint);
    procedure uniform2fv(aLocation: IJSWebGLUniformLocation; aData: IJSFloat32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint);
    procedure uniform2fv(aLocation: IJSWebGLUniformLocation; const aData: TGLfloatDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint);
    procedure uniform2fv(aLocation: IJSWebGLUniformLocation; const aData: TGLfloatDynArray);
    procedure uniform2fv(aLocation: IJSWebGLUniformLocation; aData: IJSFloat32Array);
    procedure uniform2fv(aLocation: IJSWebGLUniformLocation; const aData: TGLfloatDynArray; aSrcOffset: TGLuint);
    procedure uniform2fv(aLocation: IJSWebGLUniformLocation; aData: IJSFloat32Array; aSrcOffset: TGLuint);
    procedure uniform3fv(aLocation: IJSWebGLUniformLocation; aData: IJSFloat32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint);
    procedure uniform3fv(aLocation: IJSWebGLUniformLocation; const aData: TGLfloatDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint);
    procedure uniform3fv(aLocation: IJSWebGLUniformLocation; const aData: TGLfloatDynArray);
    procedure uniform3fv(aLocation: IJSWebGLUniformLocation; aData: IJSFloat32Array);
    procedure uniform3fv(aLocation: IJSWebGLUniformLocation; const aData: TGLfloatDynArray; aSrcOffset: TGLuint);
    procedure uniform3fv(aLocation: IJSWebGLUniformLocation; aData: IJSFloat32Array; aSrcOffset: TGLuint);
    procedure uniform4fv(aLocation: IJSWebGLUniformLocation; aData: IJSFloat32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint);
    procedure uniform4fv(aLocation: IJSWebGLUniformLocation; const aData: TGLfloatDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint);
    procedure uniform4fv(aLocation: IJSWebGLUniformLocation; const aData: TGLfloatDynArray);
    procedure uniform4fv(aLocation: IJSWebGLUniformLocation; aData: IJSFloat32Array);
    procedure uniform4fv(aLocation: IJSWebGLUniformLocation; const aData: TGLfloatDynArray; aSrcOffset: TGLuint);
    procedure uniform4fv(aLocation: IJSWebGLUniformLocation; aData: IJSFloat32Array; aSrcOffset: TGLuint);
    procedure uniform1iv(aLocation: IJSWebGLUniformLocation; aData: IJSInt32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint);
    procedure uniform1iv(aLocation: IJSWebGLUniformLocation; const aData: TGLintDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint);
    procedure uniform1iv(aLocation: IJSWebGLUniformLocation; const aData: TGLintDynArray);
    procedure uniform1iv(aLocation: IJSWebGLUniformLocation; aData: IJSInt32Array);
    procedure uniform1iv(aLocation: IJSWebGLUniformLocation; const aData: TGLintDynArray; aSrcOffset: TGLuint);
    procedure uniform1iv(aLocation: IJSWebGLUniformLocation; aData: IJSInt32Array; aSrcOffset: TGLuint);
    procedure uniform2iv(aLocation: IJSWebGLUniformLocation; aData: IJSInt32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint);
    procedure uniform2iv(aLocation: IJSWebGLUniformLocation; const aData: TGLintDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint);
    procedure uniform2iv(aLocation: IJSWebGLUniformLocation; const aData: TGLintDynArray);
    procedure uniform2iv(aLocation: IJSWebGLUniformLocation; aData: IJSInt32Array);
    procedure uniform2iv(aLocation: IJSWebGLUniformLocation; const aData: TGLintDynArray; aSrcOffset: TGLuint);
    procedure uniform2iv(aLocation: IJSWebGLUniformLocation; aData: IJSInt32Array; aSrcOffset: TGLuint);
    procedure uniform3iv(aLocation: IJSWebGLUniformLocation; aData: IJSInt32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint);
    procedure uniform3iv(aLocation: IJSWebGLUniformLocation; const aData: TGLintDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint);
    procedure uniform3iv(aLocation: IJSWebGLUniformLocation; const aData: TGLintDynArray);
    procedure uniform3iv(aLocation: IJSWebGLUniformLocation; aData: IJSInt32Array);
    procedure uniform3iv(aLocation: IJSWebGLUniformLocation; const aData: TGLintDynArray; aSrcOffset: TGLuint);
    procedure uniform3iv(aLocation: IJSWebGLUniformLocation; aData: IJSInt32Array; aSrcOffset: TGLuint);
    procedure uniform4iv(aLocation: IJSWebGLUniformLocation; aData: IJSInt32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint);
    procedure uniform4iv(aLocation: IJSWebGLUniformLocation; const aData: TGLintDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint);
    procedure uniform4iv(aLocation: IJSWebGLUniformLocation; const aData: TGLintDynArray);
    procedure uniform4iv(aLocation: IJSWebGLUniformLocation; aData: IJSInt32Array);
    procedure uniform4iv(aLocation: IJSWebGLUniformLocation; const aData: TGLintDynArray; aSrcOffset: TGLuint);
    procedure uniform4iv(aLocation: IJSWebGLUniformLocation; aData: IJSInt32Array; aSrcOffset: TGLuint);
    procedure uniform1uiv(aLocation: IJSWebGLUniformLocation; const aData: TGLuintDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint);
    procedure uniform1uiv(aLocation: IJSWebGLUniformLocation; aData: IJSUint32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint);
    procedure uniform1uiv(aLocation: IJSWebGLUniformLocation; aData: IJSUint32Array);
    procedure uniform1uiv(aLocation: IJSWebGLUniformLocation; const aData: TGLuintDynArray);
    procedure uniform1uiv(aLocation: IJSWebGLUniformLocation; aData: IJSUint32Array; aSrcOffset: TGLuint);
    procedure uniform1uiv(aLocation: IJSWebGLUniformLocation; const aData: TGLuintDynArray; aSrcOffset: TGLuint);
    procedure uniform2uiv(aLocation: IJSWebGLUniformLocation; const aData: TGLuintDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint);
    procedure uniform2uiv(aLocation: IJSWebGLUniformLocation; aData: IJSUint32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint);
    procedure uniform2uiv(aLocation: IJSWebGLUniformLocation; aData: IJSUint32Array);
    procedure uniform2uiv(aLocation: IJSWebGLUniformLocation; const aData: TGLuintDynArray);
    procedure uniform2uiv(aLocation: IJSWebGLUniformLocation; aData: IJSUint32Array; aSrcOffset: TGLuint);
    procedure uniform2uiv(aLocation: IJSWebGLUniformLocation; const aData: TGLuintDynArray; aSrcOffset: TGLuint);
    procedure uniform3uiv(aLocation: IJSWebGLUniformLocation; const aData: TGLuintDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint);
    procedure uniform3uiv(aLocation: IJSWebGLUniformLocation; aData: IJSUint32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint);
    procedure uniform3uiv(aLocation: IJSWebGLUniformLocation; aData: IJSUint32Array);
    procedure uniform3uiv(aLocation: IJSWebGLUniformLocation; const aData: TGLuintDynArray);
    procedure uniform3uiv(aLocation: IJSWebGLUniformLocation; aData: IJSUint32Array; aSrcOffset: TGLuint);
    procedure uniform3uiv(aLocation: IJSWebGLUniformLocation; const aData: TGLuintDynArray; aSrcOffset: TGLuint);
    procedure uniform4uiv(aLocation: IJSWebGLUniformLocation; const aData: TGLuintDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint);
    procedure uniform4uiv(aLocation: IJSWebGLUniformLocation; aData: IJSUint32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint);
    procedure uniform4uiv(aLocation: IJSWebGLUniformLocation; aData: IJSUint32Array);
    procedure uniform4uiv(aLocation: IJSWebGLUniformLocation; const aData: TGLuintDynArray);
    procedure uniform4uiv(aLocation: IJSWebGLUniformLocation; aData: IJSUint32Array; aSrcOffset: TGLuint);
    procedure uniform4uiv(aLocation: IJSWebGLUniformLocation; const aData: TGLuintDynArray; aSrcOffset: TGLuint);
    procedure uniformMatrix2fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint);
    procedure uniformMatrix2fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint);
    procedure uniformMatrix2fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray);
    procedure uniformMatrix2fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array);
    procedure uniformMatrix2fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray; aSrcOffset: TGLuint);
    procedure uniformMatrix2fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array; aSrcOffset: TGLuint);
    procedure uniformMatrix3x2fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint);
    procedure uniformMatrix3x2fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint);
    procedure uniformMatrix3x2fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray);
    procedure uniformMatrix3x2fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array);
    procedure uniformMatrix3x2fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray; aSrcOffset: TGLuint);
    procedure uniformMatrix3x2fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array; aSrcOffset: TGLuint);
    procedure uniformMatrix4x2fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint);
    procedure uniformMatrix4x2fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint);
    procedure uniformMatrix4x2fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray);
    procedure uniformMatrix4x2fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array);
    procedure uniformMatrix4x2fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray; aSrcOffset: TGLuint);
    procedure uniformMatrix4x2fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array; aSrcOffset: TGLuint);
    procedure uniformMatrix2x3fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint);
    procedure uniformMatrix2x3fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint);
    procedure uniformMatrix2x3fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray);
    procedure uniformMatrix2x3fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array);
    procedure uniformMatrix2x3fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray; aSrcOffset: TGLuint);
    procedure uniformMatrix2x3fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array; aSrcOffset: TGLuint);
    procedure uniformMatrix3fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint);
    procedure uniformMatrix3fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint);
    procedure uniformMatrix3fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray);
    procedure uniformMatrix3fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array);
    procedure uniformMatrix3fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray; aSrcOffset: TGLuint);
    procedure uniformMatrix3fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array; aSrcOffset: TGLuint);
    procedure uniformMatrix4x3fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint);
    procedure uniformMatrix4x3fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint);
    procedure uniformMatrix4x3fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray);
    procedure uniformMatrix4x3fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array);
    procedure uniformMatrix4x3fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray; aSrcOffset: TGLuint);
    procedure uniformMatrix4x3fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array; aSrcOffset: TGLuint);
    procedure uniformMatrix2x4fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint);
    procedure uniformMatrix2x4fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint);
    procedure uniformMatrix2x4fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray);
    procedure uniformMatrix2x4fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array);
    procedure uniformMatrix2x4fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray; aSrcOffset: TGLuint);
    procedure uniformMatrix2x4fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array; aSrcOffset: TGLuint);
    procedure uniformMatrix3x4fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint);
    procedure uniformMatrix3x4fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint);
    procedure uniformMatrix3x4fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray);
    procedure uniformMatrix3x4fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array);
    procedure uniformMatrix3x4fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray; aSrcOffset: TGLuint);
    procedure uniformMatrix3x4fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array; aSrcOffset: TGLuint);
    procedure uniformMatrix4fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint);
    procedure uniformMatrix4fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint);
    procedure uniformMatrix4fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray);
    procedure uniformMatrix4fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array);
    procedure uniformMatrix4fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray; aSrcOffset: TGLuint);
    procedure uniformMatrix4fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array; aSrcOffset: TGLuint);
    procedure vertexAttribI4i(aIndex: TGLuint; aX: TGLint; aY: TGLint; aZ: TGLint; aW: TGLint);
    procedure vertexAttribI4iv(aIndex: TGLuint; aValues: IJSInt32Array);
    procedure vertexAttribI4iv(aIndex: TGLuint; const aValues: TGLintDynArray);
    procedure vertexAttribI4ui(aIndex: TGLuint; aX: TGLuint; aY: TGLuint; aZ: TGLuint; aW: TGLuint);
    procedure vertexAttribI4uiv(aIndex: TGLuint; const aValues: TGLuintDynArray);
    procedure vertexAttribI4uiv(aIndex: TGLuint; aValues: IJSUint32Array);
    procedure vertexAttribIPointer(aIndex: TGLuint; aSize: TGLint; aType_: TGLenum; aStride: TGLsizei; aOffset: TGLintptr);
    procedure vertexAttribDivisor(aIndex: TGLuint; aDivisor: TGLuint);
    procedure drawArraysInstanced(aMode: TGLenum; aFirst: TGLint; aCount: TGLsizei; aInstanceCount: TGLsizei);
    procedure drawElementsInstanced(aMode: TGLenum; aCount: TGLsizei; aType_: TGLenum; aOffset: TGLintptr; aInstanceCount: TGLsizei);
    procedure drawRangeElements(aMode: TGLenum; aStart: TGLuint; aEnd_: TGLuint; aCount: TGLsizei; aType_: TGLenum; aOffset: TGLintptr);
    procedure readPixels(aX: TGLint; aY: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aDstData: IJSArrayBufferView);
    procedure readPixels(aX: TGLint; aY: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aOffset: TGLintptr);
    procedure readPixels(aX: TGLint; aY: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aDstData: IJSArrayBufferView; aDstOffset: TGLuint);
    procedure drawBuffers(const aBuffers: TGLenumDynArray);
    procedure clearBufferfv(aBuffer: TGLenum; aDrawbuffer: TGLint; aValues: IJSFloat32Array; aSrcOffset: TGLuint);
    procedure clearBufferfv(aBuffer: TGLenum; aDrawbuffer: TGLint; const aValues: TGLfloatDynArray; aSrcOffset: TGLuint);
    procedure clearBufferfv(aBuffer: TGLenum; aDrawbuffer: TGLint; const aValues: TGLfloatDynArray);
    procedure clearBufferfv(aBuffer: TGLenum; aDrawbuffer: TGLint; aValues: IJSFloat32Array);
    procedure clearBufferiv(aBuffer: TGLenum; aDrawbuffer: TGLint; aValues: IJSInt32Array; aSrcOffset: TGLuint);
    procedure clearBufferiv(aBuffer: TGLenum; aDrawbuffer: TGLint; const aValues: TGLintDynArray; aSrcOffset: TGLuint);
    procedure clearBufferiv(aBuffer: TGLenum; aDrawbuffer: TGLint; const aValues: TGLintDynArray);
    procedure clearBufferiv(aBuffer: TGLenum; aDrawbuffer: TGLint; aValues: IJSInt32Array);
    procedure clearBufferuiv(aBuffer: TGLenum; aDrawbuffer: TGLint; const aValues: TGLuintDynArray; aSrcOffset: TGLuint);
    procedure clearBufferuiv(aBuffer: TGLenum; aDrawbuffer: TGLint; aValues: IJSUint32Array; aSrcOffset: TGLuint);
    procedure clearBufferuiv(aBuffer: TGLenum; aDrawbuffer: TGLint; aValues: IJSUint32Array);
    procedure clearBufferuiv(aBuffer: TGLenum; aDrawbuffer: TGLint; const aValues: TGLuintDynArray);
    procedure clearBufferfi(aBuffer: TGLenum; aDrawbuffer: TGLint; aDepth: TGLfloat; aStencil: TGLint);
    function createQuery: IJSWebGLQuery;
    procedure deleteQuery(aQuery: IJSWebGLQuery);
    function isQuery(aQuery: IJSWebGLQuery): TGLboolean;
    procedure beginQuery(aTarget: TGLenum; aQuery: IJSWebGLQuery);
    procedure endQuery(aTarget: TGLenum);
    function getQuery(aTarget: TGLenum; aPname: TGLenum): Variant;
    function getQueryParameter(aQuery: IJSWebGLQuery; aPname: TGLenum): Variant;
    function createSampler: IJSWebGLSampler;
    procedure deleteSampler(aSampler: IJSWebGLSampler);
    function isSampler(aSampler: IJSWebGLSampler): TGLboolean;
    procedure bindSampler(aUnit_: TGLuint; aSampler: IJSWebGLSampler);
    procedure samplerParameteri(aSampler: IJSWebGLSampler; aPname: TGLenum; aParam: TGLint);
    procedure samplerParameterf(aSampler: IJSWebGLSampler; aPname: TGLenum; aParam: TGLfloat);
    function getSamplerParameter(aSampler: IJSWebGLSampler; aPname: TGLenum): Variant;
    function fenceSync(aCondition: TGLenum; aFlags: TGLbitfield): IJSWebGLSync;
    function isSync(aSync: IJSWebGLSync): TGLboolean;
    procedure deleteSync(aSync: IJSWebGLSync);
    function clientWaitSync(aSync: IJSWebGLSync; aFlags: TGLbitfield; aTimeout: TGLuint64): TGLenum;
    procedure waitSync(aSync: IJSWebGLSync; aFlags: TGLbitfield; aTimeout: TGLint64);
    function getSyncParameter(aSync: IJSWebGLSync; aPname: TGLenum): Variant;
    function createTransformFeedback: IJSWebGLTransformFeedback;
    procedure deleteTransformFeedback(aTf: IJSWebGLTransformFeedback);
    function isTransformFeedback(aTf: IJSWebGLTransformFeedback): TGLboolean;
    procedure bindTransformFeedback(aTarget: TGLenum; aTf: IJSWebGLTransformFeedback);
    procedure beginTransformFeedback(aPrimitiveMode: TGLenum);
    procedure endTransformFeedback;
    procedure transformFeedbackVaryings(aProgram_: IJSWebGLProgram; const aVaryings: TUnicodeStringDynArray; aBufferMode: TGLenum);
    function getTransformFeedbackVarying(aProgram_: IJSWebGLProgram; aIndex: TGLuint): IJSWebGLActiveInfo;
    procedure pauseTransformFeedback;
    procedure resumeTransformFeedback;
    procedure bindBufferBase(aTarget: TGLenum; aIndex: TGLuint; aBuffer: IJSWebGLBuffer);
    procedure bindBufferRange(aTarget: TGLenum; aIndex: TGLuint; aBuffer: IJSWebGLBuffer; aOffset: TGLintptr; aSize: TGLsizeiptr);
    function getIndexedParameter(aTarget: TGLenum; aIndex: TGLuint): Variant;
    function getUniformIndices(aProgram_: IJSWebGLProgram; const aUniformNames: TUnicodeStringDynArray): TGLuintDynArray;
    function getActiveUniforms(aProgram_: IJSWebGLProgram; const aUniformIndices: TGLuintDynArray; aPname: TGLenum): Variant;
    function getUniformBlockIndex(aProgram_: IJSWebGLProgram; const aUniformBlockName: UnicodeString): TGLuint;
    function getActiveUniformBlockParameter(aProgram_: IJSWebGLProgram; aUniformBlockIndex: TGLuint; aPname: TGLenum): Variant;
    function getActiveUniformBlockName(aProgram_: IJSWebGLProgram; aUniformBlockIndex: TGLuint): UnicodeString;
    procedure uniformBlockBinding(aProgram_: IJSWebGLProgram; aUniformBlockIndex: TGLuint; aUniformBlockBinding: TGLuint);
    function createVertexArray: IJSWebGLVertexArrayObject;
    procedure deleteVertexArray(aVertexArray: IJSWebGLVertexArrayObject);
    function isVertexArray(aVertexArray: IJSWebGLVertexArrayObject): TGLboolean;
    procedure bindVertexArray(aArray_: IJSWebGLVertexArrayObject);
    property canvas: TCanvasSource read _Getcanvas;
    property drawingBufferWidth: TGLsizei read _GetdrawingBufferWidth;
    property drawingBufferHeight: TGLsizei read _GetdrawingBufferHeight;
    property drawingBufferColorSpace: TPredefinedColorSpace read _GetdrawingBufferColorSpace write _SetdrawingBufferColorSpace;
    property unpackColorSpace: TPredefinedColorSpace read _GetunpackColorSpace write _SetunpackColorSpace;
  end;
  
  TJSWebGL2RenderingContext = class(TJSObject,IJSWebGL2RenderingContext)
  Private
  Protected
    function _Getcanvas: TCanvasSource; 
    function _GetdrawingBufferWidth: TGLsizei; 
    function _GetdrawingBufferHeight: TGLsizei; 
    function _GetdrawingBufferColorSpace: TPredefinedColorSpace; 
    function _GetunpackColorSpace: TPredefinedColorSpace; 
    procedure _SetdrawingBufferColorSpace(const aValue: TPredefinedColorSpace);
    procedure _SetunpackColorSpace(const aValue: TPredefinedColorSpace);
  Public
    Const
      DEPTH_BUFFER_BIT = $00000100;
      STENCIL_BUFFER_BIT = $00000400;
      COLOR_BUFFER_BIT = $00004000;
      POINTS = $0000;
      LINES = $0001;
      LINE_LOOP = $0002;
      LINE_STRIP = $0003;
      TRIANGLES = $0004;
      TRIANGLE_STRIP = $0005;
      TRIANGLE_FAN = $0006;
      ZERO = 0;
      ONE = 1;
      SRC_COLOR = $0300;
      ONE_MINUS_SRC_COLOR = $0301;
      SRC_ALPHA = $0302;
      ONE_MINUS_SRC_ALPHA = $0303;
      DST_ALPHA = $0304;
      ONE_MINUS_DST_ALPHA = $0305;
      DST_COLOR = $0306;
      ONE_MINUS_DST_COLOR = $0307;
      SRC_ALPHA_SATURATE = $0308;
      FUNC_ADD = $8006;
      BLEND_EQUATION = $8009;
      BLEND_EQUATION_RGB = $8009;
      BLEND_EQUATION_ALPHA = $883D;
      FUNC_SUBTRACT = $800A;
      FUNC_REVERSE_SUBTRACT = $800B;
      BLEND_DST_RGB = $80C8;
      BLEND_SRC_RGB = $80C9;
      BLEND_DST_ALPHA = $80CA;
      BLEND_SRC_ALPHA = $80CB;
      CONSTANT_COLOR = $8001;
      ONE_MINUS_CONSTANT_COLOR = $8002;
      CONSTANT_ALPHA = $8003;
      ONE_MINUS_CONSTANT_ALPHA = $8004;
      BLEND_COLOR = $8005;
      ARRAY_BUFFER = $8892;
      ELEMENT_ARRAY_BUFFER = $8893;
      ARRAY_BUFFER_BINDING = $8894;
      ELEMENT_ARRAY_BUFFER_BINDING = $8895;
      STREAM_DRAW = $88E0;
      STATIC_DRAW = $88E4;
      DYNAMIC_DRAW = $88E8;
      BUFFER_SIZE = $8764;
      BUFFER_USAGE = $8765;
      CURRENT_VERTEX_ATTRIB = $8626;
      FRONT = $0404;
      BACK = $0405;
      FRONT_AND_BACK = $0408;
      CULL_FACE = $0B44;
      BLEND = $0BE2;
      DITHER = $0BD0;
      STENCIL_TEST = $0B90;
      DEPTH_TEST = $0B71;
      SCISSOR_TEST = $0C11;
      POLYGON_OFFSET_FILL = $8037;
      SAMPLE_ALPHA_TO_COVERAGE = $809E;
      SAMPLE_COVERAGE = $80A0;
      NO_ERROR = 0;
      INVALID_ENUM = $0500;
      INVALID_VALUE = $0501;
      INVALID_OPERATION = $0502;
      OUT_OF_MEMORY = $0505;
      CW = $0900;
      CCW = $0901;
      LINE_WIDTH = $0B21;
      ALIASED_POINT_SIZE_RANGE = $846D;
      ALIASED_LINE_WIDTH_RANGE = $846E;
      CULL_FACE_MODE = $0B45;
      FRONT_FACE = $0B46;
      DEPTH_RANGE = $0B70;
      DEPTH_WRITEMASK = $0B72;
      DEPTH_CLEAR_VALUE = $0B73;
      DEPTH_FUNC = $0B74;
      STENCIL_CLEAR_VALUE = $0B91;
      STENCIL_FUNC = $0B92;
      STENCIL_FAIL = $0B94;
      STENCIL_PASS_DEPTH_FAIL = $0B95;
      STENCIL_PASS_DEPTH_PASS = $0B96;
      STENCIL_REF = $0B97;
      STENCIL_VALUE_MASK = $0B93;
      STENCIL_WRITEMASK = $0B98;
      STENCIL_BACK_FUNC = $8800;
      STENCIL_BACK_FAIL = $8801;
      STENCIL_BACK_PASS_DEPTH_FAIL = $8802;
      STENCIL_BACK_PASS_DEPTH_PASS = $8803;
      STENCIL_BACK_REF = $8CA3;
      STENCIL_BACK_VALUE_MASK = $8CA4;
      STENCIL_BACK_WRITEMASK = $8CA5;
      VIEWPORT_ = $0BA2;
      SCISSOR_BOX = $0C10;
      COLOR_CLEAR_VALUE = $0C22;
      COLOR_WRITEMASK = $0C23;
      UNPACK_ALIGNMENT = $0CF5;
      PACK_ALIGNMENT = $0D05;
      MAX_TEXTURE_SIZE = $0D33;
      MAX_VIEWPORT_DIMS = $0D3A;
      SUBPIXEL_BITS = $0D50;
      RED_BITS = $0D52;
      GREEN_BITS = $0D53;
      BLUE_BITS = $0D54;
      ALPHA_BITS = $0D55;
      DEPTH_BITS = $0D56;
      STENCIL_BITS = $0D57;
      POLYGON_OFFSET_UNITS = $2A00;
      POLYGON_OFFSET_FACTOR = $8038;
      TEXTURE_BINDING_2D = $8069;
      SAMPLE_BUFFERS = $80A8;
      SAMPLES = $80A9;
      SAMPLE_COVERAGE_VALUE = $80AA;
      SAMPLE_COVERAGE_INVERT = $80AB;
      COMPRESSED_TEXTURE_FORMATS = $86A3;
      DONT_CARE = $1100;
      FASTEST = $1101;
      NICEST = $1102;
      GENERATE_MIPMAP_HINT = $8192;
      BYTE = $1400;
      UNSIGNED_BYTE = $1401;
      SHORT = $1402;
      UNSIGNED_SHORT = $1403;
      INT = $1404;
      UNSIGNED_INT = $1405;
      FLOAT = $1406;
      DEPTH_COMPONENT = $1902;
      ALPHA = $1906;
      RGB = $1907;
      RGBA = $1908;
      LUMINANCE = $1909;
      LUMINANCE_ALPHA = $190A;
      UNSIGNED_SHORT_4_4_4_4 = $8033;
      UNSIGNED_SHORT_5_5_5_1 = $8034;
      UNSIGNED_SHORT_5_6_5 = $8363;
      FRAGMENT_SHADER = $8B30;
      VERTEX_SHADER = $8B31;
      MAX_VERTEX_ATTRIBS = $8869;
      MAX_VERTEX_UNIFORM_VECTORS = $8DFB;
      MAX_VARYING_VECTORS = $8DFC;
      MAX_COMBINED_TEXTURE_IMAGE_UNITS = $8B4D;
      MAX_VERTEX_TEXTURE_IMAGE_UNITS = $8B4C;
      MAX_TEXTURE_IMAGE_UNITS = $8872;
      MAX_FRAGMENT_UNIFORM_VECTORS = $8DFD;
      SHADER_TYPE = $8B4F;
      DELETE_STATUS = $8B80;
      LINK_STATUS = $8B82;
      VALIDATE_STATUS = $8B83;
      ATTACHED_SHADERS = $8B85;
      ACTIVE_UNIFORMS = $8B86;
      ACTIVE_ATTRIBUTES = $8B89;
      SHADING_LANGUAGE_VERSION = $8B8C;
      CURRENT_PROGRAM = $8B8D;
      NEVER = $0200;
      LESS = $0201;
      EQUAL = $0202;
      LEQUAL = $0203;
      GREATER = $0204;
      NOTEQUAL = $0205;
      GEQUAL = $0206;
      ALWAYS = $0207;
      KEEP = $1E00;
      REPLACE = $1E01;
      INCR = $1E02;
      DECR = $1E03;
      INVERT = $150A;
      INCR_WRAP = $8507;
      DECR_WRAP = $8508;
      VENDOR = $1F00;
      RENDERER = $1F01;
      VERSION = $1F02;
      NEAREST = $2600;
      LINEAR = $2601;
      NEAREST_MIPMAP_NEAREST = $2700;
      LINEAR_MIPMAP_NEAREST = $2701;
      NEAREST_MIPMAP_LINEAR = $2702;
      LINEAR_MIPMAP_LINEAR = $2703;
      TEXTURE_MAG_FILTER = $2800;
      TEXTURE_MIN_FILTER = $2801;
      TEXTURE_WRAP_S = $2802;
      TEXTURE_WRAP_T = $2803;
      TEXTURE_2D = $0DE1;
      TEXTURE = $1702;
      TEXTURE_CUBE_MAP = $8513;
      TEXTURE_BINDING_CUBE_MAP = $8514;
      TEXTURE_CUBE_MAP_POSITIVE_X = $8515;
      TEXTURE_CUBE_MAP_NEGATIVE_X = $8516;
      TEXTURE_CUBE_MAP_POSITIVE_Y = $8517;
      TEXTURE_CUBE_MAP_NEGATIVE_Y = $8518;
      TEXTURE_CUBE_MAP_POSITIVE_Z = $8519;
      TEXTURE_CUBE_MAP_NEGATIVE_Z = $851A;
      MAX_CUBE_MAP_TEXTURE_SIZE = $851C;
      TEXTURE0 = $84C0;
      TEXTURE1 = $84C1;
      TEXTURE2 = $84C2;
      TEXTURE3 = $84C3;
      TEXTURE4 = $84C4;
      TEXTURE5 = $84C5;
      TEXTURE6 = $84C6;
      TEXTURE7 = $84C7;
      TEXTURE8 = $84C8;
      TEXTURE9 = $84C9;
      TEXTURE10 = $84CA;
      TEXTURE11 = $84CB;
      TEXTURE12 = $84CC;
      TEXTURE13 = $84CD;
      TEXTURE14 = $84CE;
      TEXTURE15 = $84CF;
      TEXTURE16 = $84D0;
      TEXTURE17 = $84D1;
      TEXTURE18 = $84D2;
      TEXTURE19 = $84D3;
      TEXTURE20 = $84D4;
      TEXTURE21 = $84D5;
      TEXTURE22 = $84D6;
      TEXTURE23 = $84D7;
      TEXTURE24 = $84D8;
      TEXTURE25 = $84D9;
      TEXTURE26 = $84DA;
      TEXTURE27 = $84DB;
      TEXTURE28 = $84DC;
      TEXTURE29 = $84DD;
      TEXTURE30 = $84DE;
      TEXTURE31 = $84DF;
      ACTIVE_TEXTURE = $84E0;
      REPEAT_ = $2901;
      CLAMP_TO_EDGE = $812F;
      MIRRORED_REPEAT = $8370;
      FLOAT_VEC2 = $8B50;
      FLOAT_VEC3 = $8B51;
      FLOAT_VEC4 = $8B52;
      INT_VEC2 = $8B53;
      INT_VEC3 = $8B54;
      INT_VEC4 = $8B55;
      BOOL = $8B56;
      BOOL_VEC2 = $8B57;
      BOOL_VEC3 = $8B58;
      BOOL_VEC4 = $8B59;
      FLOAT_MAT2 = $8B5A;
      FLOAT_MAT3 = $8B5B;
      FLOAT_MAT4 = $8B5C;
      SAMPLER_2D = $8B5E;
      SAMPLER_CUBE = $8B60;
      VERTEX_ATTRIB_ARRAY_ENABLED = $8622;
      VERTEX_ATTRIB_ARRAY_SIZE = $8623;
      VERTEX_ATTRIB_ARRAY_STRIDE = $8624;
      VERTEX_ATTRIB_ARRAY_TYPE = $8625;
      VERTEX_ATTRIB_ARRAY_NORMALIZED = $886A;
      VERTEX_ATTRIB_ARRAY_POINTER = $8645;
      VERTEX_ATTRIB_ARRAY_BUFFER_BINDING = $889F;
      IMPLEMENTATION_COLOR_READ_TYPE = $8B9A;
      IMPLEMENTATION_COLOR_READ_FORMAT = $8B9B;
      COMPILE_STATUS = $8B81;
      LOW_FLOAT = $8DF0;
      MEDIUM_FLOAT = $8DF1;
      HIGH_FLOAT = $8DF2;
      LOW_INT = $8DF3;
      MEDIUM_INT = $8DF4;
      HIGH_INT = $8DF5;
      FRAMEBUFFER = $8D40;
      RENDERBUFFER = $8D41;
      RGBA4 = $8056;
      RGB5_A1 = $8057;
      RGB565 = $8D62;
      DEPTH_COMPONENT16 = $81A5;
      STENCIL_INDEX8 = $8D48;
      DEPTH_STENCIL = $84F9;
      RENDERBUFFER_WIDTH = $8D42;
      RENDERBUFFER_HEIGHT = $8D43;
      RENDERBUFFER_INTERNAL_FORMAT = $8D44;
      RENDERBUFFER_RED_SIZE = $8D50;
      RENDERBUFFER_GREEN_SIZE = $8D51;
      RENDERBUFFER_BLUE_SIZE = $8D52;
      RENDERBUFFER_ALPHA_SIZE = $8D53;
      RENDERBUFFER_DEPTH_SIZE = $8D54;
      RENDERBUFFER_STENCIL_SIZE = $8D55;
      FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE = $8CD0;
      FRAMEBUFFER_ATTACHMENT_OBJECT_NAME = $8CD1;
      FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL = $8CD2;
      FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE = $8CD3;
      COLOR_ATTACHMENT0 = $8CE0;
      DEPTH_ATTACHMENT = $8D00;
      STENCIL_ATTACHMENT = $8D20;
      DEPTH_STENCIL_ATTACHMENT = $821A;
      NONE = 0;
      FRAMEBUFFER_COMPLETE = $8CD5;
      FRAMEBUFFER_INCOMPLETE_ATTACHMENT = $8CD6;
      FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT = $8CD7;
      FRAMEBUFFER_INCOMPLETE_DIMENSIONS = $8CD9;
      FRAMEBUFFER_UNSUPPORTED = $8CDD;
      FRAMEBUFFER_BINDING = $8CA6;
      RENDERBUFFER_BINDING = $8CA7;
      MAX_RENDERBUFFER_SIZE = $84E8;
      INVALID_FRAMEBUFFER_OPERATION = $0506;
      UNPACK_FLIP_Y_WEBGL = $9240;
      UNPACK_PREMULTIPLY_ALPHA_WEBGL = $9241;
      CONTEXT_LOST_WEBGL = $9242;
      UNPACK_COLORSPACE_CONVERSION_WEBGL = $9243;
      BROWSER_DEFAULT_WEBGL = $9244;
      READ_BUFFER = $0C02;
      UNPACK_ROW_LENGTH = $0CF2;
      UNPACK_SKIP_ROWS = $0CF3;
      UNPACK_SKIP_PIXELS = $0CF4;
      PACK_ROW_LENGTH = $0D02;
      PACK_SKIP_ROWS = $0D03;
      PACK_SKIP_PIXELS = $0D04;
      COLOR = $1800;
      DEPTH = $1801;
      STENCIL = $1802;
      RED = $1903;
      RGB8 = $8051;
      RGBA8 = $8058;
      RGB10_A2 = $8059;
      TEXTURE_BINDING_3D = $806A;
      UNPACK_SKIP_IMAGES = $806D;
      UNPACK_IMAGE_HEIGHT = $806E;
      TEXTURE_3D = $806F;
      TEXTURE_WRAP_R = $8072;
      MAX_3D_TEXTURE_SIZE = $8073;
      UNSIGNED_INT_2_10_10_10_REV = $8368;
      MAX_ELEMENTS_VERTICES = $80E8;
      MAX_ELEMENTS_INDICES = $80E9;
      TEXTURE_MIN_LOD = $813A;
      TEXTURE_MAX_LOD = $813B;
      TEXTURE_BASE_LEVEL = $813C;
      TEXTURE_MAX_LEVEL = $813D;
      MIN = $8007;
      MAX = $8008;
      DEPTH_COMPONENT24 = $81A6;
      MAX_TEXTURE_LOD_BIAS = $84FD;
      TEXTURE_COMPARE_MODE = $884C;
      TEXTURE_COMPARE_FUNC = $884D;
      CURRENT_QUERY = $8865;
      QUERY_RESULT = $8866;
      QUERY_RESULT_AVAILABLE = $8867;
      STREAM_READ = $88E1;
      STREAM_COPY = $88E2;
      STATIC_READ = $88E5;
      STATIC_COPY = $88E6;
      DYNAMIC_READ = $88E9;
      DYNAMIC_COPY = $88EA;
      MAX_DRAW_BUFFERS = $8824;
      DRAW_BUFFER0 = $8825;
      DRAW_BUFFER1 = $8826;
      DRAW_BUFFER2 = $8827;
      DRAW_BUFFER3 = $8828;
      DRAW_BUFFER4 = $8829;
      DRAW_BUFFER5 = $882A;
      DRAW_BUFFER6 = $882B;
      DRAW_BUFFER7 = $882C;
      DRAW_BUFFER8 = $882D;
      DRAW_BUFFER9 = $882E;
      DRAW_BUFFER10 = $882F;
      DRAW_BUFFER11 = $8830;
      DRAW_BUFFER12 = $8831;
      DRAW_BUFFER13 = $8832;
      DRAW_BUFFER14 = $8833;
      DRAW_BUFFER15 = $8834;
      MAX_FRAGMENT_UNIFORM_COMPONENTS = $8B49;
      MAX_VERTEX_UNIFORM_COMPONENTS = $8B4A;
      SAMPLER_3D = $8B5F;
      SAMPLER_2D_SHADOW = $8B62;
      FRAGMENT_SHADER_DERIVATIVE_HINT = $8B8B;
      PIXEL_PACK_BUFFER = $88EB;
      PIXEL_UNPACK_BUFFER = $88EC;
      PIXEL_PACK_BUFFER_BINDING = $88ED;
      PIXEL_UNPACK_BUFFER_BINDING = $88EF;
      FLOAT_MAT2x3 = $8B65;
      FLOAT_MAT2x4 = $8B66;
      FLOAT_MAT3x2 = $8B67;
      FLOAT_MAT3x4 = $8B68;
      FLOAT_MAT4x2 = $8B69;
      FLOAT_MAT4x3 = $8B6A;
      SRGB = $8C40;
      SRGB8 = $8C41;
      SRGB8_ALPHA8 = $8C43;
      COMPARE_REF_TO_TEXTURE = $884E;
      RGBA32F = $8814;
      RGB32F = $8815;
      RGBA16F = $881A;
      RGB16F = $881B;
      VERTEX_ATTRIB_ARRAY_INTEGER = $88FD;
      MAX_ARRAY_TEXTURE_LAYERS = $88FF;
      MIN_PROGRAM_TEXEL_OFFSET = $8904;
      MAX_PROGRAM_TEXEL_OFFSET = $8905;
      MAX_VARYING_COMPONENTS = $8B4B;
      TEXTURE_2D_ARRAY = $8C1A;
      TEXTURE_BINDING_2D_ARRAY = $8C1D;
      R11F_G11F_B10F = $8C3A;
      UNSIGNED_INT_10F_11F_11F_REV = $8C3B;
      RGB9_E5 = $8C3D;
      UNSIGNED_INT_5_9_9_9_REV = $8C3E;
      TRANSFORM_FEEDBACK_BUFFER_MODE = $8C7F;
      MAX_TRANSFORM_FEEDBACK_SEPARATE_COMPONENTS = $8C80;
      TRANSFORM_FEEDBACK_VARYINGS = $8C83;
      TRANSFORM_FEEDBACK_BUFFER_START = $8C84;
      TRANSFORM_FEEDBACK_BUFFER_SIZE = $8C85;
      TRANSFORM_FEEDBACK_PRIMITIVES_WRITTEN = $8C88;
      RASTERIZER_DISCARD = $8C89;
      MAX_TRANSFORM_FEEDBACK_INTERLEAVED_COMPONENTS = $8C8A;
      MAX_TRANSFORM_FEEDBACK_SEPARATE_ATTRIBS = $8C8B;
      INTERLEAVED_ATTRIBS = $8C8C;
      SEPARATE_ATTRIBS = $8C8D;
      TRANSFORM_FEEDBACK_BUFFER = $8C8E;
      TRANSFORM_FEEDBACK_BUFFER_BINDING = $8C8F;
      RGBA32UI = $8D70;
      RGB32UI = $8D71;
      RGBA16UI = $8D76;
      RGB16UI = $8D77;
      RGBA8UI = $8D7C;
      RGB8UI = $8D7D;
      RGBA32I = $8D82;
      RGB32I = $8D83;
      RGBA16I = $8D88;
      RGB16I = $8D89;
      RGBA8I = $8D8E;
      RGB8I = $8D8F;
      RED_INTEGER = $8D94;
      RGB_INTEGER = $8D98;
      RGBA_INTEGER = $8D99;
      SAMPLER_2D_ARRAY = $8DC1;
      SAMPLER_2D_ARRAY_SHADOW = $8DC4;
      SAMPLER_CUBE_SHADOW = $8DC5;
      UNSIGNED_INT_VEC2 = $8DC6;
      UNSIGNED_INT_VEC3 = $8DC7;
      UNSIGNED_INT_VEC4 = $8DC8;
      INT_SAMPLER_2D = $8DCA;
      INT_SAMPLER_3D = $8DCB;
      INT_SAMPLER_CUBE = $8DCC;
      INT_SAMPLER_2D_ARRAY = $8DCF;
      UNSIGNED_INT_SAMPLER_2D = $8DD2;
      UNSIGNED_INT_SAMPLER_3D = $8DD3;
      UNSIGNED_INT_SAMPLER_CUBE = $8DD4;
      UNSIGNED_INT_SAMPLER_2D_ARRAY = $8DD7;
      DEPTH_COMPONENT32F = $8CAC;
      DEPTH32F_STENCIL8 = $8CAD;
      FLOAT_32_UNSIGNED_INT_24_8_REV = $8DAD;
      FRAMEBUFFER_ATTACHMENT_COLOR_ENCODING = $8210;
      FRAMEBUFFER_ATTACHMENT_COMPONENT_TYPE = $8211;
      FRAMEBUFFER_ATTACHMENT_RED_SIZE = $8212;
      FRAMEBUFFER_ATTACHMENT_GREEN_SIZE = $8213;
      FRAMEBUFFER_ATTACHMENT_BLUE_SIZE = $8214;
      FRAMEBUFFER_ATTACHMENT_ALPHA_SIZE = $8215;
      FRAMEBUFFER_ATTACHMENT_DEPTH_SIZE = $8216;
      FRAMEBUFFER_ATTACHMENT_STENCIL_SIZE = $8217;
      FRAMEBUFFER_DEFAULT = $8218;
      UNSIGNED_INT_24_8 = $84FA;
      DEPTH24_STENCIL8 = $88F0;
      UNSIGNED_NORMALIZED = $8C17;
      DRAW_FRAMEBUFFER_BINDING = $8CA6;
      READ_FRAMEBUFFER = $8CA8;
      DRAW_FRAMEBUFFER = $8CA9;
      READ_FRAMEBUFFER_BINDING = $8CAA;
      RENDERBUFFER_SAMPLES = $8CAB;
      FRAMEBUFFER_ATTACHMENT_TEXTURE_LAYER = $8CD4;
      MAX_COLOR_ATTACHMENTS = $8CDF;
      COLOR_ATTACHMENT1 = $8CE1;
      COLOR_ATTACHMENT2 = $8CE2;
      COLOR_ATTACHMENT3 = $8CE3;
      COLOR_ATTACHMENT4 = $8CE4;
      COLOR_ATTACHMENT5 = $8CE5;
      COLOR_ATTACHMENT6 = $8CE6;
      COLOR_ATTACHMENT7 = $8CE7;
      COLOR_ATTACHMENT8 = $8CE8;
      COLOR_ATTACHMENT9 = $8CE9;
      COLOR_ATTACHMENT10 = $8CEA;
      COLOR_ATTACHMENT11 = $8CEB;
      COLOR_ATTACHMENT12 = $8CEC;
      COLOR_ATTACHMENT13 = $8CED;
      COLOR_ATTACHMENT14 = $8CEE;
      COLOR_ATTACHMENT15 = $8CEF;
      FRAMEBUFFER_INCOMPLETE_MULTISAMPLE = $8D56;
      MAX_SAMPLES = $8D57;
      HALF_FLOAT = $140B;
      RG = $8227;
      RG_INTEGER = $8228;
      R8 = $8229;
      RG8 = $822B;
      R16F = $822D;
      R32F = $822E;
      RG16F = $822F;
      RG32F = $8230;
      R8I = $8231;
      R8UI = $8232;
      R16I = $8233;
      R16UI = $8234;
      R32I = $8235;
      R32UI = $8236;
      RG8I = $8237;
      RG8UI = $8238;
      RG16I = $8239;
      RG16UI = $823A;
      RG32I = $823B;
      RG32UI = $823C;
      VERTEX_ARRAY_BINDING = $85B5;
      R8_SNORM = $8F94;
      RG8_SNORM = $8F95;
      RGB8_SNORM = $8F96;
      RGBA8_SNORM = $8F97;
      SIGNED_NORMALIZED = $8F9C;
      COPY_READ_BUFFER = $8F36;
      COPY_WRITE_BUFFER = $8F37;
      COPY_READ_BUFFER_BINDING = $8F36;
      COPY_WRITE_BUFFER_BINDING = $8F37;
      UNIFORM_BUFFER = $8A11;
      UNIFORM_BUFFER_BINDING = $8A28;
      UNIFORM_BUFFER_START = $8A29;
      UNIFORM_BUFFER_SIZE = $8A2A;
      MAX_VERTEX_UNIFORM_BLOCKS = $8A2B;
      MAX_FRAGMENT_UNIFORM_BLOCKS = $8A2D;
      MAX_COMBINED_UNIFORM_BLOCKS = $8A2E;
      MAX_UNIFORM_BUFFER_BINDINGS = $8A2F;
      MAX_UNIFORM_BLOCK_SIZE = $8A30;
      MAX_COMBINED_VERTEX_UNIFORM_COMPONENTS = $8A31;
      MAX_COMBINED_FRAGMENT_UNIFORM_COMPONENTS = $8A33;
      UNIFORM_BUFFER_OFFSET_ALIGNMENT = $8A34;
      ACTIVE_UNIFORM_BLOCKS = $8A36;
      UNIFORM_TYPE = $8A37;
      UNIFORM_SIZE = $8A38;
      UNIFORM_BLOCK_INDEX = $8A3A;
      UNIFORM_OFFSET = $8A3B;
      UNIFORM_ARRAY_STRIDE = $8A3C;
      UNIFORM_MATRIX_STRIDE = $8A3D;
      UNIFORM_IS_ROW_MAJOR = $8A3E;
      UNIFORM_BLOCK_BINDING = $8A3F;
      UNIFORM_BLOCK_DATA_SIZE = $8A40;
      UNIFORM_BLOCK_ACTIVE_UNIFORMS = $8A42;
      UNIFORM_BLOCK_ACTIVE_UNIFORM_INDICES = $8A43;
      UNIFORM_BLOCK_REFERENCED_BY_VERTEX_SHADER = $8A44;
      UNIFORM_BLOCK_REFERENCED_BY_FRAGMENT_SHADER = $8A46;
      INVALID_INDEX = $FFFFFFFF;
      MAX_VERTEX_OUTPUT_COMPONENTS = $9122;
      MAX_FRAGMENT_INPUT_COMPONENTS = $9125;
      MAX_SERVER_WAIT_TIMEOUT = $9111;
      OBJECT_TYPE = $9112;
      SYNC_CONDITION = $9113;
      SYNC_STATUS = $9114;
      SYNC_FLAGS = $9115;
      SYNC_FENCE = $9116;
      SYNC_GPU_COMMANDS_COMPLETE = $9117;
      UNSIGNALED = $9118;
      SIGNALED = $9119;
      ALREADY_SIGNALED = $911A;
      TIMEOUT_EXPIRED = $911B;
      CONDITION_SATISFIED = $911C;
      WAIT_FAILED = $911D;
      SYNC_FLUSH_COMMANDS_BIT = $00000001;
      VERTEX_ATTRIB_ARRAY_DIVISOR = $88FE;
      ANY_SAMPLES_PASSED = $8C2F;
      ANY_SAMPLES_PASSED_CONSERVATIVE = $8D6A;
      SAMPLER_BINDING = $8919;
      RGB10_A2UI = $906F;
      INT_2_10_10_10_REV = $8D9F;
      TRANSFORM_FEEDBACK = $8E22;
      TRANSFORM_FEEDBACK_PAUSED = $8E23;
      TRANSFORM_FEEDBACK_ACTIVE = $8E24;
      TRANSFORM_FEEDBACK_BINDING = $8E25;
      TEXTURE_IMMUTABLE_FORMAT = $912F;
      MAX_ELEMENT_INDEX = $8D6B;
      TEXTURE_IMMUTABLE_LEVELS = $82DF;
      TIMEOUT_IGNORED = -1;
      MAX_CLIENT_WAIT_TIMEOUT_WEBGL = $9247;
  Public
    function getContextAttributes: IJSWebGLContextAttributes; overload;
    function isContextLost: Boolean; overload;
    function getSupportedExtensions: TUnicodeStringDynArray; overload;
    function getExtension(const aName: UnicodeString): IJSObject; overload;
    procedure activeTexture(aTexture: TGLenum); overload;
    procedure attachShader(aProgram_: IJSWebGLProgram; aShader: IJSWebGLShader); overload;
    procedure bindAttribLocation(aProgram_: IJSWebGLProgram; aIndex: TGLuint; const aName: UnicodeString); overload;
    procedure bindBuffer(aTarget: TGLenum; aBuffer: IJSWebGLBuffer); overload;
    procedure bindFramebuffer(aTarget: TGLenum; aFramebuffer: IJSWebGLFramebuffer); overload;
    procedure bindRenderbuffer(aTarget: TGLenum; aRenderbuffer: IJSWebGLRenderbuffer); overload;
    procedure bindTexture(aTarget: TGLenum; aTexture: IJSWebGLTexture); overload;
    procedure blendColor(aRed: TGLfloat; aGreen: TGLfloat; aBlue: TGLfloat; aAlpha: TGLfloat); overload;
    procedure blendEquation(aMode: TGLenum); overload;
    procedure blendEquationSeparate(aModeRGB: TGLenum; aModeAlpha: TGLenum); overload;
    procedure blendFunc(aSfactor: TGLenum; aDfactor: TGLenum); overload;
    procedure blendFuncSeparate(aSrcRGB: TGLenum; aDstRGB: TGLenum; aSrcAlpha: TGLenum; aDstAlpha: TGLenum); overload;
    function checkFramebufferStatus(aTarget: TGLenum): TGLenum; overload;
    procedure clear(aMask: TGLbitfield); overload;
    procedure clearColor(aRed: TGLfloat; aGreen: TGLfloat; aBlue: TGLfloat; aAlpha: TGLfloat); overload;
    procedure clearDepth(aDepth: TGLclampf); overload;
    procedure clearStencil(aS_: TGLint); overload;
    procedure colorMask(aRed: TGLboolean; aGreen: TGLboolean; aBlue: TGLboolean; aAlpha: TGLboolean); overload;
    procedure compileShader(aShader: IJSWebGLShader); overload;
    procedure copyTexImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLenum; aX: TGLint; aY: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aBorder: TGLint); overload;
    procedure copyTexSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aX: TGLint; aY: TGLint; aWidth: TGLsizei; aHeight: TGLsizei); overload;
    function createBuffer: IJSWebGLBuffer; overload;
    function createFramebuffer: IJSWebGLFramebuffer; overload;
    function createProgram: IJSWebGLProgram; overload;
    function createRenderbuffer: IJSWebGLRenderbuffer; overload;
    function createShader(aType_: TGLenum): IJSWebGLShader; overload;
    function createTexture: IJSWebGLTexture; overload;
    procedure cullFace(aMode: TGLenum); overload;
    procedure deleteBuffer(aBuffer: IJSWebGLBuffer); overload;
    procedure deleteFramebuffer(aFramebuffer: IJSWebGLFramebuffer); overload;
    procedure deleteProgram(aProgram_: IJSWebGLProgram); overload;
    procedure deleteRenderbuffer(aRenderbuffer: IJSWebGLRenderbuffer); overload;
    procedure deleteShader(aShader: IJSWebGLShader); overload;
    procedure deleteTexture(aTexture: IJSWebGLTexture); overload;
    procedure depthFunc(aFunc: TGLenum); overload;
    procedure depthMask(aFlag: TGLboolean); overload;
    procedure depthRange(aZNear: TGLclampf; aZFar: TGLclampf); overload;
    procedure detachShader(aProgram_: IJSWebGLProgram; aShader: IJSWebGLShader); overload;
    procedure disable(aCap: TGLenum); overload;
    procedure disableVertexAttribArray(aIndex: TGLuint); overload;
    procedure drawArrays(aMode: TGLenum; aFirst: TGLint; aCount: TGLsizei); overload;
    procedure drawElements(aMode: TGLenum; aCount: TGLsizei; aType_: TGLenum; aOffset: TGLintptr); overload;
    procedure enable(aCap: TGLenum); overload;
    procedure enableVertexAttribArray(aIndex: TGLuint); overload;
    procedure finish; overload;
    procedure flush; overload;
    procedure framebufferRenderbuffer(aTarget: TGLenum; aAttachment: TGLenum; aRenderbuffertarget: TGLenum; aRenderbuffer: IJSWebGLRenderbuffer); overload;
    procedure framebufferTexture2D(aTarget: TGLenum; aAttachment: TGLenum; aTextarget: TGLenum; aTexture: IJSWebGLTexture; aLevel: TGLint); overload;
    procedure frontFace(aMode: TGLenum); overload;
    procedure generateMipmap(aTarget: TGLenum); overload;
    function getActiveAttrib(aProgram_: IJSWebGLProgram; aIndex: TGLuint): IJSWebGLActiveInfo; overload;
    function getActiveUniform(aProgram_: IJSWebGLProgram; aIndex: TGLuint): IJSWebGLActiveInfo; overload;
    function getAttachedShaders(aProgram_: IJSWebGLProgram): TJSWebGLShaderDynArray; overload;
    function getAttribLocation(aProgram_: IJSWebGLProgram; const aName: UnicodeString): TGLint; overload;
    function getBufferParameter(aTarget: TGLenum; aPname: TGLenum): Variant; overload;
    function getParameter(aPname: TGLenum): Variant; overload;
    function getError: TGLenum; overload;
    function getFramebufferAttachmentParameter(aTarget: TGLenum; aAttachment: TGLenum; aPname: TGLenum): Variant; overload;
    function getProgramParameter(aProgram_: IJSWebGLProgram; aPname: TGLenum): Variant; overload;
    function getProgramInfoLog(aProgram_: IJSWebGLProgram): UnicodeString; overload;
    function getRenderbufferParameter(aTarget: TGLenum; aPname: TGLenum): Variant; overload;
    function getShaderParameter(aShader: IJSWebGLShader; aPname: TGLenum): Variant; overload;
    function getShaderPrecisionFormat(aShadertype: TGLenum; aPrecisiontype: TGLenum): IJSWebGLShaderPrecisionFormat; overload;
    function getShaderInfoLog(aShader: IJSWebGLShader): UnicodeString; overload;
    function getShaderSource(aShader: IJSWebGLShader): UnicodeString; overload;
    function getTexParameter(aTarget: TGLenum; aPname: TGLenum): Variant; overload;
    function getUniform(aProgram_: IJSWebGLProgram; aLocation: IJSWebGLUniformLocation): Variant; overload;
    function getUniformLocation(aProgram_: IJSWebGLProgram; const aName: UnicodeString): IJSWebGLUniformLocation; overload;
    function getVertexAttrib(aIndex: TGLuint; aPname: TGLenum): Variant; overload;
    function getVertexAttribOffset(aIndex: TGLuint; aPname: TGLenum): TGLintptr; overload;
    procedure hint(aTarget: TGLenum; aMode: TGLenum); overload;
    function isBuffer(aBuffer: IJSWebGLBuffer): TGLboolean; overload;
    function isEnabled(aCap: TGLenum): TGLboolean; overload;
    function isFramebuffer(aFramebuffer: IJSWebGLFramebuffer): TGLboolean; overload;
    function isProgram(aProgram_: IJSWebGLProgram): TGLboolean; overload;
    function isRenderbuffer(aRenderbuffer: IJSWebGLRenderbuffer): TGLboolean; overload;
    function isShader(aShader: IJSWebGLShader): TGLboolean; overload;
    function isTexture(aTexture: IJSWebGLTexture): TGLboolean; overload;
    procedure lineWidth(aWidth: TGLfloat); overload;
    procedure linkProgram(aProgram_: IJSWebGLProgram); overload;
    procedure pixelStorei(aPname: TGLenum; aParam: TGLint); overload;
    procedure polygonOffset(aFactor: TGLfloat; aUnits: TGLfloat); overload;
    procedure renderbufferStorage(aTarget: TGLenum; aInternalformat: TGLenum; aWidth: TGLsizei; aHeight: TGLsizei); overload;
    procedure sampleCoverage(aValue: TGLclampf; aInvert: TGLboolean); overload;
    procedure scissor(aX: TGLint; aY: TGLint; aWidth: TGLsizei; aHeight: TGLsizei); overload;
    procedure shaderSource(aShader: IJSWebGLShader; const aSource: UnicodeString); overload;
    procedure stencilFunc(aFunc: TGLenum; aRef: TGLint; aMask: TGLuint); overload;
    procedure stencilFuncSeparate(aFace: TGLenum; aFunc: TGLenum; aRef: TGLint; aMask: TGLuint); overload;
    procedure stencilMask(aMask: TGLuint); overload;
    procedure stencilMaskSeparate(aFace: TGLenum; aMask: TGLuint); overload;
    procedure stencilOp(aFail: TGLenum; aZfail: TGLenum; aZpass: TGLenum); overload;
    procedure stencilOpSeparate(aFace: TGLenum; aFail: TGLenum; aZfail: TGLenum; aZpass: TGLenum); overload;
    procedure texParameterf(aTarget: TGLenum; aPname: TGLenum; aParam: TGLfloat); overload;
    procedure texParameteri(aTarget: TGLenum; aPname: TGLenum; aParam: TGLint); overload;
    procedure uniform1f(aLocation: IJSWebGLUniformLocation; aX: TGLfloat); overload;
    procedure uniform2f(aLocation: IJSWebGLUniformLocation; aX: TGLfloat; aY: TGLfloat); overload;
    procedure uniform3f(aLocation: IJSWebGLUniformLocation; aX: TGLfloat; aY: TGLfloat; aZ: TGLfloat); overload;
    procedure uniform4f(aLocation: IJSWebGLUniformLocation; aX: TGLfloat; aY: TGLfloat; aZ: TGLfloat; aW: TGLfloat); overload;
    procedure uniform1i(aLocation: IJSWebGLUniformLocation; aX: TGLint); overload;
    procedure uniform2i(aLocation: IJSWebGLUniformLocation; aX: TGLint; aY: TGLint); overload;
    procedure uniform3i(aLocation: IJSWebGLUniformLocation; aX: TGLint; aY: TGLint; aZ: TGLint); overload;
    procedure uniform4i(aLocation: IJSWebGLUniformLocation; aX: TGLint; aY: TGLint; aZ: TGLint; aW: TGLint); overload;
    procedure useProgram(aProgram_: IJSWebGLProgram); overload;
    procedure validateProgram(aProgram_: IJSWebGLProgram); overload;
    procedure vertexAttrib1f(aIndx: TGLuint; aX: TGLfloat); overload;
    procedure vertexAttrib1fv(aIndx: TGLuint; aValues: IJSFloat32Array); overload;
    procedure vertexAttrib1fv(aIndx: TGLuint; const aValues: TGLfloatDynArray); overload;
    procedure vertexAttrib2f(aIndx: TGLuint; aX: TGLfloat; aY: TGLfloat); overload;
    procedure vertexAttrib2fv(aIndx: TGLuint; aValues: IJSFloat32Array); overload;
    procedure vertexAttrib2fv(aIndx: TGLuint; const aValues: TGLfloatDynArray); overload;
    procedure vertexAttrib3f(aIndx: TGLuint; aX: TGLfloat; aY: TGLfloat; aZ: TGLfloat); overload;
    procedure vertexAttrib3fv(aIndx: TGLuint; aValues: IJSFloat32Array); overload;
    procedure vertexAttrib3fv(aIndx: TGLuint; const aValues: TGLfloatDynArray); overload;
    procedure vertexAttrib4f(aIndx: TGLuint; aX: TGLfloat; aY: TGLfloat; aZ: TGLfloat; aW: TGLfloat); overload;
    procedure vertexAttrib4fv(aIndx: TGLuint; aValues: IJSFloat32Array); overload;
    procedure vertexAttrib4fv(aIndx: TGLuint; const aValues: TGLfloatDynArray); overload;
    procedure vertexAttribPointer(aIndx: TGLuint; aSize: TGLint; aType_: TGLenum; aNormalized: TGLboolean; aStride: TGLsizei; aOffset: TGLintptr); overload;
    procedure viewport(aX: TGLint; aY: TGLint; aWidth: TGLsizei; aHeight: TGLsizei); overload;
    function makeXRCompatible: IJSPromise; overload; // Promise<undefined>
    procedure bufferData(aTarget: TGLenum; aSize: TGLsizeiptr; aUsage: TGLenum); overload;
    procedure bufferData(aTarget: TGLenum; aSrcData: IJSArrayBuffer; aUsage: TGLenum); overload;
    procedure bufferData(aTarget: TGLenum; aSrcData: IJSArrayBufferView; aUsage: TGLenum); overload;
    procedure bufferSubData(aTarget: TGLenum; aOffset: TGLintptr; aSrcData: IJSArrayBuffer); overload;
    procedure bufferSubData(aTarget: TGLenum; aOffset: TGLintptr; aSrcData: IJSArrayBufferView); overload;
    procedure bufferData(aTarget: TGLenum; aSrcData: IJSArrayBufferView; aUsage: TGLenum; aSrcOffset: TGLuint; aLength_: TGLuint); overload;
    procedure bufferData(aTarget: TGLenum; aSrcData: IJSArrayBufferView; aUsage: TGLenum; aSrcOffset: TGLuint); overload;
    procedure bufferSubData(aTarget: TGLenum; aDstByteOffset: TGLintptr; aSrcData: IJSArrayBufferView; aSrcOffset: TGLuint; aLength_: TGLuint); overload;
    procedure bufferSubData(aTarget: TGLenum; aDstByteOffset: TGLintptr; aSrcData: IJSArrayBufferView; aSrcOffset: TGLuint); overload;
    procedure copyBufferSubData(aReadTarget: TGLenum; aWriteTarget: TGLenum; aReadOffset: TGLintptr; aWriteOffset: TGLintptr; aSize: TGLsizeiptr); overload;
    procedure getBufferSubData(aTarget: TGLenum; aSrcByteOffset: TGLintptr; aDstData: IJSArrayBufferView; aDstOffset: TGLuint; aLength_: TGLuint); overload;
    procedure getBufferSubData(aTarget: TGLenum; aSrcByteOffset: TGLintptr; aDstData: IJSArrayBufferView); overload;
    procedure getBufferSubData(aTarget: TGLenum; aSrcByteOffset: TGLintptr; aDstData: IJSArrayBufferView; aDstOffset: TGLuint); overload;
    procedure blitFramebuffer(aSrcX0: TGLint; aSrcY0: TGLint; aSrcX1: TGLint; aSrcY1: TGLint; aDstX0: TGLint; aDstY0: TGLint; aDstX1: TGLint; aDstY1: TGLint; aMask: TGLbitfield; aFilter: TGLenum); overload;
    procedure framebufferTextureLayer(aTarget: TGLenum; aAttachment: TGLenum; aTexture: IJSWebGLTexture; aLevel: TGLint; aLayer: TGLint); overload;
    procedure invalidateFramebuffer(aTarget: TGLenum; const aAttachments: TGLenumDynArray); overload;
    procedure invalidateSubFramebuffer(aTarget: TGLenum; const aAttachments: TGLenumDynArray; aX: TGLint; aY: TGLint; aWidth: TGLsizei; aHeight: TGLsizei); overload;
    procedure readBuffer(aSrc: TGLenum); overload;
    function getInternalformatParameter(aTarget: TGLenum; aInternalformat: TGLenum; aPname: TGLenum): Variant; overload;
    procedure renderbufferStorageMultisample(aTarget: TGLenum; aSamples: TGLsizei; aInternalformat: TGLenum; aWidth: TGLsizei; aHeight: TGLsizei); overload;
    procedure texStorage2D(aTarget: TGLenum; aLevels: TGLsizei; aInternalformat: TGLenum; aWidth: TGLsizei; aHeight: TGLsizei); overload;
    procedure texStorage3D(aTarget: TGLenum; aLevels: TGLsizei; aInternalformat: TGLenum; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei); overload;
    procedure texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aPixels: IJSArrayBufferView); overload;
    procedure texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSHTMLCanvasElement); overload;
    procedure texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSHTMLImageElement); overload;
    procedure texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSHTMLVideoElement); overload;
    procedure texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSImageBitmap); overload;
    procedure texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSImageData); overload;
    procedure texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSOffscreenCanvas); overload;
    procedure texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSVideoFrame); overload;
    procedure texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aPixels: IJSArrayBufferView); overload;
    procedure texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSHTMLCanvasElement); overload;
    procedure texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSHTMLImageElement); overload;
    procedure texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSHTMLVideoElement); overload;
    procedure texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSImageBitmap); overload;
    procedure texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSImageData); overload;
    procedure texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSOffscreenCanvas); overload;
    procedure texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSVideoFrame); overload;
    procedure texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aPboOffset: TGLintptr); overload;
    procedure texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSHTMLCanvasElement); overload;
    procedure texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSHTMLImageElement); overload;
    procedure texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSHTMLVideoElement); overload;
    procedure texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSImageBitmap); overload;
    procedure texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSImageData); overload;
    procedure texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSOffscreenCanvas); overload;
    procedure texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSVideoFrame); overload;
    procedure texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aSrcData: IJSArrayBufferView; aSrcOffset: TGLuint); overload;
    procedure texImage3D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aPboOffset: TGLintptr); overload;
    procedure texImage3D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSHTMLCanvasElement); overload;
    procedure texImage3D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSHTMLImageElement); overload;
    procedure texImage3D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSHTMLVideoElement); overload;
    procedure texImage3D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSImageBitmap); overload;
    procedure texImage3D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSImageData); overload;
    procedure texImage3D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSOffscreenCanvas); overload;
    procedure texImage3D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSVideoFrame); overload;
    procedure texImage3D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aSrcData: IJSArrayBufferView); overload;
    procedure texImage3D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aSrcData: IJSArrayBufferView; aSrcOffset: TGLuint); overload;
    procedure texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aPboOffset: TGLintptr); overload;
    procedure texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aSource: IJSHTMLCanvasElement); overload;
    procedure texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aSource: IJSHTMLImageElement); overload;
    procedure texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aSource: IJSHTMLVideoElement); overload;
    procedure texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aSource: IJSImageBitmap); overload;
    procedure texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aSource: IJSImageData); overload;
    procedure texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aSource: IJSOffscreenCanvas); overload;
    procedure texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aSource: IJSVideoFrame); overload;
    procedure texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aSrcData: IJSArrayBufferView; aSrcOffset: TGLuint); overload;
    procedure texSubImage3D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aZoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aPboOffset: TGLintptr); overload;
    procedure texSubImage3D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aZoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aSource: IJSHTMLCanvasElement); overload;
    procedure texSubImage3D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aZoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aSource: IJSHTMLImageElement); overload;
    procedure texSubImage3D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aZoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aSource: IJSHTMLVideoElement); overload;
    procedure texSubImage3D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aZoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aSource: IJSImageBitmap); overload;
    procedure texSubImage3D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aZoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aSource: IJSImageData); overload;
    procedure texSubImage3D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aZoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aSource: IJSOffscreenCanvas); overload;
    procedure texSubImage3D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aZoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aSource: IJSVideoFrame); overload;
    procedure texSubImage3D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aZoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aSrcData: IJSArrayBufferView; aSrcOffset: TGLuint); overload;
    procedure texSubImage3D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aZoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aSrcData: IJSArrayBufferView); overload;
    procedure copyTexSubImage3D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aZoffset: TGLint; aX: TGLint; aY: TGLint; aWidth: TGLsizei; aHeight: TGLsizei); overload;
    procedure compressedTexImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLenum; aWidth: TGLsizei; aHeight: TGLsizei; aBorder: TGLint; aImageSize: TGLsizei; aOffset: TGLintptr); overload;
    procedure compressedTexImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLenum; aWidth: TGLsizei; aHeight: TGLsizei; aBorder: TGLint; aSrcData: IJSArrayBufferView; aSrcOffset: TGLuint; aSrcLengthOverride: TGLuint); overload;
    procedure compressedTexImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLenum; aWidth: TGLsizei; aHeight: TGLsizei; aBorder: TGLint; aSrcData: IJSArrayBufferView); overload;
    procedure compressedTexImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLenum; aWidth: TGLsizei; aHeight: TGLsizei; aBorder: TGLint; aSrcData: IJSArrayBufferView; aSrcOffset: TGLuint); overload;
    procedure compressedTexImage3D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLenum; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aBorder: TGLint; aImageSize: TGLsizei; aOffset: TGLintptr); overload;
    procedure compressedTexImage3D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLenum; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aBorder: TGLint; aSrcData: IJSArrayBufferView; aSrcOffset: TGLuint; aSrcLengthOverride: TGLuint); overload;
    procedure compressedTexImage3D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLenum; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aBorder: TGLint; aSrcData: IJSArrayBufferView); overload;
    procedure compressedTexImage3D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLenum; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aBorder: TGLint; aSrcData: IJSArrayBufferView; aSrcOffset: TGLuint); overload;
    procedure compressedTexSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aFormat: TGLenum; aImageSize: TGLsizei; aOffset: TGLintptr); overload;
    procedure compressedTexSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aFormat: TGLenum; aSrcData: IJSArrayBufferView; aSrcOffset: TGLuint; aSrcLengthOverride: TGLuint); overload;
    procedure compressedTexSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aFormat: TGLenum; aSrcData: IJSArrayBufferView); overload;
    procedure compressedTexSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aFormat: TGLenum; aSrcData: IJSArrayBufferView; aSrcOffset: TGLuint); overload;
    procedure compressedTexSubImage3D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aZoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aFormat: TGLenum; aImageSize: TGLsizei; aOffset: TGLintptr); overload;
    procedure compressedTexSubImage3D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aZoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aFormat: TGLenum; aSrcData: IJSArrayBufferView; aSrcOffset: TGLuint; aSrcLengthOverride: TGLuint); overload;
    procedure compressedTexSubImage3D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aZoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aFormat: TGLenum; aSrcData: IJSArrayBufferView); overload;
    procedure compressedTexSubImage3D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aZoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aFormat: TGLenum; aSrcData: IJSArrayBufferView; aSrcOffset: TGLuint); overload;
    function getFragDataLocation(aProgram_: IJSWebGLProgram; const aName: UnicodeString): TGLint; overload;
    procedure uniform1ui(aLocation: IJSWebGLUniformLocation; aV0: TGLuint); overload;
    procedure uniform2ui(aLocation: IJSWebGLUniformLocation; aV0: TGLuint; aV1: TGLuint); overload;
    procedure uniform3ui(aLocation: IJSWebGLUniformLocation; aV0: TGLuint; aV1: TGLuint; aV2: TGLuint); overload;
    procedure uniform4ui(aLocation: IJSWebGLUniformLocation; aV0: TGLuint; aV1: TGLuint; aV2: TGLuint; aV3: TGLuint); overload;
    procedure uniform1fv(aLocation: IJSWebGLUniformLocation; aData: IJSFloat32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint); overload;
    procedure uniform1fv(aLocation: IJSWebGLUniformLocation; const aData: TGLfloatDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint); overload;
    procedure uniform1fv(aLocation: IJSWebGLUniformLocation; const aData: TGLfloatDynArray); overload;
    procedure uniform1fv(aLocation: IJSWebGLUniformLocation; aData: IJSFloat32Array); overload;
    procedure uniform1fv(aLocation: IJSWebGLUniformLocation; const aData: TGLfloatDynArray; aSrcOffset: TGLuint); overload;
    procedure uniform1fv(aLocation: IJSWebGLUniformLocation; aData: IJSFloat32Array; aSrcOffset: TGLuint); overload;
    procedure uniform2fv(aLocation: IJSWebGLUniformLocation; aData: IJSFloat32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint); overload;
    procedure uniform2fv(aLocation: IJSWebGLUniformLocation; const aData: TGLfloatDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint); overload;
    procedure uniform2fv(aLocation: IJSWebGLUniformLocation; const aData: TGLfloatDynArray); overload;
    procedure uniform2fv(aLocation: IJSWebGLUniformLocation; aData: IJSFloat32Array); overload;
    procedure uniform2fv(aLocation: IJSWebGLUniformLocation; const aData: TGLfloatDynArray; aSrcOffset: TGLuint); overload;
    procedure uniform2fv(aLocation: IJSWebGLUniformLocation; aData: IJSFloat32Array; aSrcOffset: TGLuint); overload;
    procedure uniform3fv(aLocation: IJSWebGLUniformLocation; aData: IJSFloat32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint); overload;
    procedure uniform3fv(aLocation: IJSWebGLUniformLocation; const aData: TGLfloatDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint); overload;
    procedure uniform3fv(aLocation: IJSWebGLUniformLocation; const aData: TGLfloatDynArray); overload;
    procedure uniform3fv(aLocation: IJSWebGLUniformLocation; aData: IJSFloat32Array); overload;
    procedure uniform3fv(aLocation: IJSWebGLUniformLocation; const aData: TGLfloatDynArray; aSrcOffset: TGLuint); overload;
    procedure uniform3fv(aLocation: IJSWebGLUniformLocation; aData: IJSFloat32Array; aSrcOffset: TGLuint); overload;
    procedure uniform4fv(aLocation: IJSWebGLUniformLocation; aData: IJSFloat32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint); overload;
    procedure uniform4fv(aLocation: IJSWebGLUniformLocation; const aData: TGLfloatDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint); overload;
    procedure uniform4fv(aLocation: IJSWebGLUniformLocation; const aData: TGLfloatDynArray); overload;
    procedure uniform4fv(aLocation: IJSWebGLUniformLocation; aData: IJSFloat32Array); overload;
    procedure uniform4fv(aLocation: IJSWebGLUniformLocation; const aData: TGLfloatDynArray; aSrcOffset: TGLuint); overload;
    procedure uniform4fv(aLocation: IJSWebGLUniformLocation; aData: IJSFloat32Array; aSrcOffset: TGLuint); overload;
    procedure uniform1iv(aLocation: IJSWebGLUniformLocation; aData: IJSInt32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint); overload;
    procedure uniform1iv(aLocation: IJSWebGLUniformLocation; const aData: TGLintDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint); overload;
    procedure uniform1iv(aLocation: IJSWebGLUniformLocation; const aData: TGLintDynArray); overload;
    procedure uniform1iv(aLocation: IJSWebGLUniformLocation; aData: IJSInt32Array); overload;
    procedure uniform1iv(aLocation: IJSWebGLUniformLocation; const aData: TGLintDynArray; aSrcOffset: TGLuint); overload;
    procedure uniform1iv(aLocation: IJSWebGLUniformLocation; aData: IJSInt32Array; aSrcOffset: TGLuint); overload;
    procedure uniform2iv(aLocation: IJSWebGLUniformLocation; aData: IJSInt32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint); overload;
    procedure uniform2iv(aLocation: IJSWebGLUniformLocation; const aData: TGLintDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint); overload;
    procedure uniform2iv(aLocation: IJSWebGLUniformLocation; const aData: TGLintDynArray); overload;
    procedure uniform2iv(aLocation: IJSWebGLUniformLocation; aData: IJSInt32Array); overload;
    procedure uniform2iv(aLocation: IJSWebGLUniformLocation; const aData: TGLintDynArray; aSrcOffset: TGLuint); overload;
    procedure uniform2iv(aLocation: IJSWebGLUniformLocation; aData: IJSInt32Array; aSrcOffset: TGLuint); overload;
    procedure uniform3iv(aLocation: IJSWebGLUniformLocation; aData: IJSInt32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint); overload;
    procedure uniform3iv(aLocation: IJSWebGLUniformLocation; const aData: TGLintDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint); overload;
    procedure uniform3iv(aLocation: IJSWebGLUniformLocation; const aData: TGLintDynArray); overload;
    procedure uniform3iv(aLocation: IJSWebGLUniformLocation; aData: IJSInt32Array); overload;
    procedure uniform3iv(aLocation: IJSWebGLUniformLocation; const aData: TGLintDynArray; aSrcOffset: TGLuint); overload;
    procedure uniform3iv(aLocation: IJSWebGLUniformLocation; aData: IJSInt32Array; aSrcOffset: TGLuint); overload;
    procedure uniform4iv(aLocation: IJSWebGLUniformLocation; aData: IJSInt32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint); overload;
    procedure uniform4iv(aLocation: IJSWebGLUniformLocation; const aData: TGLintDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint); overload;
    procedure uniform4iv(aLocation: IJSWebGLUniformLocation; const aData: TGLintDynArray); overload;
    procedure uniform4iv(aLocation: IJSWebGLUniformLocation; aData: IJSInt32Array); overload;
    procedure uniform4iv(aLocation: IJSWebGLUniformLocation; const aData: TGLintDynArray; aSrcOffset: TGLuint); overload;
    procedure uniform4iv(aLocation: IJSWebGLUniformLocation; aData: IJSInt32Array; aSrcOffset: TGLuint); overload;
    procedure uniform1uiv(aLocation: IJSWebGLUniformLocation; const aData: TGLuintDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint); overload;
    procedure uniform1uiv(aLocation: IJSWebGLUniformLocation; aData: IJSUint32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint); overload;
    procedure uniform1uiv(aLocation: IJSWebGLUniformLocation; aData: IJSUint32Array); overload;
    procedure uniform1uiv(aLocation: IJSWebGLUniformLocation; const aData: TGLuintDynArray); overload;
    procedure uniform1uiv(aLocation: IJSWebGLUniformLocation; aData: IJSUint32Array; aSrcOffset: TGLuint); overload;
    procedure uniform1uiv(aLocation: IJSWebGLUniformLocation; const aData: TGLuintDynArray; aSrcOffset: TGLuint); overload;
    procedure uniform2uiv(aLocation: IJSWebGLUniformLocation; const aData: TGLuintDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint); overload;
    procedure uniform2uiv(aLocation: IJSWebGLUniformLocation; aData: IJSUint32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint); overload;
    procedure uniform2uiv(aLocation: IJSWebGLUniformLocation; aData: IJSUint32Array); overload;
    procedure uniform2uiv(aLocation: IJSWebGLUniformLocation; const aData: TGLuintDynArray); overload;
    procedure uniform2uiv(aLocation: IJSWebGLUniformLocation; aData: IJSUint32Array; aSrcOffset: TGLuint); overload;
    procedure uniform2uiv(aLocation: IJSWebGLUniformLocation; const aData: TGLuintDynArray; aSrcOffset: TGLuint); overload;
    procedure uniform3uiv(aLocation: IJSWebGLUniformLocation; const aData: TGLuintDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint); overload;
    procedure uniform3uiv(aLocation: IJSWebGLUniformLocation; aData: IJSUint32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint); overload;
    procedure uniform3uiv(aLocation: IJSWebGLUniformLocation; aData: IJSUint32Array); overload;
    procedure uniform3uiv(aLocation: IJSWebGLUniformLocation; const aData: TGLuintDynArray); overload;
    procedure uniform3uiv(aLocation: IJSWebGLUniformLocation; aData: IJSUint32Array; aSrcOffset: TGLuint); overload;
    procedure uniform3uiv(aLocation: IJSWebGLUniformLocation; const aData: TGLuintDynArray; aSrcOffset: TGLuint); overload;
    procedure uniform4uiv(aLocation: IJSWebGLUniformLocation; const aData: TGLuintDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint); overload;
    procedure uniform4uiv(aLocation: IJSWebGLUniformLocation; aData: IJSUint32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint); overload;
    procedure uniform4uiv(aLocation: IJSWebGLUniformLocation; aData: IJSUint32Array); overload;
    procedure uniform4uiv(aLocation: IJSWebGLUniformLocation; const aData: TGLuintDynArray); overload;
    procedure uniform4uiv(aLocation: IJSWebGLUniformLocation; aData: IJSUint32Array; aSrcOffset: TGLuint); overload;
    procedure uniform4uiv(aLocation: IJSWebGLUniformLocation; const aData: TGLuintDynArray; aSrcOffset: TGLuint); overload;
    procedure uniformMatrix2fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint); overload;
    procedure uniformMatrix2fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint); overload;
    procedure uniformMatrix2fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray); overload;
    procedure uniformMatrix2fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array); overload;
    procedure uniformMatrix2fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray; aSrcOffset: TGLuint); overload;
    procedure uniformMatrix2fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array; aSrcOffset: TGLuint); overload;
    procedure uniformMatrix3x2fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint); overload;
    procedure uniformMatrix3x2fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint); overload;
    procedure uniformMatrix3x2fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray); overload;
    procedure uniformMatrix3x2fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array); overload;
    procedure uniformMatrix3x2fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray; aSrcOffset: TGLuint); overload;
    procedure uniformMatrix3x2fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array; aSrcOffset: TGLuint); overload;
    procedure uniformMatrix4x2fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint); overload;
    procedure uniformMatrix4x2fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint); overload;
    procedure uniformMatrix4x2fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray); overload;
    procedure uniformMatrix4x2fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array); overload;
    procedure uniformMatrix4x2fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray; aSrcOffset: TGLuint); overload;
    procedure uniformMatrix4x2fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array; aSrcOffset: TGLuint); overload;
    procedure uniformMatrix2x3fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint); overload;
    procedure uniformMatrix2x3fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint); overload;
    procedure uniformMatrix2x3fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray); overload;
    procedure uniformMatrix2x3fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array); overload;
    procedure uniformMatrix2x3fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray; aSrcOffset: TGLuint); overload;
    procedure uniformMatrix2x3fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array; aSrcOffset: TGLuint); overload;
    procedure uniformMatrix3fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint); overload;
    procedure uniformMatrix3fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint); overload;
    procedure uniformMatrix3fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray); overload;
    procedure uniformMatrix3fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array); overload;
    procedure uniformMatrix3fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray; aSrcOffset: TGLuint); overload;
    procedure uniformMatrix3fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array; aSrcOffset: TGLuint); overload;
    procedure uniformMatrix4x3fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint); overload;
    procedure uniformMatrix4x3fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint); overload;
    procedure uniformMatrix4x3fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray); overload;
    procedure uniformMatrix4x3fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array); overload;
    procedure uniformMatrix4x3fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray; aSrcOffset: TGLuint); overload;
    procedure uniformMatrix4x3fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array; aSrcOffset: TGLuint); overload;
    procedure uniformMatrix2x4fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint); overload;
    procedure uniformMatrix2x4fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint); overload;
    procedure uniformMatrix2x4fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray); overload;
    procedure uniformMatrix2x4fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array); overload;
    procedure uniformMatrix2x4fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray; aSrcOffset: TGLuint); overload;
    procedure uniformMatrix2x4fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array; aSrcOffset: TGLuint); overload;
    procedure uniformMatrix3x4fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint); overload;
    procedure uniformMatrix3x4fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint); overload;
    procedure uniformMatrix3x4fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray); overload;
    procedure uniformMatrix3x4fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array); overload;
    procedure uniformMatrix3x4fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray; aSrcOffset: TGLuint); overload;
    procedure uniformMatrix3x4fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array; aSrcOffset: TGLuint); overload;
    procedure uniformMatrix4fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint); overload;
    procedure uniformMatrix4fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint); overload;
    procedure uniformMatrix4fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray); overload;
    procedure uniformMatrix4fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array); overload;
    procedure uniformMatrix4fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray; aSrcOffset: TGLuint); overload;
    procedure uniformMatrix4fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array; aSrcOffset: TGLuint); overload;
    procedure vertexAttribI4i(aIndex: TGLuint; aX: TGLint; aY: TGLint; aZ: TGLint; aW: TGLint); overload;
    procedure vertexAttribI4iv(aIndex: TGLuint; aValues: IJSInt32Array); overload;
    procedure vertexAttribI4iv(aIndex: TGLuint; const aValues: TGLintDynArray); overload;
    procedure vertexAttribI4ui(aIndex: TGLuint; aX: TGLuint; aY: TGLuint; aZ: TGLuint; aW: TGLuint); overload;
    procedure vertexAttribI4uiv(aIndex: TGLuint; const aValues: TGLuintDynArray); overload;
    procedure vertexAttribI4uiv(aIndex: TGLuint; aValues: IJSUint32Array); overload;
    procedure vertexAttribIPointer(aIndex: TGLuint; aSize: TGLint; aType_: TGLenum; aStride: TGLsizei; aOffset: TGLintptr); overload;
    procedure vertexAttribDivisor(aIndex: TGLuint; aDivisor: TGLuint); overload;
    procedure drawArraysInstanced(aMode: TGLenum; aFirst: TGLint; aCount: TGLsizei; aInstanceCount: TGLsizei); overload;
    procedure drawElementsInstanced(aMode: TGLenum; aCount: TGLsizei; aType_: TGLenum; aOffset: TGLintptr; aInstanceCount: TGLsizei); overload;
    procedure drawRangeElements(aMode: TGLenum; aStart: TGLuint; aEnd_: TGLuint; aCount: TGLsizei; aType_: TGLenum; aOffset: TGLintptr); overload;
    procedure readPixels(aX: TGLint; aY: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aDstData: IJSArrayBufferView); overload;
    procedure readPixels(aX: TGLint; aY: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aOffset: TGLintptr); overload;
    procedure readPixels(aX: TGLint; aY: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aDstData: IJSArrayBufferView; aDstOffset: TGLuint); overload;
    procedure drawBuffers(const aBuffers: TGLenumDynArray); overload;
    procedure clearBufferfv(aBuffer: TGLenum; aDrawbuffer: TGLint; aValues: IJSFloat32Array; aSrcOffset: TGLuint); overload;
    procedure clearBufferfv(aBuffer: TGLenum; aDrawbuffer: TGLint; const aValues: TGLfloatDynArray; aSrcOffset: TGLuint); overload;
    procedure clearBufferfv(aBuffer: TGLenum; aDrawbuffer: TGLint; const aValues: TGLfloatDynArray); overload;
    procedure clearBufferfv(aBuffer: TGLenum; aDrawbuffer: TGLint; aValues: IJSFloat32Array); overload;
    procedure clearBufferiv(aBuffer: TGLenum; aDrawbuffer: TGLint; aValues: IJSInt32Array; aSrcOffset: TGLuint); overload;
    procedure clearBufferiv(aBuffer: TGLenum; aDrawbuffer: TGLint; const aValues: TGLintDynArray; aSrcOffset: TGLuint); overload;
    procedure clearBufferiv(aBuffer: TGLenum; aDrawbuffer: TGLint; const aValues: TGLintDynArray); overload;
    procedure clearBufferiv(aBuffer: TGLenum; aDrawbuffer: TGLint; aValues: IJSInt32Array); overload;
    procedure clearBufferuiv(aBuffer: TGLenum; aDrawbuffer: TGLint; const aValues: TGLuintDynArray; aSrcOffset: TGLuint); overload;
    procedure clearBufferuiv(aBuffer: TGLenum; aDrawbuffer: TGLint; aValues: IJSUint32Array; aSrcOffset: TGLuint); overload;
    procedure clearBufferuiv(aBuffer: TGLenum; aDrawbuffer: TGLint; aValues: IJSUint32Array); overload;
    procedure clearBufferuiv(aBuffer: TGLenum; aDrawbuffer: TGLint; const aValues: TGLuintDynArray); overload;
    procedure clearBufferfi(aBuffer: TGLenum; aDrawbuffer: TGLint; aDepth: TGLfloat; aStencil: TGLint); overload;
    function createQuery: IJSWebGLQuery; overload;
    procedure deleteQuery(aQuery: IJSWebGLQuery); overload;
    function isQuery(aQuery: IJSWebGLQuery): TGLboolean; overload;
    procedure beginQuery(aTarget: TGLenum; aQuery: IJSWebGLQuery); overload;
    procedure endQuery(aTarget: TGLenum); overload;
    function getQuery(aTarget: TGLenum; aPname: TGLenum): Variant; overload;
    function getQueryParameter(aQuery: IJSWebGLQuery; aPname: TGLenum): Variant; overload;
    function createSampler: IJSWebGLSampler; overload;
    procedure deleteSampler(aSampler: IJSWebGLSampler); overload;
    function isSampler(aSampler: IJSWebGLSampler): TGLboolean; overload;
    procedure bindSampler(aUnit_: TGLuint; aSampler: IJSWebGLSampler); overload;
    procedure samplerParameteri(aSampler: IJSWebGLSampler; aPname: TGLenum; aParam: TGLint); overload;
    procedure samplerParameterf(aSampler: IJSWebGLSampler; aPname: TGLenum; aParam: TGLfloat); overload;
    function getSamplerParameter(aSampler: IJSWebGLSampler; aPname: TGLenum): Variant; overload;
    function fenceSync(aCondition: TGLenum; aFlags: TGLbitfield): IJSWebGLSync; overload;
    function isSync(aSync: IJSWebGLSync): TGLboolean; overload;
    procedure deleteSync(aSync: IJSWebGLSync); overload;
    function clientWaitSync(aSync: IJSWebGLSync; aFlags: TGLbitfield; aTimeout: TGLuint64): TGLenum; overload;
    procedure waitSync(aSync: IJSWebGLSync; aFlags: TGLbitfield; aTimeout: TGLint64); overload;
    function getSyncParameter(aSync: IJSWebGLSync; aPname: TGLenum): Variant; overload;
    function createTransformFeedback: IJSWebGLTransformFeedback; overload;
    procedure deleteTransformFeedback(aTf: IJSWebGLTransformFeedback); overload;
    function isTransformFeedback(aTf: IJSWebGLTransformFeedback): TGLboolean; overload;
    procedure bindTransformFeedback(aTarget: TGLenum; aTf: IJSWebGLTransformFeedback); overload;
    procedure beginTransformFeedback(aPrimitiveMode: TGLenum); overload;
    procedure endTransformFeedback; overload;
    procedure transformFeedbackVaryings(aProgram_: IJSWebGLProgram; const aVaryings: TUnicodeStringDynArray; aBufferMode: TGLenum); overload;
    function getTransformFeedbackVarying(aProgram_: IJSWebGLProgram; aIndex: TGLuint): IJSWebGLActiveInfo; overload;
    procedure pauseTransformFeedback; overload;
    procedure resumeTransformFeedback; overload;
    procedure bindBufferBase(aTarget: TGLenum; aIndex: TGLuint; aBuffer: IJSWebGLBuffer); overload;
    procedure bindBufferRange(aTarget: TGLenum; aIndex: TGLuint; aBuffer: IJSWebGLBuffer; aOffset: TGLintptr; aSize: TGLsizeiptr); overload;
    function getIndexedParameter(aTarget: TGLenum; aIndex: TGLuint): Variant; overload;
    function getUniformIndices(aProgram_: IJSWebGLProgram; const aUniformNames: TUnicodeStringDynArray): TGLuintDynArray; overload;
    function getActiveUniforms(aProgram_: IJSWebGLProgram; const aUniformIndices: TGLuintDynArray; aPname: TGLenum): Variant; overload;
    function getUniformBlockIndex(aProgram_: IJSWebGLProgram; const aUniformBlockName: UnicodeString): TGLuint; overload;
    function getActiveUniformBlockParameter(aProgram_: IJSWebGLProgram; aUniformBlockIndex: TGLuint; aPname: TGLenum): Variant; overload;
    function getActiveUniformBlockName(aProgram_: IJSWebGLProgram; aUniformBlockIndex: TGLuint): UnicodeString; overload;
    procedure uniformBlockBinding(aProgram_: IJSWebGLProgram; aUniformBlockIndex: TGLuint; aUniformBlockBinding: TGLuint); overload;
    function createVertexArray: IJSWebGLVertexArrayObject; overload;
    procedure deleteVertexArray(aVertexArray: IJSWebGLVertexArrayObject); overload;
    function isVertexArray(aVertexArray: IJSWebGLVertexArrayObject): TGLboolean; overload;
    procedure bindVertexArray(aArray_: IJSWebGLVertexArrayObject); overload;
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSWebGL2RenderingContext;
    property canvas: TCanvasSource read _Getcanvas;
    property drawingBufferWidth: TGLsizei read _GetdrawingBufferWidth;
    property drawingBufferHeight: TGLsizei read _GetdrawingBufferHeight;
    property drawingBufferColorSpace: TPredefinedColorSpace read _GetdrawingBufferColorSpace write _SetdrawingBufferColorSpace;
    property unpackColorSpace: TPredefinedColorSpace read _GetunpackColorSpace write _SetunpackColorSpace;
  end;
  
  { --------------------------------------------------------------------
    TJSWebGL2RenderingContextBase
    --------------------------------------------------------------------}
  
  IJSWebGL2RenderingContextBase = interface(IJSObject)
    ['{F6E20F6C-0549-3256-854A-F1323ACC369C}']
    procedure bufferData(aTarget: TGLenum; aSize: TGLsizeiptr; aUsage: TGLenum);
    procedure bufferData(aTarget: TGLenum; aSrcData: IJSArrayBuffer; aUsage: TGLenum);
    procedure bufferData(aTarget: TGLenum; aSrcData: IJSArrayBufferView; aUsage: TGLenum);
    procedure bufferSubData(aTarget: TGLenum; aOffset: TGLintptr; aSrcData: IJSArrayBuffer);
    procedure bufferSubData(aTarget: TGLenum; aOffset: TGLintptr; aSrcData: IJSArrayBufferView);
    procedure bufferData(aTarget: TGLenum; aSrcData: IJSArrayBufferView; aUsage: TGLenum; aSrcOffset: TGLuint; aLength_: TGLuint);
    procedure bufferData(aTarget: TGLenum; aSrcData: IJSArrayBufferView; aUsage: TGLenum; aSrcOffset: TGLuint);
    procedure bufferSubData(aTarget: TGLenum; aDstByteOffset: TGLintptr; aSrcData: IJSArrayBufferView; aSrcOffset: TGLuint; aLength_: TGLuint);
    procedure bufferSubData(aTarget: TGLenum; aDstByteOffset: TGLintptr; aSrcData: IJSArrayBufferView; aSrcOffset: TGLuint);
    procedure copyBufferSubData(aReadTarget: TGLenum; aWriteTarget: TGLenum; aReadOffset: TGLintptr; aWriteOffset: TGLintptr; aSize: TGLsizeiptr);
    procedure getBufferSubData(aTarget: TGLenum; aSrcByteOffset: TGLintptr; aDstData: IJSArrayBufferView; aDstOffset: TGLuint; aLength_: TGLuint);
    procedure getBufferSubData(aTarget: TGLenum; aSrcByteOffset: TGLintptr; aDstData: IJSArrayBufferView);
    procedure getBufferSubData(aTarget: TGLenum; aSrcByteOffset: TGLintptr; aDstData: IJSArrayBufferView; aDstOffset: TGLuint);
    procedure blitFramebuffer(aSrcX0: TGLint; aSrcY0: TGLint; aSrcX1: TGLint; aSrcY1: TGLint; aDstX0: TGLint; aDstY0: TGLint; aDstX1: TGLint; aDstY1: TGLint; aMask: TGLbitfield; aFilter: TGLenum);
    procedure framebufferTextureLayer(aTarget: TGLenum; aAttachment: TGLenum; aTexture: IJSWebGLTexture; aLevel: TGLint; aLayer: TGLint);
    procedure invalidateFramebuffer(aTarget: TGLenum; const aAttachments: TGLenumDynArray);
    procedure invalidateSubFramebuffer(aTarget: TGLenum; const aAttachments: TGLenumDynArray; aX: TGLint; aY: TGLint; aWidth: TGLsizei; aHeight: TGLsizei);
    procedure readBuffer(aSrc: TGLenum);
    function getInternalformatParameter(aTarget: TGLenum; aInternalformat: TGLenum; aPname: TGLenum): Variant;
    procedure renderbufferStorageMultisample(aTarget: TGLenum; aSamples: TGLsizei; aInternalformat: TGLenum; aWidth: TGLsizei; aHeight: TGLsizei);
    procedure texStorage2D(aTarget: TGLenum; aLevels: TGLsizei; aInternalformat: TGLenum; aWidth: TGLsizei; aHeight: TGLsizei);
    procedure texStorage3D(aTarget: TGLenum; aLevels: TGLsizei; aInternalformat: TGLenum; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei);
    procedure texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aPixels: IJSArrayBufferView);
    procedure texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSHTMLCanvasElement);
    procedure texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSHTMLImageElement);
    procedure texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSHTMLVideoElement);
    procedure texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSImageBitmap);
    procedure texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSImageData);
    procedure texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSOffscreenCanvas);
    procedure texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSVideoFrame);
    procedure texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aPixels: IJSArrayBufferView);
    procedure texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSHTMLCanvasElement);
    procedure texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSHTMLImageElement);
    procedure texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSHTMLVideoElement);
    procedure texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSImageBitmap);
    procedure texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSImageData);
    procedure texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSOffscreenCanvas);
    procedure texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSVideoFrame);
    procedure texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aPboOffset: TGLintptr);
    procedure texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSHTMLCanvasElement);
    procedure texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSHTMLImageElement);
    procedure texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSHTMLVideoElement);
    procedure texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSImageBitmap);
    procedure texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSImageData);
    procedure texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSOffscreenCanvas);
    procedure texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSVideoFrame);
    procedure texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aSrcData: IJSArrayBufferView; aSrcOffset: TGLuint);
    procedure texImage3D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aPboOffset: TGLintptr);
    procedure texImage3D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSHTMLCanvasElement);
    procedure texImage3D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSHTMLImageElement);
    procedure texImage3D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSHTMLVideoElement);
    procedure texImage3D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSImageBitmap);
    procedure texImage3D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSImageData);
    procedure texImage3D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSOffscreenCanvas);
    procedure texImage3D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSVideoFrame);
    procedure texImage3D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aSrcData: IJSArrayBufferView);
    procedure texImage3D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aSrcData: IJSArrayBufferView; aSrcOffset: TGLuint);
    procedure texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aPboOffset: TGLintptr);
    procedure texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aSource: IJSHTMLCanvasElement);
    procedure texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aSource: IJSHTMLImageElement);
    procedure texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aSource: IJSHTMLVideoElement);
    procedure texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aSource: IJSImageBitmap);
    procedure texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aSource: IJSImageData);
    procedure texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aSource: IJSOffscreenCanvas);
    procedure texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aSource: IJSVideoFrame);
    procedure texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aSrcData: IJSArrayBufferView; aSrcOffset: TGLuint);
    procedure texSubImage3D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aZoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aPboOffset: TGLintptr);
    procedure texSubImage3D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aZoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aSource: IJSHTMLCanvasElement);
    procedure texSubImage3D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aZoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aSource: IJSHTMLImageElement);
    procedure texSubImage3D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aZoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aSource: IJSHTMLVideoElement);
    procedure texSubImage3D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aZoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aSource: IJSImageBitmap);
    procedure texSubImage3D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aZoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aSource: IJSImageData);
    procedure texSubImage3D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aZoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aSource: IJSOffscreenCanvas);
    procedure texSubImage3D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aZoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aSource: IJSVideoFrame);
    procedure texSubImage3D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aZoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aSrcData: IJSArrayBufferView; aSrcOffset: TGLuint);
    procedure texSubImage3D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aZoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aSrcData: IJSArrayBufferView);
    procedure copyTexSubImage3D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aZoffset: TGLint; aX: TGLint; aY: TGLint; aWidth: TGLsizei; aHeight: TGLsizei);
    procedure compressedTexImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLenum; aWidth: TGLsizei; aHeight: TGLsizei; aBorder: TGLint; aImageSize: TGLsizei; aOffset: TGLintptr);
    procedure compressedTexImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLenum; aWidth: TGLsizei; aHeight: TGLsizei; aBorder: TGLint; aSrcData: IJSArrayBufferView; aSrcOffset: TGLuint; aSrcLengthOverride: TGLuint);
    procedure compressedTexImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLenum; aWidth: TGLsizei; aHeight: TGLsizei; aBorder: TGLint; aSrcData: IJSArrayBufferView);
    procedure compressedTexImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLenum; aWidth: TGLsizei; aHeight: TGLsizei; aBorder: TGLint; aSrcData: IJSArrayBufferView; aSrcOffset: TGLuint);
    procedure compressedTexImage3D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLenum; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aBorder: TGLint; aImageSize: TGLsizei; aOffset: TGLintptr);
    procedure compressedTexImage3D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLenum; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aBorder: TGLint; aSrcData: IJSArrayBufferView; aSrcOffset: TGLuint; aSrcLengthOverride: TGLuint);
    procedure compressedTexImage3D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLenum; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aBorder: TGLint; aSrcData: IJSArrayBufferView);
    procedure compressedTexImage3D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLenum; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aBorder: TGLint; aSrcData: IJSArrayBufferView; aSrcOffset: TGLuint);
    procedure compressedTexSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aFormat: TGLenum; aImageSize: TGLsizei; aOffset: TGLintptr);
    procedure compressedTexSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aFormat: TGLenum; aSrcData: IJSArrayBufferView; aSrcOffset: TGLuint; aSrcLengthOverride: TGLuint);
    procedure compressedTexSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aFormat: TGLenum; aSrcData: IJSArrayBufferView);
    procedure compressedTexSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aFormat: TGLenum; aSrcData: IJSArrayBufferView; aSrcOffset: TGLuint);
    procedure compressedTexSubImage3D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aZoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aFormat: TGLenum; aImageSize: TGLsizei; aOffset: TGLintptr);
    procedure compressedTexSubImage3D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aZoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aFormat: TGLenum; aSrcData: IJSArrayBufferView; aSrcOffset: TGLuint; aSrcLengthOverride: TGLuint);
    procedure compressedTexSubImage3D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aZoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aFormat: TGLenum; aSrcData: IJSArrayBufferView);
    procedure compressedTexSubImage3D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aZoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aFormat: TGLenum; aSrcData: IJSArrayBufferView; aSrcOffset: TGLuint);
    function getFragDataLocation(aProgram_: IJSWebGLProgram; const aName: UnicodeString): TGLint;
    procedure uniform1ui(aLocation: IJSWebGLUniformLocation; aV0: TGLuint);
    procedure uniform2ui(aLocation: IJSWebGLUniformLocation; aV0: TGLuint; aV1: TGLuint);
    procedure uniform3ui(aLocation: IJSWebGLUniformLocation; aV0: TGLuint; aV1: TGLuint; aV2: TGLuint);
    procedure uniform4ui(aLocation: IJSWebGLUniformLocation; aV0: TGLuint; aV1: TGLuint; aV2: TGLuint; aV3: TGLuint);
    procedure uniform1fv(aLocation: IJSWebGLUniformLocation; aData: IJSFloat32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint);
    procedure uniform1fv(aLocation: IJSWebGLUniformLocation; const aData: TGLfloatDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint);
    procedure uniform1fv(aLocation: IJSWebGLUniformLocation; const aData: TGLfloatDynArray);
    procedure uniform1fv(aLocation: IJSWebGLUniformLocation; aData: IJSFloat32Array);
    procedure uniform1fv(aLocation: IJSWebGLUniformLocation; const aData: TGLfloatDynArray; aSrcOffset: TGLuint);
    procedure uniform1fv(aLocation: IJSWebGLUniformLocation; aData: IJSFloat32Array; aSrcOffset: TGLuint);
    procedure uniform2fv(aLocation: IJSWebGLUniformLocation; aData: IJSFloat32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint);
    procedure uniform2fv(aLocation: IJSWebGLUniformLocation; const aData: TGLfloatDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint);
    procedure uniform2fv(aLocation: IJSWebGLUniformLocation; const aData: TGLfloatDynArray);
    procedure uniform2fv(aLocation: IJSWebGLUniformLocation; aData: IJSFloat32Array);
    procedure uniform2fv(aLocation: IJSWebGLUniformLocation; const aData: TGLfloatDynArray; aSrcOffset: TGLuint);
    procedure uniform2fv(aLocation: IJSWebGLUniformLocation; aData: IJSFloat32Array; aSrcOffset: TGLuint);
    procedure uniform3fv(aLocation: IJSWebGLUniformLocation; aData: IJSFloat32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint);
    procedure uniform3fv(aLocation: IJSWebGLUniformLocation; const aData: TGLfloatDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint);
    procedure uniform3fv(aLocation: IJSWebGLUniformLocation; const aData: TGLfloatDynArray);
    procedure uniform3fv(aLocation: IJSWebGLUniformLocation; aData: IJSFloat32Array);
    procedure uniform3fv(aLocation: IJSWebGLUniformLocation; const aData: TGLfloatDynArray; aSrcOffset: TGLuint);
    procedure uniform3fv(aLocation: IJSWebGLUniformLocation; aData: IJSFloat32Array; aSrcOffset: TGLuint);
    procedure uniform4fv(aLocation: IJSWebGLUniformLocation; aData: IJSFloat32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint);
    procedure uniform4fv(aLocation: IJSWebGLUniformLocation; const aData: TGLfloatDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint);
    procedure uniform4fv(aLocation: IJSWebGLUniformLocation; const aData: TGLfloatDynArray);
    procedure uniform4fv(aLocation: IJSWebGLUniformLocation; aData: IJSFloat32Array);
    procedure uniform4fv(aLocation: IJSWebGLUniformLocation; const aData: TGLfloatDynArray; aSrcOffset: TGLuint);
    procedure uniform4fv(aLocation: IJSWebGLUniformLocation; aData: IJSFloat32Array; aSrcOffset: TGLuint);
    procedure uniform1iv(aLocation: IJSWebGLUniformLocation; aData: IJSInt32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint);
    procedure uniform1iv(aLocation: IJSWebGLUniformLocation; const aData: TGLintDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint);
    procedure uniform1iv(aLocation: IJSWebGLUniformLocation; const aData: TGLintDynArray);
    procedure uniform1iv(aLocation: IJSWebGLUniformLocation; aData: IJSInt32Array);
    procedure uniform1iv(aLocation: IJSWebGLUniformLocation; const aData: TGLintDynArray; aSrcOffset: TGLuint);
    procedure uniform1iv(aLocation: IJSWebGLUniformLocation; aData: IJSInt32Array; aSrcOffset: TGLuint);
    procedure uniform2iv(aLocation: IJSWebGLUniformLocation; aData: IJSInt32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint);
    procedure uniform2iv(aLocation: IJSWebGLUniformLocation; const aData: TGLintDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint);
    procedure uniform2iv(aLocation: IJSWebGLUniformLocation; const aData: TGLintDynArray);
    procedure uniform2iv(aLocation: IJSWebGLUniformLocation; aData: IJSInt32Array);
    procedure uniform2iv(aLocation: IJSWebGLUniformLocation; const aData: TGLintDynArray; aSrcOffset: TGLuint);
    procedure uniform2iv(aLocation: IJSWebGLUniformLocation; aData: IJSInt32Array; aSrcOffset: TGLuint);
    procedure uniform3iv(aLocation: IJSWebGLUniformLocation; aData: IJSInt32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint);
    procedure uniform3iv(aLocation: IJSWebGLUniformLocation; const aData: TGLintDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint);
    procedure uniform3iv(aLocation: IJSWebGLUniformLocation; const aData: TGLintDynArray);
    procedure uniform3iv(aLocation: IJSWebGLUniformLocation; aData: IJSInt32Array);
    procedure uniform3iv(aLocation: IJSWebGLUniformLocation; const aData: TGLintDynArray; aSrcOffset: TGLuint);
    procedure uniform3iv(aLocation: IJSWebGLUniformLocation; aData: IJSInt32Array; aSrcOffset: TGLuint);
    procedure uniform4iv(aLocation: IJSWebGLUniformLocation; aData: IJSInt32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint);
    procedure uniform4iv(aLocation: IJSWebGLUniformLocation; const aData: TGLintDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint);
    procedure uniform4iv(aLocation: IJSWebGLUniformLocation; const aData: TGLintDynArray);
    procedure uniform4iv(aLocation: IJSWebGLUniformLocation; aData: IJSInt32Array);
    procedure uniform4iv(aLocation: IJSWebGLUniformLocation; const aData: TGLintDynArray; aSrcOffset: TGLuint);
    procedure uniform4iv(aLocation: IJSWebGLUniformLocation; aData: IJSInt32Array; aSrcOffset: TGLuint);
    procedure uniform1uiv(aLocation: IJSWebGLUniformLocation; const aData: TGLuintDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint);
    procedure uniform1uiv(aLocation: IJSWebGLUniformLocation; aData: IJSUint32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint);
    procedure uniform1uiv(aLocation: IJSWebGLUniformLocation; aData: IJSUint32Array);
    procedure uniform1uiv(aLocation: IJSWebGLUniformLocation; const aData: TGLuintDynArray);
    procedure uniform1uiv(aLocation: IJSWebGLUniformLocation; aData: IJSUint32Array; aSrcOffset: TGLuint);
    procedure uniform1uiv(aLocation: IJSWebGLUniformLocation; const aData: TGLuintDynArray; aSrcOffset: TGLuint);
    procedure uniform2uiv(aLocation: IJSWebGLUniformLocation; const aData: TGLuintDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint);
    procedure uniform2uiv(aLocation: IJSWebGLUniformLocation; aData: IJSUint32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint);
    procedure uniform2uiv(aLocation: IJSWebGLUniformLocation; aData: IJSUint32Array);
    procedure uniform2uiv(aLocation: IJSWebGLUniformLocation; const aData: TGLuintDynArray);
    procedure uniform2uiv(aLocation: IJSWebGLUniformLocation; aData: IJSUint32Array; aSrcOffset: TGLuint);
    procedure uniform2uiv(aLocation: IJSWebGLUniformLocation; const aData: TGLuintDynArray; aSrcOffset: TGLuint);
    procedure uniform3uiv(aLocation: IJSWebGLUniformLocation; const aData: TGLuintDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint);
    procedure uniform3uiv(aLocation: IJSWebGLUniformLocation; aData: IJSUint32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint);
    procedure uniform3uiv(aLocation: IJSWebGLUniformLocation; aData: IJSUint32Array);
    procedure uniform3uiv(aLocation: IJSWebGLUniformLocation; const aData: TGLuintDynArray);
    procedure uniform3uiv(aLocation: IJSWebGLUniformLocation; aData: IJSUint32Array; aSrcOffset: TGLuint);
    procedure uniform3uiv(aLocation: IJSWebGLUniformLocation; const aData: TGLuintDynArray; aSrcOffset: TGLuint);
    procedure uniform4uiv(aLocation: IJSWebGLUniformLocation; const aData: TGLuintDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint);
    procedure uniform4uiv(aLocation: IJSWebGLUniformLocation; aData: IJSUint32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint);
    procedure uniform4uiv(aLocation: IJSWebGLUniformLocation; aData: IJSUint32Array);
    procedure uniform4uiv(aLocation: IJSWebGLUniformLocation; const aData: TGLuintDynArray);
    procedure uniform4uiv(aLocation: IJSWebGLUniformLocation; aData: IJSUint32Array; aSrcOffset: TGLuint);
    procedure uniform4uiv(aLocation: IJSWebGLUniformLocation; const aData: TGLuintDynArray; aSrcOffset: TGLuint);
    procedure uniformMatrix2fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint);
    procedure uniformMatrix2fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint);
    procedure uniformMatrix2fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray);
    procedure uniformMatrix2fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array);
    procedure uniformMatrix2fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray; aSrcOffset: TGLuint);
    procedure uniformMatrix2fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array; aSrcOffset: TGLuint);
    procedure uniformMatrix3x2fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint);
    procedure uniformMatrix3x2fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint);
    procedure uniformMatrix3x2fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray);
    procedure uniformMatrix3x2fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array);
    procedure uniformMatrix3x2fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray; aSrcOffset: TGLuint);
    procedure uniformMatrix3x2fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array; aSrcOffset: TGLuint);
    procedure uniformMatrix4x2fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint);
    procedure uniformMatrix4x2fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint);
    procedure uniformMatrix4x2fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray);
    procedure uniformMatrix4x2fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array);
    procedure uniformMatrix4x2fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray; aSrcOffset: TGLuint);
    procedure uniformMatrix4x2fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array; aSrcOffset: TGLuint);
    procedure uniformMatrix2x3fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint);
    procedure uniformMatrix2x3fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint);
    procedure uniformMatrix2x3fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray);
    procedure uniformMatrix2x3fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array);
    procedure uniformMatrix2x3fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray; aSrcOffset: TGLuint);
    procedure uniformMatrix2x3fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array; aSrcOffset: TGLuint);
    procedure uniformMatrix3fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint);
    procedure uniformMatrix3fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint);
    procedure uniformMatrix3fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray);
    procedure uniformMatrix3fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array);
    procedure uniformMatrix3fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray; aSrcOffset: TGLuint);
    procedure uniformMatrix3fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array; aSrcOffset: TGLuint);
    procedure uniformMatrix4x3fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint);
    procedure uniformMatrix4x3fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint);
    procedure uniformMatrix4x3fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray);
    procedure uniformMatrix4x3fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array);
    procedure uniformMatrix4x3fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray; aSrcOffset: TGLuint);
    procedure uniformMatrix4x3fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array; aSrcOffset: TGLuint);
    procedure uniformMatrix2x4fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint);
    procedure uniformMatrix2x4fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint);
    procedure uniformMatrix2x4fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray);
    procedure uniformMatrix2x4fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array);
    procedure uniformMatrix2x4fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray; aSrcOffset: TGLuint);
    procedure uniformMatrix2x4fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array; aSrcOffset: TGLuint);
    procedure uniformMatrix3x4fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint);
    procedure uniformMatrix3x4fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint);
    procedure uniformMatrix3x4fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray);
    procedure uniformMatrix3x4fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array);
    procedure uniformMatrix3x4fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray; aSrcOffset: TGLuint);
    procedure uniformMatrix3x4fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array; aSrcOffset: TGLuint);
    procedure uniformMatrix4fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint);
    procedure uniformMatrix4fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint);
    procedure uniformMatrix4fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray);
    procedure uniformMatrix4fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array);
    procedure uniformMatrix4fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray; aSrcOffset: TGLuint);
    procedure uniformMatrix4fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array; aSrcOffset: TGLuint);
    procedure vertexAttribI4i(aIndex: TGLuint; aX: TGLint; aY: TGLint; aZ: TGLint; aW: TGLint);
    procedure vertexAttribI4iv(aIndex: TGLuint; aValues: IJSInt32Array);
    procedure vertexAttribI4iv(aIndex: TGLuint; const aValues: TGLintDynArray);
    procedure vertexAttribI4ui(aIndex: TGLuint; aX: TGLuint; aY: TGLuint; aZ: TGLuint; aW: TGLuint);
    procedure vertexAttribI4uiv(aIndex: TGLuint; const aValues: TGLuintDynArray);
    procedure vertexAttribI4uiv(aIndex: TGLuint; aValues: IJSUint32Array);
    procedure vertexAttribIPointer(aIndex: TGLuint; aSize: TGLint; aType_: TGLenum; aStride: TGLsizei; aOffset: TGLintptr);
    procedure vertexAttribDivisor(aIndex: TGLuint; aDivisor: TGLuint);
    procedure drawArraysInstanced(aMode: TGLenum; aFirst: TGLint; aCount: TGLsizei; aInstanceCount: TGLsizei);
    procedure drawElementsInstanced(aMode: TGLenum; aCount: TGLsizei; aType_: TGLenum; aOffset: TGLintptr; aInstanceCount: TGLsizei);
    procedure drawRangeElements(aMode: TGLenum; aStart: TGLuint; aEnd_: TGLuint; aCount: TGLsizei; aType_: TGLenum; aOffset: TGLintptr);
    procedure readPixels(aX: TGLint; aY: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aDstData: IJSArrayBufferView);
    procedure readPixels(aX: TGLint; aY: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aOffset: TGLintptr);
    procedure readPixels(aX: TGLint; aY: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aDstData: IJSArrayBufferView; aDstOffset: TGLuint);
    procedure drawBuffers(const aBuffers: TGLenumDynArray);
    procedure clearBufferfv(aBuffer: TGLenum; aDrawbuffer: TGLint; aValues: IJSFloat32Array; aSrcOffset: TGLuint);
    procedure clearBufferfv(aBuffer: TGLenum; aDrawbuffer: TGLint; const aValues: TGLfloatDynArray; aSrcOffset: TGLuint);
    procedure clearBufferfv(aBuffer: TGLenum; aDrawbuffer: TGLint; const aValues: TGLfloatDynArray);
    procedure clearBufferfv(aBuffer: TGLenum; aDrawbuffer: TGLint; aValues: IJSFloat32Array);
    procedure clearBufferiv(aBuffer: TGLenum; aDrawbuffer: TGLint; aValues: IJSInt32Array; aSrcOffset: TGLuint);
    procedure clearBufferiv(aBuffer: TGLenum; aDrawbuffer: TGLint; const aValues: TGLintDynArray; aSrcOffset: TGLuint);
    procedure clearBufferiv(aBuffer: TGLenum; aDrawbuffer: TGLint; const aValues: TGLintDynArray);
    procedure clearBufferiv(aBuffer: TGLenum; aDrawbuffer: TGLint; aValues: IJSInt32Array);
    procedure clearBufferuiv(aBuffer: TGLenum; aDrawbuffer: TGLint; const aValues: TGLuintDynArray; aSrcOffset: TGLuint);
    procedure clearBufferuiv(aBuffer: TGLenum; aDrawbuffer: TGLint; aValues: IJSUint32Array; aSrcOffset: TGLuint);
    procedure clearBufferuiv(aBuffer: TGLenum; aDrawbuffer: TGLint; aValues: IJSUint32Array);
    procedure clearBufferuiv(aBuffer: TGLenum; aDrawbuffer: TGLint; const aValues: TGLuintDynArray);
    procedure clearBufferfi(aBuffer: TGLenum; aDrawbuffer: TGLint; aDepth: TGLfloat; aStencil: TGLint);
    function createQuery: IJSWebGLQuery;
    procedure deleteQuery(aQuery: IJSWebGLQuery);
    function isQuery(aQuery: IJSWebGLQuery): TGLboolean;
    procedure beginQuery(aTarget: TGLenum; aQuery: IJSWebGLQuery);
    procedure endQuery(aTarget: TGLenum);
    function getQuery(aTarget: TGLenum; aPname: TGLenum): Variant;
    function getQueryParameter(aQuery: IJSWebGLQuery; aPname: TGLenum): Variant;
    function createSampler: IJSWebGLSampler;
    procedure deleteSampler(aSampler: IJSWebGLSampler);
    function isSampler(aSampler: IJSWebGLSampler): TGLboolean;
    procedure bindSampler(aUnit_: TGLuint; aSampler: IJSWebGLSampler);
    procedure samplerParameteri(aSampler: IJSWebGLSampler; aPname: TGLenum; aParam: TGLint);
    procedure samplerParameterf(aSampler: IJSWebGLSampler; aPname: TGLenum; aParam: TGLfloat);
    function getSamplerParameter(aSampler: IJSWebGLSampler; aPname: TGLenum): Variant;
    function fenceSync(aCondition: TGLenum; aFlags: TGLbitfield): IJSWebGLSync;
    function isSync(aSync: IJSWebGLSync): TGLboolean;
    procedure deleteSync(aSync: IJSWebGLSync);
    function clientWaitSync(aSync: IJSWebGLSync; aFlags: TGLbitfield; aTimeout: TGLuint64): TGLenum;
    procedure waitSync(aSync: IJSWebGLSync; aFlags: TGLbitfield; aTimeout: TGLint64);
    function getSyncParameter(aSync: IJSWebGLSync; aPname: TGLenum): Variant;
    function createTransformFeedback: IJSWebGLTransformFeedback;
    procedure deleteTransformFeedback(aTf: IJSWebGLTransformFeedback);
    function isTransformFeedback(aTf: IJSWebGLTransformFeedback): TGLboolean;
    procedure bindTransformFeedback(aTarget: TGLenum; aTf: IJSWebGLTransformFeedback);
    procedure beginTransformFeedback(aPrimitiveMode: TGLenum);
    procedure endTransformFeedback;
    procedure transformFeedbackVaryings(aProgram_: IJSWebGLProgram; const aVaryings: TUnicodeStringDynArray; aBufferMode: TGLenum);
    function getTransformFeedbackVarying(aProgram_: IJSWebGLProgram; aIndex: TGLuint): IJSWebGLActiveInfo;
    procedure pauseTransformFeedback;
    procedure resumeTransformFeedback;
    procedure bindBufferBase(aTarget: TGLenum; aIndex: TGLuint; aBuffer: IJSWebGLBuffer);
    procedure bindBufferRange(aTarget: TGLenum; aIndex: TGLuint; aBuffer: IJSWebGLBuffer; aOffset: TGLintptr; aSize: TGLsizeiptr);
    function getIndexedParameter(aTarget: TGLenum; aIndex: TGLuint): Variant;
    function getUniformIndices(aProgram_: IJSWebGLProgram; const aUniformNames: TUnicodeStringDynArray): TGLuintDynArray;
    function getActiveUniforms(aProgram_: IJSWebGLProgram; const aUniformIndices: TGLuintDynArray; aPname: TGLenum): Variant;
    function getUniformBlockIndex(aProgram_: IJSWebGLProgram; const aUniformBlockName: UnicodeString): TGLuint;
    function getActiveUniformBlockParameter(aProgram_: IJSWebGLProgram; aUniformBlockIndex: TGLuint; aPname: TGLenum): Variant;
    function getActiveUniformBlockName(aProgram_: IJSWebGLProgram; aUniformBlockIndex: TGLuint): UnicodeString;
    procedure uniformBlockBinding(aProgram_: IJSWebGLProgram; aUniformBlockIndex: TGLuint; aUniformBlockBinding: TGLuint);
    function createVertexArray: IJSWebGLVertexArrayObject;
    procedure deleteVertexArray(aVertexArray: IJSWebGLVertexArrayObject);
    function isVertexArray(aVertexArray: IJSWebGLVertexArrayObject): TGLboolean;
    procedure bindVertexArray(aArray_: IJSWebGLVertexArrayObject);
  end;
  
  TJSWebGL2RenderingContextBase = class(TJSObject,IJSWebGL2RenderingContextBase)
  Private
  Protected
  Public
    Const
      READ_BUFFER = $0C02;
      UNPACK_ROW_LENGTH = $0CF2;
      UNPACK_SKIP_ROWS = $0CF3;
      UNPACK_SKIP_PIXELS = $0CF4;
      PACK_ROW_LENGTH = $0D02;
      PACK_SKIP_ROWS = $0D03;
      PACK_SKIP_PIXELS = $0D04;
      COLOR = $1800;
      DEPTH = $1801;
      STENCIL = $1802;
      RED = $1903;
      RGB8 = $8051;
      RGBA8 = $8058;
      RGB10_A2 = $8059;
      TEXTURE_BINDING_3D = $806A;
      UNPACK_SKIP_IMAGES = $806D;
      UNPACK_IMAGE_HEIGHT = $806E;
      TEXTURE_3D = $806F;
      TEXTURE_WRAP_R = $8072;
      MAX_3D_TEXTURE_SIZE = $8073;
      UNSIGNED_INT_2_10_10_10_REV = $8368;
      MAX_ELEMENTS_VERTICES = $80E8;
      MAX_ELEMENTS_INDICES = $80E9;
      TEXTURE_MIN_LOD = $813A;
      TEXTURE_MAX_LOD = $813B;
      TEXTURE_BASE_LEVEL = $813C;
      TEXTURE_MAX_LEVEL = $813D;
      MIN = $8007;
      MAX = $8008;
      DEPTH_COMPONENT24 = $81A6;
      MAX_TEXTURE_LOD_BIAS = $84FD;
      TEXTURE_COMPARE_MODE = $884C;
      TEXTURE_COMPARE_FUNC = $884D;
      CURRENT_QUERY = $8865;
      QUERY_RESULT = $8866;
      QUERY_RESULT_AVAILABLE = $8867;
      STREAM_READ = $88E1;
      STREAM_COPY = $88E2;
      STATIC_READ = $88E5;
      STATIC_COPY = $88E6;
      DYNAMIC_READ = $88E9;
      DYNAMIC_COPY = $88EA;
      MAX_DRAW_BUFFERS = $8824;
      DRAW_BUFFER0 = $8825;
      DRAW_BUFFER1 = $8826;
      DRAW_BUFFER2 = $8827;
      DRAW_BUFFER3 = $8828;
      DRAW_BUFFER4 = $8829;
      DRAW_BUFFER5 = $882A;
      DRAW_BUFFER6 = $882B;
      DRAW_BUFFER7 = $882C;
      DRAW_BUFFER8 = $882D;
      DRAW_BUFFER9 = $882E;
      DRAW_BUFFER10 = $882F;
      DRAW_BUFFER11 = $8830;
      DRAW_BUFFER12 = $8831;
      DRAW_BUFFER13 = $8832;
      DRAW_BUFFER14 = $8833;
      DRAW_BUFFER15 = $8834;
      MAX_FRAGMENT_UNIFORM_COMPONENTS = $8B49;
      MAX_VERTEX_UNIFORM_COMPONENTS = $8B4A;
      SAMPLER_3D = $8B5F;
      SAMPLER_2D_SHADOW = $8B62;
      FRAGMENT_SHADER_DERIVATIVE_HINT = $8B8B;
      PIXEL_PACK_BUFFER = $88EB;
      PIXEL_UNPACK_BUFFER = $88EC;
      PIXEL_PACK_BUFFER_BINDING = $88ED;
      PIXEL_UNPACK_BUFFER_BINDING = $88EF;
      FLOAT_MAT2x3 = $8B65;
      FLOAT_MAT2x4 = $8B66;
      FLOAT_MAT3x2 = $8B67;
      FLOAT_MAT3x4 = $8B68;
      FLOAT_MAT4x2 = $8B69;
      FLOAT_MAT4x3 = $8B6A;
      SRGB = $8C40;
      SRGB8 = $8C41;
      SRGB8_ALPHA8 = $8C43;
      COMPARE_REF_TO_TEXTURE = $884E;
      RGBA32F = $8814;
      RGB32F = $8815;
      RGBA16F = $881A;
      RGB16F = $881B;
      VERTEX_ATTRIB_ARRAY_INTEGER = $88FD;
      MAX_ARRAY_TEXTURE_LAYERS = $88FF;
      MIN_PROGRAM_TEXEL_OFFSET = $8904;
      MAX_PROGRAM_TEXEL_OFFSET = $8905;
      MAX_VARYING_COMPONENTS = $8B4B;
      TEXTURE_2D_ARRAY = $8C1A;
      TEXTURE_BINDING_2D_ARRAY = $8C1D;
      R11F_G11F_B10F = $8C3A;
      UNSIGNED_INT_10F_11F_11F_REV = $8C3B;
      RGB9_E5 = $8C3D;
      UNSIGNED_INT_5_9_9_9_REV = $8C3E;
      TRANSFORM_FEEDBACK_BUFFER_MODE = $8C7F;
      MAX_TRANSFORM_FEEDBACK_SEPARATE_COMPONENTS = $8C80;
      TRANSFORM_FEEDBACK_VARYINGS = $8C83;
      TRANSFORM_FEEDBACK_BUFFER_START = $8C84;
      TRANSFORM_FEEDBACK_BUFFER_SIZE = $8C85;
      TRANSFORM_FEEDBACK_PRIMITIVES_WRITTEN = $8C88;
      RASTERIZER_DISCARD = $8C89;
      MAX_TRANSFORM_FEEDBACK_INTERLEAVED_COMPONENTS = $8C8A;
      MAX_TRANSFORM_FEEDBACK_SEPARATE_ATTRIBS = $8C8B;
      INTERLEAVED_ATTRIBS = $8C8C;
      SEPARATE_ATTRIBS = $8C8D;
      TRANSFORM_FEEDBACK_BUFFER = $8C8E;
      TRANSFORM_FEEDBACK_BUFFER_BINDING = $8C8F;
      RGBA32UI = $8D70;
      RGB32UI = $8D71;
      RGBA16UI = $8D76;
      RGB16UI = $8D77;
      RGBA8UI = $8D7C;
      RGB8UI = $8D7D;
      RGBA32I = $8D82;
      RGB32I = $8D83;
      RGBA16I = $8D88;
      RGB16I = $8D89;
      RGBA8I = $8D8E;
      RGB8I = $8D8F;
      RED_INTEGER = $8D94;
      RGB_INTEGER = $8D98;
      RGBA_INTEGER = $8D99;
      SAMPLER_2D_ARRAY = $8DC1;
      SAMPLER_2D_ARRAY_SHADOW = $8DC4;
      SAMPLER_CUBE_SHADOW = $8DC5;
      UNSIGNED_INT_VEC2 = $8DC6;
      UNSIGNED_INT_VEC3 = $8DC7;
      UNSIGNED_INT_VEC4 = $8DC8;
      INT_SAMPLER_2D = $8DCA;
      INT_SAMPLER_3D = $8DCB;
      INT_SAMPLER_CUBE = $8DCC;
      INT_SAMPLER_2D_ARRAY = $8DCF;
      UNSIGNED_INT_SAMPLER_2D = $8DD2;
      UNSIGNED_INT_SAMPLER_3D = $8DD3;
      UNSIGNED_INT_SAMPLER_CUBE = $8DD4;
      UNSIGNED_INT_SAMPLER_2D_ARRAY = $8DD7;
      DEPTH_COMPONENT32F = $8CAC;
      DEPTH32F_STENCIL8 = $8CAD;
      FLOAT_32_UNSIGNED_INT_24_8_REV = $8DAD;
      FRAMEBUFFER_ATTACHMENT_COLOR_ENCODING = $8210;
      FRAMEBUFFER_ATTACHMENT_COMPONENT_TYPE = $8211;
      FRAMEBUFFER_ATTACHMENT_RED_SIZE = $8212;
      FRAMEBUFFER_ATTACHMENT_GREEN_SIZE = $8213;
      FRAMEBUFFER_ATTACHMENT_BLUE_SIZE = $8214;
      FRAMEBUFFER_ATTACHMENT_ALPHA_SIZE = $8215;
      FRAMEBUFFER_ATTACHMENT_DEPTH_SIZE = $8216;
      FRAMEBUFFER_ATTACHMENT_STENCIL_SIZE = $8217;
      FRAMEBUFFER_DEFAULT = $8218;
      UNSIGNED_INT_24_8 = $84FA;
      DEPTH24_STENCIL8 = $88F0;
      UNSIGNED_NORMALIZED = $8C17;
      DRAW_FRAMEBUFFER_BINDING = $8CA6;
      READ_FRAMEBUFFER = $8CA8;
      DRAW_FRAMEBUFFER = $8CA9;
      READ_FRAMEBUFFER_BINDING = $8CAA;
      RENDERBUFFER_SAMPLES = $8CAB;
      FRAMEBUFFER_ATTACHMENT_TEXTURE_LAYER = $8CD4;
      MAX_COLOR_ATTACHMENTS = $8CDF;
      COLOR_ATTACHMENT1 = $8CE1;
      COLOR_ATTACHMENT2 = $8CE2;
      COLOR_ATTACHMENT3 = $8CE3;
      COLOR_ATTACHMENT4 = $8CE4;
      COLOR_ATTACHMENT5 = $8CE5;
      COLOR_ATTACHMENT6 = $8CE6;
      COLOR_ATTACHMENT7 = $8CE7;
      COLOR_ATTACHMENT8 = $8CE8;
      COLOR_ATTACHMENT9 = $8CE9;
      COLOR_ATTACHMENT10 = $8CEA;
      COLOR_ATTACHMENT11 = $8CEB;
      COLOR_ATTACHMENT12 = $8CEC;
      COLOR_ATTACHMENT13 = $8CED;
      COLOR_ATTACHMENT14 = $8CEE;
      COLOR_ATTACHMENT15 = $8CEF;
      FRAMEBUFFER_INCOMPLETE_MULTISAMPLE = $8D56;
      MAX_SAMPLES = $8D57;
      HALF_FLOAT = $140B;
      RG = $8227;
      RG_INTEGER = $8228;
      R8 = $8229;
      RG8 = $822B;
      R16F = $822D;
      R32F = $822E;
      RG16F = $822F;
      RG32F = $8230;
      R8I = $8231;
      R8UI = $8232;
      R16I = $8233;
      R16UI = $8234;
      R32I = $8235;
      R32UI = $8236;
      RG8I = $8237;
      RG8UI = $8238;
      RG16I = $8239;
      RG16UI = $823A;
      RG32I = $823B;
      RG32UI = $823C;
      VERTEX_ARRAY_BINDING = $85B5;
      R8_SNORM = $8F94;
      RG8_SNORM = $8F95;
      RGB8_SNORM = $8F96;
      RGBA8_SNORM = $8F97;
      SIGNED_NORMALIZED = $8F9C;
      COPY_READ_BUFFER = $8F36;
      COPY_WRITE_BUFFER = $8F37;
      COPY_READ_BUFFER_BINDING = $8F36;
      COPY_WRITE_BUFFER_BINDING = $8F37;
      UNIFORM_BUFFER = $8A11;
      UNIFORM_BUFFER_BINDING = $8A28;
      UNIFORM_BUFFER_START = $8A29;
      UNIFORM_BUFFER_SIZE = $8A2A;
      MAX_VERTEX_UNIFORM_BLOCKS = $8A2B;
      MAX_FRAGMENT_UNIFORM_BLOCKS = $8A2D;
      MAX_COMBINED_UNIFORM_BLOCKS = $8A2E;
      MAX_UNIFORM_BUFFER_BINDINGS = $8A2F;
      MAX_UNIFORM_BLOCK_SIZE = $8A30;
      MAX_COMBINED_VERTEX_UNIFORM_COMPONENTS = $8A31;
      MAX_COMBINED_FRAGMENT_UNIFORM_COMPONENTS = $8A33;
      UNIFORM_BUFFER_OFFSET_ALIGNMENT = $8A34;
      ACTIVE_UNIFORM_BLOCKS = $8A36;
      UNIFORM_TYPE = $8A37;
      UNIFORM_SIZE = $8A38;
      UNIFORM_BLOCK_INDEX = $8A3A;
      UNIFORM_OFFSET = $8A3B;
      UNIFORM_ARRAY_STRIDE = $8A3C;
      UNIFORM_MATRIX_STRIDE = $8A3D;
      UNIFORM_IS_ROW_MAJOR = $8A3E;
      UNIFORM_BLOCK_BINDING = $8A3F;
      UNIFORM_BLOCK_DATA_SIZE = $8A40;
      UNIFORM_BLOCK_ACTIVE_UNIFORMS = $8A42;
      UNIFORM_BLOCK_ACTIVE_UNIFORM_INDICES = $8A43;
      UNIFORM_BLOCK_REFERENCED_BY_VERTEX_SHADER = $8A44;
      UNIFORM_BLOCK_REFERENCED_BY_FRAGMENT_SHADER = $8A46;
      INVALID_INDEX = $FFFFFFFF;
      MAX_VERTEX_OUTPUT_COMPONENTS = $9122;
      MAX_FRAGMENT_INPUT_COMPONENTS = $9125;
      MAX_SERVER_WAIT_TIMEOUT = $9111;
      OBJECT_TYPE = $9112;
      SYNC_CONDITION = $9113;
      SYNC_STATUS = $9114;
      SYNC_FLAGS = $9115;
      SYNC_FENCE = $9116;
      SYNC_GPU_COMMANDS_COMPLETE = $9117;
      UNSIGNALED = $9118;
      SIGNALED = $9119;
      ALREADY_SIGNALED = $911A;
      TIMEOUT_EXPIRED = $911B;
      CONDITION_SATISFIED = $911C;
      WAIT_FAILED = $911D;
      SYNC_FLUSH_COMMANDS_BIT = $00000001;
      VERTEX_ATTRIB_ARRAY_DIVISOR = $88FE;
      ANY_SAMPLES_PASSED = $8C2F;
      ANY_SAMPLES_PASSED_CONSERVATIVE = $8D6A;
      SAMPLER_BINDING = $8919;
      RGB10_A2UI = $906F;
      INT_2_10_10_10_REV = $8D9F;
      TRANSFORM_FEEDBACK = $8E22;
      TRANSFORM_FEEDBACK_PAUSED = $8E23;
      TRANSFORM_FEEDBACK_ACTIVE = $8E24;
      TRANSFORM_FEEDBACK_BINDING = $8E25;
      TEXTURE_IMMUTABLE_FORMAT = $912F;
      MAX_ELEMENT_INDEX = $8D6B;
      TEXTURE_IMMUTABLE_LEVELS = $82DF;
      TIMEOUT_IGNORED = -1;
      MAX_CLIENT_WAIT_TIMEOUT_WEBGL = $9247;
  Public
    procedure bufferData(aTarget: TGLenum; aSize: TGLsizeiptr; aUsage: TGLenum); overload;
    procedure bufferData(aTarget: TGLenum; aSrcData: IJSArrayBuffer; aUsage: TGLenum); overload;
    procedure bufferData(aTarget: TGLenum; aSrcData: IJSArrayBufferView; aUsage: TGLenum); overload;
    procedure bufferSubData(aTarget: TGLenum; aOffset: TGLintptr; aSrcData: IJSArrayBuffer); overload;
    procedure bufferSubData(aTarget: TGLenum; aOffset: TGLintptr; aSrcData: IJSArrayBufferView); overload;
    procedure bufferData(aTarget: TGLenum; aSrcData: IJSArrayBufferView; aUsage: TGLenum; aSrcOffset: TGLuint; aLength_: TGLuint); overload;
    procedure bufferData(aTarget: TGLenum; aSrcData: IJSArrayBufferView; aUsage: TGLenum; aSrcOffset: TGLuint); overload;
    procedure bufferSubData(aTarget: TGLenum; aDstByteOffset: TGLintptr; aSrcData: IJSArrayBufferView; aSrcOffset: TGLuint; aLength_: TGLuint); overload;
    procedure bufferSubData(aTarget: TGLenum; aDstByteOffset: TGLintptr; aSrcData: IJSArrayBufferView; aSrcOffset: TGLuint); overload;
    procedure copyBufferSubData(aReadTarget: TGLenum; aWriteTarget: TGLenum; aReadOffset: TGLintptr; aWriteOffset: TGLintptr; aSize: TGLsizeiptr); overload;
    procedure getBufferSubData(aTarget: TGLenum; aSrcByteOffset: TGLintptr; aDstData: IJSArrayBufferView; aDstOffset: TGLuint; aLength_: TGLuint); overload;
    procedure getBufferSubData(aTarget: TGLenum; aSrcByteOffset: TGLintptr; aDstData: IJSArrayBufferView); overload;
    procedure getBufferSubData(aTarget: TGLenum; aSrcByteOffset: TGLintptr; aDstData: IJSArrayBufferView; aDstOffset: TGLuint); overload;
    procedure blitFramebuffer(aSrcX0: TGLint; aSrcY0: TGLint; aSrcX1: TGLint; aSrcY1: TGLint; aDstX0: TGLint; aDstY0: TGLint; aDstX1: TGLint; aDstY1: TGLint; aMask: TGLbitfield; aFilter: TGLenum); overload;
    procedure framebufferTextureLayer(aTarget: TGLenum; aAttachment: TGLenum; aTexture: IJSWebGLTexture; aLevel: TGLint; aLayer: TGLint); overload;
    procedure invalidateFramebuffer(aTarget: TGLenum; const aAttachments: TGLenumDynArray); overload;
    procedure invalidateSubFramebuffer(aTarget: TGLenum; const aAttachments: TGLenumDynArray; aX: TGLint; aY: TGLint; aWidth: TGLsizei; aHeight: TGLsizei); overload;
    procedure readBuffer(aSrc: TGLenum); overload;
    function getInternalformatParameter(aTarget: TGLenum; aInternalformat: TGLenum; aPname: TGLenum): Variant; overload;
    procedure renderbufferStorageMultisample(aTarget: TGLenum; aSamples: TGLsizei; aInternalformat: TGLenum; aWidth: TGLsizei; aHeight: TGLsizei); overload;
    procedure texStorage2D(aTarget: TGLenum; aLevels: TGLsizei; aInternalformat: TGLenum; aWidth: TGLsizei; aHeight: TGLsizei); overload;
    procedure texStorage3D(aTarget: TGLenum; aLevels: TGLsizei; aInternalformat: TGLenum; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei); overload;
    procedure texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aPixels: IJSArrayBufferView); overload;
    procedure texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSHTMLCanvasElement); overload;
    procedure texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSHTMLImageElement); overload;
    procedure texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSHTMLVideoElement); overload;
    procedure texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSImageBitmap); overload;
    procedure texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSImageData); overload;
    procedure texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSOffscreenCanvas); overload;
    procedure texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSVideoFrame); overload;
    procedure texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aPixels: IJSArrayBufferView); overload;
    procedure texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSHTMLCanvasElement); overload;
    procedure texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSHTMLImageElement); overload;
    procedure texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSHTMLVideoElement); overload;
    procedure texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSImageBitmap); overload;
    procedure texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSImageData); overload;
    procedure texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSOffscreenCanvas); overload;
    procedure texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSVideoFrame); overload;
    procedure texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aPboOffset: TGLintptr); overload;
    procedure texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSHTMLCanvasElement); overload;
    procedure texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSHTMLImageElement); overload;
    procedure texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSHTMLVideoElement); overload;
    procedure texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSImageBitmap); overload;
    procedure texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSImageData); overload;
    procedure texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSOffscreenCanvas); overload;
    procedure texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSVideoFrame); overload;
    procedure texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aSrcData: IJSArrayBufferView; aSrcOffset: TGLuint); overload;
    procedure texImage3D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aPboOffset: TGLintptr); overload;
    procedure texImage3D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSHTMLCanvasElement); overload;
    procedure texImage3D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSHTMLImageElement); overload;
    procedure texImage3D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSHTMLVideoElement); overload;
    procedure texImage3D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSImageBitmap); overload;
    procedure texImage3D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSImageData); overload;
    procedure texImage3D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSOffscreenCanvas); overload;
    procedure texImage3D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSVideoFrame); overload;
    procedure texImage3D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aSrcData: IJSArrayBufferView); overload;
    procedure texImage3D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aSrcData: IJSArrayBufferView; aSrcOffset: TGLuint); overload;
    procedure texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aPboOffset: TGLintptr); overload;
    procedure texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aSource: IJSHTMLCanvasElement); overload;
    procedure texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aSource: IJSHTMLImageElement); overload;
    procedure texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aSource: IJSHTMLVideoElement); overload;
    procedure texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aSource: IJSImageBitmap); overload;
    procedure texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aSource: IJSImageData); overload;
    procedure texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aSource: IJSOffscreenCanvas); overload;
    procedure texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aSource: IJSVideoFrame); overload;
    procedure texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aSrcData: IJSArrayBufferView; aSrcOffset: TGLuint); overload;
    procedure texSubImage3D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aZoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aPboOffset: TGLintptr); overload;
    procedure texSubImage3D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aZoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aSource: IJSHTMLCanvasElement); overload;
    procedure texSubImage3D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aZoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aSource: IJSHTMLImageElement); overload;
    procedure texSubImage3D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aZoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aSource: IJSHTMLVideoElement); overload;
    procedure texSubImage3D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aZoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aSource: IJSImageBitmap); overload;
    procedure texSubImage3D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aZoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aSource: IJSImageData); overload;
    procedure texSubImage3D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aZoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aSource: IJSOffscreenCanvas); overload;
    procedure texSubImage3D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aZoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aSource: IJSVideoFrame); overload;
    procedure texSubImage3D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aZoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aSrcData: IJSArrayBufferView; aSrcOffset: TGLuint); overload;
    procedure texSubImage3D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aZoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aSrcData: IJSArrayBufferView); overload;
    procedure copyTexSubImage3D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aZoffset: TGLint; aX: TGLint; aY: TGLint; aWidth: TGLsizei; aHeight: TGLsizei); overload;
    procedure compressedTexImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLenum; aWidth: TGLsizei; aHeight: TGLsizei; aBorder: TGLint; aImageSize: TGLsizei; aOffset: TGLintptr); overload;
    procedure compressedTexImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLenum; aWidth: TGLsizei; aHeight: TGLsizei; aBorder: TGLint; aSrcData: IJSArrayBufferView; aSrcOffset: TGLuint; aSrcLengthOverride: TGLuint); overload;
    procedure compressedTexImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLenum; aWidth: TGLsizei; aHeight: TGLsizei; aBorder: TGLint; aSrcData: IJSArrayBufferView); overload;
    procedure compressedTexImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLenum; aWidth: TGLsizei; aHeight: TGLsizei; aBorder: TGLint; aSrcData: IJSArrayBufferView; aSrcOffset: TGLuint); overload;
    procedure compressedTexImage3D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLenum; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aBorder: TGLint; aImageSize: TGLsizei; aOffset: TGLintptr); overload;
    procedure compressedTexImage3D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLenum; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aBorder: TGLint; aSrcData: IJSArrayBufferView; aSrcOffset: TGLuint; aSrcLengthOverride: TGLuint); overload;
    procedure compressedTexImage3D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLenum; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aBorder: TGLint; aSrcData: IJSArrayBufferView); overload;
    procedure compressedTexImage3D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLenum; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aBorder: TGLint; aSrcData: IJSArrayBufferView; aSrcOffset: TGLuint); overload;
    procedure compressedTexSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aFormat: TGLenum; aImageSize: TGLsizei; aOffset: TGLintptr); overload;
    procedure compressedTexSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aFormat: TGLenum; aSrcData: IJSArrayBufferView; aSrcOffset: TGLuint; aSrcLengthOverride: TGLuint); overload;
    procedure compressedTexSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aFormat: TGLenum; aSrcData: IJSArrayBufferView); overload;
    procedure compressedTexSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aFormat: TGLenum; aSrcData: IJSArrayBufferView; aSrcOffset: TGLuint); overload;
    procedure compressedTexSubImage3D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aZoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aFormat: TGLenum; aImageSize: TGLsizei; aOffset: TGLintptr); overload;
    procedure compressedTexSubImage3D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aZoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aFormat: TGLenum; aSrcData: IJSArrayBufferView; aSrcOffset: TGLuint; aSrcLengthOverride: TGLuint); overload;
    procedure compressedTexSubImage3D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aZoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aFormat: TGLenum; aSrcData: IJSArrayBufferView); overload;
    procedure compressedTexSubImage3D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aZoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aFormat: TGLenum; aSrcData: IJSArrayBufferView; aSrcOffset: TGLuint); overload;
    function getFragDataLocation(aProgram_: IJSWebGLProgram; const aName: UnicodeString): TGLint; overload;
    procedure uniform1ui(aLocation: IJSWebGLUniformLocation; aV0: TGLuint); overload;
    procedure uniform2ui(aLocation: IJSWebGLUniformLocation; aV0: TGLuint; aV1: TGLuint); overload;
    procedure uniform3ui(aLocation: IJSWebGLUniformLocation; aV0: TGLuint; aV1: TGLuint; aV2: TGLuint); overload;
    procedure uniform4ui(aLocation: IJSWebGLUniformLocation; aV0: TGLuint; aV1: TGLuint; aV2: TGLuint; aV3: TGLuint); overload;
    procedure uniform1fv(aLocation: IJSWebGLUniformLocation; aData: IJSFloat32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint); overload;
    procedure uniform1fv(aLocation: IJSWebGLUniformLocation; const aData: TGLfloatDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint); overload;
    procedure uniform1fv(aLocation: IJSWebGLUniformLocation; const aData: TGLfloatDynArray); overload;
    procedure uniform1fv(aLocation: IJSWebGLUniformLocation; aData: IJSFloat32Array); overload;
    procedure uniform1fv(aLocation: IJSWebGLUniformLocation; const aData: TGLfloatDynArray; aSrcOffset: TGLuint); overload;
    procedure uniform1fv(aLocation: IJSWebGLUniformLocation; aData: IJSFloat32Array; aSrcOffset: TGLuint); overload;
    procedure uniform2fv(aLocation: IJSWebGLUniformLocation; aData: IJSFloat32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint); overload;
    procedure uniform2fv(aLocation: IJSWebGLUniformLocation; const aData: TGLfloatDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint); overload;
    procedure uniform2fv(aLocation: IJSWebGLUniformLocation; const aData: TGLfloatDynArray); overload;
    procedure uniform2fv(aLocation: IJSWebGLUniformLocation; aData: IJSFloat32Array); overload;
    procedure uniform2fv(aLocation: IJSWebGLUniformLocation; const aData: TGLfloatDynArray; aSrcOffset: TGLuint); overload;
    procedure uniform2fv(aLocation: IJSWebGLUniformLocation; aData: IJSFloat32Array; aSrcOffset: TGLuint); overload;
    procedure uniform3fv(aLocation: IJSWebGLUniformLocation; aData: IJSFloat32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint); overload;
    procedure uniform3fv(aLocation: IJSWebGLUniformLocation; const aData: TGLfloatDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint); overload;
    procedure uniform3fv(aLocation: IJSWebGLUniformLocation; const aData: TGLfloatDynArray); overload;
    procedure uniform3fv(aLocation: IJSWebGLUniformLocation; aData: IJSFloat32Array); overload;
    procedure uniform3fv(aLocation: IJSWebGLUniformLocation; const aData: TGLfloatDynArray; aSrcOffset: TGLuint); overload;
    procedure uniform3fv(aLocation: IJSWebGLUniformLocation; aData: IJSFloat32Array; aSrcOffset: TGLuint); overload;
    procedure uniform4fv(aLocation: IJSWebGLUniformLocation; aData: IJSFloat32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint); overload;
    procedure uniform4fv(aLocation: IJSWebGLUniformLocation; const aData: TGLfloatDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint); overload;
    procedure uniform4fv(aLocation: IJSWebGLUniformLocation; const aData: TGLfloatDynArray); overload;
    procedure uniform4fv(aLocation: IJSWebGLUniformLocation; aData: IJSFloat32Array); overload;
    procedure uniform4fv(aLocation: IJSWebGLUniformLocation; const aData: TGLfloatDynArray; aSrcOffset: TGLuint); overload;
    procedure uniform4fv(aLocation: IJSWebGLUniformLocation; aData: IJSFloat32Array; aSrcOffset: TGLuint); overload;
    procedure uniform1iv(aLocation: IJSWebGLUniformLocation; aData: IJSInt32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint); overload;
    procedure uniform1iv(aLocation: IJSWebGLUniformLocation; const aData: TGLintDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint); overload;
    procedure uniform1iv(aLocation: IJSWebGLUniformLocation; const aData: TGLintDynArray); overload;
    procedure uniform1iv(aLocation: IJSWebGLUniformLocation; aData: IJSInt32Array); overload;
    procedure uniform1iv(aLocation: IJSWebGLUniformLocation; const aData: TGLintDynArray; aSrcOffset: TGLuint); overload;
    procedure uniform1iv(aLocation: IJSWebGLUniformLocation; aData: IJSInt32Array; aSrcOffset: TGLuint); overload;
    procedure uniform2iv(aLocation: IJSWebGLUniformLocation; aData: IJSInt32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint); overload;
    procedure uniform2iv(aLocation: IJSWebGLUniformLocation; const aData: TGLintDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint); overload;
    procedure uniform2iv(aLocation: IJSWebGLUniformLocation; const aData: TGLintDynArray); overload;
    procedure uniform2iv(aLocation: IJSWebGLUniformLocation; aData: IJSInt32Array); overload;
    procedure uniform2iv(aLocation: IJSWebGLUniformLocation; const aData: TGLintDynArray; aSrcOffset: TGLuint); overload;
    procedure uniform2iv(aLocation: IJSWebGLUniformLocation; aData: IJSInt32Array; aSrcOffset: TGLuint); overload;
    procedure uniform3iv(aLocation: IJSWebGLUniformLocation; aData: IJSInt32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint); overload;
    procedure uniform3iv(aLocation: IJSWebGLUniformLocation; const aData: TGLintDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint); overload;
    procedure uniform3iv(aLocation: IJSWebGLUniformLocation; const aData: TGLintDynArray); overload;
    procedure uniform3iv(aLocation: IJSWebGLUniformLocation; aData: IJSInt32Array); overload;
    procedure uniform3iv(aLocation: IJSWebGLUniformLocation; const aData: TGLintDynArray; aSrcOffset: TGLuint); overload;
    procedure uniform3iv(aLocation: IJSWebGLUniformLocation; aData: IJSInt32Array; aSrcOffset: TGLuint); overload;
    procedure uniform4iv(aLocation: IJSWebGLUniformLocation; aData: IJSInt32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint); overload;
    procedure uniform4iv(aLocation: IJSWebGLUniformLocation; const aData: TGLintDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint); overload;
    procedure uniform4iv(aLocation: IJSWebGLUniformLocation; const aData: TGLintDynArray); overload;
    procedure uniform4iv(aLocation: IJSWebGLUniformLocation; aData: IJSInt32Array); overload;
    procedure uniform4iv(aLocation: IJSWebGLUniformLocation; const aData: TGLintDynArray; aSrcOffset: TGLuint); overload;
    procedure uniform4iv(aLocation: IJSWebGLUniformLocation; aData: IJSInt32Array; aSrcOffset: TGLuint); overload;
    procedure uniform1uiv(aLocation: IJSWebGLUniformLocation; const aData: TGLuintDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint); overload;
    procedure uniform1uiv(aLocation: IJSWebGLUniformLocation; aData: IJSUint32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint); overload;
    procedure uniform1uiv(aLocation: IJSWebGLUniformLocation; aData: IJSUint32Array); overload;
    procedure uniform1uiv(aLocation: IJSWebGLUniformLocation; const aData: TGLuintDynArray); overload;
    procedure uniform1uiv(aLocation: IJSWebGLUniformLocation; aData: IJSUint32Array; aSrcOffset: TGLuint); overload;
    procedure uniform1uiv(aLocation: IJSWebGLUniformLocation; const aData: TGLuintDynArray; aSrcOffset: TGLuint); overload;
    procedure uniform2uiv(aLocation: IJSWebGLUniformLocation; const aData: TGLuintDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint); overload;
    procedure uniform2uiv(aLocation: IJSWebGLUniformLocation; aData: IJSUint32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint); overload;
    procedure uniform2uiv(aLocation: IJSWebGLUniformLocation; aData: IJSUint32Array); overload;
    procedure uniform2uiv(aLocation: IJSWebGLUniformLocation; const aData: TGLuintDynArray); overload;
    procedure uniform2uiv(aLocation: IJSWebGLUniformLocation; aData: IJSUint32Array; aSrcOffset: TGLuint); overload;
    procedure uniform2uiv(aLocation: IJSWebGLUniformLocation; const aData: TGLuintDynArray; aSrcOffset: TGLuint); overload;
    procedure uniform3uiv(aLocation: IJSWebGLUniformLocation; const aData: TGLuintDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint); overload;
    procedure uniform3uiv(aLocation: IJSWebGLUniformLocation; aData: IJSUint32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint); overload;
    procedure uniform3uiv(aLocation: IJSWebGLUniformLocation; aData: IJSUint32Array); overload;
    procedure uniform3uiv(aLocation: IJSWebGLUniformLocation; const aData: TGLuintDynArray); overload;
    procedure uniform3uiv(aLocation: IJSWebGLUniformLocation; aData: IJSUint32Array; aSrcOffset: TGLuint); overload;
    procedure uniform3uiv(aLocation: IJSWebGLUniformLocation; const aData: TGLuintDynArray; aSrcOffset: TGLuint); overload;
    procedure uniform4uiv(aLocation: IJSWebGLUniformLocation; const aData: TGLuintDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint); overload;
    procedure uniform4uiv(aLocation: IJSWebGLUniformLocation; aData: IJSUint32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint); overload;
    procedure uniform4uiv(aLocation: IJSWebGLUniformLocation; aData: IJSUint32Array); overload;
    procedure uniform4uiv(aLocation: IJSWebGLUniformLocation; const aData: TGLuintDynArray); overload;
    procedure uniform4uiv(aLocation: IJSWebGLUniformLocation; aData: IJSUint32Array; aSrcOffset: TGLuint); overload;
    procedure uniform4uiv(aLocation: IJSWebGLUniformLocation; const aData: TGLuintDynArray; aSrcOffset: TGLuint); overload;
    procedure uniformMatrix2fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint); overload;
    procedure uniformMatrix2fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint); overload;
    procedure uniformMatrix2fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray); overload;
    procedure uniformMatrix2fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array); overload;
    procedure uniformMatrix2fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray; aSrcOffset: TGLuint); overload;
    procedure uniformMatrix2fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array; aSrcOffset: TGLuint); overload;
    procedure uniformMatrix3x2fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint); overload;
    procedure uniformMatrix3x2fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint); overload;
    procedure uniformMatrix3x2fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray); overload;
    procedure uniformMatrix3x2fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array); overload;
    procedure uniformMatrix3x2fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray; aSrcOffset: TGLuint); overload;
    procedure uniformMatrix3x2fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array; aSrcOffset: TGLuint); overload;
    procedure uniformMatrix4x2fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint); overload;
    procedure uniformMatrix4x2fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint); overload;
    procedure uniformMatrix4x2fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray); overload;
    procedure uniformMatrix4x2fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array); overload;
    procedure uniformMatrix4x2fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray; aSrcOffset: TGLuint); overload;
    procedure uniformMatrix4x2fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array; aSrcOffset: TGLuint); overload;
    procedure uniformMatrix2x3fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint); overload;
    procedure uniformMatrix2x3fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint); overload;
    procedure uniformMatrix2x3fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray); overload;
    procedure uniformMatrix2x3fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array); overload;
    procedure uniformMatrix2x3fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray; aSrcOffset: TGLuint); overload;
    procedure uniformMatrix2x3fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array; aSrcOffset: TGLuint); overload;
    procedure uniformMatrix3fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint); overload;
    procedure uniformMatrix3fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint); overload;
    procedure uniformMatrix3fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray); overload;
    procedure uniformMatrix3fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array); overload;
    procedure uniformMatrix3fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray; aSrcOffset: TGLuint); overload;
    procedure uniformMatrix3fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array; aSrcOffset: TGLuint); overload;
    procedure uniformMatrix4x3fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint); overload;
    procedure uniformMatrix4x3fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint); overload;
    procedure uniformMatrix4x3fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray); overload;
    procedure uniformMatrix4x3fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array); overload;
    procedure uniformMatrix4x3fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray; aSrcOffset: TGLuint); overload;
    procedure uniformMatrix4x3fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array; aSrcOffset: TGLuint); overload;
    procedure uniformMatrix2x4fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint); overload;
    procedure uniformMatrix2x4fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint); overload;
    procedure uniformMatrix2x4fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray); overload;
    procedure uniformMatrix2x4fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array); overload;
    procedure uniformMatrix2x4fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray; aSrcOffset: TGLuint); overload;
    procedure uniformMatrix2x4fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array; aSrcOffset: TGLuint); overload;
    procedure uniformMatrix3x4fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint); overload;
    procedure uniformMatrix3x4fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint); overload;
    procedure uniformMatrix3x4fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray); overload;
    procedure uniformMatrix3x4fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array); overload;
    procedure uniformMatrix3x4fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray; aSrcOffset: TGLuint); overload;
    procedure uniformMatrix3x4fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array; aSrcOffset: TGLuint); overload;
    procedure uniformMatrix4fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint); overload;
    procedure uniformMatrix4fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint); overload;
    procedure uniformMatrix4fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray); overload;
    procedure uniformMatrix4fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array); overload;
    procedure uniformMatrix4fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray; aSrcOffset: TGLuint); overload;
    procedure uniformMatrix4fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array; aSrcOffset: TGLuint); overload;
    procedure vertexAttribI4i(aIndex: TGLuint; aX: TGLint; aY: TGLint; aZ: TGLint; aW: TGLint); overload;
    procedure vertexAttribI4iv(aIndex: TGLuint; aValues: IJSInt32Array); overload;
    procedure vertexAttribI4iv(aIndex: TGLuint; const aValues: TGLintDynArray); overload;
    procedure vertexAttribI4ui(aIndex: TGLuint; aX: TGLuint; aY: TGLuint; aZ: TGLuint; aW: TGLuint); overload;
    procedure vertexAttribI4uiv(aIndex: TGLuint; const aValues: TGLuintDynArray); overload;
    procedure vertexAttribI4uiv(aIndex: TGLuint; aValues: IJSUint32Array); overload;
    procedure vertexAttribIPointer(aIndex: TGLuint; aSize: TGLint; aType_: TGLenum; aStride: TGLsizei; aOffset: TGLintptr); overload;
    procedure vertexAttribDivisor(aIndex: TGLuint; aDivisor: TGLuint); overload;
    procedure drawArraysInstanced(aMode: TGLenum; aFirst: TGLint; aCount: TGLsizei; aInstanceCount: TGLsizei); overload;
    procedure drawElementsInstanced(aMode: TGLenum; aCount: TGLsizei; aType_: TGLenum; aOffset: TGLintptr; aInstanceCount: TGLsizei); overload;
    procedure drawRangeElements(aMode: TGLenum; aStart: TGLuint; aEnd_: TGLuint; aCount: TGLsizei; aType_: TGLenum; aOffset: TGLintptr); overload;
    procedure readPixels(aX: TGLint; aY: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aDstData: IJSArrayBufferView); overload;
    procedure readPixels(aX: TGLint; aY: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aOffset: TGLintptr); overload;
    procedure readPixels(aX: TGLint; aY: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aDstData: IJSArrayBufferView; aDstOffset: TGLuint); overload;
    procedure drawBuffers(const aBuffers: TGLenumDynArray); overload;
    procedure clearBufferfv(aBuffer: TGLenum; aDrawbuffer: TGLint; aValues: IJSFloat32Array; aSrcOffset: TGLuint); overload;
    procedure clearBufferfv(aBuffer: TGLenum; aDrawbuffer: TGLint; const aValues: TGLfloatDynArray; aSrcOffset: TGLuint); overload;
    procedure clearBufferfv(aBuffer: TGLenum; aDrawbuffer: TGLint; const aValues: TGLfloatDynArray); overload;
    procedure clearBufferfv(aBuffer: TGLenum; aDrawbuffer: TGLint; aValues: IJSFloat32Array); overload;
    procedure clearBufferiv(aBuffer: TGLenum; aDrawbuffer: TGLint; aValues: IJSInt32Array; aSrcOffset: TGLuint); overload;
    procedure clearBufferiv(aBuffer: TGLenum; aDrawbuffer: TGLint; const aValues: TGLintDynArray; aSrcOffset: TGLuint); overload;
    procedure clearBufferiv(aBuffer: TGLenum; aDrawbuffer: TGLint; const aValues: TGLintDynArray); overload;
    procedure clearBufferiv(aBuffer: TGLenum; aDrawbuffer: TGLint; aValues: IJSInt32Array); overload;
    procedure clearBufferuiv(aBuffer: TGLenum; aDrawbuffer: TGLint; const aValues: TGLuintDynArray; aSrcOffset: TGLuint); overload;
    procedure clearBufferuiv(aBuffer: TGLenum; aDrawbuffer: TGLint; aValues: IJSUint32Array; aSrcOffset: TGLuint); overload;
    procedure clearBufferuiv(aBuffer: TGLenum; aDrawbuffer: TGLint; aValues: IJSUint32Array); overload;
    procedure clearBufferuiv(aBuffer: TGLenum; aDrawbuffer: TGLint; const aValues: TGLuintDynArray); overload;
    procedure clearBufferfi(aBuffer: TGLenum; aDrawbuffer: TGLint; aDepth: TGLfloat; aStencil: TGLint); overload;
    function createQuery: IJSWebGLQuery; overload;
    procedure deleteQuery(aQuery: IJSWebGLQuery); overload;
    function isQuery(aQuery: IJSWebGLQuery): TGLboolean; overload;
    procedure beginQuery(aTarget: TGLenum; aQuery: IJSWebGLQuery); overload;
    procedure endQuery(aTarget: TGLenum); overload;
    function getQuery(aTarget: TGLenum; aPname: TGLenum): Variant; overload;
    function getQueryParameter(aQuery: IJSWebGLQuery; aPname: TGLenum): Variant; overload;
    function createSampler: IJSWebGLSampler; overload;
    procedure deleteSampler(aSampler: IJSWebGLSampler); overload;
    function isSampler(aSampler: IJSWebGLSampler): TGLboolean; overload;
    procedure bindSampler(aUnit_: TGLuint; aSampler: IJSWebGLSampler); overload;
    procedure samplerParameteri(aSampler: IJSWebGLSampler; aPname: TGLenum; aParam: TGLint); overload;
    procedure samplerParameterf(aSampler: IJSWebGLSampler; aPname: TGLenum; aParam: TGLfloat); overload;
    function getSamplerParameter(aSampler: IJSWebGLSampler; aPname: TGLenum): Variant; overload;
    function fenceSync(aCondition: TGLenum; aFlags: TGLbitfield): IJSWebGLSync; overload;
    function isSync(aSync: IJSWebGLSync): TGLboolean; overload;
    procedure deleteSync(aSync: IJSWebGLSync); overload;
    function clientWaitSync(aSync: IJSWebGLSync; aFlags: TGLbitfield; aTimeout: TGLuint64): TGLenum; overload;
    procedure waitSync(aSync: IJSWebGLSync; aFlags: TGLbitfield; aTimeout: TGLint64); overload;
    function getSyncParameter(aSync: IJSWebGLSync; aPname: TGLenum): Variant; overload;
    function createTransformFeedback: IJSWebGLTransformFeedback; overload;
    procedure deleteTransformFeedback(aTf: IJSWebGLTransformFeedback); overload;
    function isTransformFeedback(aTf: IJSWebGLTransformFeedback): TGLboolean; overload;
    procedure bindTransformFeedback(aTarget: TGLenum; aTf: IJSWebGLTransformFeedback); overload;
    procedure beginTransformFeedback(aPrimitiveMode: TGLenum); overload;
    procedure endTransformFeedback; overload;
    procedure transformFeedbackVaryings(aProgram_: IJSWebGLProgram; const aVaryings: TUnicodeStringDynArray; aBufferMode: TGLenum); overload;
    function getTransformFeedbackVarying(aProgram_: IJSWebGLProgram; aIndex: TGLuint): IJSWebGLActiveInfo; overload;
    procedure pauseTransformFeedback; overload;
    procedure resumeTransformFeedback; overload;
    procedure bindBufferBase(aTarget: TGLenum; aIndex: TGLuint; aBuffer: IJSWebGLBuffer); overload;
    procedure bindBufferRange(aTarget: TGLenum; aIndex: TGLuint; aBuffer: IJSWebGLBuffer; aOffset: TGLintptr; aSize: TGLsizeiptr); overload;
    function getIndexedParameter(aTarget: TGLenum; aIndex: TGLuint): Variant; overload;
    function getUniformIndices(aProgram_: IJSWebGLProgram; const aUniformNames: TUnicodeStringDynArray): TGLuintDynArray; overload;
    function getActiveUniforms(aProgram_: IJSWebGLProgram; const aUniformIndices: TGLuintDynArray; aPname: TGLenum): Variant; overload;
    function getUniformBlockIndex(aProgram_: IJSWebGLProgram; const aUniformBlockName: UnicodeString): TGLuint; overload;
    function getActiveUniformBlockParameter(aProgram_: IJSWebGLProgram; aUniformBlockIndex: TGLuint; aPname: TGLenum): Variant; overload;
    function getActiveUniformBlockName(aProgram_: IJSWebGLProgram; aUniformBlockIndex: TGLuint): UnicodeString; overload;
    procedure uniformBlockBinding(aProgram_: IJSWebGLProgram; aUniformBlockIndex: TGLuint; aUniformBlockBinding: TGLuint); overload;
    function createVertexArray: IJSWebGLVertexArrayObject; overload;
    procedure deleteVertexArray(aVertexArray: IJSWebGLVertexArrayObject); overload;
    function isVertexArray(aVertexArray: IJSWebGLVertexArrayObject): TGLboolean; overload;
    procedure bindVertexArray(aArray_: IJSWebGLVertexArrayObject); overload;
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSWebGL2RenderingContextBase;
  end;
  
  { --------------------------------------------------------------------
    TJSEXT_color_buffer_float
    --------------------------------------------------------------------}
  
  IJSEXT_color_buffer_float = interface(IJSObject)
    ['{D65EC240-EDDC-3778-80B2-D3FB3E66A1A8}']
  end;
  
  TJSEXT_color_buffer_float = class(TJSObject,IJSEXT_color_buffer_float)
  Private
  Protected
  Public
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSEXT_color_buffer_float;
  end;
  
  { --------------------------------------------------------------------
    TJSOVR_multiview2
    --------------------------------------------------------------------}
  
  IJSOVR_multiview2 = interface(IJSObject)
    ['{4B6B11A4-5576-3060-B02E-10347131D7B5}']
    procedure framebufferTextureMultiviewOVR(aTarget: TGLenum; aAttachment: TGLenum; aTexture: IJSWebGLTexture; aLevel: TGLint; aBaseViewIndex: TGLint; aNumViews: TGLsizei);
  end;
  
  TJSOVR_multiview2 = class(TJSObject,IJSOVR_multiview2)
  Private
  Protected
  Public
    Const
      FRAMEBUFFER_ATTACHMENT_TEXTURE_NUM_VIEWS_OVR = $9630;
      FRAMEBUFFER_ATTACHMENT_TEXTURE_BASE_VIEW_INDEX_OVR = $9632;
      MAX_VIEWS_OVR = $9631;
      FRAMEBUFFER_INCOMPLETE_VIEW_TARGETS_OVR = $9633;
  Public
    procedure framebufferTextureMultiviewOVR(aTarget: TGLenum; aAttachment: TGLenum; aTexture: IJSWebGLTexture; aLevel: TGLint; aBaseViewIndex: TGLint; aNumViews: TGLsizei); overload;
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSOVR_multiview2;
  end;
  
  { --------------------------------------------------------------------
    TJSWebGLBuffer
    --------------------------------------------------------------------}
  
  IJSWebGLBuffer = interface(IJSObject)
    ['{EEE0F752-953A-37C2-8C3D-1A2372542131}']
  end;
  
  TJSWebGLBuffer = class(TJSObject,IJSWebGLBuffer)
  Private
  Protected
  Public
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSWebGLBuffer;
  end;
  
  { --------------------------------------------------------------------
    TJSWebGLFramebuffer
    --------------------------------------------------------------------}
  
  IJSWebGLFramebuffer = interface(IJSObject)
    ['{84BCBAD8-8677-39B6-99B0-E1261E0D8EB9}']
  end;
  
  TJSWebGLFramebuffer = class(TJSObject,IJSWebGLFramebuffer)
  Private
  Protected
  Public
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSWebGLFramebuffer;
  end;
  
  { --------------------------------------------------------------------
    TJSWebGLProgram
    --------------------------------------------------------------------}
  
  IJSWebGLProgram = interface(IJSObject)
    ['{E6D054BF-57EC-3E53-91C0-F0D81AA2A621}']
  end;
  
  TJSWebGLProgram = class(TJSObject,IJSWebGLProgram)
  Private
  Protected
  Public
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSWebGLProgram;
  end;
  
  { --------------------------------------------------------------------
    TJSWebGLRenderbuffer
    --------------------------------------------------------------------}
  
  IJSWebGLRenderbuffer = interface(IJSObject)
    ['{2B7E32D5-7E82-3987-B9EA-9E1130F8698D}']
  end;
  
  TJSWebGLRenderbuffer = class(TJSObject,IJSWebGLRenderbuffer)
  Private
  Protected
  Public
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSWebGLRenderbuffer;
  end;
  
  { --------------------------------------------------------------------
    TJSWebGLShader
    --------------------------------------------------------------------}
  
  IJSWebGLShader = interface(IJSObject)
    ['{EEE0F752-95C0-3CA2-8C3D-1A2372542131}']
  end;
  
  TJSWebGLShader = class(TJSObject,IJSWebGLShader)
  Private
  Protected
  Public
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSWebGLShader;
  end;
  
  { --------------------------------------------------------------------
    TJSWebGLTexture
    --------------------------------------------------------------------}
  
  IJSWebGLTexture = interface(IJSObject)
    ['{E6D054BF-5709-3123-94C0-F0D81AA2A621}']
  end;
  
  TJSWebGLTexture = class(TJSObject,IJSWebGLTexture)
  Private
  Protected
  Public
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSWebGLTexture;
  end;
  
  { --------------------------------------------------------------------
    TJSWebGLUniformLocation
    --------------------------------------------------------------------}
  
  IJSWebGLUniformLocation = interface(IJSObject)
    ['{334544B6-4D4F-3D49-8417-E0F31DC9E0F0}']
  end;
  
  TJSWebGLUniformLocation = class(TJSObject,IJSWebGLUniformLocation)
  Private
  Protected
  Public
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSWebGLUniformLocation;
  end;
  
  { --------------------------------------------------------------------
    TJSWebGLVertexArrayObject
    --------------------------------------------------------------------}
  
  IJSWebGLVertexArrayObject = interface(IJSObject)
    ['{D660E8F5-D611-3D9B-923E-32083C5D63A8}']
  end;
  
  TJSWebGLVertexArrayObject = class(TJSObject,IJSWebGLVertexArrayObject)
  Private
  Protected
  Public
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSWebGLVertexArrayObject;
  end;
  
  { --------------------------------------------------------------------
    TJSWebGLActiveInfo
    --------------------------------------------------------------------}
  
  IJSWebGLActiveInfo = interface(IJSObject)
    ['{5C4F499C-7D51-3EF2-AC04-FFF3B1C17B95}']
    function _Getsize: TGLint; 
    function _Gettype_: TGLenum; 
    function _Getname: UnicodeString; 
    property size: TGLint read _Getsize;
    property type_: TGLenum read _Gettype_;
    property name: UnicodeString read _Getname;
  end;
  
  TJSWebGLActiveInfo = class(TJSObject,IJSWebGLActiveInfo)
  Private
  Protected
    function _Getsize: TGLint; 
    function _Gettype_: TGLenum; 
    function _Getname: UnicodeString; 
  Public
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSWebGLActiveInfo;
    property size: TGLint read _Getsize;
    property type_: TGLenum read _Gettype_;
    property name: UnicodeString read _Getname;
  end;
  
  { --------------------------------------------------------------------
    TJSWebGLShaderPrecisionFormat
    --------------------------------------------------------------------}
  
  IJSWebGLShaderPrecisionFormat = interface(IJSObject)
    ['{C7BA1534-7F59-3861-8144-E24C30E5D63F}']
    function _GetrangeMin: TGLint; 
    function _GetrangeMax: TGLint; 
    function _Getprecision: TGLint; 
    property rangeMin: TGLint read _GetrangeMin;
    property rangeMax: TGLint read _GetrangeMax;
    property precision: TGLint read _Getprecision;
  end;
  
  TJSWebGLShaderPrecisionFormat = class(TJSObject,IJSWebGLShaderPrecisionFormat)
  Private
  Protected
    function _GetrangeMin: TGLint; 
    function _GetrangeMax: TGLint; 
    function _Getprecision: TGLint; 
  Public
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSWebGLShaderPrecisionFormat;
    property rangeMin: TGLint read _GetrangeMin;
    property rangeMax: TGLint read _GetrangeMax;
    property precision: TGLint read _Getprecision;
  end;
  
  { --------------------------------------------------------------------
    TJSWebGLRenderingContextBase
    --------------------------------------------------------------------}
  
  IJSWebGLRenderingContextBase = interface(IJSObject)
    ['{656C1EF1-85FE-3AFC-8D2E-760D43F60AFF}']
    function _Getcanvas: TCanvasSource; 
    function _GetdrawingBufferWidth: TGLsizei; 
    function _GetdrawingBufferHeight: TGLsizei; 
    function _GetdrawingBufferColorSpace: TPredefinedColorSpace; 
    function _GetunpackColorSpace: TPredefinedColorSpace; 
    procedure _SetdrawingBufferColorSpace(const aValue: TPredefinedColorSpace);
    procedure _SetunpackColorSpace(const aValue: TPredefinedColorSpace);
    function getContextAttributes: IJSWebGLContextAttributes;
    function isContextLost: Boolean;
    function getSupportedExtensions: TUnicodeStringDynArray;
    function getExtension(const aName: UnicodeString): IJSObject;
    procedure activeTexture(aTexture: TGLenum);
    procedure attachShader(aProgram_: IJSWebGLProgram; aShader: IJSWebGLShader);
    procedure bindAttribLocation(aProgram_: IJSWebGLProgram; aIndex: TGLuint; const aName: UnicodeString);
    procedure bindBuffer(aTarget: TGLenum; aBuffer: IJSWebGLBuffer);
    procedure bindFramebuffer(aTarget: TGLenum; aFramebuffer: IJSWebGLFramebuffer);
    procedure bindRenderbuffer(aTarget: TGLenum; aRenderbuffer: IJSWebGLRenderbuffer);
    procedure bindTexture(aTarget: TGLenum; aTexture: IJSWebGLTexture);
    procedure blendColor(aRed: TGLfloat; aGreen: TGLfloat; aBlue: TGLfloat; aAlpha: TGLfloat);
    procedure blendEquation(aMode: TGLenum);
    procedure blendEquationSeparate(aModeRGB: TGLenum; aModeAlpha: TGLenum);
    procedure blendFunc(aSfactor: TGLenum; aDfactor: TGLenum);
    procedure blendFuncSeparate(aSrcRGB: TGLenum; aDstRGB: TGLenum; aSrcAlpha: TGLenum; aDstAlpha: TGLenum);
    function checkFramebufferStatus(aTarget: TGLenum): TGLenum;
    procedure clear(aMask: TGLbitfield);
    procedure clearColor(aRed: TGLfloat; aGreen: TGLfloat; aBlue: TGLfloat; aAlpha: TGLfloat);
    procedure clearDepth(aDepth: TGLclampf);
    procedure clearStencil(aS_: TGLint);
    procedure colorMask(aRed: TGLboolean; aGreen: TGLboolean; aBlue: TGLboolean; aAlpha: TGLboolean);
    procedure compileShader(aShader: IJSWebGLShader);
    procedure copyTexImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLenum; aX: TGLint; aY: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aBorder: TGLint);
    procedure copyTexSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aX: TGLint; aY: TGLint; aWidth: TGLsizei; aHeight: TGLsizei);
    function createBuffer: IJSWebGLBuffer;
    function createFramebuffer: IJSWebGLFramebuffer;
    function createProgram: IJSWebGLProgram;
    function createRenderbuffer: IJSWebGLRenderbuffer;
    function createShader(aType_: TGLenum): IJSWebGLShader;
    function createTexture: IJSWebGLTexture;
    procedure cullFace(aMode: TGLenum);
    procedure deleteBuffer(aBuffer: IJSWebGLBuffer);
    procedure deleteFramebuffer(aFramebuffer: IJSWebGLFramebuffer);
    procedure deleteProgram(aProgram_: IJSWebGLProgram);
    procedure deleteRenderbuffer(aRenderbuffer: IJSWebGLRenderbuffer);
    procedure deleteShader(aShader: IJSWebGLShader);
    procedure deleteTexture(aTexture: IJSWebGLTexture);
    procedure depthFunc(aFunc: TGLenum);
    procedure depthMask(aFlag: TGLboolean);
    procedure depthRange(aZNear: TGLclampf; aZFar: TGLclampf);
    procedure detachShader(aProgram_: IJSWebGLProgram; aShader: IJSWebGLShader);
    procedure disable(aCap: TGLenum);
    procedure disableVertexAttribArray(aIndex: TGLuint);
    procedure drawArrays(aMode: TGLenum; aFirst: TGLint; aCount: TGLsizei);
    procedure drawElements(aMode: TGLenum; aCount: TGLsizei; aType_: TGLenum; aOffset: TGLintptr);
    procedure enable(aCap: TGLenum);
    procedure enableVertexAttribArray(aIndex: TGLuint);
    procedure finish;
    procedure flush;
    procedure framebufferRenderbuffer(aTarget: TGLenum; aAttachment: TGLenum; aRenderbuffertarget: TGLenum; aRenderbuffer: IJSWebGLRenderbuffer);
    procedure framebufferTexture2D(aTarget: TGLenum; aAttachment: TGLenum; aTextarget: TGLenum; aTexture: IJSWebGLTexture; aLevel: TGLint);
    procedure frontFace(aMode: TGLenum);
    procedure generateMipmap(aTarget: TGLenum);
    function getActiveAttrib(aProgram_: IJSWebGLProgram; aIndex: TGLuint): IJSWebGLActiveInfo;
    function getActiveUniform(aProgram_: IJSWebGLProgram; aIndex: TGLuint): IJSWebGLActiveInfo;
    function getAttachedShaders(aProgram_: IJSWebGLProgram): TJSWebGLShaderDynArray;
    function getAttribLocation(aProgram_: IJSWebGLProgram; const aName: UnicodeString): TGLint;
    function getBufferParameter(aTarget: TGLenum; aPname: TGLenum): Variant;
    function getParameter(aPname: TGLenum): Variant;
    function getError: TGLenum;
    function getFramebufferAttachmentParameter(aTarget: TGLenum; aAttachment: TGLenum; aPname: TGLenum): Variant;
    function getProgramParameter(aProgram_: IJSWebGLProgram; aPname: TGLenum): Variant;
    function getProgramInfoLog(aProgram_: IJSWebGLProgram): UnicodeString;
    function getRenderbufferParameter(aTarget: TGLenum; aPname: TGLenum): Variant;
    function getShaderParameter(aShader: IJSWebGLShader; aPname: TGLenum): Variant;
    function getShaderPrecisionFormat(aShadertype: TGLenum; aPrecisiontype: TGLenum): IJSWebGLShaderPrecisionFormat;
    function getShaderInfoLog(aShader: IJSWebGLShader): UnicodeString;
    function getShaderSource(aShader: IJSWebGLShader): UnicodeString;
    function getTexParameter(aTarget: TGLenum; aPname: TGLenum): Variant;
    function getUniform(aProgram_: IJSWebGLProgram; aLocation: IJSWebGLUniformLocation): Variant;
    function getUniformLocation(aProgram_: IJSWebGLProgram; const aName: UnicodeString): IJSWebGLUniformLocation;
    function getVertexAttrib(aIndex: TGLuint; aPname: TGLenum): Variant;
    function getVertexAttribOffset(aIndex: TGLuint; aPname: TGLenum): TGLintptr;
    procedure hint(aTarget: TGLenum; aMode: TGLenum);
    function isBuffer(aBuffer: IJSWebGLBuffer): TGLboolean;
    function isEnabled(aCap: TGLenum): TGLboolean;
    function isFramebuffer(aFramebuffer: IJSWebGLFramebuffer): TGLboolean;
    function isProgram(aProgram_: IJSWebGLProgram): TGLboolean;
    function isRenderbuffer(aRenderbuffer: IJSWebGLRenderbuffer): TGLboolean;
    function isShader(aShader: IJSWebGLShader): TGLboolean;
    function isTexture(aTexture: IJSWebGLTexture): TGLboolean;
    procedure lineWidth(aWidth: TGLfloat);
    procedure linkProgram(aProgram_: IJSWebGLProgram);
    procedure pixelStorei(aPname: TGLenum; aParam: TGLint);
    procedure polygonOffset(aFactor: TGLfloat; aUnits: TGLfloat);
    procedure renderbufferStorage(aTarget: TGLenum; aInternalformat: TGLenum; aWidth: TGLsizei; aHeight: TGLsizei);
    procedure sampleCoverage(aValue: TGLclampf; aInvert: TGLboolean);
    procedure scissor(aX: TGLint; aY: TGLint; aWidth: TGLsizei; aHeight: TGLsizei);
    procedure shaderSource(aShader: IJSWebGLShader; const aSource: UnicodeString);
    procedure stencilFunc(aFunc: TGLenum; aRef: TGLint; aMask: TGLuint);
    procedure stencilFuncSeparate(aFace: TGLenum; aFunc: TGLenum; aRef: TGLint; aMask: TGLuint);
    procedure stencilMask(aMask: TGLuint);
    procedure stencilMaskSeparate(aFace: TGLenum; aMask: TGLuint);
    procedure stencilOp(aFail: TGLenum; aZfail: TGLenum; aZpass: TGLenum);
    procedure stencilOpSeparate(aFace: TGLenum; aFail: TGLenum; aZfail: TGLenum; aZpass: TGLenum);
    procedure texParameterf(aTarget: TGLenum; aPname: TGLenum; aParam: TGLfloat);
    procedure texParameteri(aTarget: TGLenum; aPname: TGLenum; aParam: TGLint);
    procedure uniform1f(aLocation: IJSWebGLUniformLocation; aX: TGLfloat);
    procedure uniform2f(aLocation: IJSWebGLUniformLocation; aX: TGLfloat; aY: TGLfloat);
    procedure uniform3f(aLocation: IJSWebGLUniformLocation; aX: TGLfloat; aY: TGLfloat; aZ: TGLfloat);
    procedure uniform4f(aLocation: IJSWebGLUniformLocation; aX: TGLfloat; aY: TGLfloat; aZ: TGLfloat; aW: TGLfloat);
    procedure uniform1i(aLocation: IJSWebGLUniformLocation; aX: TGLint);
    procedure uniform2i(aLocation: IJSWebGLUniformLocation; aX: TGLint; aY: TGLint);
    procedure uniform3i(aLocation: IJSWebGLUniformLocation; aX: TGLint; aY: TGLint; aZ: TGLint);
    procedure uniform4i(aLocation: IJSWebGLUniformLocation; aX: TGLint; aY: TGLint; aZ: TGLint; aW: TGLint);
    procedure useProgram(aProgram_: IJSWebGLProgram);
    procedure validateProgram(aProgram_: IJSWebGLProgram);
    procedure vertexAttrib1f(aIndx: TGLuint; aX: TGLfloat);
    procedure vertexAttrib1fv(aIndx: TGLuint; aValues: IJSFloat32Array);
    procedure vertexAttrib1fv(aIndx: TGLuint; const aValues: TGLfloatDynArray);
    procedure vertexAttrib2f(aIndx: TGLuint; aX: TGLfloat; aY: TGLfloat);
    procedure vertexAttrib2fv(aIndx: TGLuint; aValues: IJSFloat32Array);
    procedure vertexAttrib2fv(aIndx: TGLuint; const aValues: TGLfloatDynArray);
    procedure vertexAttrib3f(aIndx: TGLuint; aX: TGLfloat; aY: TGLfloat; aZ: TGLfloat);
    procedure vertexAttrib3fv(aIndx: TGLuint; aValues: IJSFloat32Array);
    procedure vertexAttrib3fv(aIndx: TGLuint; const aValues: TGLfloatDynArray);
    procedure vertexAttrib4f(aIndx: TGLuint; aX: TGLfloat; aY: TGLfloat; aZ: TGLfloat; aW: TGLfloat);
    procedure vertexAttrib4fv(aIndx: TGLuint; aValues: IJSFloat32Array);
    procedure vertexAttrib4fv(aIndx: TGLuint; const aValues: TGLfloatDynArray);
    procedure vertexAttribPointer(aIndx: TGLuint; aSize: TGLint; aType_: TGLenum; aNormalized: TGLboolean; aStride: TGLsizei; aOffset: TGLintptr);
    procedure viewport(aX: TGLint; aY: TGLint; aWidth: TGLsizei; aHeight: TGLsizei);
    function makeXRCompatible: IJSPromise; // Promise<undefined>
    property canvas: TCanvasSource read _Getcanvas;
    property drawingBufferWidth: TGLsizei read _GetdrawingBufferWidth;
    property drawingBufferHeight: TGLsizei read _GetdrawingBufferHeight;
    property drawingBufferColorSpace: TPredefinedColorSpace read _GetdrawingBufferColorSpace write _SetdrawingBufferColorSpace;
    property unpackColorSpace: TPredefinedColorSpace read _GetunpackColorSpace write _SetunpackColorSpace;
  end;
  
  TJSWebGLRenderingContextBase = class(TJSObject,IJSWebGLRenderingContextBase)
  Private
  Protected
    function _Getcanvas: TCanvasSource; 
    function _GetdrawingBufferWidth: TGLsizei; 
    function _GetdrawingBufferHeight: TGLsizei; 
    function _GetdrawingBufferColorSpace: TPredefinedColorSpace; 
    function _GetunpackColorSpace: TPredefinedColorSpace; 
    procedure _SetdrawingBufferColorSpace(const aValue: TPredefinedColorSpace);
    procedure _SetunpackColorSpace(const aValue: TPredefinedColorSpace);
  Public
    Const
      DEPTH_BUFFER_BIT = $00000100;
      STENCIL_BUFFER_BIT = $00000400;
      COLOR_BUFFER_BIT = $00004000;
      POINTS = $0000;
      LINES = $0001;
      LINE_LOOP = $0002;
      LINE_STRIP = $0003;
      TRIANGLES = $0004;
      TRIANGLE_STRIP = $0005;
      TRIANGLE_FAN = $0006;
      ZERO = 0;
      ONE = 1;
      SRC_COLOR = $0300;
      ONE_MINUS_SRC_COLOR = $0301;
      SRC_ALPHA = $0302;
      ONE_MINUS_SRC_ALPHA = $0303;
      DST_ALPHA = $0304;
      ONE_MINUS_DST_ALPHA = $0305;
      DST_COLOR = $0306;
      ONE_MINUS_DST_COLOR = $0307;
      SRC_ALPHA_SATURATE = $0308;
      FUNC_ADD = $8006;
      BLEND_EQUATION = $8009;
      BLEND_EQUATION_RGB = $8009;
      BLEND_EQUATION_ALPHA = $883D;
      FUNC_SUBTRACT = $800A;
      FUNC_REVERSE_SUBTRACT = $800B;
      BLEND_DST_RGB = $80C8;
      BLEND_SRC_RGB = $80C9;
      BLEND_DST_ALPHA = $80CA;
      BLEND_SRC_ALPHA = $80CB;
      CONSTANT_COLOR = $8001;
      ONE_MINUS_CONSTANT_COLOR = $8002;
      CONSTANT_ALPHA = $8003;
      ONE_MINUS_CONSTANT_ALPHA = $8004;
      BLEND_COLOR = $8005;
      ARRAY_BUFFER = $8892;
      ELEMENT_ARRAY_BUFFER = $8893;
      ARRAY_BUFFER_BINDING = $8894;
      ELEMENT_ARRAY_BUFFER_BINDING = $8895;
      STREAM_DRAW = $88E0;
      STATIC_DRAW = $88E4;
      DYNAMIC_DRAW = $88E8;
      BUFFER_SIZE = $8764;
      BUFFER_USAGE = $8765;
      CURRENT_VERTEX_ATTRIB = $8626;
      FRONT = $0404;
      BACK = $0405;
      FRONT_AND_BACK = $0408;
      CULL_FACE = $0B44;
      BLEND = $0BE2;
      DITHER = $0BD0;
      STENCIL_TEST = $0B90;
      DEPTH_TEST = $0B71;
      SCISSOR_TEST = $0C11;
      POLYGON_OFFSET_FILL = $8037;
      SAMPLE_ALPHA_TO_COVERAGE = $809E;
      SAMPLE_COVERAGE = $80A0;
      NO_ERROR = 0;
      INVALID_ENUM = $0500;
      INVALID_VALUE = $0501;
      INVALID_OPERATION = $0502;
      OUT_OF_MEMORY = $0505;
      CW = $0900;
      CCW = $0901;
      LINE_WIDTH = $0B21;
      ALIASED_POINT_SIZE_RANGE = $846D;
      ALIASED_LINE_WIDTH_RANGE = $846E;
      CULL_FACE_MODE = $0B45;
      FRONT_FACE = $0B46;
      DEPTH_RANGE = $0B70;
      DEPTH_WRITEMASK = $0B72;
      DEPTH_CLEAR_VALUE = $0B73;
      DEPTH_FUNC = $0B74;
      STENCIL_CLEAR_VALUE = $0B91;
      STENCIL_FUNC = $0B92;
      STENCIL_FAIL = $0B94;
      STENCIL_PASS_DEPTH_FAIL = $0B95;
      STENCIL_PASS_DEPTH_PASS = $0B96;
      STENCIL_REF = $0B97;
      STENCIL_VALUE_MASK = $0B93;
      STENCIL_WRITEMASK = $0B98;
      STENCIL_BACK_FUNC = $8800;
      STENCIL_BACK_FAIL = $8801;
      STENCIL_BACK_PASS_DEPTH_FAIL = $8802;
      STENCIL_BACK_PASS_DEPTH_PASS = $8803;
      STENCIL_BACK_REF = $8CA3;
      STENCIL_BACK_VALUE_MASK = $8CA4;
      STENCIL_BACK_WRITEMASK = $8CA5;
      VIEWPORT_ = $0BA2;
      SCISSOR_BOX = $0C10;
      COLOR_CLEAR_VALUE = $0C22;
      COLOR_WRITEMASK = $0C23;
      UNPACK_ALIGNMENT = $0CF5;
      PACK_ALIGNMENT = $0D05;
      MAX_TEXTURE_SIZE = $0D33;
      MAX_VIEWPORT_DIMS = $0D3A;
      SUBPIXEL_BITS = $0D50;
      RED_BITS = $0D52;
      GREEN_BITS = $0D53;
      BLUE_BITS = $0D54;
      ALPHA_BITS = $0D55;
      DEPTH_BITS = $0D56;
      STENCIL_BITS = $0D57;
      POLYGON_OFFSET_UNITS = $2A00;
      POLYGON_OFFSET_FACTOR = $8038;
      TEXTURE_BINDING_2D = $8069;
      SAMPLE_BUFFERS = $80A8;
      SAMPLES = $80A9;
      SAMPLE_COVERAGE_VALUE = $80AA;
      SAMPLE_COVERAGE_INVERT = $80AB;
      COMPRESSED_TEXTURE_FORMATS = $86A3;
      DONT_CARE = $1100;
      FASTEST = $1101;
      NICEST = $1102;
      GENERATE_MIPMAP_HINT = $8192;
      BYTE = $1400;
      UNSIGNED_BYTE = $1401;
      SHORT = $1402;
      UNSIGNED_SHORT = $1403;
      INT = $1404;
      UNSIGNED_INT = $1405;
      FLOAT = $1406;
      DEPTH_COMPONENT = $1902;
      ALPHA = $1906;
      RGB = $1907;
      RGBA = $1908;
      LUMINANCE = $1909;
      LUMINANCE_ALPHA = $190A;
      UNSIGNED_SHORT_4_4_4_4 = $8033;
      UNSIGNED_SHORT_5_5_5_1 = $8034;
      UNSIGNED_SHORT_5_6_5 = $8363;
      FRAGMENT_SHADER = $8B30;
      VERTEX_SHADER = $8B31;
      MAX_VERTEX_ATTRIBS = $8869;
      MAX_VERTEX_UNIFORM_VECTORS = $8DFB;
      MAX_VARYING_VECTORS = $8DFC;
      MAX_COMBINED_TEXTURE_IMAGE_UNITS = $8B4D;
      MAX_VERTEX_TEXTURE_IMAGE_UNITS = $8B4C;
      MAX_TEXTURE_IMAGE_UNITS = $8872;
      MAX_FRAGMENT_UNIFORM_VECTORS = $8DFD;
      SHADER_TYPE = $8B4F;
      DELETE_STATUS = $8B80;
      LINK_STATUS = $8B82;
      VALIDATE_STATUS = $8B83;
      ATTACHED_SHADERS = $8B85;
      ACTIVE_UNIFORMS = $8B86;
      ACTIVE_ATTRIBUTES = $8B89;
      SHADING_LANGUAGE_VERSION = $8B8C;
      CURRENT_PROGRAM = $8B8D;
      NEVER = $0200;
      LESS = $0201;
      EQUAL = $0202;
      LEQUAL = $0203;
      GREATER = $0204;
      NOTEQUAL = $0205;
      GEQUAL = $0206;
      ALWAYS = $0207;
      KEEP = $1E00;
      REPLACE = $1E01;
      INCR = $1E02;
      DECR = $1E03;
      INVERT = $150A;
      INCR_WRAP = $8507;
      DECR_WRAP = $8508;
      VENDOR = $1F00;
      RENDERER = $1F01;
      VERSION = $1F02;
      NEAREST = $2600;
      LINEAR = $2601;
      NEAREST_MIPMAP_NEAREST = $2700;
      LINEAR_MIPMAP_NEAREST = $2701;
      NEAREST_MIPMAP_LINEAR = $2702;
      LINEAR_MIPMAP_LINEAR = $2703;
      TEXTURE_MAG_FILTER = $2800;
      TEXTURE_MIN_FILTER = $2801;
      TEXTURE_WRAP_S = $2802;
      TEXTURE_WRAP_T = $2803;
      TEXTURE_2D = $0DE1;
      TEXTURE = $1702;
      TEXTURE_CUBE_MAP = $8513;
      TEXTURE_BINDING_CUBE_MAP = $8514;
      TEXTURE_CUBE_MAP_POSITIVE_X = $8515;
      TEXTURE_CUBE_MAP_NEGATIVE_X = $8516;
      TEXTURE_CUBE_MAP_POSITIVE_Y = $8517;
      TEXTURE_CUBE_MAP_NEGATIVE_Y = $8518;
      TEXTURE_CUBE_MAP_POSITIVE_Z = $8519;
      TEXTURE_CUBE_MAP_NEGATIVE_Z = $851A;
      MAX_CUBE_MAP_TEXTURE_SIZE = $851C;
      TEXTURE0 = $84C0;
      TEXTURE1 = $84C1;
      TEXTURE2 = $84C2;
      TEXTURE3 = $84C3;
      TEXTURE4 = $84C4;
      TEXTURE5 = $84C5;
      TEXTURE6 = $84C6;
      TEXTURE7 = $84C7;
      TEXTURE8 = $84C8;
      TEXTURE9 = $84C9;
      TEXTURE10 = $84CA;
      TEXTURE11 = $84CB;
      TEXTURE12 = $84CC;
      TEXTURE13 = $84CD;
      TEXTURE14 = $84CE;
      TEXTURE15 = $84CF;
      TEXTURE16 = $84D0;
      TEXTURE17 = $84D1;
      TEXTURE18 = $84D2;
      TEXTURE19 = $84D3;
      TEXTURE20 = $84D4;
      TEXTURE21 = $84D5;
      TEXTURE22 = $84D6;
      TEXTURE23 = $84D7;
      TEXTURE24 = $84D8;
      TEXTURE25 = $84D9;
      TEXTURE26 = $84DA;
      TEXTURE27 = $84DB;
      TEXTURE28 = $84DC;
      TEXTURE29 = $84DD;
      TEXTURE30 = $84DE;
      TEXTURE31 = $84DF;
      ACTIVE_TEXTURE = $84E0;
      REPEAT_ = $2901;
      CLAMP_TO_EDGE = $812F;
      MIRRORED_REPEAT = $8370;
      FLOAT_VEC2 = $8B50;
      FLOAT_VEC3 = $8B51;
      FLOAT_VEC4 = $8B52;
      INT_VEC2 = $8B53;
      INT_VEC3 = $8B54;
      INT_VEC4 = $8B55;
      BOOL = $8B56;
      BOOL_VEC2 = $8B57;
      BOOL_VEC3 = $8B58;
      BOOL_VEC4 = $8B59;
      FLOAT_MAT2 = $8B5A;
      FLOAT_MAT3 = $8B5B;
      FLOAT_MAT4 = $8B5C;
      SAMPLER_2D = $8B5E;
      SAMPLER_CUBE = $8B60;
      VERTEX_ATTRIB_ARRAY_ENABLED = $8622;
      VERTEX_ATTRIB_ARRAY_SIZE = $8623;
      VERTEX_ATTRIB_ARRAY_STRIDE = $8624;
      VERTEX_ATTRIB_ARRAY_TYPE = $8625;
      VERTEX_ATTRIB_ARRAY_NORMALIZED = $886A;
      VERTEX_ATTRIB_ARRAY_POINTER = $8645;
      VERTEX_ATTRIB_ARRAY_BUFFER_BINDING = $889F;
      IMPLEMENTATION_COLOR_READ_TYPE = $8B9A;
      IMPLEMENTATION_COLOR_READ_FORMAT = $8B9B;
      COMPILE_STATUS = $8B81;
      LOW_FLOAT = $8DF0;
      MEDIUM_FLOAT = $8DF1;
      HIGH_FLOAT = $8DF2;
      LOW_INT = $8DF3;
      MEDIUM_INT = $8DF4;
      HIGH_INT = $8DF5;
      FRAMEBUFFER = $8D40;
      RENDERBUFFER = $8D41;
      RGBA4 = $8056;
      RGB5_A1 = $8057;
      RGB565 = $8D62;
      DEPTH_COMPONENT16 = $81A5;
      STENCIL_INDEX8 = $8D48;
      DEPTH_STENCIL = $84F9;
      RENDERBUFFER_WIDTH = $8D42;
      RENDERBUFFER_HEIGHT = $8D43;
      RENDERBUFFER_INTERNAL_FORMAT = $8D44;
      RENDERBUFFER_RED_SIZE = $8D50;
      RENDERBUFFER_GREEN_SIZE = $8D51;
      RENDERBUFFER_BLUE_SIZE = $8D52;
      RENDERBUFFER_ALPHA_SIZE = $8D53;
      RENDERBUFFER_DEPTH_SIZE = $8D54;
      RENDERBUFFER_STENCIL_SIZE = $8D55;
      FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE = $8CD0;
      FRAMEBUFFER_ATTACHMENT_OBJECT_NAME = $8CD1;
      FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL = $8CD2;
      FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE = $8CD3;
      COLOR_ATTACHMENT0 = $8CE0;
      DEPTH_ATTACHMENT = $8D00;
      STENCIL_ATTACHMENT = $8D20;
      DEPTH_STENCIL_ATTACHMENT = $821A;
      NONE = 0;
      FRAMEBUFFER_COMPLETE = $8CD5;
      FRAMEBUFFER_INCOMPLETE_ATTACHMENT = $8CD6;
      FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT = $8CD7;
      FRAMEBUFFER_INCOMPLETE_DIMENSIONS = $8CD9;
      FRAMEBUFFER_UNSUPPORTED = $8CDD;
      FRAMEBUFFER_BINDING = $8CA6;
      RENDERBUFFER_BINDING = $8CA7;
      MAX_RENDERBUFFER_SIZE = $84E8;
      INVALID_FRAMEBUFFER_OPERATION = $0506;
      UNPACK_FLIP_Y_WEBGL = $9240;
      UNPACK_PREMULTIPLY_ALPHA_WEBGL = $9241;
      CONTEXT_LOST_WEBGL = $9242;
      UNPACK_COLORSPACE_CONVERSION_WEBGL = $9243;
      BROWSER_DEFAULT_WEBGL = $9244;
  Public
    function getContextAttributes: IJSWebGLContextAttributes; overload;
    function isContextLost: Boolean; overload;
    function getSupportedExtensions: TUnicodeStringDynArray; overload;
    function getExtension(const aName: UnicodeString): IJSObject; overload;
    procedure activeTexture(aTexture: TGLenum); overload;
    procedure attachShader(aProgram_: IJSWebGLProgram; aShader: IJSWebGLShader); overload;
    procedure bindAttribLocation(aProgram_: IJSWebGLProgram; aIndex: TGLuint; const aName: UnicodeString); overload;
    procedure bindBuffer(aTarget: TGLenum; aBuffer: IJSWebGLBuffer); overload;
    procedure bindFramebuffer(aTarget: TGLenum; aFramebuffer: IJSWebGLFramebuffer); overload;
    procedure bindRenderbuffer(aTarget: TGLenum; aRenderbuffer: IJSWebGLRenderbuffer); overload;
    procedure bindTexture(aTarget: TGLenum; aTexture: IJSWebGLTexture); overload;
    procedure blendColor(aRed: TGLfloat; aGreen: TGLfloat; aBlue: TGLfloat; aAlpha: TGLfloat); overload;
    procedure blendEquation(aMode: TGLenum); overload;
    procedure blendEquationSeparate(aModeRGB: TGLenum; aModeAlpha: TGLenum); overload;
    procedure blendFunc(aSfactor: TGLenum; aDfactor: TGLenum); overload;
    procedure blendFuncSeparate(aSrcRGB: TGLenum; aDstRGB: TGLenum; aSrcAlpha: TGLenum; aDstAlpha: TGLenum); overload;
    function checkFramebufferStatus(aTarget: TGLenum): TGLenum; overload;
    procedure clear(aMask: TGLbitfield); overload;
    procedure clearColor(aRed: TGLfloat; aGreen: TGLfloat; aBlue: TGLfloat; aAlpha: TGLfloat); overload;
    procedure clearDepth(aDepth: TGLclampf); overload;
    procedure clearStencil(aS_: TGLint); overload;
    procedure colorMask(aRed: TGLboolean; aGreen: TGLboolean; aBlue: TGLboolean; aAlpha: TGLboolean); overload;
    procedure compileShader(aShader: IJSWebGLShader); overload;
    procedure copyTexImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLenum; aX: TGLint; aY: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aBorder: TGLint); overload;
    procedure copyTexSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aX: TGLint; aY: TGLint; aWidth: TGLsizei; aHeight: TGLsizei); overload;
    function createBuffer: IJSWebGLBuffer; overload;
    function createFramebuffer: IJSWebGLFramebuffer; overload;
    function createProgram: IJSWebGLProgram; overload;
    function createRenderbuffer: IJSWebGLRenderbuffer; overload;
    function createShader(aType_: TGLenum): IJSWebGLShader; overload;
    function createTexture: IJSWebGLTexture; overload;
    procedure cullFace(aMode: TGLenum); overload;
    procedure deleteBuffer(aBuffer: IJSWebGLBuffer); overload;
    procedure deleteFramebuffer(aFramebuffer: IJSWebGLFramebuffer); overload;
    procedure deleteProgram(aProgram_: IJSWebGLProgram); overload;
    procedure deleteRenderbuffer(aRenderbuffer: IJSWebGLRenderbuffer); overload;
    procedure deleteShader(aShader: IJSWebGLShader); overload;
    procedure deleteTexture(aTexture: IJSWebGLTexture); overload;
    procedure depthFunc(aFunc: TGLenum); overload;
    procedure depthMask(aFlag: TGLboolean); overload;
    procedure depthRange(aZNear: TGLclampf; aZFar: TGLclampf); overload;
    procedure detachShader(aProgram_: IJSWebGLProgram; aShader: IJSWebGLShader); overload;
    procedure disable(aCap: TGLenum); overload;
    procedure disableVertexAttribArray(aIndex: TGLuint); overload;
    procedure drawArrays(aMode: TGLenum; aFirst: TGLint; aCount: TGLsizei); overload;
    procedure drawElements(aMode: TGLenum; aCount: TGLsizei; aType_: TGLenum; aOffset: TGLintptr); overload;
    procedure enable(aCap: TGLenum); overload;
    procedure enableVertexAttribArray(aIndex: TGLuint); overload;
    procedure finish; overload;
    procedure flush; overload;
    procedure framebufferRenderbuffer(aTarget: TGLenum; aAttachment: TGLenum; aRenderbuffertarget: TGLenum; aRenderbuffer: IJSWebGLRenderbuffer); overload;
    procedure framebufferTexture2D(aTarget: TGLenum; aAttachment: TGLenum; aTextarget: TGLenum; aTexture: IJSWebGLTexture; aLevel: TGLint); overload;
    procedure frontFace(aMode: TGLenum); overload;
    procedure generateMipmap(aTarget: TGLenum); overload;
    function getActiveAttrib(aProgram_: IJSWebGLProgram; aIndex: TGLuint): IJSWebGLActiveInfo; overload;
    function getActiveUniform(aProgram_: IJSWebGLProgram; aIndex: TGLuint): IJSWebGLActiveInfo; overload;
    function getAttachedShaders(aProgram_: IJSWebGLProgram): TJSWebGLShaderDynArray; overload;
    function getAttribLocation(aProgram_: IJSWebGLProgram; const aName: UnicodeString): TGLint; overload;
    function getBufferParameter(aTarget: TGLenum; aPname: TGLenum): Variant; overload;
    function getParameter(aPname: TGLenum): Variant; overload;
    function getError: TGLenum; overload;
    function getFramebufferAttachmentParameter(aTarget: TGLenum; aAttachment: TGLenum; aPname: TGLenum): Variant; overload;
    function getProgramParameter(aProgram_: IJSWebGLProgram; aPname: TGLenum): Variant; overload;
    function getProgramInfoLog(aProgram_: IJSWebGLProgram): UnicodeString; overload;
    function getRenderbufferParameter(aTarget: TGLenum; aPname: TGLenum): Variant; overload;
    function getShaderParameter(aShader: IJSWebGLShader; aPname: TGLenum): Variant; overload;
    function getShaderPrecisionFormat(aShadertype: TGLenum; aPrecisiontype: TGLenum): IJSWebGLShaderPrecisionFormat; overload;
    function getShaderInfoLog(aShader: IJSWebGLShader): UnicodeString; overload;
    function getShaderSource(aShader: IJSWebGLShader): UnicodeString; overload;
    function getTexParameter(aTarget: TGLenum; aPname: TGLenum): Variant; overload;
    function getUniform(aProgram_: IJSWebGLProgram; aLocation: IJSWebGLUniformLocation): Variant; overload;
    function getUniformLocation(aProgram_: IJSWebGLProgram; const aName: UnicodeString): IJSWebGLUniformLocation; overload;
    function getVertexAttrib(aIndex: TGLuint; aPname: TGLenum): Variant; overload;
    function getVertexAttribOffset(aIndex: TGLuint; aPname: TGLenum): TGLintptr; overload;
    procedure hint(aTarget: TGLenum; aMode: TGLenum); overload;
    function isBuffer(aBuffer: IJSWebGLBuffer): TGLboolean; overload;
    function isEnabled(aCap: TGLenum): TGLboolean; overload;
    function isFramebuffer(aFramebuffer: IJSWebGLFramebuffer): TGLboolean; overload;
    function isProgram(aProgram_: IJSWebGLProgram): TGLboolean; overload;
    function isRenderbuffer(aRenderbuffer: IJSWebGLRenderbuffer): TGLboolean; overload;
    function isShader(aShader: IJSWebGLShader): TGLboolean; overload;
    function isTexture(aTexture: IJSWebGLTexture): TGLboolean; overload;
    procedure lineWidth(aWidth: TGLfloat); overload;
    procedure linkProgram(aProgram_: IJSWebGLProgram); overload;
    procedure pixelStorei(aPname: TGLenum; aParam: TGLint); overload;
    procedure polygonOffset(aFactor: TGLfloat; aUnits: TGLfloat); overload;
    procedure renderbufferStorage(aTarget: TGLenum; aInternalformat: TGLenum; aWidth: TGLsizei; aHeight: TGLsizei); overload;
    procedure sampleCoverage(aValue: TGLclampf; aInvert: TGLboolean); overload;
    procedure scissor(aX: TGLint; aY: TGLint; aWidth: TGLsizei; aHeight: TGLsizei); overload;
    procedure shaderSource(aShader: IJSWebGLShader; const aSource: UnicodeString); overload;
    procedure stencilFunc(aFunc: TGLenum; aRef: TGLint; aMask: TGLuint); overload;
    procedure stencilFuncSeparate(aFace: TGLenum; aFunc: TGLenum; aRef: TGLint; aMask: TGLuint); overload;
    procedure stencilMask(aMask: TGLuint); overload;
    procedure stencilMaskSeparate(aFace: TGLenum; aMask: TGLuint); overload;
    procedure stencilOp(aFail: TGLenum; aZfail: TGLenum; aZpass: TGLenum); overload;
    procedure stencilOpSeparate(aFace: TGLenum; aFail: TGLenum; aZfail: TGLenum; aZpass: TGLenum); overload;
    procedure texParameterf(aTarget: TGLenum; aPname: TGLenum; aParam: TGLfloat); overload;
    procedure texParameteri(aTarget: TGLenum; aPname: TGLenum; aParam: TGLint); overload;
    procedure uniform1f(aLocation: IJSWebGLUniformLocation; aX: TGLfloat); overload;
    procedure uniform2f(aLocation: IJSWebGLUniformLocation; aX: TGLfloat; aY: TGLfloat); overload;
    procedure uniform3f(aLocation: IJSWebGLUniformLocation; aX: TGLfloat; aY: TGLfloat; aZ: TGLfloat); overload;
    procedure uniform4f(aLocation: IJSWebGLUniformLocation; aX: TGLfloat; aY: TGLfloat; aZ: TGLfloat; aW: TGLfloat); overload;
    procedure uniform1i(aLocation: IJSWebGLUniformLocation; aX: TGLint); overload;
    procedure uniform2i(aLocation: IJSWebGLUniformLocation; aX: TGLint; aY: TGLint); overload;
    procedure uniform3i(aLocation: IJSWebGLUniformLocation; aX: TGLint; aY: TGLint; aZ: TGLint); overload;
    procedure uniform4i(aLocation: IJSWebGLUniformLocation; aX: TGLint; aY: TGLint; aZ: TGLint; aW: TGLint); overload;
    procedure useProgram(aProgram_: IJSWebGLProgram); overload;
    procedure validateProgram(aProgram_: IJSWebGLProgram); overload;
    procedure vertexAttrib1f(aIndx: TGLuint; aX: TGLfloat); overload;
    procedure vertexAttrib1fv(aIndx: TGLuint; aValues: IJSFloat32Array); overload;
    procedure vertexAttrib1fv(aIndx: TGLuint; const aValues: TGLfloatDynArray); overload;
    procedure vertexAttrib2f(aIndx: TGLuint; aX: TGLfloat; aY: TGLfloat); overload;
    procedure vertexAttrib2fv(aIndx: TGLuint; aValues: IJSFloat32Array); overload;
    procedure vertexAttrib2fv(aIndx: TGLuint; const aValues: TGLfloatDynArray); overload;
    procedure vertexAttrib3f(aIndx: TGLuint; aX: TGLfloat; aY: TGLfloat; aZ: TGLfloat); overload;
    procedure vertexAttrib3fv(aIndx: TGLuint; aValues: IJSFloat32Array); overload;
    procedure vertexAttrib3fv(aIndx: TGLuint; const aValues: TGLfloatDynArray); overload;
    procedure vertexAttrib4f(aIndx: TGLuint; aX: TGLfloat; aY: TGLfloat; aZ: TGLfloat; aW: TGLfloat); overload;
    procedure vertexAttrib4fv(aIndx: TGLuint; aValues: IJSFloat32Array); overload;
    procedure vertexAttrib4fv(aIndx: TGLuint; const aValues: TGLfloatDynArray); overload;
    procedure vertexAttribPointer(aIndx: TGLuint; aSize: TGLint; aType_: TGLenum; aNormalized: TGLboolean; aStride: TGLsizei; aOffset: TGLintptr); overload;
    procedure viewport(aX: TGLint; aY: TGLint; aWidth: TGLsizei; aHeight: TGLsizei); overload;
    function makeXRCompatible: IJSPromise; overload; // Promise<undefined>
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSWebGLRenderingContextBase;
    property canvas: TCanvasSource read _Getcanvas;
    property drawingBufferWidth: TGLsizei read _GetdrawingBufferWidth;
    property drawingBufferHeight: TGLsizei read _GetdrawingBufferHeight;
    property drawingBufferColorSpace: TPredefinedColorSpace read _GetdrawingBufferColorSpace write _SetdrawingBufferColorSpace;
    property unpackColorSpace: TPredefinedColorSpace read _GetunpackColorSpace write _SetunpackColorSpace;
  end;
  
  { --------------------------------------------------------------------
    TJSEXT_texture_compression_bptc
    --------------------------------------------------------------------}
  
  IJSEXT_texture_compression_bptc = interface(IJSObject)
    ['{E907EACE-5EAE-32C7-82B2-4427249175C8}']
  end;
  
  TJSEXT_texture_compression_bptc = class(TJSObject,IJSEXT_texture_compression_bptc)
  Private
  Protected
  Public
    Const
      COMPRESSED_RGBA_BPTC_UNORM_EXT = $8E8C;
      COMPRESSED_SRGB_ALPHA_BPTC_UNORM_EXT = $8E8D;
      COMPRESSED_RGB_BPTC_SIGNED_FLOAT_EXT = $8E8E;
      COMPRESSED_RGB_BPTC_UNSIGNED_FLOAT_EXT = $8E8F;
  Public
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSEXT_texture_compression_bptc;
  end;
  
  { --------------------------------------------------------------------
    TJSEXT_texture_compression_rgtc
    --------------------------------------------------------------------}
  
  IJSEXT_texture_compression_rgtc = interface(IJSObject)
    ['{EAEE17C3-49D9-3FFE-92EB-61ACF6E42A49}']
  end;
  
  TJSEXT_texture_compression_rgtc = class(TJSObject,IJSEXT_texture_compression_rgtc)
  Private
  Protected
  Public
    Const
      COMPRESSED_RED_RGTC1_EXT = $8DBB;
      COMPRESSED_SIGNED_RED_RGTC1_EXT = $8DBC;
      COMPRESSED_RED_GREEN_RGTC2_EXT = $8DBD;
      COMPRESSED_SIGNED_RED_GREEN_RGTC2_EXT = $8DBE;
  Public
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSEXT_texture_compression_rgtc;
  end;
  
  { --------------------------------------------------------------------
    TJSEXT_texture_norm16
    --------------------------------------------------------------------}
  
  IJSEXT_texture_norm16 = interface(IJSObject)
    ['{BFAC4B76-DA94-3C24-A0D4-72B7A7DFC618}']
  end;
  
  TJSEXT_texture_norm16 = class(TJSObject,IJSEXT_texture_norm16)
  Private
  Protected
  Public
    Const
      R16_EXT = $822A;
      RG16_EXT = $822C;
      RGB16_EXT = $8054;
      RGBA16_EXT = $805B;
      R16_SNORM_EXT = $8F98;
      RG16_SNORM_EXT = $8F99;
      RGB16_SNORM_EXT = $8F9A;
      RGBA16_SNORM_EXT = $8F9B;
  Public
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSEXT_texture_norm16;
  end;
  
  { --------------------------------------------------------------------
    TJSWEBGL_compressed_texture_s3tc
    --------------------------------------------------------------------}
  
  IJSWEBGL_compressed_texture_s3tc = interface(IJSObject)
    ['{F0274618-8139-396E-8236-B6B7EF89D263}']
  end;
  
  TJSWEBGL_compressed_texture_s3tc = class(TJSObject,IJSWEBGL_compressed_texture_s3tc)
  Private
  Protected
  Public
    Const
      COMPRESSED_RGB_S3TC_DXT1_EXT = $83F0;
      COMPRESSED_RGBA_S3TC_DXT1_EXT = $83F1;
      COMPRESSED_RGBA_S3TC_DXT3_EXT = $83F2;
      COMPRESSED_RGBA_S3TC_DXT5_EXT = $83F3;
  Public
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSWEBGL_compressed_texture_s3tc;
  end;
  
  { --------------------------------------------------------------------
    TJSWEBGL_compressed_texture_s3tc_srgb
    --------------------------------------------------------------------}
  
  IJSWEBGL_compressed_texture_s3tc_srgb = interface(IJSObject)
    ['{2374FE4F-FBCA-3132-8F97-C771A425D9EB}']
  end;
  
  TJSWEBGL_compressed_texture_s3tc_srgb = class(TJSObject,IJSWEBGL_compressed_texture_s3tc_srgb)
  Private
  Protected
  Public
    Const
      COMPRESSED_SRGB_S3TC_DXT1_EXT = $8C4C;
      COMPRESSED_SRGB_ALPHA_S3TC_DXT1_EXT = $8C4D;
      COMPRESSED_SRGB_ALPHA_S3TC_DXT3_EXT = $8C4E;
      COMPRESSED_SRGB_ALPHA_S3TC_DXT5_EXT = $8C4F;
  Public
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSWEBGL_compressed_texture_s3tc_srgb;
  end;
  
  { --------------------------------------------------------------------
    TJSWEBGL_compressed_texture_astc
    --------------------------------------------------------------------}
  
  IJSWEBGL_compressed_texture_astc = interface(IJSObject)
    ['{FF6C2C3A-962A-33F1-BD30-05C620E357C6}']
    function getSupportedProfiles: TUnicodeStringDynArray;
  end;
  
  TJSWEBGL_compressed_texture_astc = class(TJSObject,IJSWEBGL_compressed_texture_astc)
  Private
  Protected
  Public
    Const
      COMPRESSED_RGBA_ASTC_4x4_KHR = $93B0;
      COMPRESSED_RGBA_ASTC_5x4_KHR = $93B1;
      COMPRESSED_RGBA_ASTC_5x5_KHR = $93B2;
      COMPRESSED_RGBA_ASTC_6x5_KHR = $93B3;
      COMPRESSED_RGBA_ASTC_6x6_KHR = $93B4;
      COMPRESSED_RGBA_ASTC_8x5_KHR = $93B5;
      COMPRESSED_RGBA_ASTC_8x6_KHR = $93B6;
      COMPRESSED_RGBA_ASTC_8x8_KHR = $93B7;
      COMPRESSED_RGBA_ASTC_10x5_KHR = $93B8;
      COMPRESSED_RGBA_ASTC_10x6_KHR = $93B9;
      COMPRESSED_RGBA_ASTC_10x8_KHR = $93BA;
      COMPRESSED_RGBA_ASTC_10x10_KHR = $93BB;
      COMPRESSED_RGBA_ASTC_12x10_KHR = $93BC;
      COMPRESSED_RGBA_ASTC_12x12_KHR = $93BD;
      COMPRESSED_SRGB8_ALPHA8_ASTC_4x4_KHR = $93D0;
      COMPRESSED_SRGB8_ALPHA8_ASTC_5x4_KHR = $93D1;
      COMPRESSED_SRGB8_ALPHA8_ASTC_5x5_KHR = $93D2;
      COMPRESSED_SRGB8_ALPHA8_ASTC_6x5_KHR = $93D3;
      COMPRESSED_SRGB8_ALPHA8_ASTC_6x6_KHR = $93D4;
      COMPRESSED_SRGB8_ALPHA8_ASTC_8x5_KHR = $93D5;
      COMPRESSED_SRGB8_ALPHA8_ASTC_8x6_KHR = $93D6;
      COMPRESSED_SRGB8_ALPHA8_ASTC_8x8_KHR = $93D7;
      COMPRESSED_SRGB8_ALPHA8_ASTC_10x5_KHR = $93D8;
      COMPRESSED_SRGB8_ALPHA8_ASTC_10x6_KHR = $93D9;
      COMPRESSED_SRGB8_ALPHA8_ASTC_10x8_KHR = $93DA;
      COMPRESSED_SRGB8_ALPHA8_ASTC_10x10_KHR = $93DB;
      COMPRESSED_SRGB8_ALPHA8_ASTC_12x10_KHR = $93DC;
      COMPRESSED_SRGB8_ALPHA8_ASTC_12x12_KHR = $93DD;
  Public
    function getSupportedProfiles: TUnicodeStringDynArray; overload;
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSWEBGL_compressed_texture_astc;
  end;
  
  { --------------------------------------------------------------------
    TJSWEBGL_compressed_texture_etc
    --------------------------------------------------------------------}
  
  IJSWEBGL_compressed_texture_etc = interface(IJSObject)
    ['{502905B1-5FD2-3862-9A5D-FD88E4ABA50F}']
  end;
  
  TJSWEBGL_compressed_texture_etc = class(TJSObject,IJSWEBGL_compressed_texture_etc)
  Private
  Protected
  Public
    Const
      COMPRESSED_R11_EAC = $9270;
      COMPRESSED_SIGNED_R11_EAC = $9271;
      COMPRESSED_RG11_EAC = $9272;
      COMPRESSED_SIGNED_RG11_EAC = $9273;
      COMPRESSED_RGB8_ETC2 = $9274;
      COMPRESSED_SRGB8_ETC2 = $9275;
      COMPRESSED_RGB8_PUNCHTHROUGH_ALPHA1_ETC2 = $9276;
      COMPRESSED_SRGB8_PUNCHTHROUGH_ALPHA1_ETC2 = $9277;
      COMPRESSED_RGBA8_ETC2_EAC = $9278;
      COMPRESSED_SRGB8_ALPHA8_ETC2_EAC = $9279;
  Public
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSWEBGL_compressed_texture_etc;
  end;
  
  { --------------------------------------------------------------------
    TJSWEBGL_compressed_texture_etc1
    --------------------------------------------------------------------}
  
  IJSWEBGL_compressed_texture_etc1 = interface(IJSObject)
    ['{10868591-9E8F-3CD8-8F29-D695F6738E5D}']
  end;
  
  TJSWEBGL_compressed_texture_etc1 = class(TJSObject,IJSWEBGL_compressed_texture_etc1)
  Private
  Protected
  Public
    Const
      COMPRESSED_RGB_ETC1_WEBGL = $8D64;
  Public
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSWEBGL_compressed_texture_etc1;
  end;
  
  { --------------------------------------------------------------------
    TJSWEBGL_compressed_texture_pvrtc
    --------------------------------------------------------------------}
  
  IJSWEBGL_compressed_texture_pvrtc = interface(IJSObject)
    ['{A88FA31E-9545-3BCD-8706-4258291039F2}']
  end;
  
  TJSWEBGL_compressed_texture_pvrtc = class(TJSObject,IJSWEBGL_compressed_texture_pvrtc)
  Private
  Protected
  Public
    Const
      COMPRESSED_RGB_PVRTC_4BPPV1_IMG = $8C00;
      COMPRESSED_RGB_PVRTC_2BPPV1_IMG = $8C01;
      COMPRESSED_RGBA_PVRTC_4BPPV1_IMG = $8C02;
      COMPRESSED_RGBA_PVRTC_2BPPV1_IMG = $8C03;
  Public
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSWEBGL_compressed_texture_pvrtc;
  end;
  
  { --------------------------------------------------------------------
    TJSWEBGL_debug_renderer_info
    --------------------------------------------------------------------}
  
  IJSWEBGL_debug_renderer_info = interface(IJSObject)
    ['{E196B370-86DB-3FF0-9082-445C2080E78E}']
  end;
  
  TJSWEBGL_debug_renderer_info = class(TJSObject,IJSWEBGL_debug_renderer_info)
  Private
  Protected
  Public
    Const
      UNMASKED_VENDOR_WEBGL = $9245;
      UNMASKED_RENDERER_WEBGL = $9246;
  Public
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSWEBGL_debug_renderer_info;
  end;
  
  { --------------------------------------------------------------------
    TJSWEBGL_debug_shaders
    --------------------------------------------------------------------}
  
  IJSWEBGL_debug_shaders = interface(IJSObject)
    ['{CEE5B3D6-2D10-3134-AEE9-8FB884B782C9}']
    function getTranslatedShaderSource(aShader: IJSWebGLShader): UnicodeString;
  end;
  
  TJSWEBGL_debug_shaders = class(TJSObject,IJSWEBGL_debug_shaders)
  Private
  Protected
  Public
    function getTranslatedShaderSource(aShader: IJSWebGLShader): UnicodeString; overload;
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSWEBGL_debug_shaders;
  end;
  
  { --------------------------------------------------------------------
    TJSWEBGL_depth_texture
    --------------------------------------------------------------------}
  
  IJSWEBGL_depth_texture = interface(IJSObject)
    ['{14BBC60C-8BAF-32A8-BB0B-97F4BF6DC170}']
  end;
  
  TJSWEBGL_depth_texture = class(TJSObject,IJSWEBGL_depth_texture)
  Private
  Protected
  Public
    Const
      UNSIGNED_INT_24_8_WEBGL = $84FA;
  Public
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSWEBGL_depth_texture;
  end;
  
  { --------------------------------------------------------------------
    TJSOES_element_index_uint
    --------------------------------------------------------------------}
  
  IJSOES_element_index_uint = interface(IJSObject)
    ['{D65E59BC-EEC1-33B0-85F5-14A4C90AEEA8}']
  end;
  
  TJSOES_element_index_uint = class(TJSObject,IJSOES_element_index_uint)
  Private
  Protected
  Public
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSOES_element_index_uint;
  end;
  
  { --------------------------------------------------------------------
    TJSEXT_frag_depth
    --------------------------------------------------------------------}
  
  IJSEXT_frag_depth = interface(IJSObject)
    ['{A28CA5C5-F232-353D-8BD4-487C3446E5A9}']
  end;
  
  TJSEXT_frag_depth = class(TJSObject,IJSEXT_frag_depth)
  Private
  Protected
  Public
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSEXT_frag_depth;
  end;
  
  { --------------------------------------------------------------------
    TJSWEBGL_lose_context
    --------------------------------------------------------------------}
  
  IJSWEBGL_lose_context = interface(IJSObject)
    ['{EF904EBE-4834-3C7C-AAB5-521AEC767130}']
    procedure loseContext;
    procedure restoreContext;
  end;
  
  TJSWEBGL_lose_context = class(TJSObject,IJSWEBGL_lose_context)
  Private
  Protected
  Public
    procedure loseContext; overload;
    procedure restoreContext; overload;
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSWEBGL_lose_context;
  end;
  
  { --------------------------------------------------------------------
    TJSEXT_texture_filter_anisotropic
    --------------------------------------------------------------------}
  
  IJSEXT_texture_filter_anisotropic = interface(IJSObject)
    ['{B19DFFDC-9D1C-3DB2-A948-4CDC9F913183}']
  end;
  
  TJSEXT_texture_filter_anisotropic = class(TJSObject,IJSEXT_texture_filter_anisotropic)
  Private
  Protected
  Public
    Const
      TEXTURE_MAX_ANISOTROPY_EXT = $84FE;
      MAX_TEXTURE_MAX_ANISOTROPY_EXT = $84FF;
  Public
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSEXT_texture_filter_anisotropic;
  end;
  
  { --------------------------------------------------------------------
    TJSEXT_sRGB
    --------------------------------------------------------------------}
  
  IJSEXT_sRGB = interface(IJSObject)
    ['{B9C6AE83-8AFF-34F8-8274-9419BD51456A}']
  end;
  
  TJSEXT_sRGB = class(TJSObject,IJSEXT_sRGB)
  Private
  Protected
  Public
    Const
      SRGB_EXT = $8C40;
      SRGB_ALPHA_EXT = $8C42;
      SRGB8_ALPHA8_EXT = $8C43;
      FRAMEBUFFER_ATTACHMENT_COLOR_ENCODING_EXT = $8210;
  Public
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSEXT_sRGB;
  end;
  
  { --------------------------------------------------------------------
    TJSOES_standard_derivatives
    --------------------------------------------------------------------}
  
  IJSOES_standard_derivatives = interface(IJSObject)
    ['{85C0E20C-05F0-37C9-8D23-76690955801F}']
  end;
  
  TJSOES_standard_derivatives = class(TJSObject,IJSOES_standard_derivatives)
  Private
  Protected
  Public
    Const
      FRAGMENT_SHADER_DERIVATIVE_HINT_OES = $8B8B;
  Public
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSOES_standard_derivatives;
  end;
  
  { --------------------------------------------------------------------
    TJSOES_texture_float
    --------------------------------------------------------------------}
  
  IJSOES_texture_float = interface(IJSObject)
    ['{2B7DA39C-A620-379E-847B-D04950F8698D}']
  end;
  
  TJSOES_texture_float = class(TJSObject,IJSOES_texture_float)
  Private
  Protected
  Public
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSOES_texture_float;
  end;
  
  { --------------------------------------------------------------------
    TJSWEBGL_draw_buffers
    --------------------------------------------------------------------}
  
  IJSWEBGL_draw_buffers = interface(IJSObject)
    ['{32A0CDB9-3796-3649-8E26-EBFEA16BC652}']
    procedure drawBuffersWEBGL(const aBuffers: TGLenumDynArray);
  end;
  
  TJSWEBGL_draw_buffers = class(TJSObject,IJSWEBGL_draw_buffers)
  Private
  Protected
  Public
    Const
      COLOR_ATTACHMENT0_WEBGL = $8CE0;
      COLOR_ATTACHMENT1_WEBGL = $8CE1;
      COLOR_ATTACHMENT2_WEBGL = $8CE2;
      COLOR_ATTACHMENT3_WEBGL = $8CE3;
      COLOR_ATTACHMENT4_WEBGL = $8CE4;
      COLOR_ATTACHMENT5_WEBGL = $8CE5;
      COLOR_ATTACHMENT6_WEBGL = $8CE6;
      COLOR_ATTACHMENT7_WEBGL = $8CE7;
      COLOR_ATTACHMENT8_WEBGL = $8CE8;
      COLOR_ATTACHMENT9_WEBGL = $8CE9;
      COLOR_ATTACHMENT10_WEBGL = $8CEA;
      COLOR_ATTACHMENT11_WEBGL = $8CEB;
      COLOR_ATTACHMENT12_WEBGL = $8CEC;
      COLOR_ATTACHMENT13_WEBGL = $8CED;
      COLOR_ATTACHMENT14_WEBGL = $8CEE;
      COLOR_ATTACHMENT15_WEBGL = $8CEF;
      DRAW_BUFFER0_WEBGL = $8825;
      DRAW_BUFFER1_WEBGL = $8826;
      DRAW_BUFFER2_WEBGL = $8827;
      DRAW_BUFFER3_WEBGL = $8828;
      DRAW_BUFFER4_WEBGL = $8829;
      DRAW_BUFFER5_WEBGL = $882A;
      DRAW_BUFFER6_WEBGL = $882B;
      DRAW_BUFFER7_WEBGL = $882C;
      DRAW_BUFFER8_WEBGL = $882D;
      DRAW_BUFFER9_WEBGL = $882E;
      DRAW_BUFFER10_WEBGL = $882F;
      DRAW_BUFFER11_WEBGL = $8830;
      DRAW_BUFFER12_WEBGL = $8831;
      DRAW_BUFFER13_WEBGL = $8832;
      DRAW_BUFFER14_WEBGL = $8833;
      DRAW_BUFFER15_WEBGL = $8834;
      MAX_COLOR_ATTACHMENTS_WEBGL = $8CDF;
      MAX_DRAW_BUFFERS_WEBGL = $8824;
  Public
    procedure drawBuffersWEBGL(const aBuffers: TGLenumDynArray); overload;
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSWEBGL_draw_buffers;
  end;
  
  { --------------------------------------------------------------------
    TJSOES_texture_float_linear
    --------------------------------------------------------------------}
  
  IJSOES_texture_float_linear = interface(IJSObject)
    ['{80481F65-0D71-30A2-A7A0-A6AA234038D8}']
  end;
  
  TJSOES_texture_float_linear = class(TJSObject,IJSOES_texture_float_linear)
  Private
  Protected
  Public
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSOES_texture_float_linear;
  end;
  
  { --------------------------------------------------------------------
    TJSEXT_shader_texture_lod
    --------------------------------------------------------------------}
  
  IJSEXT_shader_texture_lod = interface(IJSObject)
    ['{D65EC240-FCA0-31B2-B491-68C36CB24F28}']
  end;
  
  TJSEXT_shader_texture_lod = class(TJSObject,IJSEXT_shader_texture_lod)
  Private
  Protected
  Public
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSEXT_shader_texture_lod;
  end;
  
  { --------------------------------------------------------------------
    TJSOES_texture_half_float
    --------------------------------------------------------------------}
  
  IJSOES_texture_half_float = interface(IJSObject)
    ['{0C827F30-44FE-37A5-89F3-AEA33E66AC3C}']
  end;
  
  TJSOES_texture_half_float = class(TJSObject,IJSOES_texture_half_float)
  Private
  Protected
  Public
    Const
      HALF_FLOAT_OES = $8D61;
  Public
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSOES_texture_half_float;
  end;
  
  { --------------------------------------------------------------------
    TJSOES_texture_half_float_linear
    --------------------------------------------------------------------}
  
  IJSOES_texture_half_float_linear = interface(IJSObject)
    ['{7A46FE69-63CD-3666-B190-30355050D213}']
  end;
  
  TJSOES_texture_half_float_linear = class(TJSObject,IJSOES_texture_half_float_linear)
  Private
  Protected
  Public
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSOES_texture_half_float_linear;
  end;
  
  { --------------------------------------------------------------------
    TJSWEBGL_color_buffer_float
    --------------------------------------------------------------------}
  
  IJSWEBGL_color_buffer_float = interface(IJSObject)
    ['{1C25836A-B929-3322-9D51-EAFC9FAAAD30}']
  end;
  
  TJSWEBGL_color_buffer_float = class(TJSObject,IJSWEBGL_color_buffer_float)
  Private
  Protected
  Public
    Const
      RGBA32F_EXT = $8814;
      RGB32F_EXT = $8815;
      FRAMEBUFFER_ATTACHMENT_COMPONENT_TYPE_EXT = $8211;
      UNSIGNED_NORMALIZED_EXT = $8C17;
  Public
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSWEBGL_color_buffer_float;
  end;
  
  { --------------------------------------------------------------------
    TJSEXT_color_buffer_half_float
    --------------------------------------------------------------------}
  
  IJSEXT_color_buffer_half_float = interface(IJSObject)
    ['{BB900451-2C4D-3683-9563-62457BEB166D}']
  end;
  
  TJSEXT_color_buffer_half_float = class(TJSObject,IJSEXT_color_buffer_half_float)
  Private
  Protected
  Public
    Const
      RGBA16F_EXT = $881A;
      RGB16F_EXT = $881B;
      FRAMEBUFFER_ATTACHMENT_COMPONENT_TYPE_EXT = $8211;
      UNSIGNED_NORMALIZED_EXT = $8C17;
  Public
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSEXT_color_buffer_half_float;
  end;
  
  { --------------------------------------------------------------------
    TJSOES_vertex_array_object
    --------------------------------------------------------------------}
  
  IJSOES_vertex_array_object = interface(IJSObject)
    ['{4ABF74AF-5F5E-3E43-A4B2-5C9944CE6A3C}']
    function createVertexArrayOES: IJSWebGLVertexArrayObject;
    procedure deleteVertexArrayOES(aArrayObject: IJSWebGLVertexArrayObject);
    function isVertexArrayOES(aArrayObject: IJSWebGLVertexArrayObject): TGLboolean;
    procedure bindVertexArrayOES(aArrayObject: IJSWebGLVertexArrayObject);
  end;
  
  TJSOES_vertex_array_object = class(TJSObject,IJSOES_vertex_array_object)
  Private
  Protected
  Public
    Const
      VERTEX_ARRAY_BINDING_OES = $85B5;
  Public
    function createVertexArrayOES: IJSWebGLVertexArrayObject; overload;
    procedure deleteVertexArrayOES(aArrayObject: IJSWebGLVertexArrayObject); overload;
    function isVertexArrayOES(aArrayObject: IJSWebGLVertexArrayObject): TGLboolean; overload;
    procedure bindVertexArrayOES(aArrayObject: IJSWebGLVertexArrayObject); overload;
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSOES_vertex_array_object;
  end;
  
  { --------------------------------------------------------------------
    TJSANGLE_instanced_arrays
    --------------------------------------------------------------------}
  
  IJSANGLE_instanced_arrays = interface(IJSObject)
    ['{0E0941FF-892B-3230-85CA-25E0156FDC86}']
    procedure drawArraysInstancedANGLE(aMode: TGLenum; aFirst: TGLint; aCount: TGLsizei; aPrimcount: TGLsizei);
    procedure drawElementsInstancedANGLE(aMode: TGLenum; aCount: TGLsizei; aType_: TGLenum; aOffset: TGLintptr; aPrimcount: TGLsizei);
    procedure vertexAttribDivisorANGLE(aIndex: TGLuint; aDivisor: TGLuint);
  end;
  
  TJSANGLE_instanced_arrays = class(TJSObject,IJSANGLE_instanced_arrays)
  Private
  Protected
  Public
    Const
      VERTEX_ATTRIB_ARRAY_DIVISOR_ANGLE = $88FE;
  Public
    procedure drawArraysInstancedANGLE(aMode: TGLenum; aFirst: TGLint; aCount: TGLsizei; aPrimcount: TGLsizei); overload;
    procedure drawElementsInstancedANGLE(aMode: TGLenum; aCount: TGLsizei; aType_: TGLenum; aOffset: TGLintptr; aPrimcount: TGLsizei); overload;
    procedure vertexAttribDivisorANGLE(aIndex: TGLuint; aDivisor: TGLuint); overload;
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSANGLE_instanced_arrays;
  end;
  
  { --------------------------------------------------------------------
    TJSEXT_blend_minmax
    --------------------------------------------------------------------}
  
  IJSEXT_blend_minmax = interface(IJSObject)
    ['{6B68B361-44A4-3323-8438-1A18AD6CCC64}']
  end;
  
  TJSEXT_blend_minmax = class(TJSObject,IJSEXT_blend_minmax)
  Private
  Protected
  Public
    Const
      MIN_EXT = $8007;
      MAX_EXT = $8008;
  Public
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSEXT_blend_minmax;
  end;
  
  { --------------------------------------------------------------------
    TJSWebGLQuery
    --------------------------------------------------------------------}
  
  IJSWebGLQuery = interface(IJSObject)
    ['{F0F56ABA-6D4F-375C-87C0-636E89A48332}']
  end;
  
  TJSWebGLQuery = class(TJSObject,IJSWebGLQuery)
  Private
  Protected
  Public
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSWebGLQuery;
  end;
  
  { --------------------------------------------------------------------
    TJSEXT_disjoint_timer_query
    --------------------------------------------------------------------}
  
  IJSEXT_disjoint_timer_query = interface(IJSObject)
    ['{B08F1DD2-20BA-3E87-B41A-09AB51EF7B9F}']
    function createQueryEXT: IJSWebGLQuery;
    procedure deleteQueryEXT(aQuery: IJSWebGLQuery);
    function isQueryEXT(aQuery: IJSWebGLQuery): Boolean;
    procedure beginQueryEXT(aTarget: TGLenum; aQuery: IJSWebGLQuery);
    procedure endQueryEXT(aTarget: TGLenum);
    procedure queryCounterEXT(aQuery: IJSWebGLQuery; aTarget: TGLenum);
    function getQueryEXT(aTarget: TGLenum; aPname: TGLenum): Variant;
    function getQueryObjectEXT(aQuery: IJSWebGLQuery; aPname: TGLenum): Variant;
  end;
  
  TJSEXT_disjoint_timer_query = class(TJSObject,IJSEXT_disjoint_timer_query)
  Private
  Protected
  Public
    Const
      QUERY_COUNTER_BITS_EXT = $8864;
      CURRENT_QUERY_EXT = $8865;
      QUERY_RESULT_EXT = $8866;
      QUERY_RESULT_AVAILABLE_EXT = $8867;
      TIME_ELAPSED_EXT = $88BF;
      TIMESTAMP_EXT = $8E28;
      GPU_DISJOINT_EXT = $8FBB;
  Public
    function createQueryEXT: IJSWebGLQuery; overload;
    procedure deleteQueryEXT(aQuery: IJSWebGLQuery); overload;
    function isQueryEXT(aQuery: IJSWebGLQuery): Boolean; overload;
    procedure beginQueryEXT(aTarget: TGLenum; aQuery: IJSWebGLQuery); overload;
    procedure endQueryEXT(aTarget: TGLenum); overload;
    procedure queryCounterEXT(aQuery: IJSWebGLQuery; aTarget: TGLenum); overload;
    function getQueryEXT(aTarget: TGLenum; aPname: TGLenum): Variant; overload;
    function getQueryObjectEXT(aQuery: IJSWebGLQuery; aPname: TGLenum): Variant; overload;
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSEXT_disjoint_timer_query;
  end;
  
  { --------------------------------------------------------------------
    TJSMOZ_debug
    --------------------------------------------------------------------}
  
  IJSMOZ_debug = interface(IJSObject)
    ['{CD04A8F0-CBC4-3417-BF04-B53E2D77961D}']
    function getParameter(aPname: TGLenum): Variant;
  end;
  
  TJSMOZ_debug = class(TJSObject,IJSMOZ_debug)
  Private
  Protected
  Public
    Const
      EXTENSIONS = $1F03;
      WSI_INFO = $10000;
      UNPACK_REQUIRE_FASTPATH = $10001;
      DOES_INDEX_VALIDATION = $10002;
      CONTEXT_TYPE = $10003;
  Public
    function getParameter(aPname: TGLenum): Variant; overload;
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSMOZ_debug;
  end;
  
  { --------------------------------------------------------------------
    TJSEXT_float_blend
    --------------------------------------------------------------------}
  
  IJSEXT_float_blend = interface(IJSObject)
    ['{AC6B95A4-ABDF-3E0F-A938-8943E0B13444}']
  end;
  
  TJSEXT_float_blend = class(TJSObject,IJSEXT_float_blend)
  Private
  Protected
  Public
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSEXT_float_blend;
  end;
  
  { --------------------------------------------------------------------
    TJSOES_fbo_render_mipmap
    --------------------------------------------------------------------}
  
  IJSOES_fbo_render_mipmap = interface(IJSObject)
    ['{E0F1EFE9-8E37-397F-9776-01221D0C900F}']
  end;
  
  TJSOES_fbo_render_mipmap = class(TJSObject,IJSOES_fbo_render_mipmap)
  Private
  Protected
  Public
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSOES_fbo_render_mipmap;
  end;
  
  { --------------------------------------------------------------------
    TJSWEBGL_explicit_present
    --------------------------------------------------------------------}
  
  IJSWEBGL_explicit_present = interface(IJSObject)
    ['{28B9E715-D559-395F-9A75-42796C01798F}']
    procedure present;
  end;
  
  TJSWEBGL_explicit_present = class(TJSObject,IJSWEBGL_explicit_present)
  Private
  Protected
  Public
    procedure present; overload;
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSWEBGL_explicit_present;
  end;
  
  { --------------------------------------------------------------------
    TJSOES_draw_buffers_indexed
    --------------------------------------------------------------------}
  
  IJSOES_draw_buffers_indexed = interface(IJSObject)
    ['{302CA20A-D54E-3465-BD83-DE6842BDF66E}']
    procedure enableiOES(aTarget: TGLenum; aIndex: TGLuint);
    procedure disableiOES(aTarget: TGLenum; aIndex: TGLuint);
    procedure blendEquationiOES(aBuf: TGLuint; aMode: TGLenum);
    procedure blendEquationSeparateiOES(aBuf: TGLuint; aModeRGB: TGLenum; aModeAlpha: TGLenum);
    procedure blendFunciOES(aBuf: TGLuint; aSrc: TGLenum; aDst: TGLenum);
    procedure blendFuncSeparateiOES(aBuf: TGLuint; aSrcRGB: TGLenum; aDstRGB: TGLenum; aSrcAlpha: TGLenum; aDstAlpha: TGLenum);
    procedure colorMaskiOES(aBuf: TGLuint; aR: TGLboolean; aG: TGLboolean; aB: TGLboolean; aA: TGLboolean);
  end;
  
  TJSOES_draw_buffers_indexed = class(TJSObject,IJSOES_draw_buffers_indexed)
  Private
  Protected
  Public
    procedure enableiOES(aTarget: TGLenum; aIndex: TGLuint); overload;
    procedure disableiOES(aTarget: TGLenum; aIndex: TGLuint); overload;
    procedure blendEquationiOES(aBuf: TGLuint; aMode: TGLenum); overload;
    procedure blendEquationSeparateiOES(aBuf: TGLuint; aModeRGB: TGLenum; aModeAlpha: TGLenum); overload;
    procedure blendFunciOES(aBuf: TGLuint; aSrc: TGLenum; aDst: TGLenum); overload;
    procedure blendFuncSeparateiOES(aBuf: TGLuint; aSrcRGB: TGLenum; aDstRGB: TGLenum; aSrcAlpha: TGLenum; aDstAlpha: TGLenum); overload;
    procedure colorMaskiOES(aBuf: TGLuint; aR: TGLboolean; aG: TGLboolean; aB: TGLboolean; aA: TGLboolean); overload;
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSOES_draw_buffers_indexed;
  end;
  
  { --------------------------------------------------------------------
    TJSWEBGL_provoking_vertex
    --------------------------------------------------------------------}
  
  IJSWEBGL_provoking_vertex = interface(IJSObject)
    ['{0EF532CE-CEC1-3696-90F1-403816C02DB3}']
    procedure provokingVertexWEBGL(aProvokeMode: TGLenum);
  end;
  
  TJSWEBGL_provoking_vertex = class(TJSObject,IJSWEBGL_provoking_vertex)
  Private
  Protected
  Public
    Const
      FIRST_VERTEX_CONVENTION_WEBGL = $8E4D;
      LAST_VERTEX_CONVENTION_WEBGL = $8E4E;
      PROVOKING_VERTEX_WEBGL = $8E4F;
  Public
    procedure provokingVertexWEBGL(aProvokeMode: TGLenum); overload;
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSWEBGL_provoking_vertex;
  end;
  
  { --------------------------------------------------------------------
    TJSEXT_depth_clamp
    --------------------------------------------------------------------}
  
  IJSEXT_depth_clamp = interface(IJSObject)
    ['{E237BA79-A9A9-334F-AD34-E0A6AB793DA0}']
  end;
  
  TJSEXT_depth_clamp = class(TJSObject,IJSEXT_depth_clamp)
  Private
  Protected
  Public
    Const
      DEPTH_CLAMP_EXT = $864F;
  Public
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSEXT_depth_clamp;
  end;
  
  { --------------------------------------------------------------------
    TJSnsIBrowserDOMWindow
    --------------------------------------------------------------------}
  
  IJSnsIBrowserDOMWindow = interface(IJSObject)
    ['{7D9E6ECD-E32A-31DE-9B97-DF76A7603D1A}']
  end;
  
  TJSnsIBrowserDOMWindow = class(TJSObject,IJSnsIBrowserDOMWindow)
  Private
  Protected
  Public
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSnsIBrowserDOMWindow;
  end;
  
  { --------------------------------------------------------------------
    TJSXULControllers
    --------------------------------------------------------------------}
  
  IJSXULControllers = interface(IJSObject)
    ['{A28ED322-7B18-3C41-A572-A07C3446E5A9}']
  end;
  
  TJSXULControllers = class(TJSObject,IJSXULControllers)
  Private
  Protected
  Public
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSXULControllers;
  end;
  
  { --------------------------------------------------------------------
    TJSnsIDOMWindowUtils
    --------------------------------------------------------------------}
  
  IJSnsIDOMWindowUtils = interface(IJSObject)
    ['{2B7FAA71-2156-3F28-AF6A-0EDF40F8698D}']
  end;
  
  TJSnsIDOMWindowUtils = class(TJSObject,IJSnsIDOMWindowUtils)
  Private
  Protected
  Public
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSnsIDOMWindowUtils;
  end;
  
  { --------------------------------------------------------------------
    TJSnsIPrintSettings
    --------------------------------------------------------------------}
  
  IJSnsIPrintSettings = interface(IJSObject)
    ['{84BC3276-CC8E-3E16-A193-21A81E0D8EB9}']
  end;
  
  TJSnsIPrintSettings = class(TJSObject,IJSnsIPrintSettings)
  Private
  Protected
  Public
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSnsIPrintSettings;
  end;
  
  { --------------------------------------------------------------------
    TJSDOMRect
    --------------------------------------------------------------------}
  
  IJSDOMRect = interface(IJSDOMRectReadOnly)
    ['{491BD5AD-8D74-3F01-BC3C-2DF00C3E9384}']
    function _Getx2: Double; 
    function _Gety2: Double; 
    function _Getwidth2: Double; 
    function _Getheight2: Double; 
    procedure _Setx2(const aValue: Double);
    procedure _Sety2(const aValue: Double);
    procedure _Setwidth2(const aValue: Double);
    procedure _Setheight2(const aValue: Double);
    property x: Double read _Getx2 write _Setx2;
    property y: Double read _Gety2 write _Sety2;
    property width: Double read _Getwidth2 write _Setwidth2;
    property height: Double read _Getheight2 write _Setheight2;
  end;
  
  TJSDOMRect = class(TJSDOMRectReadOnly,IJSDOMRect)
  Private
  Protected
    function _Getx2: Double; 
    function _Gety2: Double; 
    function _Getwidth2: Double; 
    function _Getheight2: Double; 
    procedure _Setx2(const aValue: Double);
    procedure _Sety2(const aValue: Double);
    procedure _Setwidth2(const aValue: Double);
    procedure _Setheight2(const aValue: Double);
  Public
    constructor Create(aX: Double; aY: Double; aWidth: Double; aHeight: Double); overload;
    constructor Create; overload;
    constructor Create(aX: Double); overload;
    constructor Create(aX: Double; aY: Double); overload;
    constructor Create(aX: Double; aY: Double; aWidth: Double); overload;
    function fromRect(const aOther: IJSDOMRectInit): IJSDOMRect; overload;
    function fromRect: IJSDOMRect; overload;
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSDOMRect;
    property x: Double read _Getx2 write _Setx2;
    property y: Double read _Gety2 write _Sety2;
    property width: Double read _Getwidth2 write _Setwidth2;
    property height: Double read _Getheight2 write _Setheight2;
  end;
  
  { --------------------------------------------------------------------
    TJSNode
    --------------------------------------------------------------------}
  
  IJSNode = interface(IJSEventTarget)
    ['{DA0389F5-96B2-36AE-ABD0-206CDC946CD9}']
    function _GetnodeType: Word; 
    function _GetnodeName: UnicodeString; 
    function _GetbaseURI: UnicodeString; 
    function _GetisConnected: Boolean; 
    function _GetownerDocument: IJSDocument; 
    function _GetparentNode: IJSNode; 
    function _GetparentElement: IJSElement; 
    function _GetfirstChild: IJSNode; 
    function _GetlastChild: IJSNode; 
    function _GetpreviousSibling: IJSNode; 
    function _GetnextSibling: IJSNode; 
    function _GetnodeValue: UnicodeString; 
    function _GettextContent: UnicodeString; 
    procedure _SetnodeValue(const aValue: UnicodeString);
    procedure _SettextContent(const aValue: UnicodeString);
    function hasChildNodes: Boolean;
    function insertBefore(aNode: IJSNode; aChild: IJSNode): IJSNode;
    function appendChild(aNode: IJSNode): IJSNode;
    function replaceChild(aNode: IJSNode; aChild: IJSNode): IJSNode;
    function removeChild(aChild: IJSNode): IJSNode;
    procedure normalize;
    function cloneNode(aDeep: Boolean): IJSNode;
    function cloneNode: IJSNode;
    function isSameNode(aNode: IJSNode): Boolean;
    function isEqualNode(aNode: IJSNode): Boolean;
    function compareDocumentPosition(aOther: IJSNode): Word;
    function contains(aOther: IJSNode): Boolean;
    function lookupPrefix(const aNamespace: UnicodeString): UnicodeString;
    function lookupNamespaceURI(const aPrefix: UnicodeString): UnicodeString;
    function isDefaultNamespace(const aNamespace: UnicodeString): Boolean;
    property nodeType: Word read _GetnodeType;
    property nodeName: UnicodeString read _GetnodeName;
    property baseURI: UnicodeString read _GetbaseURI;
    property isConnected: Boolean read _GetisConnected;
    property ownerDocument: IJSDocument read _GetownerDocument;
    property parentNode: IJSNode read _GetparentNode;
    property parentElement: IJSElement read _GetparentElement;
    property firstChild: IJSNode read _GetfirstChild;
    property lastChild: IJSNode read _GetlastChild;
    property previousSibling: IJSNode read _GetpreviousSibling;
    property nextSibling: IJSNode read _GetnextSibling;
    property nodeValue: UnicodeString read _GetnodeValue write _SetnodeValue;
    property textContent: UnicodeString read _GettextContent write _SettextContent;
  end;
  
  TJSNode = class(TJSEventTarget,IJSNode)
  Private
  Protected
    function _GetnodeType: Word; 
    function _GetnodeName: UnicodeString; 
    function _GetbaseURI: UnicodeString; 
    function _GetisConnected: Boolean; 
    function _GetownerDocument: IJSDocument; 
    function _GetparentNode: IJSNode; 
    function _GetparentElement: IJSElement; 
    function _GetfirstChild: IJSNode; 
    function _GetlastChild: IJSNode; 
    function _GetpreviousSibling: IJSNode; 
    function _GetnextSibling: IJSNode; 
    function _GetnodeValue: UnicodeString; 
    function _GettextContent: UnicodeString; 
    procedure _SetnodeValue(const aValue: UnicodeString);
    procedure _SettextContent(const aValue: UnicodeString);
  Public
    Const
      ELEMENT_NODE = 1;
      ATTRIBUTE_NODE = 2;
      TEXT_NODE = 3;
      CDATA_SECTION_NODE = 4;
      ENTITY_REFERENCE_NODE = 5;
      ENTITY_NODE = 6;
      PROCESSING_INSTRUCTION_NODE = 7;
      COMMENT_NODE = 8;
      DOCUMENT_NODE = 9;
      DOCUMENT_TYPE_NODE = 10;
      DOCUMENT_FRAGMENT_NODE = 11;
      NOTATION_NODE = 12;
      DOCUMENT_POSITION_DISCONNECTED = $01;
      DOCUMENT_POSITION_PRECEDING = $02;
      DOCUMENT_POSITION_FOLLOWING = $04;
      DOCUMENT_POSITION_CONTAINS = $08;
      DOCUMENT_POSITION_CONTAINED_BY = $10;
      DOCUMENT_POSITION_IMPLEMENTATION_SPECIFIC = $20;
  Public
    function hasChildNodes: Boolean; overload;
    function insertBefore(aNode: IJSNode; aChild: IJSNode): IJSNode; overload;
    function appendChild(aNode: IJSNode): IJSNode; overload;
    function replaceChild(aNode: IJSNode; aChild: IJSNode): IJSNode; overload;
    function removeChild(aChild: IJSNode): IJSNode; overload;
    procedure normalize; overload;
    function cloneNode(aDeep: Boolean): IJSNode; overload;
    function cloneNode: IJSNode; overload;
    function isSameNode(aNode: IJSNode): Boolean; overload;
    function isEqualNode(aNode: IJSNode): Boolean; overload;
    function compareDocumentPosition(aOther: IJSNode): Word; overload;
    function contains(aOther: IJSNode): Boolean; overload;
    function lookupPrefix(const aNamespace: UnicodeString): UnicodeString; overload;
    function lookupNamespaceURI(const aPrefix: UnicodeString): UnicodeString; overload;
    function isDefaultNamespace(const aNamespace: UnicodeString): Boolean; overload;
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSNode;
    property nodeType: Word read _GetnodeType;
    property nodeName: UnicodeString read _GetnodeName;
    property baseURI: UnicodeString read _GetbaseURI;
    property isConnected: Boolean read _GetisConnected;
    property ownerDocument: IJSDocument read _GetownerDocument;
    property parentNode: IJSNode read _GetparentNode;
    property parentElement: IJSElement read _GetparentElement;
    property firstChild: IJSNode read _GetfirstChild;
    property lastChild: IJSNode read _GetlastChild;
    property previousSibling: IJSNode read _GetpreviousSibling;
    property nextSibling: IJSNode read _GetnextSibling;
    property nodeValue: UnicodeString read _GetnodeValue write _SetnodeValue;
    property textContent: UnicodeString read _GettextContent write _SettextContent;
  end;
  
  { --------------------------------------------------------------------
    TJSOffscreenCanvas
    --------------------------------------------------------------------}
  
  IJSOffscreenCanvas = interface(IJSEventTarget)
    ['{02FBF65C-AE8C-347A-B169-594077C8E35A}']
    function _Getwidth: Cardinal; 
    function _Getheight: Cardinal; 
    function _Getoncontextlost: TEventHandler; 
    function _Getoncontextrestored: TEventHandler; 
    procedure _Setwidth(const aValue: Cardinal);
    procedure _Setheight(const aValue: Cardinal);
    procedure _Setoncontextlost(const aValue: TEventHandler);
    procedure _Setoncontextrestored(const aValue: TEventHandler);
    function getContext(aContextId: TOffscreenRenderingContextId; const aContextOptions: Variant): TOffscreenRenderingContext;
    function getContext(aContextId: TOffscreenRenderingContextId): TOffscreenRenderingContext;
    function transferToImageBitmap: IJSImageBitmap;
    property width: Cardinal read _Getwidth write _Setwidth;
    property height: Cardinal read _Getheight write _Setheight;
    property oncontextlost: TEventHandler read _Getoncontextlost write _Setoncontextlost;
    property oncontextrestored: TEventHandler read _Getoncontextrestored write _Setoncontextrestored;
  end;
  
  TJSOffscreenCanvas = class(TJSEventTarget,IJSOffscreenCanvas)
  Private
  Protected
    function _Getwidth: Cardinal; 
    function _Getheight: Cardinal; 
    function _Getoncontextlost: TEventHandler; 
    function _Getoncontextrestored: TEventHandler; 
    procedure _Setwidth(const aValue: Cardinal);
    procedure _Setheight(const aValue: Cardinal);
    procedure _Setoncontextlost(const aValue: TEventHandler);
    procedure _Setoncontextrestored(const aValue: TEventHandler);
  Public
    constructor Create(aWidth: Cardinal; aHeight: Cardinal); overload;
    function getContext(aContextId: TOffscreenRenderingContextId; const aContextOptions: Variant): TOffscreenRenderingContext; overload;
    function getContext(aContextId: TOffscreenRenderingContextId): TOffscreenRenderingContext; overload;
    function transferToImageBitmap: IJSImageBitmap; overload;
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSOffscreenCanvas;
    property width: Cardinal read _Getwidth write _Setwidth;
    property height: Cardinal read _Getheight write _Setheight;
    property oncontextlost: TEventHandler read _Getoncontextlost write _Setoncontextlost;
    property oncontextrestored: TEventHandler read _Getoncontextrestored write _Setoncontextrestored;
  end;
  
  { --------------------------------------------------------------------
    TJSUIEvent
    --------------------------------------------------------------------}
  
  IJSUIEvent = interface(IJSEvent)
    ['{E1117B61-9923-311E-BFA0-D6A31F90F3F4}']
    function _Getview: IJSWindowProxy; 
    function _Getdetail: LongInt; 
    function _GetlayerX: LongInt; 
    function _GetlayerY: LongInt; 
    function _Getwhich: Cardinal; 
    function _GetrangeParent: IJSNode; 
    function _GetrangeOffset: LongInt; 
    procedure initUIEvent(const aAType: UnicodeString; aACanBubble: Boolean; aACancelable: Boolean; aAView: IJSWindow; aADetail: LongInt);
    procedure initUIEvent(const aAType: UnicodeString);
    procedure initUIEvent(const aAType: UnicodeString; aACanBubble: Boolean);
    procedure initUIEvent(const aAType: UnicodeString; aACanBubble: Boolean; aACancelable: Boolean);
    procedure initUIEvent(const aAType: UnicodeString; aACanBubble: Boolean; aACancelable: Boolean; aAView: IJSWindow);
    property view: IJSWindowProxy read _Getview;
    property detail: LongInt read _Getdetail;
    property layerX: LongInt read _GetlayerX;
    property layerY: LongInt read _GetlayerY;
    property which: Cardinal read _Getwhich;
    property rangeParent: IJSNode read _GetrangeParent;
    property rangeOffset: LongInt read _GetrangeOffset;
  end;
  
  TJSUIEvent = class(TJSEvent,IJSUIEvent)
  Private
  Protected
    function _Getview: IJSWindowProxy; 
    function _Getdetail: LongInt; 
    function _GetlayerX: LongInt; 
    function _GetlayerY: LongInt; 
    function _Getwhich: Cardinal; 
    function _GetrangeParent: IJSNode; 
    function _GetrangeOffset: LongInt; 
  Public
    Const
      SCROLL_PAGE_UP = -32768;
      SCROLL_PAGE_DOWN = 32768;
  Public
    constructor Create(const aType_: UnicodeString; const aEventInitDict: IJSUIEventInit); overload;
    constructor Create(const aType_: UnicodeString); overload;
    procedure initUIEvent(const aAType: UnicodeString; aACanBubble: Boolean; aACancelable: Boolean; aAView: IJSWindow; aADetail: LongInt); overload;
    procedure initUIEvent(const aAType: UnicodeString); overload;
    procedure initUIEvent(const aAType: UnicodeString; aACanBubble: Boolean); overload;
    procedure initUIEvent(const aAType: UnicodeString; aACanBubble: Boolean; aACancelable: Boolean); overload;
    procedure initUIEvent(const aAType: UnicodeString; aACanBubble: Boolean; aACancelable: Boolean; aAView: IJSWindow); overload;
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSUIEvent;
    property view: IJSWindowProxy read _Getview;
    property detail: LongInt read _Getdetail;
    property layerX: LongInt read _GetlayerX;
    property layerY: LongInt read _GetlayerY;
    property which: Cardinal read _Getwhich;
    property rangeParent: IJSNode read _GetrangeParent;
    property rangeOffset: LongInt read _GetrangeOffset;
  end;
  
  { --------------------------------------------------------------------
    TJSWebGLContextEvent
    --------------------------------------------------------------------}
  
  IJSWebGLContextEvent = interface(IJSEvent)
    ['{A5D3378A-06AF-36A6-B91F-B74FD3041E56}']
    function _GetstatusMessage: UnicodeString; 
    property statusMessage: UnicodeString read _GetstatusMessage;
  end;
  
  TJSWebGLContextEvent = class(TJSEvent,IJSWebGLContextEvent)
  Private
  Protected
    function _GetstatusMessage: UnicodeString; 
  Public
    constructor Create(const aType_: UnicodeString; const aEventInit: IJSWebGLContextEventInit); overload;
    constructor Create(const aType_: UnicodeString); overload;
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSWebGLContextEvent;
    property statusMessage: UnicodeString read _GetstatusMessage;
  end;
  
  { --------------------------------------------------------------------
    TJSWebGLRenderingContext
    --------------------------------------------------------------------}
  
  IJSWebGLRenderingContext = interface(IJSWebGLRenderingContextBase)
    ['{934B04AF-A7AC-3EB4-9728-0BCBA82CAAF2}']
    procedure bufferData(aTarget: TGLenum; aSize: TGLsizeiptr; aUsage: TGLenum);
    procedure bufferData(aTarget: TGLenum; aData: IJSArrayBuffer; aUsage: TGLenum);
    procedure bufferData(aTarget: TGLenum; aData: IJSArrayBufferView; aUsage: TGLenum);
    procedure bufferSubData(aTarget: TGLenum; aOffset: TGLintptr; aData: IJSArrayBuffer);
    procedure bufferSubData(aTarget: TGLenum; aOffset: TGLintptr; aData: IJSArrayBufferView);
    procedure compressedTexImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLenum; aWidth: TGLsizei; aHeight: TGLsizei; aBorder: TGLint; aData: IJSArrayBufferView);
    procedure compressedTexSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aFormat: TGLenum; aData: IJSArrayBufferView);
    procedure readPixels(aX: TGLint; aY: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aPixels: IJSArrayBufferView);
    procedure texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aPixels: IJSArrayBufferView);
    procedure texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aFormat: TGLenum; aType_: TGLenum; aPixels: IJSImageBitmap);
    procedure texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aFormat: TGLenum; aType_: TGLenum; aPixels: IJSImageData);
    procedure texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aFormat: TGLenum; aType_: TGLenum; aImage: IJSHTMLImageElement);
    procedure texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aFormat: TGLenum; aType_: TGLenum; aCanvas: IJSHTMLCanvasElement);
    procedure texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aFormat: TGLenum; aType_: TGLenum; aVideo: IJSHTMLVideoElement);
    procedure texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aFormat: TGLenum; aType_: TGLenum; aCanvas: IJSOffscreenCanvas);
    procedure texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aFormat: TGLenum; aType_: TGLenum; aVideoFrame: IJSVideoFrame);
    procedure texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aPixels: IJSArrayBufferView);
    procedure texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aFormat: TGLenum; aType_: TGLenum; aPixels: IJSImageBitmap);
    procedure texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aFormat: TGLenum; aType_: TGLenum; aPixels: IJSImageData);
    procedure texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aFormat: TGLenum; aType_: TGLenum; aImage: IJSHTMLImageElement);
    procedure texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aFormat: TGLenum; aType_: TGLenum; aCanvas: IJSHTMLCanvasElement);
    procedure texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aFormat: TGLenum; aType_: TGLenum; aVideo: IJSHTMLVideoElement);
    procedure texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aFormat: TGLenum; aType_: TGLenum; aCanvas: IJSOffscreenCanvas);
    procedure texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aFormat: TGLenum; aType_: TGLenum; aVideoFrame: IJSVideoFrame);
    procedure uniform1fv(aLocation: IJSWebGLUniformLocation; aData: IJSFloat32Array);
    procedure uniform1fv(aLocation: IJSWebGLUniformLocation; const aData: TGLfloatDynArray);
    procedure uniform2fv(aLocation: IJSWebGLUniformLocation; aData: IJSFloat32Array);
    procedure uniform2fv(aLocation: IJSWebGLUniformLocation; const aData: TGLfloatDynArray);
    procedure uniform3fv(aLocation: IJSWebGLUniformLocation; aData: IJSFloat32Array);
    procedure uniform3fv(aLocation: IJSWebGLUniformLocation; const aData: TGLfloatDynArray);
    procedure uniform4fv(aLocation: IJSWebGLUniformLocation; aData: IJSFloat32Array);
    procedure uniform4fv(aLocation: IJSWebGLUniformLocation; const aData: TGLfloatDynArray);
    procedure uniform1iv(aLocation: IJSWebGLUniformLocation; aData: IJSInt32Array);
    procedure uniform1iv(aLocation: IJSWebGLUniformLocation; const aData: TGLintDynArray);
    procedure uniform2iv(aLocation: IJSWebGLUniformLocation; aData: IJSInt32Array);
    procedure uniform2iv(aLocation: IJSWebGLUniformLocation; const aData: TGLintDynArray);
    procedure uniform3iv(aLocation: IJSWebGLUniformLocation; aData: IJSInt32Array);
    procedure uniform3iv(aLocation: IJSWebGLUniformLocation; const aData: TGLintDynArray);
    procedure uniform4iv(aLocation: IJSWebGLUniformLocation; aData: IJSInt32Array);
    procedure uniform4iv(aLocation: IJSWebGLUniformLocation; const aData: TGLintDynArray);
    procedure uniformMatrix2fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array);
    procedure uniformMatrix2fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray);
    procedure uniformMatrix3fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array);
    procedure uniformMatrix3fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray);
    procedure uniformMatrix4fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array);
    procedure uniformMatrix4fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray);
  end;
  
  TJSWebGLRenderingContext = class(TJSWebGLRenderingContextBase,IJSWebGLRenderingContext)
  Private
  Protected
  Public
    procedure bufferData(aTarget: TGLenum; aSize: TGLsizeiptr; aUsage: TGLenum); overload;
    procedure bufferData(aTarget: TGLenum; aData: IJSArrayBuffer; aUsage: TGLenum); overload;
    procedure bufferData(aTarget: TGLenum; aData: IJSArrayBufferView; aUsage: TGLenum); overload;
    procedure bufferSubData(aTarget: TGLenum; aOffset: TGLintptr; aData: IJSArrayBuffer); overload;
    procedure bufferSubData(aTarget: TGLenum; aOffset: TGLintptr; aData: IJSArrayBufferView); overload;
    procedure compressedTexImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLenum; aWidth: TGLsizei; aHeight: TGLsizei; aBorder: TGLint; aData: IJSArrayBufferView); overload;
    procedure compressedTexSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aFormat: TGLenum; aData: IJSArrayBufferView); overload;
    procedure readPixels(aX: TGLint; aY: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aPixels: IJSArrayBufferView); overload;
    procedure texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aPixels: IJSArrayBufferView); overload;
    procedure texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aFormat: TGLenum; aType_: TGLenum; aPixels: IJSImageBitmap); overload;
    procedure texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aFormat: TGLenum; aType_: TGLenum; aPixels: IJSImageData); overload;
    procedure texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aFormat: TGLenum; aType_: TGLenum; aImage: IJSHTMLImageElement); overload;
    procedure texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aFormat: TGLenum; aType_: TGLenum; aCanvas: IJSHTMLCanvasElement); overload;
    procedure texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aFormat: TGLenum; aType_: TGLenum; aVideo: IJSHTMLVideoElement); overload;
    procedure texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aFormat: TGLenum; aType_: TGLenum; aCanvas: IJSOffscreenCanvas); overload;
    procedure texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aFormat: TGLenum; aType_: TGLenum; aVideoFrame: IJSVideoFrame); overload;
    procedure texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aPixels: IJSArrayBufferView); overload;
    procedure texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aFormat: TGLenum; aType_: TGLenum; aPixels: IJSImageBitmap); overload;
    procedure texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aFormat: TGLenum; aType_: TGLenum; aPixels: IJSImageData); overload;
    procedure texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aFormat: TGLenum; aType_: TGLenum; aImage: IJSHTMLImageElement); overload;
    procedure texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aFormat: TGLenum; aType_: TGLenum; aCanvas: IJSHTMLCanvasElement); overload;
    procedure texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aFormat: TGLenum; aType_: TGLenum; aVideo: IJSHTMLVideoElement); overload;
    procedure texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aFormat: TGLenum; aType_: TGLenum; aCanvas: IJSOffscreenCanvas); overload;
    procedure texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aFormat: TGLenum; aType_: TGLenum; aVideoFrame: IJSVideoFrame); overload;
    procedure uniform1fv(aLocation: IJSWebGLUniformLocation; aData: IJSFloat32Array); overload;
    procedure uniform1fv(aLocation: IJSWebGLUniformLocation; const aData: TGLfloatDynArray); overload;
    procedure uniform2fv(aLocation: IJSWebGLUniformLocation; aData: IJSFloat32Array); overload;
    procedure uniform2fv(aLocation: IJSWebGLUniformLocation; const aData: TGLfloatDynArray); overload;
    procedure uniform3fv(aLocation: IJSWebGLUniformLocation; aData: IJSFloat32Array); overload;
    procedure uniform3fv(aLocation: IJSWebGLUniformLocation; const aData: TGLfloatDynArray); overload;
    procedure uniform4fv(aLocation: IJSWebGLUniformLocation; aData: IJSFloat32Array); overload;
    procedure uniform4fv(aLocation: IJSWebGLUniformLocation; const aData: TGLfloatDynArray); overload;
    procedure uniform1iv(aLocation: IJSWebGLUniformLocation; aData: IJSInt32Array); overload;
    procedure uniform1iv(aLocation: IJSWebGLUniformLocation; const aData: TGLintDynArray); overload;
    procedure uniform2iv(aLocation: IJSWebGLUniformLocation; aData: IJSInt32Array); overload;
    procedure uniform2iv(aLocation: IJSWebGLUniformLocation; const aData: TGLintDynArray); overload;
    procedure uniform3iv(aLocation: IJSWebGLUniformLocation; aData: IJSInt32Array); overload;
    procedure uniform3iv(aLocation: IJSWebGLUniformLocation; const aData: TGLintDynArray); overload;
    procedure uniform4iv(aLocation: IJSWebGLUniformLocation; aData: IJSInt32Array); overload;
    procedure uniform4iv(aLocation: IJSWebGLUniformLocation; const aData: TGLintDynArray); overload;
    procedure uniformMatrix2fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array); overload;
    procedure uniformMatrix2fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray); overload;
    procedure uniformMatrix3fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array); overload;
    procedure uniformMatrix3fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray); overload;
    procedure uniformMatrix4fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array); overload;
    procedure uniformMatrix4fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray); overload;
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSWebGLRenderingContext;
  end;
  
  { --------------------------------------------------------------------
    TJSWindow
    --------------------------------------------------------------------}
  
  IJSWindow = interface(IJSEventTarget)
    ['{7F70DB15-9FE5-38D7-8737-7370FF9CC6BA}']
    function _GetdevicePixelRatio: Double; 
    function requestAnimationFrame(const aCallback: TFrameRequestCallback): LongInt;
    procedure cancelAnimationFrame(aHandle: LongInt);
    property devicePixelRatio: Double read _GetdevicePixelRatio;
  end;
  
  TJSWindow = class(TJSEventTarget,IJSWindow)
  Private
  Protected
    function _GetdevicePixelRatio: Double; 
  Public
    function requestAnimationFrame(const aCallback: TFrameRequestCallback): LongInt; overload;
    procedure cancelAnimationFrame(aHandle: LongInt); overload;
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSWindow;
    property devicePixelRatio: Double read _GetdevicePixelRatio;
  end;
  
  { --------------------------------------------------------------------
    TJSDocument
    --------------------------------------------------------------------}
  
  IJSDocument = interface(IJSNode)
    ['{854193E3-7817-343F-B20F-DC8B7D9CC85A}']
    function getElementById(const aElementId: UnicodeString): IJSElement;
  end;
  
  TJSDocument = class(TJSNode,IJSDocument)
  Private
  Protected
  Public
    function getElementById(const aElementId: UnicodeString): IJSElement; overload;
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSDocument;
  end;
  
  { --------------------------------------------------------------------
    TJSElement
    --------------------------------------------------------------------}
  
  IJSElement = interface(IJSNode)
    ['{ABE51DF6-570F-31A0-917E-3E93C20CD2B8}']
    function _GetnamespaceURI: UnicodeString; 
    function _Getprefix: UnicodeString; 
    function _GetlocalName: UnicodeString; 
    function _GettagName: UnicodeString; 
    function _Getid: UnicodeString; 
    function _GetclassName_: UnicodeString; 
    function _GetclientTop: LongInt; 
    function _GetclientLeft: LongInt; 
    function _GetclientWidth: LongInt; 
    function _GetclientHeight: LongInt; 
    function _GetcurrentCSSZoom: Double; 
    function _GetinnerHTML: UnicodeString; 
    function _GetouterHTML: UnicodeString; 
    procedure _Setid(const aValue: UnicodeString);
    procedure _SetclassName_(const aValue: UnicodeString);
    procedure _SetinnerHTML(const aValue: UnicodeString);
    procedure _SetouterHTML(const aValue: UnicodeString);
    function getBoundingClientRect: IJSDOMRect;
    procedure insertAdjacentHTML(const aPosition: UnicodeString; const aText: UnicodeString);
    property namespaceURI: UnicodeString read _GetnamespaceURI;
    property prefix: UnicodeString read _Getprefix;
    property localName: UnicodeString read _GetlocalName;
    property tagName: UnicodeString read _GettagName;
    property id: UnicodeString read _Getid write _Setid;
    property className_: UnicodeString read _GetclassName_ write _SetclassName_;
    property clientTop: LongInt read _GetclientTop;
    property clientLeft: LongInt read _GetclientLeft;
    property clientWidth: LongInt read _GetclientWidth;
    property clientHeight: LongInt read _GetclientHeight;
    property currentCSSZoom: Double read _GetcurrentCSSZoom;
    property innerHTML: UnicodeString read _GetinnerHTML write _SetinnerHTML;
    property outerHTML: UnicodeString read _GetouterHTML write _SetouterHTML;
  end;
  
  TJSElement = class(TJSNode,IJSElement)
  Private
  Protected
    function _GetnamespaceURI: UnicodeString; 
    function _Getprefix: UnicodeString; 
    function _GetlocalName: UnicodeString; 
    function _GettagName: UnicodeString; 
    function _Getid: UnicodeString; 
    function _GetclassName_: UnicodeString; 
    function _GetclientTop: LongInt; 
    function _GetclientLeft: LongInt; 
    function _GetclientWidth: LongInt; 
    function _GetclientHeight: LongInt; 
    function _GetcurrentCSSZoom: Double; 
    function _GetinnerHTML: UnicodeString; 
    function _GetouterHTML: UnicodeString; 
    procedure _Setid(const aValue: UnicodeString);
    procedure _SetclassName_(const aValue: UnicodeString);
    procedure _SetinnerHTML(const aValue: UnicodeString);
    procedure _SetouterHTML(const aValue: UnicodeString);
  Public
    function getBoundingClientRect: IJSDOMRect; overload;
    procedure insertAdjacentHTML(const aPosition: UnicodeString; const aText: UnicodeString); overload;
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSElement;
    property namespaceURI: UnicodeString read _GetnamespaceURI;
    property prefix: UnicodeString read _Getprefix;
    property localName: UnicodeString read _GetlocalName;
    property tagName: UnicodeString read _GettagName;
    property id: UnicodeString read _Getid write _Setid;
    property className_: UnicodeString read _GetclassName_ write _SetclassName_;
    property clientTop: LongInt read _GetclientTop;
    property clientLeft: LongInt read _GetclientLeft;
    property clientWidth: LongInt read _GetclientWidth;
    property clientHeight: LongInt read _GetclientHeight;
    property currentCSSZoom: Double read _GetcurrentCSSZoom;
    property innerHTML: UnicodeString read _GetinnerHTML write _SetinnerHTML;
    property outerHTML: UnicodeString read _GetouterHTML write _SetouterHTML;
  end;
  
  { --------------------------------------------------------------------
    TJSKeyboardEvent
    --------------------------------------------------------------------}
  
  IJSKeyboardEvent = interface(IJSUIEvent)
    ['{E4815410-81F4-3460-9137-25F38B8CCB57}']
    function _GetcharCode: Cardinal; 
    function _GetkeyCode: Cardinal; 
    function _GetaltKey: Boolean; 
    function _GetctrlKey: Boolean; 
    function _GetshiftKey: Boolean; 
    function _GetmetaKey: Boolean; 
    function _Getlocation: Cardinal; 
    function _Getrepeat_: Boolean; 
    function _GetisComposing: Boolean; 
    function _Getkey: UnicodeString; 
    function _Getcode: UnicodeString; 
    function getModifierState(const aKey: UnicodeString): Boolean;
    procedure initKeyboardEvent(const aTypeArg: UnicodeString; aBubblesArg: Boolean; aCancelableArg: Boolean; aViewArg: IJSWindow; const aKeyArg: UnicodeString; aLocationArg: Cardinal; aCtrlKey: Boolean; aAltKey: Boolean; aShiftKey: Boolean; aMetaKey: Boolean);
    procedure initKeyboardEvent(const aTypeArg: UnicodeString);
    procedure initKeyboardEvent(const aTypeArg: UnicodeString; aBubblesArg: Boolean);
    procedure initKeyboardEvent(const aTypeArg: UnicodeString; aBubblesArg: Boolean; aCancelableArg: Boolean);
    procedure initKeyboardEvent(const aTypeArg: UnicodeString; aBubblesArg: Boolean; aCancelableArg: Boolean; aViewArg: IJSWindow);
    procedure initKeyboardEvent(const aTypeArg: UnicodeString; aBubblesArg: Boolean; aCancelableArg: Boolean; aViewArg: IJSWindow; const aKeyArg: UnicodeString);
    procedure initKeyboardEvent(const aTypeArg: UnicodeString; aBubblesArg: Boolean; aCancelableArg: Boolean; aViewArg: IJSWindow; const aKeyArg: UnicodeString; aLocationArg: Cardinal);
    procedure initKeyboardEvent(const aTypeArg: UnicodeString; aBubblesArg: Boolean; aCancelableArg: Boolean; aViewArg: IJSWindow; const aKeyArg: UnicodeString; aLocationArg: Cardinal; aCtrlKey: Boolean);
    procedure initKeyboardEvent(const aTypeArg: UnicodeString; aBubblesArg: Boolean; aCancelableArg: Boolean; aViewArg: IJSWindow; const aKeyArg: UnicodeString; aLocationArg: Cardinal; aCtrlKey: Boolean; aAltKey: Boolean);
    procedure initKeyboardEvent(const aTypeArg: UnicodeString; aBubblesArg: Boolean; aCancelableArg: Boolean; aViewArg: IJSWindow; const aKeyArg: UnicodeString; aLocationArg: Cardinal; aCtrlKey: Boolean; aAltKey: Boolean; aShiftKey: Boolean);
    property charCode: Cardinal read _GetcharCode;
    property keyCode: Cardinal read _GetkeyCode;
    property altKey: Boolean read _GetaltKey;
    property ctrlKey: Boolean read _GetctrlKey;
    property shiftKey: Boolean read _GetshiftKey;
    property metaKey: Boolean read _GetmetaKey;
    property location: Cardinal read _Getlocation;
    property repeat_: Boolean read _Getrepeat_;
    property isComposing: Boolean read _GetisComposing;
    property key: UnicodeString read _Getkey;
    property code: UnicodeString read _Getcode;
  end;
  
  TJSKeyboardEvent = class(TJSUIEvent,IJSKeyboardEvent)
  Private
  Protected
    function _GetcharCode: Cardinal; 
    function _GetkeyCode: Cardinal; 
    function _GetaltKey: Boolean; 
    function _GetctrlKey: Boolean; 
    function _GetshiftKey: Boolean; 
    function _GetmetaKey: Boolean; 
    function _Getlocation: Cardinal; 
    function _Getrepeat_: Boolean; 
    function _GetisComposing: Boolean; 
    function _Getkey: UnicodeString; 
    function _Getcode: UnicodeString; 
  Public
    Const
      DOM_KEY_LOCATION_STANDARD = $00;
      DOM_KEY_LOCATION_LEFT = $01;
      DOM_KEY_LOCATION_RIGHT = $02;
      DOM_KEY_LOCATION_NUMPAD = $03;
  Public
    constructor Create(const aTypeArg: UnicodeString; const aKeyboardEventInitDict: IJSKeyboardEventInit); overload;
    constructor Create(const aTypeArg: UnicodeString); overload;
    function getModifierState(const aKey: UnicodeString): Boolean; overload;
    procedure initKeyboardEvent(const aTypeArg: UnicodeString; aBubblesArg: Boolean; aCancelableArg: Boolean; aViewArg: IJSWindow; const aKeyArg: UnicodeString; aLocationArg: Cardinal; aCtrlKey: Boolean; aAltKey: Boolean; aShiftKey: Boolean; aMetaKey: Boolean); overload;
    procedure initKeyboardEvent(const aTypeArg: UnicodeString); overload;
    procedure initKeyboardEvent(const aTypeArg: UnicodeString; aBubblesArg: Boolean); overload;
    procedure initKeyboardEvent(const aTypeArg: UnicodeString; aBubblesArg: Boolean; aCancelableArg: Boolean); overload;
    procedure initKeyboardEvent(const aTypeArg: UnicodeString; aBubblesArg: Boolean; aCancelableArg: Boolean; aViewArg: IJSWindow); overload;
    procedure initKeyboardEvent(const aTypeArg: UnicodeString; aBubblesArg: Boolean; aCancelableArg: Boolean; aViewArg: IJSWindow; const aKeyArg: UnicodeString); overload;
    procedure initKeyboardEvent(const aTypeArg: UnicodeString; aBubblesArg: Boolean; aCancelableArg: Boolean; aViewArg: IJSWindow; const aKeyArg: UnicodeString; aLocationArg: Cardinal); overload;
    procedure initKeyboardEvent(const aTypeArg: UnicodeString; aBubblesArg: Boolean; aCancelableArg: Boolean; aViewArg: IJSWindow; const aKeyArg: UnicodeString; aLocationArg: Cardinal; aCtrlKey: Boolean); overload;
    procedure initKeyboardEvent(const aTypeArg: UnicodeString; aBubblesArg: Boolean; aCancelableArg: Boolean; aViewArg: IJSWindow; const aKeyArg: UnicodeString; aLocationArg: Cardinal; aCtrlKey: Boolean; aAltKey: Boolean); overload;
    procedure initKeyboardEvent(const aTypeArg: UnicodeString; aBubblesArg: Boolean; aCancelableArg: Boolean; aViewArg: IJSWindow; const aKeyArg: UnicodeString; aLocationArg: Cardinal; aCtrlKey: Boolean; aAltKey: Boolean; aShiftKey: Boolean); overload;
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSKeyboardEvent;
    property charCode: Cardinal read _GetcharCode;
    property keyCode: Cardinal read _GetkeyCode;
    property altKey: Boolean read _GetaltKey;
    property ctrlKey: Boolean read _GetctrlKey;
    property shiftKey: Boolean read _GetshiftKey;
    property metaKey: Boolean read _GetmetaKey;
    property location: Cardinal read _Getlocation;
    property repeat_: Boolean read _Getrepeat_;
    property isComposing: Boolean read _GetisComposing;
    property key: UnicodeString read _Getkey;
    property code: UnicodeString read _Getcode;
  end;
  
  { --------------------------------------------------------------------
    TJSMouseEvent
    --------------------------------------------------------------------}
  
  IJSMouseEvent = interface(IJSUIEvent)
    ['{FEAE5AEA-A8E7-3CA8-BD34-D58191B0F886}']
    function _GetscreenX: Double; 
    function _GetscreenY: Double; 
    function _GetpageX: Double; 
    function _GetpageY: Double; 
    function _GetclientX: Double; 
    function _GetclientY: Double; 
    function _Getx: Double; 
    function _Gety: Double; 
    function _GetoffsetX: Double; 
    function _GetoffsetY: Double; 
    function _GetctrlKey: Boolean; 
    function _GetshiftKey: Boolean; 
    function _GetaltKey: Boolean; 
    function _GetmetaKey: Boolean; 
    function _Getbutton: SmallInt; 
    function _Getbuttons: Word; 
    function _GetrelatedTarget: IJSEventTarget; 
    function _GetmovementX: LongInt; 
    function _GetmovementY: LongInt; 
    function _GetmozPressure: Single; 
    function _GetmozInputSource: Word; 
    procedure initMouseEvent(const aTypeArg: UnicodeString; aCanBubbleArg: Boolean; aCancelableArg: Boolean; aViewArg: IJSWindow; aDetailArg: LongInt; aScreenXArg: LongInt; aScreenYArg: LongInt; aClientXArg: LongInt; aClientYArg: LongInt; aCtrlKeyArg: Boolean; aAltKeyArg: Boolean; aShiftKeyArg: Boolean; aMetaKeyArg: Boolean; aButtonArg: SmallInt; aRelatedTargetArg: IJSEventTarget);
    procedure initMouseEvent(const aTypeArg: UnicodeString);
    procedure initMouseEvent(const aTypeArg: UnicodeString; aCanBubbleArg: Boolean);
    procedure initMouseEvent(const aTypeArg: UnicodeString; aCanBubbleArg: Boolean; aCancelableArg: Boolean);
    procedure initMouseEvent(const aTypeArg: UnicodeString; aCanBubbleArg: Boolean; aCancelableArg: Boolean; aViewArg: IJSWindow);
    procedure initMouseEvent(const aTypeArg: UnicodeString; aCanBubbleArg: Boolean; aCancelableArg: Boolean; aViewArg: IJSWindow; aDetailArg: LongInt);
    procedure initMouseEvent(const aTypeArg: UnicodeString; aCanBubbleArg: Boolean; aCancelableArg: Boolean; aViewArg: IJSWindow; aDetailArg: LongInt; aScreenXArg: LongInt);
    procedure initMouseEvent(const aTypeArg: UnicodeString; aCanBubbleArg: Boolean; aCancelableArg: Boolean; aViewArg: IJSWindow; aDetailArg: LongInt; aScreenXArg: LongInt; aScreenYArg: LongInt);
    procedure initMouseEvent(const aTypeArg: UnicodeString; aCanBubbleArg: Boolean; aCancelableArg: Boolean; aViewArg: IJSWindow; aDetailArg: LongInt; aScreenXArg: LongInt; aScreenYArg: LongInt; aClientXArg: LongInt);
    procedure initMouseEvent(const aTypeArg: UnicodeString; aCanBubbleArg: Boolean; aCancelableArg: Boolean; aViewArg: IJSWindow; aDetailArg: LongInt; aScreenXArg: LongInt; aScreenYArg: LongInt; aClientXArg: LongInt; aClientYArg: LongInt);
    procedure initMouseEvent(const aTypeArg: UnicodeString; aCanBubbleArg: Boolean; aCancelableArg: Boolean; aViewArg: IJSWindow; aDetailArg: LongInt; aScreenXArg: LongInt; aScreenYArg: LongInt; aClientXArg: LongInt; aClientYArg: LongInt; aCtrlKeyArg: Boolean);
    procedure initMouseEvent(const aTypeArg: UnicodeString; aCanBubbleArg: Boolean; aCancelableArg: Boolean; aViewArg: IJSWindow; aDetailArg: LongInt; aScreenXArg: LongInt; aScreenYArg: LongInt; aClientXArg: LongInt; aClientYArg: LongInt; aCtrlKeyArg: Boolean; aAltKeyArg: Boolean);
    procedure initMouseEvent(const aTypeArg: UnicodeString; aCanBubbleArg: Boolean; aCancelableArg: Boolean; aViewArg: IJSWindow; aDetailArg: LongInt; aScreenXArg: LongInt; aScreenYArg: LongInt; aClientXArg: LongInt; aClientYArg: LongInt; aCtrlKeyArg: Boolean; aAltKeyArg: Boolean; aShiftKeyArg: Boolean);
    procedure initMouseEvent(const aTypeArg: UnicodeString; aCanBubbleArg: Boolean; aCancelableArg: Boolean; aViewArg: IJSWindow; aDetailArg: LongInt; aScreenXArg: LongInt; aScreenYArg: LongInt; aClientXArg: LongInt; aClientYArg: LongInt; aCtrlKeyArg: Boolean; aAltKeyArg: Boolean; aShiftKeyArg: Boolean; aMetaKeyArg: Boolean);
    procedure initMouseEvent(const aTypeArg: UnicodeString; aCanBubbleArg: Boolean; aCancelableArg: Boolean; aViewArg: IJSWindow; aDetailArg: LongInt; aScreenXArg: LongInt; aScreenYArg: LongInt; aClientXArg: LongInt; aClientYArg: LongInt; aCtrlKeyArg: Boolean; aAltKeyArg: Boolean; aShiftKeyArg: Boolean; aMetaKeyArg: Boolean; aButtonArg: SmallInt);
    function getModifierState(const aKeyArg: UnicodeString): Boolean;
    procedure initNSMouseEvent(const aTypeArg: UnicodeString; aCanBubbleArg: Boolean; aCancelableArg: Boolean; aViewArg: IJSWindow; aDetailArg: LongInt; aScreenXArg: LongInt; aScreenYArg: LongInt; aClientXArg: LongInt; aClientYArg: LongInt; aCtrlKeyArg: Boolean; aAltKeyArg: Boolean; aShiftKeyArg: Boolean; aMetaKeyArg: Boolean; aButtonArg: SmallInt; aRelatedTargetArg: IJSEventTarget; aPressure: Single; aInputSourceArg: Word);
    procedure initNSMouseEvent(const aTypeArg: UnicodeString);
    procedure initNSMouseEvent(const aTypeArg: UnicodeString; aCanBubbleArg: Boolean);
    procedure initNSMouseEvent(const aTypeArg: UnicodeString; aCanBubbleArg: Boolean; aCancelableArg: Boolean);
    procedure initNSMouseEvent(const aTypeArg: UnicodeString; aCanBubbleArg: Boolean; aCancelableArg: Boolean; aViewArg: IJSWindow);
    procedure initNSMouseEvent(const aTypeArg: UnicodeString; aCanBubbleArg: Boolean; aCancelableArg: Boolean; aViewArg: IJSWindow; aDetailArg: LongInt);
    procedure initNSMouseEvent(const aTypeArg: UnicodeString; aCanBubbleArg: Boolean; aCancelableArg: Boolean; aViewArg: IJSWindow; aDetailArg: LongInt; aScreenXArg: LongInt);
    procedure initNSMouseEvent(const aTypeArg: UnicodeString; aCanBubbleArg: Boolean; aCancelableArg: Boolean; aViewArg: IJSWindow; aDetailArg: LongInt; aScreenXArg: LongInt; aScreenYArg: LongInt);
    procedure initNSMouseEvent(const aTypeArg: UnicodeString; aCanBubbleArg: Boolean; aCancelableArg: Boolean; aViewArg: IJSWindow; aDetailArg: LongInt; aScreenXArg: LongInt; aScreenYArg: LongInt; aClientXArg: LongInt);
    procedure initNSMouseEvent(const aTypeArg: UnicodeString; aCanBubbleArg: Boolean; aCancelableArg: Boolean; aViewArg: IJSWindow; aDetailArg: LongInt; aScreenXArg: LongInt; aScreenYArg: LongInt; aClientXArg: LongInt; aClientYArg: LongInt);
    procedure initNSMouseEvent(const aTypeArg: UnicodeString; aCanBubbleArg: Boolean; aCancelableArg: Boolean; aViewArg: IJSWindow; aDetailArg: LongInt; aScreenXArg: LongInt; aScreenYArg: LongInt; aClientXArg: LongInt; aClientYArg: LongInt; aCtrlKeyArg: Boolean);
    procedure initNSMouseEvent(const aTypeArg: UnicodeString; aCanBubbleArg: Boolean; aCancelableArg: Boolean; aViewArg: IJSWindow; aDetailArg: LongInt; aScreenXArg: LongInt; aScreenYArg: LongInt; aClientXArg: LongInt; aClientYArg: LongInt; aCtrlKeyArg: Boolean; aAltKeyArg: Boolean);
    procedure initNSMouseEvent(const aTypeArg: UnicodeString; aCanBubbleArg: Boolean; aCancelableArg: Boolean; aViewArg: IJSWindow; aDetailArg: LongInt; aScreenXArg: LongInt; aScreenYArg: LongInt; aClientXArg: LongInt; aClientYArg: LongInt; aCtrlKeyArg: Boolean; aAltKeyArg: Boolean; aShiftKeyArg: Boolean);
    procedure initNSMouseEvent(const aTypeArg: UnicodeString; aCanBubbleArg: Boolean; aCancelableArg: Boolean; aViewArg: IJSWindow; aDetailArg: LongInt; aScreenXArg: LongInt; aScreenYArg: LongInt; aClientXArg: LongInt; aClientYArg: LongInt; aCtrlKeyArg: Boolean; aAltKeyArg: Boolean; aShiftKeyArg: Boolean; aMetaKeyArg: Boolean);
    procedure initNSMouseEvent(const aTypeArg: UnicodeString; aCanBubbleArg: Boolean; aCancelableArg: Boolean; aViewArg: IJSWindow; aDetailArg: LongInt; aScreenXArg: LongInt; aScreenYArg: LongInt; aClientXArg: LongInt; aClientYArg: LongInt; aCtrlKeyArg: Boolean; aAltKeyArg: Boolean; aShiftKeyArg: Boolean; aMetaKeyArg: Boolean; aButtonArg: SmallInt);
    procedure initNSMouseEvent(const aTypeArg: UnicodeString; aCanBubbleArg: Boolean; aCancelableArg: Boolean; aViewArg: IJSWindow; aDetailArg: LongInt; aScreenXArg: LongInt; aScreenYArg: LongInt; aClientXArg: LongInt; aClientYArg: LongInt; aCtrlKeyArg: Boolean; aAltKeyArg: Boolean; aShiftKeyArg: Boolean; aMetaKeyArg: Boolean; aButtonArg: SmallInt; aRelatedTargetArg: IJSEventTarget);
    procedure initNSMouseEvent(const aTypeArg: UnicodeString; aCanBubbleArg: Boolean; aCancelableArg: Boolean; aViewArg: IJSWindow; aDetailArg: LongInt; aScreenXArg: LongInt; aScreenYArg: LongInt; aClientXArg: LongInt; aClientYArg: LongInt; aCtrlKeyArg: Boolean; aAltKeyArg: Boolean; aShiftKeyArg: Boolean; aMetaKeyArg: Boolean; aButtonArg: SmallInt; aRelatedTargetArg: IJSEventTarget; aPressure: Single);
    property screenX: Double read _GetscreenX;
    property screenY: Double read _GetscreenY;
    property pageX: Double read _GetpageX;
    property pageY: Double read _GetpageY;
    property clientX: Double read _GetclientX;
    property clientY: Double read _GetclientY;
    property x: Double read _Getx;
    property y: Double read _Gety;
    property offsetX: Double read _GetoffsetX;
    property offsetY: Double read _GetoffsetY;
    property ctrlKey: Boolean read _GetctrlKey;
    property shiftKey: Boolean read _GetshiftKey;
    property altKey: Boolean read _GetaltKey;
    property metaKey: Boolean read _GetmetaKey;
    property button: SmallInt read _Getbutton;
    property buttons: Word read _Getbuttons;
    property relatedTarget: IJSEventTarget read _GetrelatedTarget;
    property movementX: LongInt read _GetmovementX;
    property movementY: LongInt read _GetmovementY;
    property mozPressure: Single read _GetmozPressure;
    property mozInputSource: Word read _GetmozInputSource;
  end;
  
  TJSMouseEvent = class(TJSUIEvent,IJSMouseEvent)
  Private
  Protected
    function _GetscreenX: Double; 
    function _GetscreenY: Double; 
    function _GetpageX: Double; 
    function _GetpageY: Double; 
    function _GetclientX: Double; 
    function _GetclientY: Double; 
    function _Getx: Double; 
    function _Gety: Double; 
    function _GetoffsetX: Double; 
    function _GetoffsetY: Double; 
    function _GetctrlKey: Boolean; 
    function _GetshiftKey: Boolean; 
    function _GetaltKey: Boolean; 
    function _GetmetaKey: Boolean; 
    function _Getbutton: SmallInt; 
    function _Getbuttons: Word; 
    function _GetrelatedTarget: IJSEventTarget; 
    function _GetmovementX: LongInt; 
    function _GetmovementY: LongInt; 
    function _GetmozPressure: Single; 
    function _GetmozInputSource: Word; 
  Public
    Const
      MOZ_SOURCE_UNKNOWN = 0;
      MOZ_SOURCE_MOUSE = 1;
      MOZ_SOURCE_PEN = 2;
      MOZ_SOURCE_ERASER = 3;
      MOZ_SOURCE_CURSOR = 4;
      MOZ_SOURCE_TOUCH = 5;
      MOZ_SOURCE_KEYBOARD = 6;
  Public
    constructor Create(const aTypeArg: UnicodeString; const aMouseEventInitDict: IJSMouseEventInit); overload;
    constructor Create(const aTypeArg: UnicodeString); overload;
    procedure initMouseEvent(const aTypeArg: UnicodeString; aCanBubbleArg: Boolean; aCancelableArg: Boolean; aViewArg: IJSWindow; aDetailArg: LongInt; aScreenXArg: LongInt; aScreenYArg: LongInt; aClientXArg: LongInt; aClientYArg: LongInt; aCtrlKeyArg: Boolean; aAltKeyArg: Boolean; aShiftKeyArg: Boolean; aMetaKeyArg: Boolean; aButtonArg: SmallInt; aRelatedTargetArg: IJSEventTarget); overload;
    procedure initMouseEvent(const aTypeArg: UnicodeString); overload;
    procedure initMouseEvent(const aTypeArg: UnicodeString; aCanBubbleArg: Boolean); overload;
    procedure initMouseEvent(const aTypeArg: UnicodeString; aCanBubbleArg: Boolean; aCancelableArg: Boolean); overload;
    procedure initMouseEvent(const aTypeArg: UnicodeString; aCanBubbleArg: Boolean; aCancelableArg: Boolean; aViewArg: IJSWindow); overload;
    procedure initMouseEvent(const aTypeArg: UnicodeString; aCanBubbleArg: Boolean; aCancelableArg: Boolean; aViewArg: IJSWindow; aDetailArg: LongInt); overload;
    procedure initMouseEvent(const aTypeArg: UnicodeString; aCanBubbleArg: Boolean; aCancelableArg: Boolean; aViewArg: IJSWindow; aDetailArg: LongInt; aScreenXArg: LongInt); overload;
    procedure initMouseEvent(const aTypeArg: UnicodeString; aCanBubbleArg: Boolean; aCancelableArg: Boolean; aViewArg: IJSWindow; aDetailArg: LongInt; aScreenXArg: LongInt; aScreenYArg: LongInt); overload;
    procedure initMouseEvent(const aTypeArg: UnicodeString; aCanBubbleArg: Boolean; aCancelableArg: Boolean; aViewArg: IJSWindow; aDetailArg: LongInt; aScreenXArg: LongInt; aScreenYArg: LongInt; aClientXArg: LongInt); overload;
    procedure initMouseEvent(const aTypeArg: UnicodeString; aCanBubbleArg: Boolean; aCancelableArg: Boolean; aViewArg: IJSWindow; aDetailArg: LongInt; aScreenXArg: LongInt; aScreenYArg: LongInt; aClientXArg: LongInt; aClientYArg: LongInt); overload;
    procedure initMouseEvent(const aTypeArg: UnicodeString; aCanBubbleArg: Boolean; aCancelableArg: Boolean; aViewArg: IJSWindow; aDetailArg: LongInt; aScreenXArg: LongInt; aScreenYArg: LongInt; aClientXArg: LongInt; aClientYArg: LongInt; aCtrlKeyArg: Boolean); overload;
    procedure initMouseEvent(const aTypeArg: UnicodeString; aCanBubbleArg: Boolean; aCancelableArg: Boolean; aViewArg: IJSWindow; aDetailArg: LongInt; aScreenXArg: LongInt; aScreenYArg: LongInt; aClientXArg: LongInt; aClientYArg: LongInt; aCtrlKeyArg: Boolean; aAltKeyArg: Boolean); overload;
    procedure initMouseEvent(const aTypeArg: UnicodeString; aCanBubbleArg: Boolean; aCancelableArg: Boolean; aViewArg: IJSWindow; aDetailArg: LongInt; aScreenXArg: LongInt; aScreenYArg: LongInt; aClientXArg: LongInt; aClientYArg: LongInt; aCtrlKeyArg: Boolean; aAltKeyArg: Boolean; aShiftKeyArg: Boolean); overload;
    procedure initMouseEvent(const aTypeArg: UnicodeString; aCanBubbleArg: Boolean; aCancelableArg: Boolean; aViewArg: IJSWindow; aDetailArg: LongInt; aScreenXArg: LongInt; aScreenYArg: LongInt; aClientXArg: LongInt; aClientYArg: LongInt; aCtrlKeyArg: Boolean; aAltKeyArg: Boolean; aShiftKeyArg: Boolean; aMetaKeyArg: Boolean); overload;
    procedure initMouseEvent(const aTypeArg: UnicodeString; aCanBubbleArg: Boolean; aCancelableArg: Boolean; aViewArg: IJSWindow; aDetailArg: LongInt; aScreenXArg: LongInt; aScreenYArg: LongInt; aClientXArg: LongInt; aClientYArg: LongInt; aCtrlKeyArg: Boolean; aAltKeyArg: Boolean; aShiftKeyArg: Boolean; aMetaKeyArg: Boolean; aButtonArg: SmallInt); overload;
    function getModifierState(const aKeyArg: UnicodeString): Boolean; overload;
    procedure initNSMouseEvent(const aTypeArg: UnicodeString; aCanBubbleArg: Boolean; aCancelableArg: Boolean; aViewArg: IJSWindow; aDetailArg: LongInt; aScreenXArg: LongInt; aScreenYArg: LongInt; aClientXArg: LongInt; aClientYArg: LongInt; aCtrlKeyArg: Boolean; aAltKeyArg: Boolean; aShiftKeyArg: Boolean; aMetaKeyArg: Boolean; aButtonArg: SmallInt; aRelatedTargetArg: IJSEventTarget; aPressure: Single; aInputSourceArg: Word); overload;
    procedure initNSMouseEvent(const aTypeArg: UnicodeString); overload;
    procedure initNSMouseEvent(const aTypeArg: UnicodeString; aCanBubbleArg: Boolean); overload;
    procedure initNSMouseEvent(const aTypeArg: UnicodeString; aCanBubbleArg: Boolean; aCancelableArg: Boolean); overload;
    procedure initNSMouseEvent(const aTypeArg: UnicodeString; aCanBubbleArg: Boolean; aCancelableArg: Boolean; aViewArg: IJSWindow); overload;
    procedure initNSMouseEvent(const aTypeArg: UnicodeString; aCanBubbleArg: Boolean; aCancelableArg: Boolean; aViewArg: IJSWindow; aDetailArg: LongInt); overload;
    procedure initNSMouseEvent(const aTypeArg: UnicodeString; aCanBubbleArg: Boolean; aCancelableArg: Boolean; aViewArg: IJSWindow; aDetailArg: LongInt; aScreenXArg: LongInt); overload;
    procedure initNSMouseEvent(const aTypeArg: UnicodeString; aCanBubbleArg: Boolean; aCancelableArg: Boolean; aViewArg: IJSWindow; aDetailArg: LongInt; aScreenXArg: LongInt; aScreenYArg: LongInt); overload;
    procedure initNSMouseEvent(const aTypeArg: UnicodeString; aCanBubbleArg: Boolean; aCancelableArg: Boolean; aViewArg: IJSWindow; aDetailArg: LongInt; aScreenXArg: LongInt; aScreenYArg: LongInt; aClientXArg: LongInt); overload;
    procedure initNSMouseEvent(const aTypeArg: UnicodeString; aCanBubbleArg: Boolean; aCancelableArg: Boolean; aViewArg: IJSWindow; aDetailArg: LongInt; aScreenXArg: LongInt; aScreenYArg: LongInt; aClientXArg: LongInt; aClientYArg: LongInt); overload;
    procedure initNSMouseEvent(const aTypeArg: UnicodeString; aCanBubbleArg: Boolean; aCancelableArg: Boolean; aViewArg: IJSWindow; aDetailArg: LongInt; aScreenXArg: LongInt; aScreenYArg: LongInt; aClientXArg: LongInt; aClientYArg: LongInt; aCtrlKeyArg: Boolean); overload;
    procedure initNSMouseEvent(const aTypeArg: UnicodeString; aCanBubbleArg: Boolean; aCancelableArg: Boolean; aViewArg: IJSWindow; aDetailArg: LongInt; aScreenXArg: LongInt; aScreenYArg: LongInt; aClientXArg: LongInt; aClientYArg: LongInt; aCtrlKeyArg: Boolean; aAltKeyArg: Boolean); overload;
    procedure initNSMouseEvent(const aTypeArg: UnicodeString; aCanBubbleArg: Boolean; aCancelableArg: Boolean; aViewArg: IJSWindow; aDetailArg: LongInt; aScreenXArg: LongInt; aScreenYArg: LongInt; aClientXArg: LongInt; aClientYArg: LongInt; aCtrlKeyArg: Boolean; aAltKeyArg: Boolean; aShiftKeyArg: Boolean); overload;
    procedure initNSMouseEvent(const aTypeArg: UnicodeString; aCanBubbleArg: Boolean; aCancelableArg: Boolean; aViewArg: IJSWindow; aDetailArg: LongInt; aScreenXArg: LongInt; aScreenYArg: LongInt; aClientXArg: LongInt; aClientYArg: LongInt; aCtrlKeyArg: Boolean; aAltKeyArg: Boolean; aShiftKeyArg: Boolean; aMetaKeyArg: Boolean); overload;
    procedure initNSMouseEvent(const aTypeArg: UnicodeString; aCanBubbleArg: Boolean; aCancelableArg: Boolean; aViewArg: IJSWindow; aDetailArg: LongInt; aScreenXArg: LongInt; aScreenYArg: LongInt; aClientXArg: LongInt; aClientYArg: LongInt; aCtrlKeyArg: Boolean; aAltKeyArg: Boolean; aShiftKeyArg: Boolean; aMetaKeyArg: Boolean; aButtonArg: SmallInt); overload;
    procedure initNSMouseEvent(const aTypeArg: UnicodeString; aCanBubbleArg: Boolean; aCancelableArg: Boolean; aViewArg: IJSWindow; aDetailArg: LongInt; aScreenXArg: LongInt; aScreenYArg: LongInt; aClientXArg: LongInt; aClientYArg: LongInt; aCtrlKeyArg: Boolean; aAltKeyArg: Boolean; aShiftKeyArg: Boolean; aMetaKeyArg: Boolean; aButtonArg: SmallInt; aRelatedTargetArg: IJSEventTarget); overload;
    procedure initNSMouseEvent(const aTypeArg: UnicodeString; aCanBubbleArg: Boolean; aCancelableArg: Boolean; aViewArg: IJSWindow; aDetailArg: LongInt; aScreenXArg: LongInt; aScreenYArg: LongInt; aClientXArg: LongInt; aClientYArg: LongInt; aCtrlKeyArg: Boolean; aAltKeyArg: Boolean; aShiftKeyArg: Boolean; aMetaKeyArg: Boolean; aButtonArg: SmallInt; aRelatedTargetArg: IJSEventTarget; aPressure: Single); overload;
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSMouseEvent;
    property screenX: Double read _GetscreenX;
    property screenY: Double read _GetscreenY;
    property pageX: Double read _GetpageX;
    property pageY: Double read _GetpageY;
    property clientX: Double read _GetclientX;
    property clientY: Double read _GetclientY;
    property x: Double read _Getx;
    property y: Double read _Gety;
    property offsetX: Double read _GetoffsetX;
    property offsetY: Double read _GetoffsetY;
    property ctrlKey: Boolean read _GetctrlKey;
    property shiftKey: Boolean read _GetshiftKey;
    property altKey: Boolean read _GetaltKey;
    property metaKey: Boolean read _GetmetaKey;
    property button: SmallInt read _Getbutton;
    property buttons: Word read _Getbuttons;
    property relatedTarget: IJSEventTarget read _GetrelatedTarget;
    property movementX: LongInt read _GetmovementX;
    property movementY: LongInt read _GetmovementY;
    property mozPressure: Single read _GetmozPressure;
    property mozInputSource: Word read _GetmozInputSource;
  end;
  
  { --------------------------------------------------------------------
    TJSHTMLElement
    --------------------------------------------------------------------}
  
  IJSHTMLElement = interface(IJSElement)
    ['{8646D969-2C68-3AC7-8132-28BD49F51868}']
    function _Gettitle: UnicodeString; 
    function _Getlang: UnicodeString; 
    function _Gettranslate: Boolean; 
    function _Getdir: UnicodeString; 
    function _GetinnerText: UnicodeString; 
    function _GetouterText: UnicodeString; 
    function _Gethidden: Boolean; 
    function _Getinert: Boolean; 
    function _GetaccessKey: UnicodeString; 
    function _GetaccessKeyLabel: UnicodeString; 
    function _Getdraggable: Boolean; 
    function _GetcontentEditable: UnicodeString; 
    function _GetisContentEditable: Boolean; 
    function _Getpopover: UnicodeString; 
    function _Getspellcheck: Boolean; 
    function _GetinputMode: UnicodeString; 
    function _GetenterKeyHint: UnicodeString; 
    function _Getautocapitalize: UnicodeString; 
    function _Getautocorrect: Boolean; 
    function _Getnonce: UnicodeString; 
    function _GetoffsetParent: IJSElement; 
    function _GetoffsetTop: LongInt; 
    function _GetoffsetLeft: LongInt; 
    function _GetoffsetWidth: LongInt; 
    function _GetoffsetHeight: LongInt; 
    function _Getstyle: IJSCSSStyleDeclaration; 
    procedure _Settitle(const aValue: UnicodeString);
    procedure _Setlang(const aValue: UnicodeString);
    procedure _Settranslate(const aValue: Boolean);
    procedure _Setdir(const aValue: UnicodeString);
    procedure _SetinnerText(const aValue: UnicodeString);
    procedure _SetouterText(const aValue: UnicodeString);
    procedure _Sethidden(const aValue: Boolean);
    procedure _Setinert(const aValue: Boolean);
    procedure _SetaccessKey(const aValue: UnicodeString);
    procedure _Setdraggable(const aValue: Boolean);
    procedure _SetcontentEditable(const aValue: UnicodeString);
    procedure _Setpopover(const aValue: UnicodeString);
    procedure _Setspellcheck(const aValue: Boolean);
    procedure _SetinputMode(const aValue: UnicodeString);
    procedure _SetenterKeyHint(const aValue: UnicodeString);
    procedure _Setautocapitalize(const aValue: UnicodeString);
    procedure _Setautocorrect(const aValue: Boolean);
    procedure _Setnonce(const aValue: UnicodeString);
    procedure click;
    property title: UnicodeString read _Gettitle write _Settitle;
    property lang: UnicodeString read _Getlang write _Setlang;
    property translate: Boolean read _Gettranslate write _Settranslate;
    property dir: UnicodeString read _Getdir write _Setdir;
    property innerText: UnicodeString read _GetinnerText write _SetinnerText;
    property outerText: UnicodeString read _GetouterText write _SetouterText;
    property hidden: Boolean read _Gethidden write _Sethidden;
    property inert: Boolean read _Getinert write _Setinert;
    property accessKey: UnicodeString read _GetaccessKey write _SetaccessKey;
    property accessKeyLabel: UnicodeString read _GetaccessKeyLabel;
    property draggable: Boolean read _Getdraggable write _Setdraggable;
    property contentEditable: UnicodeString read _GetcontentEditable write _SetcontentEditable;
    property isContentEditable: Boolean read _GetisContentEditable;
    property popover: UnicodeString read _Getpopover write _Setpopover;
    property spellcheck: Boolean read _Getspellcheck write _Setspellcheck;
    property inputMode: UnicodeString read _GetinputMode write _SetinputMode;
    property enterKeyHint: UnicodeString read _GetenterKeyHint write _SetenterKeyHint;
    property autocapitalize: UnicodeString read _Getautocapitalize write _Setautocapitalize;
    property autocorrect: Boolean read _Getautocorrect write _Setautocorrect;
    property nonce: UnicodeString read _Getnonce write _Setnonce;
    property offsetParent: IJSElement read _GetoffsetParent;
    property offsetTop: LongInt read _GetoffsetTop;
    property offsetLeft: LongInt read _GetoffsetLeft;
    property offsetWidth: LongInt read _GetoffsetWidth;
    property offsetHeight: LongInt read _GetoffsetHeight;
    property style: IJSCSSStyleDeclaration read _Getstyle;
  end;
  
  TJSHTMLElement = class(TJSElement,IJSHTMLElement)
  Private
  Protected
    function _Gettitle: UnicodeString; 
    function _Getlang: UnicodeString; 
    function _Gettranslate: Boolean; 
    function _Getdir: UnicodeString; 
    function _GetinnerText: UnicodeString; 
    function _GetouterText: UnicodeString; 
    function _Gethidden: Boolean; 
    function _Getinert: Boolean; 
    function _GetaccessKey: UnicodeString; 
    function _GetaccessKeyLabel: UnicodeString; 
    function _Getdraggable: Boolean; 
    function _GetcontentEditable: UnicodeString; 
    function _GetisContentEditable: Boolean; 
    function _Getpopover: UnicodeString; 
    function _Getspellcheck: Boolean; 
    function _GetinputMode: UnicodeString; 
    function _GetenterKeyHint: UnicodeString; 
    function _Getautocapitalize: UnicodeString; 
    function _Getautocorrect: Boolean; 
    function _Getnonce: UnicodeString; 
    function _GetoffsetParent: IJSElement; 
    function _GetoffsetTop: LongInt; 
    function _GetoffsetLeft: LongInt; 
    function _GetoffsetWidth: LongInt; 
    function _GetoffsetHeight: LongInt; 
    function _Getstyle: IJSCSSStyleDeclaration; 
    procedure _Settitle(const aValue: UnicodeString);
    procedure _Setlang(const aValue: UnicodeString);
    procedure _Settranslate(const aValue: Boolean);
    procedure _Setdir(const aValue: UnicodeString);
    procedure _SetinnerText(const aValue: UnicodeString);
    procedure _SetouterText(const aValue: UnicodeString);
    procedure _Sethidden(const aValue: Boolean);
    procedure _Setinert(const aValue: Boolean);
    procedure _SetaccessKey(const aValue: UnicodeString);
    procedure _Setdraggable(const aValue: Boolean);
    procedure _SetcontentEditable(const aValue: UnicodeString);
    procedure _Setpopover(const aValue: UnicodeString);
    procedure _Setspellcheck(const aValue: Boolean);
    procedure _SetinputMode(const aValue: UnicodeString);
    procedure _SetenterKeyHint(const aValue: UnicodeString);
    procedure _Setautocapitalize(const aValue: UnicodeString);
    procedure _Setautocorrect(const aValue: Boolean);
    procedure _Setnonce(const aValue: UnicodeString);
  Public
    constructor Create; overload;
    procedure click; overload;
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSHTMLElement;
    property title: UnicodeString read _Gettitle write _Settitle;
    property lang: UnicodeString read _Getlang write _Setlang;
    property translate: Boolean read _Gettranslate write _Settranslate;
    property dir: UnicodeString read _Getdir write _Setdir;
    property innerText: UnicodeString read _GetinnerText write _SetinnerText;
    property outerText: UnicodeString read _GetouterText write _SetouterText;
    property hidden: Boolean read _Gethidden write _Sethidden;
    property inert: Boolean read _Getinert write _Setinert;
    property accessKey: UnicodeString read _GetaccessKey write _SetaccessKey;
    property accessKeyLabel: UnicodeString read _GetaccessKeyLabel;
    property draggable: Boolean read _Getdraggable write _Setdraggable;
    property contentEditable: UnicodeString read _GetcontentEditable write _SetcontentEditable;
    property isContentEditable: Boolean read _GetisContentEditable;
    property popover: UnicodeString read _Getpopover write _Setpopover;
    property spellcheck: Boolean read _Getspellcheck write _Setspellcheck;
    property inputMode: UnicodeString read _GetinputMode write _SetinputMode;
    property enterKeyHint: UnicodeString read _GetenterKeyHint write _SetenterKeyHint;
    property autocapitalize: UnicodeString read _Getautocapitalize write _Setautocapitalize;
    property autocorrect: Boolean read _Getautocorrect write _Setautocorrect;
    property nonce: UnicodeString read _Getnonce write _Setnonce;
    property offsetParent: IJSElement read _GetoffsetParent;
    property offsetTop: LongInt read _GetoffsetTop;
    property offsetLeft: LongInt read _GetoffsetLeft;
    property offsetWidth: LongInt read _GetoffsetWidth;
    property offsetHeight: LongInt read _GetoffsetHeight;
    property style: IJSCSSStyleDeclaration read _Getstyle;
  end;
  
  { --------------------------------------------------------------------
    TJSPointerEvent
    --------------------------------------------------------------------}
  
  IJSPointerEvent = interface(IJSMouseEvent)
    ['{7F83339C-7E97-30CD-B986-3FD153C09D32}']
    function _GetpointerId: LongInt; 
    function _Getwidth: Double; 
    function _Getheight: Double; 
    function _Getpressure: Single; 
    function _GettangentialPressure: Single; 
    function _GettiltX: LongInt; 
    function _GettiltY: LongInt; 
    function _Gettwist: LongInt; 
    function _GetaltitudeAngle: Double; 
    function _GetazimuthAngle: Double; 
    function _GetpointerType: UnicodeString; 
    function _GetisPrimary: Boolean; 
    function getCoalescedEvents: TJSPointerEventDynArray;
    function getPredictedEvents: TJSPointerEventDynArray;
    property pointerId: LongInt read _GetpointerId;
    property width: Double read _Getwidth;
    property height: Double read _Getheight;
    property pressure: Single read _Getpressure;
    property tangentialPressure: Single read _GettangentialPressure;
    property tiltX: LongInt read _GettiltX;
    property tiltY: LongInt read _GettiltY;
    property twist: LongInt read _Gettwist;
    property altitudeAngle: Double read _GetaltitudeAngle;
    property azimuthAngle: Double read _GetazimuthAngle;
    property pointerType: UnicodeString read _GetpointerType;
    property isPrimary: Boolean read _GetisPrimary;
  end;
  
  TJSPointerEvent = class(TJSMouseEvent,IJSPointerEvent)
  Private
  Protected
    function _GetpointerId: LongInt; 
    function _Getwidth: Double; 
    function _Getheight: Double; 
    function _Getpressure: Single; 
    function _GettangentialPressure: Single; 
    function _GettiltX: LongInt; 
    function _GettiltY: LongInt; 
    function _Gettwist: LongInt; 
    function _GetaltitudeAngle: Double; 
    function _GetazimuthAngle: Double; 
    function _GetpointerType: UnicodeString; 
    function _GetisPrimary: Boolean; 
  Public
    constructor Create(const aType_: UnicodeString; const aEventInitDict: IJSPointerEventInit); overload;
    constructor Create(const aType_: UnicodeString); overload;
    function getCoalescedEvents: TJSPointerEventDynArray; overload;
    function getPredictedEvents: TJSPointerEventDynArray; overload;
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSPointerEvent;
    property pointerId: LongInt read _GetpointerId;
    property width: Double read _Getwidth;
    property height: Double read _Getheight;
    property pressure: Single read _Getpressure;
    property tangentialPressure: Single read _GettangentialPressure;
    property tiltX: LongInt read _GettiltX;
    property tiltY: LongInt read _GettiltY;
    property twist: LongInt read _Gettwist;
    property altitudeAngle: Double read _GetaltitudeAngle;
    property azimuthAngle: Double read _GetazimuthAngle;
    property pointerType: UnicodeString read _GetpointerType;
    property isPrimary: Boolean read _GetisPrimary;
  end;
  
  { --------------------------------------------------------------------
    TJSHTMLCanvasElement
    --------------------------------------------------------------------}
  
  IJSHTMLCanvasElement = interface(IJSHTMLElement)
    ['{8198E070-ECFC-3633-8D4D-FC17330498A8}']
    function _Getwidth: Cardinal; 
    function _Getheight: Cardinal; 
    procedure _Setwidth(const aValue: Cardinal);
    procedure _Setheight(const aValue: Cardinal);
    function getContext(const aContextId: UnicodeString; const aContextOptions: Variant): IJSnsISupports;
    function getContext(const aContextId: UnicodeString): IJSnsISupports;
    function toDataURL(const aType_: UnicodeString; const aEncoderOptions: Variant): UnicodeString;
    function toDataURL: UnicodeString;
    function toDataURL(const aType_: UnicodeString): UnicodeString;
    property width: Cardinal read _Getwidth write _Setwidth;
    property height: Cardinal read _Getheight write _Setheight;
  end;
  
  TJSHTMLCanvasElement = class(TJSHTMLElement,IJSHTMLCanvasElement)
  Private
  Protected
    function _Getwidth: Cardinal; 
    function _Getheight: Cardinal; 
    procedure _Setwidth(const aValue: Cardinal);
    procedure _Setheight(const aValue: Cardinal);
  Public
    constructor Create; overload;
    function getContext(const aContextId: UnicodeString; const aContextOptions: Variant): IJSnsISupports; overload;
    function getContext(const aContextId: UnicodeString): IJSnsISupports; overload;
    function toDataURL(const aType_: UnicodeString; const aEncoderOptions: Variant): UnicodeString; overload;
    function toDataURL: UnicodeString; overload;
    function toDataURL(const aType_: UnicodeString): UnicodeString; overload;
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSHTMLCanvasElement;
    property width: Cardinal read _Getwidth write _Setwidth;
    property height: Cardinal read _Getheight write _Setheight;
  end;
  
  { --------------------------------------------------------------------
    TJSHTMLDivElement
    --------------------------------------------------------------------}
  
  IJSHTMLDivElement = interface(IJSHTMLElement)
    ['{9FA7005C-58D8-37E1-AB4F-CBD069275FBE}']
    function _Getalign: UnicodeString; 
    procedure _Setalign(const aValue: UnicodeString);
    property align: UnicodeString read _Getalign write _Setalign;
  end;
  
  TJSHTMLDivElement = class(TJSHTMLElement,IJSHTMLDivElement)
  Private
  Protected
    function _Getalign: UnicodeString; 
    procedure _Setalign(const aValue: UnicodeString);
  Public
    constructor Create; overload;
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSHTMLDivElement;
    property align: UnicodeString read _Getalign write _Setalign;
  end;
  
  { --------------------------------------------------------------------
    TJSHTMLImageElement
    --------------------------------------------------------------------}
  
  IJSHTMLImageElement = interface(IJSHTMLElement)
    ['{C3D04BC7-A826-3FA6-AB5E-F55DD6840CF2}']
    function _Getalt: UnicodeString; 
    function _Getsrc: UnicodeString; 
    function _Getsrcset: UnicodeString; 
    function _GetcrossOrigin: UnicodeString; 
    function _GetuseMap: UnicodeString; 
    function _GetreferrerPolicy: UnicodeString; 
    function _GetisMap: Boolean; 
    function _Getwidth: Cardinal; 
    function _Getheight: Cardinal; 
    function _Getdecoding: UnicodeString; 
    function _Getloading: UnicodeString; 
    function _GetfetchPriority: UnicodeString; 
    function _GetnaturalWidth: Cardinal; 
    function _GetnaturalHeight: Cardinal; 
    function _Getcomplete: Boolean; 
    function _Getname: UnicodeString; 
    function _Getalign: UnicodeString; 
    function _Gethspace: Cardinal; 
    function _Getvspace: Cardinal; 
    function _GetlongDesc: UnicodeString; 
    function _Getborder: UnicodeString; 
    function _Getsizes: UnicodeString; 
    function _GetcurrentSrc: UnicodeString; 
    function _Getlowsrc: UnicodeString; 
    function _Getx: LongInt; 
    function _Gety: LongInt; 
    procedure _Setalt(const aValue: UnicodeString);
    procedure _Setsrc(const aValue: UnicodeString);
    procedure _Setsrcset(const aValue: UnicodeString);
    procedure _SetcrossOrigin(const aValue: UnicodeString);
    procedure _SetuseMap(const aValue: UnicodeString);
    procedure _SetreferrerPolicy(const aValue: UnicodeString);
    procedure _SetisMap(const aValue: Boolean);
    procedure _Setwidth(const aValue: Cardinal);
    procedure _Setheight(const aValue: Cardinal);
    procedure _Setdecoding(const aValue: UnicodeString);
    procedure _Setloading(const aValue: UnicodeString);
    procedure _SetfetchPriority(const aValue: UnicodeString);
    procedure _Setname(const aValue: UnicodeString);
    procedure _Setalign(const aValue: UnicodeString);
    procedure _Sethspace(const aValue: Cardinal);
    procedure _Setvspace(const aValue: Cardinal);
    procedure _SetlongDesc(const aValue: UnicodeString);
    procedure _Setborder(const aValue: UnicodeString);
    procedure _Setsizes(const aValue: UnicodeString);
    procedure _Setlowsrc(const aValue: UnicodeString);
    function decode: IJSPromise; // Promise<undefined>
    property alt: UnicodeString read _Getalt write _Setalt;
    property src: UnicodeString read _Getsrc write _Setsrc;
    property srcset: UnicodeString read _Getsrcset write _Setsrcset;
    property crossOrigin: UnicodeString read _GetcrossOrigin write _SetcrossOrigin;
    property useMap: UnicodeString read _GetuseMap write _SetuseMap;
    property referrerPolicy: UnicodeString read _GetreferrerPolicy write _SetreferrerPolicy;
    property isMap: Boolean read _GetisMap write _SetisMap;
    property width: Cardinal read _Getwidth write _Setwidth;
    property height: Cardinal read _Getheight write _Setheight;
    property decoding: UnicodeString read _Getdecoding write _Setdecoding;
    property loading: UnicodeString read _Getloading write _Setloading;
    property fetchPriority: UnicodeString read _GetfetchPriority write _SetfetchPriority;
    property naturalWidth: Cardinal read _GetnaturalWidth;
    property naturalHeight: Cardinal read _GetnaturalHeight;
    property complete: Boolean read _Getcomplete;
    property name: UnicodeString read _Getname write _Setname;
    property align: UnicodeString read _Getalign write _Setalign;
    property hspace: Cardinal read _Gethspace write _Sethspace;
    property vspace: Cardinal read _Getvspace write _Setvspace;
    property longDesc: UnicodeString read _GetlongDesc write _SetlongDesc;
    property border: UnicodeString read _Getborder write _Setborder;
    property sizes: UnicodeString read _Getsizes write _Setsizes;
    property currentSrc: UnicodeString read _GetcurrentSrc;
    property lowsrc: UnicodeString read _Getlowsrc write _Setlowsrc;
    property x: LongInt read _Getx;
    property y: LongInt read _Gety;
  end;
  
  TJSHTMLImageElement = class(TJSHTMLElement,IJSHTMLImageElement)
  Private
  Protected
    function _Getalt: UnicodeString; 
    function _Getsrc: UnicodeString; 
    function _Getsrcset: UnicodeString; 
    function _GetcrossOrigin: UnicodeString; 
    function _GetuseMap: UnicodeString; 
    function _GetreferrerPolicy: UnicodeString; 
    function _GetisMap: Boolean; 
    function _Getwidth: Cardinal; 
    function _Getheight: Cardinal; 
    function _Getdecoding: UnicodeString; 
    function _Getloading: UnicodeString; 
    function _GetfetchPriority: UnicodeString; 
    function _GetnaturalWidth: Cardinal; 
    function _GetnaturalHeight: Cardinal; 
    function _Getcomplete: Boolean; 
    function _Getname: UnicodeString; 
    function _Getalign: UnicodeString; 
    function _Gethspace: Cardinal; 
    function _Getvspace: Cardinal; 
    function _GetlongDesc: UnicodeString; 
    function _Getborder: UnicodeString; 
    function _Getsizes: UnicodeString; 
    function _GetcurrentSrc: UnicodeString; 
    function _Getlowsrc: UnicodeString; 
    function _Getx: LongInt; 
    function _Gety: LongInt; 
    procedure _Setalt(const aValue: UnicodeString);
    procedure _Setsrc(const aValue: UnicodeString);
    procedure _Setsrcset(const aValue: UnicodeString);
    procedure _SetcrossOrigin(const aValue: UnicodeString);
    procedure _SetuseMap(const aValue: UnicodeString);
    procedure _SetreferrerPolicy(const aValue: UnicodeString);
    procedure _SetisMap(const aValue: Boolean);
    procedure _Setwidth(const aValue: Cardinal);
    procedure _Setheight(const aValue: Cardinal);
    procedure _Setdecoding(const aValue: UnicodeString);
    procedure _Setloading(const aValue: UnicodeString);
    procedure _SetfetchPriority(const aValue: UnicodeString);
    procedure _Setname(const aValue: UnicodeString);
    procedure _Setalign(const aValue: UnicodeString);
    procedure _Sethspace(const aValue: Cardinal);
    procedure _Setvspace(const aValue: Cardinal);
    procedure _SetlongDesc(const aValue: UnicodeString);
    procedure _Setborder(const aValue: UnicodeString);
    procedure _Setsizes(const aValue: UnicodeString);
    procedure _Setlowsrc(const aValue: UnicodeString);
  Public
    constructor Create; overload;
    function decode: IJSPromise; overload; // Promise<undefined>
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSHTMLImageElement;
    property alt: UnicodeString read _Getalt write _Setalt;
    property src: UnicodeString read _Getsrc write _Setsrc;
    property srcset: UnicodeString read _Getsrcset write _Setsrcset;
    property crossOrigin: UnicodeString read _GetcrossOrigin write _SetcrossOrigin;
    property useMap: UnicodeString read _GetuseMap write _SetuseMap;
    property referrerPolicy: UnicodeString read _GetreferrerPolicy write _SetreferrerPolicy;
    property isMap: Boolean read _GetisMap write _SetisMap;
    property width: Cardinal read _Getwidth write _Setwidth;
    property height: Cardinal read _Getheight write _Setheight;
    property decoding: UnicodeString read _Getdecoding write _Setdecoding;
    property loading: UnicodeString read _Getloading write _Setloading;
    property fetchPriority: UnicodeString read _GetfetchPriority write _SetfetchPriority;
    property naturalWidth: Cardinal read _GetnaturalWidth;
    property naturalHeight: Cardinal read _GetnaturalHeight;
    property complete: Boolean read _Getcomplete;
    property name: UnicodeString read _Getname write _Setname;
    property align: UnicodeString read _Getalign write _Setalign;
    property hspace: Cardinal read _Gethspace write _Sethspace;
    property vspace: Cardinal read _Getvspace write _Setvspace;
    property longDesc: UnicodeString read _GetlongDesc write _SetlongDesc;
    property border: UnicodeString read _Getborder write _Setborder;
    property sizes: UnicodeString read _Getsizes write _Setsizes;
    property currentSrc: UnicodeString read _GetcurrentSrc;
    property lowsrc: UnicodeString read _Getlowsrc write _Setlowsrc;
    property x: LongInt read _Getx;
    property y: LongInt read _Gety;
  end;
  
  { --------------------------------------------------------------------
    TJSHTMLMediaElement
    --------------------------------------------------------------------}
  
  IJSHTMLMediaElement = interface(IJSHTMLElement)
    ['{CAA22A57-B7D9-3C13-A83B-82C5363FF662}']
    function _Getsrc: UnicodeString; 
    function _GetcurrentSrc: UnicodeString; 
    function _GetcrossOrigin: UnicodeString; 
    function _GetnetworkState: Word; 
    function _Getpreload: UnicodeString; 
    procedure _Setsrc(const aValue: UnicodeString);
    procedure _SetcrossOrigin(const aValue: UnicodeString);
    procedure _Setpreload(const aValue: UnicodeString);
    property src: UnicodeString read _Getsrc write _Setsrc;
    property currentSrc: UnicodeString read _GetcurrentSrc;
    property crossOrigin: UnicodeString read _GetcrossOrigin write _SetcrossOrigin;
    property networkState: Word read _GetnetworkState;
    property preload: UnicodeString read _Getpreload write _Setpreload;
  end;
  
  TJSHTMLMediaElement = class(TJSHTMLElement,IJSHTMLMediaElement)
  Private
  Protected
    function _Getsrc: UnicodeString; 
    function _GetcurrentSrc: UnicodeString; 
    function _GetcrossOrigin: UnicodeString; 
    function _GetnetworkState: Word; 
    function _Getpreload: UnicodeString; 
    procedure _Setsrc(const aValue: UnicodeString);
    procedure _SetcrossOrigin(const aValue: UnicodeString);
    procedure _Setpreload(const aValue: UnicodeString);
  Public
    Const
      NETWORK_EMPTY = 0;
      NETWORK_IDLE = 1;
      NETWORK_LOADING = 2;
      NETWORK_NO_SOURCE = 3;
  Public
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSHTMLMediaElement;
    property src: UnicodeString read _Getsrc write _Setsrc;
    property currentSrc: UnicodeString read _GetcurrentSrc;
    property crossOrigin: UnicodeString read _GetcrossOrigin write _SetcrossOrigin;
    property networkState: Word read _GetnetworkState;
    property preload: UnicodeString read _Getpreload write _Setpreload;
  end;
  
  { --------------------------------------------------------------------
    TJSHTMLSpanElement
    --------------------------------------------------------------------}
  
  IJSHTMLSpanElement = interface(IJSHTMLElement)
    ['{AC6CC386-38ED-373F-AD48-9E5D8A5DBD72}']
  end;
  
  TJSHTMLSpanElement = class(TJSHTMLElement,IJSHTMLSpanElement)
  Private
  Protected
  Public
    constructor Create; overload;
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSHTMLSpanElement;
  end;
  
  { --------------------------------------------------------------------
    TJSHTMLVideoElement
    --------------------------------------------------------------------}
  
  IJSHTMLVideoElement = interface(IJSHTMLMediaElement)
    ['{180D6EF6-F2E3-386D-B8F2-BA97AA930008}']
    function _Getwidth: Cardinal; 
    function _Getheight: Cardinal; 
    function _GetvideoWidth: Cardinal; 
    function _GetvideoHeight: Cardinal; 
    function _Getposter: UnicodeString; 
    procedure _Setwidth(const aValue: Cardinal);
    procedure _Setheight(const aValue: Cardinal);
    procedure _Setposter(const aValue: UnicodeString);
    property width: Cardinal read _Getwidth write _Setwidth;
    property height: Cardinal read _Getheight write _Setheight;
    property videoWidth: Cardinal read _GetvideoWidth;
    property videoHeight: Cardinal read _GetvideoHeight;
    property poster: UnicodeString read _Getposter write _Setposter;
  end;
  
  TJSHTMLVideoElement = class(TJSHTMLMediaElement,IJSHTMLVideoElement)
  Private
  Protected
    function _Getwidth: Cardinal; 
    function _Getheight: Cardinal; 
    function _GetvideoWidth: Cardinal; 
    function _GetvideoHeight: Cardinal; 
    function _Getposter: UnicodeString; 
    procedure _Setwidth(const aValue: Cardinal);
    procedure _Setheight(const aValue: Cardinal);
    procedure _Setposter(const aValue: UnicodeString);
  Public
    constructor Create; overload;
    class function JSClassName: UnicodeString; override;
    class function Cast(const Intf: IJSObject): IJSHTMLVideoElement;
    property width: Cardinal read _Getwidth write _Setwidth;
    property height: Cardinal read _Getheight write _Setheight;
    property videoWidth: Cardinal read _GetvideoWidth;
    property videoHeight: Cardinal read _GetvideoHeight;
    property poster: UnicodeString read _Getposter write _Setposter;
  end;
var
  JSDocument: TJSDocument;
  JSWindow: TJSWindow;

implementation


function JOBCallFrameRequestCallback(const aMethod: TMethod; var H: TJOBCallbackHelper): PByte;
var
  time: TDOMHighResTimeStamp;
begin
  time:=H.GetDouble;
  TFrameRequestCallback(aMethod)(time);
  Result:=H.AllocUndefined;
end;

function JOBCallEventHandlerNonNull(const aMethod: TMethod; var H: TJOBCallbackHelper): PByte;
var
  event: IJSEvent;
begin
  event:=H.GetObject(TJSEvent) as IJSEvent;
  Result:=H.AllocVariant(TEventHandlerNonNull(aMethod)(event));
end;

function JOBCallOnBeforeUnloadEventHandlerNonNull(const aMethod: TMethod; var H: TJOBCallbackHelper): PByte;
var
  event: IJSEvent;
begin
  event:=H.GetObject(TJSEvent) as IJSEvent;
  Result:=H.AllocString(TOnBeforeUnloadEventHandlerNonNull(aMethod)(event));
end;

function JOBCallOnErrorEventHandlerNonNull(const aMethod: TMethod; var H: TJOBCallbackHelper): PByte;
var
  event: TOnErrorEventHandlerNonNull_event_Type;
  source: UTF8String;
  lineno: Cardinal;
  column: Cardinal;
  error: Variant;
begin
  event:=H.GetVariant;
  source:=H.GetString;
  lineno:=H.GetMaxInt;
  column:=H.GetMaxInt;
  error:=H.GetVariant;
  Result:=H.AllocVariant(TOnErrorEventHandlerNonNull(aMethod)(event,source,lineno,column,error));
end;

function JOBCallEventListener(const aMethod: TMethod; var H: TJOBCallbackHelper): PByte;
var
  event: IJSEvent;
begin
  event:=H.GetObject(TJSEvent) as IJSEvent;
  Result:=H.AllocBool(TEventListener(aMethod)(event));
end;

function TJSAnimationFrameProvider.requestAnimationFrame(const aCallback: TFrameRequestCallback): LongInt;
var
  m: TJOB_Method;
begin
  m:=TJOB_Method.Create(TMethod(aCallback),@JOBCallFrameRequestCallback);
  try
    Result:=InvokeJSLongIntResult('requestAnimationFrame',[m]);
  finally
    m.free;
  end;
end;

procedure TJSAnimationFrameProvider.cancelAnimationFrame(aHandle: LongInt);
begin
  InvokeJSNoResult('cancelAnimationFrame',[aHandle]);
end;

class function TJSAnimationFrameProvider.JSClassName: UnicodeString;
begin
  Result:='AnimationFrameProvider';
end;

class function TJSAnimationFrameProvider.Cast(const Intf: IJSObject): IJSAnimationFrameProvider;
begin
  Result:=TJSAnimationFrameProvider.JOBCast(Intf);
end;

function TJSCSSStyleDeclaration._GetcssText: UTF8String;
begin
  Result:=ReadJSPropertyUTF8String('cssText');
end;

function TJSCSSStyleDeclaration._Getlength_: Cardinal;
begin
  Result:=ReadJSPropertyInt64('length');
end;

procedure TJSCSSStyleDeclaration._SetcssText(const aValue : UTF8String);
begin
  WriteJSPropertyUTF8String('cssText',aValue);
end;

function TJSCSSStyleDeclaration.item(aIndex: Cardinal): UTF8String;
begin
  Result:=InvokeJSUTF8StringResult('item',[aIndex]);
end;

function TJSCSSStyleDeclaration.getPropertyValue(const aProperty_: UTF8String): UTF8String;
begin
  Result:=InvokeJSUTF8StringResult('getPropertyValue',[aProperty_]);
end;

function TJSCSSStyleDeclaration.getPropertyPriority(const aProperty_: UTF8String): UTF8String;
begin
  Result:=InvokeJSUTF8StringResult('getPropertyPriority',[aProperty_]);
end;

procedure TJSCSSStyleDeclaration.setProperty(const aProperty_: UTF8String; const aValue: UTF8String; const aPriority: UTF8String);
begin
  InvokeJSNoResult('setProperty',[aProperty_,aValue,aPriority]);
end;

procedure TJSCSSStyleDeclaration.setProperty(const aProperty_: UTF8String; const aValue: UTF8String);
begin
  InvokeJSNoResult('setProperty',[aProperty_,aValue]);
end;

function TJSCSSStyleDeclaration.removeProperty(const aProperty_: UTF8String): UTF8String;
begin
  Result:=InvokeJSUTF8StringResult('removeProperty',[aProperty_]);
end;

class function TJSCSSStyleDeclaration.JSClassName: UnicodeString;
begin
  Result:='CSSStyleDeclaration';
end;

class function TJSCSSStyleDeclaration.Cast(const Intf: IJSObject): IJSCSSStyleDeclaration;
begin
  Result:=TJSCSSStyleDeclaration.JOBCast(Intf);
end;

class function TJSContentSecurityPolicy.JSClassName: UnicodeString;
begin
  Result:='ContentSecurityPolicy';
end;

class function TJSContentSecurityPolicy.Cast(const Intf: IJSObject): IJSContentSecurityPolicy;
begin
  Result:=TJSContentSecurityPolicy.JOBCast(Intf);
end;

class function TJSPrincipal.JSClassName: UnicodeString;
begin
  Result:='Principal';
end;

class function TJSPrincipal.Cast(const Intf: IJSObject): IJSPrincipal;
begin
  Result:=TJSPrincipal.JOBCast(Intf);
end;

class function TJSWindowProxy.JSClassName: UnicodeString;
begin
  Result:='WindowProxy';
end;

class function TJSWindowProxy.Cast(const Intf: IJSObject): IJSWindowProxy;
begin
  Result:=TJSWindowProxy.JOBCast(Intf);
end;

class function TJSnsISupports.JSClassName: UnicodeString;
begin
  Result:='nsISupports';
end;

class function TJSnsISupports.Cast(const Intf: IJSObject): IJSnsISupports;
begin
  Result:=TJSnsISupports.JOBCast(Intf);
end;

class function TJSURI.JSClassName: UnicodeString;
begin
  Result:='URI';
end;

class function TJSURI.Cast(const Intf: IJSObject): IJSURI;
begin
  Result:=TJSURI.JOBCast(Intf);
end;

class function TJSnsIDocShell.JSClassName: UnicodeString;
begin
  Result:='nsIDocShell';
end;

class function TJSnsIDocShell.Cast(const Intf: IJSObject): IJSnsIDocShell;
begin
  Result:=TJSnsIDocShell.JOBCast(Intf);
end;

class function TJSnsILoadGroup.JSClassName: UnicodeString;
begin
  Result:='nsILoadGroup';
end;

class function TJSnsILoadGroup.Cast(const Intf: IJSObject): IJSnsILoadGroup;
begin
  Result:=TJSnsILoadGroup.JOBCast(Intf);
end;

class function TJSnsIReferrerInfo.JSClassName: UnicodeString;
begin
  Result:='nsIReferrerInfo';
end;

class function TJSnsIReferrerInfo.Cast(const Intf: IJSObject): IJSnsIReferrerInfo;
begin
  Result:=TJSnsIReferrerInfo.JOBCast(Intf);
end;

class function TJSnsICookieJarSettings.JSClassName: UnicodeString;
begin
  Result:='nsICookieJarSettings';
end;

class function TJSnsICookieJarSettings.Cast(const Intf: IJSObject): IJSnsICookieJarSettings;
begin
  Result:=TJSnsICookieJarSettings.JOBCast(Intf);
end;

class function TJSnsIPermissionDelegateHandler.JSClassName: UnicodeString;
begin
  Result:='nsIPermissionDelegateHandler';
end;

class function TJSnsIPermissionDelegateHandler.Cast(const Intf: IJSObject): IJSnsIPermissionDelegateHandler;
begin
  Result:=TJSnsIPermissionDelegateHandler.JOBCast(Intf);
end;

class function TJSXULCommandDispatcher.JSClassName: UnicodeString;
begin
  Result:='XULCommandDispatcher';
end;

class function TJSXULCommandDispatcher.Cast(const Intf: IJSObject): IJSXULCommandDispatcher;
begin
  Result:=TJSXULCommandDispatcher.JOBCast(Intf);
end;

function TJSDocument.getElementById(const aElementId: UnicodeString): IJSElement;
begin
  Result:=InvokeJSObjectResult('getElementById',[aElementId],TJSElement) as IJSElement;
end;

class function TJSDocument.JSClassName: UnicodeString;
begin
  Result:='Document';
end;

class function TJSDocument.Cast(const Intf: IJSObject): IJSDocument;
begin
  Result:=TJSDocument.JOBCast(Intf);
end;

function TJSDOMRect._Getx2: Double;
begin
  Result:=ReadJSPropertyDouble('x');
end;

function TJSDOMRect._Gety2: Double;
begin
  Result:=ReadJSPropertyDouble('y');
end;

function TJSDOMRect._Getwidth2: Double;
begin
  Result:=ReadJSPropertyDouble('width');
end;

function TJSDOMRect._Getheight2: Double;
begin
  Result:=ReadJSPropertyDouble('height');
end;

procedure TJSDOMRect._Setx2(const aValue : Double);
begin
  WriteJSPropertyDouble('x',aValue);
end;

procedure TJSDOMRect._Sety2(const aValue : Double);
begin
  WriteJSPropertyDouble('y',aValue);
end;

procedure TJSDOMRect._Setwidth2(const aValue : Double);
begin
  WriteJSPropertyDouble('width',aValue);
end;

procedure TJSDOMRect._Setheight2(const aValue : Double);
begin
  WriteJSPropertyDouble('height',aValue);
end;

constructor TJSDOMRect.Create(aX: Double; aY: Double; aWidth: Double; aHeight: Double);
begin
  JOBCreate([aX,aY,aWidth,aHeight]);
end;

constructor TJSDOMRect.Create;
begin
  JOBCreate([]);
end;

constructor TJSDOMRect.Create(aX: Double);
begin
  JOBCreate([aX]);
end;

constructor TJSDOMRect.Create(aX: Double; aY: Double);
begin
  JOBCreate([aX,aY]);
end;

constructor TJSDOMRect.Create(aX: Double; aY: Double; aWidth: Double);
begin
  JOBCreate([aX,aY,aWidth]);
end;

function TJSDOMRect.fromRect(const aOther: IJSDOMRectInit): IJSDOMRect;
begin
  Result:=InvokeJSObjectResult('fromRect',[aOther],TJSDOMRect) as IJSDOMRect;
end;

function TJSDOMRect.fromRect: IJSDOMRect;
begin
  Result:=InvokeJSObjectResult('fromRect',[],TJSDOMRect) as IJSDOMRect;
end;

class function TJSDOMRect.JSClassName: UnicodeString;
begin
  Result:='DOMRect';
end;

class function TJSDOMRect.Cast(const Intf: IJSObject): IJSDOMRect;
begin
  Result:=TJSDOMRect.JOBCast(Intf);
end;

function TJSDOMRectReadOnly._Getx: Double;
begin
  Result:=ReadJSPropertyDouble('x');
end;

function TJSDOMRectReadOnly._Gety: Double;
begin
  Result:=ReadJSPropertyDouble('y');
end;

function TJSDOMRectReadOnly._Getwidth: Double;
begin
  Result:=ReadJSPropertyDouble('width');
end;

function TJSDOMRectReadOnly._Getheight: Double;
begin
  Result:=ReadJSPropertyDouble('height');
end;

function TJSDOMRectReadOnly._Gettop: Double;
begin
  Result:=ReadJSPropertyDouble('top');
end;

function TJSDOMRectReadOnly._Getright: Double;
begin
  Result:=ReadJSPropertyDouble('right');
end;

function TJSDOMRectReadOnly._Getbottom: Double;
begin
  Result:=ReadJSPropertyDouble('bottom');
end;

function TJSDOMRectReadOnly._Getleft: Double;
begin
  Result:=ReadJSPropertyDouble('left');
end;

constructor TJSDOMRectReadOnly.Create(aX: Double; aY: Double; aWidth: Double; aHeight: Double);
begin
  JOBCreate([aX,aY,aWidth,aHeight]);
end;

constructor TJSDOMRectReadOnly.Create;
begin
  JOBCreate([]);
end;

constructor TJSDOMRectReadOnly.Create(aX: Double);
begin
  JOBCreate([aX]);
end;

constructor TJSDOMRectReadOnly.Create(aX: Double; aY: Double);
begin
  JOBCreate([aX,aY]);
end;

constructor TJSDOMRectReadOnly.Create(aX: Double; aY: Double; aWidth: Double);
begin
  JOBCreate([aX,aY,aWidth]);
end;

function TJSDOMRectReadOnly.fromRect(const aOther: IJSDOMRectInit): IJSDOMRectReadOnly;
begin
  Result:=InvokeJSObjectResult('fromRect',[aOther],TJSDOMRectReadOnly) as IJSDOMRectReadOnly;
end;

function TJSDOMRectReadOnly.fromRect: IJSDOMRectReadOnly;
begin
  Result:=InvokeJSObjectResult('fromRect',[],TJSDOMRectReadOnly) as IJSDOMRectReadOnly;
end;

function TJSDOMRectReadOnly.toJSON: IJSObject;
begin
  Result:=InvokeJSObjectResult('toJSON',[],TJSObject) as IJSObject;
end;

class function TJSDOMRectReadOnly.JSClassName: UnicodeString;
begin
  Result:='DOMRectReadOnly';
end;

class function TJSDOMRectReadOnly.Cast(const Intf: IJSObject): IJSDOMRectReadOnly;
begin
  Result:=TJSDOMRectReadOnly.JOBCast(Intf);
end;

function TJSDOMRectInit._Getx: Double;
begin
  Result:=ReadJSPropertyDouble('x');
end;

function TJSDOMRectInit._Gety: Double;
begin
  Result:=ReadJSPropertyDouble('y');
end;

function TJSDOMRectInit._Getwidth: Double;
begin
  Result:=ReadJSPropertyDouble('width');
end;

function TJSDOMRectInit._Getheight: Double;
begin
  Result:=ReadJSPropertyDouble('height');
end;

procedure TJSDOMRectInit._Setx(const aValue : Double);
begin
  WriteJSPropertyDouble('x',aValue);
end;

procedure TJSDOMRectInit._Sety(const aValue : Double);
begin
  WriteJSPropertyDouble('y',aValue);
end;

procedure TJSDOMRectInit._Setwidth(const aValue : Double);
begin
  WriteJSPropertyDouble('width',aValue);
end;

procedure TJSDOMRectInit._Setheight(const aValue : Double);
begin
  WriteJSPropertyDouble('height',aValue);
end;

constructor TJSDOMRectInit.create(const aDict : TJSDOMRectInitRec); overload;
begin
  Self.x:=aDict.x;
  Self.y:=aDict.y;
  Self.width:=aDict.width;
  Self.height:=aDict.height;
end;

class function TJSDOMRectInit.JSClassName: UnicodeString;
begin
  Result:='Object';
end;

class function TJSDOMRectInit.Cast(const Intf: IJSObject): IJSDOMRectInit;
begin
  Result:=TJSDOMRectInit.JOBCast(Intf);
end;

class function TJSnsIScreen.JSClassName: UnicodeString;
begin
  Result:='nsIScreen';
end;

class function TJSnsIScreen.Cast(const Intf: IJSObject): IJSnsIScreen;
begin
  Result:=TJSnsIScreen.JOBCast(Intf);
end;

function TJSElement._GetnamespaceURI: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('namespaceURI');
end;

function TJSElement._Getprefix: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('prefix');
end;

function TJSElement._GetlocalName: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('localName');
end;

function TJSElement._GettagName: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('tagName');
end;

function TJSElement._Getid: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('id');
end;

function TJSElement._GetclassName_: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('className');
end;

function TJSElement._GetclientTop: LongInt;
begin
  Result:=ReadJSPropertyLongInt('clientTop');
end;

function TJSElement._GetclientLeft: LongInt;
begin
  Result:=ReadJSPropertyLongInt('clientLeft');
end;

function TJSElement._GetclientWidth: LongInt;
begin
  Result:=ReadJSPropertyLongInt('clientWidth');
end;

function TJSElement._GetclientHeight: LongInt;
begin
  Result:=ReadJSPropertyLongInt('clientHeight');
end;

function TJSElement._GetcurrentCSSZoom: Double;
begin
  Result:=ReadJSPropertyDouble('currentCSSZoom');
end;

function TJSElement._GetinnerHTML: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('innerHTML');
end;

function TJSElement._GetouterHTML: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('outerHTML');
end;

procedure TJSElement._Setid(const aValue : UnicodeString);
begin
  WriteJSPropertyUnicodeString('id',aValue);
end;

procedure TJSElement._SetclassName_(const aValue : UnicodeString);
begin
  WriteJSPropertyUnicodeString('className',aValue);
end;

procedure TJSElement._SetinnerHTML(const aValue : UnicodeString);
begin
  WriteJSPropertyUnicodeString('innerHTML',aValue);
end;

procedure TJSElement._SetouterHTML(const aValue : UnicodeString);
begin
  WriteJSPropertyUnicodeString('outerHTML',aValue);
end;

function TJSElement.getBoundingClientRect: IJSDOMRect;
begin
  Result:=InvokeJSObjectResult('getBoundingClientRect',[],TJSDOMRect) as IJSDOMRect;
end;

procedure TJSElement.insertAdjacentHTML(const aPosition: UnicodeString; const aText: UnicodeString);
begin
  InvokeJSNoResult('insertAdjacentHTML',[aPosition,aText]);
end;

class function TJSElement.JSClassName: UnicodeString;
begin
  Result:='Element';
end;

class function TJSElement.Cast(const Intf: IJSObject): IJSElement;
begin
  Result:=TJSElement.JOBCast(Intf);
end;

function TJSElementCSSInlineStyle._Getstyle: IJSCSSStyleDeclaration;
begin
  Result:=ReadJSPropertyObject('style',TJSCSSStyleDeclaration) as IJSCSSStyleDeclaration;
end;

class function TJSElementCSSInlineStyle.JSClassName: UnicodeString;
begin
  Result:='ElementCSSInlineStyle';
end;

class function TJSElementCSSInlineStyle.Cast(const Intf: IJSObject): IJSElementCSSInlineStyle;
begin
  Result:=TJSElementCSSInlineStyle.JOBCast(Intf);
end;

function TJSGlobalEventHandlers._Getonabort: TEventHandler;
begin
  Result:=(TEventHandlerNonNull(ReadJSPropertyMethod('onabort')));
end;

function TJSGlobalEventHandlers._Getonblur: TEventHandler;
begin
  Result:=(TEventHandlerNonNull(ReadJSPropertyMethod('onblur')));
end;

function TJSGlobalEventHandlers._Getonfocus: TEventHandler;
begin
  Result:=(TEventHandlerNonNull(ReadJSPropertyMethod('onfocus')));
end;

function TJSGlobalEventHandlers._Getoncancel: TEventHandler;
begin
  Result:=(TEventHandlerNonNull(ReadJSPropertyMethod('oncancel')));
end;

function TJSGlobalEventHandlers._Getonauxclick: TEventHandler;
begin
  Result:=(TEventHandlerNonNull(ReadJSPropertyMethod('onauxclick')));
end;

function TJSGlobalEventHandlers._Getonbeforeinput: TEventHandler;
begin
  Result:=(TEventHandlerNonNull(ReadJSPropertyMethod('onbeforeinput')));
end;

function TJSGlobalEventHandlers._Getonbeforetoggle: TEventHandler;
begin
  Result:=(TEventHandlerNonNull(ReadJSPropertyMethod('onbeforetoggle')));
end;

function TJSGlobalEventHandlers._Getoncanplay: TEventHandler;
begin
  Result:=(TEventHandlerNonNull(ReadJSPropertyMethod('oncanplay')));
end;

function TJSGlobalEventHandlers._Getoncanplaythrough: TEventHandler;
begin
  Result:=(TEventHandlerNonNull(ReadJSPropertyMethod('oncanplaythrough')));
end;

function TJSGlobalEventHandlers._Getonchange: TEventHandler;
begin
  Result:=(TEventHandlerNonNull(ReadJSPropertyMethod('onchange')));
end;

function TJSGlobalEventHandlers._Getonclick: TEventHandler;
begin
  Result:=(TEventHandlerNonNull(ReadJSPropertyMethod('onclick')));
end;

function TJSGlobalEventHandlers._Getonclose: TEventHandler;
begin
  Result:=(TEventHandlerNonNull(ReadJSPropertyMethod('onclose')));
end;

function TJSGlobalEventHandlers._Getoncontentvisibilityautostatechange: TEventHandler;
begin
  Result:=(TEventHandlerNonNull(ReadJSPropertyMethod('oncontentvisibilityautostatechange')));
end;

function TJSGlobalEventHandlers._Getoncontextlost: TEventHandler;
begin
  Result:=(TEventHandlerNonNull(ReadJSPropertyMethod('oncontextlost')));
end;

function TJSGlobalEventHandlers._Getoncontextmenu: TEventHandler;
begin
  Result:=(TEventHandlerNonNull(ReadJSPropertyMethod('oncontextmenu')));
end;

function TJSGlobalEventHandlers._Getoncontextrestored: TEventHandler;
begin
  Result:=(TEventHandlerNonNull(ReadJSPropertyMethod('oncontextrestored')));
end;

function TJSGlobalEventHandlers._Getoncopy: TEventHandler;
begin
  Result:=(TEventHandlerNonNull(ReadJSPropertyMethod('oncopy')));
end;

function TJSGlobalEventHandlers._Getoncuechange: TEventHandler;
begin
  Result:=(TEventHandlerNonNull(ReadJSPropertyMethod('oncuechange')));
end;

function TJSGlobalEventHandlers._Getoncut: TEventHandler;
begin
  Result:=(TEventHandlerNonNull(ReadJSPropertyMethod('oncut')));
end;

function TJSGlobalEventHandlers._Getondblclick: TEventHandler;
begin
  Result:=(TEventHandlerNonNull(ReadJSPropertyMethod('ondblclick')));
end;

function TJSGlobalEventHandlers._Getondrag: TEventHandler;
begin
  Result:=(TEventHandlerNonNull(ReadJSPropertyMethod('ondrag')));
end;

function TJSGlobalEventHandlers._Getondragend: TEventHandler;
begin
  Result:=(TEventHandlerNonNull(ReadJSPropertyMethod('ondragend')));
end;

function TJSGlobalEventHandlers._Getondragenter: TEventHandler;
begin
  Result:=(TEventHandlerNonNull(ReadJSPropertyMethod('ondragenter')));
end;

function TJSGlobalEventHandlers._Getondragexit: TEventHandler;
begin
  Result:=(TEventHandlerNonNull(ReadJSPropertyMethod('ondragexit')));
end;

function TJSGlobalEventHandlers._Getondragleave: TEventHandler;
begin
  Result:=(TEventHandlerNonNull(ReadJSPropertyMethod('ondragleave')));
end;

function TJSGlobalEventHandlers._Getondragover: TEventHandler;
begin
  Result:=(TEventHandlerNonNull(ReadJSPropertyMethod('ondragover')));
end;

function TJSGlobalEventHandlers._Getondragstart: TEventHandler;
begin
  Result:=(TEventHandlerNonNull(ReadJSPropertyMethod('ondragstart')));
end;

function TJSGlobalEventHandlers._Getondrop: TEventHandler;
begin
  Result:=(TEventHandlerNonNull(ReadJSPropertyMethod('ondrop')));
end;

function TJSGlobalEventHandlers._Getondurationchange: TEventHandler;
begin
  Result:=(TEventHandlerNonNull(ReadJSPropertyMethod('ondurationchange')));
end;

function TJSGlobalEventHandlers._Getonemptied: TEventHandler;
begin
  Result:=(TEventHandlerNonNull(ReadJSPropertyMethod('onemptied')));
end;

function TJSGlobalEventHandlers._Getonended: TEventHandler;
begin
  Result:=(TEventHandlerNonNull(ReadJSPropertyMethod('onended')));
end;

function TJSGlobalEventHandlers._Getonformdata: TEventHandler;
begin
  Result:=(TEventHandlerNonNull(ReadJSPropertyMethod('onformdata')));
end;

function TJSGlobalEventHandlers._Getoninput: TEventHandler;
begin
  Result:=(TEventHandlerNonNull(ReadJSPropertyMethod('oninput')));
end;

function TJSGlobalEventHandlers._Getoninvalid: TEventHandler;
begin
  Result:=(TEventHandlerNonNull(ReadJSPropertyMethod('oninvalid')));
end;

function TJSGlobalEventHandlers._Getonkeydown: TEventHandler;
begin
  Result:=(TEventHandlerNonNull(ReadJSPropertyMethod('onkeydown')));
end;

function TJSGlobalEventHandlers._Getonkeypress: TEventHandler;
begin
  Result:=(TEventHandlerNonNull(ReadJSPropertyMethod('onkeypress')));
end;

function TJSGlobalEventHandlers._Getonkeyup: TEventHandler;
begin
  Result:=(TEventHandlerNonNull(ReadJSPropertyMethod('onkeyup')));
end;

function TJSGlobalEventHandlers._Getonload: TEventHandler;
begin
  Result:=(TEventHandlerNonNull(ReadJSPropertyMethod('onload')));
end;

function TJSGlobalEventHandlers._Getonloadeddata: TEventHandler;
begin
  Result:=(TEventHandlerNonNull(ReadJSPropertyMethod('onloadeddata')));
end;

function TJSGlobalEventHandlers._Getonloadedmetadata: TEventHandler;
begin
  Result:=(TEventHandlerNonNull(ReadJSPropertyMethod('onloadedmetadata')));
end;

function TJSGlobalEventHandlers._Getonloadstart: TEventHandler;
begin
  Result:=(TEventHandlerNonNull(ReadJSPropertyMethod('onloadstart')));
end;

function TJSGlobalEventHandlers._Getonmousedown: TEventHandler;
begin
  Result:=(TEventHandlerNonNull(ReadJSPropertyMethod('onmousedown')));
end;

function TJSGlobalEventHandlers._Getonmouseenter: TEventHandler;
begin
  Result:=(TEventHandlerNonNull(ReadJSPropertyMethod('onmouseenter')));
end;

function TJSGlobalEventHandlers._Getonmouseleave: TEventHandler;
begin
  Result:=(TEventHandlerNonNull(ReadJSPropertyMethod('onmouseleave')));
end;

function TJSGlobalEventHandlers._Getonmousemove: TEventHandler;
begin
  Result:=(TEventHandlerNonNull(ReadJSPropertyMethod('onmousemove')));
end;

function TJSGlobalEventHandlers._Getonmouseout: TEventHandler;
begin
  Result:=(TEventHandlerNonNull(ReadJSPropertyMethod('onmouseout')));
end;

function TJSGlobalEventHandlers._Getonmouseover: TEventHandler;
begin
  Result:=(TEventHandlerNonNull(ReadJSPropertyMethod('onmouseover')));
end;

function TJSGlobalEventHandlers._Getonmouseup: TEventHandler;
begin
  Result:=(TEventHandlerNonNull(ReadJSPropertyMethod('onmouseup')));
end;

function TJSGlobalEventHandlers._Getonwheel: TEventHandler;
begin
  Result:=(TEventHandlerNonNull(ReadJSPropertyMethod('onwheel')));
end;

function TJSGlobalEventHandlers._Getonpaste: TEventHandler;
begin
  Result:=(TEventHandlerNonNull(ReadJSPropertyMethod('onpaste')));
end;

function TJSGlobalEventHandlers._Getonpause: TEventHandler;
begin
  Result:=(TEventHandlerNonNull(ReadJSPropertyMethod('onpause')));
end;

function TJSGlobalEventHandlers._Getonplay: TEventHandler;
begin
  Result:=(TEventHandlerNonNull(ReadJSPropertyMethod('onplay')));
end;

function TJSGlobalEventHandlers._Getonplaying: TEventHandler;
begin
  Result:=(TEventHandlerNonNull(ReadJSPropertyMethod('onplaying')));
end;

function TJSGlobalEventHandlers._Getonprogress: TEventHandler;
begin
  Result:=(TEventHandlerNonNull(ReadJSPropertyMethod('onprogress')));
end;

function TJSGlobalEventHandlers._Getonratechange: TEventHandler;
begin
  Result:=(TEventHandlerNonNull(ReadJSPropertyMethod('onratechange')));
end;

function TJSGlobalEventHandlers._Getonreset: TEventHandler;
begin
  Result:=(TEventHandlerNonNull(ReadJSPropertyMethod('onreset')));
end;

function TJSGlobalEventHandlers._Getonresize: TEventHandler;
begin
  Result:=(TEventHandlerNonNull(ReadJSPropertyMethod('onresize')));
end;

function TJSGlobalEventHandlers._Getonscroll: TEventHandler;
begin
  Result:=(TEventHandlerNonNull(ReadJSPropertyMethod('onscroll')));
end;

function TJSGlobalEventHandlers._Getonscrollend: TEventHandler;
begin
  Result:=(TEventHandlerNonNull(ReadJSPropertyMethod('onscrollend')));
end;

function TJSGlobalEventHandlers._Getonsecuritypolicyviolation: TEventHandler;
begin
  Result:=(TEventHandlerNonNull(ReadJSPropertyMethod('onsecuritypolicyviolation')));
end;

function TJSGlobalEventHandlers._Getonseeked: TEventHandler;
begin
  Result:=(TEventHandlerNonNull(ReadJSPropertyMethod('onseeked')));
end;

function TJSGlobalEventHandlers._Getonseeking: TEventHandler;
begin
  Result:=(TEventHandlerNonNull(ReadJSPropertyMethod('onseeking')));
end;

function TJSGlobalEventHandlers._Getonselect: TEventHandler;
begin
  Result:=(TEventHandlerNonNull(ReadJSPropertyMethod('onselect')));
end;

function TJSGlobalEventHandlers._Getonslotchange: TEventHandler;
begin
  Result:=(TEventHandlerNonNull(ReadJSPropertyMethod('onslotchange')));
end;

function TJSGlobalEventHandlers._Getonstalled: TEventHandler;
begin
  Result:=(TEventHandlerNonNull(ReadJSPropertyMethod('onstalled')));
end;

function TJSGlobalEventHandlers._Getonsubmit: TEventHandler;
begin
  Result:=(TEventHandlerNonNull(ReadJSPropertyMethod('onsubmit')));
end;

function TJSGlobalEventHandlers._Getonsuspend: TEventHandler;
begin
  Result:=(TEventHandlerNonNull(ReadJSPropertyMethod('onsuspend')));
end;

function TJSGlobalEventHandlers._Getontimeupdate: TEventHandler;
begin
  Result:=(TEventHandlerNonNull(ReadJSPropertyMethod('ontimeupdate')));
end;

function TJSGlobalEventHandlers._Getonvolumechange: TEventHandler;
begin
  Result:=(TEventHandlerNonNull(ReadJSPropertyMethod('onvolumechange')));
end;

function TJSGlobalEventHandlers._Getonwaiting: TEventHandler;
begin
  Result:=(TEventHandlerNonNull(ReadJSPropertyMethod('onwaiting')));
end;

function TJSGlobalEventHandlers._Getonselectstart: TEventHandler;
begin
  Result:=(TEventHandlerNonNull(ReadJSPropertyMethod('onselectstart')));
end;

function TJSGlobalEventHandlers._Getonselectionchange: TEventHandler;
begin
  Result:=(TEventHandlerNonNull(ReadJSPropertyMethod('onselectionchange')));
end;

function TJSGlobalEventHandlers._Getontoggle: TEventHandler;
begin
  Result:=(TEventHandlerNonNull(ReadJSPropertyMethod('ontoggle')));
end;

function TJSGlobalEventHandlers._Getonpointercancel: TEventHandler;
begin
  Result:=(TEventHandlerNonNull(ReadJSPropertyMethod('onpointercancel')));
end;

function TJSGlobalEventHandlers._Getonpointerdown: TEventHandler;
begin
  Result:=(TEventHandlerNonNull(ReadJSPropertyMethod('onpointerdown')));
end;

function TJSGlobalEventHandlers._Getonpointerup: TEventHandler;
begin
  Result:=(TEventHandlerNonNull(ReadJSPropertyMethod('onpointerup')));
end;

function TJSGlobalEventHandlers._Getonpointermove: TEventHandler;
begin
  Result:=(TEventHandlerNonNull(ReadJSPropertyMethod('onpointermove')));
end;

function TJSGlobalEventHandlers._Getonpointerout: TEventHandler;
begin
  Result:=(TEventHandlerNonNull(ReadJSPropertyMethod('onpointerout')));
end;

function TJSGlobalEventHandlers._Getonpointerover: TEventHandler;
begin
  Result:=(TEventHandlerNonNull(ReadJSPropertyMethod('onpointerover')));
end;

function TJSGlobalEventHandlers._Getonpointerenter: TEventHandler;
begin
  Result:=(TEventHandlerNonNull(ReadJSPropertyMethod('onpointerenter')));
end;

function TJSGlobalEventHandlers._Getonpointerleave: TEventHandler;
begin
  Result:=(TEventHandlerNonNull(ReadJSPropertyMethod('onpointerleave')));
end;

function TJSGlobalEventHandlers._Getongotpointercapture: TEventHandler;
begin
  Result:=(TEventHandlerNonNull(ReadJSPropertyMethod('ongotpointercapture')));
end;

function TJSGlobalEventHandlers._Getonlostpointercapture: TEventHandler;
begin
  Result:=(TEventHandlerNonNull(ReadJSPropertyMethod('onlostpointercapture')));
end;

function TJSGlobalEventHandlers._Getonmozfullscreenchange: TEventHandler;
begin
  Result:=(TEventHandlerNonNull(ReadJSPropertyMethod('onmozfullscreenchange')));
end;

function TJSGlobalEventHandlers._Getonmozfullscreenerror: TEventHandler;
begin
  Result:=(TEventHandlerNonNull(ReadJSPropertyMethod('onmozfullscreenerror')));
end;

function TJSGlobalEventHandlers._Getonanimationcancel: TEventHandler;
begin
  Result:=(TEventHandlerNonNull(ReadJSPropertyMethod('onanimationcancel')));
end;

function TJSGlobalEventHandlers._Getonanimationend: TEventHandler;
begin
  Result:=(TEventHandlerNonNull(ReadJSPropertyMethod('onanimationend')));
end;

function TJSGlobalEventHandlers._Getonanimationiteration: TEventHandler;
begin
  Result:=(TEventHandlerNonNull(ReadJSPropertyMethod('onanimationiteration')));
end;

function TJSGlobalEventHandlers._Getonanimationstart: TEventHandler;
begin
  Result:=(TEventHandlerNonNull(ReadJSPropertyMethod('onanimationstart')));
end;

function TJSGlobalEventHandlers._Getontransitioncancel: TEventHandler;
begin
  Result:=(TEventHandlerNonNull(ReadJSPropertyMethod('ontransitioncancel')));
end;

function TJSGlobalEventHandlers._Getontransitionend: TEventHandler;
begin
  Result:=(TEventHandlerNonNull(ReadJSPropertyMethod('ontransitionend')));
end;

function TJSGlobalEventHandlers._Getontransitionrun: TEventHandler;
begin
  Result:=(TEventHandlerNonNull(ReadJSPropertyMethod('ontransitionrun')));
end;

function TJSGlobalEventHandlers._Getontransitionstart: TEventHandler;
begin
  Result:=(TEventHandlerNonNull(ReadJSPropertyMethod('ontransitionstart')));
end;

function TJSGlobalEventHandlers._Getonwebkitanimationend: TEventHandler;
begin
  Result:=(TEventHandlerNonNull(ReadJSPropertyMethod('onwebkitanimationend')));
end;

function TJSGlobalEventHandlers._Getonwebkitanimationiteration: TEventHandler;
begin
  Result:=(TEventHandlerNonNull(ReadJSPropertyMethod('onwebkitanimationiteration')));
end;

function TJSGlobalEventHandlers._Getonwebkitanimationstart: TEventHandler;
begin
  Result:=(TEventHandlerNonNull(ReadJSPropertyMethod('onwebkitanimationstart')));
end;

function TJSGlobalEventHandlers._Getonwebkittransitionend: TEventHandler;
begin
  Result:=(TEventHandlerNonNull(ReadJSPropertyMethod('onwebkittransitionend')));
end;

procedure TJSGlobalEventHandlers._Setonabort(const aValue : TEventHandler);
var
  m : TJOB_Method;
begin
  m:=TJOB_Method.create(TMethod(aValue),@JobCallEventHandlerNonNull);
  try
    InvokeJSNoResult('onabort',[m],jiSet);
  finally
    m.free
  end;
end;

procedure TJSGlobalEventHandlers._Setonblur(const aValue : TEventHandler);
var
  m : TJOB_Method;
begin
  m:=TJOB_Method.create(TMethod(aValue),@JobCallEventHandlerNonNull);
  try
    InvokeJSNoResult('onblur',[m],jiSet);
  finally
    m.free
  end;
end;

procedure TJSGlobalEventHandlers._Setonfocus(const aValue : TEventHandler);
var
  m : TJOB_Method;
begin
  m:=TJOB_Method.create(TMethod(aValue),@JobCallEventHandlerNonNull);
  try
    InvokeJSNoResult('onfocus',[m],jiSet);
  finally
    m.free
  end;
end;

procedure TJSGlobalEventHandlers._Setoncancel(const aValue : TEventHandler);
var
  m : TJOB_Method;
begin
  m:=TJOB_Method.create(TMethod(aValue),@JobCallEventHandlerNonNull);
  try
    InvokeJSNoResult('oncancel',[m],jiSet);
  finally
    m.free
  end;
end;

procedure TJSGlobalEventHandlers._Setonauxclick(const aValue : TEventHandler);
var
  m : TJOB_Method;
begin
  m:=TJOB_Method.create(TMethod(aValue),@JobCallEventHandlerNonNull);
  try
    InvokeJSNoResult('onauxclick',[m],jiSet);
  finally
    m.free
  end;
end;

procedure TJSGlobalEventHandlers._Setonbeforeinput(const aValue : TEventHandler);
var
  m : TJOB_Method;
begin
  m:=TJOB_Method.create(TMethod(aValue),@JobCallEventHandlerNonNull);
  try
    InvokeJSNoResult('onbeforeinput',[m],jiSet);
  finally
    m.free
  end;
end;

procedure TJSGlobalEventHandlers._Setonbeforetoggle(const aValue : TEventHandler);
var
  m : TJOB_Method;
begin
  m:=TJOB_Method.create(TMethod(aValue),@JobCallEventHandlerNonNull);
  try
    InvokeJSNoResult('onbeforetoggle',[m],jiSet);
  finally
    m.free
  end;
end;

procedure TJSGlobalEventHandlers._Setoncanplay(const aValue : TEventHandler);
var
  m : TJOB_Method;
begin
  m:=TJOB_Method.create(TMethod(aValue),@JobCallEventHandlerNonNull);
  try
    InvokeJSNoResult('oncanplay',[m],jiSet);
  finally
    m.free
  end;
end;

procedure TJSGlobalEventHandlers._Setoncanplaythrough(const aValue : TEventHandler);
var
  m : TJOB_Method;
begin
  m:=TJOB_Method.create(TMethod(aValue),@JobCallEventHandlerNonNull);
  try
    InvokeJSNoResult('oncanplaythrough',[m],jiSet);
  finally
    m.free
  end;
end;

procedure TJSGlobalEventHandlers._Setonchange(const aValue : TEventHandler);
var
  m : TJOB_Method;
begin
  m:=TJOB_Method.create(TMethod(aValue),@JobCallEventHandlerNonNull);
  try
    InvokeJSNoResult('onchange',[m],jiSet);
  finally
    m.free
  end;
end;

procedure TJSGlobalEventHandlers._Setonclick(const aValue : TEventHandler);
var
  m : TJOB_Method;
begin
  m:=TJOB_Method.create(TMethod(aValue),@JobCallEventHandlerNonNull);
  try
    InvokeJSNoResult('onclick',[m],jiSet);
  finally
    m.free
  end;
end;

procedure TJSGlobalEventHandlers._Setonclose(const aValue : TEventHandler);
var
  m : TJOB_Method;
begin
  m:=TJOB_Method.create(TMethod(aValue),@JobCallEventHandlerNonNull);
  try
    InvokeJSNoResult('onclose',[m],jiSet);
  finally
    m.free
  end;
end;

procedure TJSGlobalEventHandlers._Setoncontentvisibilityautostatechange(const aValue : TEventHandler);
var
  m : TJOB_Method;
begin
  m:=TJOB_Method.create(TMethod(aValue),@JobCallEventHandlerNonNull);
  try
    InvokeJSNoResult('oncontentvisibilityautostatechange',[m],jiSet);
  finally
    m.free
  end;
end;

procedure TJSGlobalEventHandlers._Setoncontextlost(const aValue : TEventHandler);
var
  m : TJOB_Method;
begin
  m:=TJOB_Method.create(TMethod(aValue),@JobCallEventHandlerNonNull);
  try
    InvokeJSNoResult('oncontextlost',[m],jiSet);
  finally
    m.free
  end;
end;

procedure TJSGlobalEventHandlers._Setoncontextmenu(const aValue : TEventHandler);
var
  m : TJOB_Method;
begin
  m:=TJOB_Method.create(TMethod(aValue),@JobCallEventHandlerNonNull);
  try
    InvokeJSNoResult('oncontextmenu',[m],jiSet);
  finally
    m.free
  end;
end;

procedure TJSGlobalEventHandlers._Setoncontextrestored(const aValue : TEventHandler);
var
  m : TJOB_Method;
begin
  m:=TJOB_Method.create(TMethod(aValue),@JobCallEventHandlerNonNull);
  try
    InvokeJSNoResult('oncontextrestored',[m],jiSet);
  finally
    m.free
  end;
end;

procedure TJSGlobalEventHandlers._Setoncopy(const aValue : TEventHandler);
var
  m : TJOB_Method;
begin
  m:=TJOB_Method.create(TMethod(aValue),@JobCallEventHandlerNonNull);
  try
    InvokeJSNoResult('oncopy',[m],jiSet);
  finally
    m.free
  end;
end;

procedure TJSGlobalEventHandlers._Setoncuechange(const aValue : TEventHandler);
var
  m : TJOB_Method;
begin
  m:=TJOB_Method.create(TMethod(aValue),@JobCallEventHandlerNonNull);
  try
    InvokeJSNoResult('oncuechange',[m],jiSet);
  finally
    m.free
  end;
end;

procedure TJSGlobalEventHandlers._Setoncut(const aValue : TEventHandler);
var
  m : TJOB_Method;
begin
  m:=TJOB_Method.create(TMethod(aValue),@JobCallEventHandlerNonNull);
  try
    InvokeJSNoResult('oncut',[m],jiSet);
  finally
    m.free
  end;
end;

procedure TJSGlobalEventHandlers._Setondblclick(const aValue : TEventHandler);
var
  m : TJOB_Method;
begin
  m:=TJOB_Method.create(TMethod(aValue),@JobCallEventHandlerNonNull);
  try
    InvokeJSNoResult('ondblclick',[m],jiSet);
  finally
    m.free
  end;
end;

procedure TJSGlobalEventHandlers._Setondrag(const aValue : TEventHandler);
var
  m : TJOB_Method;
begin
  m:=TJOB_Method.create(TMethod(aValue),@JobCallEventHandlerNonNull);
  try
    InvokeJSNoResult('ondrag',[m],jiSet);
  finally
    m.free
  end;
end;

procedure TJSGlobalEventHandlers._Setondragend(const aValue : TEventHandler);
var
  m : TJOB_Method;
begin
  m:=TJOB_Method.create(TMethod(aValue),@JobCallEventHandlerNonNull);
  try
    InvokeJSNoResult('ondragend',[m],jiSet);
  finally
    m.free
  end;
end;

procedure TJSGlobalEventHandlers._Setondragenter(const aValue : TEventHandler);
var
  m : TJOB_Method;
begin
  m:=TJOB_Method.create(TMethod(aValue),@JobCallEventHandlerNonNull);
  try
    InvokeJSNoResult('ondragenter',[m],jiSet);
  finally
    m.free
  end;
end;

procedure TJSGlobalEventHandlers._Setondragexit(const aValue : TEventHandler);
var
  m : TJOB_Method;
begin
  m:=TJOB_Method.create(TMethod(aValue),@JobCallEventHandlerNonNull);
  try
    InvokeJSNoResult('ondragexit',[m],jiSet);
  finally
    m.free
  end;
end;

procedure TJSGlobalEventHandlers._Setondragleave(const aValue : TEventHandler);
var
  m : TJOB_Method;
begin
  m:=TJOB_Method.create(TMethod(aValue),@JobCallEventHandlerNonNull);
  try
    InvokeJSNoResult('ondragleave',[m],jiSet);
  finally
    m.free
  end;
end;

procedure TJSGlobalEventHandlers._Setondragover(const aValue : TEventHandler);
var
  m : TJOB_Method;
begin
  m:=TJOB_Method.create(TMethod(aValue),@JobCallEventHandlerNonNull);
  try
    InvokeJSNoResult('ondragover',[m],jiSet);
  finally
    m.free
  end;
end;

procedure TJSGlobalEventHandlers._Setondragstart(const aValue : TEventHandler);
var
  m : TJOB_Method;
begin
  m:=TJOB_Method.create(TMethod(aValue),@JobCallEventHandlerNonNull);
  try
    InvokeJSNoResult('ondragstart',[m],jiSet);
  finally
    m.free
  end;
end;

procedure TJSGlobalEventHandlers._Setondrop(const aValue : TEventHandler);
var
  m : TJOB_Method;
begin
  m:=TJOB_Method.create(TMethod(aValue),@JobCallEventHandlerNonNull);
  try
    InvokeJSNoResult('ondrop',[m],jiSet);
  finally
    m.free
  end;
end;

procedure TJSGlobalEventHandlers._Setondurationchange(const aValue : TEventHandler);
var
  m : TJOB_Method;
begin
  m:=TJOB_Method.create(TMethod(aValue),@JobCallEventHandlerNonNull);
  try
    InvokeJSNoResult('ondurationchange',[m],jiSet);
  finally
    m.free
  end;
end;

procedure TJSGlobalEventHandlers._Setonemptied(const aValue : TEventHandler);
var
  m : TJOB_Method;
begin
  m:=TJOB_Method.create(TMethod(aValue),@JobCallEventHandlerNonNull);
  try
    InvokeJSNoResult('onemptied',[m],jiSet);
  finally
    m.free
  end;
end;

procedure TJSGlobalEventHandlers._Setonended(const aValue : TEventHandler);
var
  m : TJOB_Method;
begin
  m:=TJOB_Method.create(TMethod(aValue),@JobCallEventHandlerNonNull);
  try
    InvokeJSNoResult('onended',[m],jiSet);
  finally
    m.free
  end;
end;

procedure TJSGlobalEventHandlers._Setonformdata(const aValue : TEventHandler);
var
  m : TJOB_Method;
begin
  m:=TJOB_Method.create(TMethod(aValue),@JobCallEventHandlerNonNull);
  try
    InvokeJSNoResult('onformdata',[m],jiSet);
  finally
    m.free
  end;
end;

procedure TJSGlobalEventHandlers._Setoninput(const aValue : TEventHandler);
var
  m : TJOB_Method;
begin
  m:=TJOB_Method.create(TMethod(aValue),@JobCallEventHandlerNonNull);
  try
    InvokeJSNoResult('oninput',[m],jiSet);
  finally
    m.free
  end;
end;

procedure TJSGlobalEventHandlers._Setoninvalid(const aValue : TEventHandler);
var
  m : TJOB_Method;
begin
  m:=TJOB_Method.create(TMethod(aValue),@JobCallEventHandlerNonNull);
  try
    InvokeJSNoResult('oninvalid',[m],jiSet);
  finally
    m.free
  end;
end;

procedure TJSGlobalEventHandlers._Setonkeydown(const aValue : TEventHandler);
var
  m : TJOB_Method;
begin
  m:=TJOB_Method.create(TMethod(aValue),@JobCallEventHandlerNonNull);
  try
    InvokeJSNoResult('onkeydown',[m],jiSet);
  finally
    m.free
  end;
end;

procedure TJSGlobalEventHandlers._Setonkeypress(const aValue : TEventHandler);
var
  m : TJOB_Method;
begin
  m:=TJOB_Method.create(TMethod(aValue),@JobCallEventHandlerNonNull);
  try
    InvokeJSNoResult('onkeypress',[m],jiSet);
  finally
    m.free
  end;
end;

procedure TJSGlobalEventHandlers._Setonkeyup(const aValue : TEventHandler);
var
  m : TJOB_Method;
begin
  m:=TJOB_Method.create(TMethod(aValue),@JobCallEventHandlerNonNull);
  try
    InvokeJSNoResult('onkeyup',[m],jiSet);
  finally
    m.free
  end;
end;

procedure TJSGlobalEventHandlers._Setonload(const aValue : TEventHandler);
var
  m : TJOB_Method;
begin
  m:=TJOB_Method.create(TMethod(aValue),@JobCallEventHandlerNonNull);
  try
    InvokeJSNoResult('onload',[m],jiSet);
  finally
    m.free
  end;
end;

procedure TJSGlobalEventHandlers._Setonloadeddata(const aValue : TEventHandler);
var
  m : TJOB_Method;
begin
  m:=TJOB_Method.create(TMethod(aValue),@JobCallEventHandlerNonNull);
  try
    InvokeJSNoResult('onloadeddata',[m],jiSet);
  finally
    m.free
  end;
end;

procedure TJSGlobalEventHandlers._Setonloadedmetadata(const aValue : TEventHandler);
var
  m : TJOB_Method;
begin
  m:=TJOB_Method.create(TMethod(aValue),@JobCallEventHandlerNonNull);
  try
    InvokeJSNoResult('onloadedmetadata',[m],jiSet);
  finally
    m.free
  end;
end;

procedure TJSGlobalEventHandlers._Setonloadstart(const aValue : TEventHandler);
var
  m : TJOB_Method;
begin
  m:=TJOB_Method.create(TMethod(aValue),@JobCallEventHandlerNonNull);
  try
    InvokeJSNoResult('onloadstart',[m],jiSet);
  finally
    m.free
  end;
end;

procedure TJSGlobalEventHandlers._Setonmousedown(const aValue : TEventHandler);
var
  m : TJOB_Method;
begin
  m:=TJOB_Method.create(TMethod(aValue),@JobCallEventHandlerNonNull);
  try
    InvokeJSNoResult('onmousedown',[m],jiSet);
  finally
    m.free
  end;
end;

procedure TJSGlobalEventHandlers._Setonmouseenter(const aValue : TEventHandler);
var
  m : TJOB_Method;
begin
  m:=TJOB_Method.create(TMethod(aValue),@JobCallEventHandlerNonNull);
  try
    InvokeJSNoResult('onmouseenter',[m],jiSet);
  finally
    m.free
  end;
end;

procedure TJSGlobalEventHandlers._Setonmouseleave(const aValue : TEventHandler);
var
  m : TJOB_Method;
begin
  m:=TJOB_Method.create(TMethod(aValue),@JobCallEventHandlerNonNull);
  try
    InvokeJSNoResult('onmouseleave',[m],jiSet);
  finally
    m.free
  end;
end;

procedure TJSGlobalEventHandlers._Setonmousemove(const aValue : TEventHandler);
var
  m : TJOB_Method;
begin
  m:=TJOB_Method.create(TMethod(aValue),@JobCallEventHandlerNonNull);
  try
    InvokeJSNoResult('onmousemove',[m],jiSet);
  finally
    m.free
  end;
end;

procedure TJSGlobalEventHandlers._Setonmouseout(const aValue : TEventHandler);
var
  m : TJOB_Method;
begin
  m:=TJOB_Method.create(TMethod(aValue),@JobCallEventHandlerNonNull);
  try
    InvokeJSNoResult('onmouseout',[m],jiSet);
  finally
    m.free
  end;
end;

procedure TJSGlobalEventHandlers._Setonmouseover(const aValue : TEventHandler);
var
  m : TJOB_Method;
begin
  m:=TJOB_Method.create(TMethod(aValue),@JobCallEventHandlerNonNull);
  try
    InvokeJSNoResult('onmouseover',[m],jiSet);
  finally
    m.free
  end;
end;

procedure TJSGlobalEventHandlers._Setonmouseup(const aValue : TEventHandler);
var
  m : TJOB_Method;
begin
  m:=TJOB_Method.create(TMethod(aValue),@JobCallEventHandlerNonNull);
  try
    InvokeJSNoResult('onmouseup',[m],jiSet);
  finally
    m.free
  end;
end;

procedure TJSGlobalEventHandlers._Setonwheel(const aValue : TEventHandler);
var
  m : TJOB_Method;
begin
  m:=TJOB_Method.create(TMethod(aValue),@JobCallEventHandlerNonNull);
  try
    InvokeJSNoResult('onwheel',[m],jiSet);
  finally
    m.free
  end;
end;

procedure TJSGlobalEventHandlers._Setonpaste(const aValue : TEventHandler);
var
  m : TJOB_Method;
begin
  m:=TJOB_Method.create(TMethod(aValue),@JobCallEventHandlerNonNull);
  try
    InvokeJSNoResult('onpaste',[m],jiSet);
  finally
    m.free
  end;
end;

procedure TJSGlobalEventHandlers._Setonpause(const aValue : TEventHandler);
var
  m : TJOB_Method;
begin
  m:=TJOB_Method.create(TMethod(aValue),@JobCallEventHandlerNonNull);
  try
    InvokeJSNoResult('onpause',[m],jiSet);
  finally
    m.free
  end;
end;

procedure TJSGlobalEventHandlers._Setonplay(const aValue : TEventHandler);
var
  m : TJOB_Method;
begin
  m:=TJOB_Method.create(TMethod(aValue),@JobCallEventHandlerNonNull);
  try
    InvokeJSNoResult('onplay',[m],jiSet);
  finally
    m.free
  end;
end;

procedure TJSGlobalEventHandlers._Setonplaying(const aValue : TEventHandler);
var
  m : TJOB_Method;
begin
  m:=TJOB_Method.create(TMethod(aValue),@JobCallEventHandlerNonNull);
  try
    InvokeJSNoResult('onplaying',[m],jiSet);
  finally
    m.free
  end;
end;

procedure TJSGlobalEventHandlers._Setonprogress(const aValue : TEventHandler);
var
  m : TJOB_Method;
begin
  m:=TJOB_Method.create(TMethod(aValue),@JobCallEventHandlerNonNull);
  try
    InvokeJSNoResult('onprogress',[m],jiSet);
  finally
    m.free
  end;
end;

procedure TJSGlobalEventHandlers._Setonratechange(const aValue : TEventHandler);
var
  m : TJOB_Method;
begin
  m:=TJOB_Method.create(TMethod(aValue),@JobCallEventHandlerNonNull);
  try
    InvokeJSNoResult('onratechange',[m],jiSet);
  finally
    m.free
  end;
end;

procedure TJSGlobalEventHandlers._Setonreset(const aValue : TEventHandler);
var
  m : TJOB_Method;
begin
  m:=TJOB_Method.create(TMethod(aValue),@JobCallEventHandlerNonNull);
  try
    InvokeJSNoResult('onreset',[m],jiSet);
  finally
    m.free
  end;
end;

procedure TJSGlobalEventHandlers._Setonresize(const aValue : TEventHandler);
var
  m : TJOB_Method;
begin
  m:=TJOB_Method.create(TMethod(aValue),@JobCallEventHandlerNonNull);
  try
    InvokeJSNoResult('onresize',[m],jiSet);
  finally
    m.free
  end;
end;

procedure TJSGlobalEventHandlers._Setonscroll(const aValue : TEventHandler);
var
  m : TJOB_Method;
begin
  m:=TJOB_Method.create(TMethod(aValue),@JobCallEventHandlerNonNull);
  try
    InvokeJSNoResult('onscroll',[m],jiSet);
  finally
    m.free
  end;
end;

procedure TJSGlobalEventHandlers._Setonscrollend(const aValue : TEventHandler);
var
  m : TJOB_Method;
begin
  m:=TJOB_Method.create(TMethod(aValue),@JobCallEventHandlerNonNull);
  try
    InvokeJSNoResult('onscrollend',[m],jiSet);
  finally
    m.free
  end;
end;

procedure TJSGlobalEventHandlers._Setonsecuritypolicyviolation(const aValue : TEventHandler);
var
  m : TJOB_Method;
begin
  m:=TJOB_Method.create(TMethod(aValue),@JobCallEventHandlerNonNull);
  try
    InvokeJSNoResult('onsecuritypolicyviolation',[m],jiSet);
  finally
    m.free
  end;
end;

procedure TJSGlobalEventHandlers._Setonseeked(const aValue : TEventHandler);
var
  m : TJOB_Method;
begin
  m:=TJOB_Method.create(TMethod(aValue),@JobCallEventHandlerNonNull);
  try
    InvokeJSNoResult('onseeked',[m],jiSet);
  finally
    m.free
  end;
end;

procedure TJSGlobalEventHandlers._Setonseeking(const aValue : TEventHandler);
var
  m : TJOB_Method;
begin
  m:=TJOB_Method.create(TMethod(aValue),@JobCallEventHandlerNonNull);
  try
    InvokeJSNoResult('onseeking',[m],jiSet);
  finally
    m.free
  end;
end;

procedure TJSGlobalEventHandlers._Setonselect(const aValue : TEventHandler);
var
  m : TJOB_Method;
begin
  m:=TJOB_Method.create(TMethod(aValue),@JobCallEventHandlerNonNull);
  try
    InvokeJSNoResult('onselect',[m],jiSet);
  finally
    m.free
  end;
end;

procedure TJSGlobalEventHandlers._Setonslotchange(const aValue : TEventHandler);
var
  m : TJOB_Method;
begin
  m:=TJOB_Method.create(TMethod(aValue),@JobCallEventHandlerNonNull);
  try
    InvokeJSNoResult('onslotchange',[m],jiSet);
  finally
    m.free
  end;
end;

procedure TJSGlobalEventHandlers._Setonstalled(const aValue : TEventHandler);
var
  m : TJOB_Method;
begin
  m:=TJOB_Method.create(TMethod(aValue),@JobCallEventHandlerNonNull);
  try
    InvokeJSNoResult('onstalled',[m],jiSet);
  finally
    m.free
  end;
end;

procedure TJSGlobalEventHandlers._Setonsubmit(const aValue : TEventHandler);
var
  m : TJOB_Method;
begin
  m:=TJOB_Method.create(TMethod(aValue),@JobCallEventHandlerNonNull);
  try
    InvokeJSNoResult('onsubmit',[m],jiSet);
  finally
    m.free
  end;
end;

procedure TJSGlobalEventHandlers._Setonsuspend(const aValue : TEventHandler);
var
  m : TJOB_Method;
begin
  m:=TJOB_Method.create(TMethod(aValue),@JobCallEventHandlerNonNull);
  try
    InvokeJSNoResult('onsuspend',[m],jiSet);
  finally
    m.free
  end;
end;

procedure TJSGlobalEventHandlers._Setontimeupdate(const aValue : TEventHandler);
var
  m : TJOB_Method;
begin
  m:=TJOB_Method.create(TMethod(aValue),@JobCallEventHandlerNonNull);
  try
    InvokeJSNoResult('ontimeupdate',[m],jiSet);
  finally
    m.free
  end;
end;

procedure TJSGlobalEventHandlers._Setonvolumechange(const aValue : TEventHandler);
var
  m : TJOB_Method;
begin
  m:=TJOB_Method.create(TMethod(aValue),@JobCallEventHandlerNonNull);
  try
    InvokeJSNoResult('onvolumechange',[m],jiSet);
  finally
    m.free
  end;
end;

procedure TJSGlobalEventHandlers._Setonwaiting(const aValue : TEventHandler);
var
  m : TJOB_Method;
begin
  m:=TJOB_Method.create(TMethod(aValue),@JobCallEventHandlerNonNull);
  try
    InvokeJSNoResult('onwaiting',[m],jiSet);
  finally
    m.free
  end;
end;

procedure TJSGlobalEventHandlers._Setonselectstart(const aValue : TEventHandler);
var
  m : TJOB_Method;
begin
  m:=TJOB_Method.create(TMethod(aValue),@JobCallEventHandlerNonNull);
  try
    InvokeJSNoResult('onselectstart',[m],jiSet);
  finally
    m.free
  end;
end;

procedure TJSGlobalEventHandlers._Setonselectionchange(const aValue : TEventHandler);
var
  m : TJOB_Method;
begin
  m:=TJOB_Method.create(TMethod(aValue),@JobCallEventHandlerNonNull);
  try
    InvokeJSNoResult('onselectionchange',[m],jiSet);
  finally
    m.free
  end;
end;

procedure TJSGlobalEventHandlers._Setontoggle(const aValue : TEventHandler);
var
  m : TJOB_Method;
begin
  m:=TJOB_Method.create(TMethod(aValue),@JobCallEventHandlerNonNull);
  try
    InvokeJSNoResult('ontoggle',[m],jiSet);
  finally
    m.free
  end;
end;

procedure TJSGlobalEventHandlers._Setonpointercancel(const aValue : TEventHandler);
var
  m : TJOB_Method;
begin
  m:=TJOB_Method.create(TMethod(aValue),@JobCallEventHandlerNonNull);
  try
    InvokeJSNoResult('onpointercancel',[m],jiSet);
  finally
    m.free
  end;
end;

procedure TJSGlobalEventHandlers._Setonpointerdown(const aValue : TEventHandler);
var
  m : TJOB_Method;
begin
  m:=TJOB_Method.create(TMethod(aValue),@JobCallEventHandlerNonNull);
  try
    InvokeJSNoResult('onpointerdown',[m],jiSet);
  finally
    m.free
  end;
end;

procedure TJSGlobalEventHandlers._Setonpointerup(const aValue : TEventHandler);
var
  m : TJOB_Method;
begin
  m:=TJOB_Method.create(TMethod(aValue),@JobCallEventHandlerNonNull);
  try
    InvokeJSNoResult('onpointerup',[m],jiSet);
  finally
    m.free
  end;
end;

procedure TJSGlobalEventHandlers._Setonpointermove(const aValue : TEventHandler);
var
  m : TJOB_Method;
begin
  m:=TJOB_Method.create(TMethod(aValue),@JobCallEventHandlerNonNull);
  try
    InvokeJSNoResult('onpointermove',[m],jiSet);
  finally
    m.free
  end;
end;

procedure TJSGlobalEventHandlers._Setonpointerout(const aValue : TEventHandler);
var
  m : TJOB_Method;
begin
  m:=TJOB_Method.create(TMethod(aValue),@JobCallEventHandlerNonNull);
  try
    InvokeJSNoResult('onpointerout',[m],jiSet);
  finally
    m.free
  end;
end;

procedure TJSGlobalEventHandlers._Setonpointerover(const aValue : TEventHandler);
var
  m : TJOB_Method;
begin
  m:=TJOB_Method.create(TMethod(aValue),@JobCallEventHandlerNonNull);
  try
    InvokeJSNoResult('onpointerover',[m],jiSet);
  finally
    m.free
  end;
end;

procedure TJSGlobalEventHandlers._Setonpointerenter(const aValue : TEventHandler);
var
  m : TJOB_Method;
begin
  m:=TJOB_Method.create(TMethod(aValue),@JobCallEventHandlerNonNull);
  try
    InvokeJSNoResult('onpointerenter',[m],jiSet);
  finally
    m.free
  end;
end;

procedure TJSGlobalEventHandlers._Setonpointerleave(const aValue : TEventHandler);
var
  m : TJOB_Method;
begin
  m:=TJOB_Method.create(TMethod(aValue),@JobCallEventHandlerNonNull);
  try
    InvokeJSNoResult('onpointerleave',[m],jiSet);
  finally
    m.free
  end;
end;

procedure TJSGlobalEventHandlers._Setongotpointercapture(const aValue : TEventHandler);
var
  m : TJOB_Method;
begin
  m:=TJOB_Method.create(TMethod(aValue),@JobCallEventHandlerNonNull);
  try
    InvokeJSNoResult('ongotpointercapture',[m],jiSet);
  finally
    m.free
  end;
end;

procedure TJSGlobalEventHandlers._Setonlostpointercapture(const aValue : TEventHandler);
var
  m : TJOB_Method;
begin
  m:=TJOB_Method.create(TMethod(aValue),@JobCallEventHandlerNonNull);
  try
    InvokeJSNoResult('onlostpointercapture',[m],jiSet);
  finally
    m.free
  end;
end;

procedure TJSGlobalEventHandlers._Setonmozfullscreenchange(const aValue : TEventHandler);
var
  m : TJOB_Method;
begin
  m:=TJOB_Method.create(TMethod(aValue),@JobCallEventHandlerNonNull);
  try
    InvokeJSNoResult('onmozfullscreenchange',[m],jiSet);
  finally
    m.free
  end;
end;

procedure TJSGlobalEventHandlers._Setonmozfullscreenerror(const aValue : TEventHandler);
var
  m : TJOB_Method;
begin
  m:=TJOB_Method.create(TMethod(aValue),@JobCallEventHandlerNonNull);
  try
    InvokeJSNoResult('onmozfullscreenerror',[m],jiSet);
  finally
    m.free
  end;
end;

procedure TJSGlobalEventHandlers._Setonanimationcancel(const aValue : TEventHandler);
var
  m : TJOB_Method;
begin
  m:=TJOB_Method.create(TMethod(aValue),@JobCallEventHandlerNonNull);
  try
    InvokeJSNoResult('onanimationcancel',[m],jiSet);
  finally
    m.free
  end;
end;

procedure TJSGlobalEventHandlers._Setonanimationend(const aValue : TEventHandler);
var
  m : TJOB_Method;
begin
  m:=TJOB_Method.create(TMethod(aValue),@JobCallEventHandlerNonNull);
  try
    InvokeJSNoResult('onanimationend',[m],jiSet);
  finally
    m.free
  end;
end;

procedure TJSGlobalEventHandlers._Setonanimationiteration(const aValue : TEventHandler);
var
  m : TJOB_Method;
begin
  m:=TJOB_Method.create(TMethod(aValue),@JobCallEventHandlerNonNull);
  try
    InvokeJSNoResult('onanimationiteration',[m],jiSet);
  finally
    m.free
  end;
end;

procedure TJSGlobalEventHandlers._Setonanimationstart(const aValue : TEventHandler);
var
  m : TJOB_Method;
begin
  m:=TJOB_Method.create(TMethod(aValue),@JobCallEventHandlerNonNull);
  try
    InvokeJSNoResult('onanimationstart',[m],jiSet);
  finally
    m.free
  end;
end;

procedure TJSGlobalEventHandlers._Setontransitioncancel(const aValue : TEventHandler);
var
  m : TJOB_Method;
begin
  m:=TJOB_Method.create(TMethod(aValue),@JobCallEventHandlerNonNull);
  try
    InvokeJSNoResult('ontransitioncancel',[m],jiSet);
  finally
    m.free
  end;
end;

procedure TJSGlobalEventHandlers._Setontransitionend(const aValue : TEventHandler);
var
  m : TJOB_Method;
begin
  m:=TJOB_Method.create(TMethod(aValue),@JobCallEventHandlerNonNull);
  try
    InvokeJSNoResult('ontransitionend',[m],jiSet);
  finally
    m.free
  end;
end;

procedure TJSGlobalEventHandlers._Setontransitionrun(const aValue : TEventHandler);
var
  m : TJOB_Method;
begin
  m:=TJOB_Method.create(TMethod(aValue),@JobCallEventHandlerNonNull);
  try
    InvokeJSNoResult('ontransitionrun',[m],jiSet);
  finally
    m.free
  end;
end;

procedure TJSGlobalEventHandlers._Setontransitionstart(const aValue : TEventHandler);
var
  m : TJOB_Method;
begin
  m:=TJOB_Method.create(TMethod(aValue),@JobCallEventHandlerNonNull);
  try
    InvokeJSNoResult('ontransitionstart',[m],jiSet);
  finally
    m.free
  end;
end;

procedure TJSGlobalEventHandlers._Setonwebkitanimationend(const aValue : TEventHandler);
var
  m : TJOB_Method;
begin
  m:=TJOB_Method.create(TMethod(aValue),@JobCallEventHandlerNonNull);
  try
    InvokeJSNoResult('onwebkitanimationend',[m],jiSet);
  finally
    m.free
  end;
end;

procedure TJSGlobalEventHandlers._Setonwebkitanimationiteration(const aValue : TEventHandler);
var
  m : TJOB_Method;
begin
  m:=TJOB_Method.create(TMethod(aValue),@JobCallEventHandlerNonNull);
  try
    InvokeJSNoResult('onwebkitanimationiteration',[m],jiSet);
  finally
    m.free
  end;
end;

procedure TJSGlobalEventHandlers._Setonwebkitanimationstart(const aValue : TEventHandler);
var
  m : TJOB_Method;
begin
  m:=TJOB_Method.create(TMethod(aValue),@JobCallEventHandlerNonNull);
  try
    InvokeJSNoResult('onwebkitanimationstart',[m],jiSet);
  finally
    m.free
  end;
end;

procedure TJSGlobalEventHandlers._Setonwebkittransitionend(const aValue : TEventHandler);
var
  m : TJOB_Method;
begin
  m:=TJOB_Method.create(TMethod(aValue),@JobCallEventHandlerNonNull);
  try
    InvokeJSNoResult('onwebkittransitionend',[m],jiSet);
  finally
    m.free
  end;
end;

class function TJSGlobalEventHandlers.JSClassName: UnicodeString;
begin
  Result:='GlobalEventHandlers';
end;

class function TJSGlobalEventHandlers.Cast(const Intf: IJSObject): IJSGlobalEventHandlers;
begin
  Result:=TJSGlobalEventHandlers.JOBCast(Intf);
end;

function TJSWindowEventHandlers._Getonafterprint: TEventHandler;
begin
  Result:=(TEventHandlerNonNull(ReadJSPropertyMethod('onafterprint')));
end;

function TJSWindowEventHandlers._Getonbeforeprint: TEventHandler;
begin
  Result:=(TEventHandlerNonNull(ReadJSPropertyMethod('onbeforeprint')));
end;

function TJSWindowEventHandlers._Getonbeforeunload: TOnBeforeUnloadEventHandler;
begin
  Result:=(TOnBeforeUnloadEventHandlerNonNull(ReadJSPropertyMethod('onbeforeunload')));
end;

function TJSWindowEventHandlers._Getonhashchange: TEventHandler;
begin
  Result:=(TEventHandlerNonNull(ReadJSPropertyMethod('onhashchange')));
end;

function TJSWindowEventHandlers._Getonlanguagechange: TEventHandler;
begin
  Result:=(TEventHandlerNonNull(ReadJSPropertyMethod('onlanguagechange')));
end;

function TJSWindowEventHandlers._Getonmessage: TEventHandler;
begin
  Result:=(TEventHandlerNonNull(ReadJSPropertyMethod('onmessage')));
end;

function TJSWindowEventHandlers._Getonmessageerror: TEventHandler;
begin
  Result:=(TEventHandlerNonNull(ReadJSPropertyMethod('onmessageerror')));
end;

function TJSWindowEventHandlers._Getonoffline: TEventHandler;
begin
  Result:=(TEventHandlerNonNull(ReadJSPropertyMethod('onoffline')));
end;

function TJSWindowEventHandlers._Getononline: TEventHandler;
begin
  Result:=(TEventHandlerNonNull(ReadJSPropertyMethod('ononline')));
end;

function TJSWindowEventHandlers._Getonpagehide: TEventHandler;
begin
  Result:=(TEventHandlerNonNull(ReadJSPropertyMethod('onpagehide')));
end;

function TJSWindowEventHandlers._Getonpageshow: TEventHandler;
begin
  Result:=(TEventHandlerNonNull(ReadJSPropertyMethod('onpageshow')));
end;

function TJSWindowEventHandlers._Getonpopstate: TEventHandler;
begin
  Result:=(TEventHandlerNonNull(ReadJSPropertyMethod('onpopstate')));
end;

function TJSWindowEventHandlers._Getonrejectionhandled: TEventHandler;
begin
  Result:=(TEventHandlerNonNull(ReadJSPropertyMethod('onrejectionhandled')));
end;

function TJSWindowEventHandlers._Getonstorage: TEventHandler;
begin
  Result:=(TEventHandlerNonNull(ReadJSPropertyMethod('onstorage')));
end;

function TJSWindowEventHandlers._Getonunhandledrejection: TEventHandler;
begin
  Result:=(TEventHandlerNonNull(ReadJSPropertyMethod('onunhandledrejection')));
end;

function TJSWindowEventHandlers._Getonunload: TEventHandler;
begin
  Result:=(TEventHandlerNonNull(ReadJSPropertyMethod('onunload')));
end;

function TJSWindowEventHandlers._Getongamepadconnected: TEventHandler;
begin
  Result:=(TEventHandlerNonNull(ReadJSPropertyMethod('ongamepadconnected')));
end;

function TJSWindowEventHandlers._Getongamepaddisconnected: TEventHandler;
begin
  Result:=(TEventHandlerNonNull(ReadJSPropertyMethod('ongamepaddisconnected')));
end;

procedure TJSWindowEventHandlers._Setonafterprint(const aValue : TEventHandler);
var
  m : TJOB_Method;
begin
  m:=TJOB_Method.create(TMethod(aValue),@JobCallEventHandlerNonNull);
  try
    InvokeJSNoResult('onafterprint',[m],jiSet);
  finally
    m.free
  end;
end;

procedure TJSWindowEventHandlers._Setonbeforeprint(const aValue : TEventHandler);
var
  m : TJOB_Method;
begin
  m:=TJOB_Method.create(TMethod(aValue),@JobCallEventHandlerNonNull);
  try
    InvokeJSNoResult('onbeforeprint',[m],jiSet);
  finally
    m.free
  end;
end;

procedure TJSWindowEventHandlers._Setonbeforeunload(const aValue : TOnBeforeUnloadEventHandler);
var
  m : TJOB_Method;
begin
  m:=TJOB_Method.create(TMethod(aValue),@JobCallOnBeforeUnloadEventHandlerNonNull);
  try
    InvokeJSNoResult('onbeforeunload',[m],jiSet);
  finally
    m.free
  end;
end;

procedure TJSWindowEventHandlers._Setonhashchange(const aValue : TEventHandler);
var
  m : TJOB_Method;
begin
  m:=TJOB_Method.create(TMethod(aValue),@JobCallEventHandlerNonNull);
  try
    InvokeJSNoResult('onhashchange',[m],jiSet);
  finally
    m.free
  end;
end;

procedure TJSWindowEventHandlers._Setonlanguagechange(const aValue : TEventHandler);
var
  m : TJOB_Method;
begin
  m:=TJOB_Method.create(TMethod(aValue),@JobCallEventHandlerNonNull);
  try
    InvokeJSNoResult('onlanguagechange',[m],jiSet);
  finally
    m.free
  end;
end;

procedure TJSWindowEventHandlers._Setonmessage(const aValue : TEventHandler);
var
  m : TJOB_Method;
begin
  m:=TJOB_Method.create(TMethod(aValue),@JobCallEventHandlerNonNull);
  try
    InvokeJSNoResult('onmessage',[m],jiSet);
  finally
    m.free
  end;
end;

procedure TJSWindowEventHandlers._Setonmessageerror(const aValue : TEventHandler);
var
  m : TJOB_Method;
begin
  m:=TJOB_Method.create(TMethod(aValue),@JobCallEventHandlerNonNull);
  try
    InvokeJSNoResult('onmessageerror',[m],jiSet);
  finally
    m.free
  end;
end;

procedure TJSWindowEventHandlers._Setonoffline(const aValue : TEventHandler);
var
  m : TJOB_Method;
begin
  m:=TJOB_Method.create(TMethod(aValue),@JobCallEventHandlerNonNull);
  try
    InvokeJSNoResult('onoffline',[m],jiSet);
  finally
    m.free
  end;
end;

procedure TJSWindowEventHandlers._Setononline(const aValue : TEventHandler);
var
  m : TJOB_Method;
begin
  m:=TJOB_Method.create(TMethod(aValue),@JobCallEventHandlerNonNull);
  try
    InvokeJSNoResult('ononline',[m],jiSet);
  finally
    m.free
  end;
end;

procedure TJSWindowEventHandlers._Setonpagehide(const aValue : TEventHandler);
var
  m : TJOB_Method;
begin
  m:=TJOB_Method.create(TMethod(aValue),@JobCallEventHandlerNonNull);
  try
    InvokeJSNoResult('onpagehide',[m],jiSet);
  finally
    m.free
  end;
end;

procedure TJSWindowEventHandlers._Setonpageshow(const aValue : TEventHandler);
var
  m : TJOB_Method;
begin
  m:=TJOB_Method.create(TMethod(aValue),@JobCallEventHandlerNonNull);
  try
    InvokeJSNoResult('onpageshow',[m],jiSet);
  finally
    m.free
  end;
end;

procedure TJSWindowEventHandlers._Setonpopstate(const aValue : TEventHandler);
var
  m : TJOB_Method;
begin
  m:=TJOB_Method.create(TMethod(aValue),@JobCallEventHandlerNonNull);
  try
    InvokeJSNoResult('onpopstate',[m],jiSet);
  finally
    m.free
  end;
end;

procedure TJSWindowEventHandlers._Setonrejectionhandled(const aValue : TEventHandler);
var
  m : TJOB_Method;
begin
  m:=TJOB_Method.create(TMethod(aValue),@JobCallEventHandlerNonNull);
  try
    InvokeJSNoResult('onrejectionhandled',[m],jiSet);
  finally
    m.free
  end;
end;

procedure TJSWindowEventHandlers._Setonstorage(const aValue : TEventHandler);
var
  m : TJOB_Method;
begin
  m:=TJOB_Method.create(TMethod(aValue),@JobCallEventHandlerNonNull);
  try
    InvokeJSNoResult('onstorage',[m],jiSet);
  finally
    m.free
  end;
end;

procedure TJSWindowEventHandlers._Setonunhandledrejection(const aValue : TEventHandler);
var
  m : TJOB_Method;
begin
  m:=TJOB_Method.create(TMethod(aValue),@JobCallEventHandlerNonNull);
  try
    InvokeJSNoResult('onunhandledrejection',[m],jiSet);
  finally
    m.free
  end;
end;

procedure TJSWindowEventHandlers._Setonunload(const aValue : TEventHandler);
var
  m : TJOB_Method;
begin
  m:=TJOB_Method.create(TMethod(aValue),@JobCallEventHandlerNonNull);
  try
    InvokeJSNoResult('onunload',[m],jiSet);
  finally
    m.free
  end;
end;

procedure TJSWindowEventHandlers._Setongamepadconnected(const aValue : TEventHandler);
var
  m : TJOB_Method;
begin
  m:=TJOB_Method.create(TMethod(aValue),@JobCallEventHandlerNonNull);
  try
    InvokeJSNoResult('ongamepadconnected',[m],jiSet);
  finally
    m.free
  end;
end;

procedure TJSWindowEventHandlers._Setongamepaddisconnected(const aValue : TEventHandler);
var
  m : TJOB_Method;
begin
  m:=TJOB_Method.create(TMethod(aValue),@JobCallEventHandlerNonNull);
  try
    InvokeJSNoResult('ongamepaddisconnected',[m],jiSet);
  finally
    m.free
  end;
end;

class function TJSWindowEventHandlers.JSClassName: UnicodeString;
begin
  Result:='WindowEventHandlers';
end;

class function TJSWindowEventHandlers.Cast(const Intf: IJSObject): IJSWindowEventHandlers;
begin
  Result:=TJSWindowEventHandlers.JOBCast(Intf);
end;

function TJSOnErrorEventHandlerForNodes._Getonerror: TEventHandler;
begin
  Result:=(TEventHandlerNonNull(ReadJSPropertyMethod('onerror')));
end;

procedure TJSOnErrorEventHandlerForNodes._Setonerror(const aValue : TEventHandler);
var
  m : TJOB_Method;
begin
  m:=TJOB_Method.create(TMethod(aValue),@JobCallEventHandlerNonNull);
  try
    InvokeJSNoResult('onerror',[m],jiSet);
  finally
    m.free
  end;
end;

class function TJSOnErrorEventHandlerForNodes.JSClassName: UnicodeString;
begin
  Result:='OnErrorEventHandlerForNodes';
end;

class function TJSOnErrorEventHandlerForNodes.Cast(const Intf: IJSObject): IJSOnErrorEventHandlerForNodes;
begin
  Result:=TJSOnErrorEventHandlerForNodes.JOBCast(Intf);
end;

function TJSOnErrorEventHandlerForWindow._Getonerror: TOnErrorEventHandler;
begin
  Result:=(TOnErrorEventHandlerNonNull(ReadJSPropertyMethod('onerror')));
end;

procedure TJSOnErrorEventHandlerForWindow._Setonerror(const aValue : TOnErrorEventHandler);
var
  m : TJOB_Method;
begin
  m:=TJOB_Method.create(TMethod(aValue),@JobCallOnErrorEventHandlerNonNull);
  try
    InvokeJSNoResult('onerror',[m],jiSet);
  finally
    m.free
  end;
end;

class function TJSOnErrorEventHandlerForWindow.JSClassName: UnicodeString;
begin
  Result:='OnErrorEventHandlerForWindow';
end;

class function TJSOnErrorEventHandlerForWindow.Cast(const Intf: IJSObject): IJSOnErrorEventHandlerForWindow;
begin
  Result:=TJSOnErrorEventHandlerForWindow.JOBCast(Intf);
end;

function TJSEventListenerOptions._Getcapture: Boolean;
begin
  Result:=ReadJSPropertyBoolean('capture');
end;

function TJSEventListenerOptions._GetmozSystemGroup: Boolean;
begin
  Result:=ReadJSPropertyBoolean('mozSystemGroup');
end;

procedure TJSEventListenerOptions._Setcapture(const aValue : Boolean);
begin
  WriteJSPropertyBoolean('capture',aValue);
end;

procedure TJSEventListenerOptions._SetmozSystemGroup(const aValue : Boolean);
begin
  WriteJSPropertyBoolean('mozSystemGroup',aValue);
end;

constructor TJSEventListenerOptions.create(const aDict : TJSEventListenerOptionsRec); overload;
begin
  Self.capture:=aDict.capture;
  Self.mozSystemGroup:=aDict.mozSystemGroup;
end;

class function TJSEventListenerOptions.JSClassName: UnicodeString;
begin
  Result:='Object';
end;

class function TJSEventListenerOptions.Cast(const Intf: IJSObject): IJSEventListenerOptions;
begin
  Result:=TJSEventListenerOptions.JOBCast(Intf);
end;

function TJSAddEventListenerOptions._Getpassive: Boolean;
begin
  Result:=ReadJSPropertyBoolean('passive');
end;

function TJSAddEventListenerOptions._Getonce: Boolean;
begin
  Result:=ReadJSPropertyBoolean('once');
end;

procedure TJSAddEventListenerOptions._Setpassive(const aValue : Boolean);
begin
  WriteJSPropertyBoolean('passive',aValue);
end;

procedure TJSAddEventListenerOptions._Setonce(const aValue : Boolean);
begin
  WriteJSPropertyBoolean('once',aValue);
end;

constructor TJSAddEventListenerOptions.create(const aDict : TJSAddEventListenerOptionsRec); overload;
begin
  Self.passive:=aDict.passive;
  Self.once:=aDict.once;
end;

class function TJSAddEventListenerOptions.JSClassName: UnicodeString;
begin
  Result:='Object';
end;

class function TJSAddEventListenerOptions.Cast(const Intf: IJSObject): IJSAddEventListenerOptions;
begin
  Result:=TJSAddEventListenerOptions.JOBCast(Intf);
end;

constructor TJSEventTarget.Create;
begin
  JOBCreate([]);
end;

procedure TJSEventTarget.addEventListener(const aType_: UnicodeString; const aListener: TEventListener; const aOptions: IJSAddEventListenerOptions; aWantsUntrusted: Boolean);
var
  m: TJOB_Method;
begin
  m:=TJOB_Method.Create(TMethod(aListener),@JOBCallEventListener);
  try
    InvokeJSNoResult('addEventListener',[aType_,m,aOptions,aWantsUntrusted]);
  finally
    m.free;
  end;
end;

procedure TJSEventTarget.addEventListener(const aType_: UnicodeString; const aListener: TEventListener);
var
  m: TJOB_Method;
begin
  m:=TJOB_Method.Create(TMethod(aListener),@JOBCallEventListener);
  try
    InvokeJSNoResult('addEventListener',[aType_,m]);
  finally
    m.free;
  end;
end;

procedure TJSEventTarget.addEventListener(const aType_: UnicodeString; const aListener: TEventListener; aOptions: Boolean; aWantsUntrusted: Boolean);
var
  m: TJOB_Method;
begin
  m:=TJOB_Method.Create(TMethod(aListener),@JOBCallEventListener);
  try
    InvokeJSNoResult('addEventListener',[aType_,m,aOptions,aWantsUntrusted]);
  finally
    m.free;
  end;
end;

procedure TJSEventTarget.addEventListener(const aType_: UnicodeString; const aListener: TEventListener; aOptions: Boolean);
var
  m: TJOB_Method;
begin
  m:=TJOB_Method.Create(TMethod(aListener),@JOBCallEventListener);
  try
    InvokeJSNoResult('addEventListener',[aType_,m,aOptions]);
  finally
    m.free;
  end;
end;

procedure TJSEventTarget.addEventListener(const aType_: UnicodeString; const aListener: TEventListener; const aOptions: IJSAddEventListenerOptions);
var
  m: TJOB_Method;
begin
  m:=TJOB_Method.Create(TMethod(aListener),@JOBCallEventListener);
  try
    InvokeJSNoResult('addEventListener',[aType_,m,aOptions]);
  finally
    m.free;
  end;
end;

procedure TJSEventTarget.removeEventListener(const aType_: UnicodeString; const aListener: TEventListener; aOptions: Boolean);
var
  m: TJOB_Method;
begin
  m:=TJOB_Method.Create(TMethod(aListener),@JOBCallEventListener);
  try
    InvokeJSNoResult('removeEventListener',[aType_,m,aOptions]);
  finally
    m.free;
  end;
end;

procedure TJSEventTarget.removeEventListener(const aType_: UnicodeString; const aListener: TEventListener);
var
  m: TJOB_Method;
begin
  m:=TJOB_Method.Create(TMethod(aListener),@JOBCallEventListener);
  try
    InvokeJSNoResult('removeEventListener',[aType_,m]);
  finally
    m.free;
  end;
end;

procedure TJSEventTarget.removeEventListener(const aType_: UnicodeString; const aListener: TEventListener; const aOptions: IJSEventListenerOptions);
var
  m: TJOB_Method;
begin
  m:=TJOB_Method.Create(TMethod(aListener),@JOBCallEventListener);
  try
    InvokeJSNoResult('removeEventListener',[aType_,m,aOptions]);
  finally
    m.free;
  end;
end;

function TJSEventTarget.dispatchEvent(aEvent: IJSEvent): Boolean;
begin
  Result:=InvokeJSBooleanResult('dispatchEvent',[aEvent]);
end;

class function TJSEventTarget.JSClassName: UnicodeString;
begin
  Result:='EventTarget';
end;

class function TJSEventTarget.Cast(const Intf: IJSObject): IJSEventTarget;
begin
  Result:=TJSEventTarget.JOBCast(Intf);
end;

function TJSEvent._Gettype_: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('type');
end;

function TJSEvent._Gettarget: IJSEventTarget;
begin
  Result:=ReadJSPropertyObject('target',TJSEventTarget) as IJSEventTarget;
end;

function TJSEvent._GetcurrentTarget: IJSEventTarget;
begin
  Result:=ReadJSPropertyObject('currentTarget',TJSEventTarget) as IJSEventTarget;
end;

function TJSEvent._GeteventPhase: Word;
begin
  Result:=ReadJSPropertyLongInt('eventPhase');
end;

function TJSEvent._Getbubbles: Boolean;
begin
  Result:=ReadJSPropertyBoolean('bubbles');
end;

function TJSEvent._Getcancelable: Boolean;
begin
  Result:=ReadJSPropertyBoolean('cancelable');
end;

function TJSEvent._GetreturnValue: Boolean;
begin
  Result:=ReadJSPropertyBoolean('returnValue');
end;

function TJSEvent._GetdefaultPrevented: Boolean;
begin
  Result:=ReadJSPropertyBoolean('defaultPrevented');
end;

function TJSEvent._Getcomposed: Boolean;
begin
  Result:=ReadJSPropertyBoolean('composed');
end;

function TJSEvent._GetisTrusted: Boolean;
begin
  Result:=ReadJSPropertyBoolean('isTrusted');
end;

function TJSEvent._GettimeStamp: TDOMHighResTimeStamp;
begin
  Result:=ReadJSPropertyDouble('timeStamp');
end;

function TJSEvent._GetcancelBubble: Boolean;
begin
  Result:=ReadJSPropertyBoolean('cancelBubble');
end;

function TJSEvent._GetoriginalTarget: IJSEventTarget;
begin
  Result:=ReadJSPropertyObject('originalTarget',TJSEventTarget) as IJSEventTarget;
end;

function TJSEvent._GetexplicitOriginalTarget: IJSEventTarget;
begin
  Result:=ReadJSPropertyObject('explicitOriginalTarget',TJSEventTarget) as IJSEventTarget;
end;

procedure TJSEvent._SetreturnValue(const aValue : Boolean);
begin
  WriteJSPropertyBoolean('returnValue',aValue);
end;

procedure TJSEvent._SetcancelBubble(const aValue : Boolean);
begin
  WriteJSPropertyBoolean('cancelBubble',aValue);
end;

constructor TJSEvent.Create(const aType_: UnicodeString; const aEventInitDict: IJSEventInit);
begin
  JOBCreate([aType_,aEventInitDict]);
end;

constructor TJSEvent.Create(const aType_: UnicodeString);
begin
  JOBCreate([aType_]);
end;

function TJSEvent.composedPath: TJSEventTargetDynArray;
begin
  Result:=InvokeJSObjectResult('composedPath',[],TJSArray) as TJSEventTargetDynArray;
end;

procedure TJSEvent.stopPropagation;
begin
  InvokeJSNoResult('stopPropagation',[]);
end;

procedure TJSEvent.stopImmediatePropagation;
begin
  InvokeJSNoResult('stopImmediatePropagation',[]);
end;

procedure TJSEvent.preventDefault;
begin
  InvokeJSNoResult('preventDefault',[]);
end;

procedure TJSEvent.initEvent(const aType_: UnicodeString; aBubbles: Boolean; aCancelable: Boolean);
begin
  InvokeJSNoResult('initEvent',[aType_,aBubbles,aCancelable]);
end;

procedure TJSEvent.initEvent(const aType_: UnicodeString);
begin
  InvokeJSNoResult('initEvent',[aType_]);
end;

procedure TJSEvent.initEvent(const aType_: UnicodeString; aBubbles: Boolean);
begin
  InvokeJSNoResult('initEvent',[aType_,aBubbles]);
end;

class function TJSEvent.JSClassName: UnicodeString;
begin
  Result:='Event';
end;

class function TJSEvent.Cast(const Intf: IJSObject): IJSEvent;
begin
  Result:=TJSEvent.JOBCast(Intf);
end;

function TJSEventInit._Getbubbles: Boolean;
begin
  Result:=ReadJSPropertyBoolean('bubbles');
end;

function TJSEventInit._Getcancelable: Boolean;
begin
  Result:=ReadJSPropertyBoolean('cancelable');
end;

function TJSEventInit._Getcomposed: Boolean;
begin
  Result:=ReadJSPropertyBoolean('composed');
end;

procedure TJSEventInit._Setbubbles(const aValue : Boolean);
begin
  WriteJSPropertyBoolean('bubbles',aValue);
end;

procedure TJSEventInit._Setcancelable(const aValue : Boolean);
begin
  WriteJSPropertyBoolean('cancelable',aValue);
end;

procedure TJSEventInit._Setcomposed(const aValue : Boolean);
begin
  WriteJSPropertyBoolean('composed',aValue);
end;

constructor TJSEventInit.create(const aDict : TJSEventInitRec); overload;
begin
  Self.bubbles:=aDict.bubbles;
  Self.cancelable:=aDict.cancelable;
  Self.composed:=aDict.composed;
end;

class function TJSEventInit.JSClassName: UnicodeString;
begin
  Result:='Object';
end;

class function TJSEventInit.Cast(const Intf: IJSObject): IJSEventInit;
begin
  Result:=TJSEventInit.JOBCast(Intf);
end;

function TJSHTMLCanvasElement._Getwidth: Cardinal;
begin
  Result:=ReadJSPropertyInt64('width');
end;

function TJSHTMLCanvasElement._Getheight: Cardinal;
begin
  Result:=ReadJSPropertyInt64('height');
end;

procedure TJSHTMLCanvasElement._Setwidth(const aValue : Cardinal);
begin
  WriteJSPropertyDouble('width',aValue);
end;

procedure TJSHTMLCanvasElement._Setheight(const aValue : Cardinal);
begin
  WriteJSPropertyDouble('height',aValue);
end;

constructor TJSHTMLCanvasElement.Create;
begin
  JOBCreate([]);
end;

function TJSHTMLCanvasElement.getContext(const aContextId: UnicodeString; const aContextOptions: Variant): IJSnsISupports;
begin
  Result:=InvokeJSObjectResult('getContext',[aContextId,aContextOptions],TJSnsISupports) as IJSnsISupports;
end;

function TJSHTMLCanvasElement.getContext(const aContextId: UnicodeString): IJSnsISupports;
begin
  Result:=InvokeJSObjectResult('getContext',[aContextId],TJSnsISupports) as IJSnsISupports;
end;

function TJSHTMLCanvasElement.toDataURL(const aType_: UnicodeString; const aEncoderOptions: Variant): UnicodeString;
begin
  Result:=InvokeJSUnicodeStringResult('toDataURL',[aType_,aEncoderOptions]);
end;

function TJSHTMLCanvasElement.toDataURL: UnicodeString;
begin
  Result:=InvokeJSUnicodeStringResult('toDataURL',[]);
end;

function TJSHTMLCanvasElement.toDataURL(const aType_: UnicodeString): UnicodeString;
begin
  Result:=InvokeJSUnicodeStringResult('toDataURL',[aType_]);
end;

class function TJSHTMLCanvasElement.JSClassName: UnicodeString;
begin
  Result:='HTMLCanvasElement';
end;

class function TJSHTMLCanvasElement.Cast(const Intf: IJSObject): IJSHTMLCanvasElement;
begin
  Result:=TJSHTMLCanvasElement.JOBCast(Intf);
end;

function TJSHTMLDivElement._Getalign: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('align');
end;

procedure TJSHTMLDivElement._Setalign(const aValue : UnicodeString);
begin
  WriteJSPropertyUnicodeString('align',aValue);
end;

constructor TJSHTMLDivElement.Create;
begin
  JOBCreate([]);
end;

class function TJSHTMLDivElement.JSClassName: UnicodeString;
begin
  Result:='HTMLDivElement';
end;

class function TJSHTMLDivElement.Cast(const Intf: IJSObject): IJSHTMLDivElement;
begin
  Result:=TJSHTMLDivElement.JOBCast(Intf);
end;

function TJSHTMLElement._Gettitle: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('title');
end;

function TJSHTMLElement._Getlang: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('lang');
end;

function TJSHTMLElement._Gettranslate: Boolean;
begin
  Result:=ReadJSPropertyBoolean('translate');
end;

function TJSHTMLElement._Getdir: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('dir');
end;

function TJSHTMLElement._GetinnerText: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('innerText');
end;

function TJSHTMLElement._GetouterText: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('outerText');
end;

function TJSHTMLElement._Gethidden: Boolean;
begin
  Result:=ReadJSPropertyBoolean('hidden');
end;

function TJSHTMLElement._Getinert: Boolean;
begin
  Result:=ReadJSPropertyBoolean('inert');
end;

function TJSHTMLElement._GetaccessKey: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('accessKey');
end;

function TJSHTMLElement._GetaccessKeyLabel: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('accessKeyLabel');
end;

function TJSHTMLElement._Getdraggable: Boolean;
begin
  Result:=ReadJSPropertyBoolean('draggable');
end;

function TJSHTMLElement._GetcontentEditable: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('contentEditable');
end;

function TJSHTMLElement._GetisContentEditable: Boolean;
begin
  Result:=ReadJSPropertyBoolean('isContentEditable');
end;

function TJSHTMLElement._Getpopover: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('popover');
end;

function TJSHTMLElement._Getspellcheck: Boolean;
begin
  Result:=ReadJSPropertyBoolean('spellcheck');
end;

function TJSHTMLElement._GetinputMode: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('inputMode');
end;

function TJSHTMLElement._GetenterKeyHint: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('enterKeyHint');
end;

function TJSHTMLElement._Getautocapitalize: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('autocapitalize');
end;

function TJSHTMLElement._Getautocorrect: Boolean;
begin
  Result:=ReadJSPropertyBoolean('autocorrect');
end;

function TJSHTMLElement._Getnonce: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('nonce');
end;

function TJSHTMLElement._GetoffsetParent: IJSElement;
begin
  Result:=ReadJSPropertyObject('offsetParent',TJSElement) as IJSElement;
end;

function TJSHTMLElement._GetoffsetTop: LongInt;
begin
  Result:=ReadJSPropertyLongInt('offsetTop');
end;

function TJSHTMLElement._GetoffsetLeft: LongInt;
begin
  Result:=ReadJSPropertyLongInt('offsetLeft');
end;

function TJSHTMLElement._GetoffsetWidth: LongInt;
begin
  Result:=ReadJSPropertyLongInt('offsetWidth');
end;

function TJSHTMLElement._GetoffsetHeight: LongInt;
begin
  Result:=ReadJSPropertyLongInt('offsetHeight');
end;

function TJSHTMLElement._Getstyle: IJSCSSStyleDeclaration;
begin
  Result:=ReadJSPropertyObject('style',TJSCSSStyleDeclaration) as IJSCSSStyleDeclaration;
end;

procedure TJSHTMLElement._Settitle(const aValue : UnicodeString);
begin
  WriteJSPropertyUnicodeString('title',aValue);
end;

procedure TJSHTMLElement._Setlang(const aValue : UnicodeString);
begin
  WriteJSPropertyUnicodeString('lang',aValue);
end;

procedure TJSHTMLElement._Settranslate(const aValue : Boolean);
begin
  WriteJSPropertyBoolean('translate',aValue);
end;

procedure TJSHTMLElement._Setdir(const aValue : UnicodeString);
begin
  WriteJSPropertyUnicodeString('dir',aValue);
end;

procedure TJSHTMLElement._SetinnerText(const aValue : UnicodeString);
begin
  WriteJSPropertyUnicodeString('innerText',aValue);
end;

procedure TJSHTMLElement._SetouterText(const aValue : UnicodeString);
begin
  WriteJSPropertyUnicodeString('outerText',aValue);
end;

procedure TJSHTMLElement._Sethidden(const aValue : Boolean);
begin
  WriteJSPropertyBoolean('hidden',aValue);
end;

procedure TJSHTMLElement._Setinert(const aValue : Boolean);
begin
  WriteJSPropertyBoolean('inert',aValue);
end;

procedure TJSHTMLElement._SetaccessKey(const aValue : UnicodeString);
begin
  WriteJSPropertyUnicodeString('accessKey',aValue);
end;

procedure TJSHTMLElement._Setdraggable(const aValue : Boolean);
begin
  WriteJSPropertyBoolean('draggable',aValue);
end;

procedure TJSHTMLElement._SetcontentEditable(const aValue : UnicodeString);
begin
  WriteJSPropertyUnicodeString('contentEditable',aValue);
end;

procedure TJSHTMLElement._Setpopover(const aValue : UnicodeString);
begin
  WriteJSPropertyUnicodeString('popover',aValue);
end;

procedure TJSHTMLElement._Setspellcheck(const aValue : Boolean);
begin
  WriteJSPropertyBoolean('spellcheck',aValue);
end;

procedure TJSHTMLElement._SetinputMode(const aValue : UnicodeString);
begin
  WriteJSPropertyUnicodeString('inputMode',aValue);
end;

procedure TJSHTMLElement._SetenterKeyHint(const aValue : UnicodeString);
begin
  WriteJSPropertyUnicodeString('enterKeyHint',aValue);
end;

procedure TJSHTMLElement._Setautocapitalize(const aValue : UnicodeString);
begin
  WriteJSPropertyUnicodeString('autocapitalize',aValue);
end;

procedure TJSHTMLElement._Setautocorrect(const aValue : Boolean);
begin
  WriteJSPropertyBoolean('autocorrect',aValue);
end;

procedure TJSHTMLElement._Setnonce(const aValue : UnicodeString);
begin
  WriteJSPropertyUnicodeString('nonce',aValue);
end;

constructor TJSHTMLElement.Create;
begin
  JOBCreate([]);
end;

procedure TJSHTMLElement.click;
begin
  InvokeJSNoResult('click',[]);
end;

class function TJSHTMLElement.JSClassName: UnicodeString;
begin
  Result:='HTMLElement';
end;

class function TJSHTMLElement.Cast(const Intf: IJSObject): IJSHTMLElement;
begin
  Result:=TJSHTMLElement.JOBCast(Intf);
end;

class function TJSimgINotificationObserver.JSClassName: UnicodeString;
begin
  Result:='imgINotificationObserver';
end;

class function TJSimgINotificationObserver.Cast(const Intf: IJSObject): IJSimgINotificationObserver;
begin
  Result:=TJSimgINotificationObserver.JOBCast(Intf);
end;

class function TJSimgIRequest.JSClassName: UnicodeString;
begin
  Result:='imgIRequest';
end;

class function TJSimgIRequest.Cast(const Intf: IJSObject): IJSimgIRequest;
begin
  Result:=TJSimgIRequest.JOBCast(Intf);
end;

class function TJSnsIStreamListener.JSClassName: UnicodeString;
begin
  Result:='nsIStreamListener';
end;

class function TJSnsIStreamListener.Cast(const Intf: IJSObject): IJSnsIStreamListener;
begin
  Result:=TJSnsIStreamListener.JOBCast(Intf);
end;

function TJSHTMLImageElement._Getalt: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('alt');
end;

function TJSHTMLImageElement._Getsrc: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('src');
end;

function TJSHTMLImageElement._Getsrcset: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('srcset');
end;

function TJSHTMLImageElement._GetcrossOrigin: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('crossOrigin');
end;

function TJSHTMLImageElement._GetuseMap: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('useMap');
end;

function TJSHTMLImageElement._GetreferrerPolicy: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('referrerPolicy');
end;

function TJSHTMLImageElement._GetisMap: Boolean;
begin
  Result:=ReadJSPropertyBoolean('isMap');
end;

function TJSHTMLImageElement._Getwidth: Cardinal;
begin
  Result:=ReadJSPropertyInt64('width');
end;

function TJSHTMLImageElement._Getheight: Cardinal;
begin
  Result:=ReadJSPropertyInt64('height');
end;

function TJSHTMLImageElement._Getdecoding: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('decoding');
end;

function TJSHTMLImageElement._Getloading: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('loading');
end;

function TJSHTMLImageElement._GetfetchPriority: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('fetchPriority');
end;

function TJSHTMLImageElement._GetnaturalWidth: Cardinal;
begin
  Result:=ReadJSPropertyInt64('naturalWidth');
end;

function TJSHTMLImageElement._GetnaturalHeight: Cardinal;
begin
  Result:=ReadJSPropertyInt64('naturalHeight');
end;

function TJSHTMLImageElement._Getcomplete: Boolean;
begin
  Result:=ReadJSPropertyBoolean('complete');
end;

function TJSHTMLImageElement._Getname: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('name');
end;

function TJSHTMLImageElement._Getalign: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('align');
end;

function TJSHTMLImageElement._Gethspace: Cardinal;
begin
  Result:=ReadJSPropertyInt64('hspace');
end;

function TJSHTMLImageElement._Getvspace: Cardinal;
begin
  Result:=ReadJSPropertyInt64('vspace');
end;

function TJSHTMLImageElement._GetlongDesc: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('longDesc');
end;

function TJSHTMLImageElement._Getborder: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('border');
end;

function TJSHTMLImageElement._Getsizes: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('sizes');
end;

function TJSHTMLImageElement._GetcurrentSrc: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('currentSrc');
end;

function TJSHTMLImageElement._Getlowsrc: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('lowsrc');
end;

function TJSHTMLImageElement._Getx: LongInt;
begin
  Result:=ReadJSPropertyLongInt('x');
end;

function TJSHTMLImageElement._Gety: LongInt;
begin
  Result:=ReadJSPropertyLongInt('y');
end;

procedure TJSHTMLImageElement._Setalt(const aValue : UnicodeString);
begin
  WriteJSPropertyUnicodeString('alt',aValue);
end;

procedure TJSHTMLImageElement._Setsrc(const aValue : UnicodeString);
begin
  WriteJSPropertyUnicodeString('src',aValue);
end;

procedure TJSHTMLImageElement._Setsrcset(const aValue : UnicodeString);
begin
  WriteJSPropertyUnicodeString('srcset',aValue);
end;

procedure TJSHTMLImageElement._SetcrossOrigin(const aValue : UnicodeString);
begin
  WriteJSPropertyUnicodeString('crossOrigin',aValue);
end;

procedure TJSHTMLImageElement._SetuseMap(const aValue : UnicodeString);
begin
  WriteJSPropertyUnicodeString('useMap',aValue);
end;

procedure TJSHTMLImageElement._SetreferrerPolicy(const aValue : UnicodeString);
begin
  WriteJSPropertyUnicodeString('referrerPolicy',aValue);
end;

procedure TJSHTMLImageElement._SetisMap(const aValue : Boolean);
begin
  WriteJSPropertyBoolean('isMap',aValue);
end;

procedure TJSHTMLImageElement._Setwidth(const aValue : Cardinal);
begin
  WriteJSPropertyDouble('width',aValue);
end;

procedure TJSHTMLImageElement._Setheight(const aValue : Cardinal);
begin
  WriteJSPropertyDouble('height',aValue);
end;

procedure TJSHTMLImageElement._Setdecoding(const aValue : UnicodeString);
begin
  WriteJSPropertyUnicodeString('decoding',aValue);
end;

procedure TJSHTMLImageElement._Setloading(const aValue : UnicodeString);
begin
  WriteJSPropertyUnicodeString('loading',aValue);
end;

procedure TJSHTMLImageElement._SetfetchPriority(const aValue : UnicodeString);
begin
  WriteJSPropertyUnicodeString('fetchPriority',aValue);
end;

procedure TJSHTMLImageElement._Setname(const aValue : UnicodeString);
begin
  WriteJSPropertyUnicodeString('name',aValue);
end;

procedure TJSHTMLImageElement._Setalign(const aValue : UnicodeString);
begin
  WriteJSPropertyUnicodeString('align',aValue);
end;

procedure TJSHTMLImageElement._Sethspace(const aValue : Cardinal);
begin
  WriteJSPropertyDouble('hspace',aValue);
end;

procedure TJSHTMLImageElement._Setvspace(const aValue : Cardinal);
begin
  WriteJSPropertyDouble('vspace',aValue);
end;

procedure TJSHTMLImageElement._SetlongDesc(const aValue : UnicodeString);
begin
  WriteJSPropertyUnicodeString('longDesc',aValue);
end;

procedure TJSHTMLImageElement._Setborder(const aValue : UnicodeString);
begin
  WriteJSPropertyUnicodeString('border',aValue);
end;

procedure TJSHTMLImageElement._Setsizes(const aValue : UnicodeString);
begin
  WriteJSPropertyUnicodeString('sizes',aValue);
end;

procedure TJSHTMLImageElement._Setlowsrc(const aValue : UnicodeString);
begin
  WriteJSPropertyUnicodeString('lowsrc',aValue);
end;

constructor TJSHTMLImageElement.Create;
begin
  JOBCreate([]);
end;

function TJSHTMLImageElement.decode: IJSPromise; // Promise<undefined>
begin
  Result:=InvokeJSObjectResult('decode',[],TJSPromise) as IJSPromise;
end;

class function TJSHTMLImageElement.JSClassName: UnicodeString;
begin
  Result:='HTMLImageElement';
end;

class function TJSHTMLImageElement.Cast(const Intf: IJSObject): IJSHTMLImageElement;
begin
  Result:=TJSHTMLImageElement.JOBCast(Intf);
end;

class function TJSMozImageLoadingContent.JSClassName: UnicodeString;
begin
  Result:='MozImageLoadingContent';
end;

class function TJSMozImageLoadingContent.Cast(const Intf: IJSObject): IJSMozImageLoadingContent;
begin
  Result:=TJSMozImageLoadingContent.JOBCast(Intf);
end;

function TJSHTMLMediaElement._Getsrc: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('src');
end;

function TJSHTMLMediaElement._GetcurrentSrc: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('currentSrc');
end;

function TJSHTMLMediaElement._GetcrossOrigin: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('crossOrigin');
end;

function TJSHTMLMediaElement._GetnetworkState: Word;
begin
  Result:=ReadJSPropertyLongInt('networkState');
end;

function TJSHTMLMediaElement._Getpreload: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('preload');
end;

procedure TJSHTMLMediaElement._Setsrc(const aValue : UnicodeString);
begin
  WriteJSPropertyUnicodeString('src',aValue);
end;

procedure TJSHTMLMediaElement._SetcrossOrigin(const aValue : UnicodeString);
begin
  WriteJSPropertyUnicodeString('crossOrigin',aValue);
end;

procedure TJSHTMLMediaElement._Setpreload(const aValue : UnicodeString);
begin
  WriteJSPropertyUnicodeString('preload',aValue);
end;

class function TJSHTMLMediaElement.JSClassName: UnicodeString;
begin
  Result:='HTMLMediaElement';
end;

class function TJSHTMLMediaElement.Cast(const Intf: IJSObject): IJSHTMLMediaElement;
begin
  Result:=TJSHTMLMediaElement.JOBCast(Intf);
end;

constructor TJSHTMLSpanElement.Create;
begin
  JOBCreate([]);
end;

class function TJSHTMLSpanElement.JSClassName: UnicodeString;
begin
  Result:='HTMLSpanElement';
end;

class function TJSHTMLSpanElement.Cast(const Intf: IJSObject): IJSHTMLSpanElement;
begin
  Result:=TJSHTMLSpanElement.JOBCast(Intf);
end;

function TJSHTMLVideoElement._Getwidth: Cardinal;
begin
  Result:=ReadJSPropertyInt64('width');
end;

function TJSHTMLVideoElement._Getheight: Cardinal;
begin
  Result:=ReadJSPropertyInt64('height');
end;

function TJSHTMLVideoElement._GetvideoWidth: Cardinal;
begin
  Result:=ReadJSPropertyInt64('videoWidth');
end;

function TJSHTMLVideoElement._GetvideoHeight: Cardinal;
begin
  Result:=ReadJSPropertyInt64('videoHeight');
end;

function TJSHTMLVideoElement._Getposter: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('poster');
end;

procedure TJSHTMLVideoElement._Setwidth(const aValue : Cardinal);
begin
  WriteJSPropertyDouble('width',aValue);
end;

procedure TJSHTMLVideoElement._Setheight(const aValue : Cardinal);
begin
  WriteJSPropertyDouble('height',aValue);
end;

procedure TJSHTMLVideoElement._Setposter(const aValue : UnicodeString);
begin
  WriteJSPropertyUnicodeString('poster',aValue);
end;

constructor TJSHTMLVideoElement.Create;
begin
  JOBCreate([]);
end;

class function TJSHTMLVideoElement.JSClassName: UnicodeString;
begin
  Result:='HTMLVideoElement';
end;

class function TJSHTMLVideoElement.Cast(const Intf: IJSObject): IJSHTMLVideoElement;
begin
  Result:=TJSHTMLVideoElement.JOBCast(Intf);
end;

function TJSImageBitmap._Getwidth: Cardinal;
begin
  Result:=ReadJSPropertyInt64('width');
end;

function TJSImageBitmap._Getheight: Cardinal;
begin
  Result:=ReadJSPropertyInt64('height');
end;

class function TJSImageBitmap.JSClassName: UnicodeString;
begin
  Result:='ImageBitmap';
end;

class function TJSImageBitmap.Cast(const Intf: IJSObject): IJSImageBitmap;
begin
  Result:=TJSImageBitmap.JOBCast(Intf);
end;

function TJSImageData._Getwidth: Cardinal;
begin
  Result:=ReadJSPropertyInt64('width');
end;

function TJSImageData._Getheight: Cardinal;
begin
  Result:=ReadJSPropertyInt64('height');
end;

function TJSImageData._Getdata: IJSUint8ClampedArray;
begin
  Result:=ReadJSPropertyObject('data',TJSUint8ClampedArray) as IJSUint8ClampedArray;
end;

constructor TJSImageData.Create(aSw: Cardinal; aSh: Cardinal);
begin
  JOBCreate([aSw,aSh]);
end;

constructor TJSImageData.Create(aData: IJSUint8ClampedArray; aSw: Cardinal; aSh: Cardinal);
begin
  JOBCreate([aData,aSw,aSh]);
end;

constructor TJSImageData.Create(aData: IJSUint8ClampedArray; aSw: Cardinal);
begin
  JOBCreate([aData,aSw]);
end;

class function TJSImageData.JSClassName: UnicodeString;
begin
  Result:='ImageData';
end;

class function TJSImageData.Cast(const Intf: IJSObject): IJSImageData;
begin
  Result:=TJSImageData.JOBCast(Intf);
end;

function TJSKeyboardEvent._GetcharCode: Cardinal;
begin
  Result:=ReadJSPropertyInt64('charCode');
end;

function TJSKeyboardEvent._GetkeyCode: Cardinal;
begin
  Result:=ReadJSPropertyInt64('keyCode');
end;

function TJSKeyboardEvent._GetaltKey: Boolean;
begin
  Result:=ReadJSPropertyBoolean('altKey');
end;

function TJSKeyboardEvent._GetctrlKey: Boolean;
begin
  Result:=ReadJSPropertyBoolean('ctrlKey');
end;

function TJSKeyboardEvent._GetshiftKey: Boolean;
begin
  Result:=ReadJSPropertyBoolean('shiftKey');
end;

function TJSKeyboardEvent._GetmetaKey: Boolean;
begin
  Result:=ReadJSPropertyBoolean('metaKey');
end;

function TJSKeyboardEvent._Getlocation: Cardinal;
begin
  Result:=ReadJSPropertyInt64('location');
end;

function TJSKeyboardEvent._Getrepeat_: Boolean;
begin
  Result:=ReadJSPropertyBoolean('repeat');
end;

function TJSKeyboardEvent._GetisComposing: Boolean;
begin
  Result:=ReadJSPropertyBoolean('isComposing');
end;

function TJSKeyboardEvent._Getkey: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('key');
end;

function TJSKeyboardEvent._Getcode: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('code');
end;

constructor TJSKeyboardEvent.Create(const aTypeArg: UnicodeString; const aKeyboardEventInitDict: IJSKeyboardEventInit);
begin
  JOBCreate([aTypeArg,aKeyboardEventInitDict]);
end;

constructor TJSKeyboardEvent.Create(const aTypeArg: UnicodeString);
begin
  JOBCreate([aTypeArg]);
end;

function TJSKeyboardEvent.getModifierState(const aKey: UnicodeString): Boolean;
begin
  Result:=InvokeJSBooleanResult('getModifierState',[aKey]);
end;

procedure TJSKeyboardEvent.initKeyboardEvent(const aTypeArg: UnicodeString; aBubblesArg: Boolean; aCancelableArg: Boolean; aViewArg: IJSWindow; const aKeyArg: UnicodeString; aLocationArg: Cardinal; aCtrlKey: Boolean; aAltKey: Boolean; aShiftKey: Boolean; aMetaKey: Boolean);
begin
  InvokeJSNoResult('initKeyboardEvent',[aTypeArg,aBubblesArg,aCancelableArg,aViewArg,aKeyArg,aLocationArg,aCtrlKey,aAltKey,aShiftKey,aMetaKey]);
end;

procedure TJSKeyboardEvent.initKeyboardEvent(const aTypeArg: UnicodeString);
begin
  InvokeJSNoResult('initKeyboardEvent',[aTypeArg]);
end;

procedure TJSKeyboardEvent.initKeyboardEvent(const aTypeArg: UnicodeString; aBubblesArg: Boolean);
begin
  InvokeJSNoResult('initKeyboardEvent',[aTypeArg,aBubblesArg]);
end;

procedure TJSKeyboardEvent.initKeyboardEvent(const aTypeArg: UnicodeString; aBubblesArg: Boolean; aCancelableArg: Boolean);
begin
  InvokeJSNoResult('initKeyboardEvent',[aTypeArg,aBubblesArg,aCancelableArg]);
end;

procedure TJSKeyboardEvent.initKeyboardEvent(const aTypeArg: UnicodeString; aBubblesArg: Boolean; aCancelableArg: Boolean; aViewArg: IJSWindow);
begin
  InvokeJSNoResult('initKeyboardEvent',[aTypeArg,aBubblesArg,aCancelableArg,aViewArg]);
end;

procedure TJSKeyboardEvent.initKeyboardEvent(const aTypeArg: UnicodeString; aBubblesArg: Boolean; aCancelableArg: Boolean; aViewArg: IJSWindow; const aKeyArg: UnicodeString);
begin
  InvokeJSNoResult('initKeyboardEvent',[aTypeArg,aBubblesArg,aCancelableArg,aViewArg,aKeyArg]);
end;

procedure TJSKeyboardEvent.initKeyboardEvent(const aTypeArg: UnicodeString; aBubblesArg: Boolean; aCancelableArg: Boolean; aViewArg: IJSWindow; const aKeyArg: UnicodeString; aLocationArg: Cardinal);
begin
  InvokeJSNoResult('initKeyboardEvent',[aTypeArg,aBubblesArg,aCancelableArg,aViewArg,aKeyArg,aLocationArg]);
end;

procedure TJSKeyboardEvent.initKeyboardEvent(const aTypeArg: UnicodeString; aBubblesArg: Boolean; aCancelableArg: Boolean; aViewArg: IJSWindow; const aKeyArg: UnicodeString; aLocationArg: Cardinal; aCtrlKey: Boolean);
begin
  InvokeJSNoResult('initKeyboardEvent',[aTypeArg,aBubblesArg,aCancelableArg,aViewArg,aKeyArg,aLocationArg,aCtrlKey]);
end;

procedure TJSKeyboardEvent.initKeyboardEvent(const aTypeArg: UnicodeString; aBubblesArg: Boolean; aCancelableArg: Boolean; aViewArg: IJSWindow; const aKeyArg: UnicodeString; aLocationArg: Cardinal; aCtrlKey: Boolean; aAltKey: Boolean);
begin
  InvokeJSNoResult('initKeyboardEvent',[aTypeArg,aBubblesArg,aCancelableArg,aViewArg,aKeyArg,aLocationArg,aCtrlKey,aAltKey]);
end;

procedure TJSKeyboardEvent.initKeyboardEvent(const aTypeArg: UnicodeString; aBubblesArg: Boolean; aCancelableArg: Boolean; aViewArg: IJSWindow; const aKeyArg: UnicodeString; aLocationArg: Cardinal; aCtrlKey: Boolean; aAltKey: Boolean; aShiftKey: Boolean);
begin
  InvokeJSNoResult('initKeyboardEvent',[aTypeArg,aBubblesArg,aCancelableArg,aViewArg,aKeyArg,aLocationArg,aCtrlKey,aAltKey,aShiftKey]);
end;

class function TJSKeyboardEvent.JSClassName: UnicodeString;
begin
  Result:='KeyboardEvent';
end;

class function TJSKeyboardEvent.Cast(const Intf: IJSObject): IJSKeyboardEvent;
begin
  Result:=TJSKeyboardEvent.JOBCast(Intf);
end;

function TJSKeyboardEventInit._Getkey: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('key');
end;

function TJSKeyboardEventInit._Getcode: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('code');
end;

function TJSKeyboardEventInit._Getlocation: Cardinal;
begin
  Result:=ReadJSPropertyInt64('location');
end;

function TJSKeyboardEventInit._Getrepeat_: Boolean;
begin
  Result:=ReadJSPropertyBoolean('repeat');
end;

function TJSKeyboardEventInit._GetisComposing: Boolean;
begin
  Result:=ReadJSPropertyBoolean('isComposing');
end;

function TJSKeyboardEventInit._GetcharCode: Cardinal;
begin
  Result:=ReadJSPropertyInt64('charCode');
end;

function TJSKeyboardEventInit._GetkeyCode: Cardinal;
begin
  Result:=ReadJSPropertyInt64('keyCode');
end;

function TJSKeyboardEventInit._Getwhich: Cardinal;
begin
  Result:=ReadJSPropertyInt64('which');
end;

procedure TJSKeyboardEventInit._Setkey(const aValue : UnicodeString);
begin
  WriteJSPropertyUnicodeString('key',aValue);
end;

procedure TJSKeyboardEventInit._Setcode(const aValue : UnicodeString);
begin
  WriteJSPropertyUnicodeString('code',aValue);
end;

procedure TJSKeyboardEventInit._Setlocation(const aValue : Cardinal);
begin
  WriteJSPropertyDouble('location',aValue);
end;

procedure TJSKeyboardEventInit._Setrepeat_(const aValue : Boolean);
begin
  WriteJSPropertyBoolean('repeat',aValue);
end;

procedure TJSKeyboardEventInit._SetisComposing(const aValue : Boolean);
begin
  WriteJSPropertyBoolean('isComposing',aValue);
end;

procedure TJSKeyboardEventInit._SetcharCode(const aValue : Cardinal);
begin
  WriteJSPropertyDouble('charCode',aValue);
end;

procedure TJSKeyboardEventInit._SetkeyCode(const aValue : Cardinal);
begin
  WriteJSPropertyDouble('keyCode',aValue);
end;

procedure TJSKeyboardEventInit._Setwhich(const aValue : Cardinal);
begin
  WriteJSPropertyDouble('which',aValue);
end;

constructor TJSKeyboardEventInit.create(const aDict : TJSKeyboardEventInitRec); overload;
begin
  Self.key:=aDict.key;
  Self.code:=aDict.code;
  Self.location:=aDict.location;
  Self.repeat_:=aDict.repeat_;
  Self.isComposing:=aDict.isComposing;
  Self.charCode:=aDict.charCode;
  Self.keyCode:=aDict.keyCode;
  Self.which:=aDict.which;
end;

class function TJSKeyboardEventInit.JSClassName: UnicodeString;
begin
  Result:='Object';
end;

class function TJSKeyboardEventInit.Cast(const Intf: IJSObject): IJSKeyboardEventInit;
begin
  Result:=TJSKeyboardEventInit.JOBCast(Intf);
end;

function TJSMouseEvent._GetscreenX: Double;
begin
  Result:=ReadJSPropertyDouble('screenX');
end;

function TJSMouseEvent._GetscreenY: Double;
begin
  Result:=ReadJSPropertyDouble('screenY');
end;

function TJSMouseEvent._GetpageX: Double;
begin
  Result:=ReadJSPropertyDouble('pageX');
end;

function TJSMouseEvent._GetpageY: Double;
begin
  Result:=ReadJSPropertyDouble('pageY');
end;

function TJSMouseEvent._GetclientX: Double;
begin
  Result:=ReadJSPropertyDouble('clientX');
end;

function TJSMouseEvent._GetclientY: Double;
begin
  Result:=ReadJSPropertyDouble('clientY');
end;

function TJSMouseEvent._Getx: Double;
begin
  Result:=ReadJSPropertyDouble('x');
end;

function TJSMouseEvent._Gety: Double;
begin
  Result:=ReadJSPropertyDouble('y');
end;

function TJSMouseEvent._GetoffsetX: Double;
begin
  Result:=ReadJSPropertyDouble('offsetX');
end;

function TJSMouseEvent._GetoffsetY: Double;
begin
  Result:=ReadJSPropertyDouble('offsetY');
end;

function TJSMouseEvent._GetctrlKey: Boolean;
begin
  Result:=ReadJSPropertyBoolean('ctrlKey');
end;

function TJSMouseEvent._GetshiftKey: Boolean;
begin
  Result:=ReadJSPropertyBoolean('shiftKey');
end;

function TJSMouseEvent._GetaltKey: Boolean;
begin
  Result:=ReadJSPropertyBoolean('altKey');
end;

function TJSMouseEvent._GetmetaKey: Boolean;
begin
  Result:=ReadJSPropertyBoolean('metaKey');
end;

function TJSMouseEvent._Getbutton: SmallInt;
begin
  Result:=ReadJSPropertyLongInt('button');
end;

function TJSMouseEvent._Getbuttons: Word;
begin
  Result:=ReadJSPropertyLongInt('buttons');
end;

function TJSMouseEvent._GetrelatedTarget: IJSEventTarget;
begin
  Result:=ReadJSPropertyObject('relatedTarget',TJSEventTarget) as IJSEventTarget;
end;

function TJSMouseEvent._GetmovementX: LongInt;
begin
  Result:=ReadJSPropertyLongInt('movementX');
end;

function TJSMouseEvent._GetmovementY: LongInt;
begin
  Result:=ReadJSPropertyLongInt('movementY');
end;

function TJSMouseEvent._GetmozPressure: Single;
begin
  Result:=ReadJSPropertyDouble('mozPressure');
end;

function TJSMouseEvent._GetmozInputSource: Word;
begin
  Result:=ReadJSPropertyLongInt('mozInputSource');
end;

constructor TJSMouseEvent.Create(const aTypeArg: UnicodeString; const aMouseEventInitDict: IJSMouseEventInit);
begin
  JOBCreate([aTypeArg,aMouseEventInitDict]);
end;

constructor TJSMouseEvent.Create(const aTypeArg: UnicodeString);
begin
  JOBCreate([aTypeArg]);
end;

procedure TJSMouseEvent.initMouseEvent(const aTypeArg: UnicodeString; aCanBubbleArg: Boolean; aCancelableArg: Boolean; aViewArg: IJSWindow; aDetailArg: LongInt; aScreenXArg: LongInt; aScreenYArg: LongInt; aClientXArg: LongInt; aClientYArg: LongInt; aCtrlKeyArg: Boolean; aAltKeyArg: Boolean; aShiftKeyArg: Boolean; aMetaKeyArg: Boolean; aButtonArg: SmallInt; aRelatedTargetArg: IJSEventTarget);
begin
  InvokeJSNoResult('initMouseEvent',[aTypeArg,aCanBubbleArg,aCancelableArg,aViewArg,aDetailArg,aScreenXArg,aScreenYArg,aClientXArg,aClientYArg,aCtrlKeyArg,aAltKeyArg,aShiftKeyArg,aMetaKeyArg,aButtonArg,aRelatedTargetArg]);
end;

procedure TJSMouseEvent.initMouseEvent(const aTypeArg: UnicodeString);
begin
  InvokeJSNoResult('initMouseEvent',[aTypeArg]);
end;

procedure TJSMouseEvent.initMouseEvent(const aTypeArg: UnicodeString; aCanBubbleArg: Boolean);
begin
  InvokeJSNoResult('initMouseEvent',[aTypeArg,aCanBubbleArg]);
end;

procedure TJSMouseEvent.initMouseEvent(const aTypeArg: UnicodeString; aCanBubbleArg: Boolean; aCancelableArg: Boolean);
begin
  InvokeJSNoResult('initMouseEvent',[aTypeArg,aCanBubbleArg,aCancelableArg]);
end;

procedure TJSMouseEvent.initMouseEvent(const aTypeArg: UnicodeString; aCanBubbleArg: Boolean; aCancelableArg: Boolean; aViewArg: IJSWindow);
begin
  InvokeJSNoResult('initMouseEvent',[aTypeArg,aCanBubbleArg,aCancelableArg,aViewArg]);
end;

procedure TJSMouseEvent.initMouseEvent(const aTypeArg: UnicodeString; aCanBubbleArg: Boolean; aCancelableArg: Boolean; aViewArg: IJSWindow; aDetailArg: LongInt);
begin
  InvokeJSNoResult('initMouseEvent',[aTypeArg,aCanBubbleArg,aCancelableArg,aViewArg,aDetailArg]);
end;

procedure TJSMouseEvent.initMouseEvent(const aTypeArg: UnicodeString; aCanBubbleArg: Boolean; aCancelableArg: Boolean; aViewArg: IJSWindow; aDetailArg: LongInt; aScreenXArg: LongInt);
begin
  InvokeJSNoResult('initMouseEvent',[aTypeArg,aCanBubbleArg,aCancelableArg,aViewArg,aDetailArg,aScreenXArg]);
end;

procedure TJSMouseEvent.initMouseEvent(const aTypeArg: UnicodeString; aCanBubbleArg: Boolean; aCancelableArg: Boolean; aViewArg: IJSWindow; aDetailArg: LongInt; aScreenXArg: LongInt; aScreenYArg: LongInt);
begin
  InvokeJSNoResult('initMouseEvent',[aTypeArg,aCanBubbleArg,aCancelableArg,aViewArg,aDetailArg,aScreenXArg,aScreenYArg]);
end;

procedure TJSMouseEvent.initMouseEvent(const aTypeArg: UnicodeString; aCanBubbleArg: Boolean; aCancelableArg: Boolean; aViewArg: IJSWindow; aDetailArg: LongInt; aScreenXArg: LongInt; aScreenYArg: LongInt; aClientXArg: LongInt);
begin
  InvokeJSNoResult('initMouseEvent',[aTypeArg,aCanBubbleArg,aCancelableArg,aViewArg,aDetailArg,aScreenXArg,aScreenYArg,aClientXArg]);
end;

procedure TJSMouseEvent.initMouseEvent(const aTypeArg: UnicodeString; aCanBubbleArg: Boolean; aCancelableArg: Boolean; aViewArg: IJSWindow; aDetailArg: LongInt; aScreenXArg: LongInt; aScreenYArg: LongInt; aClientXArg: LongInt; aClientYArg: LongInt);
begin
  InvokeJSNoResult('initMouseEvent',[aTypeArg,aCanBubbleArg,aCancelableArg,aViewArg,aDetailArg,aScreenXArg,aScreenYArg,aClientXArg,aClientYArg]);
end;

procedure TJSMouseEvent.initMouseEvent(const aTypeArg: UnicodeString; aCanBubbleArg: Boolean; aCancelableArg: Boolean; aViewArg: IJSWindow; aDetailArg: LongInt; aScreenXArg: LongInt; aScreenYArg: LongInt; aClientXArg: LongInt; aClientYArg: LongInt; aCtrlKeyArg: Boolean);
begin
  InvokeJSNoResult('initMouseEvent',[aTypeArg,aCanBubbleArg,aCancelableArg,aViewArg,aDetailArg,aScreenXArg,aScreenYArg,aClientXArg,aClientYArg,aCtrlKeyArg]);
end;

procedure TJSMouseEvent.initMouseEvent(const aTypeArg: UnicodeString; aCanBubbleArg: Boolean; aCancelableArg: Boolean; aViewArg: IJSWindow; aDetailArg: LongInt; aScreenXArg: LongInt; aScreenYArg: LongInt; aClientXArg: LongInt; aClientYArg: LongInt; aCtrlKeyArg: Boolean; aAltKeyArg: Boolean);
begin
  InvokeJSNoResult('initMouseEvent',[aTypeArg,aCanBubbleArg,aCancelableArg,aViewArg,aDetailArg,aScreenXArg,aScreenYArg,aClientXArg,aClientYArg,aCtrlKeyArg,aAltKeyArg]);
end;

procedure TJSMouseEvent.initMouseEvent(const aTypeArg: UnicodeString; aCanBubbleArg: Boolean; aCancelableArg: Boolean; aViewArg: IJSWindow; aDetailArg: LongInt; aScreenXArg: LongInt; aScreenYArg: LongInt; aClientXArg: LongInt; aClientYArg: LongInt; aCtrlKeyArg: Boolean; aAltKeyArg: Boolean; aShiftKeyArg: Boolean);
begin
  InvokeJSNoResult('initMouseEvent',[aTypeArg,aCanBubbleArg,aCancelableArg,aViewArg,aDetailArg,aScreenXArg,aScreenYArg,aClientXArg,aClientYArg,aCtrlKeyArg,aAltKeyArg,aShiftKeyArg]);
end;

procedure TJSMouseEvent.initMouseEvent(const aTypeArg: UnicodeString; aCanBubbleArg: Boolean; aCancelableArg: Boolean; aViewArg: IJSWindow; aDetailArg: LongInt; aScreenXArg: LongInt; aScreenYArg: LongInt; aClientXArg: LongInt; aClientYArg: LongInt; aCtrlKeyArg: Boolean; aAltKeyArg: Boolean; aShiftKeyArg: Boolean; aMetaKeyArg: Boolean);
begin
  InvokeJSNoResult('initMouseEvent',[aTypeArg,aCanBubbleArg,aCancelableArg,aViewArg,aDetailArg,aScreenXArg,aScreenYArg,aClientXArg,aClientYArg,aCtrlKeyArg,aAltKeyArg,aShiftKeyArg,aMetaKeyArg]);
end;

procedure TJSMouseEvent.initMouseEvent(const aTypeArg: UnicodeString; aCanBubbleArg: Boolean; aCancelableArg: Boolean; aViewArg: IJSWindow; aDetailArg: LongInt; aScreenXArg: LongInt; aScreenYArg: LongInt; aClientXArg: LongInt; aClientYArg: LongInt; aCtrlKeyArg: Boolean; aAltKeyArg: Boolean; aShiftKeyArg: Boolean; aMetaKeyArg: Boolean; aButtonArg: SmallInt);
begin
  InvokeJSNoResult('initMouseEvent',[aTypeArg,aCanBubbleArg,aCancelableArg,aViewArg,aDetailArg,aScreenXArg,aScreenYArg,aClientXArg,aClientYArg,aCtrlKeyArg,aAltKeyArg,aShiftKeyArg,aMetaKeyArg,aButtonArg]);
end;

function TJSMouseEvent.getModifierState(const aKeyArg: UnicodeString): Boolean;
begin
  Result:=InvokeJSBooleanResult('getModifierState',[aKeyArg]);
end;

procedure TJSMouseEvent.initNSMouseEvent(const aTypeArg: UnicodeString; aCanBubbleArg: Boolean; aCancelableArg: Boolean; aViewArg: IJSWindow; aDetailArg: LongInt; aScreenXArg: LongInt; aScreenYArg: LongInt; aClientXArg: LongInt; aClientYArg: LongInt; aCtrlKeyArg: Boolean; aAltKeyArg: Boolean; aShiftKeyArg: Boolean; aMetaKeyArg: Boolean; aButtonArg: SmallInt; aRelatedTargetArg: IJSEventTarget; aPressure: Single; aInputSourceArg: Word);
begin
  InvokeJSNoResult('initNSMouseEvent',[aTypeArg,aCanBubbleArg,aCancelableArg,aViewArg,aDetailArg,aScreenXArg,aScreenYArg,aClientXArg,aClientYArg,aCtrlKeyArg,aAltKeyArg,aShiftKeyArg,aMetaKeyArg,aButtonArg,aRelatedTargetArg,aPressure,aInputSourceArg]);
end;

procedure TJSMouseEvent.initNSMouseEvent(const aTypeArg: UnicodeString);
begin
  InvokeJSNoResult('initNSMouseEvent',[aTypeArg]);
end;

procedure TJSMouseEvent.initNSMouseEvent(const aTypeArg: UnicodeString; aCanBubbleArg: Boolean);
begin
  InvokeJSNoResult('initNSMouseEvent',[aTypeArg,aCanBubbleArg]);
end;

procedure TJSMouseEvent.initNSMouseEvent(const aTypeArg: UnicodeString; aCanBubbleArg: Boolean; aCancelableArg: Boolean);
begin
  InvokeJSNoResult('initNSMouseEvent',[aTypeArg,aCanBubbleArg,aCancelableArg]);
end;

procedure TJSMouseEvent.initNSMouseEvent(const aTypeArg: UnicodeString; aCanBubbleArg: Boolean; aCancelableArg: Boolean; aViewArg: IJSWindow);
begin
  InvokeJSNoResult('initNSMouseEvent',[aTypeArg,aCanBubbleArg,aCancelableArg,aViewArg]);
end;

procedure TJSMouseEvent.initNSMouseEvent(const aTypeArg: UnicodeString; aCanBubbleArg: Boolean; aCancelableArg: Boolean; aViewArg: IJSWindow; aDetailArg: LongInt);
begin
  InvokeJSNoResult('initNSMouseEvent',[aTypeArg,aCanBubbleArg,aCancelableArg,aViewArg,aDetailArg]);
end;

procedure TJSMouseEvent.initNSMouseEvent(const aTypeArg: UnicodeString; aCanBubbleArg: Boolean; aCancelableArg: Boolean; aViewArg: IJSWindow; aDetailArg: LongInt; aScreenXArg: LongInt);
begin
  InvokeJSNoResult('initNSMouseEvent',[aTypeArg,aCanBubbleArg,aCancelableArg,aViewArg,aDetailArg,aScreenXArg]);
end;

procedure TJSMouseEvent.initNSMouseEvent(const aTypeArg: UnicodeString; aCanBubbleArg: Boolean; aCancelableArg: Boolean; aViewArg: IJSWindow; aDetailArg: LongInt; aScreenXArg: LongInt; aScreenYArg: LongInt);
begin
  InvokeJSNoResult('initNSMouseEvent',[aTypeArg,aCanBubbleArg,aCancelableArg,aViewArg,aDetailArg,aScreenXArg,aScreenYArg]);
end;

procedure TJSMouseEvent.initNSMouseEvent(const aTypeArg: UnicodeString; aCanBubbleArg: Boolean; aCancelableArg: Boolean; aViewArg: IJSWindow; aDetailArg: LongInt; aScreenXArg: LongInt; aScreenYArg: LongInt; aClientXArg: LongInt);
begin
  InvokeJSNoResult('initNSMouseEvent',[aTypeArg,aCanBubbleArg,aCancelableArg,aViewArg,aDetailArg,aScreenXArg,aScreenYArg,aClientXArg]);
end;

procedure TJSMouseEvent.initNSMouseEvent(const aTypeArg: UnicodeString; aCanBubbleArg: Boolean; aCancelableArg: Boolean; aViewArg: IJSWindow; aDetailArg: LongInt; aScreenXArg: LongInt; aScreenYArg: LongInt; aClientXArg: LongInt; aClientYArg: LongInt);
begin
  InvokeJSNoResult('initNSMouseEvent',[aTypeArg,aCanBubbleArg,aCancelableArg,aViewArg,aDetailArg,aScreenXArg,aScreenYArg,aClientXArg,aClientYArg]);
end;

procedure TJSMouseEvent.initNSMouseEvent(const aTypeArg: UnicodeString; aCanBubbleArg: Boolean; aCancelableArg: Boolean; aViewArg: IJSWindow; aDetailArg: LongInt; aScreenXArg: LongInt; aScreenYArg: LongInt; aClientXArg: LongInt; aClientYArg: LongInt; aCtrlKeyArg: Boolean);
begin
  InvokeJSNoResult('initNSMouseEvent',[aTypeArg,aCanBubbleArg,aCancelableArg,aViewArg,aDetailArg,aScreenXArg,aScreenYArg,aClientXArg,aClientYArg,aCtrlKeyArg]);
end;

procedure TJSMouseEvent.initNSMouseEvent(const aTypeArg: UnicodeString; aCanBubbleArg: Boolean; aCancelableArg: Boolean; aViewArg: IJSWindow; aDetailArg: LongInt; aScreenXArg: LongInt; aScreenYArg: LongInt; aClientXArg: LongInt; aClientYArg: LongInt; aCtrlKeyArg: Boolean; aAltKeyArg: Boolean);
begin
  InvokeJSNoResult('initNSMouseEvent',[aTypeArg,aCanBubbleArg,aCancelableArg,aViewArg,aDetailArg,aScreenXArg,aScreenYArg,aClientXArg,aClientYArg,aCtrlKeyArg,aAltKeyArg]);
end;

procedure TJSMouseEvent.initNSMouseEvent(const aTypeArg: UnicodeString; aCanBubbleArg: Boolean; aCancelableArg: Boolean; aViewArg: IJSWindow; aDetailArg: LongInt; aScreenXArg: LongInt; aScreenYArg: LongInt; aClientXArg: LongInt; aClientYArg: LongInt; aCtrlKeyArg: Boolean; aAltKeyArg: Boolean; aShiftKeyArg: Boolean);
begin
  InvokeJSNoResult('initNSMouseEvent',[aTypeArg,aCanBubbleArg,aCancelableArg,aViewArg,aDetailArg,aScreenXArg,aScreenYArg,aClientXArg,aClientYArg,aCtrlKeyArg,aAltKeyArg,aShiftKeyArg]);
end;

procedure TJSMouseEvent.initNSMouseEvent(const aTypeArg: UnicodeString; aCanBubbleArg: Boolean; aCancelableArg: Boolean; aViewArg: IJSWindow; aDetailArg: LongInt; aScreenXArg: LongInt; aScreenYArg: LongInt; aClientXArg: LongInt; aClientYArg: LongInt; aCtrlKeyArg: Boolean; aAltKeyArg: Boolean; aShiftKeyArg: Boolean; aMetaKeyArg: Boolean);
begin
  InvokeJSNoResult('initNSMouseEvent',[aTypeArg,aCanBubbleArg,aCancelableArg,aViewArg,aDetailArg,aScreenXArg,aScreenYArg,aClientXArg,aClientYArg,aCtrlKeyArg,aAltKeyArg,aShiftKeyArg,aMetaKeyArg]);
end;

procedure TJSMouseEvent.initNSMouseEvent(const aTypeArg: UnicodeString; aCanBubbleArg: Boolean; aCancelableArg: Boolean; aViewArg: IJSWindow; aDetailArg: LongInt; aScreenXArg: LongInt; aScreenYArg: LongInt; aClientXArg: LongInt; aClientYArg: LongInt; aCtrlKeyArg: Boolean; aAltKeyArg: Boolean; aShiftKeyArg: Boolean; aMetaKeyArg: Boolean; aButtonArg: SmallInt);
begin
  InvokeJSNoResult('initNSMouseEvent',[aTypeArg,aCanBubbleArg,aCancelableArg,aViewArg,aDetailArg,aScreenXArg,aScreenYArg,aClientXArg,aClientYArg,aCtrlKeyArg,aAltKeyArg,aShiftKeyArg,aMetaKeyArg,aButtonArg]);
end;

procedure TJSMouseEvent.initNSMouseEvent(const aTypeArg: UnicodeString; aCanBubbleArg: Boolean; aCancelableArg: Boolean; aViewArg: IJSWindow; aDetailArg: LongInt; aScreenXArg: LongInt; aScreenYArg: LongInt; aClientXArg: LongInt; aClientYArg: LongInt; aCtrlKeyArg: Boolean; aAltKeyArg: Boolean; aShiftKeyArg: Boolean; aMetaKeyArg: Boolean; aButtonArg: SmallInt; aRelatedTargetArg: IJSEventTarget);
begin
  InvokeJSNoResult('initNSMouseEvent',[aTypeArg,aCanBubbleArg,aCancelableArg,aViewArg,aDetailArg,aScreenXArg,aScreenYArg,aClientXArg,aClientYArg,aCtrlKeyArg,aAltKeyArg,aShiftKeyArg,aMetaKeyArg,aButtonArg,aRelatedTargetArg]);
end;

procedure TJSMouseEvent.initNSMouseEvent(const aTypeArg: UnicodeString; aCanBubbleArg: Boolean; aCancelableArg: Boolean; aViewArg: IJSWindow; aDetailArg: LongInt; aScreenXArg: LongInt; aScreenYArg: LongInt; aClientXArg: LongInt; aClientYArg: LongInt; aCtrlKeyArg: Boolean; aAltKeyArg: Boolean; aShiftKeyArg: Boolean; aMetaKeyArg: Boolean; aButtonArg: SmallInt; aRelatedTargetArg: IJSEventTarget; aPressure: Single);
begin
  InvokeJSNoResult('initNSMouseEvent',[aTypeArg,aCanBubbleArg,aCancelableArg,aViewArg,aDetailArg,aScreenXArg,aScreenYArg,aClientXArg,aClientYArg,aCtrlKeyArg,aAltKeyArg,aShiftKeyArg,aMetaKeyArg,aButtonArg,aRelatedTargetArg,aPressure]);
end;

class function TJSMouseEvent.JSClassName: UnicodeString;
begin
  Result:='MouseEvent';
end;

class function TJSMouseEvent.Cast(const Intf: IJSObject): IJSMouseEvent;
begin
  Result:=TJSMouseEvent.JOBCast(Intf);
end;

function TJSMouseEventInit._GetscreenX: Double;
begin
  Result:=ReadJSPropertyDouble('screenX');
end;

function TJSMouseEventInit._GetscreenY: Double;
begin
  Result:=ReadJSPropertyDouble('screenY');
end;

function TJSMouseEventInit._GetclientX: Double;
begin
  Result:=ReadJSPropertyDouble('clientX');
end;

function TJSMouseEventInit._GetclientY: Double;
begin
  Result:=ReadJSPropertyDouble('clientY');
end;

function TJSMouseEventInit._Getbutton: SmallInt;
begin
  Result:=ReadJSPropertyLongInt('button');
end;

function TJSMouseEventInit._Getbuttons: Word;
begin
  Result:=ReadJSPropertyLongInt('buttons');
end;

function TJSMouseEventInit._GetrelatedTarget: IJSEventTarget;
begin
  Result:=ReadJSPropertyObject('relatedTarget',TJSEventTarget) as IJSEventTarget;
end;

function TJSMouseEventInit._GetmovementX: LongInt;
begin
  Result:=ReadJSPropertyLongInt('movementX');
end;

function TJSMouseEventInit._GetmovementY: LongInt;
begin
  Result:=ReadJSPropertyLongInt('movementY');
end;

procedure TJSMouseEventInit._SetscreenX(const aValue : Double);
begin
  WriteJSPropertyDouble('screenX',aValue);
end;

procedure TJSMouseEventInit._SetscreenY(const aValue : Double);
begin
  WriteJSPropertyDouble('screenY',aValue);
end;

procedure TJSMouseEventInit._SetclientX(const aValue : Double);
begin
  WriteJSPropertyDouble('clientX',aValue);
end;

procedure TJSMouseEventInit._SetclientY(const aValue : Double);
begin
  WriteJSPropertyDouble('clientY',aValue);
end;

procedure TJSMouseEventInit._Setbutton(const aValue : SmallInt);
begin
  WriteJSPropertyLongInt('button',aValue);
end;

procedure TJSMouseEventInit._Setbuttons(const aValue : Word);
begin
  WriteJSPropertyLongInt('buttons',aValue);
end;

procedure TJSMouseEventInit._SetrelatedTarget(const aValue : IJSEventTarget);
begin
  WriteJSPropertyObject('relatedTarget',aValue);
end;

procedure TJSMouseEventInit._SetmovementX(const aValue : LongInt);
begin
  WriteJSPropertyLongInt('movementX',aValue);
end;

procedure TJSMouseEventInit._SetmovementY(const aValue : LongInt);
begin
  WriteJSPropertyLongInt('movementY',aValue);
end;

constructor TJSMouseEventInit.create(const aDict : TJSMouseEventInitRec); overload;
begin
  Self.screenX:=aDict.screenX;
  Self.screenY:=aDict.screenY;
  Self.clientX:=aDict.clientX;
  Self.clientY:=aDict.clientY;
  Self.button:=aDict.button;
  Self.buttons:=aDict.buttons;
  Self.relatedTarget:=aDict.relatedTarget;
  Self.movementX:=aDict.movementX;
  Self.movementY:=aDict.movementY;
end;

class function TJSMouseEventInit.JSClassName: UnicodeString;
begin
  Result:='Object';
end;

class function TJSMouseEventInit.Cast(const Intf: IJSObject): IJSMouseEventInit;
begin
  Result:=TJSMouseEventInit.JOBCast(Intf);
end;

function TJSNode._GetnodeType: Word;
begin
  Result:=ReadJSPropertyLongInt('nodeType');
end;

function TJSNode._GetnodeName: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('nodeName');
end;

function TJSNode._GetbaseURI: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('baseURI');
end;

function TJSNode._GetisConnected: Boolean;
begin
  Result:=ReadJSPropertyBoolean('isConnected');
end;

function TJSNode._GetownerDocument: IJSDocument;
begin
  Result:=ReadJSPropertyObject('ownerDocument',TJSDocument) as IJSDocument;
end;

function TJSNode._GetparentNode: IJSNode;
begin
  Result:=ReadJSPropertyObject('parentNode',TJSNode) as IJSNode;
end;

function TJSNode._GetparentElement: IJSElement;
begin
  Result:=ReadJSPropertyObject('parentElement',TJSElement) as IJSElement;
end;

function TJSNode._GetfirstChild: IJSNode;
begin
  Result:=ReadJSPropertyObject('firstChild',TJSNode) as IJSNode;
end;

function TJSNode._GetlastChild: IJSNode;
begin
  Result:=ReadJSPropertyObject('lastChild',TJSNode) as IJSNode;
end;

function TJSNode._GetpreviousSibling: IJSNode;
begin
  Result:=ReadJSPropertyObject('previousSibling',TJSNode) as IJSNode;
end;

function TJSNode._GetnextSibling: IJSNode;
begin
  Result:=ReadJSPropertyObject('nextSibling',TJSNode) as IJSNode;
end;

function TJSNode._GetnodeValue: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('nodeValue');
end;

function TJSNode._GettextContent: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('textContent');
end;

procedure TJSNode._SetnodeValue(const aValue : UnicodeString);
begin
  WriteJSPropertyUnicodeString('nodeValue',aValue);
end;

procedure TJSNode._SettextContent(const aValue : UnicodeString);
begin
  WriteJSPropertyUnicodeString('textContent',aValue);
end;

function TJSNode.hasChildNodes: Boolean;
begin
  Result:=InvokeJSBooleanResult('hasChildNodes',[]);
end;

function TJSNode.insertBefore(aNode: IJSNode; aChild: IJSNode): IJSNode;
begin
  Result:=InvokeJSObjectResult('insertBefore',[aNode,aChild],TJSNode) as IJSNode;
end;

function TJSNode.appendChild(aNode: IJSNode): IJSNode;
begin
  Result:=InvokeJSObjectResult('appendChild',[aNode],TJSNode) as IJSNode;
end;

function TJSNode.replaceChild(aNode: IJSNode; aChild: IJSNode): IJSNode;
begin
  Result:=InvokeJSObjectResult('replaceChild',[aNode,aChild],TJSNode) as IJSNode;
end;

function TJSNode.removeChild(aChild: IJSNode): IJSNode;
begin
  Result:=InvokeJSObjectResult('removeChild',[aChild],TJSNode) as IJSNode;
end;

procedure TJSNode.normalize;
begin
  InvokeJSNoResult('normalize',[]);
end;

function TJSNode.cloneNode(aDeep: Boolean): IJSNode;
begin
  Result:=InvokeJSObjectResult('cloneNode',[aDeep],TJSNode) as IJSNode;
end;

function TJSNode.cloneNode: IJSNode;
begin
  Result:=InvokeJSObjectResult('cloneNode',[],TJSNode) as IJSNode;
end;

function TJSNode.isSameNode(aNode: IJSNode): Boolean;
begin
  Result:=InvokeJSBooleanResult('isSameNode',[aNode]);
end;

function TJSNode.isEqualNode(aNode: IJSNode): Boolean;
begin
  Result:=InvokeJSBooleanResult('isEqualNode',[aNode]);
end;

function TJSNode.compareDocumentPosition(aOther: IJSNode): Word;
begin
  Result:=InvokeJSLongIntResult('compareDocumentPosition',[aOther]);
end;

function TJSNode.contains(aOther: IJSNode): Boolean;
begin
  Result:=InvokeJSBooleanResult('contains',[aOther]);
end;

function TJSNode.lookupPrefix(const aNamespace: UnicodeString): UnicodeString;
begin
  Result:=InvokeJSUnicodeStringResult('lookupPrefix',[aNamespace]);
end;

function TJSNode.lookupNamespaceURI(const aPrefix: UnicodeString): UnicodeString;
begin
  Result:=InvokeJSUnicodeStringResult('lookupNamespaceURI',[aPrefix]);
end;

function TJSNode.isDefaultNamespace(const aNamespace: UnicodeString): Boolean;
begin
  Result:=InvokeJSBooleanResult('isDefaultNamespace',[aNamespace]);
end;

class function TJSNode.JSClassName: UnicodeString;
begin
  Result:='Node';
end;

class function TJSNode.Cast(const Intf: IJSObject): IJSNode;
begin
  Result:=TJSNode.JOBCast(Intf);
end;

function TJSNonElementParentNode.getElementById(const aElementId: UnicodeString): IJSElement;
begin
  Result:=InvokeJSObjectResult('getElementById',[aElementId],TJSElement) as IJSElement;
end;

class function TJSNonElementParentNode.JSClassName: UnicodeString;
begin
  Result:='NonElementParentNode';
end;

class function TJSNonElementParentNode.Cast(const Intf: IJSObject): IJSNonElementParentNode;
begin
  Result:=TJSNonElementParentNode.JOBCast(Intf);
end;

function TJSImageEncodeOptions._Gettype_: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('type');
end;

function TJSImageEncodeOptions._Getquality: Double;
begin
  Result:=ReadJSPropertyDouble('quality');
end;

procedure TJSImageEncodeOptions._Settype_(const aValue : UnicodeString);
begin
  WriteJSPropertyUnicodeString('type',aValue);
end;

procedure TJSImageEncodeOptions._Setquality(const aValue : Double);
begin
  WriteJSPropertyDouble('quality',aValue);
end;

constructor TJSImageEncodeOptions.create(const aDict : TJSImageEncodeOptionsRec); overload;
begin
  Self.type_:=aDict.type_;
  Self.quality:=aDict.quality;
end;

class function TJSImageEncodeOptions.JSClassName: UnicodeString;
begin
  Result:='Object';
end;

class function TJSImageEncodeOptions.Cast(const Intf: IJSObject): IJSImageEncodeOptions;
begin
  Result:=TJSImageEncodeOptions.JOBCast(Intf);
end;

function TJSOffscreenCanvas._Getwidth: Cardinal;
begin
  Result:=ReadJSPropertyInt64('width');
end;

function TJSOffscreenCanvas._Getheight: Cardinal;
begin
  Result:=ReadJSPropertyInt64('height');
end;

function TJSOffscreenCanvas._Getoncontextlost: TEventHandler;
begin
  Result:=(TEventHandlerNonNull(ReadJSPropertyMethod('oncontextlost')));
end;

function TJSOffscreenCanvas._Getoncontextrestored: TEventHandler;
begin
  Result:=(TEventHandlerNonNull(ReadJSPropertyMethod('oncontextrestored')));
end;

procedure TJSOffscreenCanvas._Setwidth(const aValue : Cardinal);
begin
  WriteJSPropertyDouble('width',aValue);
end;

procedure TJSOffscreenCanvas._Setheight(const aValue : Cardinal);
begin
  WriteJSPropertyDouble('height',aValue);
end;

procedure TJSOffscreenCanvas._Setoncontextlost(const aValue : TEventHandler);
var
  m : TJOB_Method;
begin
  m:=TJOB_Method.create(TMethod(aValue),@JobCallEventHandlerNonNull);
  try
    InvokeJSNoResult('oncontextlost',[m],jiSet);
  finally
    m.free
  end;
end;

procedure TJSOffscreenCanvas._Setoncontextrestored(const aValue : TEventHandler);
var
  m : TJOB_Method;
begin
  m:=TJOB_Method.create(TMethod(aValue),@JobCallEventHandlerNonNull);
  try
    InvokeJSNoResult('oncontextrestored',[m],jiSet);
  finally
    m.free
  end;
end;

constructor TJSOffscreenCanvas.Create(aWidth: Cardinal; aHeight: Cardinal);
begin
  JOBCreate([aWidth,aHeight]);
end;

function TJSOffscreenCanvas.getContext(aContextId: TOffscreenRenderingContextId; const aContextOptions: Variant): TOffscreenRenderingContext;
begin
  Result:=InvokeJSVariantResult('getContext',[aContextId,aContextOptions]);
end;

function TJSOffscreenCanvas.getContext(aContextId: TOffscreenRenderingContextId): TOffscreenRenderingContext;
begin
  Result:=InvokeJSVariantResult('getContext',[aContextId]);
end;

function TJSOffscreenCanvas.transferToImageBitmap: IJSImageBitmap;
begin
  Result:=InvokeJSObjectResult('transferToImageBitmap',[],TJSImageBitmap) as IJSImageBitmap;
end;

class function TJSOffscreenCanvas.JSClassName: UnicodeString;
begin
  Result:='OffscreenCanvas';
end;

class function TJSOffscreenCanvas.Cast(const Intf: IJSObject): IJSOffscreenCanvas;
begin
  Result:=TJSOffscreenCanvas.JOBCast(Intf);
end;

function TJSPointerEvent._GetpointerId: LongInt;
begin
  Result:=ReadJSPropertyLongInt('pointerId');
end;

function TJSPointerEvent._Getwidth: Double;
begin
  Result:=ReadJSPropertyDouble('width');
end;

function TJSPointerEvent._Getheight: Double;
begin
  Result:=ReadJSPropertyDouble('height');
end;

function TJSPointerEvent._Getpressure: Single;
begin
  Result:=ReadJSPropertyDouble('pressure');
end;

function TJSPointerEvent._GettangentialPressure: Single;
begin
  Result:=ReadJSPropertyDouble('tangentialPressure');
end;

function TJSPointerEvent._GettiltX: LongInt;
begin
  Result:=ReadJSPropertyLongInt('tiltX');
end;

function TJSPointerEvent._GettiltY: LongInt;
begin
  Result:=ReadJSPropertyLongInt('tiltY');
end;

function TJSPointerEvent._Gettwist: LongInt;
begin
  Result:=ReadJSPropertyLongInt('twist');
end;

function TJSPointerEvent._GetaltitudeAngle: Double;
begin
  Result:=ReadJSPropertyDouble('altitudeAngle');
end;

function TJSPointerEvent._GetazimuthAngle: Double;
begin
  Result:=ReadJSPropertyDouble('azimuthAngle');
end;

function TJSPointerEvent._GetpointerType: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('pointerType');
end;

function TJSPointerEvent._GetisPrimary: Boolean;
begin
  Result:=ReadJSPropertyBoolean('isPrimary');
end;

constructor TJSPointerEvent.Create(const aType_: UnicodeString; const aEventInitDict: IJSPointerEventInit);
begin
  JOBCreate([aType_,aEventInitDict]);
end;

constructor TJSPointerEvent.Create(const aType_: UnicodeString);
begin
  JOBCreate([aType_]);
end;

function TJSPointerEvent.getCoalescedEvents: TJSPointerEventDynArray;
begin
  Result:=InvokeJSObjectResult('getCoalescedEvents',[],TJSArray) as TJSPointerEventDynArray;
end;

function TJSPointerEvent.getPredictedEvents: TJSPointerEventDynArray;
begin
  Result:=InvokeJSObjectResult('getPredictedEvents',[],TJSArray) as TJSPointerEventDynArray;
end;

class function TJSPointerEvent.JSClassName: UnicodeString;
begin
  Result:='PointerEvent';
end;

class function TJSPointerEvent.Cast(const Intf: IJSObject): IJSPointerEvent;
begin
  Result:=TJSPointerEvent.JOBCast(Intf);
end;

function TJSPointerEventInit._GetpointerId: LongInt;
begin
  Result:=ReadJSPropertyLongInt('pointerId');
end;

function TJSPointerEventInit._Getwidth: Double;
begin
  Result:=ReadJSPropertyDouble('width');
end;

function TJSPointerEventInit._Getheight: Double;
begin
  Result:=ReadJSPropertyDouble('height');
end;

function TJSPointerEventInit._Getpressure: Single;
begin
  Result:=ReadJSPropertyDouble('pressure');
end;

function TJSPointerEventInit._GettangentialPressure: Single;
begin
  Result:=ReadJSPropertyDouble('tangentialPressure');
end;

function TJSPointerEventInit._GettiltX: LongInt;
begin
  Result:=ReadJSPropertyLongInt('tiltX');
end;

function TJSPointerEventInit._GettiltY: LongInt;
begin
  Result:=ReadJSPropertyLongInt('tiltY');
end;

function TJSPointerEventInit._Gettwist: LongInt;
begin
  Result:=ReadJSPropertyLongInt('twist');
end;

function TJSPointerEventInit._GetaltitudeAngle: Double;
begin
  Result:=ReadJSPropertyDouble('altitudeAngle');
end;

function TJSPointerEventInit._GetazimuthAngle: Double;
begin
  Result:=ReadJSPropertyDouble('azimuthAngle');
end;

function TJSPointerEventInit._GetpointerType: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('pointerType');
end;

function TJSPointerEventInit._GetisPrimary: Boolean;
begin
  Result:=ReadJSPropertyBoolean('isPrimary');
end;

function TJSPointerEventInit._GetcoalescedEvents: TJSPointerEventDynArray;
begin
  Result:=ReadJSPropertyObject('coalescedEvents',TJSArray) as TJSPointerEventDynArray;
end;

function TJSPointerEventInit._GetpredictedEvents: TJSPointerEventDynArray;
begin
  Result:=ReadJSPropertyObject('predictedEvents',TJSArray) as TJSPointerEventDynArray;
end;

procedure TJSPointerEventInit._SetpointerId(const aValue : LongInt);
begin
  WriteJSPropertyLongInt('pointerId',aValue);
end;

procedure TJSPointerEventInit._Setwidth(const aValue : Double);
begin
  WriteJSPropertyDouble('width',aValue);
end;

procedure TJSPointerEventInit._Setheight(const aValue : Double);
begin
  WriteJSPropertyDouble('height',aValue);
end;

procedure TJSPointerEventInit._Setpressure(const aValue : Single);
begin
  WriteJSPropertyDouble('pressure',aValue);
end;

procedure TJSPointerEventInit._SettangentialPressure(const aValue : Single);
begin
  WriteJSPropertyDouble('tangentialPressure',aValue);
end;

procedure TJSPointerEventInit._SettiltX(const aValue : LongInt);
begin
  WriteJSPropertyLongInt('tiltX',aValue);
end;

procedure TJSPointerEventInit._SettiltY(const aValue : LongInt);
begin
  WriteJSPropertyLongInt('tiltY',aValue);
end;

procedure TJSPointerEventInit._Settwist(const aValue : LongInt);
begin
  WriteJSPropertyLongInt('twist',aValue);
end;

procedure TJSPointerEventInit._SetaltitudeAngle(const aValue : Double);
begin
  WriteJSPropertyDouble('altitudeAngle',aValue);
end;

procedure TJSPointerEventInit._SetazimuthAngle(const aValue : Double);
begin
  WriteJSPropertyDouble('azimuthAngle',aValue);
end;

procedure TJSPointerEventInit._SetpointerType(const aValue : UnicodeString);
begin
  WriteJSPropertyUnicodeString('pointerType',aValue);
end;

procedure TJSPointerEventInit._SetisPrimary(const aValue : Boolean);
begin
  WriteJSPropertyBoolean('isPrimary',aValue);
end;

procedure TJSPointerEventInit._SetcoalescedEvents(const aValue : TJSPointerEventDynArray);
begin
  WriteJSPropertyObject('coalescedEvents',aValue);
end;

procedure TJSPointerEventInit._SetpredictedEvents(const aValue : TJSPointerEventDynArray);
begin
  WriteJSPropertyObject('predictedEvents',aValue);
end;

constructor TJSPointerEventInit.create(const aDict : TJSPointerEventInitRec); overload;
begin
  Self.pointerId:=aDict.pointerId;
  Self.width:=aDict.width;
  Self.height:=aDict.height;
  Self.pressure:=aDict.pressure;
  Self.tangentialPressure:=aDict.tangentialPressure;
  Self.tiltX:=aDict.tiltX;
  Self.tiltY:=aDict.tiltY;
  Self.twist:=aDict.twist;
  Self.altitudeAngle:=aDict.altitudeAngle;
  Self.azimuthAngle:=aDict.azimuthAngle;
  Self.pointerType:=aDict.pointerType;
  Self.isPrimary:=aDict.isPrimary;
  Self.coalescedEvents:=aDict.coalescedEvents;
  Self.predictedEvents:=aDict.predictedEvents;
end;

class function TJSPointerEventInit.JSClassName: UnicodeString;
begin
  Result:='Object';
end;

class function TJSPointerEventInit.Cast(const Intf: IJSObject): IJSPointerEventInit;
begin
  Result:=TJSPointerEventInit.JOBCast(Intf);
end;

function TJSUIEvent._Getview: IJSWindowProxy;
begin
  Result:=ReadJSPropertyObject('view',TJSWindowProxy) as IJSWindowProxy;
end;

function TJSUIEvent._Getdetail: LongInt;
begin
  Result:=ReadJSPropertyLongInt('detail');
end;

function TJSUIEvent._GetlayerX: LongInt;
begin
  Result:=ReadJSPropertyLongInt('layerX');
end;

function TJSUIEvent._GetlayerY: LongInt;
begin
  Result:=ReadJSPropertyLongInt('layerY');
end;

function TJSUIEvent._Getwhich: Cardinal;
begin
  Result:=ReadJSPropertyInt64('which');
end;

function TJSUIEvent._GetrangeParent: IJSNode;
begin
  Result:=ReadJSPropertyObject('rangeParent',TJSNode) as IJSNode;
end;

function TJSUIEvent._GetrangeOffset: LongInt;
begin
  Result:=ReadJSPropertyLongInt('rangeOffset');
end;

constructor TJSUIEvent.Create(const aType_: UnicodeString; const aEventInitDict: IJSUIEventInit);
begin
  JOBCreate([aType_,aEventInitDict]);
end;

constructor TJSUIEvent.Create(const aType_: UnicodeString);
begin
  JOBCreate([aType_]);
end;

procedure TJSUIEvent.initUIEvent(const aAType: UnicodeString; aACanBubble: Boolean; aACancelable: Boolean; aAView: IJSWindow; aADetail: LongInt);
begin
  InvokeJSNoResult('initUIEvent',[aAType,aACanBubble,aACancelable,aAView,aADetail]);
end;

procedure TJSUIEvent.initUIEvent(const aAType: UnicodeString);
begin
  InvokeJSNoResult('initUIEvent',[aAType]);
end;

procedure TJSUIEvent.initUIEvent(const aAType: UnicodeString; aACanBubble: Boolean);
begin
  InvokeJSNoResult('initUIEvent',[aAType,aACanBubble]);
end;

procedure TJSUIEvent.initUIEvent(const aAType: UnicodeString; aACanBubble: Boolean; aACancelable: Boolean);
begin
  InvokeJSNoResult('initUIEvent',[aAType,aACanBubble,aACancelable]);
end;

procedure TJSUIEvent.initUIEvent(const aAType: UnicodeString; aACanBubble: Boolean; aACancelable: Boolean; aAView: IJSWindow);
begin
  InvokeJSNoResult('initUIEvent',[aAType,aACanBubble,aACancelable,aAView]);
end;

class function TJSUIEvent.JSClassName: UnicodeString;
begin
  Result:='UIEvent';
end;

class function TJSUIEvent.Cast(const Intf: IJSObject): IJSUIEvent;
begin
  Result:=TJSUIEvent.JOBCast(Intf);
end;

function TJSUIEventInit._Getview: IJSWindow;
begin
  Result:=ReadJSPropertyObject('view',TJSWindow) as IJSWindow;
end;

function TJSUIEventInit._Getdetail: LongInt;
begin
  Result:=ReadJSPropertyLongInt('detail');
end;

procedure TJSUIEventInit._Setview(const aValue : IJSWindow);
begin
  WriteJSPropertyObject('view',aValue);
end;

procedure TJSUIEventInit._Setdetail(const aValue : LongInt);
begin
  WriteJSPropertyLongInt('detail',aValue);
end;

constructor TJSUIEventInit.create(const aDict : TJSUIEventInitRec); overload;
begin
  Self.view:=aDict.view;
  Self.detail:=aDict.detail;
end;

class function TJSUIEventInit.JSClassName: UnicodeString;
begin
  Result:='Object';
end;

class function TJSUIEventInit.Cast(const Intf: IJSObject): IJSUIEventInit;
begin
  Result:=TJSUIEventInit.JOBCast(Intf);
end;

function TJSEventModifierInit._GetctrlKey: Boolean;
begin
  Result:=ReadJSPropertyBoolean('ctrlKey');
end;

function TJSEventModifierInit._GetshiftKey: Boolean;
begin
  Result:=ReadJSPropertyBoolean('shiftKey');
end;

function TJSEventModifierInit._GetaltKey: Boolean;
begin
  Result:=ReadJSPropertyBoolean('altKey');
end;

function TJSEventModifierInit._GetmetaKey: Boolean;
begin
  Result:=ReadJSPropertyBoolean('metaKey');
end;

function TJSEventModifierInit._GetmodifierAltGraph: Boolean;
begin
  Result:=ReadJSPropertyBoolean('modifierAltGraph');
end;

function TJSEventModifierInit._GetmodifierCapsLock: Boolean;
begin
  Result:=ReadJSPropertyBoolean('modifierCapsLock');
end;

function TJSEventModifierInit._GetmodifierFn: Boolean;
begin
  Result:=ReadJSPropertyBoolean('modifierFn');
end;

function TJSEventModifierInit._GetmodifierFnLock: Boolean;
begin
  Result:=ReadJSPropertyBoolean('modifierFnLock');
end;

function TJSEventModifierInit._GetmodifierNumLock: Boolean;
begin
  Result:=ReadJSPropertyBoolean('modifierNumLock');
end;

function TJSEventModifierInit._GetmodifierOS: Boolean;
begin
  Result:=ReadJSPropertyBoolean('modifierOS');
end;

function TJSEventModifierInit._GetmodifierScrollLock: Boolean;
begin
  Result:=ReadJSPropertyBoolean('modifierScrollLock');
end;

function TJSEventModifierInit._GetmodifierSymbol: Boolean;
begin
  Result:=ReadJSPropertyBoolean('modifierSymbol');
end;

function TJSEventModifierInit._GetmodifierSymbolLock: Boolean;
begin
  Result:=ReadJSPropertyBoolean('modifierSymbolLock');
end;

procedure TJSEventModifierInit._SetctrlKey(const aValue : Boolean);
begin
  WriteJSPropertyBoolean('ctrlKey',aValue);
end;

procedure TJSEventModifierInit._SetshiftKey(const aValue : Boolean);
begin
  WriteJSPropertyBoolean('shiftKey',aValue);
end;

procedure TJSEventModifierInit._SetaltKey(const aValue : Boolean);
begin
  WriteJSPropertyBoolean('altKey',aValue);
end;

procedure TJSEventModifierInit._SetmetaKey(const aValue : Boolean);
begin
  WriteJSPropertyBoolean('metaKey',aValue);
end;

procedure TJSEventModifierInit._SetmodifierAltGraph(const aValue : Boolean);
begin
  WriteJSPropertyBoolean('modifierAltGraph',aValue);
end;

procedure TJSEventModifierInit._SetmodifierCapsLock(const aValue : Boolean);
begin
  WriteJSPropertyBoolean('modifierCapsLock',aValue);
end;

procedure TJSEventModifierInit._SetmodifierFn(const aValue : Boolean);
begin
  WriteJSPropertyBoolean('modifierFn',aValue);
end;

procedure TJSEventModifierInit._SetmodifierFnLock(const aValue : Boolean);
begin
  WriteJSPropertyBoolean('modifierFnLock',aValue);
end;

procedure TJSEventModifierInit._SetmodifierNumLock(const aValue : Boolean);
begin
  WriteJSPropertyBoolean('modifierNumLock',aValue);
end;

procedure TJSEventModifierInit._SetmodifierOS(const aValue : Boolean);
begin
  WriteJSPropertyBoolean('modifierOS',aValue);
end;

procedure TJSEventModifierInit._SetmodifierScrollLock(const aValue : Boolean);
begin
  WriteJSPropertyBoolean('modifierScrollLock',aValue);
end;

procedure TJSEventModifierInit._SetmodifierSymbol(const aValue : Boolean);
begin
  WriteJSPropertyBoolean('modifierSymbol',aValue);
end;

procedure TJSEventModifierInit._SetmodifierSymbolLock(const aValue : Boolean);
begin
  WriteJSPropertyBoolean('modifierSymbolLock',aValue);
end;

constructor TJSEventModifierInit.create(const aDict : TJSEventModifierInitRec); overload;
begin
  Self.ctrlKey:=aDict.ctrlKey;
  Self.shiftKey:=aDict.shiftKey;
  Self.altKey:=aDict.altKey;
  Self.metaKey:=aDict.metaKey;
  Self.modifierAltGraph:=aDict.modifierAltGraph;
  Self.modifierCapsLock:=aDict.modifierCapsLock;
  Self.modifierFn:=aDict.modifierFn;
  Self.modifierFnLock:=aDict.modifierFnLock;
  Self.modifierNumLock:=aDict.modifierNumLock;
  Self.modifierOS:=aDict.modifierOS;
  Self.modifierScrollLock:=aDict.modifierScrollLock;
  Self.modifierSymbol:=aDict.modifierSymbol;
  Self.modifierSymbolLock:=aDict.modifierSymbolLock;
end;

class function TJSEventModifierInit.JSClassName: UnicodeString;
begin
  Result:='Object';
end;

class function TJSEventModifierInit.Cast(const Intf: IJSObject): IJSEventModifierInit;
begin
  Result:=TJSEventModifierInit.JOBCast(Intf);
end;

function TJSVideoFrame._Getformat: TVideoPixelFormat;
begin
  Result:=ReadJSPropertyUnicodeString('format');
end;

function TJSVideoFrame._GetcodedWidth: Cardinal;
begin
  Result:=ReadJSPropertyInt64('codedWidth');
end;

function TJSVideoFrame._GetcodedHeight: Cardinal;
begin
  Result:=ReadJSPropertyInt64('codedHeight');
end;

function TJSVideoFrame._GetdisplayWidth: Cardinal;
begin
  Result:=ReadJSPropertyInt64('displayWidth');
end;

function TJSVideoFrame._GetdisplayHeight: Cardinal;
begin
  Result:=ReadJSPropertyInt64('displayHeight');
end;

function TJSVideoFrame._Getduration: QWord;
begin
  Result:=ReadJSPropertyInt64('duration');
end;

function TJSVideoFrame._Gettimestamp: Int64;
begin
  Result:=ReadJSPropertyInt64('timestamp');
end;

constructor TJSVideoFrame.Create(aImageElement: IJSHTMLImageElement; const aInit: IJSVideoFrameInit);
begin
  JOBCreate([aImageElement,aInit]);
end;

constructor TJSVideoFrame.Create(aImageElement: IJSHTMLImageElement);
begin
  JOBCreate([aImageElement]);
end;

constructor TJSVideoFrame.Create(aCanvasElement: IJSHTMLCanvasElement; const aInit: IJSVideoFrameInit);
begin
  JOBCreate([aCanvasElement,aInit]);
end;

constructor TJSVideoFrame.Create(aCanvasElement: IJSHTMLCanvasElement);
begin
  JOBCreate([aCanvasElement]);
end;

constructor TJSVideoFrame.Create(aVideoElement: IJSHTMLVideoElement; const aInit: IJSVideoFrameInit);
begin
  JOBCreate([aVideoElement,aInit]);
end;

constructor TJSVideoFrame.Create(aVideoElement: IJSHTMLVideoElement);
begin
  JOBCreate([aVideoElement]);
end;

constructor TJSVideoFrame.Create(aOffscreenCanvas: IJSOffscreenCanvas; const aInit: IJSVideoFrameInit);
begin
  JOBCreate([aOffscreenCanvas,aInit]);
end;

constructor TJSVideoFrame.Create(aOffscreenCanvas: IJSOffscreenCanvas);
begin
  JOBCreate([aOffscreenCanvas]);
end;

constructor TJSVideoFrame.Create(aImageBitmap: IJSImageBitmap; const aInit: IJSVideoFrameInit);
begin
  JOBCreate([aImageBitmap,aInit]);
end;

constructor TJSVideoFrame.Create(aImageBitmap: IJSImageBitmap);
begin
  JOBCreate([aImageBitmap]);
end;

constructor TJSVideoFrame.Create(aVideoFrame: IJSVideoFrame; const aInit: IJSVideoFrameInit);
begin
  JOBCreate([aVideoFrame,aInit]);
end;

constructor TJSVideoFrame.Create(aVideoFrame: IJSVideoFrame);
begin
  JOBCreate([aVideoFrame]);
end;

constructor TJSVideoFrame.Create(aBufferView: IJSArrayBufferView; const aInit: IJSVideoFrameBufferInit);
begin
  JOBCreate([aBufferView,aInit]);
end;

constructor TJSVideoFrame.Create(aBuffer: IJSArrayBuffer; const aInit: IJSVideoFrameBufferInit);
begin
  JOBCreate([aBuffer,aInit]);
end;

function TJSVideoFrame.allocationSize(const aOptions: IJSVideoFrameCopyToOptions): Cardinal;
begin
  Result:=InvokeJSLongIntResult('allocationSize',[aOptions]);
end;

function TJSVideoFrame.allocationSize: Cardinal;
begin
  Result:=InvokeJSLongIntResult('allocationSize',[]);
end;

function TJSVideoFrame.copyTo(aDestination: IJSArrayBuffer; const aOptions: IJSVideoFrameCopyToOptions): IJSPromise; // Promise<sequence>
begin
  Result:=InvokeJSObjectResult('copyTo',[aDestination,aOptions],TJSPromise) as IJSPromise;
end;

function TJSVideoFrame.copyTo(aDestination: IJSArrayBufferView; const aOptions: IJSVideoFrameCopyToOptions): IJSPromise; // Promise<sequence>
begin
  Result:=InvokeJSObjectResult('copyTo',[aDestination,aOptions],TJSPromise) as IJSPromise;
end;

function TJSVideoFrame.copyTo(aDestination: IJSArrayBufferView): IJSPromise; // Promise<sequence>
begin
  Result:=InvokeJSObjectResult('copyTo',[aDestination],TJSPromise) as IJSPromise;
end;

function TJSVideoFrame.copyTo(aDestination: IJSArrayBuffer): IJSPromise; // Promise<sequence>
begin
  Result:=InvokeJSObjectResult('copyTo',[aDestination],TJSPromise) as IJSPromise;
end;

function TJSVideoFrame.clone: IJSVideoFrame;
begin
  Result:=InvokeJSObjectResult('clone',[],TJSVideoFrame) as IJSVideoFrame;
end;

procedure TJSVideoFrame.close;
begin
  InvokeJSNoResult('close',[]);
end;

class function TJSVideoFrame.JSClassName: UnicodeString;
begin
  Result:='VideoFrame';
end;

class function TJSVideoFrame.Cast(const Intf: IJSObject): IJSVideoFrame;
begin
  Result:=TJSVideoFrame.JOBCast(Intf);
end;

function TJSVideoFrameInit._Getduration: QWord;
begin
  Result:=ReadJSPropertyInt64('duration');
end;

function TJSVideoFrameInit._Gettimestamp: Int64;
begin
  Result:=ReadJSPropertyInt64('timestamp');
end;

function TJSVideoFrameInit._Getalpha: TAlphaOption;
begin
  Result:=ReadJSPropertyUnicodeString('alpha');
end;

function TJSVideoFrameInit._GetdisplayWidth: Cardinal;
begin
  Result:=ReadJSPropertyInt64('displayWidth');
end;

function TJSVideoFrameInit._GetdisplayHeight: Cardinal;
begin
  Result:=ReadJSPropertyInt64('displayHeight');
end;

procedure TJSVideoFrameInit._Setduration(const aValue : QWord);
begin
  WriteJSPropertyDouble('duration',aValue);
end;

procedure TJSVideoFrameInit._Settimestamp(const aValue : Int64);
begin
  WriteJSPropertyDouble('timestamp',aValue);
end;

procedure TJSVideoFrameInit._Setalpha(const aValue : TAlphaOption);
begin
  WriteJSPropertyUnicodeString('alpha',aValue);
end;

procedure TJSVideoFrameInit._SetdisplayWidth(const aValue : Cardinal);
begin
  WriteJSPropertyDouble('displayWidth',aValue);
end;

procedure TJSVideoFrameInit._SetdisplayHeight(const aValue : Cardinal);
begin
  WriteJSPropertyDouble('displayHeight',aValue);
end;

constructor TJSVideoFrameInit.create(const aDict : TJSVideoFrameInitRec); overload;
begin
  Self.duration:=aDict.duration;
  Self.timestamp:=aDict.timestamp;
  Self.alpha:=aDict.alpha;
  Self.displayWidth:=aDict.displayWidth;
  Self.displayHeight:=aDict.displayHeight;
end;

class function TJSVideoFrameInit.JSClassName: UnicodeString;
begin
  Result:='Object';
end;

class function TJSVideoFrameInit.Cast(const Intf: IJSObject): IJSVideoFrameInit;
begin
  Result:=TJSVideoFrameInit.JOBCast(Intf);
end;

function TJSVideoFrameBufferInit._Getformat: TVideoPixelFormat;
begin
  Result:=ReadJSPropertyUnicodeString('format');
end;

function TJSVideoFrameBufferInit._GetcodedWidth: Cardinal;
begin
  Result:=ReadJSPropertyInt64('codedWidth');
end;

function TJSVideoFrameBufferInit._GetcodedHeight: Cardinal;
begin
  Result:=ReadJSPropertyInt64('codedHeight');
end;

function TJSVideoFrameBufferInit._Gettimestamp: Int64;
begin
  Result:=ReadJSPropertyInt64('timestamp');
end;

function TJSVideoFrameBufferInit._Getduration: QWord;
begin
  Result:=ReadJSPropertyInt64('duration');
end;

function TJSVideoFrameBufferInit._Getlayout: TJSPlaneLayoutDynArray;
begin
  Result:=ReadJSPropertyObject('layout',TJSArray) as TJSPlaneLayoutDynArray;
end;

function TJSVideoFrameBufferInit._GetdisplayWidth: Cardinal;
begin
  Result:=ReadJSPropertyInt64('displayWidth');
end;

function TJSVideoFrameBufferInit._GetdisplayHeight: Cardinal;
begin
  Result:=ReadJSPropertyInt64('displayHeight');
end;

procedure TJSVideoFrameBufferInit._Setformat(const aValue : TVideoPixelFormat);
begin
  WriteJSPropertyUnicodeString('format',aValue);
end;

procedure TJSVideoFrameBufferInit._SetcodedWidth(const aValue : Cardinal);
begin
  WriteJSPropertyDouble('codedWidth',aValue);
end;

procedure TJSVideoFrameBufferInit._SetcodedHeight(const aValue : Cardinal);
begin
  WriteJSPropertyDouble('codedHeight',aValue);
end;

procedure TJSVideoFrameBufferInit._Settimestamp(const aValue : Int64);
begin
  WriteJSPropertyDouble('timestamp',aValue);
end;

procedure TJSVideoFrameBufferInit._Setduration(const aValue : QWord);
begin
  WriteJSPropertyDouble('duration',aValue);
end;

procedure TJSVideoFrameBufferInit._Setlayout(const aValue : TJSPlaneLayoutDynArray);
begin
  WriteJSPropertyObject('layout',aValue);
end;

procedure TJSVideoFrameBufferInit._SetdisplayWidth(const aValue : Cardinal);
begin
  WriteJSPropertyDouble('displayWidth',aValue);
end;

procedure TJSVideoFrameBufferInit._SetdisplayHeight(const aValue : Cardinal);
begin
  WriteJSPropertyDouble('displayHeight',aValue);
end;

constructor TJSVideoFrameBufferInit.create(const aDict : TJSVideoFrameBufferInitRec); overload;
begin
  Self.format:=aDict.format;
  Self.codedWidth:=aDict.codedWidth;
  Self.codedHeight:=aDict.codedHeight;
  Self.timestamp:=aDict.timestamp;
  Self.duration:=aDict.duration;
  Self.layout:=aDict.layout;
  Self.displayWidth:=aDict.displayWidth;
  Self.displayHeight:=aDict.displayHeight;
end;

class function TJSVideoFrameBufferInit.JSClassName: UnicodeString;
begin
  Result:='Object';
end;

class function TJSVideoFrameBufferInit.Cast(const Intf: IJSObject): IJSVideoFrameBufferInit;
begin
  Result:=TJSVideoFrameBufferInit.JOBCast(Intf);
end;

function TJSVideoFrameCopyToOptions._Getlayout: TJSPlaneLayoutDynArray;
begin
  Result:=ReadJSPropertyObject('layout',TJSArray) as TJSPlaneLayoutDynArray;
end;

function TJSVideoFrameCopyToOptions._Getformat: TVideoPixelFormat;
begin
  Result:=ReadJSPropertyUnicodeString('format');
end;

function TJSVideoFrameCopyToOptions._GetcolorSpace: TPredefinedColorSpace;
begin
  Result:=ReadJSPropertyUnicodeString('colorSpace');
end;

procedure TJSVideoFrameCopyToOptions._Setlayout(const aValue : TJSPlaneLayoutDynArray);
begin
  WriteJSPropertyObject('layout',aValue);
end;

procedure TJSVideoFrameCopyToOptions._Setformat(const aValue : TVideoPixelFormat);
begin
  WriteJSPropertyUnicodeString('format',aValue);
end;

procedure TJSVideoFrameCopyToOptions._SetcolorSpace(const aValue : TPredefinedColorSpace);
begin
  WriteJSPropertyUnicodeString('colorSpace',aValue);
end;

constructor TJSVideoFrameCopyToOptions.create(const aDict : TJSVideoFrameCopyToOptionsRec); overload;
begin
  Self.layout:=aDict.layout;
  Self.format:=aDict.format;
  Self.colorSpace:=aDict.colorSpace;
end;

class function TJSVideoFrameCopyToOptions.JSClassName: UnicodeString;
begin
  Result:='Object';
end;

class function TJSVideoFrameCopyToOptions.Cast(const Intf: IJSObject): IJSVideoFrameCopyToOptions;
begin
  Result:=TJSVideoFrameCopyToOptions.JOBCast(Intf);
end;

function TJSPlaneLayout._Getoffset: Cardinal;
begin
  Result:=ReadJSPropertyInt64('offset');
end;

function TJSPlaneLayout._Getstride: Cardinal;
begin
  Result:=ReadJSPropertyInt64('stride');
end;

procedure TJSPlaneLayout._Setoffset(const aValue : Cardinal);
begin
  WriteJSPropertyDouble('offset',aValue);
end;

procedure TJSPlaneLayout._Setstride(const aValue : Cardinal);
begin
  WriteJSPropertyDouble('stride',aValue);
end;

constructor TJSPlaneLayout.create(const aDict : TJSPlaneLayoutRec); overload;
begin
  Self.offset:=aDict.offset;
  Self.stride:=aDict.stride;
end;

class function TJSPlaneLayout.JSClassName: UnicodeString;
begin
  Result:='Object';
end;

class function TJSPlaneLayout.Cast(const Intf: IJSObject): IJSPlaneLayout;
begin
  Result:=TJSPlaneLayout.JOBCast(Intf);
end;

class function TJSWebGLSampler.JSClassName: UnicodeString;
begin
  Result:='WebGLSampler';
end;

class function TJSWebGLSampler.Cast(const Intf: IJSObject): IJSWebGLSampler;
begin
  Result:=TJSWebGLSampler.JOBCast(Intf);
end;

class function TJSWebGLSync.JSClassName: UnicodeString;
begin
  Result:='WebGLSync';
end;

class function TJSWebGLSync.Cast(const Intf: IJSObject): IJSWebGLSync;
begin
  Result:=TJSWebGLSync.JOBCast(Intf);
end;

class function TJSWebGLTransformFeedback.JSClassName: UnicodeString;
begin
  Result:='WebGLTransformFeedback';
end;

class function TJSWebGLTransformFeedback.Cast(const Intf: IJSObject): IJSWebGLTransformFeedback;
begin
  Result:=TJSWebGLTransformFeedback.JOBCast(Intf);
end;

function TJSWebGL2RenderingContext._Getcanvas: TCanvasSource;
begin
  Result:=ReadJSPropertyVariant('canvas');
end;

function TJSWebGL2RenderingContext._GetdrawingBufferWidth: TGLsizei;
begin
  Result:=ReadJSPropertyLongInt('drawingBufferWidth');
end;

function TJSWebGL2RenderingContext._GetdrawingBufferHeight: TGLsizei;
begin
  Result:=ReadJSPropertyLongInt('drawingBufferHeight');
end;

function TJSWebGL2RenderingContext._GetdrawingBufferColorSpace: TPredefinedColorSpace;
begin
  Result:=ReadJSPropertyUnicodeString('drawingBufferColorSpace');
end;

function TJSWebGL2RenderingContext._GetunpackColorSpace: TPredefinedColorSpace;
begin
  Result:=ReadJSPropertyUnicodeString('unpackColorSpace');
end;

procedure TJSWebGL2RenderingContext._SetdrawingBufferColorSpace(const aValue : TPredefinedColorSpace);
begin
  WriteJSPropertyUnicodeString('drawingBufferColorSpace',aValue);
end;

procedure TJSWebGL2RenderingContext._SetunpackColorSpace(const aValue : TPredefinedColorSpace);
begin
  WriteJSPropertyUnicodeString('unpackColorSpace',aValue);
end;

function TJSWebGL2RenderingContext.getContextAttributes: IJSWebGLContextAttributes;
begin
  Result:=InvokeJSObjectResult('getContextAttributes',[],TJSWebGLContextAttributes) as IJSWebGLContextAttributes;
end;

function TJSWebGL2RenderingContext.isContextLost: Boolean;
begin
  Result:=InvokeJSBooleanResult('isContextLost',[]);
end;

function TJSWebGL2RenderingContext.getSupportedExtensions: TUnicodeStringDynArray;
begin
  Result:=InvokeJSObjectResult('getSupportedExtensions',[],TJSArray) as TUnicodeStringDynArray;
end;

function TJSWebGL2RenderingContext.getExtension(const aName: UnicodeString): IJSObject;
begin
  Result:=InvokeJSObjectResult('getExtension',[aName],TJSObject) as IJSObject;
end;

procedure TJSWebGL2RenderingContext.activeTexture(aTexture: TGLenum);
begin
  InvokeJSNoResult('activeTexture',[aTexture]);
end;

procedure TJSWebGL2RenderingContext.attachShader(aProgram_: IJSWebGLProgram; aShader: IJSWebGLShader);
begin
  InvokeJSNoResult('attachShader',[aProgram_,aShader]);
end;

procedure TJSWebGL2RenderingContext.bindAttribLocation(aProgram_: IJSWebGLProgram; aIndex: TGLuint; const aName: UnicodeString);
begin
  InvokeJSNoResult('bindAttribLocation',[aProgram_,aIndex,aName]);
end;

procedure TJSWebGL2RenderingContext.bindBuffer(aTarget: TGLenum; aBuffer: IJSWebGLBuffer);
begin
  InvokeJSNoResult('bindBuffer',[aTarget,aBuffer]);
end;

procedure TJSWebGL2RenderingContext.bindFramebuffer(aTarget: TGLenum; aFramebuffer: IJSWebGLFramebuffer);
begin
  InvokeJSNoResult('bindFramebuffer',[aTarget,aFramebuffer]);
end;

procedure TJSWebGL2RenderingContext.bindRenderbuffer(aTarget: TGLenum; aRenderbuffer: IJSWebGLRenderbuffer);
begin
  InvokeJSNoResult('bindRenderbuffer',[aTarget,aRenderbuffer]);
end;

procedure TJSWebGL2RenderingContext.bindTexture(aTarget: TGLenum; aTexture: IJSWebGLTexture);
begin
  InvokeJSNoResult('bindTexture',[aTarget,aTexture]);
end;

procedure TJSWebGL2RenderingContext.blendColor(aRed: TGLfloat; aGreen: TGLfloat; aBlue: TGLfloat; aAlpha: TGLfloat);
begin
  InvokeJSNoResult('blendColor',[aRed,aGreen,aBlue,aAlpha]);
end;

procedure TJSWebGL2RenderingContext.blendEquation(aMode: TGLenum);
begin
  InvokeJSNoResult('blendEquation',[aMode]);
end;

procedure TJSWebGL2RenderingContext.blendEquationSeparate(aModeRGB: TGLenum; aModeAlpha: TGLenum);
begin
  InvokeJSNoResult('blendEquationSeparate',[aModeRGB,aModeAlpha]);
end;

procedure TJSWebGL2RenderingContext.blendFunc(aSfactor: TGLenum; aDfactor: TGLenum);
begin
  InvokeJSNoResult('blendFunc',[aSfactor,aDfactor]);
end;

procedure TJSWebGL2RenderingContext.blendFuncSeparate(aSrcRGB: TGLenum; aDstRGB: TGLenum; aSrcAlpha: TGLenum; aDstAlpha: TGLenum);
begin
  InvokeJSNoResult('blendFuncSeparate',[aSrcRGB,aDstRGB,aSrcAlpha,aDstAlpha]);
end;

function TJSWebGL2RenderingContext.checkFramebufferStatus(aTarget: TGLenum): TGLenum;
begin
  Result:=InvokeJSLongIntResult('checkFramebufferStatus',[aTarget]);
end;

procedure TJSWebGL2RenderingContext.clear(aMask: TGLbitfield);
begin
  InvokeJSNoResult('clear',[aMask]);
end;

procedure TJSWebGL2RenderingContext.clearColor(aRed: TGLfloat; aGreen: TGLfloat; aBlue: TGLfloat; aAlpha: TGLfloat);
begin
  InvokeJSNoResult('clearColor',[aRed,aGreen,aBlue,aAlpha]);
end;

procedure TJSWebGL2RenderingContext.clearDepth(aDepth: TGLclampf);
begin
  InvokeJSNoResult('clearDepth',[aDepth]);
end;

procedure TJSWebGL2RenderingContext.clearStencil(aS_: TGLint);
begin
  InvokeJSNoResult('clearStencil',[aS_]);
end;

procedure TJSWebGL2RenderingContext.colorMask(aRed: TGLboolean; aGreen: TGLboolean; aBlue: TGLboolean; aAlpha: TGLboolean);
begin
  InvokeJSNoResult('colorMask',[aRed,aGreen,aBlue,aAlpha]);
end;

procedure TJSWebGL2RenderingContext.compileShader(aShader: IJSWebGLShader);
begin
  InvokeJSNoResult('compileShader',[aShader]);
end;

procedure TJSWebGL2RenderingContext.copyTexImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLenum; aX: TGLint; aY: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aBorder: TGLint);
begin
  InvokeJSNoResult('copyTexImage2D',[aTarget,aLevel,aInternalformat,aX,aY,aWidth,aHeight,aBorder]);
end;

procedure TJSWebGL2RenderingContext.copyTexSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aX: TGLint; aY: TGLint; aWidth: TGLsizei; aHeight: TGLsizei);
begin
  InvokeJSNoResult('copyTexSubImage2D',[aTarget,aLevel,aXoffset,aYoffset,aX,aY,aWidth,aHeight]);
end;

function TJSWebGL2RenderingContext.createBuffer: IJSWebGLBuffer;
begin
  Result:=InvokeJSObjectResult('createBuffer',[],TJSWebGLBuffer) as IJSWebGLBuffer;
end;

function TJSWebGL2RenderingContext.createFramebuffer: IJSWebGLFramebuffer;
begin
  Result:=InvokeJSObjectResult('createFramebuffer',[],TJSWebGLFramebuffer) as IJSWebGLFramebuffer;
end;

function TJSWebGL2RenderingContext.createProgram: IJSWebGLProgram;
begin
  Result:=InvokeJSObjectResult('createProgram',[],TJSWebGLProgram) as IJSWebGLProgram;
end;

function TJSWebGL2RenderingContext.createRenderbuffer: IJSWebGLRenderbuffer;
begin
  Result:=InvokeJSObjectResult('createRenderbuffer',[],TJSWebGLRenderbuffer) as IJSWebGLRenderbuffer;
end;

function TJSWebGL2RenderingContext.createShader(aType_: TGLenum): IJSWebGLShader;
begin
  Result:=InvokeJSObjectResult('createShader',[aType_],TJSWebGLShader) as IJSWebGLShader;
end;

function TJSWebGL2RenderingContext.createTexture: IJSWebGLTexture;
begin
  Result:=InvokeJSObjectResult('createTexture',[],TJSWebGLTexture) as IJSWebGLTexture;
end;

procedure TJSWebGL2RenderingContext.cullFace(aMode: TGLenum);
begin
  InvokeJSNoResult('cullFace',[aMode]);
end;

procedure TJSWebGL2RenderingContext.deleteBuffer(aBuffer: IJSWebGLBuffer);
begin
  InvokeJSNoResult('deleteBuffer',[aBuffer]);
end;

procedure TJSWebGL2RenderingContext.deleteFramebuffer(aFramebuffer: IJSWebGLFramebuffer);
begin
  InvokeJSNoResult('deleteFramebuffer',[aFramebuffer]);
end;

procedure TJSWebGL2RenderingContext.deleteProgram(aProgram_: IJSWebGLProgram);
begin
  InvokeJSNoResult('deleteProgram',[aProgram_]);
end;

procedure TJSWebGL2RenderingContext.deleteRenderbuffer(aRenderbuffer: IJSWebGLRenderbuffer);
begin
  InvokeJSNoResult('deleteRenderbuffer',[aRenderbuffer]);
end;

procedure TJSWebGL2RenderingContext.deleteShader(aShader: IJSWebGLShader);
begin
  InvokeJSNoResult('deleteShader',[aShader]);
end;

procedure TJSWebGL2RenderingContext.deleteTexture(aTexture: IJSWebGLTexture);
begin
  InvokeJSNoResult('deleteTexture',[aTexture]);
end;

procedure TJSWebGL2RenderingContext.depthFunc(aFunc: TGLenum);
begin
  InvokeJSNoResult('depthFunc',[aFunc]);
end;

procedure TJSWebGL2RenderingContext.depthMask(aFlag: TGLboolean);
begin
  InvokeJSNoResult('depthMask',[aFlag]);
end;

procedure TJSWebGL2RenderingContext.depthRange(aZNear: TGLclampf; aZFar: TGLclampf);
begin
  InvokeJSNoResult('depthRange',[aZNear,aZFar]);
end;

procedure TJSWebGL2RenderingContext.detachShader(aProgram_: IJSWebGLProgram; aShader: IJSWebGLShader);
begin
  InvokeJSNoResult('detachShader',[aProgram_,aShader]);
end;

procedure TJSWebGL2RenderingContext.disable(aCap: TGLenum);
begin
  InvokeJSNoResult('disable',[aCap]);
end;

procedure TJSWebGL2RenderingContext.disableVertexAttribArray(aIndex: TGLuint);
begin
  InvokeJSNoResult('disableVertexAttribArray',[aIndex]);
end;

procedure TJSWebGL2RenderingContext.drawArrays(aMode: TGLenum; aFirst: TGLint; aCount: TGLsizei);
begin
  InvokeJSNoResult('drawArrays',[aMode,aFirst,aCount]);
end;

procedure TJSWebGL2RenderingContext.drawElements(aMode: TGLenum; aCount: TGLsizei; aType_: TGLenum; aOffset: TGLintptr);
begin
  InvokeJSNoResult('drawElements',[aMode,aCount,aType_,aOffset]);
end;

procedure TJSWebGL2RenderingContext.enable(aCap: TGLenum);
begin
  InvokeJSNoResult('enable',[aCap]);
end;

procedure TJSWebGL2RenderingContext.enableVertexAttribArray(aIndex: TGLuint);
begin
  InvokeJSNoResult('enableVertexAttribArray',[aIndex]);
end;

procedure TJSWebGL2RenderingContext.finish;
begin
  InvokeJSNoResult('finish',[]);
end;

procedure TJSWebGL2RenderingContext.flush;
begin
  InvokeJSNoResult('flush',[]);
end;

procedure TJSWebGL2RenderingContext.framebufferRenderbuffer(aTarget: TGLenum; aAttachment: TGLenum; aRenderbuffertarget: TGLenum; aRenderbuffer: IJSWebGLRenderbuffer);
begin
  InvokeJSNoResult('framebufferRenderbuffer',[aTarget,aAttachment,aRenderbuffertarget,aRenderbuffer]);
end;

procedure TJSWebGL2RenderingContext.framebufferTexture2D(aTarget: TGLenum; aAttachment: TGLenum; aTextarget: TGLenum; aTexture: IJSWebGLTexture; aLevel: TGLint);
begin
  InvokeJSNoResult('framebufferTexture2D',[aTarget,aAttachment,aTextarget,aTexture,aLevel]);
end;

procedure TJSWebGL2RenderingContext.frontFace(aMode: TGLenum);
begin
  InvokeJSNoResult('frontFace',[aMode]);
end;

procedure TJSWebGL2RenderingContext.generateMipmap(aTarget: TGLenum);
begin
  InvokeJSNoResult('generateMipmap',[aTarget]);
end;

function TJSWebGL2RenderingContext.getActiveAttrib(aProgram_: IJSWebGLProgram; aIndex: TGLuint): IJSWebGLActiveInfo;
begin
  Result:=InvokeJSObjectResult('getActiveAttrib',[aProgram_,aIndex],TJSWebGLActiveInfo) as IJSWebGLActiveInfo;
end;

function TJSWebGL2RenderingContext.getActiveUniform(aProgram_: IJSWebGLProgram; aIndex: TGLuint): IJSWebGLActiveInfo;
begin
  Result:=InvokeJSObjectResult('getActiveUniform',[aProgram_,aIndex],TJSWebGLActiveInfo) as IJSWebGLActiveInfo;
end;

function TJSWebGL2RenderingContext.getAttachedShaders(aProgram_: IJSWebGLProgram): TJSWebGLShaderDynArray;
begin
  Result:=InvokeJSObjectResult('getAttachedShaders',[aProgram_],TJSArray) as TJSWebGLShaderDynArray;
end;

function TJSWebGL2RenderingContext.getAttribLocation(aProgram_: IJSWebGLProgram; const aName: UnicodeString): TGLint;
begin
  Result:=InvokeJSLongIntResult('getAttribLocation',[aProgram_,aName]);
end;

function TJSWebGL2RenderingContext.getBufferParameter(aTarget: TGLenum; aPname: TGLenum): Variant;
begin
  Result:=InvokeJSVariantResult('getBufferParameter',[aTarget,aPname]);
end;

function TJSWebGL2RenderingContext.getParameter(aPname: TGLenum): Variant;
begin
  Result:=InvokeJSVariantResult('getParameter',[aPname]);
end;

function TJSWebGL2RenderingContext.getError: TGLenum;
begin
  Result:=InvokeJSLongIntResult('getError',[]);
end;

function TJSWebGL2RenderingContext.getFramebufferAttachmentParameter(aTarget: TGLenum; aAttachment: TGLenum; aPname: TGLenum): Variant;
begin
  Result:=InvokeJSVariantResult('getFramebufferAttachmentParameter',[aTarget,aAttachment,aPname]);
end;

function TJSWebGL2RenderingContext.getProgramParameter(aProgram_: IJSWebGLProgram; aPname: TGLenum): Variant;
begin
  Result:=InvokeJSVariantResult('getProgramParameter',[aProgram_,aPname]);
end;

function TJSWebGL2RenderingContext.getProgramInfoLog(aProgram_: IJSWebGLProgram): UnicodeString;
begin
  Result:=InvokeJSUnicodeStringResult('getProgramInfoLog',[aProgram_]);
end;

function TJSWebGL2RenderingContext.getRenderbufferParameter(aTarget: TGLenum; aPname: TGLenum): Variant;
begin
  Result:=InvokeJSVariantResult('getRenderbufferParameter',[aTarget,aPname]);
end;

function TJSWebGL2RenderingContext.getShaderParameter(aShader: IJSWebGLShader; aPname: TGLenum): Variant;
begin
  Result:=InvokeJSVariantResult('getShaderParameter',[aShader,aPname]);
end;

function TJSWebGL2RenderingContext.getShaderPrecisionFormat(aShadertype: TGLenum; aPrecisiontype: TGLenum): IJSWebGLShaderPrecisionFormat;
begin
  Result:=InvokeJSObjectResult('getShaderPrecisionFormat',[aShadertype,aPrecisiontype],TJSWebGLShaderPrecisionFormat) as IJSWebGLShaderPrecisionFormat;
end;

function TJSWebGL2RenderingContext.getShaderInfoLog(aShader: IJSWebGLShader): UnicodeString;
begin
  Result:=InvokeJSUnicodeStringResult('getShaderInfoLog',[aShader]);
end;

function TJSWebGL2RenderingContext.getShaderSource(aShader: IJSWebGLShader): UnicodeString;
begin
  Result:=InvokeJSUnicodeStringResult('getShaderSource',[aShader]);
end;

function TJSWebGL2RenderingContext.getTexParameter(aTarget: TGLenum; aPname: TGLenum): Variant;
begin
  Result:=InvokeJSVariantResult('getTexParameter',[aTarget,aPname]);
end;

function TJSWebGL2RenderingContext.getUniform(aProgram_: IJSWebGLProgram; aLocation: IJSWebGLUniformLocation): Variant;
begin
  Result:=InvokeJSVariantResult('getUniform',[aProgram_,aLocation]);
end;

function TJSWebGL2RenderingContext.getUniformLocation(aProgram_: IJSWebGLProgram; const aName: UnicodeString): IJSWebGLUniformLocation;
begin
  Result:=InvokeJSObjectResult('getUniformLocation',[aProgram_,aName],TJSWebGLUniformLocation) as IJSWebGLUniformLocation;
end;

function TJSWebGL2RenderingContext.getVertexAttrib(aIndex: TGLuint; aPname: TGLenum): Variant;
begin
  Result:=InvokeJSVariantResult('getVertexAttrib',[aIndex,aPname]);
end;

function TJSWebGL2RenderingContext.getVertexAttribOffset(aIndex: TGLuint; aPname: TGLenum): TGLintptr;
begin
  Result:=InvokeJSMaxIntResult('getVertexAttribOffset',[aIndex,aPname]);
end;

procedure TJSWebGL2RenderingContext.hint(aTarget: TGLenum; aMode: TGLenum);
begin
  InvokeJSNoResult('hint',[aTarget,aMode]);
end;

function TJSWebGL2RenderingContext.isBuffer(aBuffer: IJSWebGLBuffer): TGLboolean;
begin
  Result:=InvokeJSBooleanResult('isBuffer',[aBuffer]);
end;

function TJSWebGL2RenderingContext.isEnabled(aCap: TGLenum): TGLboolean;
begin
  Result:=InvokeJSBooleanResult('isEnabled',[aCap]);
end;

function TJSWebGL2RenderingContext.isFramebuffer(aFramebuffer: IJSWebGLFramebuffer): TGLboolean;
begin
  Result:=InvokeJSBooleanResult('isFramebuffer',[aFramebuffer]);
end;

function TJSWebGL2RenderingContext.isProgram(aProgram_: IJSWebGLProgram): TGLboolean;
begin
  Result:=InvokeJSBooleanResult('isProgram',[aProgram_]);
end;

function TJSWebGL2RenderingContext.isRenderbuffer(aRenderbuffer: IJSWebGLRenderbuffer): TGLboolean;
begin
  Result:=InvokeJSBooleanResult('isRenderbuffer',[aRenderbuffer]);
end;

function TJSWebGL2RenderingContext.isShader(aShader: IJSWebGLShader): TGLboolean;
begin
  Result:=InvokeJSBooleanResult('isShader',[aShader]);
end;

function TJSWebGL2RenderingContext.isTexture(aTexture: IJSWebGLTexture): TGLboolean;
begin
  Result:=InvokeJSBooleanResult('isTexture',[aTexture]);
end;

procedure TJSWebGL2RenderingContext.lineWidth(aWidth: TGLfloat);
begin
  InvokeJSNoResult('lineWidth',[aWidth]);
end;

procedure TJSWebGL2RenderingContext.linkProgram(aProgram_: IJSWebGLProgram);
begin
  InvokeJSNoResult('linkProgram',[aProgram_]);
end;

procedure TJSWebGL2RenderingContext.pixelStorei(aPname: TGLenum; aParam: TGLint);
begin
  InvokeJSNoResult('pixelStorei',[aPname,aParam]);
end;

procedure TJSWebGL2RenderingContext.polygonOffset(aFactor: TGLfloat; aUnits: TGLfloat);
begin
  InvokeJSNoResult('polygonOffset',[aFactor,aUnits]);
end;

procedure TJSWebGL2RenderingContext.renderbufferStorage(aTarget: TGLenum; aInternalformat: TGLenum; aWidth: TGLsizei; aHeight: TGLsizei);
begin
  InvokeJSNoResult('renderbufferStorage',[aTarget,aInternalformat,aWidth,aHeight]);
end;

procedure TJSWebGL2RenderingContext.sampleCoverage(aValue: TGLclampf; aInvert: TGLboolean);
begin
  InvokeJSNoResult('sampleCoverage',[aValue,aInvert]);
end;

procedure TJSWebGL2RenderingContext.scissor(aX: TGLint; aY: TGLint; aWidth: TGLsizei; aHeight: TGLsizei);
begin
  InvokeJSNoResult('scissor',[aX,aY,aWidth,aHeight]);
end;

procedure TJSWebGL2RenderingContext.shaderSource(aShader: IJSWebGLShader; const aSource: UnicodeString);
begin
  InvokeJSNoResult('shaderSource',[aShader,aSource]);
end;

procedure TJSWebGL2RenderingContext.stencilFunc(aFunc: TGLenum; aRef: TGLint; aMask: TGLuint);
begin
  InvokeJSNoResult('stencilFunc',[aFunc,aRef,aMask]);
end;

procedure TJSWebGL2RenderingContext.stencilFuncSeparate(aFace: TGLenum; aFunc: TGLenum; aRef: TGLint; aMask: TGLuint);
begin
  InvokeJSNoResult('stencilFuncSeparate',[aFace,aFunc,aRef,aMask]);
end;

procedure TJSWebGL2RenderingContext.stencilMask(aMask: TGLuint);
begin
  InvokeJSNoResult('stencilMask',[aMask]);
end;

procedure TJSWebGL2RenderingContext.stencilMaskSeparate(aFace: TGLenum; aMask: TGLuint);
begin
  InvokeJSNoResult('stencilMaskSeparate',[aFace,aMask]);
end;

procedure TJSWebGL2RenderingContext.stencilOp(aFail: TGLenum; aZfail: TGLenum; aZpass: TGLenum);
begin
  InvokeJSNoResult('stencilOp',[aFail,aZfail,aZpass]);
end;

procedure TJSWebGL2RenderingContext.stencilOpSeparate(aFace: TGLenum; aFail: TGLenum; aZfail: TGLenum; aZpass: TGLenum);
begin
  InvokeJSNoResult('stencilOpSeparate',[aFace,aFail,aZfail,aZpass]);
end;

procedure TJSWebGL2RenderingContext.texParameterf(aTarget: TGLenum; aPname: TGLenum; aParam: TGLfloat);
begin
  InvokeJSNoResult('texParameterf',[aTarget,aPname,aParam]);
end;

procedure TJSWebGL2RenderingContext.texParameteri(aTarget: TGLenum; aPname: TGLenum; aParam: TGLint);
begin
  InvokeJSNoResult('texParameteri',[aTarget,aPname,aParam]);
end;

procedure TJSWebGL2RenderingContext.uniform1f(aLocation: IJSWebGLUniformLocation; aX: TGLfloat);
begin
  InvokeJSNoResult('uniform1f',[aLocation,aX]);
end;

procedure TJSWebGL2RenderingContext.uniform2f(aLocation: IJSWebGLUniformLocation; aX: TGLfloat; aY: TGLfloat);
begin
  InvokeJSNoResult('uniform2f',[aLocation,aX,aY]);
end;

procedure TJSWebGL2RenderingContext.uniform3f(aLocation: IJSWebGLUniformLocation; aX: TGLfloat; aY: TGLfloat; aZ: TGLfloat);
begin
  InvokeJSNoResult('uniform3f',[aLocation,aX,aY,aZ]);
end;

procedure TJSWebGL2RenderingContext.uniform4f(aLocation: IJSWebGLUniformLocation; aX: TGLfloat; aY: TGLfloat; aZ: TGLfloat; aW: TGLfloat);
begin
  InvokeJSNoResult('uniform4f',[aLocation,aX,aY,aZ,aW]);
end;

procedure TJSWebGL2RenderingContext.uniform1i(aLocation: IJSWebGLUniformLocation; aX: TGLint);
begin
  InvokeJSNoResult('uniform1i',[aLocation,aX]);
end;

procedure TJSWebGL2RenderingContext.uniform2i(aLocation: IJSWebGLUniformLocation; aX: TGLint; aY: TGLint);
begin
  InvokeJSNoResult('uniform2i',[aLocation,aX,aY]);
end;

procedure TJSWebGL2RenderingContext.uniform3i(aLocation: IJSWebGLUniformLocation; aX: TGLint; aY: TGLint; aZ: TGLint);
begin
  InvokeJSNoResult('uniform3i',[aLocation,aX,aY,aZ]);
end;

procedure TJSWebGL2RenderingContext.uniform4i(aLocation: IJSWebGLUniformLocation; aX: TGLint; aY: TGLint; aZ: TGLint; aW: TGLint);
begin
  InvokeJSNoResult('uniform4i',[aLocation,aX,aY,aZ,aW]);
end;

procedure TJSWebGL2RenderingContext.useProgram(aProgram_: IJSWebGLProgram);
begin
  InvokeJSNoResult('useProgram',[aProgram_]);
end;

procedure TJSWebGL2RenderingContext.validateProgram(aProgram_: IJSWebGLProgram);
begin
  InvokeJSNoResult('validateProgram',[aProgram_]);
end;

procedure TJSWebGL2RenderingContext.vertexAttrib1f(aIndx: TGLuint; aX: TGLfloat);
begin
  InvokeJSNoResult('vertexAttrib1f',[aIndx,aX]);
end;

procedure TJSWebGL2RenderingContext.vertexAttrib1fv(aIndx: TGLuint; aValues: IJSFloat32Array);
begin
  InvokeJSNoResult('vertexAttrib1fv',[aIndx,aValues]);
end;

procedure TJSWebGL2RenderingContext.vertexAttrib1fv(aIndx: TGLuint; const aValues: TGLfloatDynArray);
begin
  InvokeJSNoResult('vertexAttrib1fv',[aIndx,aValues]);
end;

procedure TJSWebGL2RenderingContext.vertexAttrib2f(aIndx: TGLuint; aX: TGLfloat; aY: TGLfloat);
begin
  InvokeJSNoResult('vertexAttrib2f',[aIndx,aX,aY]);
end;

procedure TJSWebGL2RenderingContext.vertexAttrib2fv(aIndx: TGLuint; aValues: IJSFloat32Array);
begin
  InvokeJSNoResult('vertexAttrib2fv',[aIndx,aValues]);
end;

procedure TJSWebGL2RenderingContext.vertexAttrib2fv(aIndx: TGLuint; const aValues: TGLfloatDynArray);
begin
  InvokeJSNoResult('vertexAttrib2fv',[aIndx,aValues]);
end;

procedure TJSWebGL2RenderingContext.vertexAttrib3f(aIndx: TGLuint; aX: TGLfloat; aY: TGLfloat; aZ: TGLfloat);
begin
  InvokeJSNoResult('vertexAttrib3f',[aIndx,aX,aY,aZ]);
end;

procedure TJSWebGL2RenderingContext.vertexAttrib3fv(aIndx: TGLuint; aValues: IJSFloat32Array);
begin
  InvokeJSNoResult('vertexAttrib3fv',[aIndx,aValues]);
end;

procedure TJSWebGL2RenderingContext.vertexAttrib3fv(aIndx: TGLuint; const aValues: TGLfloatDynArray);
begin
  InvokeJSNoResult('vertexAttrib3fv',[aIndx,aValues]);
end;

procedure TJSWebGL2RenderingContext.vertexAttrib4f(aIndx: TGLuint; aX: TGLfloat; aY: TGLfloat; aZ: TGLfloat; aW: TGLfloat);
begin
  InvokeJSNoResult('vertexAttrib4f',[aIndx,aX,aY,aZ,aW]);
end;

procedure TJSWebGL2RenderingContext.vertexAttrib4fv(aIndx: TGLuint; aValues: IJSFloat32Array);
begin
  InvokeJSNoResult('vertexAttrib4fv',[aIndx,aValues]);
end;

procedure TJSWebGL2RenderingContext.vertexAttrib4fv(aIndx: TGLuint; const aValues: TGLfloatDynArray);
begin
  InvokeJSNoResult('vertexAttrib4fv',[aIndx,aValues]);
end;

procedure TJSWebGL2RenderingContext.vertexAttribPointer(aIndx: TGLuint; aSize: TGLint; aType_: TGLenum; aNormalized: TGLboolean; aStride: TGLsizei; aOffset: TGLintptr);
begin
  InvokeJSNoResult('vertexAttribPointer',[aIndx,aSize,aType_,aNormalized,aStride,aOffset]);
end;

procedure TJSWebGL2RenderingContext.viewport(aX: TGLint; aY: TGLint; aWidth: TGLsizei; aHeight: TGLsizei);
begin
  InvokeJSNoResult('viewport',[aX,aY,aWidth,aHeight]);
end;

function TJSWebGL2RenderingContext.makeXRCompatible: IJSPromise; // Promise<undefined>
begin
  Result:=InvokeJSObjectResult('makeXRCompatible',[],TJSPromise) as IJSPromise;
end;

procedure TJSWebGL2RenderingContext.bufferData(aTarget: TGLenum; aSize: TGLsizeiptr; aUsage: TGLenum);
begin
  InvokeJSNoResult('bufferData',[aTarget,aSize,aUsage]);
end;

procedure TJSWebGL2RenderingContext.bufferData(aTarget: TGLenum; aSrcData: IJSArrayBuffer; aUsage: TGLenum);
begin
  InvokeJSNoResult('bufferData',[aTarget,aSrcData,aUsage]);
end;

procedure TJSWebGL2RenderingContext.bufferData(aTarget: TGLenum; aSrcData: IJSArrayBufferView; aUsage: TGLenum);
begin
  InvokeJSNoResult('bufferData',[aTarget,aSrcData,aUsage]);
end;

procedure TJSWebGL2RenderingContext.bufferSubData(aTarget: TGLenum; aOffset: TGLintptr; aSrcData: IJSArrayBuffer);
begin
  InvokeJSNoResult('bufferSubData',[aTarget,aOffset,aSrcData]);
end;

procedure TJSWebGL2RenderingContext.bufferSubData(aTarget: TGLenum; aOffset: TGLintptr; aSrcData: IJSArrayBufferView);
begin
  InvokeJSNoResult('bufferSubData',[aTarget,aOffset,aSrcData]);
end;

procedure TJSWebGL2RenderingContext.bufferData(aTarget: TGLenum; aSrcData: IJSArrayBufferView; aUsage: TGLenum; aSrcOffset: TGLuint; aLength_: TGLuint);
begin
  InvokeJSNoResult('bufferData',[aTarget,aSrcData,aUsage,aSrcOffset,aLength_]);
end;

procedure TJSWebGL2RenderingContext.bufferData(aTarget: TGLenum; aSrcData: IJSArrayBufferView; aUsage: TGLenum; aSrcOffset: TGLuint);
begin
  InvokeJSNoResult('bufferData',[aTarget,aSrcData,aUsage,aSrcOffset]);
end;

procedure TJSWebGL2RenderingContext.bufferSubData(aTarget: TGLenum; aDstByteOffset: TGLintptr; aSrcData: IJSArrayBufferView; aSrcOffset: TGLuint; aLength_: TGLuint);
begin
  InvokeJSNoResult('bufferSubData',[aTarget,aDstByteOffset,aSrcData,aSrcOffset,aLength_]);
end;

procedure TJSWebGL2RenderingContext.bufferSubData(aTarget: TGLenum; aDstByteOffset: TGLintptr; aSrcData: IJSArrayBufferView; aSrcOffset: TGLuint);
begin
  InvokeJSNoResult('bufferSubData',[aTarget,aDstByteOffset,aSrcData,aSrcOffset]);
end;

procedure TJSWebGL2RenderingContext.copyBufferSubData(aReadTarget: TGLenum; aWriteTarget: TGLenum; aReadOffset: TGLintptr; aWriteOffset: TGLintptr; aSize: TGLsizeiptr);
begin
  InvokeJSNoResult('copyBufferSubData',[aReadTarget,aWriteTarget,aReadOffset,aWriteOffset,aSize]);
end;

procedure TJSWebGL2RenderingContext.getBufferSubData(aTarget: TGLenum; aSrcByteOffset: TGLintptr; aDstData: IJSArrayBufferView; aDstOffset: TGLuint; aLength_: TGLuint);
begin
  InvokeJSNoResult('getBufferSubData',[aTarget,aSrcByteOffset,aDstData,aDstOffset,aLength_]);
end;

procedure TJSWebGL2RenderingContext.getBufferSubData(aTarget: TGLenum; aSrcByteOffset: TGLintptr; aDstData: IJSArrayBufferView);
begin
  InvokeJSNoResult('getBufferSubData',[aTarget,aSrcByteOffset,aDstData]);
end;

procedure TJSWebGL2RenderingContext.getBufferSubData(aTarget: TGLenum; aSrcByteOffset: TGLintptr; aDstData: IJSArrayBufferView; aDstOffset: TGLuint);
begin
  InvokeJSNoResult('getBufferSubData',[aTarget,aSrcByteOffset,aDstData,aDstOffset]);
end;

procedure TJSWebGL2RenderingContext.blitFramebuffer(aSrcX0: TGLint; aSrcY0: TGLint; aSrcX1: TGLint; aSrcY1: TGLint; aDstX0: TGLint; aDstY0: TGLint; aDstX1: TGLint; aDstY1: TGLint; aMask: TGLbitfield; aFilter: TGLenum);
begin
  InvokeJSNoResult('blitFramebuffer',[aSrcX0,aSrcY0,aSrcX1,aSrcY1,aDstX0,aDstY0,aDstX1,aDstY1,aMask,aFilter]);
end;

procedure TJSWebGL2RenderingContext.framebufferTextureLayer(aTarget: TGLenum; aAttachment: TGLenum; aTexture: IJSWebGLTexture; aLevel: TGLint; aLayer: TGLint);
begin
  InvokeJSNoResult('framebufferTextureLayer',[aTarget,aAttachment,aTexture,aLevel,aLayer]);
end;

procedure TJSWebGL2RenderingContext.invalidateFramebuffer(aTarget: TGLenum; const aAttachments: TGLenumDynArray);
begin
  InvokeJSNoResult('invalidateFramebuffer',[aTarget,aAttachments]);
end;

procedure TJSWebGL2RenderingContext.invalidateSubFramebuffer(aTarget: TGLenum; const aAttachments: TGLenumDynArray; aX: TGLint; aY: TGLint; aWidth: TGLsizei; aHeight: TGLsizei);
begin
  InvokeJSNoResult('invalidateSubFramebuffer',[aTarget,aAttachments,aX,aY,aWidth,aHeight]);
end;

procedure TJSWebGL2RenderingContext.readBuffer(aSrc: TGLenum);
begin
  InvokeJSNoResult('readBuffer',[aSrc]);
end;

function TJSWebGL2RenderingContext.getInternalformatParameter(aTarget: TGLenum; aInternalformat: TGLenum; aPname: TGLenum): Variant;
begin
  Result:=InvokeJSVariantResult('getInternalformatParameter',[aTarget,aInternalformat,aPname]);
end;

procedure TJSWebGL2RenderingContext.renderbufferStorageMultisample(aTarget: TGLenum; aSamples: TGLsizei; aInternalformat: TGLenum; aWidth: TGLsizei; aHeight: TGLsizei);
begin
  InvokeJSNoResult('renderbufferStorageMultisample',[aTarget,aSamples,aInternalformat,aWidth,aHeight]);
end;

procedure TJSWebGL2RenderingContext.texStorage2D(aTarget: TGLenum; aLevels: TGLsizei; aInternalformat: TGLenum; aWidth: TGLsizei; aHeight: TGLsizei);
begin
  InvokeJSNoResult('texStorage2D',[aTarget,aLevels,aInternalformat,aWidth,aHeight]);
end;

procedure TJSWebGL2RenderingContext.texStorage3D(aTarget: TGLenum; aLevels: TGLsizei; aInternalformat: TGLenum; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei);
begin
  InvokeJSNoResult('texStorage3D',[aTarget,aLevels,aInternalformat,aWidth,aHeight,aDepth]);
end;

procedure TJSWebGL2RenderingContext.texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aPixels: IJSArrayBufferView);
begin
  InvokeJSNoResult('texImage2D',[aTarget,aLevel,aInternalformat,aWidth,aHeight,aBorder,aFormat,aType_,aPixels]);
end;

procedure TJSWebGL2RenderingContext.texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSHTMLCanvasElement);
begin
  InvokeJSNoResult('texImage2D',[aTarget,aLevel,aInternalformat,aFormat,aType_,aSource]);
end;

procedure TJSWebGL2RenderingContext.texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSHTMLImageElement);
begin
  InvokeJSNoResult('texImage2D',[aTarget,aLevel,aInternalformat,aFormat,aType_,aSource]);
end;

procedure TJSWebGL2RenderingContext.texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSHTMLVideoElement);
begin
  InvokeJSNoResult('texImage2D',[aTarget,aLevel,aInternalformat,aFormat,aType_,aSource]);
end;

procedure TJSWebGL2RenderingContext.texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSImageBitmap);
begin
  InvokeJSNoResult('texImage2D',[aTarget,aLevel,aInternalformat,aFormat,aType_,aSource]);
end;

procedure TJSWebGL2RenderingContext.texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSImageData);
begin
  InvokeJSNoResult('texImage2D',[aTarget,aLevel,aInternalformat,aFormat,aType_,aSource]);
end;

procedure TJSWebGL2RenderingContext.texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSOffscreenCanvas);
begin
  InvokeJSNoResult('texImage2D',[aTarget,aLevel,aInternalformat,aFormat,aType_,aSource]);
end;

procedure TJSWebGL2RenderingContext.texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSVideoFrame);
begin
  InvokeJSNoResult('texImage2D',[aTarget,aLevel,aInternalformat,aFormat,aType_,aSource]);
end;

procedure TJSWebGL2RenderingContext.texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aPixels: IJSArrayBufferView);
begin
  InvokeJSNoResult('texSubImage2D',[aTarget,aLevel,aXoffset,aYoffset,aWidth,aHeight,aFormat,aType_,aPixels]);
end;

procedure TJSWebGL2RenderingContext.texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSHTMLCanvasElement);
begin
  InvokeJSNoResult('texSubImage2D',[aTarget,aLevel,aXoffset,aYoffset,aFormat,aType_,aSource]);
end;

procedure TJSWebGL2RenderingContext.texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSHTMLImageElement);
begin
  InvokeJSNoResult('texSubImage2D',[aTarget,aLevel,aXoffset,aYoffset,aFormat,aType_,aSource]);
end;

procedure TJSWebGL2RenderingContext.texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSHTMLVideoElement);
begin
  InvokeJSNoResult('texSubImage2D',[aTarget,aLevel,aXoffset,aYoffset,aFormat,aType_,aSource]);
end;

procedure TJSWebGL2RenderingContext.texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSImageBitmap);
begin
  InvokeJSNoResult('texSubImage2D',[aTarget,aLevel,aXoffset,aYoffset,aFormat,aType_,aSource]);
end;

procedure TJSWebGL2RenderingContext.texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSImageData);
begin
  InvokeJSNoResult('texSubImage2D',[aTarget,aLevel,aXoffset,aYoffset,aFormat,aType_,aSource]);
end;

procedure TJSWebGL2RenderingContext.texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSOffscreenCanvas);
begin
  InvokeJSNoResult('texSubImage2D',[aTarget,aLevel,aXoffset,aYoffset,aFormat,aType_,aSource]);
end;

procedure TJSWebGL2RenderingContext.texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSVideoFrame);
begin
  InvokeJSNoResult('texSubImage2D',[aTarget,aLevel,aXoffset,aYoffset,aFormat,aType_,aSource]);
end;

procedure TJSWebGL2RenderingContext.texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aPboOffset: TGLintptr);
begin
  InvokeJSNoResult('texImage2D',[aTarget,aLevel,aInternalformat,aWidth,aHeight,aBorder,aFormat,aType_,aPboOffset]);
end;

procedure TJSWebGL2RenderingContext.texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSHTMLCanvasElement);
begin
  InvokeJSNoResult('texImage2D',[aTarget,aLevel,aInternalformat,aWidth,aHeight,aBorder,aFormat,aType_,aSource]);
end;

procedure TJSWebGL2RenderingContext.texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSHTMLImageElement);
begin
  InvokeJSNoResult('texImage2D',[aTarget,aLevel,aInternalformat,aWidth,aHeight,aBorder,aFormat,aType_,aSource]);
end;

procedure TJSWebGL2RenderingContext.texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSHTMLVideoElement);
begin
  InvokeJSNoResult('texImage2D',[aTarget,aLevel,aInternalformat,aWidth,aHeight,aBorder,aFormat,aType_,aSource]);
end;

procedure TJSWebGL2RenderingContext.texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSImageBitmap);
begin
  InvokeJSNoResult('texImage2D',[aTarget,aLevel,aInternalformat,aWidth,aHeight,aBorder,aFormat,aType_,aSource]);
end;

procedure TJSWebGL2RenderingContext.texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSImageData);
begin
  InvokeJSNoResult('texImage2D',[aTarget,aLevel,aInternalformat,aWidth,aHeight,aBorder,aFormat,aType_,aSource]);
end;

procedure TJSWebGL2RenderingContext.texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSOffscreenCanvas);
begin
  InvokeJSNoResult('texImage2D',[aTarget,aLevel,aInternalformat,aWidth,aHeight,aBorder,aFormat,aType_,aSource]);
end;

procedure TJSWebGL2RenderingContext.texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSVideoFrame);
begin
  InvokeJSNoResult('texImage2D',[aTarget,aLevel,aInternalformat,aWidth,aHeight,aBorder,aFormat,aType_,aSource]);
end;

procedure TJSWebGL2RenderingContext.texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aSrcData: IJSArrayBufferView; aSrcOffset: TGLuint);
begin
  InvokeJSNoResult('texImage2D',[aTarget,aLevel,aInternalformat,aWidth,aHeight,aBorder,aFormat,aType_,aSrcData,aSrcOffset]);
end;

procedure TJSWebGL2RenderingContext.texImage3D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aPboOffset: TGLintptr);
begin
  InvokeJSNoResult('texImage3D',[aTarget,aLevel,aInternalformat,aWidth,aHeight,aDepth,aBorder,aFormat,aType_,aPboOffset]);
end;

procedure TJSWebGL2RenderingContext.texImage3D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSHTMLCanvasElement);
begin
  InvokeJSNoResult('texImage3D',[aTarget,aLevel,aInternalformat,aWidth,aHeight,aDepth,aBorder,aFormat,aType_,aSource]);
end;

procedure TJSWebGL2RenderingContext.texImage3D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSHTMLImageElement);
begin
  InvokeJSNoResult('texImage3D',[aTarget,aLevel,aInternalformat,aWidth,aHeight,aDepth,aBorder,aFormat,aType_,aSource]);
end;

procedure TJSWebGL2RenderingContext.texImage3D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSHTMLVideoElement);
begin
  InvokeJSNoResult('texImage3D',[aTarget,aLevel,aInternalformat,aWidth,aHeight,aDepth,aBorder,aFormat,aType_,aSource]);
end;

procedure TJSWebGL2RenderingContext.texImage3D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSImageBitmap);
begin
  InvokeJSNoResult('texImage3D',[aTarget,aLevel,aInternalformat,aWidth,aHeight,aDepth,aBorder,aFormat,aType_,aSource]);
end;

procedure TJSWebGL2RenderingContext.texImage3D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSImageData);
begin
  InvokeJSNoResult('texImage3D',[aTarget,aLevel,aInternalformat,aWidth,aHeight,aDepth,aBorder,aFormat,aType_,aSource]);
end;

procedure TJSWebGL2RenderingContext.texImage3D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSOffscreenCanvas);
begin
  InvokeJSNoResult('texImage3D',[aTarget,aLevel,aInternalformat,aWidth,aHeight,aDepth,aBorder,aFormat,aType_,aSource]);
end;

procedure TJSWebGL2RenderingContext.texImage3D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSVideoFrame);
begin
  InvokeJSNoResult('texImage3D',[aTarget,aLevel,aInternalformat,aWidth,aHeight,aDepth,aBorder,aFormat,aType_,aSource]);
end;

procedure TJSWebGL2RenderingContext.texImage3D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aSrcData: IJSArrayBufferView);
begin
  InvokeJSNoResult('texImage3D',[aTarget,aLevel,aInternalformat,aWidth,aHeight,aDepth,aBorder,aFormat,aType_,aSrcData]);
end;

procedure TJSWebGL2RenderingContext.texImage3D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aSrcData: IJSArrayBufferView; aSrcOffset: TGLuint);
begin
  InvokeJSNoResult('texImage3D',[aTarget,aLevel,aInternalformat,aWidth,aHeight,aDepth,aBorder,aFormat,aType_,aSrcData,aSrcOffset]);
end;

procedure TJSWebGL2RenderingContext.texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aPboOffset: TGLintptr);
begin
  InvokeJSNoResult('texSubImage2D',[aTarget,aLevel,aXoffset,aYoffset,aWidth,aHeight,aFormat,aType_,aPboOffset]);
end;

procedure TJSWebGL2RenderingContext.texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aSource: IJSHTMLCanvasElement);
begin
  InvokeJSNoResult('texSubImage2D',[aTarget,aLevel,aXoffset,aYoffset,aWidth,aHeight,aFormat,aType_,aSource]);
end;

procedure TJSWebGL2RenderingContext.texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aSource: IJSHTMLImageElement);
begin
  InvokeJSNoResult('texSubImage2D',[aTarget,aLevel,aXoffset,aYoffset,aWidth,aHeight,aFormat,aType_,aSource]);
end;

procedure TJSWebGL2RenderingContext.texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aSource: IJSHTMLVideoElement);
begin
  InvokeJSNoResult('texSubImage2D',[aTarget,aLevel,aXoffset,aYoffset,aWidth,aHeight,aFormat,aType_,aSource]);
end;

procedure TJSWebGL2RenderingContext.texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aSource: IJSImageBitmap);
begin
  InvokeJSNoResult('texSubImage2D',[aTarget,aLevel,aXoffset,aYoffset,aWidth,aHeight,aFormat,aType_,aSource]);
end;

procedure TJSWebGL2RenderingContext.texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aSource: IJSImageData);
begin
  InvokeJSNoResult('texSubImage2D',[aTarget,aLevel,aXoffset,aYoffset,aWidth,aHeight,aFormat,aType_,aSource]);
end;

procedure TJSWebGL2RenderingContext.texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aSource: IJSOffscreenCanvas);
begin
  InvokeJSNoResult('texSubImage2D',[aTarget,aLevel,aXoffset,aYoffset,aWidth,aHeight,aFormat,aType_,aSource]);
end;

procedure TJSWebGL2RenderingContext.texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aSource: IJSVideoFrame);
begin
  InvokeJSNoResult('texSubImage2D',[aTarget,aLevel,aXoffset,aYoffset,aWidth,aHeight,aFormat,aType_,aSource]);
end;

procedure TJSWebGL2RenderingContext.texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aSrcData: IJSArrayBufferView; aSrcOffset: TGLuint);
begin
  InvokeJSNoResult('texSubImage2D',[aTarget,aLevel,aXoffset,aYoffset,aWidth,aHeight,aFormat,aType_,aSrcData,aSrcOffset]);
end;

procedure TJSWebGL2RenderingContext.texSubImage3D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aZoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aPboOffset: TGLintptr);
begin
  InvokeJSNoResult('texSubImage3D',[aTarget,aLevel,aXoffset,aYoffset,aZoffset,aWidth,aHeight,aDepth,aFormat,aType_,aPboOffset]);
end;

procedure TJSWebGL2RenderingContext.texSubImage3D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aZoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aSource: IJSHTMLCanvasElement);
begin
  InvokeJSNoResult('texSubImage3D',[aTarget,aLevel,aXoffset,aYoffset,aZoffset,aWidth,aHeight,aDepth,aFormat,aType_,aSource]);
end;

procedure TJSWebGL2RenderingContext.texSubImage3D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aZoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aSource: IJSHTMLImageElement);
begin
  InvokeJSNoResult('texSubImage3D',[aTarget,aLevel,aXoffset,aYoffset,aZoffset,aWidth,aHeight,aDepth,aFormat,aType_,aSource]);
end;

procedure TJSWebGL2RenderingContext.texSubImage3D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aZoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aSource: IJSHTMLVideoElement);
begin
  InvokeJSNoResult('texSubImage3D',[aTarget,aLevel,aXoffset,aYoffset,aZoffset,aWidth,aHeight,aDepth,aFormat,aType_,aSource]);
end;

procedure TJSWebGL2RenderingContext.texSubImage3D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aZoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aSource: IJSImageBitmap);
begin
  InvokeJSNoResult('texSubImage3D',[aTarget,aLevel,aXoffset,aYoffset,aZoffset,aWidth,aHeight,aDepth,aFormat,aType_,aSource]);
end;

procedure TJSWebGL2RenderingContext.texSubImage3D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aZoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aSource: IJSImageData);
begin
  InvokeJSNoResult('texSubImage3D',[aTarget,aLevel,aXoffset,aYoffset,aZoffset,aWidth,aHeight,aDepth,aFormat,aType_,aSource]);
end;

procedure TJSWebGL2RenderingContext.texSubImage3D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aZoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aSource: IJSOffscreenCanvas);
begin
  InvokeJSNoResult('texSubImage3D',[aTarget,aLevel,aXoffset,aYoffset,aZoffset,aWidth,aHeight,aDepth,aFormat,aType_,aSource]);
end;

procedure TJSWebGL2RenderingContext.texSubImage3D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aZoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aSource: IJSVideoFrame);
begin
  InvokeJSNoResult('texSubImage3D',[aTarget,aLevel,aXoffset,aYoffset,aZoffset,aWidth,aHeight,aDepth,aFormat,aType_,aSource]);
end;

procedure TJSWebGL2RenderingContext.texSubImage3D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aZoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aSrcData: IJSArrayBufferView; aSrcOffset: TGLuint);
begin
  InvokeJSNoResult('texSubImage3D',[aTarget,aLevel,aXoffset,aYoffset,aZoffset,aWidth,aHeight,aDepth,aFormat,aType_,aSrcData,aSrcOffset]);
end;

procedure TJSWebGL2RenderingContext.texSubImage3D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aZoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aSrcData: IJSArrayBufferView);
begin
  InvokeJSNoResult('texSubImage3D',[aTarget,aLevel,aXoffset,aYoffset,aZoffset,aWidth,aHeight,aDepth,aFormat,aType_,aSrcData]);
end;

procedure TJSWebGL2RenderingContext.copyTexSubImage3D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aZoffset: TGLint; aX: TGLint; aY: TGLint; aWidth: TGLsizei; aHeight: TGLsizei);
begin
  InvokeJSNoResult('copyTexSubImage3D',[aTarget,aLevel,aXoffset,aYoffset,aZoffset,aX,aY,aWidth,aHeight]);
end;

procedure TJSWebGL2RenderingContext.compressedTexImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLenum; aWidth: TGLsizei; aHeight: TGLsizei; aBorder: TGLint; aImageSize: TGLsizei; aOffset: TGLintptr);
begin
  InvokeJSNoResult('compressedTexImage2D',[aTarget,aLevel,aInternalformat,aWidth,aHeight,aBorder,aImageSize,aOffset]);
end;

procedure TJSWebGL2RenderingContext.compressedTexImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLenum; aWidth: TGLsizei; aHeight: TGLsizei; aBorder: TGLint; aSrcData: IJSArrayBufferView; aSrcOffset: TGLuint; aSrcLengthOverride: TGLuint);
begin
  InvokeJSNoResult('compressedTexImage2D',[aTarget,aLevel,aInternalformat,aWidth,aHeight,aBorder,aSrcData,aSrcOffset,aSrcLengthOverride]);
end;

procedure TJSWebGL2RenderingContext.compressedTexImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLenum; aWidth: TGLsizei; aHeight: TGLsizei; aBorder: TGLint; aSrcData: IJSArrayBufferView);
begin
  InvokeJSNoResult('compressedTexImage2D',[aTarget,aLevel,aInternalformat,aWidth,aHeight,aBorder,aSrcData]);
end;

procedure TJSWebGL2RenderingContext.compressedTexImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLenum; aWidth: TGLsizei; aHeight: TGLsizei; aBorder: TGLint; aSrcData: IJSArrayBufferView; aSrcOffset: TGLuint);
begin
  InvokeJSNoResult('compressedTexImage2D',[aTarget,aLevel,aInternalformat,aWidth,aHeight,aBorder,aSrcData,aSrcOffset]);
end;

procedure TJSWebGL2RenderingContext.compressedTexImage3D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLenum; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aBorder: TGLint; aImageSize: TGLsizei; aOffset: TGLintptr);
begin
  InvokeJSNoResult('compressedTexImage3D',[aTarget,aLevel,aInternalformat,aWidth,aHeight,aDepth,aBorder,aImageSize,aOffset]);
end;

procedure TJSWebGL2RenderingContext.compressedTexImage3D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLenum; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aBorder: TGLint; aSrcData: IJSArrayBufferView; aSrcOffset: TGLuint; aSrcLengthOverride: TGLuint);
begin
  InvokeJSNoResult('compressedTexImage3D',[aTarget,aLevel,aInternalformat,aWidth,aHeight,aDepth,aBorder,aSrcData,aSrcOffset,aSrcLengthOverride]);
end;

procedure TJSWebGL2RenderingContext.compressedTexImage3D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLenum; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aBorder: TGLint; aSrcData: IJSArrayBufferView);
begin
  InvokeJSNoResult('compressedTexImage3D',[aTarget,aLevel,aInternalformat,aWidth,aHeight,aDepth,aBorder,aSrcData]);
end;

procedure TJSWebGL2RenderingContext.compressedTexImage3D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLenum; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aBorder: TGLint; aSrcData: IJSArrayBufferView; aSrcOffset: TGLuint);
begin
  InvokeJSNoResult('compressedTexImage3D',[aTarget,aLevel,aInternalformat,aWidth,aHeight,aDepth,aBorder,aSrcData,aSrcOffset]);
end;

procedure TJSWebGL2RenderingContext.compressedTexSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aFormat: TGLenum; aImageSize: TGLsizei; aOffset: TGLintptr);
begin
  InvokeJSNoResult('compressedTexSubImage2D',[aTarget,aLevel,aXoffset,aYoffset,aWidth,aHeight,aFormat,aImageSize,aOffset]);
end;

procedure TJSWebGL2RenderingContext.compressedTexSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aFormat: TGLenum; aSrcData: IJSArrayBufferView; aSrcOffset: TGLuint; aSrcLengthOverride: TGLuint);
begin
  InvokeJSNoResult('compressedTexSubImage2D',[aTarget,aLevel,aXoffset,aYoffset,aWidth,aHeight,aFormat,aSrcData,aSrcOffset,aSrcLengthOverride]);
end;

procedure TJSWebGL2RenderingContext.compressedTexSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aFormat: TGLenum; aSrcData: IJSArrayBufferView);
begin
  InvokeJSNoResult('compressedTexSubImage2D',[aTarget,aLevel,aXoffset,aYoffset,aWidth,aHeight,aFormat,aSrcData]);
end;

procedure TJSWebGL2RenderingContext.compressedTexSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aFormat: TGLenum; aSrcData: IJSArrayBufferView; aSrcOffset: TGLuint);
begin
  InvokeJSNoResult('compressedTexSubImage2D',[aTarget,aLevel,aXoffset,aYoffset,aWidth,aHeight,aFormat,aSrcData,aSrcOffset]);
end;

procedure TJSWebGL2RenderingContext.compressedTexSubImage3D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aZoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aFormat: TGLenum; aImageSize: TGLsizei; aOffset: TGLintptr);
begin
  InvokeJSNoResult('compressedTexSubImage3D',[aTarget,aLevel,aXoffset,aYoffset,aZoffset,aWidth,aHeight,aDepth,aFormat,aImageSize,aOffset]);
end;

procedure TJSWebGL2RenderingContext.compressedTexSubImage3D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aZoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aFormat: TGLenum; aSrcData: IJSArrayBufferView; aSrcOffset: TGLuint; aSrcLengthOverride: TGLuint);
begin
  InvokeJSNoResult('compressedTexSubImage3D',[aTarget,aLevel,aXoffset,aYoffset,aZoffset,aWidth,aHeight,aDepth,aFormat,aSrcData,aSrcOffset,aSrcLengthOverride]);
end;

procedure TJSWebGL2RenderingContext.compressedTexSubImage3D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aZoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aFormat: TGLenum; aSrcData: IJSArrayBufferView);
begin
  InvokeJSNoResult('compressedTexSubImage3D',[aTarget,aLevel,aXoffset,aYoffset,aZoffset,aWidth,aHeight,aDepth,aFormat,aSrcData]);
end;

procedure TJSWebGL2RenderingContext.compressedTexSubImage3D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aZoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aFormat: TGLenum; aSrcData: IJSArrayBufferView; aSrcOffset: TGLuint);
begin
  InvokeJSNoResult('compressedTexSubImage3D',[aTarget,aLevel,aXoffset,aYoffset,aZoffset,aWidth,aHeight,aDepth,aFormat,aSrcData,aSrcOffset]);
end;

function TJSWebGL2RenderingContext.getFragDataLocation(aProgram_: IJSWebGLProgram; const aName: UnicodeString): TGLint;
begin
  Result:=InvokeJSLongIntResult('getFragDataLocation',[aProgram_,aName]);
end;

procedure TJSWebGL2RenderingContext.uniform1ui(aLocation: IJSWebGLUniformLocation; aV0: TGLuint);
begin
  InvokeJSNoResult('uniform1ui',[aLocation,aV0]);
end;

procedure TJSWebGL2RenderingContext.uniform2ui(aLocation: IJSWebGLUniformLocation; aV0: TGLuint; aV1: TGLuint);
begin
  InvokeJSNoResult('uniform2ui',[aLocation,aV0,aV1]);
end;

procedure TJSWebGL2RenderingContext.uniform3ui(aLocation: IJSWebGLUniformLocation; aV0: TGLuint; aV1: TGLuint; aV2: TGLuint);
begin
  InvokeJSNoResult('uniform3ui',[aLocation,aV0,aV1,aV2]);
end;

procedure TJSWebGL2RenderingContext.uniform4ui(aLocation: IJSWebGLUniformLocation; aV0: TGLuint; aV1: TGLuint; aV2: TGLuint; aV3: TGLuint);
begin
  InvokeJSNoResult('uniform4ui',[aLocation,aV0,aV1,aV2,aV3]);
end;

procedure TJSWebGL2RenderingContext.uniform1fv(aLocation: IJSWebGLUniformLocation; aData: IJSFloat32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint);
begin
  InvokeJSNoResult('uniform1fv',[aLocation,aData,aSrcOffset,aSrcLength]);
end;

procedure TJSWebGL2RenderingContext.uniform1fv(aLocation: IJSWebGLUniformLocation; const aData: TGLfloatDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint);
begin
  InvokeJSNoResult('uniform1fv',[aLocation,aData,aSrcOffset,aSrcLength]);
end;

procedure TJSWebGL2RenderingContext.uniform1fv(aLocation: IJSWebGLUniformLocation; const aData: TGLfloatDynArray);
begin
  InvokeJSNoResult('uniform1fv',[aLocation,aData]);
end;

procedure TJSWebGL2RenderingContext.uniform1fv(aLocation: IJSWebGLUniformLocation; aData: IJSFloat32Array);
begin
  InvokeJSNoResult('uniform1fv',[aLocation,aData]);
end;

procedure TJSWebGL2RenderingContext.uniform1fv(aLocation: IJSWebGLUniformLocation; const aData: TGLfloatDynArray; aSrcOffset: TGLuint);
begin
  InvokeJSNoResult('uniform1fv',[aLocation,aData,aSrcOffset]);
end;

procedure TJSWebGL2RenderingContext.uniform1fv(aLocation: IJSWebGLUniformLocation; aData: IJSFloat32Array; aSrcOffset: TGLuint);
begin
  InvokeJSNoResult('uniform1fv',[aLocation,aData,aSrcOffset]);
end;

procedure TJSWebGL2RenderingContext.uniform2fv(aLocation: IJSWebGLUniformLocation; aData: IJSFloat32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint);
begin
  InvokeJSNoResult('uniform2fv',[aLocation,aData,aSrcOffset,aSrcLength]);
end;

procedure TJSWebGL2RenderingContext.uniform2fv(aLocation: IJSWebGLUniformLocation; const aData: TGLfloatDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint);
begin
  InvokeJSNoResult('uniform2fv',[aLocation,aData,aSrcOffset,aSrcLength]);
end;

procedure TJSWebGL2RenderingContext.uniform2fv(aLocation: IJSWebGLUniformLocation; const aData: TGLfloatDynArray);
begin
  InvokeJSNoResult('uniform2fv',[aLocation,aData]);
end;

procedure TJSWebGL2RenderingContext.uniform2fv(aLocation: IJSWebGLUniformLocation; aData: IJSFloat32Array);
begin
  InvokeJSNoResult('uniform2fv',[aLocation,aData]);
end;

procedure TJSWebGL2RenderingContext.uniform2fv(aLocation: IJSWebGLUniformLocation; const aData: TGLfloatDynArray; aSrcOffset: TGLuint);
begin
  InvokeJSNoResult('uniform2fv',[aLocation,aData,aSrcOffset]);
end;

procedure TJSWebGL2RenderingContext.uniform2fv(aLocation: IJSWebGLUniformLocation; aData: IJSFloat32Array; aSrcOffset: TGLuint);
begin
  InvokeJSNoResult('uniform2fv',[aLocation,aData,aSrcOffset]);
end;

procedure TJSWebGL2RenderingContext.uniform3fv(aLocation: IJSWebGLUniformLocation; aData: IJSFloat32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint);
begin
  InvokeJSNoResult('uniform3fv',[aLocation,aData,aSrcOffset,aSrcLength]);
end;

procedure TJSWebGL2RenderingContext.uniform3fv(aLocation: IJSWebGLUniformLocation; const aData: TGLfloatDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint);
begin
  InvokeJSNoResult('uniform3fv',[aLocation,aData,aSrcOffset,aSrcLength]);
end;

procedure TJSWebGL2RenderingContext.uniform3fv(aLocation: IJSWebGLUniformLocation; const aData: TGLfloatDynArray);
begin
  InvokeJSNoResult('uniform3fv',[aLocation,aData]);
end;

procedure TJSWebGL2RenderingContext.uniform3fv(aLocation: IJSWebGLUniformLocation; aData: IJSFloat32Array);
begin
  InvokeJSNoResult('uniform3fv',[aLocation,aData]);
end;

procedure TJSWebGL2RenderingContext.uniform3fv(aLocation: IJSWebGLUniformLocation; const aData: TGLfloatDynArray; aSrcOffset: TGLuint);
begin
  InvokeJSNoResult('uniform3fv',[aLocation,aData,aSrcOffset]);
end;

procedure TJSWebGL2RenderingContext.uniform3fv(aLocation: IJSWebGLUniformLocation; aData: IJSFloat32Array; aSrcOffset: TGLuint);
begin
  InvokeJSNoResult('uniform3fv',[aLocation,aData,aSrcOffset]);
end;

procedure TJSWebGL2RenderingContext.uniform4fv(aLocation: IJSWebGLUniformLocation; aData: IJSFloat32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint);
begin
  InvokeJSNoResult('uniform4fv',[aLocation,aData,aSrcOffset,aSrcLength]);
end;

procedure TJSWebGL2RenderingContext.uniform4fv(aLocation: IJSWebGLUniformLocation; const aData: TGLfloatDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint);
begin
  InvokeJSNoResult('uniform4fv',[aLocation,aData,aSrcOffset,aSrcLength]);
end;

procedure TJSWebGL2RenderingContext.uniform4fv(aLocation: IJSWebGLUniformLocation; const aData: TGLfloatDynArray);
begin
  InvokeJSNoResult('uniform4fv',[aLocation,aData]);
end;

procedure TJSWebGL2RenderingContext.uniform4fv(aLocation: IJSWebGLUniformLocation; aData: IJSFloat32Array);
begin
  InvokeJSNoResult('uniform4fv',[aLocation,aData]);
end;

procedure TJSWebGL2RenderingContext.uniform4fv(aLocation: IJSWebGLUniformLocation; const aData: TGLfloatDynArray; aSrcOffset: TGLuint);
begin
  InvokeJSNoResult('uniform4fv',[aLocation,aData,aSrcOffset]);
end;

procedure TJSWebGL2RenderingContext.uniform4fv(aLocation: IJSWebGLUniformLocation; aData: IJSFloat32Array; aSrcOffset: TGLuint);
begin
  InvokeJSNoResult('uniform4fv',[aLocation,aData,aSrcOffset]);
end;

procedure TJSWebGL2RenderingContext.uniform1iv(aLocation: IJSWebGLUniformLocation; aData: IJSInt32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint);
begin
  InvokeJSNoResult('uniform1iv',[aLocation,aData,aSrcOffset,aSrcLength]);
end;

procedure TJSWebGL2RenderingContext.uniform1iv(aLocation: IJSWebGLUniformLocation; const aData: TGLintDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint);
begin
  InvokeJSNoResult('uniform1iv',[aLocation,aData,aSrcOffset,aSrcLength]);
end;

procedure TJSWebGL2RenderingContext.uniform1iv(aLocation: IJSWebGLUniformLocation; const aData: TGLintDynArray);
begin
  InvokeJSNoResult('uniform1iv',[aLocation,aData]);
end;

procedure TJSWebGL2RenderingContext.uniform1iv(aLocation: IJSWebGLUniformLocation; aData: IJSInt32Array);
begin
  InvokeJSNoResult('uniform1iv',[aLocation,aData]);
end;

procedure TJSWebGL2RenderingContext.uniform1iv(aLocation: IJSWebGLUniformLocation; const aData: TGLintDynArray; aSrcOffset: TGLuint);
begin
  InvokeJSNoResult('uniform1iv',[aLocation,aData,aSrcOffset]);
end;

procedure TJSWebGL2RenderingContext.uniform1iv(aLocation: IJSWebGLUniformLocation; aData: IJSInt32Array; aSrcOffset: TGLuint);
begin
  InvokeJSNoResult('uniform1iv',[aLocation,aData,aSrcOffset]);
end;

procedure TJSWebGL2RenderingContext.uniform2iv(aLocation: IJSWebGLUniformLocation; aData: IJSInt32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint);
begin
  InvokeJSNoResult('uniform2iv',[aLocation,aData,aSrcOffset,aSrcLength]);
end;

procedure TJSWebGL2RenderingContext.uniform2iv(aLocation: IJSWebGLUniformLocation; const aData: TGLintDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint);
begin
  InvokeJSNoResult('uniform2iv',[aLocation,aData,aSrcOffset,aSrcLength]);
end;

procedure TJSWebGL2RenderingContext.uniform2iv(aLocation: IJSWebGLUniformLocation; const aData: TGLintDynArray);
begin
  InvokeJSNoResult('uniform2iv',[aLocation,aData]);
end;

procedure TJSWebGL2RenderingContext.uniform2iv(aLocation: IJSWebGLUniformLocation; aData: IJSInt32Array);
begin
  InvokeJSNoResult('uniform2iv',[aLocation,aData]);
end;

procedure TJSWebGL2RenderingContext.uniform2iv(aLocation: IJSWebGLUniformLocation; const aData: TGLintDynArray; aSrcOffset: TGLuint);
begin
  InvokeJSNoResult('uniform2iv',[aLocation,aData,aSrcOffset]);
end;

procedure TJSWebGL2RenderingContext.uniform2iv(aLocation: IJSWebGLUniformLocation; aData: IJSInt32Array; aSrcOffset: TGLuint);
begin
  InvokeJSNoResult('uniform2iv',[aLocation,aData,aSrcOffset]);
end;

procedure TJSWebGL2RenderingContext.uniform3iv(aLocation: IJSWebGLUniformLocation; aData: IJSInt32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint);
begin
  InvokeJSNoResult('uniform3iv',[aLocation,aData,aSrcOffset,aSrcLength]);
end;

procedure TJSWebGL2RenderingContext.uniform3iv(aLocation: IJSWebGLUniformLocation; const aData: TGLintDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint);
begin
  InvokeJSNoResult('uniform3iv',[aLocation,aData,aSrcOffset,aSrcLength]);
end;

procedure TJSWebGL2RenderingContext.uniform3iv(aLocation: IJSWebGLUniformLocation; const aData: TGLintDynArray);
begin
  InvokeJSNoResult('uniform3iv',[aLocation,aData]);
end;

procedure TJSWebGL2RenderingContext.uniform3iv(aLocation: IJSWebGLUniformLocation; aData: IJSInt32Array);
begin
  InvokeJSNoResult('uniform3iv',[aLocation,aData]);
end;

procedure TJSWebGL2RenderingContext.uniform3iv(aLocation: IJSWebGLUniformLocation; const aData: TGLintDynArray; aSrcOffset: TGLuint);
begin
  InvokeJSNoResult('uniform3iv',[aLocation,aData,aSrcOffset]);
end;

procedure TJSWebGL2RenderingContext.uniform3iv(aLocation: IJSWebGLUniformLocation; aData: IJSInt32Array; aSrcOffset: TGLuint);
begin
  InvokeJSNoResult('uniform3iv',[aLocation,aData,aSrcOffset]);
end;

procedure TJSWebGL2RenderingContext.uniform4iv(aLocation: IJSWebGLUniformLocation; aData: IJSInt32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint);
begin
  InvokeJSNoResult('uniform4iv',[aLocation,aData,aSrcOffset,aSrcLength]);
end;

procedure TJSWebGL2RenderingContext.uniform4iv(aLocation: IJSWebGLUniformLocation; const aData: TGLintDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint);
begin
  InvokeJSNoResult('uniform4iv',[aLocation,aData,aSrcOffset,aSrcLength]);
end;

procedure TJSWebGL2RenderingContext.uniform4iv(aLocation: IJSWebGLUniformLocation; const aData: TGLintDynArray);
begin
  InvokeJSNoResult('uniform4iv',[aLocation,aData]);
end;

procedure TJSWebGL2RenderingContext.uniform4iv(aLocation: IJSWebGLUniformLocation; aData: IJSInt32Array);
begin
  InvokeJSNoResult('uniform4iv',[aLocation,aData]);
end;

procedure TJSWebGL2RenderingContext.uniform4iv(aLocation: IJSWebGLUniformLocation; const aData: TGLintDynArray; aSrcOffset: TGLuint);
begin
  InvokeJSNoResult('uniform4iv',[aLocation,aData,aSrcOffset]);
end;

procedure TJSWebGL2RenderingContext.uniform4iv(aLocation: IJSWebGLUniformLocation; aData: IJSInt32Array; aSrcOffset: TGLuint);
begin
  InvokeJSNoResult('uniform4iv',[aLocation,aData,aSrcOffset]);
end;

procedure TJSWebGL2RenderingContext.uniform1uiv(aLocation: IJSWebGLUniformLocation; const aData: TGLuintDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint);
begin
  InvokeJSNoResult('uniform1uiv',[aLocation,aData,aSrcOffset,aSrcLength]);
end;

procedure TJSWebGL2RenderingContext.uniform1uiv(aLocation: IJSWebGLUniformLocation; aData: IJSUint32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint);
begin
  InvokeJSNoResult('uniform1uiv',[aLocation,aData,aSrcOffset,aSrcLength]);
end;

procedure TJSWebGL2RenderingContext.uniform1uiv(aLocation: IJSWebGLUniformLocation; aData: IJSUint32Array);
begin
  InvokeJSNoResult('uniform1uiv',[aLocation,aData]);
end;

procedure TJSWebGL2RenderingContext.uniform1uiv(aLocation: IJSWebGLUniformLocation; const aData: TGLuintDynArray);
begin
  InvokeJSNoResult('uniform1uiv',[aLocation,aData]);
end;

procedure TJSWebGL2RenderingContext.uniform1uiv(aLocation: IJSWebGLUniformLocation; aData: IJSUint32Array; aSrcOffset: TGLuint);
begin
  InvokeJSNoResult('uniform1uiv',[aLocation,aData,aSrcOffset]);
end;

procedure TJSWebGL2RenderingContext.uniform1uiv(aLocation: IJSWebGLUniformLocation; const aData: TGLuintDynArray; aSrcOffset: TGLuint);
begin
  InvokeJSNoResult('uniform1uiv',[aLocation,aData,aSrcOffset]);
end;

procedure TJSWebGL2RenderingContext.uniform2uiv(aLocation: IJSWebGLUniformLocation; const aData: TGLuintDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint);
begin
  InvokeJSNoResult('uniform2uiv',[aLocation,aData,aSrcOffset,aSrcLength]);
end;

procedure TJSWebGL2RenderingContext.uniform2uiv(aLocation: IJSWebGLUniformLocation; aData: IJSUint32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint);
begin
  InvokeJSNoResult('uniform2uiv',[aLocation,aData,aSrcOffset,aSrcLength]);
end;

procedure TJSWebGL2RenderingContext.uniform2uiv(aLocation: IJSWebGLUniformLocation; aData: IJSUint32Array);
begin
  InvokeJSNoResult('uniform2uiv',[aLocation,aData]);
end;

procedure TJSWebGL2RenderingContext.uniform2uiv(aLocation: IJSWebGLUniformLocation; const aData: TGLuintDynArray);
begin
  InvokeJSNoResult('uniform2uiv',[aLocation,aData]);
end;

procedure TJSWebGL2RenderingContext.uniform2uiv(aLocation: IJSWebGLUniformLocation; aData: IJSUint32Array; aSrcOffset: TGLuint);
begin
  InvokeJSNoResult('uniform2uiv',[aLocation,aData,aSrcOffset]);
end;

procedure TJSWebGL2RenderingContext.uniform2uiv(aLocation: IJSWebGLUniformLocation; const aData: TGLuintDynArray; aSrcOffset: TGLuint);
begin
  InvokeJSNoResult('uniform2uiv',[aLocation,aData,aSrcOffset]);
end;

procedure TJSWebGL2RenderingContext.uniform3uiv(aLocation: IJSWebGLUniformLocation; const aData: TGLuintDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint);
begin
  InvokeJSNoResult('uniform3uiv',[aLocation,aData,aSrcOffset,aSrcLength]);
end;

procedure TJSWebGL2RenderingContext.uniform3uiv(aLocation: IJSWebGLUniformLocation; aData: IJSUint32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint);
begin
  InvokeJSNoResult('uniform3uiv',[aLocation,aData,aSrcOffset,aSrcLength]);
end;

procedure TJSWebGL2RenderingContext.uniform3uiv(aLocation: IJSWebGLUniformLocation; aData: IJSUint32Array);
begin
  InvokeJSNoResult('uniform3uiv',[aLocation,aData]);
end;

procedure TJSWebGL2RenderingContext.uniform3uiv(aLocation: IJSWebGLUniformLocation; const aData: TGLuintDynArray);
begin
  InvokeJSNoResult('uniform3uiv',[aLocation,aData]);
end;

procedure TJSWebGL2RenderingContext.uniform3uiv(aLocation: IJSWebGLUniformLocation; aData: IJSUint32Array; aSrcOffset: TGLuint);
begin
  InvokeJSNoResult('uniform3uiv',[aLocation,aData,aSrcOffset]);
end;

procedure TJSWebGL2RenderingContext.uniform3uiv(aLocation: IJSWebGLUniformLocation; const aData: TGLuintDynArray; aSrcOffset: TGLuint);
begin
  InvokeJSNoResult('uniform3uiv',[aLocation,aData,aSrcOffset]);
end;

procedure TJSWebGL2RenderingContext.uniform4uiv(aLocation: IJSWebGLUniformLocation; const aData: TGLuintDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint);
begin
  InvokeJSNoResult('uniform4uiv',[aLocation,aData,aSrcOffset,aSrcLength]);
end;

procedure TJSWebGL2RenderingContext.uniform4uiv(aLocation: IJSWebGLUniformLocation; aData: IJSUint32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint);
begin
  InvokeJSNoResult('uniform4uiv',[aLocation,aData,aSrcOffset,aSrcLength]);
end;

procedure TJSWebGL2RenderingContext.uniform4uiv(aLocation: IJSWebGLUniformLocation; aData: IJSUint32Array);
begin
  InvokeJSNoResult('uniform4uiv',[aLocation,aData]);
end;

procedure TJSWebGL2RenderingContext.uniform4uiv(aLocation: IJSWebGLUniformLocation; const aData: TGLuintDynArray);
begin
  InvokeJSNoResult('uniform4uiv',[aLocation,aData]);
end;

procedure TJSWebGL2RenderingContext.uniform4uiv(aLocation: IJSWebGLUniformLocation; aData: IJSUint32Array; aSrcOffset: TGLuint);
begin
  InvokeJSNoResult('uniform4uiv',[aLocation,aData,aSrcOffset]);
end;

procedure TJSWebGL2RenderingContext.uniform4uiv(aLocation: IJSWebGLUniformLocation; const aData: TGLuintDynArray; aSrcOffset: TGLuint);
begin
  InvokeJSNoResult('uniform4uiv',[aLocation,aData,aSrcOffset]);
end;

procedure TJSWebGL2RenderingContext.uniformMatrix2fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint);
begin
  InvokeJSNoResult('uniformMatrix2fv',[aLocation,aTranspose,aData,aSrcOffset,aSrcLength]);
end;

procedure TJSWebGL2RenderingContext.uniformMatrix2fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint);
begin
  InvokeJSNoResult('uniformMatrix2fv',[aLocation,aTranspose,aData,aSrcOffset,aSrcLength]);
end;

procedure TJSWebGL2RenderingContext.uniformMatrix2fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray);
begin
  InvokeJSNoResult('uniformMatrix2fv',[aLocation,aTranspose,aData]);
end;

procedure TJSWebGL2RenderingContext.uniformMatrix2fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array);
begin
  InvokeJSNoResult('uniformMatrix2fv',[aLocation,aTranspose,aData]);
end;

procedure TJSWebGL2RenderingContext.uniformMatrix2fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray; aSrcOffset: TGLuint);
begin
  InvokeJSNoResult('uniformMatrix2fv',[aLocation,aTranspose,aData,aSrcOffset]);
end;

procedure TJSWebGL2RenderingContext.uniformMatrix2fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array; aSrcOffset: TGLuint);
begin
  InvokeJSNoResult('uniformMatrix2fv',[aLocation,aTranspose,aData,aSrcOffset]);
end;

procedure TJSWebGL2RenderingContext.uniformMatrix3x2fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint);
begin
  InvokeJSNoResult('uniformMatrix3x2fv',[aLocation,aTranspose,aData,aSrcOffset,aSrcLength]);
end;

procedure TJSWebGL2RenderingContext.uniformMatrix3x2fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint);
begin
  InvokeJSNoResult('uniformMatrix3x2fv',[aLocation,aTranspose,aData,aSrcOffset,aSrcLength]);
end;

procedure TJSWebGL2RenderingContext.uniformMatrix3x2fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray);
begin
  InvokeJSNoResult('uniformMatrix3x2fv',[aLocation,aTranspose,aData]);
end;

procedure TJSWebGL2RenderingContext.uniformMatrix3x2fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array);
begin
  InvokeJSNoResult('uniformMatrix3x2fv',[aLocation,aTranspose,aData]);
end;

procedure TJSWebGL2RenderingContext.uniformMatrix3x2fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray; aSrcOffset: TGLuint);
begin
  InvokeJSNoResult('uniformMatrix3x2fv',[aLocation,aTranspose,aData,aSrcOffset]);
end;

procedure TJSWebGL2RenderingContext.uniformMatrix3x2fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array; aSrcOffset: TGLuint);
begin
  InvokeJSNoResult('uniformMatrix3x2fv',[aLocation,aTranspose,aData,aSrcOffset]);
end;

procedure TJSWebGL2RenderingContext.uniformMatrix4x2fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint);
begin
  InvokeJSNoResult('uniformMatrix4x2fv',[aLocation,aTranspose,aData,aSrcOffset,aSrcLength]);
end;

procedure TJSWebGL2RenderingContext.uniformMatrix4x2fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint);
begin
  InvokeJSNoResult('uniformMatrix4x2fv',[aLocation,aTranspose,aData,aSrcOffset,aSrcLength]);
end;

procedure TJSWebGL2RenderingContext.uniformMatrix4x2fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray);
begin
  InvokeJSNoResult('uniformMatrix4x2fv',[aLocation,aTranspose,aData]);
end;

procedure TJSWebGL2RenderingContext.uniformMatrix4x2fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array);
begin
  InvokeJSNoResult('uniformMatrix4x2fv',[aLocation,aTranspose,aData]);
end;

procedure TJSWebGL2RenderingContext.uniformMatrix4x2fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray; aSrcOffset: TGLuint);
begin
  InvokeJSNoResult('uniformMatrix4x2fv',[aLocation,aTranspose,aData,aSrcOffset]);
end;

procedure TJSWebGL2RenderingContext.uniformMatrix4x2fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array; aSrcOffset: TGLuint);
begin
  InvokeJSNoResult('uniformMatrix4x2fv',[aLocation,aTranspose,aData,aSrcOffset]);
end;

procedure TJSWebGL2RenderingContext.uniformMatrix2x3fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint);
begin
  InvokeJSNoResult('uniformMatrix2x3fv',[aLocation,aTranspose,aData,aSrcOffset,aSrcLength]);
end;

procedure TJSWebGL2RenderingContext.uniformMatrix2x3fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint);
begin
  InvokeJSNoResult('uniformMatrix2x3fv',[aLocation,aTranspose,aData,aSrcOffset,aSrcLength]);
end;

procedure TJSWebGL2RenderingContext.uniformMatrix2x3fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray);
begin
  InvokeJSNoResult('uniformMatrix2x3fv',[aLocation,aTranspose,aData]);
end;

procedure TJSWebGL2RenderingContext.uniformMatrix2x3fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array);
begin
  InvokeJSNoResult('uniformMatrix2x3fv',[aLocation,aTranspose,aData]);
end;

procedure TJSWebGL2RenderingContext.uniformMatrix2x3fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray; aSrcOffset: TGLuint);
begin
  InvokeJSNoResult('uniformMatrix2x3fv',[aLocation,aTranspose,aData,aSrcOffset]);
end;

procedure TJSWebGL2RenderingContext.uniformMatrix2x3fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array; aSrcOffset: TGLuint);
begin
  InvokeJSNoResult('uniformMatrix2x3fv',[aLocation,aTranspose,aData,aSrcOffset]);
end;

procedure TJSWebGL2RenderingContext.uniformMatrix3fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint);
begin
  InvokeJSNoResult('uniformMatrix3fv',[aLocation,aTranspose,aData,aSrcOffset,aSrcLength]);
end;

procedure TJSWebGL2RenderingContext.uniformMatrix3fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint);
begin
  InvokeJSNoResult('uniformMatrix3fv',[aLocation,aTranspose,aData,aSrcOffset,aSrcLength]);
end;

procedure TJSWebGL2RenderingContext.uniformMatrix3fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray);
begin
  InvokeJSNoResult('uniformMatrix3fv',[aLocation,aTranspose,aData]);
end;

procedure TJSWebGL2RenderingContext.uniformMatrix3fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array);
begin
  InvokeJSNoResult('uniformMatrix3fv',[aLocation,aTranspose,aData]);
end;

procedure TJSWebGL2RenderingContext.uniformMatrix3fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray; aSrcOffset: TGLuint);
begin
  InvokeJSNoResult('uniformMatrix3fv',[aLocation,aTranspose,aData,aSrcOffset]);
end;

procedure TJSWebGL2RenderingContext.uniformMatrix3fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array; aSrcOffset: TGLuint);
begin
  InvokeJSNoResult('uniformMatrix3fv',[aLocation,aTranspose,aData,aSrcOffset]);
end;

procedure TJSWebGL2RenderingContext.uniformMatrix4x3fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint);
begin
  InvokeJSNoResult('uniformMatrix4x3fv',[aLocation,aTranspose,aData,aSrcOffset,aSrcLength]);
end;

procedure TJSWebGL2RenderingContext.uniformMatrix4x3fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint);
begin
  InvokeJSNoResult('uniformMatrix4x3fv',[aLocation,aTranspose,aData,aSrcOffset,aSrcLength]);
end;

procedure TJSWebGL2RenderingContext.uniformMatrix4x3fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray);
begin
  InvokeJSNoResult('uniformMatrix4x3fv',[aLocation,aTranspose,aData]);
end;

procedure TJSWebGL2RenderingContext.uniformMatrix4x3fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array);
begin
  InvokeJSNoResult('uniformMatrix4x3fv',[aLocation,aTranspose,aData]);
end;

procedure TJSWebGL2RenderingContext.uniformMatrix4x3fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray; aSrcOffset: TGLuint);
begin
  InvokeJSNoResult('uniformMatrix4x3fv',[aLocation,aTranspose,aData,aSrcOffset]);
end;

procedure TJSWebGL2RenderingContext.uniformMatrix4x3fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array; aSrcOffset: TGLuint);
begin
  InvokeJSNoResult('uniformMatrix4x3fv',[aLocation,aTranspose,aData,aSrcOffset]);
end;

procedure TJSWebGL2RenderingContext.uniformMatrix2x4fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint);
begin
  InvokeJSNoResult('uniformMatrix2x4fv',[aLocation,aTranspose,aData,aSrcOffset,aSrcLength]);
end;

procedure TJSWebGL2RenderingContext.uniformMatrix2x4fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint);
begin
  InvokeJSNoResult('uniformMatrix2x4fv',[aLocation,aTranspose,aData,aSrcOffset,aSrcLength]);
end;

procedure TJSWebGL2RenderingContext.uniformMatrix2x4fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray);
begin
  InvokeJSNoResult('uniformMatrix2x4fv',[aLocation,aTranspose,aData]);
end;

procedure TJSWebGL2RenderingContext.uniformMatrix2x4fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array);
begin
  InvokeJSNoResult('uniformMatrix2x4fv',[aLocation,aTranspose,aData]);
end;

procedure TJSWebGL2RenderingContext.uniformMatrix2x4fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray; aSrcOffset: TGLuint);
begin
  InvokeJSNoResult('uniformMatrix2x4fv',[aLocation,aTranspose,aData,aSrcOffset]);
end;

procedure TJSWebGL2RenderingContext.uniformMatrix2x4fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array; aSrcOffset: TGLuint);
begin
  InvokeJSNoResult('uniformMatrix2x4fv',[aLocation,aTranspose,aData,aSrcOffset]);
end;

procedure TJSWebGL2RenderingContext.uniformMatrix3x4fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint);
begin
  InvokeJSNoResult('uniformMatrix3x4fv',[aLocation,aTranspose,aData,aSrcOffset,aSrcLength]);
end;

procedure TJSWebGL2RenderingContext.uniformMatrix3x4fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint);
begin
  InvokeJSNoResult('uniformMatrix3x4fv',[aLocation,aTranspose,aData,aSrcOffset,aSrcLength]);
end;

procedure TJSWebGL2RenderingContext.uniformMatrix3x4fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray);
begin
  InvokeJSNoResult('uniformMatrix3x4fv',[aLocation,aTranspose,aData]);
end;

procedure TJSWebGL2RenderingContext.uniformMatrix3x4fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array);
begin
  InvokeJSNoResult('uniformMatrix3x4fv',[aLocation,aTranspose,aData]);
end;

procedure TJSWebGL2RenderingContext.uniformMatrix3x4fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray; aSrcOffset: TGLuint);
begin
  InvokeJSNoResult('uniformMatrix3x4fv',[aLocation,aTranspose,aData,aSrcOffset]);
end;

procedure TJSWebGL2RenderingContext.uniformMatrix3x4fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array; aSrcOffset: TGLuint);
begin
  InvokeJSNoResult('uniformMatrix3x4fv',[aLocation,aTranspose,aData,aSrcOffset]);
end;

procedure TJSWebGL2RenderingContext.uniformMatrix4fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint);
begin
  InvokeJSNoResult('uniformMatrix4fv',[aLocation,aTranspose,aData,aSrcOffset,aSrcLength]);
end;

procedure TJSWebGL2RenderingContext.uniformMatrix4fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint);
begin
  InvokeJSNoResult('uniformMatrix4fv',[aLocation,aTranspose,aData,aSrcOffset,aSrcLength]);
end;

procedure TJSWebGL2RenderingContext.uniformMatrix4fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray);
begin
  InvokeJSNoResult('uniformMatrix4fv',[aLocation,aTranspose,aData]);
end;

procedure TJSWebGL2RenderingContext.uniformMatrix4fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array);
begin
  InvokeJSNoResult('uniformMatrix4fv',[aLocation,aTranspose,aData]);
end;

procedure TJSWebGL2RenderingContext.uniformMatrix4fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray; aSrcOffset: TGLuint);
begin
  InvokeJSNoResult('uniformMatrix4fv',[aLocation,aTranspose,aData,aSrcOffset]);
end;

procedure TJSWebGL2RenderingContext.uniformMatrix4fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array; aSrcOffset: TGLuint);
begin
  InvokeJSNoResult('uniformMatrix4fv',[aLocation,aTranspose,aData,aSrcOffset]);
end;

procedure TJSWebGL2RenderingContext.vertexAttribI4i(aIndex: TGLuint; aX: TGLint; aY: TGLint; aZ: TGLint; aW: TGLint);
begin
  InvokeJSNoResult('vertexAttribI4i',[aIndex,aX,aY,aZ,aW]);
end;

procedure TJSWebGL2RenderingContext.vertexAttribI4iv(aIndex: TGLuint; aValues: IJSInt32Array);
begin
  InvokeJSNoResult('vertexAttribI4iv',[aIndex,aValues]);
end;

procedure TJSWebGL2RenderingContext.vertexAttribI4iv(aIndex: TGLuint; const aValues: TGLintDynArray);
begin
  InvokeJSNoResult('vertexAttribI4iv',[aIndex,aValues]);
end;

procedure TJSWebGL2RenderingContext.vertexAttribI4ui(aIndex: TGLuint; aX: TGLuint; aY: TGLuint; aZ: TGLuint; aW: TGLuint);
begin
  InvokeJSNoResult('vertexAttribI4ui',[aIndex,aX,aY,aZ,aW]);
end;

procedure TJSWebGL2RenderingContext.vertexAttribI4uiv(aIndex: TGLuint; const aValues: TGLuintDynArray);
begin
  InvokeJSNoResult('vertexAttribI4uiv',[aIndex,aValues]);
end;

procedure TJSWebGL2RenderingContext.vertexAttribI4uiv(aIndex: TGLuint; aValues: IJSUint32Array);
begin
  InvokeJSNoResult('vertexAttribI4uiv',[aIndex,aValues]);
end;

procedure TJSWebGL2RenderingContext.vertexAttribIPointer(aIndex: TGLuint; aSize: TGLint; aType_: TGLenum; aStride: TGLsizei; aOffset: TGLintptr);
begin
  InvokeJSNoResult('vertexAttribIPointer',[aIndex,aSize,aType_,aStride,aOffset]);
end;

procedure TJSWebGL2RenderingContext.vertexAttribDivisor(aIndex: TGLuint; aDivisor: TGLuint);
begin
  InvokeJSNoResult('vertexAttribDivisor',[aIndex,aDivisor]);
end;

procedure TJSWebGL2RenderingContext.drawArraysInstanced(aMode: TGLenum; aFirst: TGLint; aCount: TGLsizei; aInstanceCount: TGLsizei);
begin
  InvokeJSNoResult('drawArraysInstanced',[aMode,aFirst,aCount,aInstanceCount]);
end;

procedure TJSWebGL2RenderingContext.drawElementsInstanced(aMode: TGLenum; aCount: TGLsizei; aType_: TGLenum; aOffset: TGLintptr; aInstanceCount: TGLsizei);
begin
  InvokeJSNoResult('drawElementsInstanced',[aMode,aCount,aType_,aOffset,aInstanceCount]);
end;

procedure TJSWebGL2RenderingContext.drawRangeElements(aMode: TGLenum; aStart: TGLuint; aEnd_: TGLuint; aCount: TGLsizei; aType_: TGLenum; aOffset: TGLintptr);
begin
  InvokeJSNoResult('drawRangeElements',[aMode,aStart,aEnd_,aCount,aType_,aOffset]);
end;

procedure TJSWebGL2RenderingContext.readPixels(aX: TGLint; aY: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aDstData: IJSArrayBufferView);
begin
  InvokeJSNoResult('readPixels',[aX,aY,aWidth,aHeight,aFormat,aType_,aDstData]);
end;

procedure TJSWebGL2RenderingContext.readPixels(aX: TGLint; aY: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aOffset: TGLintptr);
begin
  InvokeJSNoResult('readPixels',[aX,aY,aWidth,aHeight,aFormat,aType_,aOffset]);
end;

procedure TJSWebGL2RenderingContext.readPixels(aX: TGLint; aY: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aDstData: IJSArrayBufferView; aDstOffset: TGLuint);
begin
  InvokeJSNoResult('readPixels',[aX,aY,aWidth,aHeight,aFormat,aType_,aDstData,aDstOffset]);
end;

procedure TJSWebGL2RenderingContext.drawBuffers(const aBuffers: TGLenumDynArray);
begin
  InvokeJSNoResult('drawBuffers',[aBuffers]);
end;

procedure TJSWebGL2RenderingContext.clearBufferfv(aBuffer: TGLenum; aDrawbuffer: TGLint; aValues: IJSFloat32Array; aSrcOffset: TGLuint);
begin
  InvokeJSNoResult('clearBufferfv',[aBuffer,aDrawbuffer,aValues,aSrcOffset]);
end;

procedure TJSWebGL2RenderingContext.clearBufferfv(aBuffer: TGLenum; aDrawbuffer: TGLint; const aValues: TGLfloatDynArray; aSrcOffset: TGLuint);
begin
  InvokeJSNoResult('clearBufferfv',[aBuffer,aDrawbuffer,aValues,aSrcOffset]);
end;

procedure TJSWebGL2RenderingContext.clearBufferfv(aBuffer: TGLenum; aDrawbuffer: TGLint; const aValues: TGLfloatDynArray);
begin
  InvokeJSNoResult('clearBufferfv',[aBuffer,aDrawbuffer,aValues]);
end;

procedure TJSWebGL2RenderingContext.clearBufferfv(aBuffer: TGLenum; aDrawbuffer: TGLint; aValues: IJSFloat32Array);
begin
  InvokeJSNoResult('clearBufferfv',[aBuffer,aDrawbuffer,aValues]);
end;

procedure TJSWebGL2RenderingContext.clearBufferiv(aBuffer: TGLenum; aDrawbuffer: TGLint; aValues: IJSInt32Array; aSrcOffset: TGLuint);
begin
  InvokeJSNoResult('clearBufferiv',[aBuffer,aDrawbuffer,aValues,aSrcOffset]);
end;

procedure TJSWebGL2RenderingContext.clearBufferiv(aBuffer: TGLenum; aDrawbuffer: TGLint; const aValues: TGLintDynArray; aSrcOffset: TGLuint);
begin
  InvokeJSNoResult('clearBufferiv',[aBuffer,aDrawbuffer,aValues,aSrcOffset]);
end;

procedure TJSWebGL2RenderingContext.clearBufferiv(aBuffer: TGLenum; aDrawbuffer: TGLint; const aValues: TGLintDynArray);
begin
  InvokeJSNoResult('clearBufferiv',[aBuffer,aDrawbuffer,aValues]);
end;

procedure TJSWebGL2RenderingContext.clearBufferiv(aBuffer: TGLenum; aDrawbuffer: TGLint; aValues: IJSInt32Array);
begin
  InvokeJSNoResult('clearBufferiv',[aBuffer,aDrawbuffer,aValues]);
end;

procedure TJSWebGL2RenderingContext.clearBufferuiv(aBuffer: TGLenum; aDrawbuffer: TGLint; const aValues: TGLuintDynArray; aSrcOffset: TGLuint);
begin
  InvokeJSNoResult('clearBufferuiv',[aBuffer,aDrawbuffer,aValues,aSrcOffset]);
end;

procedure TJSWebGL2RenderingContext.clearBufferuiv(aBuffer: TGLenum; aDrawbuffer: TGLint; aValues: IJSUint32Array; aSrcOffset: TGLuint);
begin
  InvokeJSNoResult('clearBufferuiv',[aBuffer,aDrawbuffer,aValues,aSrcOffset]);
end;

procedure TJSWebGL2RenderingContext.clearBufferuiv(aBuffer: TGLenum; aDrawbuffer: TGLint; aValues: IJSUint32Array);
begin
  InvokeJSNoResult('clearBufferuiv',[aBuffer,aDrawbuffer,aValues]);
end;

procedure TJSWebGL2RenderingContext.clearBufferuiv(aBuffer: TGLenum; aDrawbuffer: TGLint; const aValues: TGLuintDynArray);
begin
  InvokeJSNoResult('clearBufferuiv',[aBuffer,aDrawbuffer,aValues]);
end;

procedure TJSWebGL2RenderingContext.clearBufferfi(aBuffer: TGLenum; aDrawbuffer: TGLint; aDepth: TGLfloat; aStencil: TGLint);
begin
  InvokeJSNoResult('clearBufferfi',[aBuffer,aDrawbuffer,aDepth,aStencil]);
end;

function TJSWebGL2RenderingContext.createQuery: IJSWebGLQuery;
begin
  Result:=InvokeJSObjectResult('createQuery',[],TJSWebGLQuery) as IJSWebGLQuery;
end;

procedure TJSWebGL2RenderingContext.deleteQuery(aQuery: IJSWebGLQuery);
begin
  InvokeJSNoResult('deleteQuery',[aQuery]);
end;

function TJSWebGL2RenderingContext.isQuery(aQuery: IJSWebGLQuery): TGLboolean;
begin
  Result:=InvokeJSBooleanResult('isQuery',[aQuery]);
end;

procedure TJSWebGL2RenderingContext.beginQuery(aTarget: TGLenum; aQuery: IJSWebGLQuery);
begin
  InvokeJSNoResult('beginQuery',[aTarget,aQuery]);
end;

procedure TJSWebGL2RenderingContext.endQuery(aTarget: TGLenum);
begin
  InvokeJSNoResult('endQuery',[aTarget]);
end;

function TJSWebGL2RenderingContext.getQuery(aTarget: TGLenum; aPname: TGLenum): Variant;
begin
  Result:=InvokeJSVariantResult('getQuery',[aTarget,aPname]);
end;

function TJSWebGL2RenderingContext.getQueryParameter(aQuery: IJSWebGLQuery; aPname: TGLenum): Variant;
begin
  Result:=InvokeJSVariantResult('getQueryParameter',[aQuery,aPname]);
end;

function TJSWebGL2RenderingContext.createSampler: IJSWebGLSampler;
begin
  Result:=InvokeJSObjectResult('createSampler',[],TJSWebGLSampler) as IJSWebGLSampler;
end;

procedure TJSWebGL2RenderingContext.deleteSampler(aSampler: IJSWebGLSampler);
begin
  InvokeJSNoResult('deleteSampler',[aSampler]);
end;

function TJSWebGL2RenderingContext.isSampler(aSampler: IJSWebGLSampler): TGLboolean;
begin
  Result:=InvokeJSBooleanResult('isSampler',[aSampler]);
end;

procedure TJSWebGL2RenderingContext.bindSampler(aUnit_: TGLuint; aSampler: IJSWebGLSampler);
begin
  InvokeJSNoResult('bindSampler',[aUnit_,aSampler]);
end;

procedure TJSWebGL2RenderingContext.samplerParameteri(aSampler: IJSWebGLSampler; aPname: TGLenum; aParam: TGLint);
begin
  InvokeJSNoResult('samplerParameteri',[aSampler,aPname,aParam]);
end;

procedure TJSWebGL2RenderingContext.samplerParameterf(aSampler: IJSWebGLSampler; aPname: TGLenum; aParam: TGLfloat);
begin
  InvokeJSNoResult('samplerParameterf',[aSampler,aPname,aParam]);
end;

function TJSWebGL2RenderingContext.getSamplerParameter(aSampler: IJSWebGLSampler; aPname: TGLenum): Variant;
begin
  Result:=InvokeJSVariantResult('getSamplerParameter',[aSampler,aPname]);
end;

function TJSWebGL2RenderingContext.fenceSync(aCondition: TGLenum; aFlags: TGLbitfield): IJSWebGLSync;
begin
  Result:=InvokeJSObjectResult('fenceSync',[aCondition,aFlags],TJSWebGLSync) as IJSWebGLSync;
end;

function TJSWebGL2RenderingContext.isSync(aSync: IJSWebGLSync): TGLboolean;
begin
  Result:=InvokeJSBooleanResult('isSync',[aSync]);
end;

procedure TJSWebGL2RenderingContext.deleteSync(aSync: IJSWebGLSync);
begin
  InvokeJSNoResult('deleteSync',[aSync]);
end;

function TJSWebGL2RenderingContext.clientWaitSync(aSync: IJSWebGLSync; aFlags: TGLbitfield; aTimeout: TGLuint64): TGLenum;
begin
  Result:=InvokeJSLongIntResult('clientWaitSync',[aSync,aFlags,aTimeout]);
end;

procedure TJSWebGL2RenderingContext.waitSync(aSync: IJSWebGLSync; aFlags: TGLbitfield; aTimeout: TGLint64);
begin
  InvokeJSNoResult('waitSync',[aSync,aFlags,aTimeout]);
end;

function TJSWebGL2RenderingContext.getSyncParameter(aSync: IJSWebGLSync; aPname: TGLenum): Variant;
begin
  Result:=InvokeJSVariantResult('getSyncParameter',[aSync,aPname]);
end;

function TJSWebGL2RenderingContext.createTransformFeedback: IJSWebGLTransformFeedback;
begin
  Result:=InvokeJSObjectResult('createTransformFeedback',[],TJSWebGLTransformFeedback) as IJSWebGLTransformFeedback;
end;

procedure TJSWebGL2RenderingContext.deleteTransformFeedback(aTf: IJSWebGLTransformFeedback);
begin
  InvokeJSNoResult('deleteTransformFeedback',[aTf]);
end;

function TJSWebGL2RenderingContext.isTransformFeedback(aTf: IJSWebGLTransformFeedback): TGLboolean;
begin
  Result:=InvokeJSBooleanResult('isTransformFeedback',[aTf]);
end;

procedure TJSWebGL2RenderingContext.bindTransformFeedback(aTarget: TGLenum; aTf: IJSWebGLTransformFeedback);
begin
  InvokeJSNoResult('bindTransformFeedback',[aTarget,aTf]);
end;

procedure TJSWebGL2RenderingContext.beginTransformFeedback(aPrimitiveMode: TGLenum);
begin
  InvokeJSNoResult('beginTransformFeedback',[aPrimitiveMode]);
end;

procedure TJSWebGL2RenderingContext.endTransformFeedback;
begin
  InvokeJSNoResult('endTransformFeedback',[]);
end;

procedure TJSWebGL2RenderingContext.transformFeedbackVaryings(aProgram_: IJSWebGLProgram; const aVaryings: TUnicodeStringDynArray; aBufferMode: TGLenum);
begin
  InvokeJSNoResult('transformFeedbackVaryings',[aProgram_,aVaryings,aBufferMode]);
end;

function TJSWebGL2RenderingContext.getTransformFeedbackVarying(aProgram_: IJSWebGLProgram; aIndex: TGLuint): IJSWebGLActiveInfo;
begin
  Result:=InvokeJSObjectResult('getTransformFeedbackVarying',[aProgram_,aIndex],TJSWebGLActiveInfo) as IJSWebGLActiveInfo;
end;

procedure TJSWebGL2RenderingContext.pauseTransformFeedback;
begin
  InvokeJSNoResult('pauseTransformFeedback',[]);
end;

procedure TJSWebGL2RenderingContext.resumeTransformFeedback;
begin
  InvokeJSNoResult('resumeTransformFeedback',[]);
end;

procedure TJSWebGL2RenderingContext.bindBufferBase(aTarget: TGLenum; aIndex: TGLuint; aBuffer: IJSWebGLBuffer);
begin
  InvokeJSNoResult('bindBufferBase',[aTarget,aIndex,aBuffer]);
end;

procedure TJSWebGL2RenderingContext.bindBufferRange(aTarget: TGLenum; aIndex: TGLuint; aBuffer: IJSWebGLBuffer; aOffset: TGLintptr; aSize: TGLsizeiptr);
begin
  InvokeJSNoResult('bindBufferRange',[aTarget,aIndex,aBuffer,aOffset,aSize]);
end;

function TJSWebGL2RenderingContext.getIndexedParameter(aTarget: TGLenum; aIndex: TGLuint): Variant;
begin
  Result:=InvokeJSVariantResult('getIndexedParameter',[aTarget,aIndex]);
end;

function TJSWebGL2RenderingContext.getUniformIndices(aProgram_: IJSWebGLProgram; const aUniformNames: TUnicodeStringDynArray): TGLuintDynArray;
begin
  Result:=InvokeJSObjectResult('getUniformIndices',[aProgram_,aUniformNames],TJSArray) as TGLuintDynArray;
end;

function TJSWebGL2RenderingContext.getActiveUniforms(aProgram_: IJSWebGLProgram; const aUniformIndices: TGLuintDynArray; aPname: TGLenum): Variant;
begin
  Result:=InvokeJSVariantResult('getActiveUniforms',[aProgram_,aUniformIndices,aPname]);
end;

function TJSWebGL2RenderingContext.getUniformBlockIndex(aProgram_: IJSWebGLProgram; const aUniformBlockName: UnicodeString): TGLuint;
begin
  Result:=InvokeJSLongIntResult('getUniformBlockIndex',[aProgram_,aUniformBlockName]);
end;

function TJSWebGL2RenderingContext.getActiveUniformBlockParameter(aProgram_: IJSWebGLProgram; aUniformBlockIndex: TGLuint; aPname: TGLenum): Variant;
begin
  Result:=InvokeJSVariantResult('getActiveUniformBlockParameter',[aProgram_,aUniformBlockIndex,aPname]);
end;

function TJSWebGL2RenderingContext.getActiveUniformBlockName(aProgram_: IJSWebGLProgram; aUniformBlockIndex: TGLuint): UnicodeString;
begin
  Result:=InvokeJSUnicodeStringResult('getActiveUniformBlockName',[aProgram_,aUniformBlockIndex]);
end;

procedure TJSWebGL2RenderingContext.uniformBlockBinding(aProgram_: IJSWebGLProgram; aUniformBlockIndex: TGLuint; aUniformBlockBinding: TGLuint);
begin
  InvokeJSNoResult('uniformBlockBinding',[aProgram_,aUniformBlockIndex,aUniformBlockBinding]);
end;

function TJSWebGL2RenderingContext.createVertexArray: IJSWebGLVertexArrayObject;
begin
  Result:=InvokeJSObjectResult('createVertexArray',[],TJSWebGLVertexArrayObject) as IJSWebGLVertexArrayObject;
end;

procedure TJSWebGL2RenderingContext.deleteVertexArray(aVertexArray: IJSWebGLVertexArrayObject);
begin
  InvokeJSNoResult('deleteVertexArray',[aVertexArray]);
end;

function TJSWebGL2RenderingContext.isVertexArray(aVertexArray: IJSWebGLVertexArrayObject): TGLboolean;
begin
  Result:=InvokeJSBooleanResult('isVertexArray',[aVertexArray]);
end;

procedure TJSWebGL2RenderingContext.bindVertexArray(aArray_: IJSWebGLVertexArrayObject);
begin
  InvokeJSNoResult('bindVertexArray',[aArray_]);
end;

class function TJSWebGL2RenderingContext.JSClassName: UnicodeString;
begin
  Result:='WebGL2RenderingContext';
end;

class function TJSWebGL2RenderingContext.Cast(const Intf: IJSObject): IJSWebGL2RenderingContext;
begin
  Result:=TJSWebGL2RenderingContext.JOBCast(Intf);
end;

procedure TJSWebGL2RenderingContextBase.bufferData(aTarget: TGLenum; aSize: TGLsizeiptr; aUsage: TGLenum);
begin
  InvokeJSNoResult('bufferData',[aTarget,aSize,aUsage]);
end;

procedure TJSWebGL2RenderingContextBase.bufferData(aTarget: TGLenum; aSrcData: IJSArrayBuffer; aUsage: TGLenum);
begin
  InvokeJSNoResult('bufferData',[aTarget,aSrcData,aUsage]);
end;

procedure TJSWebGL2RenderingContextBase.bufferData(aTarget: TGLenum; aSrcData: IJSArrayBufferView; aUsage: TGLenum);
begin
  InvokeJSNoResult('bufferData',[aTarget,aSrcData,aUsage]);
end;

procedure TJSWebGL2RenderingContextBase.bufferSubData(aTarget: TGLenum; aOffset: TGLintptr; aSrcData: IJSArrayBuffer);
begin
  InvokeJSNoResult('bufferSubData',[aTarget,aOffset,aSrcData]);
end;

procedure TJSWebGL2RenderingContextBase.bufferSubData(aTarget: TGLenum; aOffset: TGLintptr; aSrcData: IJSArrayBufferView);
begin
  InvokeJSNoResult('bufferSubData',[aTarget,aOffset,aSrcData]);
end;

procedure TJSWebGL2RenderingContextBase.bufferData(aTarget: TGLenum; aSrcData: IJSArrayBufferView; aUsage: TGLenum; aSrcOffset: TGLuint; aLength_: TGLuint);
begin
  InvokeJSNoResult('bufferData',[aTarget,aSrcData,aUsage,aSrcOffset,aLength_]);
end;

procedure TJSWebGL2RenderingContextBase.bufferData(aTarget: TGLenum; aSrcData: IJSArrayBufferView; aUsage: TGLenum; aSrcOffset: TGLuint);
begin
  InvokeJSNoResult('bufferData',[aTarget,aSrcData,aUsage,aSrcOffset]);
end;

procedure TJSWebGL2RenderingContextBase.bufferSubData(aTarget: TGLenum; aDstByteOffset: TGLintptr; aSrcData: IJSArrayBufferView; aSrcOffset: TGLuint; aLength_: TGLuint);
begin
  InvokeJSNoResult('bufferSubData',[aTarget,aDstByteOffset,aSrcData,aSrcOffset,aLength_]);
end;

procedure TJSWebGL2RenderingContextBase.bufferSubData(aTarget: TGLenum; aDstByteOffset: TGLintptr; aSrcData: IJSArrayBufferView; aSrcOffset: TGLuint);
begin
  InvokeJSNoResult('bufferSubData',[aTarget,aDstByteOffset,aSrcData,aSrcOffset]);
end;

procedure TJSWebGL2RenderingContextBase.copyBufferSubData(aReadTarget: TGLenum; aWriteTarget: TGLenum; aReadOffset: TGLintptr; aWriteOffset: TGLintptr; aSize: TGLsizeiptr);
begin
  InvokeJSNoResult('copyBufferSubData',[aReadTarget,aWriteTarget,aReadOffset,aWriteOffset,aSize]);
end;

procedure TJSWebGL2RenderingContextBase.getBufferSubData(aTarget: TGLenum; aSrcByteOffset: TGLintptr; aDstData: IJSArrayBufferView; aDstOffset: TGLuint; aLength_: TGLuint);
begin
  InvokeJSNoResult('getBufferSubData',[aTarget,aSrcByteOffset,aDstData,aDstOffset,aLength_]);
end;

procedure TJSWebGL2RenderingContextBase.getBufferSubData(aTarget: TGLenum; aSrcByteOffset: TGLintptr; aDstData: IJSArrayBufferView);
begin
  InvokeJSNoResult('getBufferSubData',[aTarget,aSrcByteOffset,aDstData]);
end;

procedure TJSWebGL2RenderingContextBase.getBufferSubData(aTarget: TGLenum; aSrcByteOffset: TGLintptr; aDstData: IJSArrayBufferView; aDstOffset: TGLuint);
begin
  InvokeJSNoResult('getBufferSubData',[aTarget,aSrcByteOffset,aDstData,aDstOffset]);
end;

procedure TJSWebGL2RenderingContextBase.blitFramebuffer(aSrcX0: TGLint; aSrcY0: TGLint; aSrcX1: TGLint; aSrcY1: TGLint; aDstX0: TGLint; aDstY0: TGLint; aDstX1: TGLint; aDstY1: TGLint; aMask: TGLbitfield; aFilter: TGLenum);
begin
  InvokeJSNoResult('blitFramebuffer',[aSrcX0,aSrcY0,aSrcX1,aSrcY1,aDstX0,aDstY0,aDstX1,aDstY1,aMask,aFilter]);
end;

procedure TJSWebGL2RenderingContextBase.framebufferTextureLayer(aTarget: TGLenum; aAttachment: TGLenum; aTexture: IJSWebGLTexture; aLevel: TGLint; aLayer: TGLint);
begin
  InvokeJSNoResult('framebufferTextureLayer',[aTarget,aAttachment,aTexture,aLevel,aLayer]);
end;

procedure TJSWebGL2RenderingContextBase.invalidateFramebuffer(aTarget: TGLenum; const aAttachments: TGLenumDynArray);
begin
  InvokeJSNoResult('invalidateFramebuffer',[aTarget,aAttachments]);
end;

procedure TJSWebGL2RenderingContextBase.invalidateSubFramebuffer(aTarget: TGLenum; const aAttachments: TGLenumDynArray; aX: TGLint; aY: TGLint; aWidth: TGLsizei; aHeight: TGLsizei);
begin
  InvokeJSNoResult('invalidateSubFramebuffer',[aTarget,aAttachments,aX,aY,aWidth,aHeight]);
end;

procedure TJSWebGL2RenderingContextBase.readBuffer(aSrc: TGLenum);
begin
  InvokeJSNoResult('readBuffer',[aSrc]);
end;

function TJSWebGL2RenderingContextBase.getInternalformatParameter(aTarget: TGLenum; aInternalformat: TGLenum; aPname: TGLenum): Variant;
begin
  Result:=InvokeJSVariantResult('getInternalformatParameter',[aTarget,aInternalformat,aPname]);
end;

procedure TJSWebGL2RenderingContextBase.renderbufferStorageMultisample(aTarget: TGLenum; aSamples: TGLsizei; aInternalformat: TGLenum; aWidth: TGLsizei; aHeight: TGLsizei);
begin
  InvokeJSNoResult('renderbufferStorageMultisample',[aTarget,aSamples,aInternalformat,aWidth,aHeight]);
end;

procedure TJSWebGL2RenderingContextBase.texStorage2D(aTarget: TGLenum; aLevels: TGLsizei; aInternalformat: TGLenum; aWidth: TGLsizei; aHeight: TGLsizei);
begin
  InvokeJSNoResult('texStorage2D',[aTarget,aLevels,aInternalformat,aWidth,aHeight]);
end;

procedure TJSWebGL2RenderingContextBase.texStorage3D(aTarget: TGLenum; aLevels: TGLsizei; aInternalformat: TGLenum; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei);
begin
  InvokeJSNoResult('texStorage3D',[aTarget,aLevels,aInternalformat,aWidth,aHeight,aDepth]);
end;

procedure TJSWebGL2RenderingContextBase.texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aPixels: IJSArrayBufferView);
begin
  InvokeJSNoResult('texImage2D',[aTarget,aLevel,aInternalformat,aWidth,aHeight,aBorder,aFormat,aType_,aPixels]);
end;

procedure TJSWebGL2RenderingContextBase.texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSHTMLCanvasElement);
begin
  InvokeJSNoResult('texImage2D',[aTarget,aLevel,aInternalformat,aFormat,aType_,aSource]);
end;

procedure TJSWebGL2RenderingContextBase.texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSHTMLImageElement);
begin
  InvokeJSNoResult('texImage2D',[aTarget,aLevel,aInternalformat,aFormat,aType_,aSource]);
end;

procedure TJSWebGL2RenderingContextBase.texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSHTMLVideoElement);
begin
  InvokeJSNoResult('texImage2D',[aTarget,aLevel,aInternalformat,aFormat,aType_,aSource]);
end;

procedure TJSWebGL2RenderingContextBase.texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSImageBitmap);
begin
  InvokeJSNoResult('texImage2D',[aTarget,aLevel,aInternalformat,aFormat,aType_,aSource]);
end;

procedure TJSWebGL2RenderingContextBase.texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSImageData);
begin
  InvokeJSNoResult('texImage2D',[aTarget,aLevel,aInternalformat,aFormat,aType_,aSource]);
end;

procedure TJSWebGL2RenderingContextBase.texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSOffscreenCanvas);
begin
  InvokeJSNoResult('texImage2D',[aTarget,aLevel,aInternalformat,aFormat,aType_,aSource]);
end;

procedure TJSWebGL2RenderingContextBase.texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSVideoFrame);
begin
  InvokeJSNoResult('texImage2D',[aTarget,aLevel,aInternalformat,aFormat,aType_,aSource]);
end;

procedure TJSWebGL2RenderingContextBase.texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aPixels: IJSArrayBufferView);
begin
  InvokeJSNoResult('texSubImage2D',[aTarget,aLevel,aXoffset,aYoffset,aWidth,aHeight,aFormat,aType_,aPixels]);
end;

procedure TJSWebGL2RenderingContextBase.texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSHTMLCanvasElement);
begin
  InvokeJSNoResult('texSubImage2D',[aTarget,aLevel,aXoffset,aYoffset,aFormat,aType_,aSource]);
end;

procedure TJSWebGL2RenderingContextBase.texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSHTMLImageElement);
begin
  InvokeJSNoResult('texSubImage2D',[aTarget,aLevel,aXoffset,aYoffset,aFormat,aType_,aSource]);
end;

procedure TJSWebGL2RenderingContextBase.texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSHTMLVideoElement);
begin
  InvokeJSNoResult('texSubImage2D',[aTarget,aLevel,aXoffset,aYoffset,aFormat,aType_,aSource]);
end;

procedure TJSWebGL2RenderingContextBase.texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSImageBitmap);
begin
  InvokeJSNoResult('texSubImage2D',[aTarget,aLevel,aXoffset,aYoffset,aFormat,aType_,aSource]);
end;

procedure TJSWebGL2RenderingContextBase.texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSImageData);
begin
  InvokeJSNoResult('texSubImage2D',[aTarget,aLevel,aXoffset,aYoffset,aFormat,aType_,aSource]);
end;

procedure TJSWebGL2RenderingContextBase.texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSOffscreenCanvas);
begin
  InvokeJSNoResult('texSubImage2D',[aTarget,aLevel,aXoffset,aYoffset,aFormat,aType_,aSource]);
end;

procedure TJSWebGL2RenderingContextBase.texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSVideoFrame);
begin
  InvokeJSNoResult('texSubImage2D',[aTarget,aLevel,aXoffset,aYoffset,aFormat,aType_,aSource]);
end;

procedure TJSWebGL2RenderingContextBase.texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aPboOffset: TGLintptr);
begin
  InvokeJSNoResult('texImage2D',[aTarget,aLevel,aInternalformat,aWidth,aHeight,aBorder,aFormat,aType_,aPboOffset]);
end;

procedure TJSWebGL2RenderingContextBase.texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSHTMLCanvasElement);
begin
  InvokeJSNoResult('texImage2D',[aTarget,aLevel,aInternalformat,aWidth,aHeight,aBorder,aFormat,aType_,aSource]);
end;

procedure TJSWebGL2RenderingContextBase.texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSHTMLImageElement);
begin
  InvokeJSNoResult('texImage2D',[aTarget,aLevel,aInternalformat,aWidth,aHeight,aBorder,aFormat,aType_,aSource]);
end;

procedure TJSWebGL2RenderingContextBase.texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSHTMLVideoElement);
begin
  InvokeJSNoResult('texImage2D',[aTarget,aLevel,aInternalformat,aWidth,aHeight,aBorder,aFormat,aType_,aSource]);
end;

procedure TJSWebGL2RenderingContextBase.texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSImageBitmap);
begin
  InvokeJSNoResult('texImage2D',[aTarget,aLevel,aInternalformat,aWidth,aHeight,aBorder,aFormat,aType_,aSource]);
end;

procedure TJSWebGL2RenderingContextBase.texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSImageData);
begin
  InvokeJSNoResult('texImage2D',[aTarget,aLevel,aInternalformat,aWidth,aHeight,aBorder,aFormat,aType_,aSource]);
end;

procedure TJSWebGL2RenderingContextBase.texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSOffscreenCanvas);
begin
  InvokeJSNoResult('texImage2D',[aTarget,aLevel,aInternalformat,aWidth,aHeight,aBorder,aFormat,aType_,aSource]);
end;

procedure TJSWebGL2RenderingContextBase.texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSVideoFrame);
begin
  InvokeJSNoResult('texImage2D',[aTarget,aLevel,aInternalformat,aWidth,aHeight,aBorder,aFormat,aType_,aSource]);
end;

procedure TJSWebGL2RenderingContextBase.texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aSrcData: IJSArrayBufferView; aSrcOffset: TGLuint);
begin
  InvokeJSNoResult('texImage2D',[aTarget,aLevel,aInternalformat,aWidth,aHeight,aBorder,aFormat,aType_,aSrcData,aSrcOffset]);
end;

procedure TJSWebGL2RenderingContextBase.texImage3D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aPboOffset: TGLintptr);
begin
  InvokeJSNoResult('texImage3D',[aTarget,aLevel,aInternalformat,aWidth,aHeight,aDepth,aBorder,aFormat,aType_,aPboOffset]);
end;

procedure TJSWebGL2RenderingContextBase.texImage3D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSHTMLCanvasElement);
begin
  InvokeJSNoResult('texImage3D',[aTarget,aLevel,aInternalformat,aWidth,aHeight,aDepth,aBorder,aFormat,aType_,aSource]);
end;

procedure TJSWebGL2RenderingContextBase.texImage3D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSHTMLImageElement);
begin
  InvokeJSNoResult('texImage3D',[aTarget,aLevel,aInternalformat,aWidth,aHeight,aDepth,aBorder,aFormat,aType_,aSource]);
end;

procedure TJSWebGL2RenderingContextBase.texImage3D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSHTMLVideoElement);
begin
  InvokeJSNoResult('texImage3D',[aTarget,aLevel,aInternalformat,aWidth,aHeight,aDepth,aBorder,aFormat,aType_,aSource]);
end;

procedure TJSWebGL2RenderingContextBase.texImage3D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSImageBitmap);
begin
  InvokeJSNoResult('texImage3D',[aTarget,aLevel,aInternalformat,aWidth,aHeight,aDepth,aBorder,aFormat,aType_,aSource]);
end;

procedure TJSWebGL2RenderingContextBase.texImage3D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSImageData);
begin
  InvokeJSNoResult('texImage3D',[aTarget,aLevel,aInternalformat,aWidth,aHeight,aDepth,aBorder,aFormat,aType_,aSource]);
end;

procedure TJSWebGL2RenderingContextBase.texImage3D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSOffscreenCanvas);
begin
  InvokeJSNoResult('texImage3D',[aTarget,aLevel,aInternalformat,aWidth,aHeight,aDepth,aBorder,aFormat,aType_,aSource]);
end;

procedure TJSWebGL2RenderingContextBase.texImage3D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aSource: IJSVideoFrame);
begin
  InvokeJSNoResult('texImage3D',[aTarget,aLevel,aInternalformat,aWidth,aHeight,aDepth,aBorder,aFormat,aType_,aSource]);
end;

procedure TJSWebGL2RenderingContextBase.texImage3D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aSrcData: IJSArrayBufferView);
begin
  InvokeJSNoResult('texImage3D',[aTarget,aLevel,aInternalformat,aWidth,aHeight,aDepth,aBorder,aFormat,aType_,aSrcData]);
end;

procedure TJSWebGL2RenderingContextBase.texImage3D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aSrcData: IJSArrayBufferView; aSrcOffset: TGLuint);
begin
  InvokeJSNoResult('texImage3D',[aTarget,aLevel,aInternalformat,aWidth,aHeight,aDepth,aBorder,aFormat,aType_,aSrcData,aSrcOffset]);
end;

procedure TJSWebGL2RenderingContextBase.texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aPboOffset: TGLintptr);
begin
  InvokeJSNoResult('texSubImage2D',[aTarget,aLevel,aXoffset,aYoffset,aWidth,aHeight,aFormat,aType_,aPboOffset]);
end;

procedure TJSWebGL2RenderingContextBase.texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aSource: IJSHTMLCanvasElement);
begin
  InvokeJSNoResult('texSubImage2D',[aTarget,aLevel,aXoffset,aYoffset,aWidth,aHeight,aFormat,aType_,aSource]);
end;

procedure TJSWebGL2RenderingContextBase.texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aSource: IJSHTMLImageElement);
begin
  InvokeJSNoResult('texSubImage2D',[aTarget,aLevel,aXoffset,aYoffset,aWidth,aHeight,aFormat,aType_,aSource]);
end;

procedure TJSWebGL2RenderingContextBase.texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aSource: IJSHTMLVideoElement);
begin
  InvokeJSNoResult('texSubImage2D',[aTarget,aLevel,aXoffset,aYoffset,aWidth,aHeight,aFormat,aType_,aSource]);
end;

procedure TJSWebGL2RenderingContextBase.texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aSource: IJSImageBitmap);
begin
  InvokeJSNoResult('texSubImage2D',[aTarget,aLevel,aXoffset,aYoffset,aWidth,aHeight,aFormat,aType_,aSource]);
end;

procedure TJSWebGL2RenderingContextBase.texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aSource: IJSImageData);
begin
  InvokeJSNoResult('texSubImage2D',[aTarget,aLevel,aXoffset,aYoffset,aWidth,aHeight,aFormat,aType_,aSource]);
end;

procedure TJSWebGL2RenderingContextBase.texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aSource: IJSOffscreenCanvas);
begin
  InvokeJSNoResult('texSubImage2D',[aTarget,aLevel,aXoffset,aYoffset,aWidth,aHeight,aFormat,aType_,aSource]);
end;

procedure TJSWebGL2RenderingContextBase.texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aSource: IJSVideoFrame);
begin
  InvokeJSNoResult('texSubImage2D',[aTarget,aLevel,aXoffset,aYoffset,aWidth,aHeight,aFormat,aType_,aSource]);
end;

procedure TJSWebGL2RenderingContextBase.texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aSrcData: IJSArrayBufferView; aSrcOffset: TGLuint);
begin
  InvokeJSNoResult('texSubImage2D',[aTarget,aLevel,aXoffset,aYoffset,aWidth,aHeight,aFormat,aType_,aSrcData,aSrcOffset]);
end;

procedure TJSWebGL2RenderingContextBase.texSubImage3D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aZoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aPboOffset: TGLintptr);
begin
  InvokeJSNoResult('texSubImage3D',[aTarget,aLevel,aXoffset,aYoffset,aZoffset,aWidth,aHeight,aDepth,aFormat,aType_,aPboOffset]);
end;

procedure TJSWebGL2RenderingContextBase.texSubImage3D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aZoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aSource: IJSHTMLCanvasElement);
begin
  InvokeJSNoResult('texSubImage3D',[aTarget,aLevel,aXoffset,aYoffset,aZoffset,aWidth,aHeight,aDepth,aFormat,aType_,aSource]);
end;

procedure TJSWebGL2RenderingContextBase.texSubImage3D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aZoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aSource: IJSHTMLImageElement);
begin
  InvokeJSNoResult('texSubImage3D',[aTarget,aLevel,aXoffset,aYoffset,aZoffset,aWidth,aHeight,aDepth,aFormat,aType_,aSource]);
end;

procedure TJSWebGL2RenderingContextBase.texSubImage3D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aZoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aSource: IJSHTMLVideoElement);
begin
  InvokeJSNoResult('texSubImage3D',[aTarget,aLevel,aXoffset,aYoffset,aZoffset,aWidth,aHeight,aDepth,aFormat,aType_,aSource]);
end;

procedure TJSWebGL2RenderingContextBase.texSubImage3D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aZoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aSource: IJSImageBitmap);
begin
  InvokeJSNoResult('texSubImage3D',[aTarget,aLevel,aXoffset,aYoffset,aZoffset,aWidth,aHeight,aDepth,aFormat,aType_,aSource]);
end;

procedure TJSWebGL2RenderingContextBase.texSubImage3D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aZoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aSource: IJSImageData);
begin
  InvokeJSNoResult('texSubImage3D',[aTarget,aLevel,aXoffset,aYoffset,aZoffset,aWidth,aHeight,aDepth,aFormat,aType_,aSource]);
end;

procedure TJSWebGL2RenderingContextBase.texSubImage3D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aZoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aSource: IJSOffscreenCanvas);
begin
  InvokeJSNoResult('texSubImage3D',[aTarget,aLevel,aXoffset,aYoffset,aZoffset,aWidth,aHeight,aDepth,aFormat,aType_,aSource]);
end;

procedure TJSWebGL2RenderingContextBase.texSubImage3D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aZoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aSource: IJSVideoFrame);
begin
  InvokeJSNoResult('texSubImage3D',[aTarget,aLevel,aXoffset,aYoffset,aZoffset,aWidth,aHeight,aDepth,aFormat,aType_,aSource]);
end;

procedure TJSWebGL2RenderingContextBase.texSubImage3D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aZoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aSrcData: IJSArrayBufferView; aSrcOffset: TGLuint);
begin
  InvokeJSNoResult('texSubImage3D',[aTarget,aLevel,aXoffset,aYoffset,aZoffset,aWidth,aHeight,aDepth,aFormat,aType_,aSrcData,aSrcOffset]);
end;

procedure TJSWebGL2RenderingContextBase.texSubImage3D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aZoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aSrcData: IJSArrayBufferView);
begin
  InvokeJSNoResult('texSubImage3D',[aTarget,aLevel,aXoffset,aYoffset,aZoffset,aWidth,aHeight,aDepth,aFormat,aType_,aSrcData]);
end;

procedure TJSWebGL2RenderingContextBase.copyTexSubImage3D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aZoffset: TGLint; aX: TGLint; aY: TGLint; aWidth: TGLsizei; aHeight: TGLsizei);
begin
  InvokeJSNoResult('copyTexSubImage3D',[aTarget,aLevel,aXoffset,aYoffset,aZoffset,aX,aY,aWidth,aHeight]);
end;

procedure TJSWebGL2RenderingContextBase.compressedTexImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLenum; aWidth: TGLsizei; aHeight: TGLsizei; aBorder: TGLint; aImageSize: TGLsizei; aOffset: TGLintptr);
begin
  InvokeJSNoResult('compressedTexImage2D',[aTarget,aLevel,aInternalformat,aWidth,aHeight,aBorder,aImageSize,aOffset]);
end;

procedure TJSWebGL2RenderingContextBase.compressedTexImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLenum; aWidth: TGLsizei; aHeight: TGLsizei; aBorder: TGLint; aSrcData: IJSArrayBufferView; aSrcOffset: TGLuint; aSrcLengthOverride: TGLuint);
begin
  InvokeJSNoResult('compressedTexImage2D',[aTarget,aLevel,aInternalformat,aWidth,aHeight,aBorder,aSrcData,aSrcOffset,aSrcLengthOverride]);
end;

procedure TJSWebGL2RenderingContextBase.compressedTexImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLenum; aWidth: TGLsizei; aHeight: TGLsizei; aBorder: TGLint; aSrcData: IJSArrayBufferView);
begin
  InvokeJSNoResult('compressedTexImage2D',[aTarget,aLevel,aInternalformat,aWidth,aHeight,aBorder,aSrcData]);
end;

procedure TJSWebGL2RenderingContextBase.compressedTexImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLenum; aWidth: TGLsizei; aHeight: TGLsizei; aBorder: TGLint; aSrcData: IJSArrayBufferView; aSrcOffset: TGLuint);
begin
  InvokeJSNoResult('compressedTexImage2D',[aTarget,aLevel,aInternalformat,aWidth,aHeight,aBorder,aSrcData,aSrcOffset]);
end;

procedure TJSWebGL2RenderingContextBase.compressedTexImage3D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLenum; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aBorder: TGLint; aImageSize: TGLsizei; aOffset: TGLintptr);
begin
  InvokeJSNoResult('compressedTexImage3D',[aTarget,aLevel,aInternalformat,aWidth,aHeight,aDepth,aBorder,aImageSize,aOffset]);
end;

procedure TJSWebGL2RenderingContextBase.compressedTexImage3D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLenum; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aBorder: TGLint; aSrcData: IJSArrayBufferView; aSrcOffset: TGLuint; aSrcLengthOverride: TGLuint);
begin
  InvokeJSNoResult('compressedTexImage3D',[aTarget,aLevel,aInternalformat,aWidth,aHeight,aDepth,aBorder,aSrcData,aSrcOffset,aSrcLengthOverride]);
end;

procedure TJSWebGL2RenderingContextBase.compressedTexImage3D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLenum; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aBorder: TGLint; aSrcData: IJSArrayBufferView);
begin
  InvokeJSNoResult('compressedTexImage3D',[aTarget,aLevel,aInternalformat,aWidth,aHeight,aDepth,aBorder,aSrcData]);
end;

procedure TJSWebGL2RenderingContextBase.compressedTexImage3D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLenum; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aBorder: TGLint; aSrcData: IJSArrayBufferView; aSrcOffset: TGLuint);
begin
  InvokeJSNoResult('compressedTexImage3D',[aTarget,aLevel,aInternalformat,aWidth,aHeight,aDepth,aBorder,aSrcData,aSrcOffset]);
end;

procedure TJSWebGL2RenderingContextBase.compressedTexSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aFormat: TGLenum; aImageSize: TGLsizei; aOffset: TGLintptr);
begin
  InvokeJSNoResult('compressedTexSubImage2D',[aTarget,aLevel,aXoffset,aYoffset,aWidth,aHeight,aFormat,aImageSize,aOffset]);
end;

procedure TJSWebGL2RenderingContextBase.compressedTexSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aFormat: TGLenum; aSrcData: IJSArrayBufferView; aSrcOffset: TGLuint; aSrcLengthOverride: TGLuint);
begin
  InvokeJSNoResult('compressedTexSubImage2D',[aTarget,aLevel,aXoffset,aYoffset,aWidth,aHeight,aFormat,aSrcData,aSrcOffset,aSrcLengthOverride]);
end;

procedure TJSWebGL2RenderingContextBase.compressedTexSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aFormat: TGLenum; aSrcData: IJSArrayBufferView);
begin
  InvokeJSNoResult('compressedTexSubImage2D',[aTarget,aLevel,aXoffset,aYoffset,aWidth,aHeight,aFormat,aSrcData]);
end;

procedure TJSWebGL2RenderingContextBase.compressedTexSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aFormat: TGLenum; aSrcData: IJSArrayBufferView; aSrcOffset: TGLuint);
begin
  InvokeJSNoResult('compressedTexSubImage2D',[aTarget,aLevel,aXoffset,aYoffset,aWidth,aHeight,aFormat,aSrcData,aSrcOffset]);
end;

procedure TJSWebGL2RenderingContextBase.compressedTexSubImage3D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aZoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aFormat: TGLenum; aImageSize: TGLsizei; aOffset: TGLintptr);
begin
  InvokeJSNoResult('compressedTexSubImage3D',[aTarget,aLevel,aXoffset,aYoffset,aZoffset,aWidth,aHeight,aDepth,aFormat,aImageSize,aOffset]);
end;

procedure TJSWebGL2RenderingContextBase.compressedTexSubImage3D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aZoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aFormat: TGLenum; aSrcData: IJSArrayBufferView; aSrcOffset: TGLuint; aSrcLengthOverride: TGLuint);
begin
  InvokeJSNoResult('compressedTexSubImage3D',[aTarget,aLevel,aXoffset,aYoffset,aZoffset,aWidth,aHeight,aDepth,aFormat,aSrcData,aSrcOffset,aSrcLengthOverride]);
end;

procedure TJSWebGL2RenderingContextBase.compressedTexSubImage3D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aZoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aFormat: TGLenum; aSrcData: IJSArrayBufferView);
begin
  InvokeJSNoResult('compressedTexSubImage3D',[aTarget,aLevel,aXoffset,aYoffset,aZoffset,aWidth,aHeight,aDepth,aFormat,aSrcData]);
end;

procedure TJSWebGL2RenderingContextBase.compressedTexSubImage3D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aZoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aDepth: TGLsizei; aFormat: TGLenum; aSrcData: IJSArrayBufferView; aSrcOffset: TGLuint);
begin
  InvokeJSNoResult('compressedTexSubImage3D',[aTarget,aLevel,aXoffset,aYoffset,aZoffset,aWidth,aHeight,aDepth,aFormat,aSrcData,aSrcOffset]);
end;

function TJSWebGL2RenderingContextBase.getFragDataLocation(aProgram_: IJSWebGLProgram; const aName: UnicodeString): TGLint;
begin
  Result:=InvokeJSLongIntResult('getFragDataLocation',[aProgram_,aName]);
end;

procedure TJSWebGL2RenderingContextBase.uniform1ui(aLocation: IJSWebGLUniformLocation; aV0: TGLuint);
begin
  InvokeJSNoResult('uniform1ui',[aLocation,aV0]);
end;

procedure TJSWebGL2RenderingContextBase.uniform2ui(aLocation: IJSWebGLUniformLocation; aV0: TGLuint; aV1: TGLuint);
begin
  InvokeJSNoResult('uniform2ui',[aLocation,aV0,aV1]);
end;

procedure TJSWebGL2RenderingContextBase.uniform3ui(aLocation: IJSWebGLUniformLocation; aV0: TGLuint; aV1: TGLuint; aV2: TGLuint);
begin
  InvokeJSNoResult('uniform3ui',[aLocation,aV0,aV1,aV2]);
end;

procedure TJSWebGL2RenderingContextBase.uniform4ui(aLocation: IJSWebGLUniformLocation; aV0: TGLuint; aV1: TGLuint; aV2: TGLuint; aV3: TGLuint);
begin
  InvokeJSNoResult('uniform4ui',[aLocation,aV0,aV1,aV2,aV3]);
end;

procedure TJSWebGL2RenderingContextBase.uniform1fv(aLocation: IJSWebGLUniformLocation; aData: IJSFloat32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint);
begin
  InvokeJSNoResult('uniform1fv',[aLocation,aData,aSrcOffset,aSrcLength]);
end;

procedure TJSWebGL2RenderingContextBase.uniform1fv(aLocation: IJSWebGLUniformLocation; const aData: TGLfloatDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint);
begin
  InvokeJSNoResult('uniform1fv',[aLocation,aData,aSrcOffset,aSrcLength]);
end;

procedure TJSWebGL2RenderingContextBase.uniform1fv(aLocation: IJSWebGLUniformLocation; const aData: TGLfloatDynArray);
begin
  InvokeJSNoResult('uniform1fv',[aLocation,aData]);
end;

procedure TJSWebGL2RenderingContextBase.uniform1fv(aLocation: IJSWebGLUniformLocation; aData: IJSFloat32Array);
begin
  InvokeJSNoResult('uniform1fv',[aLocation,aData]);
end;

procedure TJSWebGL2RenderingContextBase.uniform1fv(aLocation: IJSWebGLUniformLocation; const aData: TGLfloatDynArray; aSrcOffset: TGLuint);
begin
  InvokeJSNoResult('uniform1fv',[aLocation,aData,aSrcOffset]);
end;

procedure TJSWebGL2RenderingContextBase.uniform1fv(aLocation: IJSWebGLUniformLocation; aData: IJSFloat32Array; aSrcOffset: TGLuint);
begin
  InvokeJSNoResult('uniform1fv',[aLocation,aData,aSrcOffset]);
end;

procedure TJSWebGL2RenderingContextBase.uniform2fv(aLocation: IJSWebGLUniformLocation; aData: IJSFloat32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint);
begin
  InvokeJSNoResult('uniform2fv',[aLocation,aData,aSrcOffset,aSrcLength]);
end;

procedure TJSWebGL2RenderingContextBase.uniform2fv(aLocation: IJSWebGLUniformLocation; const aData: TGLfloatDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint);
begin
  InvokeJSNoResult('uniform2fv',[aLocation,aData,aSrcOffset,aSrcLength]);
end;

procedure TJSWebGL2RenderingContextBase.uniform2fv(aLocation: IJSWebGLUniformLocation; const aData: TGLfloatDynArray);
begin
  InvokeJSNoResult('uniform2fv',[aLocation,aData]);
end;

procedure TJSWebGL2RenderingContextBase.uniform2fv(aLocation: IJSWebGLUniformLocation; aData: IJSFloat32Array);
begin
  InvokeJSNoResult('uniform2fv',[aLocation,aData]);
end;

procedure TJSWebGL2RenderingContextBase.uniform2fv(aLocation: IJSWebGLUniformLocation; const aData: TGLfloatDynArray; aSrcOffset: TGLuint);
begin
  InvokeJSNoResult('uniform2fv',[aLocation,aData,aSrcOffset]);
end;

procedure TJSWebGL2RenderingContextBase.uniform2fv(aLocation: IJSWebGLUniformLocation; aData: IJSFloat32Array; aSrcOffset: TGLuint);
begin
  InvokeJSNoResult('uniform2fv',[aLocation,aData,aSrcOffset]);
end;

procedure TJSWebGL2RenderingContextBase.uniform3fv(aLocation: IJSWebGLUniformLocation; aData: IJSFloat32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint);
begin
  InvokeJSNoResult('uniform3fv',[aLocation,aData,aSrcOffset,aSrcLength]);
end;

procedure TJSWebGL2RenderingContextBase.uniform3fv(aLocation: IJSWebGLUniformLocation; const aData: TGLfloatDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint);
begin
  InvokeJSNoResult('uniform3fv',[aLocation,aData,aSrcOffset,aSrcLength]);
end;

procedure TJSWebGL2RenderingContextBase.uniform3fv(aLocation: IJSWebGLUniformLocation; const aData: TGLfloatDynArray);
begin
  InvokeJSNoResult('uniform3fv',[aLocation,aData]);
end;

procedure TJSWebGL2RenderingContextBase.uniform3fv(aLocation: IJSWebGLUniformLocation; aData: IJSFloat32Array);
begin
  InvokeJSNoResult('uniform3fv',[aLocation,aData]);
end;

procedure TJSWebGL2RenderingContextBase.uniform3fv(aLocation: IJSWebGLUniformLocation; const aData: TGLfloatDynArray; aSrcOffset: TGLuint);
begin
  InvokeJSNoResult('uniform3fv',[aLocation,aData,aSrcOffset]);
end;

procedure TJSWebGL2RenderingContextBase.uniform3fv(aLocation: IJSWebGLUniformLocation; aData: IJSFloat32Array; aSrcOffset: TGLuint);
begin
  InvokeJSNoResult('uniform3fv',[aLocation,aData,aSrcOffset]);
end;

procedure TJSWebGL2RenderingContextBase.uniform4fv(aLocation: IJSWebGLUniformLocation; aData: IJSFloat32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint);
begin
  InvokeJSNoResult('uniform4fv',[aLocation,aData,aSrcOffset,aSrcLength]);
end;

procedure TJSWebGL2RenderingContextBase.uniform4fv(aLocation: IJSWebGLUniformLocation; const aData: TGLfloatDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint);
begin
  InvokeJSNoResult('uniform4fv',[aLocation,aData,aSrcOffset,aSrcLength]);
end;

procedure TJSWebGL2RenderingContextBase.uniform4fv(aLocation: IJSWebGLUniformLocation; const aData: TGLfloatDynArray);
begin
  InvokeJSNoResult('uniform4fv',[aLocation,aData]);
end;

procedure TJSWebGL2RenderingContextBase.uniform4fv(aLocation: IJSWebGLUniformLocation; aData: IJSFloat32Array);
begin
  InvokeJSNoResult('uniform4fv',[aLocation,aData]);
end;

procedure TJSWebGL2RenderingContextBase.uniform4fv(aLocation: IJSWebGLUniformLocation; const aData: TGLfloatDynArray; aSrcOffset: TGLuint);
begin
  InvokeJSNoResult('uniform4fv',[aLocation,aData,aSrcOffset]);
end;

procedure TJSWebGL2RenderingContextBase.uniform4fv(aLocation: IJSWebGLUniformLocation; aData: IJSFloat32Array; aSrcOffset: TGLuint);
begin
  InvokeJSNoResult('uniform4fv',[aLocation,aData,aSrcOffset]);
end;

procedure TJSWebGL2RenderingContextBase.uniform1iv(aLocation: IJSWebGLUniformLocation; aData: IJSInt32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint);
begin
  InvokeJSNoResult('uniform1iv',[aLocation,aData,aSrcOffset,aSrcLength]);
end;

procedure TJSWebGL2RenderingContextBase.uniform1iv(aLocation: IJSWebGLUniformLocation; const aData: TGLintDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint);
begin
  InvokeJSNoResult('uniform1iv',[aLocation,aData,aSrcOffset,aSrcLength]);
end;

procedure TJSWebGL2RenderingContextBase.uniform1iv(aLocation: IJSWebGLUniformLocation; const aData: TGLintDynArray);
begin
  InvokeJSNoResult('uniform1iv',[aLocation,aData]);
end;

procedure TJSWebGL2RenderingContextBase.uniform1iv(aLocation: IJSWebGLUniformLocation; aData: IJSInt32Array);
begin
  InvokeJSNoResult('uniform1iv',[aLocation,aData]);
end;

procedure TJSWebGL2RenderingContextBase.uniform1iv(aLocation: IJSWebGLUniformLocation; const aData: TGLintDynArray; aSrcOffset: TGLuint);
begin
  InvokeJSNoResult('uniform1iv',[aLocation,aData,aSrcOffset]);
end;

procedure TJSWebGL2RenderingContextBase.uniform1iv(aLocation: IJSWebGLUniformLocation; aData: IJSInt32Array; aSrcOffset: TGLuint);
begin
  InvokeJSNoResult('uniform1iv',[aLocation,aData,aSrcOffset]);
end;

procedure TJSWebGL2RenderingContextBase.uniform2iv(aLocation: IJSWebGLUniformLocation; aData: IJSInt32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint);
begin
  InvokeJSNoResult('uniform2iv',[aLocation,aData,aSrcOffset,aSrcLength]);
end;

procedure TJSWebGL2RenderingContextBase.uniform2iv(aLocation: IJSWebGLUniformLocation; const aData: TGLintDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint);
begin
  InvokeJSNoResult('uniform2iv',[aLocation,aData,aSrcOffset,aSrcLength]);
end;

procedure TJSWebGL2RenderingContextBase.uniform2iv(aLocation: IJSWebGLUniformLocation; const aData: TGLintDynArray);
begin
  InvokeJSNoResult('uniform2iv',[aLocation,aData]);
end;

procedure TJSWebGL2RenderingContextBase.uniform2iv(aLocation: IJSWebGLUniformLocation; aData: IJSInt32Array);
begin
  InvokeJSNoResult('uniform2iv',[aLocation,aData]);
end;

procedure TJSWebGL2RenderingContextBase.uniform2iv(aLocation: IJSWebGLUniformLocation; const aData: TGLintDynArray; aSrcOffset: TGLuint);
begin
  InvokeJSNoResult('uniform2iv',[aLocation,aData,aSrcOffset]);
end;

procedure TJSWebGL2RenderingContextBase.uniform2iv(aLocation: IJSWebGLUniformLocation; aData: IJSInt32Array; aSrcOffset: TGLuint);
begin
  InvokeJSNoResult('uniform2iv',[aLocation,aData,aSrcOffset]);
end;

procedure TJSWebGL2RenderingContextBase.uniform3iv(aLocation: IJSWebGLUniformLocation; aData: IJSInt32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint);
begin
  InvokeJSNoResult('uniform3iv',[aLocation,aData,aSrcOffset,aSrcLength]);
end;

procedure TJSWebGL2RenderingContextBase.uniform3iv(aLocation: IJSWebGLUniformLocation; const aData: TGLintDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint);
begin
  InvokeJSNoResult('uniform3iv',[aLocation,aData,aSrcOffset,aSrcLength]);
end;

procedure TJSWebGL2RenderingContextBase.uniform3iv(aLocation: IJSWebGLUniformLocation; const aData: TGLintDynArray);
begin
  InvokeJSNoResult('uniform3iv',[aLocation,aData]);
end;

procedure TJSWebGL2RenderingContextBase.uniform3iv(aLocation: IJSWebGLUniformLocation; aData: IJSInt32Array);
begin
  InvokeJSNoResult('uniform3iv',[aLocation,aData]);
end;

procedure TJSWebGL2RenderingContextBase.uniform3iv(aLocation: IJSWebGLUniformLocation; const aData: TGLintDynArray; aSrcOffset: TGLuint);
begin
  InvokeJSNoResult('uniform3iv',[aLocation,aData,aSrcOffset]);
end;

procedure TJSWebGL2RenderingContextBase.uniform3iv(aLocation: IJSWebGLUniformLocation; aData: IJSInt32Array; aSrcOffset: TGLuint);
begin
  InvokeJSNoResult('uniform3iv',[aLocation,aData,aSrcOffset]);
end;

procedure TJSWebGL2RenderingContextBase.uniform4iv(aLocation: IJSWebGLUniformLocation; aData: IJSInt32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint);
begin
  InvokeJSNoResult('uniform4iv',[aLocation,aData,aSrcOffset,aSrcLength]);
end;

procedure TJSWebGL2RenderingContextBase.uniform4iv(aLocation: IJSWebGLUniformLocation; const aData: TGLintDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint);
begin
  InvokeJSNoResult('uniform4iv',[aLocation,aData,aSrcOffset,aSrcLength]);
end;

procedure TJSWebGL2RenderingContextBase.uniform4iv(aLocation: IJSWebGLUniformLocation; const aData: TGLintDynArray);
begin
  InvokeJSNoResult('uniform4iv',[aLocation,aData]);
end;

procedure TJSWebGL2RenderingContextBase.uniform4iv(aLocation: IJSWebGLUniformLocation; aData: IJSInt32Array);
begin
  InvokeJSNoResult('uniform4iv',[aLocation,aData]);
end;

procedure TJSWebGL2RenderingContextBase.uniform4iv(aLocation: IJSWebGLUniformLocation; const aData: TGLintDynArray; aSrcOffset: TGLuint);
begin
  InvokeJSNoResult('uniform4iv',[aLocation,aData,aSrcOffset]);
end;

procedure TJSWebGL2RenderingContextBase.uniform4iv(aLocation: IJSWebGLUniformLocation; aData: IJSInt32Array; aSrcOffset: TGLuint);
begin
  InvokeJSNoResult('uniform4iv',[aLocation,aData,aSrcOffset]);
end;

procedure TJSWebGL2RenderingContextBase.uniform1uiv(aLocation: IJSWebGLUniformLocation; const aData: TGLuintDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint);
begin
  InvokeJSNoResult('uniform1uiv',[aLocation,aData,aSrcOffset,aSrcLength]);
end;

procedure TJSWebGL2RenderingContextBase.uniform1uiv(aLocation: IJSWebGLUniformLocation; aData: IJSUint32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint);
begin
  InvokeJSNoResult('uniform1uiv',[aLocation,aData,aSrcOffset,aSrcLength]);
end;

procedure TJSWebGL2RenderingContextBase.uniform1uiv(aLocation: IJSWebGLUniformLocation; aData: IJSUint32Array);
begin
  InvokeJSNoResult('uniform1uiv',[aLocation,aData]);
end;

procedure TJSWebGL2RenderingContextBase.uniform1uiv(aLocation: IJSWebGLUniformLocation; const aData: TGLuintDynArray);
begin
  InvokeJSNoResult('uniform1uiv',[aLocation,aData]);
end;

procedure TJSWebGL2RenderingContextBase.uniform1uiv(aLocation: IJSWebGLUniformLocation; aData: IJSUint32Array; aSrcOffset: TGLuint);
begin
  InvokeJSNoResult('uniform1uiv',[aLocation,aData,aSrcOffset]);
end;

procedure TJSWebGL2RenderingContextBase.uniform1uiv(aLocation: IJSWebGLUniformLocation; const aData: TGLuintDynArray; aSrcOffset: TGLuint);
begin
  InvokeJSNoResult('uniform1uiv',[aLocation,aData,aSrcOffset]);
end;

procedure TJSWebGL2RenderingContextBase.uniform2uiv(aLocation: IJSWebGLUniformLocation; const aData: TGLuintDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint);
begin
  InvokeJSNoResult('uniform2uiv',[aLocation,aData,aSrcOffset,aSrcLength]);
end;

procedure TJSWebGL2RenderingContextBase.uniform2uiv(aLocation: IJSWebGLUniformLocation; aData: IJSUint32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint);
begin
  InvokeJSNoResult('uniform2uiv',[aLocation,aData,aSrcOffset,aSrcLength]);
end;

procedure TJSWebGL2RenderingContextBase.uniform2uiv(aLocation: IJSWebGLUniformLocation; aData: IJSUint32Array);
begin
  InvokeJSNoResult('uniform2uiv',[aLocation,aData]);
end;

procedure TJSWebGL2RenderingContextBase.uniform2uiv(aLocation: IJSWebGLUniformLocation; const aData: TGLuintDynArray);
begin
  InvokeJSNoResult('uniform2uiv',[aLocation,aData]);
end;

procedure TJSWebGL2RenderingContextBase.uniform2uiv(aLocation: IJSWebGLUniformLocation; aData: IJSUint32Array; aSrcOffset: TGLuint);
begin
  InvokeJSNoResult('uniform2uiv',[aLocation,aData,aSrcOffset]);
end;

procedure TJSWebGL2RenderingContextBase.uniform2uiv(aLocation: IJSWebGLUniformLocation; const aData: TGLuintDynArray; aSrcOffset: TGLuint);
begin
  InvokeJSNoResult('uniform2uiv',[aLocation,aData,aSrcOffset]);
end;

procedure TJSWebGL2RenderingContextBase.uniform3uiv(aLocation: IJSWebGLUniformLocation; const aData: TGLuintDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint);
begin
  InvokeJSNoResult('uniform3uiv',[aLocation,aData,aSrcOffset,aSrcLength]);
end;

procedure TJSWebGL2RenderingContextBase.uniform3uiv(aLocation: IJSWebGLUniformLocation; aData: IJSUint32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint);
begin
  InvokeJSNoResult('uniform3uiv',[aLocation,aData,aSrcOffset,aSrcLength]);
end;

procedure TJSWebGL2RenderingContextBase.uniform3uiv(aLocation: IJSWebGLUniformLocation; aData: IJSUint32Array);
begin
  InvokeJSNoResult('uniform3uiv',[aLocation,aData]);
end;

procedure TJSWebGL2RenderingContextBase.uniform3uiv(aLocation: IJSWebGLUniformLocation; const aData: TGLuintDynArray);
begin
  InvokeJSNoResult('uniform3uiv',[aLocation,aData]);
end;

procedure TJSWebGL2RenderingContextBase.uniform3uiv(aLocation: IJSWebGLUniformLocation; aData: IJSUint32Array; aSrcOffset: TGLuint);
begin
  InvokeJSNoResult('uniform3uiv',[aLocation,aData,aSrcOffset]);
end;

procedure TJSWebGL2RenderingContextBase.uniform3uiv(aLocation: IJSWebGLUniformLocation; const aData: TGLuintDynArray; aSrcOffset: TGLuint);
begin
  InvokeJSNoResult('uniform3uiv',[aLocation,aData,aSrcOffset]);
end;

procedure TJSWebGL2RenderingContextBase.uniform4uiv(aLocation: IJSWebGLUniformLocation; const aData: TGLuintDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint);
begin
  InvokeJSNoResult('uniform4uiv',[aLocation,aData,aSrcOffset,aSrcLength]);
end;

procedure TJSWebGL2RenderingContextBase.uniform4uiv(aLocation: IJSWebGLUniformLocation; aData: IJSUint32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint);
begin
  InvokeJSNoResult('uniform4uiv',[aLocation,aData,aSrcOffset,aSrcLength]);
end;

procedure TJSWebGL2RenderingContextBase.uniform4uiv(aLocation: IJSWebGLUniformLocation; aData: IJSUint32Array);
begin
  InvokeJSNoResult('uniform4uiv',[aLocation,aData]);
end;

procedure TJSWebGL2RenderingContextBase.uniform4uiv(aLocation: IJSWebGLUniformLocation; const aData: TGLuintDynArray);
begin
  InvokeJSNoResult('uniform4uiv',[aLocation,aData]);
end;

procedure TJSWebGL2RenderingContextBase.uniform4uiv(aLocation: IJSWebGLUniformLocation; aData: IJSUint32Array; aSrcOffset: TGLuint);
begin
  InvokeJSNoResult('uniform4uiv',[aLocation,aData,aSrcOffset]);
end;

procedure TJSWebGL2RenderingContextBase.uniform4uiv(aLocation: IJSWebGLUniformLocation; const aData: TGLuintDynArray; aSrcOffset: TGLuint);
begin
  InvokeJSNoResult('uniform4uiv',[aLocation,aData,aSrcOffset]);
end;

procedure TJSWebGL2RenderingContextBase.uniformMatrix2fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint);
begin
  InvokeJSNoResult('uniformMatrix2fv',[aLocation,aTranspose,aData,aSrcOffset,aSrcLength]);
end;

procedure TJSWebGL2RenderingContextBase.uniformMatrix2fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint);
begin
  InvokeJSNoResult('uniformMatrix2fv',[aLocation,aTranspose,aData,aSrcOffset,aSrcLength]);
end;

procedure TJSWebGL2RenderingContextBase.uniformMatrix2fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray);
begin
  InvokeJSNoResult('uniformMatrix2fv',[aLocation,aTranspose,aData]);
end;

procedure TJSWebGL2RenderingContextBase.uniformMatrix2fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array);
begin
  InvokeJSNoResult('uniformMatrix2fv',[aLocation,aTranspose,aData]);
end;

procedure TJSWebGL2RenderingContextBase.uniformMatrix2fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray; aSrcOffset: TGLuint);
begin
  InvokeJSNoResult('uniformMatrix2fv',[aLocation,aTranspose,aData,aSrcOffset]);
end;

procedure TJSWebGL2RenderingContextBase.uniformMatrix2fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array; aSrcOffset: TGLuint);
begin
  InvokeJSNoResult('uniformMatrix2fv',[aLocation,aTranspose,aData,aSrcOffset]);
end;

procedure TJSWebGL2RenderingContextBase.uniformMatrix3x2fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint);
begin
  InvokeJSNoResult('uniformMatrix3x2fv',[aLocation,aTranspose,aData,aSrcOffset,aSrcLength]);
end;

procedure TJSWebGL2RenderingContextBase.uniformMatrix3x2fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint);
begin
  InvokeJSNoResult('uniformMatrix3x2fv',[aLocation,aTranspose,aData,aSrcOffset,aSrcLength]);
end;

procedure TJSWebGL2RenderingContextBase.uniformMatrix3x2fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray);
begin
  InvokeJSNoResult('uniformMatrix3x2fv',[aLocation,aTranspose,aData]);
end;

procedure TJSWebGL2RenderingContextBase.uniformMatrix3x2fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array);
begin
  InvokeJSNoResult('uniformMatrix3x2fv',[aLocation,aTranspose,aData]);
end;

procedure TJSWebGL2RenderingContextBase.uniformMatrix3x2fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray; aSrcOffset: TGLuint);
begin
  InvokeJSNoResult('uniformMatrix3x2fv',[aLocation,aTranspose,aData,aSrcOffset]);
end;

procedure TJSWebGL2RenderingContextBase.uniformMatrix3x2fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array; aSrcOffset: TGLuint);
begin
  InvokeJSNoResult('uniformMatrix3x2fv',[aLocation,aTranspose,aData,aSrcOffset]);
end;

procedure TJSWebGL2RenderingContextBase.uniformMatrix4x2fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint);
begin
  InvokeJSNoResult('uniformMatrix4x2fv',[aLocation,aTranspose,aData,aSrcOffset,aSrcLength]);
end;

procedure TJSWebGL2RenderingContextBase.uniformMatrix4x2fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint);
begin
  InvokeJSNoResult('uniformMatrix4x2fv',[aLocation,aTranspose,aData,aSrcOffset,aSrcLength]);
end;

procedure TJSWebGL2RenderingContextBase.uniformMatrix4x2fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray);
begin
  InvokeJSNoResult('uniformMatrix4x2fv',[aLocation,aTranspose,aData]);
end;

procedure TJSWebGL2RenderingContextBase.uniformMatrix4x2fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array);
begin
  InvokeJSNoResult('uniformMatrix4x2fv',[aLocation,aTranspose,aData]);
end;

procedure TJSWebGL2RenderingContextBase.uniformMatrix4x2fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray; aSrcOffset: TGLuint);
begin
  InvokeJSNoResult('uniformMatrix4x2fv',[aLocation,aTranspose,aData,aSrcOffset]);
end;

procedure TJSWebGL2RenderingContextBase.uniformMatrix4x2fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array; aSrcOffset: TGLuint);
begin
  InvokeJSNoResult('uniformMatrix4x2fv',[aLocation,aTranspose,aData,aSrcOffset]);
end;

procedure TJSWebGL2RenderingContextBase.uniformMatrix2x3fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint);
begin
  InvokeJSNoResult('uniformMatrix2x3fv',[aLocation,aTranspose,aData,aSrcOffset,aSrcLength]);
end;

procedure TJSWebGL2RenderingContextBase.uniformMatrix2x3fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint);
begin
  InvokeJSNoResult('uniformMatrix2x3fv',[aLocation,aTranspose,aData,aSrcOffset,aSrcLength]);
end;

procedure TJSWebGL2RenderingContextBase.uniformMatrix2x3fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray);
begin
  InvokeJSNoResult('uniformMatrix2x3fv',[aLocation,aTranspose,aData]);
end;

procedure TJSWebGL2RenderingContextBase.uniformMatrix2x3fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array);
begin
  InvokeJSNoResult('uniformMatrix2x3fv',[aLocation,aTranspose,aData]);
end;

procedure TJSWebGL2RenderingContextBase.uniformMatrix2x3fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray; aSrcOffset: TGLuint);
begin
  InvokeJSNoResult('uniformMatrix2x3fv',[aLocation,aTranspose,aData,aSrcOffset]);
end;

procedure TJSWebGL2RenderingContextBase.uniformMatrix2x3fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array; aSrcOffset: TGLuint);
begin
  InvokeJSNoResult('uniformMatrix2x3fv',[aLocation,aTranspose,aData,aSrcOffset]);
end;

procedure TJSWebGL2RenderingContextBase.uniformMatrix3fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint);
begin
  InvokeJSNoResult('uniformMatrix3fv',[aLocation,aTranspose,aData,aSrcOffset,aSrcLength]);
end;

procedure TJSWebGL2RenderingContextBase.uniformMatrix3fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint);
begin
  InvokeJSNoResult('uniformMatrix3fv',[aLocation,aTranspose,aData,aSrcOffset,aSrcLength]);
end;

procedure TJSWebGL2RenderingContextBase.uniformMatrix3fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray);
begin
  InvokeJSNoResult('uniformMatrix3fv',[aLocation,aTranspose,aData]);
end;

procedure TJSWebGL2RenderingContextBase.uniformMatrix3fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array);
begin
  InvokeJSNoResult('uniformMatrix3fv',[aLocation,aTranspose,aData]);
end;

procedure TJSWebGL2RenderingContextBase.uniformMatrix3fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray; aSrcOffset: TGLuint);
begin
  InvokeJSNoResult('uniformMatrix3fv',[aLocation,aTranspose,aData,aSrcOffset]);
end;

procedure TJSWebGL2RenderingContextBase.uniformMatrix3fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array; aSrcOffset: TGLuint);
begin
  InvokeJSNoResult('uniformMatrix3fv',[aLocation,aTranspose,aData,aSrcOffset]);
end;

procedure TJSWebGL2RenderingContextBase.uniformMatrix4x3fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint);
begin
  InvokeJSNoResult('uniformMatrix4x3fv',[aLocation,aTranspose,aData,aSrcOffset,aSrcLength]);
end;

procedure TJSWebGL2RenderingContextBase.uniformMatrix4x3fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint);
begin
  InvokeJSNoResult('uniformMatrix4x3fv',[aLocation,aTranspose,aData,aSrcOffset,aSrcLength]);
end;

procedure TJSWebGL2RenderingContextBase.uniformMatrix4x3fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray);
begin
  InvokeJSNoResult('uniformMatrix4x3fv',[aLocation,aTranspose,aData]);
end;

procedure TJSWebGL2RenderingContextBase.uniformMatrix4x3fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array);
begin
  InvokeJSNoResult('uniformMatrix4x3fv',[aLocation,aTranspose,aData]);
end;

procedure TJSWebGL2RenderingContextBase.uniformMatrix4x3fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray; aSrcOffset: TGLuint);
begin
  InvokeJSNoResult('uniformMatrix4x3fv',[aLocation,aTranspose,aData,aSrcOffset]);
end;

procedure TJSWebGL2RenderingContextBase.uniformMatrix4x3fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array; aSrcOffset: TGLuint);
begin
  InvokeJSNoResult('uniformMatrix4x3fv',[aLocation,aTranspose,aData,aSrcOffset]);
end;

procedure TJSWebGL2RenderingContextBase.uniformMatrix2x4fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint);
begin
  InvokeJSNoResult('uniformMatrix2x4fv',[aLocation,aTranspose,aData,aSrcOffset,aSrcLength]);
end;

procedure TJSWebGL2RenderingContextBase.uniformMatrix2x4fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint);
begin
  InvokeJSNoResult('uniformMatrix2x4fv',[aLocation,aTranspose,aData,aSrcOffset,aSrcLength]);
end;

procedure TJSWebGL2RenderingContextBase.uniformMatrix2x4fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray);
begin
  InvokeJSNoResult('uniformMatrix2x4fv',[aLocation,aTranspose,aData]);
end;

procedure TJSWebGL2RenderingContextBase.uniformMatrix2x4fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array);
begin
  InvokeJSNoResult('uniformMatrix2x4fv',[aLocation,aTranspose,aData]);
end;

procedure TJSWebGL2RenderingContextBase.uniformMatrix2x4fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray; aSrcOffset: TGLuint);
begin
  InvokeJSNoResult('uniformMatrix2x4fv',[aLocation,aTranspose,aData,aSrcOffset]);
end;

procedure TJSWebGL2RenderingContextBase.uniformMatrix2x4fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array; aSrcOffset: TGLuint);
begin
  InvokeJSNoResult('uniformMatrix2x4fv',[aLocation,aTranspose,aData,aSrcOffset]);
end;

procedure TJSWebGL2RenderingContextBase.uniformMatrix3x4fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint);
begin
  InvokeJSNoResult('uniformMatrix3x4fv',[aLocation,aTranspose,aData,aSrcOffset,aSrcLength]);
end;

procedure TJSWebGL2RenderingContextBase.uniformMatrix3x4fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint);
begin
  InvokeJSNoResult('uniformMatrix3x4fv',[aLocation,aTranspose,aData,aSrcOffset,aSrcLength]);
end;

procedure TJSWebGL2RenderingContextBase.uniformMatrix3x4fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray);
begin
  InvokeJSNoResult('uniformMatrix3x4fv',[aLocation,aTranspose,aData]);
end;

procedure TJSWebGL2RenderingContextBase.uniformMatrix3x4fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array);
begin
  InvokeJSNoResult('uniformMatrix3x4fv',[aLocation,aTranspose,aData]);
end;

procedure TJSWebGL2RenderingContextBase.uniformMatrix3x4fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray; aSrcOffset: TGLuint);
begin
  InvokeJSNoResult('uniformMatrix3x4fv',[aLocation,aTranspose,aData,aSrcOffset]);
end;

procedure TJSWebGL2RenderingContextBase.uniformMatrix3x4fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array; aSrcOffset: TGLuint);
begin
  InvokeJSNoResult('uniformMatrix3x4fv',[aLocation,aTranspose,aData,aSrcOffset]);
end;

procedure TJSWebGL2RenderingContextBase.uniformMatrix4fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array; aSrcOffset: TGLuint; aSrcLength: TGLuint);
begin
  InvokeJSNoResult('uniformMatrix4fv',[aLocation,aTranspose,aData,aSrcOffset,aSrcLength]);
end;

procedure TJSWebGL2RenderingContextBase.uniformMatrix4fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray; aSrcOffset: TGLuint; aSrcLength: TGLuint);
begin
  InvokeJSNoResult('uniformMatrix4fv',[aLocation,aTranspose,aData,aSrcOffset,aSrcLength]);
end;

procedure TJSWebGL2RenderingContextBase.uniformMatrix4fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray);
begin
  InvokeJSNoResult('uniformMatrix4fv',[aLocation,aTranspose,aData]);
end;

procedure TJSWebGL2RenderingContextBase.uniformMatrix4fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array);
begin
  InvokeJSNoResult('uniformMatrix4fv',[aLocation,aTranspose,aData]);
end;

procedure TJSWebGL2RenderingContextBase.uniformMatrix4fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray; aSrcOffset: TGLuint);
begin
  InvokeJSNoResult('uniformMatrix4fv',[aLocation,aTranspose,aData,aSrcOffset]);
end;

procedure TJSWebGL2RenderingContextBase.uniformMatrix4fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array; aSrcOffset: TGLuint);
begin
  InvokeJSNoResult('uniformMatrix4fv',[aLocation,aTranspose,aData,aSrcOffset]);
end;

procedure TJSWebGL2RenderingContextBase.vertexAttribI4i(aIndex: TGLuint; aX: TGLint; aY: TGLint; aZ: TGLint; aW: TGLint);
begin
  InvokeJSNoResult('vertexAttribI4i',[aIndex,aX,aY,aZ,aW]);
end;

procedure TJSWebGL2RenderingContextBase.vertexAttribI4iv(aIndex: TGLuint; aValues: IJSInt32Array);
begin
  InvokeJSNoResult('vertexAttribI4iv',[aIndex,aValues]);
end;

procedure TJSWebGL2RenderingContextBase.vertexAttribI4iv(aIndex: TGLuint; const aValues: TGLintDynArray);
begin
  InvokeJSNoResult('vertexAttribI4iv',[aIndex,aValues]);
end;

procedure TJSWebGL2RenderingContextBase.vertexAttribI4ui(aIndex: TGLuint; aX: TGLuint; aY: TGLuint; aZ: TGLuint; aW: TGLuint);
begin
  InvokeJSNoResult('vertexAttribI4ui',[aIndex,aX,aY,aZ,aW]);
end;

procedure TJSWebGL2RenderingContextBase.vertexAttribI4uiv(aIndex: TGLuint; const aValues: TGLuintDynArray);
begin
  InvokeJSNoResult('vertexAttribI4uiv',[aIndex,aValues]);
end;

procedure TJSWebGL2RenderingContextBase.vertexAttribI4uiv(aIndex: TGLuint; aValues: IJSUint32Array);
begin
  InvokeJSNoResult('vertexAttribI4uiv',[aIndex,aValues]);
end;

procedure TJSWebGL2RenderingContextBase.vertexAttribIPointer(aIndex: TGLuint; aSize: TGLint; aType_: TGLenum; aStride: TGLsizei; aOffset: TGLintptr);
begin
  InvokeJSNoResult('vertexAttribIPointer',[aIndex,aSize,aType_,aStride,aOffset]);
end;

procedure TJSWebGL2RenderingContextBase.vertexAttribDivisor(aIndex: TGLuint; aDivisor: TGLuint);
begin
  InvokeJSNoResult('vertexAttribDivisor',[aIndex,aDivisor]);
end;

procedure TJSWebGL2RenderingContextBase.drawArraysInstanced(aMode: TGLenum; aFirst: TGLint; aCount: TGLsizei; aInstanceCount: TGLsizei);
begin
  InvokeJSNoResult('drawArraysInstanced',[aMode,aFirst,aCount,aInstanceCount]);
end;

procedure TJSWebGL2RenderingContextBase.drawElementsInstanced(aMode: TGLenum; aCount: TGLsizei; aType_: TGLenum; aOffset: TGLintptr; aInstanceCount: TGLsizei);
begin
  InvokeJSNoResult('drawElementsInstanced',[aMode,aCount,aType_,aOffset,aInstanceCount]);
end;

procedure TJSWebGL2RenderingContextBase.drawRangeElements(aMode: TGLenum; aStart: TGLuint; aEnd_: TGLuint; aCount: TGLsizei; aType_: TGLenum; aOffset: TGLintptr);
begin
  InvokeJSNoResult('drawRangeElements',[aMode,aStart,aEnd_,aCount,aType_,aOffset]);
end;

procedure TJSWebGL2RenderingContextBase.readPixels(aX: TGLint; aY: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aDstData: IJSArrayBufferView);
begin
  InvokeJSNoResult('readPixels',[aX,aY,aWidth,aHeight,aFormat,aType_,aDstData]);
end;

procedure TJSWebGL2RenderingContextBase.readPixels(aX: TGLint; aY: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aOffset: TGLintptr);
begin
  InvokeJSNoResult('readPixels',[aX,aY,aWidth,aHeight,aFormat,aType_,aOffset]);
end;

procedure TJSWebGL2RenderingContextBase.readPixels(aX: TGLint; aY: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aDstData: IJSArrayBufferView; aDstOffset: TGLuint);
begin
  InvokeJSNoResult('readPixels',[aX,aY,aWidth,aHeight,aFormat,aType_,aDstData,aDstOffset]);
end;

procedure TJSWebGL2RenderingContextBase.drawBuffers(const aBuffers: TGLenumDynArray);
begin
  InvokeJSNoResult('drawBuffers',[aBuffers]);
end;

procedure TJSWebGL2RenderingContextBase.clearBufferfv(aBuffer: TGLenum; aDrawbuffer: TGLint; aValues: IJSFloat32Array; aSrcOffset: TGLuint);
begin
  InvokeJSNoResult('clearBufferfv',[aBuffer,aDrawbuffer,aValues,aSrcOffset]);
end;

procedure TJSWebGL2RenderingContextBase.clearBufferfv(aBuffer: TGLenum; aDrawbuffer: TGLint; const aValues: TGLfloatDynArray; aSrcOffset: TGLuint);
begin
  InvokeJSNoResult('clearBufferfv',[aBuffer,aDrawbuffer,aValues,aSrcOffset]);
end;

procedure TJSWebGL2RenderingContextBase.clearBufferfv(aBuffer: TGLenum; aDrawbuffer: TGLint; const aValues: TGLfloatDynArray);
begin
  InvokeJSNoResult('clearBufferfv',[aBuffer,aDrawbuffer,aValues]);
end;

procedure TJSWebGL2RenderingContextBase.clearBufferfv(aBuffer: TGLenum; aDrawbuffer: TGLint; aValues: IJSFloat32Array);
begin
  InvokeJSNoResult('clearBufferfv',[aBuffer,aDrawbuffer,aValues]);
end;

procedure TJSWebGL2RenderingContextBase.clearBufferiv(aBuffer: TGLenum; aDrawbuffer: TGLint; aValues: IJSInt32Array; aSrcOffset: TGLuint);
begin
  InvokeJSNoResult('clearBufferiv',[aBuffer,aDrawbuffer,aValues,aSrcOffset]);
end;

procedure TJSWebGL2RenderingContextBase.clearBufferiv(aBuffer: TGLenum; aDrawbuffer: TGLint; const aValues: TGLintDynArray; aSrcOffset: TGLuint);
begin
  InvokeJSNoResult('clearBufferiv',[aBuffer,aDrawbuffer,aValues,aSrcOffset]);
end;

procedure TJSWebGL2RenderingContextBase.clearBufferiv(aBuffer: TGLenum; aDrawbuffer: TGLint; const aValues: TGLintDynArray);
begin
  InvokeJSNoResult('clearBufferiv',[aBuffer,aDrawbuffer,aValues]);
end;

procedure TJSWebGL2RenderingContextBase.clearBufferiv(aBuffer: TGLenum; aDrawbuffer: TGLint; aValues: IJSInt32Array);
begin
  InvokeJSNoResult('clearBufferiv',[aBuffer,aDrawbuffer,aValues]);
end;

procedure TJSWebGL2RenderingContextBase.clearBufferuiv(aBuffer: TGLenum; aDrawbuffer: TGLint; const aValues: TGLuintDynArray; aSrcOffset: TGLuint);
begin
  InvokeJSNoResult('clearBufferuiv',[aBuffer,aDrawbuffer,aValues,aSrcOffset]);
end;

procedure TJSWebGL2RenderingContextBase.clearBufferuiv(aBuffer: TGLenum; aDrawbuffer: TGLint; aValues: IJSUint32Array; aSrcOffset: TGLuint);
begin
  InvokeJSNoResult('clearBufferuiv',[aBuffer,aDrawbuffer,aValues,aSrcOffset]);
end;

procedure TJSWebGL2RenderingContextBase.clearBufferuiv(aBuffer: TGLenum; aDrawbuffer: TGLint; aValues: IJSUint32Array);
begin
  InvokeJSNoResult('clearBufferuiv',[aBuffer,aDrawbuffer,aValues]);
end;

procedure TJSWebGL2RenderingContextBase.clearBufferuiv(aBuffer: TGLenum; aDrawbuffer: TGLint; const aValues: TGLuintDynArray);
begin
  InvokeJSNoResult('clearBufferuiv',[aBuffer,aDrawbuffer,aValues]);
end;

procedure TJSWebGL2RenderingContextBase.clearBufferfi(aBuffer: TGLenum; aDrawbuffer: TGLint; aDepth: TGLfloat; aStencil: TGLint);
begin
  InvokeJSNoResult('clearBufferfi',[aBuffer,aDrawbuffer,aDepth,aStencil]);
end;

function TJSWebGL2RenderingContextBase.createQuery: IJSWebGLQuery;
begin
  Result:=InvokeJSObjectResult('createQuery',[],TJSWebGLQuery) as IJSWebGLQuery;
end;

procedure TJSWebGL2RenderingContextBase.deleteQuery(aQuery: IJSWebGLQuery);
begin
  InvokeJSNoResult('deleteQuery',[aQuery]);
end;

function TJSWebGL2RenderingContextBase.isQuery(aQuery: IJSWebGLQuery): TGLboolean;
begin
  Result:=InvokeJSBooleanResult('isQuery',[aQuery]);
end;

procedure TJSWebGL2RenderingContextBase.beginQuery(aTarget: TGLenum; aQuery: IJSWebGLQuery);
begin
  InvokeJSNoResult('beginQuery',[aTarget,aQuery]);
end;

procedure TJSWebGL2RenderingContextBase.endQuery(aTarget: TGLenum);
begin
  InvokeJSNoResult('endQuery',[aTarget]);
end;

function TJSWebGL2RenderingContextBase.getQuery(aTarget: TGLenum; aPname: TGLenum): Variant;
begin
  Result:=InvokeJSVariantResult('getQuery',[aTarget,aPname]);
end;

function TJSWebGL2RenderingContextBase.getQueryParameter(aQuery: IJSWebGLQuery; aPname: TGLenum): Variant;
begin
  Result:=InvokeJSVariantResult('getQueryParameter',[aQuery,aPname]);
end;

function TJSWebGL2RenderingContextBase.createSampler: IJSWebGLSampler;
begin
  Result:=InvokeJSObjectResult('createSampler',[],TJSWebGLSampler) as IJSWebGLSampler;
end;

procedure TJSWebGL2RenderingContextBase.deleteSampler(aSampler: IJSWebGLSampler);
begin
  InvokeJSNoResult('deleteSampler',[aSampler]);
end;

function TJSWebGL2RenderingContextBase.isSampler(aSampler: IJSWebGLSampler): TGLboolean;
begin
  Result:=InvokeJSBooleanResult('isSampler',[aSampler]);
end;

procedure TJSWebGL2RenderingContextBase.bindSampler(aUnit_: TGLuint; aSampler: IJSWebGLSampler);
begin
  InvokeJSNoResult('bindSampler',[aUnit_,aSampler]);
end;

procedure TJSWebGL2RenderingContextBase.samplerParameteri(aSampler: IJSWebGLSampler; aPname: TGLenum; aParam: TGLint);
begin
  InvokeJSNoResult('samplerParameteri',[aSampler,aPname,aParam]);
end;

procedure TJSWebGL2RenderingContextBase.samplerParameterf(aSampler: IJSWebGLSampler; aPname: TGLenum; aParam: TGLfloat);
begin
  InvokeJSNoResult('samplerParameterf',[aSampler,aPname,aParam]);
end;

function TJSWebGL2RenderingContextBase.getSamplerParameter(aSampler: IJSWebGLSampler; aPname: TGLenum): Variant;
begin
  Result:=InvokeJSVariantResult('getSamplerParameter',[aSampler,aPname]);
end;

function TJSWebGL2RenderingContextBase.fenceSync(aCondition: TGLenum; aFlags: TGLbitfield): IJSWebGLSync;
begin
  Result:=InvokeJSObjectResult('fenceSync',[aCondition,aFlags],TJSWebGLSync) as IJSWebGLSync;
end;

function TJSWebGL2RenderingContextBase.isSync(aSync: IJSWebGLSync): TGLboolean;
begin
  Result:=InvokeJSBooleanResult('isSync',[aSync]);
end;

procedure TJSWebGL2RenderingContextBase.deleteSync(aSync: IJSWebGLSync);
begin
  InvokeJSNoResult('deleteSync',[aSync]);
end;

function TJSWebGL2RenderingContextBase.clientWaitSync(aSync: IJSWebGLSync; aFlags: TGLbitfield; aTimeout: TGLuint64): TGLenum;
begin
  Result:=InvokeJSLongIntResult('clientWaitSync',[aSync,aFlags,aTimeout]);
end;

procedure TJSWebGL2RenderingContextBase.waitSync(aSync: IJSWebGLSync; aFlags: TGLbitfield; aTimeout: TGLint64);
begin
  InvokeJSNoResult('waitSync',[aSync,aFlags,aTimeout]);
end;

function TJSWebGL2RenderingContextBase.getSyncParameter(aSync: IJSWebGLSync; aPname: TGLenum): Variant;
begin
  Result:=InvokeJSVariantResult('getSyncParameter',[aSync,aPname]);
end;

function TJSWebGL2RenderingContextBase.createTransformFeedback: IJSWebGLTransformFeedback;
begin
  Result:=InvokeJSObjectResult('createTransformFeedback',[],TJSWebGLTransformFeedback) as IJSWebGLTransformFeedback;
end;

procedure TJSWebGL2RenderingContextBase.deleteTransformFeedback(aTf: IJSWebGLTransformFeedback);
begin
  InvokeJSNoResult('deleteTransformFeedback',[aTf]);
end;

function TJSWebGL2RenderingContextBase.isTransformFeedback(aTf: IJSWebGLTransformFeedback): TGLboolean;
begin
  Result:=InvokeJSBooleanResult('isTransformFeedback',[aTf]);
end;

procedure TJSWebGL2RenderingContextBase.bindTransformFeedback(aTarget: TGLenum; aTf: IJSWebGLTransformFeedback);
begin
  InvokeJSNoResult('bindTransformFeedback',[aTarget,aTf]);
end;

procedure TJSWebGL2RenderingContextBase.beginTransformFeedback(aPrimitiveMode: TGLenum);
begin
  InvokeJSNoResult('beginTransformFeedback',[aPrimitiveMode]);
end;

procedure TJSWebGL2RenderingContextBase.endTransformFeedback;
begin
  InvokeJSNoResult('endTransformFeedback',[]);
end;

procedure TJSWebGL2RenderingContextBase.transformFeedbackVaryings(aProgram_: IJSWebGLProgram; const aVaryings: TUnicodeStringDynArray; aBufferMode: TGLenum);
begin
  InvokeJSNoResult('transformFeedbackVaryings',[aProgram_,aVaryings,aBufferMode]);
end;

function TJSWebGL2RenderingContextBase.getTransformFeedbackVarying(aProgram_: IJSWebGLProgram; aIndex: TGLuint): IJSWebGLActiveInfo;
begin
  Result:=InvokeJSObjectResult('getTransformFeedbackVarying',[aProgram_,aIndex],TJSWebGLActiveInfo) as IJSWebGLActiveInfo;
end;

procedure TJSWebGL2RenderingContextBase.pauseTransformFeedback;
begin
  InvokeJSNoResult('pauseTransformFeedback',[]);
end;

procedure TJSWebGL2RenderingContextBase.resumeTransformFeedback;
begin
  InvokeJSNoResult('resumeTransformFeedback',[]);
end;

procedure TJSWebGL2RenderingContextBase.bindBufferBase(aTarget: TGLenum; aIndex: TGLuint; aBuffer: IJSWebGLBuffer);
begin
  InvokeJSNoResult('bindBufferBase',[aTarget,aIndex,aBuffer]);
end;

procedure TJSWebGL2RenderingContextBase.bindBufferRange(aTarget: TGLenum; aIndex: TGLuint; aBuffer: IJSWebGLBuffer; aOffset: TGLintptr; aSize: TGLsizeiptr);
begin
  InvokeJSNoResult('bindBufferRange',[aTarget,aIndex,aBuffer,aOffset,aSize]);
end;

function TJSWebGL2RenderingContextBase.getIndexedParameter(aTarget: TGLenum; aIndex: TGLuint): Variant;
begin
  Result:=InvokeJSVariantResult('getIndexedParameter',[aTarget,aIndex]);
end;

function TJSWebGL2RenderingContextBase.getUniformIndices(aProgram_: IJSWebGLProgram; const aUniformNames: TUnicodeStringDynArray): TGLuintDynArray;
begin
  Result:=InvokeJSObjectResult('getUniformIndices',[aProgram_,aUniformNames],TJSArray) as TGLuintDynArray;
end;

function TJSWebGL2RenderingContextBase.getActiveUniforms(aProgram_: IJSWebGLProgram; const aUniformIndices: TGLuintDynArray; aPname: TGLenum): Variant;
begin
  Result:=InvokeJSVariantResult('getActiveUniforms',[aProgram_,aUniformIndices,aPname]);
end;

function TJSWebGL2RenderingContextBase.getUniformBlockIndex(aProgram_: IJSWebGLProgram; const aUniformBlockName: UnicodeString): TGLuint;
begin
  Result:=InvokeJSLongIntResult('getUniformBlockIndex',[aProgram_,aUniformBlockName]);
end;

function TJSWebGL2RenderingContextBase.getActiveUniformBlockParameter(aProgram_: IJSWebGLProgram; aUniformBlockIndex: TGLuint; aPname: TGLenum): Variant;
begin
  Result:=InvokeJSVariantResult('getActiveUniformBlockParameter',[aProgram_,aUniformBlockIndex,aPname]);
end;

function TJSWebGL2RenderingContextBase.getActiveUniformBlockName(aProgram_: IJSWebGLProgram; aUniformBlockIndex: TGLuint): UnicodeString;
begin
  Result:=InvokeJSUnicodeStringResult('getActiveUniformBlockName',[aProgram_,aUniformBlockIndex]);
end;

procedure TJSWebGL2RenderingContextBase.uniformBlockBinding(aProgram_: IJSWebGLProgram; aUniformBlockIndex: TGLuint; aUniformBlockBinding: TGLuint);
begin
  InvokeJSNoResult('uniformBlockBinding',[aProgram_,aUniformBlockIndex,aUniformBlockBinding]);
end;

function TJSWebGL2RenderingContextBase.createVertexArray: IJSWebGLVertexArrayObject;
begin
  Result:=InvokeJSObjectResult('createVertexArray',[],TJSWebGLVertexArrayObject) as IJSWebGLVertexArrayObject;
end;

procedure TJSWebGL2RenderingContextBase.deleteVertexArray(aVertexArray: IJSWebGLVertexArrayObject);
begin
  InvokeJSNoResult('deleteVertexArray',[aVertexArray]);
end;

function TJSWebGL2RenderingContextBase.isVertexArray(aVertexArray: IJSWebGLVertexArrayObject): TGLboolean;
begin
  Result:=InvokeJSBooleanResult('isVertexArray',[aVertexArray]);
end;

procedure TJSWebGL2RenderingContextBase.bindVertexArray(aArray_: IJSWebGLVertexArrayObject);
begin
  InvokeJSNoResult('bindVertexArray',[aArray_]);
end;

class function TJSWebGL2RenderingContextBase.JSClassName: UnicodeString;
begin
  Result:='WebGL2RenderingContextBase';
end;

class function TJSWebGL2RenderingContextBase.Cast(const Intf: IJSObject): IJSWebGL2RenderingContextBase;
begin
  Result:=TJSWebGL2RenderingContextBase.JOBCast(Intf);
end;

class function TJSEXT_color_buffer_float.JSClassName: UnicodeString;
begin
  Result:='EXT_color_buffer_float';
end;

class function TJSEXT_color_buffer_float.Cast(const Intf: IJSObject): IJSEXT_color_buffer_float;
begin
  Result:=TJSEXT_color_buffer_float.JOBCast(Intf);
end;

procedure TJSOVR_multiview2.framebufferTextureMultiviewOVR(aTarget: TGLenum; aAttachment: TGLenum; aTexture: IJSWebGLTexture; aLevel: TGLint; aBaseViewIndex: TGLint; aNumViews: TGLsizei);
begin
  InvokeJSNoResult('framebufferTextureMultiviewOVR',[aTarget,aAttachment,aTexture,aLevel,aBaseViewIndex,aNumViews]);
end;

class function TJSOVR_multiview2.JSClassName: UnicodeString;
begin
  Result:='OVR_multiview2';
end;

class function TJSOVR_multiview2.Cast(const Intf: IJSObject): IJSOVR_multiview2;
begin
  Result:=TJSOVR_multiview2.JOBCast(Intf);
end;

function TJSWebGLContextEvent._GetstatusMessage: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('statusMessage');
end;

constructor TJSWebGLContextEvent.Create(const aType_: UnicodeString; const aEventInit: IJSWebGLContextEventInit);
begin
  JOBCreate([aType_,aEventInit]);
end;

constructor TJSWebGLContextEvent.Create(const aType_: UnicodeString);
begin
  JOBCreate([aType_]);
end;

class function TJSWebGLContextEvent.JSClassName: UnicodeString;
begin
  Result:='WebGLContextEvent';
end;

class function TJSWebGLContextEvent.Cast(const Intf: IJSObject): IJSWebGLContextEvent;
begin
  Result:=TJSWebGLContextEvent.JOBCast(Intf);
end;

function TJSWebGLContextEventInit._GetstatusMessage: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('statusMessage');
end;

procedure TJSWebGLContextEventInit._SetstatusMessage(const aValue : UnicodeString);
begin
  WriteJSPropertyUnicodeString('statusMessage',aValue);
end;

constructor TJSWebGLContextEventInit.create(const aDict : TJSWebGLContextEventInitRec); overload;
begin
  Self.statusMessage:=aDict.statusMessage;
end;

class function TJSWebGLContextEventInit.JSClassName: UnicodeString;
begin
  Result:='Object';
end;

class function TJSWebGLContextEventInit.Cast(const Intf: IJSObject): IJSWebGLContextEventInit;
begin
  Result:=TJSWebGLContextEventInit.JOBCast(Intf);
end;

function TJSWebGLContextAttributes._Getalpha: TGLboolean;
begin
  Result:=ReadJSPropertyBoolean('alpha');
end;

function TJSWebGLContextAttributes._Getdepth: TGLboolean;
begin
  Result:=ReadJSPropertyBoolean('depth');
end;

function TJSWebGLContextAttributes._Getstencil: TGLboolean;
begin
  Result:=ReadJSPropertyBoolean('stencil');
end;

function TJSWebGLContextAttributes._Getantialias: TGLboolean;
begin
  Result:=ReadJSPropertyBoolean('antialias');
end;

function TJSWebGLContextAttributes._GetpremultipliedAlpha: TGLboolean;
begin
  Result:=ReadJSPropertyBoolean('premultipliedAlpha');
end;

function TJSWebGLContextAttributes._GetpreserveDrawingBuffer: TGLboolean;
begin
  Result:=ReadJSPropertyBoolean('preserveDrawingBuffer');
end;

function TJSWebGLContextAttributes._GetfailIfMajorPerformanceCaveat: TGLboolean;
begin
  Result:=ReadJSPropertyBoolean('failIfMajorPerformanceCaveat');
end;

function TJSWebGLContextAttributes._GetpowerPreference: TWebGLPowerPreference;
begin
  Result:=ReadJSPropertyUnicodeString('powerPreference');
end;

function TJSWebGLContextAttributes._GetforceSoftwareRendering: TGLboolean;
begin
  Result:=ReadJSPropertyBoolean('forceSoftwareRendering');
end;

function TJSWebGLContextAttributes._GetxrCompatible: Boolean;
begin
  Result:=ReadJSPropertyBoolean('xrCompatible');
end;

procedure TJSWebGLContextAttributes._Setalpha(const aValue : TGLboolean);
begin
  WriteJSPropertyBoolean('alpha',aValue);
end;

procedure TJSWebGLContextAttributes._Setdepth(const aValue : TGLboolean);
begin
  WriteJSPropertyBoolean('depth',aValue);
end;

procedure TJSWebGLContextAttributes._Setstencil(const aValue : TGLboolean);
begin
  WriteJSPropertyBoolean('stencil',aValue);
end;

procedure TJSWebGLContextAttributes._Setantialias(const aValue : TGLboolean);
begin
  WriteJSPropertyBoolean('antialias',aValue);
end;

procedure TJSWebGLContextAttributes._SetpremultipliedAlpha(const aValue : TGLboolean);
begin
  WriteJSPropertyBoolean('premultipliedAlpha',aValue);
end;

procedure TJSWebGLContextAttributes._SetpreserveDrawingBuffer(const aValue : TGLboolean);
begin
  WriteJSPropertyBoolean('preserveDrawingBuffer',aValue);
end;

procedure TJSWebGLContextAttributes._SetfailIfMajorPerformanceCaveat(const aValue : TGLboolean);
begin
  WriteJSPropertyBoolean('failIfMajorPerformanceCaveat',aValue);
end;

procedure TJSWebGLContextAttributes._SetpowerPreference(const aValue : TWebGLPowerPreference);
begin
  WriteJSPropertyUnicodeString('powerPreference',aValue);
end;

procedure TJSWebGLContextAttributes._SetforceSoftwareRendering(const aValue : TGLboolean);
begin
  WriteJSPropertyBoolean('forceSoftwareRendering',aValue);
end;

procedure TJSWebGLContextAttributes._SetxrCompatible(const aValue : Boolean);
begin
  WriteJSPropertyBoolean('xrCompatible',aValue);
end;

constructor TJSWebGLContextAttributes.create(const aDict : TJSWebGLContextAttributesRec); overload;
begin
  Self.alpha:=aDict.alpha;
  Self.depth:=aDict.depth;
  Self.stencil:=aDict.stencil;
  Self.antialias:=aDict.antialias;
  Self.premultipliedAlpha:=aDict.premultipliedAlpha;
  Self.preserveDrawingBuffer:=aDict.preserveDrawingBuffer;
  Self.failIfMajorPerformanceCaveat:=aDict.failIfMajorPerformanceCaveat;
  Self.powerPreference:=aDict.powerPreference;
  Self.forceSoftwareRendering:=aDict.forceSoftwareRendering;
end;

class function TJSWebGLContextAttributes.JSClassName: UnicodeString;
begin
  Result:='Object';
end;

class function TJSWebGLContextAttributes.Cast(const Intf: IJSObject): IJSWebGLContextAttributes;
begin
  Result:=TJSWebGLContextAttributes.JOBCast(Intf);
end;

class function TJSWebGLBuffer.JSClassName: UnicodeString;
begin
  Result:='WebGLBuffer';
end;

class function TJSWebGLBuffer.Cast(const Intf: IJSObject): IJSWebGLBuffer;
begin
  Result:=TJSWebGLBuffer.JOBCast(Intf);
end;

class function TJSWebGLFramebuffer.JSClassName: UnicodeString;
begin
  Result:='WebGLFramebuffer';
end;

class function TJSWebGLFramebuffer.Cast(const Intf: IJSObject): IJSWebGLFramebuffer;
begin
  Result:=TJSWebGLFramebuffer.JOBCast(Intf);
end;

class function TJSWebGLProgram.JSClassName: UnicodeString;
begin
  Result:='WebGLProgram';
end;

class function TJSWebGLProgram.Cast(const Intf: IJSObject): IJSWebGLProgram;
begin
  Result:=TJSWebGLProgram.JOBCast(Intf);
end;

class function TJSWebGLRenderbuffer.JSClassName: UnicodeString;
begin
  Result:='WebGLRenderbuffer';
end;

class function TJSWebGLRenderbuffer.Cast(const Intf: IJSObject): IJSWebGLRenderbuffer;
begin
  Result:=TJSWebGLRenderbuffer.JOBCast(Intf);
end;

class function TJSWebGLShader.JSClassName: UnicodeString;
begin
  Result:='WebGLShader';
end;

class function TJSWebGLShader.Cast(const Intf: IJSObject): IJSWebGLShader;
begin
  Result:=TJSWebGLShader.JOBCast(Intf);
end;

class function TJSWebGLTexture.JSClassName: UnicodeString;
begin
  Result:='WebGLTexture';
end;

class function TJSWebGLTexture.Cast(const Intf: IJSObject): IJSWebGLTexture;
begin
  Result:=TJSWebGLTexture.JOBCast(Intf);
end;

class function TJSWebGLUniformLocation.JSClassName: UnicodeString;
begin
  Result:='WebGLUniformLocation';
end;

class function TJSWebGLUniformLocation.Cast(const Intf: IJSObject): IJSWebGLUniformLocation;
begin
  Result:=TJSWebGLUniformLocation.JOBCast(Intf);
end;

class function TJSWebGLVertexArrayObject.JSClassName: UnicodeString;
begin
  Result:='WebGLVertexArrayObject';
end;

class function TJSWebGLVertexArrayObject.Cast(const Intf: IJSObject): IJSWebGLVertexArrayObject;
begin
  Result:=TJSWebGLVertexArrayObject.JOBCast(Intf);
end;

function TJSWebGLActiveInfo._Getsize: TGLint;
begin
  Result:=ReadJSPropertyLongInt('size');
end;

function TJSWebGLActiveInfo._Gettype_: TGLenum;
begin
  Result:=ReadJSPropertyInt64('type');
end;

function TJSWebGLActiveInfo._Getname: UnicodeString;
begin
  Result:=ReadJSPropertyUnicodeString('name');
end;

class function TJSWebGLActiveInfo.JSClassName: UnicodeString;
begin
  Result:='WebGLActiveInfo';
end;

class function TJSWebGLActiveInfo.Cast(const Intf: IJSObject): IJSWebGLActiveInfo;
begin
  Result:=TJSWebGLActiveInfo.JOBCast(Intf);
end;

function TJSWebGLShaderPrecisionFormat._GetrangeMin: TGLint;
begin
  Result:=ReadJSPropertyLongInt('rangeMin');
end;

function TJSWebGLShaderPrecisionFormat._GetrangeMax: TGLint;
begin
  Result:=ReadJSPropertyLongInt('rangeMax');
end;

function TJSWebGLShaderPrecisionFormat._Getprecision: TGLint;
begin
  Result:=ReadJSPropertyLongInt('precision');
end;

class function TJSWebGLShaderPrecisionFormat.JSClassName: UnicodeString;
begin
  Result:='WebGLShaderPrecisionFormat';
end;

class function TJSWebGLShaderPrecisionFormat.Cast(const Intf: IJSObject): IJSWebGLShaderPrecisionFormat;
begin
  Result:=TJSWebGLShaderPrecisionFormat.JOBCast(Intf);
end;

function TJSWebGLRenderingContextBase._Getcanvas: TCanvasSource;
begin
  Result:=ReadJSPropertyVariant('canvas');
end;

function TJSWebGLRenderingContextBase._GetdrawingBufferWidth: TGLsizei;
begin
  Result:=ReadJSPropertyLongInt('drawingBufferWidth');
end;

function TJSWebGLRenderingContextBase._GetdrawingBufferHeight: TGLsizei;
begin
  Result:=ReadJSPropertyLongInt('drawingBufferHeight');
end;

function TJSWebGLRenderingContextBase._GetdrawingBufferColorSpace: TPredefinedColorSpace;
begin
  Result:=ReadJSPropertyUnicodeString('drawingBufferColorSpace');
end;

function TJSWebGLRenderingContextBase._GetunpackColorSpace: TPredefinedColorSpace;
begin
  Result:=ReadJSPropertyUnicodeString('unpackColorSpace');
end;

procedure TJSWebGLRenderingContextBase._SetdrawingBufferColorSpace(const aValue : TPredefinedColorSpace);
begin
  WriteJSPropertyUnicodeString('drawingBufferColorSpace',aValue);
end;

procedure TJSWebGLRenderingContextBase._SetunpackColorSpace(const aValue : TPredefinedColorSpace);
begin
  WriteJSPropertyUnicodeString('unpackColorSpace',aValue);
end;

function TJSWebGLRenderingContextBase.getContextAttributes: IJSWebGLContextAttributes;
begin
  Result:=InvokeJSObjectResult('getContextAttributes',[],TJSWebGLContextAttributes) as IJSWebGLContextAttributes;
end;

function TJSWebGLRenderingContextBase.isContextLost: Boolean;
begin
  Result:=InvokeJSBooleanResult('isContextLost',[]);
end;

function TJSWebGLRenderingContextBase.getSupportedExtensions: TUnicodeStringDynArray;
begin
  Result:=InvokeJSObjectResult('getSupportedExtensions',[],TJSArray) as TUnicodeStringDynArray;
end;

function TJSWebGLRenderingContextBase.getExtension(const aName: UnicodeString): IJSObject;
begin
  Result:=InvokeJSObjectResult('getExtension',[aName],TJSObject) as IJSObject;
end;

procedure TJSWebGLRenderingContextBase.activeTexture(aTexture: TGLenum);
begin
  InvokeJSNoResult('activeTexture',[aTexture]);
end;

procedure TJSWebGLRenderingContextBase.attachShader(aProgram_: IJSWebGLProgram; aShader: IJSWebGLShader);
begin
  InvokeJSNoResult('attachShader',[aProgram_,aShader]);
end;

procedure TJSWebGLRenderingContextBase.bindAttribLocation(aProgram_: IJSWebGLProgram; aIndex: TGLuint; const aName: UnicodeString);
begin
  InvokeJSNoResult('bindAttribLocation',[aProgram_,aIndex,aName]);
end;

procedure TJSWebGLRenderingContextBase.bindBuffer(aTarget: TGLenum; aBuffer: IJSWebGLBuffer);
begin
  InvokeJSNoResult('bindBuffer',[aTarget,aBuffer]);
end;

procedure TJSWebGLRenderingContextBase.bindFramebuffer(aTarget: TGLenum; aFramebuffer: IJSWebGLFramebuffer);
begin
  InvokeJSNoResult('bindFramebuffer',[aTarget,aFramebuffer]);
end;

procedure TJSWebGLRenderingContextBase.bindRenderbuffer(aTarget: TGLenum; aRenderbuffer: IJSWebGLRenderbuffer);
begin
  InvokeJSNoResult('bindRenderbuffer',[aTarget,aRenderbuffer]);
end;

procedure TJSWebGLRenderingContextBase.bindTexture(aTarget: TGLenum; aTexture: IJSWebGLTexture);
begin
  InvokeJSNoResult('bindTexture',[aTarget,aTexture]);
end;

procedure TJSWebGLRenderingContextBase.blendColor(aRed: TGLfloat; aGreen: TGLfloat; aBlue: TGLfloat; aAlpha: TGLfloat);
begin
  InvokeJSNoResult('blendColor',[aRed,aGreen,aBlue,aAlpha]);
end;

procedure TJSWebGLRenderingContextBase.blendEquation(aMode: TGLenum);
begin
  InvokeJSNoResult('blendEquation',[aMode]);
end;

procedure TJSWebGLRenderingContextBase.blendEquationSeparate(aModeRGB: TGLenum; aModeAlpha: TGLenum);
begin
  InvokeJSNoResult('blendEquationSeparate',[aModeRGB,aModeAlpha]);
end;

procedure TJSWebGLRenderingContextBase.blendFunc(aSfactor: TGLenum; aDfactor: TGLenum);
begin
  InvokeJSNoResult('blendFunc',[aSfactor,aDfactor]);
end;

procedure TJSWebGLRenderingContextBase.blendFuncSeparate(aSrcRGB: TGLenum; aDstRGB: TGLenum; aSrcAlpha: TGLenum; aDstAlpha: TGLenum);
begin
  InvokeJSNoResult('blendFuncSeparate',[aSrcRGB,aDstRGB,aSrcAlpha,aDstAlpha]);
end;

function TJSWebGLRenderingContextBase.checkFramebufferStatus(aTarget: TGLenum): TGLenum;
begin
  Result:=InvokeJSLongIntResult('checkFramebufferStatus',[aTarget]);
end;

procedure TJSWebGLRenderingContextBase.clear(aMask: TGLbitfield);
begin
  InvokeJSNoResult('clear',[aMask]);
end;

procedure TJSWebGLRenderingContextBase.clearColor(aRed: TGLfloat; aGreen: TGLfloat; aBlue: TGLfloat; aAlpha: TGLfloat);
begin
  InvokeJSNoResult('clearColor',[aRed,aGreen,aBlue,aAlpha]);
end;

procedure TJSWebGLRenderingContextBase.clearDepth(aDepth: TGLclampf);
begin
  InvokeJSNoResult('clearDepth',[aDepth]);
end;

procedure TJSWebGLRenderingContextBase.clearStencil(aS_: TGLint);
begin
  InvokeJSNoResult('clearStencil',[aS_]);
end;

procedure TJSWebGLRenderingContextBase.colorMask(aRed: TGLboolean; aGreen: TGLboolean; aBlue: TGLboolean; aAlpha: TGLboolean);
begin
  InvokeJSNoResult('colorMask',[aRed,aGreen,aBlue,aAlpha]);
end;

procedure TJSWebGLRenderingContextBase.compileShader(aShader: IJSWebGLShader);
begin
  InvokeJSNoResult('compileShader',[aShader]);
end;

procedure TJSWebGLRenderingContextBase.copyTexImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLenum; aX: TGLint; aY: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aBorder: TGLint);
begin
  InvokeJSNoResult('copyTexImage2D',[aTarget,aLevel,aInternalformat,aX,aY,aWidth,aHeight,aBorder]);
end;

procedure TJSWebGLRenderingContextBase.copyTexSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aX: TGLint; aY: TGLint; aWidth: TGLsizei; aHeight: TGLsizei);
begin
  InvokeJSNoResult('copyTexSubImage2D',[aTarget,aLevel,aXoffset,aYoffset,aX,aY,aWidth,aHeight]);
end;

function TJSWebGLRenderingContextBase.createBuffer: IJSWebGLBuffer;
begin
  Result:=InvokeJSObjectResult('createBuffer',[],TJSWebGLBuffer) as IJSWebGLBuffer;
end;

function TJSWebGLRenderingContextBase.createFramebuffer: IJSWebGLFramebuffer;
begin
  Result:=InvokeJSObjectResult('createFramebuffer',[],TJSWebGLFramebuffer) as IJSWebGLFramebuffer;
end;

function TJSWebGLRenderingContextBase.createProgram: IJSWebGLProgram;
begin
  Result:=InvokeJSObjectResult('createProgram',[],TJSWebGLProgram) as IJSWebGLProgram;
end;

function TJSWebGLRenderingContextBase.createRenderbuffer: IJSWebGLRenderbuffer;
begin
  Result:=InvokeJSObjectResult('createRenderbuffer',[],TJSWebGLRenderbuffer) as IJSWebGLRenderbuffer;
end;

function TJSWebGLRenderingContextBase.createShader(aType_: TGLenum): IJSWebGLShader;
begin
  Result:=InvokeJSObjectResult('createShader',[aType_],TJSWebGLShader) as IJSWebGLShader;
end;

function TJSWebGLRenderingContextBase.createTexture: IJSWebGLTexture;
begin
  Result:=InvokeJSObjectResult('createTexture',[],TJSWebGLTexture) as IJSWebGLTexture;
end;

procedure TJSWebGLRenderingContextBase.cullFace(aMode: TGLenum);
begin
  InvokeJSNoResult('cullFace',[aMode]);
end;

procedure TJSWebGLRenderingContextBase.deleteBuffer(aBuffer: IJSWebGLBuffer);
begin
  InvokeJSNoResult('deleteBuffer',[aBuffer]);
end;

procedure TJSWebGLRenderingContextBase.deleteFramebuffer(aFramebuffer: IJSWebGLFramebuffer);
begin
  InvokeJSNoResult('deleteFramebuffer',[aFramebuffer]);
end;

procedure TJSWebGLRenderingContextBase.deleteProgram(aProgram_: IJSWebGLProgram);
begin
  InvokeJSNoResult('deleteProgram',[aProgram_]);
end;

procedure TJSWebGLRenderingContextBase.deleteRenderbuffer(aRenderbuffer: IJSWebGLRenderbuffer);
begin
  InvokeJSNoResult('deleteRenderbuffer',[aRenderbuffer]);
end;

procedure TJSWebGLRenderingContextBase.deleteShader(aShader: IJSWebGLShader);
begin
  InvokeJSNoResult('deleteShader',[aShader]);
end;

procedure TJSWebGLRenderingContextBase.deleteTexture(aTexture: IJSWebGLTexture);
begin
  InvokeJSNoResult('deleteTexture',[aTexture]);
end;

procedure TJSWebGLRenderingContextBase.depthFunc(aFunc: TGLenum);
begin
  InvokeJSNoResult('depthFunc',[aFunc]);
end;

procedure TJSWebGLRenderingContextBase.depthMask(aFlag: TGLboolean);
begin
  InvokeJSNoResult('depthMask',[aFlag]);
end;

procedure TJSWebGLRenderingContextBase.depthRange(aZNear: TGLclampf; aZFar: TGLclampf);
begin
  InvokeJSNoResult('depthRange',[aZNear,aZFar]);
end;

procedure TJSWebGLRenderingContextBase.detachShader(aProgram_: IJSWebGLProgram; aShader: IJSWebGLShader);
begin
  InvokeJSNoResult('detachShader',[aProgram_,aShader]);
end;

procedure TJSWebGLRenderingContextBase.disable(aCap: TGLenum);
begin
  InvokeJSNoResult('disable',[aCap]);
end;

procedure TJSWebGLRenderingContextBase.disableVertexAttribArray(aIndex: TGLuint);
begin
  InvokeJSNoResult('disableVertexAttribArray',[aIndex]);
end;

procedure TJSWebGLRenderingContextBase.drawArrays(aMode: TGLenum; aFirst: TGLint; aCount: TGLsizei);
begin
  InvokeJSNoResult('drawArrays',[aMode,aFirst,aCount]);
end;

procedure TJSWebGLRenderingContextBase.drawElements(aMode: TGLenum; aCount: TGLsizei; aType_: TGLenum; aOffset: TGLintptr);
begin
  InvokeJSNoResult('drawElements',[aMode,aCount,aType_,aOffset]);
end;

procedure TJSWebGLRenderingContextBase.enable(aCap: TGLenum);
begin
  InvokeJSNoResult('enable',[aCap]);
end;

procedure TJSWebGLRenderingContextBase.enableVertexAttribArray(aIndex: TGLuint);
begin
  InvokeJSNoResult('enableVertexAttribArray',[aIndex]);
end;

procedure TJSWebGLRenderingContextBase.finish;
begin
  InvokeJSNoResult('finish',[]);
end;

procedure TJSWebGLRenderingContextBase.flush;
begin
  InvokeJSNoResult('flush',[]);
end;

procedure TJSWebGLRenderingContextBase.framebufferRenderbuffer(aTarget: TGLenum; aAttachment: TGLenum; aRenderbuffertarget: TGLenum; aRenderbuffer: IJSWebGLRenderbuffer);
begin
  InvokeJSNoResult('framebufferRenderbuffer',[aTarget,aAttachment,aRenderbuffertarget,aRenderbuffer]);
end;

procedure TJSWebGLRenderingContextBase.framebufferTexture2D(aTarget: TGLenum; aAttachment: TGLenum; aTextarget: TGLenum; aTexture: IJSWebGLTexture; aLevel: TGLint);
begin
  InvokeJSNoResult('framebufferTexture2D',[aTarget,aAttachment,aTextarget,aTexture,aLevel]);
end;

procedure TJSWebGLRenderingContextBase.frontFace(aMode: TGLenum);
begin
  InvokeJSNoResult('frontFace',[aMode]);
end;

procedure TJSWebGLRenderingContextBase.generateMipmap(aTarget: TGLenum);
begin
  InvokeJSNoResult('generateMipmap',[aTarget]);
end;

function TJSWebGLRenderingContextBase.getActiveAttrib(aProgram_: IJSWebGLProgram; aIndex: TGLuint): IJSWebGLActiveInfo;
begin
  Result:=InvokeJSObjectResult('getActiveAttrib',[aProgram_,aIndex],TJSWebGLActiveInfo) as IJSWebGLActiveInfo;
end;

function TJSWebGLRenderingContextBase.getActiveUniform(aProgram_: IJSWebGLProgram; aIndex: TGLuint): IJSWebGLActiveInfo;
begin
  Result:=InvokeJSObjectResult('getActiveUniform',[aProgram_,aIndex],TJSWebGLActiveInfo) as IJSWebGLActiveInfo;
end;

function TJSWebGLRenderingContextBase.getAttachedShaders(aProgram_: IJSWebGLProgram): TJSWebGLShaderDynArray;
begin
  Result:=InvokeJSObjectResult('getAttachedShaders',[aProgram_],TJSArray) as TJSWebGLShaderDynArray;
end;

function TJSWebGLRenderingContextBase.getAttribLocation(aProgram_: IJSWebGLProgram; const aName: UnicodeString): TGLint;
begin
  Result:=InvokeJSLongIntResult('getAttribLocation',[aProgram_,aName]);
end;

function TJSWebGLRenderingContextBase.getBufferParameter(aTarget: TGLenum; aPname: TGLenum): Variant;
begin
  Result:=InvokeJSVariantResult('getBufferParameter',[aTarget,aPname]);
end;

function TJSWebGLRenderingContextBase.getParameter(aPname: TGLenum): Variant;
begin
  Result:=InvokeJSVariantResult('getParameter',[aPname]);
end;

function TJSWebGLRenderingContextBase.getError: TGLenum;
begin
  Result:=InvokeJSLongIntResult('getError',[]);
end;

function TJSWebGLRenderingContextBase.getFramebufferAttachmentParameter(aTarget: TGLenum; aAttachment: TGLenum; aPname: TGLenum): Variant;
begin
  Result:=InvokeJSVariantResult('getFramebufferAttachmentParameter',[aTarget,aAttachment,aPname]);
end;

function TJSWebGLRenderingContextBase.getProgramParameter(aProgram_: IJSWebGLProgram; aPname: TGLenum): Variant;
begin
  Result:=InvokeJSVariantResult('getProgramParameter',[aProgram_,aPname]);
end;

function TJSWebGLRenderingContextBase.getProgramInfoLog(aProgram_: IJSWebGLProgram): UnicodeString;
begin
  Result:=InvokeJSUnicodeStringResult('getProgramInfoLog',[aProgram_]);
end;

function TJSWebGLRenderingContextBase.getRenderbufferParameter(aTarget: TGLenum; aPname: TGLenum): Variant;
begin
  Result:=InvokeJSVariantResult('getRenderbufferParameter',[aTarget,aPname]);
end;

function TJSWebGLRenderingContextBase.getShaderParameter(aShader: IJSWebGLShader; aPname: TGLenum): Variant;
begin
  Result:=InvokeJSVariantResult('getShaderParameter',[aShader,aPname]);
end;

function TJSWebGLRenderingContextBase.getShaderPrecisionFormat(aShadertype: TGLenum; aPrecisiontype: TGLenum): IJSWebGLShaderPrecisionFormat;
begin
  Result:=InvokeJSObjectResult('getShaderPrecisionFormat',[aShadertype,aPrecisiontype],TJSWebGLShaderPrecisionFormat) as IJSWebGLShaderPrecisionFormat;
end;

function TJSWebGLRenderingContextBase.getShaderInfoLog(aShader: IJSWebGLShader): UnicodeString;
begin
  Result:=InvokeJSUnicodeStringResult('getShaderInfoLog',[aShader]);
end;

function TJSWebGLRenderingContextBase.getShaderSource(aShader: IJSWebGLShader): UnicodeString;
begin
  Result:=InvokeJSUnicodeStringResult('getShaderSource',[aShader]);
end;

function TJSWebGLRenderingContextBase.getTexParameter(aTarget: TGLenum; aPname: TGLenum): Variant;
begin
  Result:=InvokeJSVariantResult('getTexParameter',[aTarget,aPname]);
end;

function TJSWebGLRenderingContextBase.getUniform(aProgram_: IJSWebGLProgram; aLocation: IJSWebGLUniformLocation): Variant;
begin
  Result:=InvokeJSVariantResult('getUniform',[aProgram_,aLocation]);
end;

function TJSWebGLRenderingContextBase.getUniformLocation(aProgram_: IJSWebGLProgram; const aName: UnicodeString): IJSWebGLUniformLocation;
begin
  Result:=InvokeJSObjectResult('getUniformLocation',[aProgram_,aName],TJSWebGLUniformLocation) as IJSWebGLUniformLocation;
end;

function TJSWebGLRenderingContextBase.getVertexAttrib(aIndex: TGLuint; aPname: TGLenum): Variant;
begin
  Result:=InvokeJSVariantResult('getVertexAttrib',[aIndex,aPname]);
end;

function TJSWebGLRenderingContextBase.getVertexAttribOffset(aIndex: TGLuint; aPname: TGLenum): TGLintptr;
begin
  Result:=InvokeJSMaxIntResult('getVertexAttribOffset',[aIndex,aPname]);
end;

procedure TJSWebGLRenderingContextBase.hint(aTarget: TGLenum; aMode: TGLenum);
begin
  InvokeJSNoResult('hint',[aTarget,aMode]);
end;

function TJSWebGLRenderingContextBase.isBuffer(aBuffer: IJSWebGLBuffer): TGLboolean;
begin
  Result:=InvokeJSBooleanResult('isBuffer',[aBuffer]);
end;

function TJSWebGLRenderingContextBase.isEnabled(aCap: TGLenum): TGLboolean;
begin
  Result:=InvokeJSBooleanResult('isEnabled',[aCap]);
end;

function TJSWebGLRenderingContextBase.isFramebuffer(aFramebuffer: IJSWebGLFramebuffer): TGLboolean;
begin
  Result:=InvokeJSBooleanResult('isFramebuffer',[aFramebuffer]);
end;

function TJSWebGLRenderingContextBase.isProgram(aProgram_: IJSWebGLProgram): TGLboolean;
begin
  Result:=InvokeJSBooleanResult('isProgram',[aProgram_]);
end;

function TJSWebGLRenderingContextBase.isRenderbuffer(aRenderbuffer: IJSWebGLRenderbuffer): TGLboolean;
begin
  Result:=InvokeJSBooleanResult('isRenderbuffer',[aRenderbuffer]);
end;

function TJSWebGLRenderingContextBase.isShader(aShader: IJSWebGLShader): TGLboolean;
begin
  Result:=InvokeJSBooleanResult('isShader',[aShader]);
end;

function TJSWebGLRenderingContextBase.isTexture(aTexture: IJSWebGLTexture): TGLboolean;
begin
  Result:=InvokeJSBooleanResult('isTexture',[aTexture]);
end;

procedure TJSWebGLRenderingContextBase.lineWidth(aWidth: TGLfloat);
begin
  InvokeJSNoResult('lineWidth',[aWidth]);
end;

procedure TJSWebGLRenderingContextBase.linkProgram(aProgram_: IJSWebGLProgram);
begin
  InvokeJSNoResult('linkProgram',[aProgram_]);
end;

procedure TJSWebGLRenderingContextBase.pixelStorei(aPname: TGLenum; aParam: TGLint);
begin
  InvokeJSNoResult('pixelStorei',[aPname,aParam]);
end;

procedure TJSWebGLRenderingContextBase.polygonOffset(aFactor: TGLfloat; aUnits: TGLfloat);
begin
  InvokeJSNoResult('polygonOffset',[aFactor,aUnits]);
end;

procedure TJSWebGLRenderingContextBase.renderbufferStorage(aTarget: TGLenum; aInternalformat: TGLenum; aWidth: TGLsizei; aHeight: TGLsizei);
begin
  InvokeJSNoResult('renderbufferStorage',[aTarget,aInternalformat,aWidth,aHeight]);
end;

procedure TJSWebGLRenderingContextBase.sampleCoverage(aValue: TGLclampf; aInvert: TGLboolean);
begin
  InvokeJSNoResult('sampleCoverage',[aValue,aInvert]);
end;

procedure TJSWebGLRenderingContextBase.scissor(aX: TGLint; aY: TGLint; aWidth: TGLsizei; aHeight: TGLsizei);
begin
  InvokeJSNoResult('scissor',[aX,aY,aWidth,aHeight]);
end;

procedure TJSWebGLRenderingContextBase.shaderSource(aShader: IJSWebGLShader; const aSource: UnicodeString);
begin
  InvokeJSNoResult('shaderSource',[aShader,aSource]);
end;

procedure TJSWebGLRenderingContextBase.stencilFunc(aFunc: TGLenum; aRef: TGLint; aMask: TGLuint);
begin
  InvokeJSNoResult('stencilFunc',[aFunc,aRef,aMask]);
end;

procedure TJSWebGLRenderingContextBase.stencilFuncSeparate(aFace: TGLenum; aFunc: TGLenum; aRef: TGLint; aMask: TGLuint);
begin
  InvokeJSNoResult('stencilFuncSeparate',[aFace,aFunc,aRef,aMask]);
end;

procedure TJSWebGLRenderingContextBase.stencilMask(aMask: TGLuint);
begin
  InvokeJSNoResult('stencilMask',[aMask]);
end;

procedure TJSWebGLRenderingContextBase.stencilMaskSeparate(aFace: TGLenum; aMask: TGLuint);
begin
  InvokeJSNoResult('stencilMaskSeparate',[aFace,aMask]);
end;

procedure TJSWebGLRenderingContextBase.stencilOp(aFail: TGLenum; aZfail: TGLenum; aZpass: TGLenum);
begin
  InvokeJSNoResult('stencilOp',[aFail,aZfail,aZpass]);
end;

procedure TJSWebGLRenderingContextBase.stencilOpSeparate(aFace: TGLenum; aFail: TGLenum; aZfail: TGLenum; aZpass: TGLenum);
begin
  InvokeJSNoResult('stencilOpSeparate',[aFace,aFail,aZfail,aZpass]);
end;

procedure TJSWebGLRenderingContextBase.texParameterf(aTarget: TGLenum; aPname: TGLenum; aParam: TGLfloat);
begin
  InvokeJSNoResult('texParameterf',[aTarget,aPname,aParam]);
end;

procedure TJSWebGLRenderingContextBase.texParameteri(aTarget: TGLenum; aPname: TGLenum; aParam: TGLint);
begin
  InvokeJSNoResult('texParameteri',[aTarget,aPname,aParam]);
end;

procedure TJSWebGLRenderingContextBase.uniform1f(aLocation: IJSWebGLUniformLocation; aX: TGLfloat);
begin
  InvokeJSNoResult('uniform1f',[aLocation,aX]);
end;

procedure TJSWebGLRenderingContextBase.uniform2f(aLocation: IJSWebGLUniformLocation; aX: TGLfloat; aY: TGLfloat);
begin
  InvokeJSNoResult('uniform2f',[aLocation,aX,aY]);
end;

procedure TJSWebGLRenderingContextBase.uniform3f(aLocation: IJSWebGLUniformLocation; aX: TGLfloat; aY: TGLfloat; aZ: TGLfloat);
begin
  InvokeJSNoResult('uniform3f',[aLocation,aX,aY,aZ]);
end;

procedure TJSWebGLRenderingContextBase.uniform4f(aLocation: IJSWebGLUniformLocation; aX: TGLfloat; aY: TGLfloat; aZ: TGLfloat; aW: TGLfloat);
begin
  InvokeJSNoResult('uniform4f',[aLocation,aX,aY,aZ,aW]);
end;

procedure TJSWebGLRenderingContextBase.uniform1i(aLocation: IJSWebGLUniformLocation; aX: TGLint);
begin
  InvokeJSNoResult('uniform1i',[aLocation,aX]);
end;

procedure TJSWebGLRenderingContextBase.uniform2i(aLocation: IJSWebGLUniformLocation; aX: TGLint; aY: TGLint);
begin
  InvokeJSNoResult('uniform2i',[aLocation,aX,aY]);
end;

procedure TJSWebGLRenderingContextBase.uniform3i(aLocation: IJSWebGLUniformLocation; aX: TGLint; aY: TGLint; aZ: TGLint);
begin
  InvokeJSNoResult('uniform3i',[aLocation,aX,aY,aZ]);
end;

procedure TJSWebGLRenderingContextBase.uniform4i(aLocation: IJSWebGLUniformLocation; aX: TGLint; aY: TGLint; aZ: TGLint; aW: TGLint);
begin
  InvokeJSNoResult('uniform4i',[aLocation,aX,aY,aZ,aW]);
end;

procedure TJSWebGLRenderingContextBase.useProgram(aProgram_: IJSWebGLProgram);
begin
  InvokeJSNoResult('useProgram',[aProgram_]);
end;

procedure TJSWebGLRenderingContextBase.validateProgram(aProgram_: IJSWebGLProgram);
begin
  InvokeJSNoResult('validateProgram',[aProgram_]);
end;

procedure TJSWebGLRenderingContextBase.vertexAttrib1f(aIndx: TGLuint; aX: TGLfloat);
begin
  InvokeJSNoResult('vertexAttrib1f',[aIndx,aX]);
end;

procedure TJSWebGLRenderingContextBase.vertexAttrib1fv(aIndx: TGLuint; aValues: IJSFloat32Array);
begin
  InvokeJSNoResult('vertexAttrib1fv',[aIndx,aValues]);
end;

procedure TJSWebGLRenderingContextBase.vertexAttrib1fv(aIndx: TGLuint; const aValues: TGLfloatDynArray);
begin
  InvokeJSNoResult('vertexAttrib1fv',[aIndx,aValues]);
end;

procedure TJSWebGLRenderingContextBase.vertexAttrib2f(aIndx: TGLuint; aX: TGLfloat; aY: TGLfloat);
begin
  InvokeJSNoResult('vertexAttrib2f',[aIndx,aX,aY]);
end;

procedure TJSWebGLRenderingContextBase.vertexAttrib2fv(aIndx: TGLuint; aValues: IJSFloat32Array);
begin
  InvokeJSNoResult('vertexAttrib2fv',[aIndx,aValues]);
end;

procedure TJSWebGLRenderingContextBase.vertexAttrib2fv(aIndx: TGLuint; const aValues: TGLfloatDynArray);
begin
  InvokeJSNoResult('vertexAttrib2fv',[aIndx,aValues]);
end;

procedure TJSWebGLRenderingContextBase.vertexAttrib3f(aIndx: TGLuint; aX: TGLfloat; aY: TGLfloat; aZ: TGLfloat);
begin
  InvokeJSNoResult('vertexAttrib3f',[aIndx,aX,aY,aZ]);
end;

procedure TJSWebGLRenderingContextBase.vertexAttrib3fv(aIndx: TGLuint; aValues: IJSFloat32Array);
begin
  InvokeJSNoResult('vertexAttrib3fv',[aIndx,aValues]);
end;

procedure TJSWebGLRenderingContextBase.vertexAttrib3fv(aIndx: TGLuint; const aValues: TGLfloatDynArray);
begin
  InvokeJSNoResult('vertexAttrib3fv',[aIndx,aValues]);
end;

procedure TJSWebGLRenderingContextBase.vertexAttrib4f(aIndx: TGLuint; aX: TGLfloat; aY: TGLfloat; aZ: TGLfloat; aW: TGLfloat);
begin
  InvokeJSNoResult('vertexAttrib4f',[aIndx,aX,aY,aZ,aW]);
end;

procedure TJSWebGLRenderingContextBase.vertexAttrib4fv(aIndx: TGLuint; aValues: IJSFloat32Array);
begin
  InvokeJSNoResult('vertexAttrib4fv',[aIndx,aValues]);
end;

procedure TJSWebGLRenderingContextBase.vertexAttrib4fv(aIndx: TGLuint; const aValues: TGLfloatDynArray);
begin
  InvokeJSNoResult('vertexAttrib4fv',[aIndx,aValues]);
end;

procedure TJSWebGLRenderingContextBase.vertexAttribPointer(aIndx: TGLuint; aSize: TGLint; aType_: TGLenum; aNormalized: TGLboolean; aStride: TGLsizei; aOffset: TGLintptr);
begin
  InvokeJSNoResult('vertexAttribPointer',[aIndx,aSize,aType_,aNormalized,aStride,aOffset]);
end;

procedure TJSWebGLRenderingContextBase.viewport(aX: TGLint; aY: TGLint; aWidth: TGLsizei; aHeight: TGLsizei);
begin
  InvokeJSNoResult('viewport',[aX,aY,aWidth,aHeight]);
end;

function TJSWebGLRenderingContextBase.makeXRCompatible: IJSPromise; // Promise<undefined>
begin
  Result:=InvokeJSObjectResult('makeXRCompatible',[],TJSPromise) as IJSPromise;
end;

class function TJSWebGLRenderingContextBase.JSClassName: UnicodeString;
begin
  Result:='WebGLRenderingContextBase';
end;

class function TJSWebGLRenderingContextBase.Cast(const Intf: IJSObject): IJSWebGLRenderingContextBase;
begin
  Result:=TJSWebGLRenderingContextBase.JOBCast(Intf);
end;

procedure TJSWebGLRenderingContext.bufferData(aTarget: TGLenum; aSize: TGLsizeiptr; aUsage: TGLenum);
begin
  InvokeJSNoResult('bufferData',[aTarget,aSize,aUsage]);
end;

procedure TJSWebGLRenderingContext.bufferData(aTarget: TGLenum; aData: IJSArrayBuffer; aUsage: TGLenum);
begin
  InvokeJSNoResult('bufferData',[aTarget,aData,aUsage]);
end;

procedure TJSWebGLRenderingContext.bufferData(aTarget: TGLenum; aData: IJSArrayBufferView; aUsage: TGLenum);
begin
  InvokeJSNoResult('bufferData',[aTarget,aData,aUsage]);
end;

procedure TJSWebGLRenderingContext.bufferSubData(aTarget: TGLenum; aOffset: TGLintptr; aData: IJSArrayBuffer);
begin
  InvokeJSNoResult('bufferSubData',[aTarget,aOffset,aData]);
end;

procedure TJSWebGLRenderingContext.bufferSubData(aTarget: TGLenum; aOffset: TGLintptr; aData: IJSArrayBufferView);
begin
  InvokeJSNoResult('bufferSubData',[aTarget,aOffset,aData]);
end;

procedure TJSWebGLRenderingContext.compressedTexImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLenum; aWidth: TGLsizei; aHeight: TGLsizei; aBorder: TGLint; aData: IJSArrayBufferView);
begin
  InvokeJSNoResult('compressedTexImage2D',[aTarget,aLevel,aInternalformat,aWidth,aHeight,aBorder,aData]);
end;

procedure TJSWebGLRenderingContext.compressedTexSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aFormat: TGLenum; aData: IJSArrayBufferView);
begin
  InvokeJSNoResult('compressedTexSubImage2D',[aTarget,aLevel,aXoffset,aYoffset,aWidth,aHeight,aFormat,aData]);
end;

procedure TJSWebGLRenderingContext.readPixels(aX: TGLint; aY: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aPixels: IJSArrayBufferView);
begin
  InvokeJSNoResult('readPixels',[aX,aY,aWidth,aHeight,aFormat,aType_,aPixels]);
end;

procedure TJSWebGLRenderingContext.texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aBorder: TGLint; aFormat: TGLenum; aType_: TGLenum; aPixels: IJSArrayBufferView);
begin
  InvokeJSNoResult('texImage2D',[aTarget,aLevel,aInternalformat,aWidth,aHeight,aBorder,aFormat,aType_,aPixels]);
end;

procedure TJSWebGLRenderingContext.texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aFormat: TGLenum; aType_: TGLenum; aPixels: IJSImageBitmap);
begin
  InvokeJSNoResult('texImage2D',[aTarget,aLevel,aInternalformat,aFormat,aType_,aPixels]);
end;

procedure TJSWebGLRenderingContext.texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aFormat: TGLenum; aType_: TGLenum; aPixels: IJSImageData);
begin
  InvokeJSNoResult('texImage2D',[aTarget,aLevel,aInternalformat,aFormat,aType_,aPixels]);
end;

procedure TJSWebGLRenderingContext.texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aFormat: TGLenum; aType_: TGLenum; aImage: IJSHTMLImageElement);
begin
  InvokeJSNoResult('texImage2D',[aTarget,aLevel,aInternalformat,aFormat,aType_,aImage]);
end;

procedure TJSWebGLRenderingContext.texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aFormat: TGLenum; aType_: TGLenum; aCanvas: IJSHTMLCanvasElement);
begin
  InvokeJSNoResult('texImage2D',[aTarget,aLevel,aInternalformat,aFormat,aType_,aCanvas]);
end;

procedure TJSWebGLRenderingContext.texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aFormat: TGLenum; aType_: TGLenum; aVideo: IJSHTMLVideoElement);
begin
  InvokeJSNoResult('texImage2D',[aTarget,aLevel,aInternalformat,aFormat,aType_,aVideo]);
end;

procedure TJSWebGLRenderingContext.texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aFormat: TGLenum; aType_: TGLenum; aCanvas: IJSOffscreenCanvas);
begin
  InvokeJSNoResult('texImage2D',[aTarget,aLevel,aInternalformat,aFormat,aType_,aCanvas]);
end;

procedure TJSWebGLRenderingContext.texImage2D(aTarget: TGLenum; aLevel: TGLint; aInternalformat: TGLint; aFormat: TGLenum; aType_: TGLenum; aVideoFrame: IJSVideoFrame);
begin
  InvokeJSNoResult('texImage2D',[aTarget,aLevel,aInternalformat,aFormat,aType_,aVideoFrame]);
end;

procedure TJSWebGLRenderingContext.texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aWidth: TGLsizei; aHeight: TGLsizei; aFormat: TGLenum; aType_: TGLenum; aPixels: IJSArrayBufferView);
begin
  InvokeJSNoResult('texSubImage2D',[aTarget,aLevel,aXoffset,aYoffset,aWidth,aHeight,aFormat,aType_,aPixels]);
end;

procedure TJSWebGLRenderingContext.texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aFormat: TGLenum; aType_: TGLenum; aPixels: IJSImageBitmap);
begin
  InvokeJSNoResult('texSubImage2D',[aTarget,aLevel,aXoffset,aYoffset,aFormat,aType_,aPixels]);
end;

procedure TJSWebGLRenderingContext.texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aFormat: TGLenum; aType_: TGLenum; aPixels: IJSImageData);
begin
  InvokeJSNoResult('texSubImage2D',[aTarget,aLevel,aXoffset,aYoffset,aFormat,aType_,aPixels]);
end;

procedure TJSWebGLRenderingContext.texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aFormat: TGLenum; aType_: TGLenum; aImage: IJSHTMLImageElement);
begin
  InvokeJSNoResult('texSubImage2D',[aTarget,aLevel,aXoffset,aYoffset,aFormat,aType_,aImage]);
end;

procedure TJSWebGLRenderingContext.texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aFormat: TGLenum; aType_: TGLenum; aCanvas: IJSHTMLCanvasElement);
begin
  InvokeJSNoResult('texSubImage2D',[aTarget,aLevel,aXoffset,aYoffset,aFormat,aType_,aCanvas]);
end;

procedure TJSWebGLRenderingContext.texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aFormat: TGLenum; aType_: TGLenum; aVideo: IJSHTMLVideoElement);
begin
  InvokeJSNoResult('texSubImage2D',[aTarget,aLevel,aXoffset,aYoffset,aFormat,aType_,aVideo]);
end;

procedure TJSWebGLRenderingContext.texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aFormat: TGLenum; aType_: TGLenum; aCanvas: IJSOffscreenCanvas);
begin
  InvokeJSNoResult('texSubImage2D',[aTarget,aLevel,aXoffset,aYoffset,aFormat,aType_,aCanvas]);
end;

procedure TJSWebGLRenderingContext.texSubImage2D(aTarget: TGLenum; aLevel: TGLint; aXoffset: TGLint; aYoffset: TGLint; aFormat: TGLenum; aType_: TGLenum; aVideoFrame: IJSVideoFrame);
begin
  InvokeJSNoResult('texSubImage2D',[aTarget,aLevel,aXoffset,aYoffset,aFormat,aType_,aVideoFrame]);
end;

procedure TJSWebGLRenderingContext.uniform1fv(aLocation: IJSWebGLUniformLocation; aData: IJSFloat32Array);
begin
  InvokeJSNoResult('uniform1fv',[aLocation,aData]);
end;

procedure TJSWebGLRenderingContext.uniform1fv(aLocation: IJSWebGLUniformLocation; const aData: TGLfloatDynArray);
begin
  InvokeJSNoResult('uniform1fv',[aLocation,aData]);
end;

procedure TJSWebGLRenderingContext.uniform2fv(aLocation: IJSWebGLUniformLocation; aData: IJSFloat32Array);
begin
  InvokeJSNoResult('uniform2fv',[aLocation,aData]);
end;

procedure TJSWebGLRenderingContext.uniform2fv(aLocation: IJSWebGLUniformLocation; const aData: TGLfloatDynArray);
begin
  InvokeJSNoResult('uniform2fv',[aLocation,aData]);
end;

procedure TJSWebGLRenderingContext.uniform3fv(aLocation: IJSWebGLUniformLocation; aData: IJSFloat32Array);
begin
  InvokeJSNoResult('uniform3fv',[aLocation,aData]);
end;

procedure TJSWebGLRenderingContext.uniform3fv(aLocation: IJSWebGLUniformLocation; const aData: TGLfloatDynArray);
begin
  InvokeJSNoResult('uniform3fv',[aLocation,aData]);
end;

procedure TJSWebGLRenderingContext.uniform4fv(aLocation: IJSWebGLUniformLocation; aData: IJSFloat32Array);
begin
  InvokeJSNoResult('uniform4fv',[aLocation,aData]);
end;

procedure TJSWebGLRenderingContext.uniform4fv(aLocation: IJSWebGLUniformLocation; const aData: TGLfloatDynArray);
begin
  InvokeJSNoResult('uniform4fv',[aLocation,aData]);
end;

procedure TJSWebGLRenderingContext.uniform1iv(aLocation: IJSWebGLUniformLocation; aData: IJSInt32Array);
begin
  InvokeJSNoResult('uniform1iv',[aLocation,aData]);
end;

procedure TJSWebGLRenderingContext.uniform1iv(aLocation: IJSWebGLUniformLocation; const aData: TGLintDynArray);
begin
  InvokeJSNoResult('uniform1iv',[aLocation,aData]);
end;

procedure TJSWebGLRenderingContext.uniform2iv(aLocation: IJSWebGLUniformLocation; aData: IJSInt32Array);
begin
  InvokeJSNoResult('uniform2iv',[aLocation,aData]);
end;

procedure TJSWebGLRenderingContext.uniform2iv(aLocation: IJSWebGLUniformLocation; const aData: TGLintDynArray);
begin
  InvokeJSNoResult('uniform2iv',[aLocation,aData]);
end;

procedure TJSWebGLRenderingContext.uniform3iv(aLocation: IJSWebGLUniformLocation; aData: IJSInt32Array);
begin
  InvokeJSNoResult('uniform3iv',[aLocation,aData]);
end;

procedure TJSWebGLRenderingContext.uniform3iv(aLocation: IJSWebGLUniformLocation; const aData: TGLintDynArray);
begin
  InvokeJSNoResult('uniform3iv',[aLocation,aData]);
end;

procedure TJSWebGLRenderingContext.uniform4iv(aLocation: IJSWebGLUniformLocation; aData: IJSInt32Array);
begin
  InvokeJSNoResult('uniform4iv',[aLocation,aData]);
end;

procedure TJSWebGLRenderingContext.uniform4iv(aLocation: IJSWebGLUniformLocation; const aData: TGLintDynArray);
begin
  InvokeJSNoResult('uniform4iv',[aLocation,aData]);
end;

procedure TJSWebGLRenderingContext.uniformMatrix2fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array);
begin
  InvokeJSNoResult('uniformMatrix2fv',[aLocation,aTranspose,aData]);
end;

procedure TJSWebGLRenderingContext.uniformMatrix2fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray);
begin
  InvokeJSNoResult('uniformMatrix2fv',[aLocation,aTranspose,aData]);
end;

procedure TJSWebGLRenderingContext.uniformMatrix3fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array);
begin
  InvokeJSNoResult('uniformMatrix3fv',[aLocation,aTranspose,aData]);
end;

procedure TJSWebGLRenderingContext.uniformMatrix3fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray);
begin
  InvokeJSNoResult('uniformMatrix3fv',[aLocation,aTranspose,aData]);
end;

procedure TJSWebGLRenderingContext.uniformMatrix4fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; aData: IJSFloat32Array);
begin
  InvokeJSNoResult('uniformMatrix4fv',[aLocation,aTranspose,aData]);
end;

procedure TJSWebGLRenderingContext.uniformMatrix4fv(aLocation: IJSWebGLUniformLocation; aTranspose: TGLboolean; const aData: TGLfloatDynArray);
begin
  InvokeJSNoResult('uniformMatrix4fv',[aLocation,aTranspose,aData]);
end;

class function TJSWebGLRenderingContext.JSClassName: UnicodeString;
begin
  Result:='WebGLRenderingContext';
end;

class function TJSWebGLRenderingContext.Cast(const Intf: IJSObject): IJSWebGLRenderingContext;
begin
  Result:=TJSWebGLRenderingContext.JOBCast(Intf);
end;

class function TJSEXT_texture_compression_bptc.JSClassName: UnicodeString;
begin
  Result:='EXT_texture_compression_bptc';
end;

class function TJSEXT_texture_compression_bptc.Cast(const Intf: IJSObject): IJSEXT_texture_compression_bptc;
begin
  Result:=TJSEXT_texture_compression_bptc.JOBCast(Intf);
end;

class function TJSEXT_texture_compression_rgtc.JSClassName: UnicodeString;
begin
  Result:='EXT_texture_compression_rgtc';
end;

class function TJSEXT_texture_compression_rgtc.Cast(const Intf: IJSObject): IJSEXT_texture_compression_rgtc;
begin
  Result:=TJSEXT_texture_compression_rgtc.JOBCast(Intf);
end;

class function TJSEXT_texture_norm16.JSClassName: UnicodeString;
begin
  Result:='EXT_texture_norm16';
end;

class function TJSEXT_texture_norm16.Cast(const Intf: IJSObject): IJSEXT_texture_norm16;
begin
  Result:=TJSEXT_texture_norm16.JOBCast(Intf);
end;

class function TJSWEBGL_compressed_texture_s3tc.JSClassName: UnicodeString;
begin
  Result:='WEBGL_compressed_texture_s3tc';
end;

class function TJSWEBGL_compressed_texture_s3tc.Cast(const Intf: IJSObject): IJSWEBGL_compressed_texture_s3tc;
begin
  Result:=TJSWEBGL_compressed_texture_s3tc.JOBCast(Intf);
end;

class function TJSWEBGL_compressed_texture_s3tc_srgb.JSClassName: UnicodeString;
begin
  Result:='WEBGL_compressed_texture_s3tc_srgb';
end;

class function TJSWEBGL_compressed_texture_s3tc_srgb.Cast(const Intf: IJSObject): IJSWEBGL_compressed_texture_s3tc_srgb;
begin
  Result:=TJSWEBGL_compressed_texture_s3tc_srgb.JOBCast(Intf);
end;

function TJSWEBGL_compressed_texture_astc.getSupportedProfiles: TUnicodeStringDynArray;
begin
  Result:=InvokeJSObjectResult('getSupportedProfiles',[],TJSArray) as TUnicodeStringDynArray;
end;

class function TJSWEBGL_compressed_texture_astc.JSClassName: UnicodeString;
begin
  Result:='WEBGL_compressed_texture_astc';
end;

class function TJSWEBGL_compressed_texture_astc.Cast(const Intf: IJSObject): IJSWEBGL_compressed_texture_astc;
begin
  Result:=TJSWEBGL_compressed_texture_astc.JOBCast(Intf);
end;

class function TJSWEBGL_compressed_texture_etc.JSClassName: UnicodeString;
begin
  Result:='WEBGL_compressed_texture_etc';
end;

class function TJSWEBGL_compressed_texture_etc.Cast(const Intf: IJSObject): IJSWEBGL_compressed_texture_etc;
begin
  Result:=TJSWEBGL_compressed_texture_etc.JOBCast(Intf);
end;

class function TJSWEBGL_compressed_texture_etc1.JSClassName: UnicodeString;
begin
  Result:='WEBGL_compressed_texture_etc1';
end;

class function TJSWEBGL_compressed_texture_etc1.Cast(const Intf: IJSObject): IJSWEBGL_compressed_texture_etc1;
begin
  Result:=TJSWEBGL_compressed_texture_etc1.JOBCast(Intf);
end;

class function TJSWEBGL_compressed_texture_pvrtc.JSClassName: UnicodeString;
begin
  Result:='WEBGL_compressed_texture_pvrtc';
end;

class function TJSWEBGL_compressed_texture_pvrtc.Cast(const Intf: IJSObject): IJSWEBGL_compressed_texture_pvrtc;
begin
  Result:=TJSWEBGL_compressed_texture_pvrtc.JOBCast(Intf);
end;

class function TJSWEBGL_debug_renderer_info.JSClassName: UnicodeString;
begin
  Result:='WEBGL_debug_renderer_info';
end;

class function TJSWEBGL_debug_renderer_info.Cast(const Intf: IJSObject): IJSWEBGL_debug_renderer_info;
begin
  Result:=TJSWEBGL_debug_renderer_info.JOBCast(Intf);
end;

function TJSWEBGL_debug_shaders.getTranslatedShaderSource(aShader: IJSWebGLShader): UnicodeString;
begin
  Result:=InvokeJSUnicodeStringResult('getTranslatedShaderSource',[aShader]);
end;

class function TJSWEBGL_debug_shaders.JSClassName: UnicodeString;
begin
  Result:='WEBGL_debug_shaders';
end;

class function TJSWEBGL_debug_shaders.Cast(const Intf: IJSObject): IJSWEBGL_debug_shaders;
begin
  Result:=TJSWEBGL_debug_shaders.JOBCast(Intf);
end;

class function TJSWEBGL_depth_texture.JSClassName: UnicodeString;
begin
  Result:='WEBGL_depth_texture';
end;

class function TJSWEBGL_depth_texture.Cast(const Intf: IJSObject): IJSWEBGL_depth_texture;
begin
  Result:=TJSWEBGL_depth_texture.JOBCast(Intf);
end;

class function TJSOES_element_index_uint.JSClassName: UnicodeString;
begin
  Result:='OES_element_index_uint';
end;

class function TJSOES_element_index_uint.Cast(const Intf: IJSObject): IJSOES_element_index_uint;
begin
  Result:=TJSOES_element_index_uint.JOBCast(Intf);
end;

class function TJSEXT_frag_depth.JSClassName: UnicodeString;
begin
  Result:='EXT_frag_depth';
end;

class function TJSEXT_frag_depth.Cast(const Intf: IJSObject): IJSEXT_frag_depth;
begin
  Result:=TJSEXT_frag_depth.JOBCast(Intf);
end;

procedure TJSWEBGL_lose_context.loseContext;
begin
  InvokeJSNoResult('loseContext',[]);
end;

procedure TJSWEBGL_lose_context.restoreContext;
begin
  InvokeJSNoResult('restoreContext',[]);
end;

class function TJSWEBGL_lose_context.JSClassName: UnicodeString;
begin
  Result:='WEBGL_lose_context';
end;

class function TJSWEBGL_lose_context.Cast(const Intf: IJSObject): IJSWEBGL_lose_context;
begin
  Result:=TJSWEBGL_lose_context.JOBCast(Intf);
end;

class function TJSEXT_texture_filter_anisotropic.JSClassName: UnicodeString;
begin
  Result:='EXT_texture_filter_anisotropic';
end;

class function TJSEXT_texture_filter_anisotropic.Cast(const Intf: IJSObject): IJSEXT_texture_filter_anisotropic;
begin
  Result:=TJSEXT_texture_filter_anisotropic.JOBCast(Intf);
end;

class function TJSEXT_sRGB.JSClassName: UnicodeString;
begin
  Result:='EXT_sRGB';
end;

class function TJSEXT_sRGB.Cast(const Intf: IJSObject): IJSEXT_sRGB;
begin
  Result:=TJSEXT_sRGB.JOBCast(Intf);
end;

class function TJSOES_standard_derivatives.JSClassName: UnicodeString;
begin
  Result:='OES_standard_derivatives';
end;

class function TJSOES_standard_derivatives.Cast(const Intf: IJSObject): IJSOES_standard_derivatives;
begin
  Result:=TJSOES_standard_derivatives.JOBCast(Intf);
end;

class function TJSOES_texture_float.JSClassName: UnicodeString;
begin
  Result:='OES_texture_float';
end;

class function TJSOES_texture_float.Cast(const Intf: IJSObject): IJSOES_texture_float;
begin
  Result:=TJSOES_texture_float.JOBCast(Intf);
end;

procedure TJSWEBGL_draw_buffers.drawBuffersWEBGL(const aBuffers: TGLenumDynArray);
begin
  InvokeJSNoResult('drawBuffersWEBGL',[aBuffers]);
end;

class function TJSWEBGL_draw_buffers.JSClassName: UnicodeString;
begin
  Result:='WEBGL_draw_buffers';
end;

class function TJSWEBGL_draw_buffers.Cast(const Intf: IJSObject): IJSWEBGL_draw_buffers;
begin
  Result:=TJSWEBGL_draw_buffers.JOBCast(Intf);
end;

class function TJSOES_texture_float_linear.JSClassName: UnicodeString;
begin
  Result:='OES_texture_float_linear';
end;

class function TJSOES_texture_float_linear.Cast(const Intf: IJSObject): IJSOES_texture_float_linear;
begin
  Result:=TJSOES_texture_float_linear.JOBCast(Intf);
end;

class function TJSEXT_shader_texture_lod.JSClassName: UnicodeString;
begin
  Result:='EXT_shader_texture_lod';
end;

class function TJSEXT_shader_texture_lod.Cast(const Intf: IJSObject): IJSEXT_shader_texture_lod;
begin
  Result:=TJSEXT_shader_texture_lod.JOBCast(Intf);
end;

class function TJSOES_texture_half_float.JSClassName: UnicodeString;
begin
  Result:='OES_texture_half_float';
end;

class function TJSOES_texture_half_float.Cast(const Intf: IJSObject): IJSOES_texture_half_float;
begin
  Result:=TJSOES_texture_half_float.JOBCast(Intf);
end;

class function TJSOES_texture_half_float_linear.JSClassName: UnicodeString;
begin
  Result:='OES_texture_half_float_linear';
end;

class function TJSOES_texture_half_float_linear.Cast(const Intf: IJSObject): IJSOES_texture_half_float_linear;
begin
  Result:=TJSOES_texture_half_float_linear.JOBCast(Intf);
end;

class function TJSWEBGL_color_buffer_float.JSClassName: UnicodeString;
begin
  Result:='WEBGL_color_buffer_float';
end;

class function TJSWEBGL_color_buffer_float.Cast(const Intf: IJSObject): IJSWEBGL_color_buffer_float;
begin
  Result:=TJSWEBGL_color_buffer_float.JOBCast(Intf);
end;

class function TJSEXT_color_buffer_half_float.JSClassName: UnicodeString;
begin
  Result:='EXT_color_buffer_half_float';
end;

class function TJSEXT_color_buffer_half_float.Cast(const Intf: IJSObject): IJSEXT_color_buffer_half_float;
begin
  Result:=TJSEXT_color_buffer_half_float.JOBCast(Intf);
end;

function TJSOES_vertex_array_object.createVertexArrayOES: IJSWebGLVertexArrayObject;
begin
  Result:=InvokeJSObjectResult('createVertexArrayOES',[],TJSWebGLVertexArrayObject) as IJSWebGLVertexArrayObject;
end;

procedure TJSOES_vertex_array_object.deleteVertexArrayOES(aArrayObject: IJSWebGLVertexArrayObject);
begin
  InvokeJSNoResult('deleteVertexArrayOES',[aArrayObject]);
end;

function TJSOES_vertex_array_object.isVertexArrayOES(aArrayObject: IJSWebGLVertexArrayObject): TGLboolean;
begin
  Result:=InvokeJSBooleanResult('isVertexArrayOES',[aArrayObject]);
end;

procedure TJSOES_vertex_array_object.bindVertexArrayOES(aArrayObject: IJSWebGLVertexArrayObject);
begin
  InvokeJSNoResult('bindVertexArrayOES',[aArrayObject]);
end;

class function TJSOES_vertex_array_object.JSClassName: UnicodeString;
begin
  Result:='OES_vertex_array_object';
end;

class function TJSOES_vertex_array_object.Cast(const Intf: IJSObject): IJSOES_vertex_array_object;
begin
  Result:=TJSOES_vertex_array_object.JOBCast(Intf);
end;

procedure TJSANGLE_instanced_arrays.drawArraysInstancedANGLE(aMode: TGLenum; aFirst: TGLint; aCount: TGLsizei; aPrimcount: TGLsizei);
begin
  InvokeJSNoResult('drawArraysInstancedANGLE',[aMode,aFirst,aCount,aPrimcount]);
end;

procedure TJSANGLE_instanced_arrays.drawElementsInstancedANGLE(aMode: TGLenum; aCount: TGLsizei; aType_: TGLenum; aOffset: TGLintptr; aPrimcount: TGLsizei);
begin
  InvokeJSNoResult('drawElementsInstancedANGLE',[aMode,aCount,aType_,aOffset,aPrimcount]);
end;

procedure TJSANGLE_instanced_arrays.vertexAttribDivisorANGLE(aIndex: TGLuint; aDivisor: TGLuint);
begin
  InvokeJSNoResult('vertexAttribDivisorANGLE',[aIndex,aDivisor]);
end;

class function TJSANGLE_instanced_arrays.JSClassName: UnicodeString;
begin
  Result:='ANGLE_instanced_arrays';
end;

class function TJSANGLE_instanced_arrays.Cast(const Intf: IJSObject): IJSANGLE_instanced_arrays;
begin
  Result:=TJSANGLE_instanced_arrays.JOBCast(Intf);
end;

class function TJSEXT_blend_minmax.JSClassName: UnicodeString;
begin
  Result:='EXT_blend_minmax';
end;

class function TJSEXT_blend_minmax.Cast(const Intf: IJSObject): IJSEXT_blend_minmax;
begin
  Result:=TJSEXT_blend_minmax.JOBCast(Intf);
end;

class function TJSWebGLQuery.JSClassName: UnicodeString;
begin
  Result:='WebGLQuery';
end;

class function TJSWebGLQuery.Cast(const Intf: IJSObject): IJSWebGLQuery;
begin
  Result:=TJSWebGLQuery.JOBCast(Intf);
end;

function TJSEXT_disjoint_timer_query.createQueryEXT: IJSWebGLQuery;
begin
  Result:=InvokeJSObjectResult('createQueryEXT',[],TJSWebGLQuery) as IJSWebGLQuery;
end;

procedure TJSEXT_disjoint_timer_query.deleteQueryEXT(aQuery: IJSWebGLQuery);
begin
  InvokeJSNoResult('deleteQueryEXT',[aQuery]);
end;

function TJSEXT_disjoint_timer_query.isQueryEXT(aQuery: IJSWebGLQuery): Boolean;
begin
  Result:=InvokeJSBooleanResult('isQueryEXT',[aQuery]);
end;

procedure TJSEXT_disjoint_timer_query.beginQueryEXT(aTarget: TGLenum; aQuery: IJSWebGLQuery);
begin
  InvokeJSNoResult('beginQueryEXT',[aTarget,aQuery]);
end;

procedure TJSEXT_disjoint_timer_query.endQueryEXT(aTarget: TGLenum);
begin
  InvokeJSNoResult('endQueryEXT',[aTarget]);
end;

procedure TJSEXT_disjoint_timer_query.queryCounterEXT(aQuery: IJSWebGLQuery; aTarget: TGLenum);
begin
  InvokeJSNoResult('queryCounterEXT',[aQuery,aTarget]);
end;

function TJSEXT_disjoint_timer_query.getQueryEXT(aTarget: TGLenum; aPname: TGLenum): Variant;
begin
  Result:=InvokeJSVariantResult('getQueryEXT',[aTarget,aPname]);
end;

function TJSEXT_disjoint_timer_query.getQueryObjectEXT(aQuery: IJSWebGLQuery; aPname: TGLenum): Variant;
begin
  Result:=InvokeJSVariantResult('getQueryObjectEXT',[aQuery,aPname]);
end;

class function TJSEXT_disjoint_timer_query.JSClassName: UnicodeString;
begin
  Result:='EXT_disjoint_timer_query';
end;

class function TJSEXT_disjoint_timer_query.Cast(const Intf: IJSObject): IJSEXT_disjoint_timer_query;
begin
  Result:=TJSEXT_disjoint_timer_query.JOBCast(Intf);
end;

function TJSMOZ_debug.getParameter(aPname: TGLenum): Variant;
begin
  Result:=InvokeJSVariantResult('getParameter',[aPname]);
end;

class function TJSMOZ_debug.JSClassName: UnicodeString;
begin
  Result:='MOZ_debug';
end;

class function TJSMOZ_debug.Cast(const Intf: IJSObject): IJSMOZ_debug;
begin
  Result:=TJSMOZ_debug.JOBCast(Intf);
end;

class function TJSEXT_float_blend.JSClassName: UnicodeString;
begin
  Result:='EXT_float_blend';
end;

class function TJSEXT_float_blend.Cast(const Intf: IJSObject): IJSEXT_float_blend;
begin
  Result:=TJSEXT_float_blend.JOBCast(Intf);
end;

class function TJSOES_fbo_render_mipmap.JSClassName: UnicodeString;
begin
  Result:='OES_fbo_render_mipmap';
end;

class function TJSOES_fbo_render_mipmap.Cast(const Intf: IJSObject): IJSOES_fbo_render_mipmap;
begin
  Result:=TJSOES_fbo_render_mipmap.JOBCast(Intf);
end;

procedure TJSWEBGL_explicit_present.present;
begin
  InvokeJSNoResult('present',[]);
end;

class function TJSWEBGL_explicit_present.JSClassName: UnicodeString;
begin
  Result:='WEBGL_explicit_present';
end;

class function TJSWEBGL_explicit_present.Cast(const Intf: IJSObject): IJSWEBGL_explicit_present;
begin
  Result:=TJSWEBGL_explicit_present.JOBCast(Intf);
end;

procedure TJSOES_draw_buffers_indexed.enableiOES(aTarget: TGLenum; aIndex: TGLuint);
begin
  InvokeJSNoResult('enableiOES',[aTarget,aIndex]);
end;

procedure TJSOES_draw_buffers_indexed.disableiOES(aTarget: TGLenum; aIndex: TGLuint);
begin
  InvokeJSNoResult('disableiOES',[aTarget,aIndex]);
end;

procedure TJSOES_draw_buffers_indexed.blendEquationiOES(aBuf: TGLuint; aMode: TGLenum);
begin
  InvokeJSNoResult('blendEquationiOES',[aBuf,aMode]);
end;

procedure TJSOES_draw_buffers_indexed.blendEquationSeparateiOES(aBuf: TGLuint; aModeRGB: TGLenum; aModeAlpha: TGLenum);
begin
  InvokeJSNoResult('blendEquationSeparateiOES',[aBuf,aModeRGB,aModeAlpha]);
end;

procedure TJSOES_draw_buffers_indexed.blendFunciOES(aBuf: TGLuint; aSrc: TGLenum; aDst: TGLenum);
begin
  InvokeJSNoResult('blendFunciOES',[aBuf,aSrc,aDst]);
end;

procedure TJSOES_draw_buffers_indexed.blendFuncSeparateiOES(aBuf: TGLuint; aSrcRGB: TGLenum; aDstRGB: TGLenum; aSrcAlpha: TGLenum; aDstAlpha: TGLenum);
begin
  InvokeJSNoResult('blendFuncSeparateiOES',[aBuf,aSrcRGB,aDstRGB,aSrcAlpha,aDstAlpha]);
end;

procedure TJSOES_draw_buffers_indexed.colorMaskiOES(aBuf: TGLuint; aR: TGLboolean; aG: TGLboolean; aB: TGLboolean; aA: TGLboolean);
begin
  InvokeJSNoResult('colorMaskiOES',[aBuf,aR,aG,aB,aA]);
end;

class function TJSOES_draw_buffers_indexed.JSClassName: UnicodeString;
begin
  Result:='OES_draw_buffers_indexed';
end;

class function TJSOES_draw_buffers_indexed.Cast(const Intf: IJSObject): IJSOES_draw_buffers_indexed;
begin
  Result:=TJSOES_draw_buffers_indexed.JOBCast(Intf);
end;

procedure TJSWEBGL_provoking_vertex.provokingVertexWEBGL(aProvokeMode: TGLenum);
begin
  InvokeJSNoResult('provokingVertexWEBGL',[aProvokeMode]);
end;

class function TJSWEBGL_provoking_vertex.JSClassName: UnicodeString;
begin
  Result:='WEBGL_provoking_vertex';
end;

class function TJSWEBGL_provoking_vertex.Cast(const Intf: IJSObject): IJSWEBGL_provoking_vertex;
begin
  Result:=TJSWEBGL_provoking_vertex.JOBCast(Intf);
end;

class function TJSEXT_depth_clamp.JSClassName: UnicodeString;
begin
  Result:='EXT_depth_clamp';
end;

class function TJSEXT_depth_clamp.Cast(const Intf: IJSObject): IJSEXT_depth_clamp;
begin
  Result:=TJSEXT_depth_clamp.JOBCast(Intf);
end;

class function TJSnsIBrowserDOMWindow.JSClassName: UnicodeString;
begin
  Result:='nsIBrowserDOMWindow';
end;

class function TJSnsIBrowserDOMWindow.Cast(const Intf: IJSObject): IJSnsIBrowserDOMWindow;
begin
  Result:=TJSnsIBrowserDOMWindow.JOBCast(Intf);
end;

class function TJSXULControllers.JSClassName: UnicodeString;
begin
  Result:='XULControllers';
end;

class function TJSXULControllers.Cast(const Intf: IJSObject): IJSXULControllers;
begin
  Result:=TJSXULControllers.JOBCast(Intf);
end;

class function TJSnsIDOMWindowUtils.JSClassName: UnicodeString;
begin
  Result:='nsIDOMWindowUtils';
end;

class function TJSnsIDOMWindowUtils.Cast(const Intf: IJSObject): IJSnsIDOMWindowUtils;
begin
  Result:=TJSnsIDOMWindowUtils.JOBCast(Intf);
end;

class function TJSnsIPrintSettings.JSClassName: UnicodeString;
begin
  Result:='nsIPrintSettings';
end;

class function TJSnsIPrintSettings.Cast(const Intf: IJSObject): IJSnsIPrintSettings;
begin
  Result:=TJSnsIPrintSettings.JOBCast(Intf);
end;

function TJSWindow._GetdevicePixelRatio: Double;
begin
  Result:=ReadJSPropertyDouble('devicePixelRatio');
end;

function TJSWindow.requestAnimationFrame(const aCallback: TFrameRequestCallback): LongInt;
var
  m: TJOB_Method;
begin
  m:=TJOB_Method.Create(TMethod(aCallback),@JOBCallFrameRequestCallback);
  try
    Result:=InvokeJSLongIntResult('requestAnimationFrame',[m]);
  finally
    m.free;
  end;
end;

procedure TJSWindow.cancelAnimationFrame(aHandle: LongInt);
begin
  InvokeJSNoResult('cancelAnimationFrame',[aHandle]);
end;

class function TJSWindow.JSClassName: UnicodeString;
begin
  Result:='Window';
end;

class function TJSWindow.Cast(const Intf: IJSObject): IJSWindow;
begin
  Result:=TJSWindow.JOBCast(Intf);
end;

initialization
  JSDocument:=TJSDocument.JOBCreateGlobal('document');
  JSWindow:=TJSWindow.JOBCreateGlobal('window');
finalization
  JSDocument.Free;
  JSWindow.Free;
end.
