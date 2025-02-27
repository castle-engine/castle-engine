/* parts/AnimationFrameProvider.webidl ----------------------------------------------------- */
/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * https://html.spec.whatwg.org/multipage/imagebitmap-and-animations.html#animation-frames
 */

callback FrameRequestCallback = undefined (DOMHighResTimeStamp time);

interface mixin AnimationFrameProvider {
  [Throws] long requestAnimationFrame(FrameRequestCallback callback);
  [Throws] undefined cancelAnimationFrame(long handle);
};
/* parts/CSSStyleDeclaration.webidl ----------------------------------------------------- */
/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */

/* Castle Game Engine notes:
   This WEBIDL was cut down to our needs.
   This is not the original (complete) WEBIDL file,
   if you want a complete file get it from
   https://hg.mozilla.org/mozilla-central/raw-file/tip/dom/webidl/

   -------------------------------------------------------------------------------
*/

/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * http://dev.w3.org/csswg/cssom/
 */

 // Because of getComputedStyle, many CSSStyleDeclaration objects can be
 // short-living.
[ProbablyShortLivingWrapper,
 Exposed=Window]
interface CSSStyleDeclaration {
  [CEReactions, SetterNeedsSubjectPrincipal=NonSystem, SetterThrows]
  attribute UTF8String cssText;

  readonly attribute unsigned long length;
  getter UTF8String item(unsigned long index);

  [Throws, ChromeOnly]
  sequence<UTF8String> getCSSImageURLs(UTF8String property);

  [ChromeOnly]
  readonly attribute float usedFontSize;

  UTF8String getPropertyValue(UTF8String property);
  UTF8String getPropertyPriority(UTF8String property);
  [CEReactions, NeedsSubjectPrincipal=NonSystem, Throws]
  undefined setProperty(UTF8String property, [LegacyNullToEmptyString] UTF8String value, optional [LegacyNullToEmptyString] UTF8String priority = "");
  [CEReactions, Throws]
  UTF8String removeProperty(UTF8String property);

  //readonly attribute CSSRule? parentRule;
};
/* parts/Document.webidl ----------------------------------------------------- */
/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */

/* Castle Game Engine notes:
   This WEBIDL was cut down to our needs.
   This is not the original (complete) WEBIDL file,
   if you want a complete file get it from
   https://hg.mozilla.org/mozilla-central/raw-file/tip/dom/webidl/

   -------------------------------------------------------------------------------
*/

/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * https://dom.spec.whatwg.org/#interface-document
 * https://html.spec.whatwg.org/multipage/dom.html#the-document-object
 * https://html.spec.whatwg.org/multipage/obsolete.html#other-elements%2C-attributes-and-apis
 * https://fullscreen.spec.whatwg.org/#api
 * https://w3c.github.io/pointerlock/#extensions-to-the-document-interface
 * https://w3c.github.io/pointerlock/#extensions-to-the-documentorshadowroot-mixin
 * https://w3c.github.io/page-visibility/#extensions-to-the-document-interface
 * https://drafts.csswg.org/cssom/#extensions-to-the-document-interface
 * https://drafts.csswg.org/cssom-view/#extensions-to-the-document-interface
 * https://wicg.github.io/feature-policy/#policy
 * https://wicg.github.io/scroll-to-text-fragment/#feature-detectability
 */

interface ContentSecurityPolicy;
interface Principal;
interface WindowProxy;
interface nsISupports;
interface URI;
interface nsIDocShell;
interface nsILoadGroup;
interface nsIReferrerInfo;
interface nsICookieJarSettings;
interface nsIPermissionDelegateHandler;
interface XULCommandDispatcher;

enum VisibilityState { "hidden", "visible" };

/* https://dom.spec.whatwg.org/#interface-document */
[Exposed=Window,
 InstrumentedProps=(caretRangeFromPoint,
                    exitPictureInPicture,
                    featurePolicy,
                    onbeforecopy,
                    onbeforecut,
                    onbeforepaste,
                    oncancel,
                    onfreeze,
                    onmousewheel,
                    onresume,
                    onsearch,
                    onwebkitfullscreenchange,
                    onwebkitfullscreenerror,
                    pictureInPictureElement,
                    pictureInPictureEnabled,
                    registerElement,
                    wasDiscarded,
                    webkitCancelFullScreen,
                    webkitCurrentFullScreenElement,
                    webkitExitFullscreen,
                    webkitFullscreenElement,
                    webkitFullscreenEnabled,
                    webkitHidden,
                    webkitIsFullScreen,
                    webkitVisibilityState,
                    xmlEncoding,
                    xmlStandalone,
                    xmlVersion)]
interface Document : Node {
};

Document includes NonElementParentNode;
/* parts/DOMRect.webidl ----------------------------------------------------- */
/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * https://drafts.fxtf.org/geometry/
 *
 * Copyright © 2012 W3C® (MIT, ERCIM, Keio), All Rights Reserved. W3C
 * liability, trademark and document use rules apply.
 */

[Exposed=(Window,Worker),
 Serializable]
interface DOMRect : DOMRectReadOnly {
    constructor(optional unrestricted double x = 0,
                optional unrestricted double y = 0,
                optional unrestricted double width = 0,
                optional unrestricted double height = 0);

    [NewObject] static DOMRect fromRect(optional DOMRectInit other = {});

    inherit attribute unrestricted double x;
    inherit attribute unrestricted double y;
    inherit attribute unrestricted double width;
    inherit attribute unrestricted double height;
};

[ProbablyShortLivingWrapper,
 Exposed=(Window,Worker),
 Serializable]
interface DOMRectReadOnly {
    constructor(optional unrestricted double x = 0,
                optional unrestricted double y = 0,
                optional unrestricted double width = 0,
                optional unrestricted double height = 0);

    [NewObject] static DOMRectReadOnly fromRect(optional DOMRectInit other = {});

    readonly attribute unrestricted double x;
    readonly attribute unrestricted double y;
    readonly attribute unrestricted double width;
    readonly attribute unrestricted double height;
    readonly attribute unrestricted double top;
    readonly attribute unrestricted double right;
    readonly attribute unrestricted double bottom;
    readonly attribute unrestricted double left;

    [Default] object toJSON();
};

dictionary DOMRectInit {
    unrestricted double x = 0;
    unrestricted double y = 0;
    unrestricted double width = 0;
    unrestricted double height = 0;
};
/* parts/Element.webidl ----------------------------------------------------- */
/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */

/* Castle Game Engine notes:
   This WEBIDL was cut down to our needs.
   This is not the original (complete) WEBIDL file,
   if you want a complete file get it from
   https://hg.mozilla.org/mozilla-central/raw-file/tip/dom/webidl/

   -------------------------------------------------------------------------------
*/

/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * https://dom.spec.whatwg.org/#interface-element
 * https://domparsing.spec.whatwg.org/
 * https://drafts.csswg.org/cssom-view/
 *
 * Copyright © 2012 W3C® (MIT, ERCIM, Keio), All Rights Reserved. W3C
 * liability, trademark and document use rules apply.
 */

interface nsIScreen;

[Exposed=Window,
 InstrumentedProps=(computedStyleMap,onmousewheel,scrollIntoViewIfNeeded)]
interface Element : Node {
  [Constant]
  readonly attribute DOMString? namespaceURI;
  [Constant]
  readonly attribute DOMString? prefix;
  [Constant]
  readonly attribute DOMString localName;

  // Not [Constant] because it depends on which document we're in
  [Pure]
  readonly attribute DOMString tagName;

  [CEReactions, Pure]
           attribute DOMString id;
  [CEReactions, Pure]
           attribute DOMString className;
};

// https://drafts.csswg.org/cssom/#the-elementcssinlinestyle-mixin
interface mixin ElementCSSInlineStyle {
  [SameObject, PutForwards=cssText]
  readonly attribute CSSStyleDeclaration style;
};

HTMLElement includes ElementCSSInlineStyle;

// https://drafts.csswg.org/cssom-view/#extensions-to-the-element-interface
partial interface Element {
  //DOMRectList getClientRects();
  DOMRect getBoundingClientRect();

  readonly attribute long clientTop;
  readonly attribute long clientLeft;
  readonly attribute long clientWidth;
  readonly attribute long clientHeight;

  [Pref="layout.css.zoom.enabled"] readonly attribute double currentCSSZoom;
};

// http://domparsing.spec.whatwg.org/#extensions-to-the-element-interface
partial interface Element {
  [CEReactions, SetterNeedsSubjectPrincipal=NonSystem, Pure, SetterThrows, GetterCanOOM]
  attribute [LegacyNullToEmptyString] DOMString innerHTML;
  [CEReactions, Pure, SetterThrows]
  attribute [LegacyNullToEmptyString] DOMString outerHTML;
  [CEReactions, Throws]
  undefined insertAdjacentHTML(DOMString position, DOMString text);
};
/* parts/EventHandler.webidl ----------------------------------------------------- */
/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */

/* Castle Game Engine notes:
   This WEBIDL was cut down to our needs.
   This is not the original (complete) WEBIDL file,
   if you want a complete file get it from
   https://hg.mozilla.org/mozilla-central/raw-file/tip/dom/webidl/

   -------------------------------------------------------------------------------
*/

/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * http://www.whatwg.org/specs/web-apps/current-work/#eventhandler
 *
 * © Copyright 2004-2011 Apple Computer, Inc., Mozilla Foundation, and
 * Opera Software ASA. You are granted a license to use, reproduce
 * and create derivative works of this document.
 */
[LegacyTreatNonObjectAsNull]
callback EventHandlerNonNull = any (Event event);
typedef EventHandlerNonNull? EventHandler;

[LegacyTreatNonObjectAsNull]
callback OnBeforeUnloadEventHandlerNonNull = DOMString? (Event event);
typedef OnBeforeUnloadEventHandlerNonNull? OnBeforeUnloadEventHandler;

[LegacyTreatNonObjectAsNull]
callback OnErrorEventHandlerNonNull = any ((Event or DOMString) event, optional UTF8String source, optional unsigned long lineno, optional unsigned long column, optional any error);
typedef OnErrorEventHandlerNonNull? OnErrorEventHandler;

interface mixin GlobalEventHandlers {
           attribute EventHandler onabort;
           attribute EventHandler onblur;
// We think the spec is wrong here. See OnErrorEventHandlerForNodes/Window
// below.
//         attribute OnErrorEventHandler onerror;
           attribute EventHandler onfocus;
           attribute EventHandler oncancel;
           attribute EventHandler onauxclick;
           attribute EventHandler onbeforeinput;
           attribute EventHandler onbeforetoggle;
           attribute EventHandler oncanplay;
           attribute EventHandler oncanplaythrough;
           attribute EventHandler onchange;
           attribute EventHandler onclick;
           attribute EventHandler onclose;
           attribute EventHandler oncontentvisibilityautostatechange;
           attribute EventHandler oncontextlost;
           attribute EventHandler oncontextmenu;
           attribute EventHandler oncontextrestored;
           attribute EventHandler oncopy;
           attribute EventHandler oncuechange;
           attribute EventHandler oncut;
           attribute EventHandler ondblclick;
           attribute EventHandler ondrag;
           attribute EventHandler ondragend;
           attribute EventHandler ondragenter;
           [Func="Event::IsDragExitEnabled"]
           attribute EventHandler ondragexit;
           attribute EventHandler ondragleave;
           attribute EventHandler ondragover;
           attribute EventHandler ondragstart;
           attribute EventHandler ondrop;
           attribute EventHandler ondurationchange;
           attribute EventHandler onemptied;
           attribute EventHandler onended;
           attribute EventHandler onformdata;
           attribute EventHandler oninput;
           attribute EventHandler oninvalid;
           attribute EventHandler onkeydown;
           attribute EventHandler onkeypress;
           attribute EventHandler onkeyup;
           attribute EventHandler onload;
           attribute EventHandler onloadeddata;
           attribute EventHandler onloadedmetadata;
           attribute EventHandler onloadstart;
           attribute EventHandler onmousedown;
  [LegacyLenientThis] attribute EventHandler onmouseenter;
  [LegacyLenientThis] attribute EventHandler onmouseleave;
           attribute EventHandler onmousemove;
           attribute EventHandler onmouseout;
           attribute EventHandler onmouseover;
           attribute EventHandler onmouseup;
           attribute EventHandler onwheel;
           attribute EventHandler onpaste;
           attribute EventHandler onpause;
           attribute EventHandler onplay;
           attribute EventHandler onplaying;
           attribute EventHandler onprogress;
           attribute EventHandler onratechange;
           attribute EventHandler onreset;
           attribute EventHandler onresize;
           attribute EventHandler onscroll;
           attribute EventHandler onscrollend;
           attribute EventHandler onsecuritypolicyviolation;
           attribute EventHandler onseeked;
           attribute EventHandler onseeking;
           attribute EventHandler onselect;
           attribute EventHandler onslotchange;
           //(Not implemented)attribute EventHandler onsort;
           attribute EventHandler onstalled;
           attribute EventHandler onsubmit;
           attribute EventHandler onsuspend;
           attribute EventHandler ontimeupdate;
           attribute EventHandler onvolumechange;
           attribute EventHandler onwaiting;

           attribute EventHandler onselectstart;
           attribute EventHandler onselectionchange;

           attribute EventHandler ontoggle;

           // Pointer events handlers
           attribute EventHandler onpointercancel;
           attribute EventHandler onpointerdown;
           attribute EventHandler onpointerup;
           attribute EventHandler onpointermove;
           attribute EventHandler onpointerout;
           attribute EventHandler onpointerover;
           attribute EventHandler onpointerenter;
           attribute EventHandler onpointerleave;
           attribute EventHandler ongotpointercapture;
           attribute EventHandler onlostpointercapture;

           // Mozilla-specific handlers. Unprefixed handlers live in
           // Document rather than here.
           [Deprecated="MozfullscreenchangeDeprecatedPrefix"]
           attribute EventHandler onmozfullscreenchange;
           [Deprecated="MozfullscreenerrorDeprecatedPrefix"]
           attribute EventHandler onmozfullscreenerror;

           // CSS-Animation and CSS-Transition handlers.
           attribute EventHandler onanimationcancel;
           attribute EventHandler onanimationend;
           attribute EventHandler onanimationiteration;
           attribute EventHandler onanimationstart;
           attribute EventHandler ontransitioncancel;
           attribute EventHandler ontransitionend;
           attribute EventHandler ontransitionrun;
           attribute EventHandler ontransitionstart;

           // CSS-Animation and CSS-Transition legacy handlers.
           // This handler isn't standard.
           [BinaryName="onwebkitAnimationEnd"]
           attribute EventHandler onwebkitanimationend;
           [BinaryName="onwebkitAnimationIteration"]
           attribute EventHandler onwebkitanimationiteration;
           [BinaryName="onwebkitAnimationStart"]
           attribute EventHandler onwebkitanimationstart;
           [BinaryName="onwebkitTransitionEnd"]
           attribute EventHandler onwebkittransitionend;
};

interface mixin WindowEventHandlers {
           attribute EventHandler onafterprint;
           attribute EventHandler onbeforeprint;
           attribute OnBeforeUnloadEventHandler onbeforeunload;
           attribute EventHandler onhashchange;
           attribute EventHandler onlanguagechange;
           attribute EventHandler onmessage;
           attribute EventHandler onmessageerror;
           attribute EventHandler onoffline;
           attribute EventHandler ononline;
           attribute EventHandler onpagehide;
           attribute EventHandler onpageshow;
           attribute EventHandler onpopstate;
           attribute EventHandler onrejectionhandled;
           attribute EventHandler onstorage;
           attribute EventHandler onunhandledrejection;
           attribute EventHandler onunload;
};

// https://w3c.github.io/gamepad/#extensions-to-the-windoweventhandlers-interface-mixin
partial interface mixin WindowEventHandlers {
  attribute EventHandler ongamepadconnected;
  attribute EventHandler ongamepaddisconnected;
};

// The spec has |attribute OnErrorEventHandler onerror;| on
// GlobalEventHandlers, and calls the handler differently depending on
// whether an ErrorEvent was fired. We don't do that, and until we do we'll
// need to distinguish between onerror on Window or on nodes.

interface mixin OnErrorEventHandlerForNodes {
           attribute EventHandler onerror;
};

interface mixin OnErrorEventHandlerForWindow {
           attribute OnErrorEventHandler onerror;
};
/* parts/EventListener.webidl ----------------------------------------------------- */
/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */

/* Castle Game Engine notes:
   This WEBIDL was cut down to our needs.
   This is not the original (complete) WEBIDL file,
   if you want a complete file get it from
   https://hg.mozilla.org/mozilla-central/raw-file/tip/dom/webidl/

   -------------------------------------------------------------------------------
*/

/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * http://www.w3.org/TR/2012/WD-dom-20120105/
 *
 * Copyright © 2012 W3C® (MIT, ERCIM, Keio), All Rights Reserved. W3C
 * liability, trademark and document use rules apply.
 */

[Exposed=Window]
// Michalis: We edit it just like Mattias did for FPC Job_Web,
// to have in Pascal TEventListener as a function that returns Boolean.
//
// Mattias:
//callback interface EventListener {
//  void handleEvent(Event event);
//};
callback EventListener = boolean (Event event);/* parts/EventTarget.webidl ----------------------------------------------------- */
/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */

/* Castle Game Engine notes:
   This WEBIDL was cut down to our needs.
   This is not the original (complete) WEBIDL file,
   if you want a complete file get it from
   https://hg.mozilla.org/mozilla-central/raw-file/tip/dom/webidl/

   -------------------------------------------------------------------------------
*/

/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * http://www.w3.org/TR/2012/WD-dom-20120105/
 *
 * Copyright © 2012 W3C® (MIT, ERCIM, Keio), All Rights Reserved. W3C
 * liability, trademark and document use rules apply.
 */


dictionary EventListenerOptions {
  boolean capture = false;
  /* Setting to true make the listener be added to the system group. */
  [Func="ThreadSafeIsChromeOrUAWidget"]
  boolean mozSystemGroup = false;
};

dictionary AddEventListenerOptions : EventListenerOptions {
  boolean passive;
  boolean once = false;
  //AbortSignal signal;
  [ChromeOnly]
  boolean wantUntrusted;
};

[Exposed=*]
interface EventTarget {
  [Throws]
  constructor();

  /* Passing null for wantsUntrusted means "default behavior", which
     differs in content and chrome.  In content that default boolean
     value is true, while in chrome the default boolean value is
     false. */
  undefined addEventListener(DOMString type,
                             EventListener? listener,
                             optional (AddEventListenerOptions or boolean) options = {},
                             optional boolean? wantsUntrusted = null);
  undefined removeEventListener(DOMString type,
                                EventListener? listener,
                                optional (EventListenerOptions or boolean) options = {});
  [Throws, NeedsCallerType]
  boolean dispatchEvent(Event event);
};

// Mozilla extensions for use by JS-implemented event targets to
// implement on* properties.
partial interface EventTarget {
  // The use of [TreatNonCallableAsNull] here is a bit of a hack: it just makes
  // the codegen check whether the type involved is either
  // [TreatNonCallableAsNull] or [TreatNonObjectAsNull] and if it is handle it
  // accordingly.  In particular, it will NOT actually treat a non-null
  // non-callable object as null here.
  [ChromeOnly, Throws]
  undefined setEventHandler(DOMString type,
                            [TreatNonCallableAsNull] EventHandler handler);

  [ChromeOnly]
  EventHandler getEventHandler(DOMString type);
};

// Mozilla extension to make firing events on event targets from
// chrome easier.  This returns the window which can be used to create
// events to fire at this EventTarget, or null if there isn't one.
partial interface EventTarget {
  [ChromeOnly, Exposed=Window, BinaryName="ownerGlobalForBindings"]
  readonly attribute WindowProxy? ownerGlobal;
};
/* parts/Event.webidl ----------------------------------------------------- */
/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */

/* Castle Game Engine notes:
   This WEBIDL was cut down to our needs.
   This is not the original (complete) WEBIDL file,
   if you want a complete file get it from
   https://hg.mozilla.org/mozilla-central/raw-file/tip/dom/webidl/

   -------------------------------------------------------------------------------
*/

/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * http://www.w3.org/TR/2012/WD-dom-20120105/
 *
 * Copyright © 2012 W3C® (MIT, ERCIM, Keio), All Rights Reserved. W3C
 * liability, trademark and document use rules apply.
 */

[Exposed=(Window,Worker,AudioWorklet), ProbablyShortLivingWrapper]
interface Event {
  constructor(DOMString type, optional EventInit eventInitDict = {});

  [Pure]
  readonly attribute DOMString type;
  [Pure, BindingAlias="srcElement"]
  readonly attribute EventTarget? target;
  [Pure]
  readonly attribute EventTarget? currentTarget;

  sequence<EventTarget> composedPath();

  const unsigned short NONE = 0;
  const unsigned short CAPTURING_PHASE = 1;
  const unsigned short AT_TARGET = 2;
  const unsigned short BUBBLING_PHASE = 3;
  [Pure]
  readonly attribute unsigned short eventPhase;

  undefined stopPropagation();
  undefined stopImmediatePropagation();

  [Pure]
  readonly attribute boolean bubbles;
  [Pure]
  readonly attribute boolean cancelable;
  [NeedsCallerType]
  attribute boolean returnValue;
  [NeedsCallerType]
  undefined preventDefault();
  [Pure, NeedsCallerType]
  readonly attribute boolean defaultPrevented;
  [ChromeOnly, Pure]
  readonly attribute boolean defaultPreventedByChrome;
  [ChromeOnly, Pure]
  readonly attribute boolean defaultPreventedByContent;
  [Pure]
  readonly attribute boolean composed;

  [LegacyUnforgeable, Pure]
  readonly attribute boolean isTrusted;
  [Pure]
  readonly attribute DOMHighResTimeStamp timeStamp;

  undefined initEvent(DOMString type,
                      optional boolean bubbles = false,
                      optional boolean cancelable = false);
  attribute boolean cancelBubble;
};

// Mozilla specific legacy stuff.
partial interface Event {
  const long ALT_MASK     = 0x00000001;
  const long CONTROL_MASK = 0x00000002;
  const long SHIFT_MASK   = 0x00000004;
  const long META_MASK    = 0x00000008;

  /** The original target of the event, before any retargetings. */
  readonly attribute EventTarget? originalTarget;
  /**
   * The explicit original target of the event.  If the event was retargeted
   * for some reason other than an anonymous boundary crossing, this will be set
   * to the target before the retargeting occurs.  For example, mouse events
   * are retargeted to their parent node when they happen over text nodes (bug
   * 185889), and in that case .target will show the parent and
   * .explicitOriginalTarget will show the text node.
   * .explicitOriginalTarget differs from .originalTarget in that it will never
   * contain anonymous content.
   */
  readonly attribute EventTarget? explicitOriginalTarget;
  [ChromeOnly] readonly attribute EventTarget? composedTarget;
  [ChromeOnly] undefined preventMultipleActions();
  [ChromeOnly] readonly attribute boolean multipleActionsPrevented;
  [ChromeOnly] readonly attribute boolean isSynthesized;
  /**
   * When the event target is a remote browser, calling this will fire an
   * reply event in the chrome process.
   */
  [ChromeOnly] undefined requestReplyFromRemoteContent();
  /**
   * Returns true when the event shouldn't be handled by chrome.
   */
  [ChromeOnly] readonly attribute boolean isWaitingReplyFromRemoteContent;
  /**
   * Returns true when the event is a reply event from a remote process.
   */
  [ChromeOnly] readonly attribute boolean isReplyEventFromRemoteContent;
};

dictionary EventInit {
  boolean bubbles = false;
  boolean cancelable = false;
  boolean composed = false;
};
/* parts/HTMLCanvasElement.webidl ----------------------------------------------------- */
/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */

/* Castle Game Engine notes:
   This WEBIDL was cut down to our needs.
   This is not the original (complete) WEBIDL file,
   if you want a complete file get it from
   https://hg.mozilla.org/mozilla-central/raw-file/tip/dom/webidl/

   -------------------------------------------------------------------------------
*/

/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * http://www.whatwg.org/specs/web-apps/current-work/#the-canvas-element
 * © Copyright 2004-2011 Apple Computer, Inc., Mozilla Foundation, and
 * Opera Software ASA. You are granted a license to use, reproduce
 * and create derivative works of this document.
 */

interface nsISupports;

typedef (HTMLCanvasElement or OffscreenCanvas) CanvasSource;

[Exposed=Window]
interface HTMLCanvasElement : HTMLElement {
  [HTMLConstructor] constructor();

  [CEReactions, Pure, SetterThrows]
           attribute unsigned long width;
  [CEReactions, Pure, SetterThrows]
           attribute unsigned long height;

  [Throws]
  nsISupports? getContext(DOMString contextId, optional any contextOptions = null);

  [Throws, NeedsSubjectPrincipal]
  DOMString toDataURL(optional DOMString type = "",
                      optional any encoderOptions);
  //[Throws, NeedsSubjectPrincipal]
  //undefined toBlob(BlobCallback callback,
  //                 optional DOMString type = "",
  //                 optional any encoderOptions);
};


/* parts/HTMLDivElement.webidl ----------------------------------------------------- */
/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */

/* Castle Game Engine notes:
   This WEBIDL was cut down to our needs.
   This is not the original (complete) WEBIDL file,
   if you want a complete file get it from
   https://hg.mozilla.org/mozilla-central/raw-file/tip/dom/webidl/

   -------------------------------------------------------------------------------
*/

/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * http://www.whatwg.org/specs/web-apps/current-work/
 *
 * © Copyright 2004-2011 Apple Computer, Inc., Mozilla Foundation, and
 * Opera Software ASA. You are granted a license to use, reproduce
 * and create derivative works of this document.
 */

[Exposed=Window, ProbablyShortLivingWrapper]
interface HTMLDivElement : HTMLElement {
  [HTMLConstructor] constructor();
};

partial interface HTMLDivElement {
  [CEReactions, SetterThrows]
           attribute DOMString align;
};
/* parts/HTMLElement.webidl ----------------------------------------------------- */
/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */

/* Castle Game Engine notes:
   This WEBIDL was cut down to our needs.
   This is not the original (complete) WEBIDL file,
   if you want a complete file get it from
   https://hg.mozilla.org/mozilla-central/raw-file/tip/dom/webidl/

   -------------------------------------------------------------------------------
*/

/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * http://www.whatwg.org/specs/web-apps/current-work/ and
 * http://dev.w3.org/csswg/cssom-view/
 *
 * © Copyright 2004-2011 Apple Computer, Inc., Mozilla Foundation, and
 * Opera Software ASA. You are granted a license to use, reproduce
 * and create derivative works of this document.
 */
[Exposed=Window, InstrumentedProps=(attributeStyleMap)]
interface HTMLElement : Element {
  [HTMLConstructor] constructor();

  // metadata attributes
  [CEReactions]
           attribute DOMString title;
  [CEReactions]
           attribute DOMString lang;
  [CEReactions, SetterThrows, Pure]
           attribute boolean translate;
  [CEReactions, SetterThrows, Pure]
           attribute DOMString dir;

  [CEReactions, GetterThrows, Pure]
           attribute [LegacyNullToEmptyString] DOMString innerText;
  [CEReactions, GetterThrows, SetterThrows, Pure]
           attribute [LegacyNullToEmptyString] DOMString outerText;

  // user interaction
  [CEReactions, SetterThrows, Pure]
           attribute boolean hidden;
  [CEReactions, SetterThrows, Pure]
           attribute boolean inert;
  [NeedsCallerType]
  undefined click();
  [CEReactions, SetterThrows, Pure]
           attribute DOMString accessKey;
  [Pure]
  readonly attribute DOMString accessKeyLabel;
  [CEReactions, SetterThrows, Pure]
           attribute boolean draggable;
  //[PutForwards=value] readonly attribute DOMTokenList dropzone;
  [CEReactions, SetterThrows, Pure]
           attribute DOMString contentEditable;
  [Pure]
  readonly attribute boolean isContentEditable;
  [CEReactions, SetterThrows, Pure]
           attribute DOMString? popover;
  [CEReactions, SetterThrows, Pure]
           attribute boolean spellcheck;
  [CEReactions, Pure, SetterThrows]
           attribute DOMString inputMode;
  [CEReactions, Pure, SetterThrows]
           attribute DOMString enterKeyHint;
  [CEReactions, Pure, SetterThrows]
           attribute DOMString autocapitalize;
  [CEReactions, Pure, SetterThrows, Pref="dom.forms.autocorrect"]
           attribute boolean autocorrect;

  attribute DOMString nonce;

  // command API
  //readonly attribute DOMString? commandType;
  //readonly attribute DOMString? commandLabel;
  //readonly attribute DOMString? commandIcon;
  //readonly attribute boolean? commandHidden;
  //readonly attribute boolean? commandDisabled;
  //readonly attribute boolean? commandChecked;
};

// http://dev.w3.org/csswg/cssom-view/#extensions-to-the-htmlelement-interface
partial interface HTMLElement {
  // CSSOM things are not [Pure] because they can flush
  readonly attribute Element? offsetParent;
  readonly attribute long offsetTop;
  readonly attribute long offsetLeft;
  readonly attribute long offsetWidth;
  readonly attribute long offsetHeight;
};
/* parts/HTMLImageElement.webidl ----------------------------------------------------- */
/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */

/* Castle Game Engine notes:
   This WEBIDL was cut down to our needs.
   This is not the original (complete) WEBIDL file,
   if you want a complete file get it from
   https://hg.mozilla.org/mozilla-central/raw-file/tip/dom/webidl/

   -------------------------------------------------------------------------------
*/

/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * http://www.whatwg.org/specs/web-apps/current-work/#htmlimageelement
 * http://www.whatwg.org/specs/web-apps/current-work/#other-elements,-attributes-and-apis
 * © Copyright 2004-2011 Apple Computer, Inc., Mozilla Foundation, and
 * Opera Software ASA. You are granted a license to use, reproduce
 * and create derivative works of this document.
 */

interface imgINotificationObserver;
interface imgIRequest;
interface URI;
interface nsIStreamListener;

[LegacyFactoryFunction=Image(optional unsigned long width, optional unsigned long height),
 Exposed=Window]
interface HTMLImageElement : HTMLElement {
  [HTMLConstructor] constructor();

           [CEReactions, SetterThrows]
           attribute DOMString alt;
           [CEReactions, SetterNeedsSubjectPrincipal=NonSystem, SetterThrows]
           attribute DOMString src;
           [CEReactions, SetterNeedsSubjectPrincipal=NonSystem, SetterThrows]
           attribute DOMString srcset;
           [CEReactions, SetterThrows]
           attribute DOMString? crossOrigin;
           [CEReactions, SetterThrows]
           attribute DOMString useMap;
           [CEReactions, SetterThrows]
           attribute DOMString referrerPolicy;
           [CEReactions, SetterThrows]
           attribute boolean isMap;
           [CEReactions, SetterThrows]
           attribute unsigned long width;
           [CEReactions, SetterThrows]
           attribute unsigned long height;
           [CEReactions, SetterThrows]
           attribute DOMString decoding;
           [CEReactions, SetterThrows]
           attribute DOMString loading;
           [Pref="network.fetchpriority.enabled", CEReactions]
           attribute DOMString fetchPriority;
  readonly attribute unsigned long naturalWidth;
  readonly attribute unsigned long naturalHeight;
  readonly attribute boolean complete;
           [NewObject]
           Promise<undefined> decode();
           [NewObject, ChromeOnly]
           Promise<sequence<ImageText>> recognizeCurrentImageText();
};

// http://www.whatwg.org/specs/web-apps/current-work/#other-elements,-attributes-and-apis
partial interface HTMLImageElement {
           [CEReactions, SetterThrows]
           attribute DOMString name;
           [CEReactions, SetterThrows]
           attribute DOMString align;
           [CEReactions, SetterThrows]
           attribute unsigned long hspace;
           [CEReactions, SetterThrows]
           attribute unsigned long vspace;
           [CEReactions, SetterThrows]
           attribute DOMString longDesc;

  [CEReactions, SetterThrows] attribute [LegacyNullToEmptyString] DOMString border;
};

// [Update me: not in whatwg spec yet]
// http://picture.responsiveimages.org/#the-img-element
partial interface HTMLImageElement {
           [CEReactions, SetterThrows]
           attribute DOMString sizes;
  readonly attribute DOMString currentSrc;
};

// Mozilla extensions.
partial interface HTMLImageElement {
           [CEReactions, SetterThrows]
           attribute DOMString lowsrc;

  // These attributes are offsets from the closest view (to mimic
  // NS4's "offset-from-layer" behavior).
  readonly attribute long x;
  readonly attribute long y;
};

interface mixin MozImageLoadingContent {
  // Mirrored chrome-only nsIImageLoadingContent methods.  Please make sure
  // to update this list if nsIImageLoadingContent changes.
  [ChromeOnly]
  const long UNKNOWN_REQUEST = -1;
  [ChromeOnly]
  const long CURRENT_REQUEST = 0;
  [ChromeOnly]
  const long PENDING_REQUEST = 1;

  [ChromeOnly]
  attribute boolean loadingEnabled;
  /**
   * Same as addNativeObserver but intended for scripted observers or observers
   * from another or without a document.
   */
  [ChromeOnly]
  undefined addObserver(imgINotificationObserver aObserver);
  /**
   * Same as removeNativeObserver but intended for scripted observers or
   * observers from another or without a document.
   */
  [ChromeOnly]
  undefined removeObserver(imgINotificationObserver aObserver);
  [ChromeOnly,Throws]
  imgIRequest? getRequest(long aRequestType);
  [ChromeOnly,Throws]
  long getRequestType(imgIRequest aRequest);
  [ChromeOnly]
  readonly attribute URI? currentURI;
  // Gets the final URI of the current request, if available.
  // Otherwise, returns null.
  [ChromeOnly]
  readonly attribute URI? currentRequestFinalURI;
  /**
   * forceReload forces reloading of the image pointed to by currentURI
   *
   * @param aNotify request should notify
   * @throws NS_ERROR_NOT_AVAILABLE if there is no current URI to reload
   */
  [ChromeOnly,Throws]
  undefined forceReload(optional boolean aNotify = true);
};

HTMLImageElement includes MozImageLoadingContent;
/* parts/HTMLMediaElement.webidl ----------------------------------------------------- */
/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */

/* Castle Game Engine notes:
   This WEBIDL was cut down to our needs.
   This is not the original (complete) WEBIDL file,
   if you want a complete file get it from
   https://hg.mozilla.org/mozilla-central/raw-file/tip/dom/webidl/

   -------------------------------------------------------------------------------
*/

/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * http://www.whatwg.org/specs/web-apps/current-work/#media-elements
 *
 * © Copyright 2004-2011 Apple Computer, Inc., Mozilla Foundation, and
 * Opera Software ASA. You are granted a license to use, reproduce
 * and create derivative works of this document.
 */

[Exposed=Window,
 InstrumentedProps=(disableRemotePlayback,remote)]
interface HTMLMediaElement : HTMLElement {

  // network state
  [CEReactions, SetterNeedsSubjectPrincipal=NonSystem, SetterThrows]
           attribute DOMString src;
  readonly attribute DOMString currentSrc;

  [CEReactions, SetterThrows]
           attribute DOMString? crossOrigin;
  const unsigned short NETWORK_EMPTY = 0;
  const unsigned short NETWORK_IDLE = 1;
  const unsigned short NETWORK_LOADING = 2;
  const unsigned short NETWORK_NO_SOURCE = 3;
  readonly attribute unsigned short networkState;
  [CEReactions, SetterThrows]
           attribute DOMString preload;
};
/* parts/HTMLSpanElement.webidl ----------------------------------------------------- */
/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * http://www.whatwg.org/specs/web-apps/current-work/#the-span-element
 *
 * © Copyright 2004-2011 Apple Computer, Inc., Mozilla Foundation, and
 * Opera Software ASA. You are granted a license to use, reproduce
 * and create derivative works of this document.
 */

// http://www.whatwg.org/specs/web-apps/current-work/#the-span-element
[Exposed=Window, ProbablyShortLivingWrapper]
interface HTMLSpanElement : HTMLElement {
  [HTMLConstructor] constructor();
};
/* parts/HTMLVideoElement.webidl ----------------------------------------------------- */
/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */

/* Castle Game Engine notes:
   This WEBIDL was cut down to our needs.
   This is not the original (complete) WEBIDL file,
   if you want a complete file get it from
   https://hg.mozilla.org/mozilla-central/raw-file/tip/dom/webidl/

   -------------------------------------------------------------------------------
*/

/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * https://html.spec.whatwg.org/multipage/media.html#the-video-element
 * https://wicg.github.io/video-rvfc/
 *
 * © Copyright 2004-2011 Apple Computer, Inc., Mozilla Foundation, and
 * Opera Software ASA. You are granted a license to use, reproduce
 * and create derivative works of this document.
 */

interface HTMLVideoElement : HTMLMediaElement {
  [HTMLConstructor] constructor();

  [CEReactions, SetterThrows]
           attribute unsigned long width;
  [CEReactions, SetterThrows]
           attribute unsigned long height;
  readonly attribute unsigned long videoWidth;
  readonly attribute unsigned long videoHeight;
  [CEReactions, SetterThrows]
           attribute DOMString poster;
};
/* parts/ImageBitmap.webidl ----------------------------------------------------- */
/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */

/* Castle Game Engine notes:
   This WEBIDL was cut down to our needs.
   This is not the original (complete) WEBIDL file,
   if you want a complete file get it from
   https://hg.mozilla.org/mozilla-central/raw-file/tip/dom/webidl/

   -------------------------------------------------------------------------------
*/

/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * https://html.spec.whatwg.org/multipage/webappapis.html#images
 *
 * The origin of the extended IDL file is
 * http://w3c.github.io/mediacapture-worker/#imagebitmap-extensions
 */

[Exposed=(Window,Worker)]
interface ImageBitmap {
  [Constant]
  readonly attribute unsigned long width;
  [Constant]
  readonly attribute unsigned long height;
};
/* parts/ImageData.webidl ----------------------------------------------------- */
/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */

/* Castle Game Engine notes:
   This WEBIDL was cut down to our needs.
   This is not the original (complete) WEBIDL file,
   if you want a complete file get it from
   https://hg.mozilla.org/mozilla-central/raw-file/tip/dom/webidl/

   -------------------------------------------------------------------------------
*/

/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * http://www.whatwg.org/specs/web-apps/current-work/multipage/the-canvas-element.html#imagedata
 *
 * © Copyright 2004-2011 Apple Computer, Inc., Mozilla Foundation, and Opera Software ASA.
 * You are granted a license to use, reproduce and create derivative works of this document.
 */

[Exposed=(Window,Worker),
 Serializable,
 InstrumentedProps=(colorSpace)]
interface ImageData {
 [Throws]
 constructor(unsigned long sw, unsigned long sh);
 [Throws]
 constructor(Uint8ClampedArray data, unsigned long sw,
             optional unsigned long sh);

 [Constant]
 readonly attribute unsigned long width;
 [Constant]
 readonly attribute unsigned long height;
 [Constant, StoreInSlot]
 readonly attribute Uint8ClampedArray data;
};
/* parts/KeyboardEvent.webidl ----------------------------------------------------- */
/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 */

[Exposed=Window]
interface KeyboardEvent : UIEvent
{
  [BinaryName="constructorJS"]
  constructor(DOMString typeArg,
              optional KeyboardEventInit keyboardEventInitDict= {});

  [NeedsCallerType]
  readonly attribute unsigned long    charCode;
  [NeedsCallerType]
  readonly attribute unsigned long    keyCode;

  [NeedsCallerType]
  readonly attribute boolean          altKey;
  [NeedsCallerType]
  readonly attribute boolean          ctrlKey;
  [NeedsCallerType]
  readonly attribute boolean          shiftKey;
  readonly attribute boolean          metaKey;

  [NeedsCallerType]
  boolean getModifierState(DOMString key);

  const unsigned long DOM_KEY_LOCATION_STANDARD = 0x00;
  const unsigned long DOM_KEY_LOCATION_LEFT     = 0x01;
  const unsigned long DOM_KEY_LOCATION_RIGHT    = 0x02;
  const unsigned long DOM_KEY_LOCATION_NUMPAD   = 0x03;

  readonly attribute unsigned long location;
  readonly attribute boolean       repeat;
  readonly attribute boolean       isComposing;

  readonly attribute DOMString key;
  [NeedsCallerType]
  readonly attribute DOMString code;

  [BinaryName="initKeyboardEventJS"]
  undefined initKeyboardEvent(DOMString typeArg,
                              optional boolean bubblesArg = false,
                              optional boolean cancelableArg = false,
                              optional Window? viewArg = null,
                              optional DOMString keyArg = "",
                              optional unsigned long locationArg = 0,
                              optional boolean ctrlKey = false,
                              optional boolean altKey = false,
                              optional boolean shiftKey = false,
                              optional boolean metaKey = false);

  // This returns the initialized dictionary for generating a
  // same-type keyboard event
  [Cached, ChromeOnly, Constant]
  readonly attribute KeyboardEventInit initDict;
};

dictionary KeyboardEventInit : EventModifierInit
{
  [BinaryType="nsAutoString"]
  DOMString      key           = "";
  [BinaryType="nsAutoString"]
  DOMString      code          = "";
  unsigned long  location      = 0;
  boolean        repeat        = false;
  boolean        isComposing   = false;

  // legacy attributes
  unsigned long  charCode      = 0;
  unsigned long  keyCode       = 0;
  unsigned long  which         = 0;
};

// Mozilla extensions
// Michalis-
//KeyboardEvent includes KeyEventMixin;
/* parts/MouseEvent.webidl ----------------------------------------------------- */
/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * For more information on this interface please see
 * http://dev.w3.org/2006/webapi/DOM-Level-3-Events/html/DOM3-Events.html
 * https://drafts.csswg.org/cssom-view/#extensions-to-the-mouseevent-interface
 *
 * Copyright © 2012 W3C® (MIT, ERCIM, Keio), All Rights Reserved. W3C
 * liability, trademark and document use rules apply.
 */

interface nsIScreen;

[Exposed=Window]
interface MouseEvent : UIEvent {
  constructor(DOMString typeArg,
              optional MouseEventInit mouseEventInitDict = {});

  [NeedsCallerType]
  readonly attribute double         screenX;
  [NeedsCallerType]
  readonly attribute double         screenY;

  [ChromeOnly]
  readonly attribute nsIScreen?     screen;

  readonly attribute double         pageX;
  readonly attribute double         pageY;
  readonly attribute double         clientX;
  readonly attribute double         clientY;
  [BinaryName="clientX"]
  readonly attribute double         x;
  [BinaryName="clientY"]
  readonly attribute double         y;
  readonly attribute double         offsetX;
  readonly attribute double         offsetY;
  readonly attribute boolean        ctrlKey;
  readonly attribute boolean        shiftKey;
  readonly attribute boolean        altKey;
  readonly attribute boolean        metaKey;
  readonly attribute short          button;
  readonly attribute unsigned short buttons;
  readonly attribute EventTarget?   relatedTarget;

  // Pointer Lock
  readonly attribute long           movementX;
  readonly attribute long           movementY;

  // Deprecated in DOM Level 3:
  [Deprecated="InitMouseEvent"]
  undefined initMouseEvent(DOMString typeArg,
                         optional boolean canBubbleArg = false,
                         optional boolean cancelableArg = false,
                         optional Window? viewArg = null,
                         optional long detailArg = 0,
                         optional long screenXArg = 0,
                         optional long screenYArg = 0,
                         optional long clientXArg = 0,
                         optional long clientYArg = 0,
                         optional boolean ctrlKeyArg = false,
                         optional boolean altKeyArg = false,
                         optional boolean shiftKeyArg = false,
                         optional boolean metaKeyArg = false,
                         optional short buttonArg = 0,
                         optional EventTarget? relatedTargetArg = null);
  // Introduced in DOM Level 3:
  boolean                           getModifierState(DOMString keyArg);
};

// Suggested initMouseEvent replacement initializer:
dictionary MouseEventInit : EventModifierInit {
  // Attributes for MouseEvent:
  double         screenX       = 0.0;
  double         screenY       = 0.0;
  double         clientX       = 0.0;
  double         clientY       = 0.0;
  short          button        = 0;
  // Note: "buttons" was not previously initializable through initMouseEvent!
  unsigned short buttons       = 0;
  EventTarget?   relatedTarget = null;

  // Pointer Lock
  long           movementX = 0;
  long           movementY = 0;
};

// Mozilla extensions
partial interface MouseEvent
{
  // Finger or touch pressure event value
  // ranges between 0.0 and 1.0
  // TODO: Remove mozPressure. (bug 1534199)
  [NeedsCallerType, Deprecated="MouseEvent_MozPressure"]
  readonly attribute float mozPressure;

  const unsigned short    MOZ_SOURCE_UNKNOWN    = 0;
  const unsigned short    MOZ_SOURCE_MOUSE      = 1;
  const unsigned short    MOZ_SOURCE_PEN        = 2;
  const unsigned short    MOZ_SOURCE_ERASER     = 3;
  const unsigned short    MOZ_SOURCE_CURSOR     = 4;
  const unsigned short    MOZ_SOURCE_TOUCH      = 5;
  const unsigned short    MOZ_SOURCE_KEYBOARD   = 6;

  [NeedsCallerType, ChromeOnly]
  readonly attribute unsigned short inputSource;

  [NeedsCallerType, Deprecated="MozInputSource", BinaryName="inputSource"]
  readonly attribute unsigned short mozInputSource;

  // TODO: Remove initNSMouseEvent. (bug 1165213)
  [Deprecated="InitNSMouseEvent"]
  undefined initNSMouseEvent(DOMString typeArg,
                             optional boolean canBubbleArg = false,
                             optional boolean cancelableArg = false,
                             optional Window? viewArg = null,
                             optional long detailArg = 0,
                             optional long screenXArg = 0,
                             optional long screenYArg = 0,
                             optional long clientXArg = 0,
                             optional long clientYArg = 0,
                             optional boolean ctrlKeyArg = false,
                             optional boolean altKeyArg = false,
                             optional boolean shiftKeyArg = false,
                             optional boolean metaKeyArg = false,
                             optional short buttonArg = 0,
                             optional EventTarget? relatedTargetArg = null,
                             optional float pressure = 0,
                             optional unsigned short inputSourceArg = 0);

  /**
   * preventClickEvent() prevents the following "click", "auxclick" and
   * "dblclick" events of "mousedown" and "mouseup" events.
   */
  [ChromeOnly]
  undefined preventClickEvent();

  /**
   * Returns true if the following "click", "auxclick" and "dblclick"
   * events of "mousedown" and "mouseup" events are prevented.
   */
  [ChromeOnly]
  boolean clickEventPrevented();
};
/* parts/Node.webidl ----------------------------------------------------- */
/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */

/* Castle Game Engine notes:
   This WEBIDL was cut down to our needs.
   This is not the original (complete) WEBIDL file,
   if you want a complete file get it from
   https://hg.mozilla.org/mozilla-central/raw-file/tip/dom/webidl/

   -------------------------------------------------------------------------------
*/

/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * http://www.w3.org/TR/2012/WD-dom-20120105/
 *
 * Copyright © 2012 W3C® (MIT, ERCIM, Keio), All Rights Reserved. W3C
 * liability, trademark and document use rules apply.
 */

interface Principal;
interface URI;

[Exposed=Window]
interface Node : EventTarget {
  const unsigned short ELEMENT_NODE = 1;
  const unsigned short ATTRIBUTE_NODE = 2; // historical
  const unsigned short TEXT_NODE = 3;
  const unsigned short CDATA_SECTION_NODE = 4; // historical
  const unsigned short ENTITY_REFERENCE_NODE = 5; // historical
  const unsigned short ENTITY_NODE = 6; // historical
  const unsigned short PROCESSING_INSTRUCTION_NODE = 7;
  const unsigned short COMMENT_NODE = 8;
  const unsigned short DOCUMENT_NODE = 9;
  const unsigned short DOCUMENT_TYPE_NODE = 10;
  const unsigned short DOCUMENT_FRAGMENT_NODE = 11;
  const unsigned short NOTATION_NODE = 12; // historical
  [Constant]
  readonly attribute unsigned short nodeType;
  [Pure]
  readonly attribute DOMString nodeName;

  [Pure, Throws, NeedsCallerType, BinaryName="baseURIFromJS"]
  readonly attribute DOMString? baseURI;

  [Pure, BinaryName=isInComposedDoc]
  readonly attribute boolean isConnected;
  [Pure]
  readonly attribute Document? ownerDocument;
  [Pure]
  readonly attribute Node? parentNode;
  [Pure]
  readonly attribute Element? parentElement;
  [Pure]
  boolean hasChildNodes();
  [Pure]
  readonly attribute Node? firstChild;
  [Pure]
  readonly attribute Node? lastChild;
  [Pure]
  readonly attribute Node? previousSibling;
  [Pure]
  readonly attribute Node? nextSibling;

  [CEReactions, SetterThrows, Pure]
           attribute DOMString? nodeValue;
  [CEReactions, SetterThrows, GetterCanOOM,
   SetterNeedsSubjectPrincipal=NonSystem, Pure]
           attribute DOMString? textContent;
  // These DOM methods cannot be accessed by UA Widget scripts
  // because the DOM element reflectors will be in the content scope,
  // instead of the desired UA Widget scope.
  [CEReactions, Throws, Func="IsNotUAWidget"]
  Node insertBefore(Node node, Node? child);
  [CEReactions, Throws, Func="IsNotUAWidget"]
  Node appendChild(Node node);
  [CEReactions, Throws, Func="IsNotUAWidget"]
  Node replaceChild(Node node, Node child);
  [CEReactions, Throws]
  Node removeChild(Node child);
  [CEReactions]
  undefined normalize();

  [CEReactions, Throws, Func="IsNotUAWidget"]
  Node cloneNode(optional boolean deep = false);
  [Pure]
  boolean isSameNode(Node? node);
  [Pure]
  boolean isEqualNode(Node? node);

  const unsigned short DOCUMENT_POSITION_DISCONNECTED = 0x01;
  const unsigned short DOCUMENT_POSITION_PRECEDING = 0x02;
  const unsigned short DOCUMENT_POSITION_FOLLOWING = 0x04;
  const unsigned short DOCUMENT_POSITION_CONTAINS = 0x08;
  const unsigned short DOCUMENT_POSITION_CONTAINED_BY = 0x10;
  const unsigned short DOCUMENT_POSITION_IMPLEMENTATION_SPECIFIC = 0x20; // historical
  [Pure]
  unsigned short compareDocumentPosition(Node other);
  [Pure]
  boolean contains(Node? other);

  [Pure]
  DOMString? lookupPrefix(DOMString? namespace);
  [Pure]
  DOMString? lookupNamespaceURI(DOMString? prefix);
  [Pure]
  boolean isDefaultNamespace(DOMString? namespace);

  // Mozilla-specific stuff
  [ChromeOnly]
  readonly attribute Principal nodePrincipal;
  [ChromeOnly]
  readonly attribute URI? baseURIObject;
  [ChromeOnly]
  DOMString generateXPath();
  [ChromeOnly, Pure, BinaryName="flattenedTreeParentNodeNonInline"]
  readonly attribute Node? flattenedTreeParentNode;
  [ChromeOnly, Pure, BinaryName="isInNativeAnonymousSubtree"]
  readonly attribute boolean isNativeAnonymous;
};
/* parts/NonElementParentNode.webidl ----------------------------------------------------- */
/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */

/* Castle Game Engine notes:
   This WEBIDL was cut down to our needs.
   This is not the original (complete) WEBIDL file,
   if you want a complete file get it from
   https://hg.mozilla.org/mozilla-central/raw-file/tip/dom/webidl/

   -------------------------------------------------------------------------------
*/

/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * https://dom.spec.whatwg.org/#nonelementparentnode
 */
interface mixin NonElementParentNode {
  [Pure]
  Element? getElementById(DOMString elementId);
};
/* parts/OffscreenCanvas.webidl ----------------------------------------------------- */
/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */

/* Castle Game Engine notes:
   This WEBIDL was cut down to our needs.
   This is not the original (complete) WEBIDL file,
   if you want a complete file get it from
   https://hg.mozilla.org/mozilla-central/raw-file/tip/dom/webidl/

   -------------------------------------------------------------------------------
*/

/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * For more information on this interface, please see
 * https://html.spec.whatwg.org/#the-offscreencanvas-interface
 */

typedef (OffscreenCanvasRenderingContext2D or ImageBitmapRenderingContext or WebGLRenderingContext or WebGL2RenderingContext or GPUCanvasContext) OffscreenRenderingContext;

dictionary ImageEncodeOptions {
  DOMString type = "image/png";
  unrestricted double quality;
};

enum OffscreenRenderingContextId { "2d", "bitmaprenderer", "webgl", "webgl2", "webgpu" };

[Exposed=(Window,Worker)]
interface OffscreenCanvas : EventTarget {
  [Throws]
  constructor([EnforceRange] unsigned long width, [EnforceRange] unsigned long height);

  [Pure, SetterThrows]
  attribute [EnforceRange] unsigned long width;
  [Pure, SetterThrows]
  attribute [EnforceRange] unsigned long height;

  [Throws]
  OffscreenRenderingContext? getContext(OffscreenRenderingContextId contextId,
                                        optional any contextOptions = null);

  [Throws]
  ImageBitmap transferToImageBitmap();

  attribute EventHandler oncontextlost;
  attribute EventHandler oncontextrestored;
};
/* parts/Performance.webidl ----------------------------------------------------- */
/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */

/* Castle Game Engine notes:
   This WEBIDL was cut down to our needs.
   This is not the original (complete) WEBIDL file,
   if you want a complete file get it from
   https://hg.mozilla.org/mozilla-central/raw-file/tip/dom/webidl/

   -------------------------------------------------------------------------------
*/

/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * https://w3c.github.io/hr-time/#sec-performance
 * https://w3c.github.io/navigation-timing/#extensions-to-the-performance-interface
 * https://w3c.github.io/performance-timeline/#extensions-to-the-performance-interface
 * https://w3c.github.io/resource-timing/#sec-extensions-performance-interface
 * https://w3c.github.io/user-timing/#extensions-performance-interface
 *
 * Copyright © 2015 W3C® (MIT, ERCIM, Keio, Beihang).
 * W3C liability, trademark and document use rules apply.
 */

// DOMTimeStamp is deprecated, use EpochTimeStamp instead.
typedef unsigned long long DOMTimeStamp;
typedef unsigned long long EpochTimeStamp;
typedef double DOMHighResTimeStamp;
/* parts/PointerEvent.webidl ----------------------------------------------------- */
/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * Portions Copyright 2013 Microsoft Open Technologies, Inc. */

interface WindowProxy;

[Exposed=Window]
interface PointerEvent : MouseEvent
{
  constructor(DOMString type, optional PointerEventInit eventInitDict = {});

  readonly attribute long pointerId;

  readonly attribute double width;
  readonly attribute double height;
  readonly attribute float pressure;
  readonly attribute float tangentialPressure;
  readonly attribute long tiltX;
  readonly attribute long tiltY;
  readonly attribute long twist;
  readonly attribute double altitudeAngle;
  readonly attribute double azimuthAngle;

  readonly attribute DOMString pointerType;
  readonly attribute boolean isPrimary;

  [Func="mozilla::dom::PointerEvent::EnableGetCoalescedEvents"]
  sequence<PointerEvent> getCoalescedEvents();
  sequence<PointerEvent> getPredictedEvents();
};

dictionary PointerEventInit : MouseEventInit
{
  long pointerId = 0;
  double width = 1.0;
  double height = 1.0;
  float pressure = 0;
  float tangentialPressure = 0;
  long tiltX;
  long tiltY;
  long twist = 0;
  double altitudeAngle;
  double azimuthAngle;
  DOMString pointerType = "";
  boolean isPrimary = false;
  sequence<PointerEvent> coalescedEvents = [];
  sequence<PointerEvent> predictedEvents = [];
};
/* parts/UIEvent.webidl ----------------------------------------------------- */
/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * For more information on this interface please see
 * http://dev.w3.org/2006/webapi/DOM-Level-3-Events/html/DOM3-Events.html
 *
 * Copyright © 2012 W3C® (MIT, ERCIM, Keio), All Rights Reserved. W3C
 * liability, trademark and document use rules apply.
 */

[Exposed=Window]
interface UIEvent : Event
{
  constructor(DOMString type, optional UIEventInit eventInitDict = {});

  readonly attribute WindowProxy? view;
  readonly attribute long         detail;
  undefined initUIEvent(DOMString aType,
                        optional boolean aCanBubble = false,
                        optional boolean aCancelable = false,
                        optional Window? aView = null,
                        optional long aDetail = 0);
};

// Additional DOM0 properties.
partial interface UIEvent {
  const long SCROLL_PAGE_UP = -32768;
  const long SCROLL_PAGE_DOWN = 32768;

  readonly attribute long          layerX;
  readonly attribute long          layerY;
  [NeedsCallerType]
  readonly attribute unsigned long which;
  readonly attribute Node?         rangeParent;
  readonly attribute long          rangeOffset;
};

dictionary UIEventInit : EventInit
{
  Window? view = null;
  long    detail = 0;
};

// NOTE: Gecko doesn't support commented out modifiers yet.
dictionary EventModifierInit : UIEventInit
{
  boolean ctrlKey = false;
  boolean shiftKey = false;
  boolean altKey = false;
  boolean metaKey = false;
  boolean modifierAltGraph = false;
  boolean modifierCapsLock = false;
  boolean modifierFn = false;
  boolean modifierFnLock = false;
  // boolean modifierHyper = false;
  boolean modifierNumLock = false;
  boolean modifierOS = false;
  boolean modifierScrollLock = false;
  // boolean modifierSuper = false;
  boolean modifierSymbol = false;
  boolean modifierSymbolLock = false;
};
/* parts/VideoFrame.webidl ----------------------------------------------------- */
/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */

/* Castle Game Engine notes:
   This WEBIDL was cut down to our needs.
   This is not the original (complete) WEBIDL file,
   if you want a complete file get it from
   https://hg.mozilla.org/mozilla-central/raw-file/tip/dom/webidl/

   -------------------------------------------------------------------------------
*/

/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * https://w3c.github.io/webcodecs/#videoframe
 */

enum AlphaOption {
  "keep",
  "discard",
};

// [Serializable, Transferable] are implemented without adding attributes here.
[Exposed=(Window,DedicatedWorker), Func="mozilla::dom::VideoFrame::PrefEnabled"]
interface VideoFrame {
  // The constructors should be shorten to:
  //   ```
  //   constructor([AllowShared] BufferSource data, VideoFrameBufferInit init);
  //   constructor(CanvasImageSource image, optional VideoFrameInit init = {});
  //   ```
  // However, `[AllowShared] BufferSource` doesn't work for now (bug 1696216), and
  // `No support for unions as distinguishing arguments yet` error occurs when using
  //   `constructor(CanvasImageSource image, optional VideoFrameInit init = {})` and
  //   `constructor(([AllowShared] ArrayBufferView or [AllowShared] ArrayBuffer) data, VideoFrameBufferInit init)`
  // at the same time (bug 1786410).
  [Throws]
  constructor(HTMLImageElement imageElement, optional VideoFrameInit init = {});
  //[Throws]
  //constructor(SVGImageElement svgImageElement, optional VideoFrameInit init = {});
  [Throws]
  constructor(HTMLCanvasElement canvasElement, optional VideoFrameInit init = {});
  [Throws]
  constructor(HTMLVideoElement videoElement, optional VideoFrameInit init = {});
  [Throws]
  constructor(OffscreenCanvas offscreenCanvas, optional VideoFrameInit init = {});
  [Throws]
  constructor(ImageBitmap imageBitmap, optional VideoFrameInit init = {});
  [Throws]
  constructor(VideoFrame videoFrame, optional VideoFrameInit init = {});
  [Throws]
  constructor([AllowShared] ArrayBufferView bufferView, VideoFrameBufferInit init);
  [Throws]
  constructor([AllowShared] ArrayBuffer buffer, VideoFrameBufferInit init);


  readonly attribute VideoPixelFormat? format;
  readonly attribute unsigned long codedWidth;
  readonly attribute unsigned long codedHeight;
  //readonly attribute DOMRectReadOnly? codedRect;
  //readonly attribute DOMRectReadOnly? visibleRect;
  readonly attribute unsigned long displayWidth;
  readonly attribute unsigned long displayHeight;
  readonly attribute unsigned long long? duration;  // microseconds
  readonly attribute long long timestamp;           // microseconds
  //readonly attribute VideoColorSpace colorSpace;

  [Throws]
  unsigned long allocationSize(
      optional VideoFrameCopyToOptions options = {});
  [Throws]
  Promise<sequence<PlaneLayout>> copyTo(
      // bug 1696216: Should be `copyTo([AllowShared] BufferSource destination, ...)`
      ([AllowShared] ArrayBufferView or [AllowShared] ArrayBuffer) destination,
      optional VideoFrameCopyToOptions options = {});
  [Throws]
  VideoFrame clone();
  undefined close();
};

dictionary VideoFrameInit {
  unsigned long long duration;  // microseconds
  long long timestamp;          // microseconds
  AlphaOption alpha = "keep";

  // Default matches image. May be used to efficiently crop. Will trigger
  // new computation of displayWidth and displayHeight using image’s pixel
  // aspect ratio unless an explicit displayWidth and displayHeight are given.
  //DOMRectInit visibleRect;

  // Default matches image unless visibleRect is provided.
  [EnforceRange] unsigned long displayWidth;
  [EnforceRange] unsigned long displayHeight;
};

dictionary VideoFrameBufferInit {
  required VideoPixelFormat format;
  required [EnforceRange] unsigned long codedWidth;
  required [EnforceRange] unsigned long codedHeight;
  required [EnforceRange] long long timestamp;  // microseconds
  [EnforceRange] unsigned long long duration;   // microseconds

  // Default layout is tightly-packed.
  sequence<PlaneLayout> layout;

  // Default visible rect is coded size positioned at (0,0)
  //DOMRectInit visibleRect;

  // Default display dimensions match visibleRect.
  [EnforceRange] unsigned long displayWidth;
  [EnforceRange] unsigned long displayHeight;

  //VideoColorSpaceInit colorSpace;
};

dictionary VideoFrameCopyToOptions {
  //DOMRectInit rect;
  sequence<PlaneLayout> layout;
  VideoPixelFormat format;
  PredefinedColorSpace colorSpace;
};

dictionary PlaneLayout {
  // TODO: https://github.com/w3c/webcodecs/pull/488
  required [EnforceRange] unsigned long offset;
  required [EnforceRange] unsigned long stride;
};

enum VideoPixelFormat {
  // 4:2:0 Y, U, V
  "I420",
  "I420P10",
  "I420P12",
  // 4:2:0 Y, U, V, A
  "I420A",
  "I420AP10",
  "I420AP12",
  // 4:2:2 Y, U, V
  "I422",
  "I422P10",
  "I422P12",
  // 4:2:2 Y, U, V, A
  "I422A",
  "I422AP10",
  "I422AP12",
  // 4:4:4 Y, U, V
  "I444",
  "I444P10",
  "I444P12",
  // 4:4:4 Y, U, V, A
  "I444A",
  "I444AP10",
  "I444AP12",
  // 4:2:0 Y, UV
  "NV12",
  // 4:4:4 RGBA
  "RGBA",
  // 4:4:4 RGBX (opaque)
  "RGBX",
  // 4:4:4 BGRA
  "BGRA",
  // 4:4:4 BGRX (opaque)
  "BGRX",
};
/* parts/WebGL2RenderingContext.webidl ----------------------------------------------------- */
/* -*- Mode: IDL; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */

/* Castle Game Engine notes:
   This WEBIDL was cut down to our needs.
   This is not the original (complete) WEBIDL file,
   if you want a complete file get it from
   https://hg.mozilla.org/mozilla-central/raw-file/tip/dom/webidl/

   -------------------------------------------------------------------------------
*/

/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The source for this IDL is found at https://www.khronos.org/registry/webgl/specs/latest/2.0
 * This IDL depends on WebGLRenderingContext.webidl
 */

typedef long long GLint64;
typedef unsigned long long GLuint64;

[Pref="webgl.enable-webgl2", Exposed=(Window,Worker)]
interface WebGLSampler {
};

[Pref="webgl.enable-webgl2", Exposed=(Window,Worker)]
interface WebGLSync {
};

[Pref="webgl.enable-webgl2", Exposed=(Window,Worker)]
interface WebGLTransformFeedback {
};

typedef ([AllowShared] Uint32Array or sequence<GLuint>) Uint32List;

// WebGL2 spec has this as an empty interface that pulls in everything
// via WebGL2RenderingContextBase.
[Pref="webgl.enable-webgl2", Exposed=(Window,Worker)]
interface WebGL2RenderingContext
{
};

interface mixin WebGL2RenderingContextBase
{
    const GLenum READ_BUFFER                                   = 0x0C02;
    const GLenum UNPACK_ROW_LENGTH                             = 0x0CF2;
    const GLenum UNPACK_SKIP_ROWS                              = 0x0CF3;
    const GLenum UNPACK_SKIP_PIXELS                            = 0x0CF4;
    const GLenum PACK_ROW_LENGTH                               = 0x0D02;
    const GLenum PACK_SKIP_ROWS                                = 0x0D03;
    const GLenum PACK_SKIP_PIXELS                              = 0x0D04;
    const GLenum COLOR                                         = 0x1800;
    const GLenum DEPTH                                         = 0x1801;
    const GLenum STENCIL                                       = 0x1802;
    const GLenum RED                                           = 0x1903;
    const GLenum RGB8                                          = 0x8051;
    const GLenum RGBA8                                         = 0x8058;
    const GLenum RGB10_A2                                      = 0x8059;
    const GLenum TEXTURE_BINDING_3D                            = 0x806A;
    const GLenum UNPACK_SKIP_IMAGES                            = 0x806D;
    const GLenum UNPACK_IMAGE_HEIGHT                           = 0x806E;
    const GLenum TEXTURE_3D                                    = 0x806F;
    const GLenum TEXTURE_WRAP_R                                = 0x8072;
    const GLenum MAX_3D_TEXTURE_SIZE                           = 0x8073;
    const GLenum UNSIGNED_INT_2_10_10_10_REV                   = 0x8368;
    const GLenum MAX_ELEMENTS_VERTICES                         = 0x80E8;
    const GLenum MAX_ELEMENTS_INDICES                          = 0x80E9;
    const GLenum TEXTURE_MIN_LOD                               = 0x813A;
    const GLenum TEXTURE_MAX_LOD                               = 0x813B;
    const GLenum TEXTURE_BASE_LEVEL                            = 0x813C;
    const GLenum TEXTURE_MAX_LEVEL                             = 0x813D;
    const GLenum MIN                                           = 0x8007;
    const GLenum MAX                                           = 0x8008;
    const GLenum DEPTH_COMPONENT24                             = 0x81A6;
    const GLenum MAX_TEXTURE_LOD_BIAS                          = 0x84FD;
    const GLenum TEXTURE_COMPARE_MODE                          = 0x884C;
    const GLenum TEXTURE_COMPARE_FUNC                          = 0x884D;
    const GLenum CURRENT_QUERY                                 = 0x8865;
    const GLenum QUERY_RESULT                                  = 0x8866;
    const GLenum QUERY_RESULT_AVAILABLE                        = 0x8867;
    const GLenum STREAM_READ                                   = 0x88E1;
    const GLenum STREAM_COPY                                   = 0x88E2;
    const GLenum STATIC_READ                                   = 0x88E5;
    const GLenum STATIC_COPY                                   = 0x88E6;
    const GLenum DYNAMIC_READ                                  = 0x88E9;
    const GLenum DYNAMIC_COPY                                  = 0x88EA;
    const GLenum MAX_DRAW_BUFFERS                              = 0x8824;
    const GLenum DRAW_BUFFER0                                  = 0x8825;
    const GLenum DRAW_BUFFER1                                  = 0x8826;
    const GLenum DRAW_BUFFER2                                  = 0x8827;
    const GLenum DRAW_BUFFER3                                  = 0x8828;
    const GLenum DRAW_BUFFER4                                  = 0x8829;
    const GLenum DRAW_BUFFER5                                  = 0x882A;
    const GLenum DRAW_BUFFER6                                  = 0x882B;
    const GLenum DRAW_BUFFER7                                  = 0x882C;
    const GLenum DRAW_BUFFER8                                  = 0x882D;
    const GLenum DRAW_BUFFER9                                  = 0x882E;
    const GLenum DRAW_BUFFER10                                 = 0x882F;
    const GLenum DRAW_BUFFER11                                 = 0x8830;
    const GLenum DRAW_BUFFER12                                 = 0x8831;
    const GLenum DRAW_BUFFER13                                 = 0x8832;
    const GLenum DRAW_BUFFER14                                 = 0x8833;
    const GLenum DRAW_BUFFER15                                 = 0x8834;
    const GLenum MAX_FRAGMENT_UNIFORM_COMPONENTS               = 0x8B49;
    const GLenum MAX_VERTEX_UNIFORM_COMPONENTS                 = 0x8B4A;
    const GLenum SAMPLER_3D                                    = 0x8B5F;
    const GLenum SAMPLER_2D_SHADOW                             = 0x8B62;
    const GLenum FRAGMENT_SHADER_DERIVATIVE_HINT               = 0x8B8B;
    const GLenum PIXEL_PACK_BUFFER                             = 0x88EB;
    const GLenum PIXEL_UNPACK_BUFFER                           = 0x88EC;
    const GLenum PIXEL_PACK_BUFFER_BINDING                     = 0x88ED;
    const GLenum PIXEL_UNPACK_BUFFER_BINDING                   = 0x88EF;
    const GLenum FLOAT_MAT2x3                                  = 0x8B65;
    const GLenum FLOAT_MAT2x4                                  = 0x8B66;
    const GLenum FLOAT_MAT3x2                                  = 0x8B67;
    const GLenum FLOAT_MAT3x4                                  = 0x8B68;
    const GLenum FLOAT_MAT4x2                                  = 0x8B69;
    const GLenum FLOAT_MAT4x3                                  = 0x8B6A;
    const GLenum SRGB                                          = 0x8C40;
    const GLenum SRGB8                                         = 0x8C41;
    const GLenum SRGB8_ALPHA8                                  = 0x8C43;
    const GLenum COMPARE_REF_TO_TEXTURE                        = 0x884E;
    const GLenum RGBA32F                                       = 0x8814;
    const GLenum RGB32F                                        = 0x8815;
    const GLenum RGBA16F                                       = 0x881A;
    const GLenum RGB16F                                        = 0x881B;
    const GLenum VERTEX_ATTRIB_ARRAY_INTEGER                   = 0x88FD;
    const GLenum MAX_ARRAY_TEXTURE_LAYERS                      = 0x88FF;
    const GLenum MIN_PROGRAM_TEXEL_OFFSET                      = 0x8904;
    const GLenum MAX_PROGRAM_TEXEL_OFFSET                      = 0x8905;
    const GLenum MAX_VARYING_COMPONENTS                        = 0x8B4B;
    const GLenum TEXTURE_2D_ARRAY                              = 0x8C1A;
    const GLenum TEXTURE_BINDING_2D_ARRAY                      = 0x8C1D;
    const GLenum R11F_G11F_B10F                                = 0x8C3A;
    const GLenum UNSIGNED_INT_10F_11F_11F_REV                  = 0x8C3B;
    const GLenum RGB9_E5                                       = 0x8C3D;
    const GLenum UNSIGNED_INT_5_9_9_9_REV                      = 0x8C3E;
    const GLenum TRANSFORM_FEEDBACK_BUFFER_MODE                = 0x8C7F;
    const GLenum MAX_TRANSFORM_FEEDBACK_SEPARATE_COMPONENTS    = 0x8C80;
    const GLenum TRANSFORM_FEEDBACK_VARYINGS                   = 0x8C83;
    const GLenum TRANSFORM_FEEDBACK_BUFFER_START               = 0x8C84;
    const GLenum TRANSFORM_FEEDBACK_BUFFER_SIZE                = 0x8C85;
    const GLenum TRANSFORM_FEEDBACK_PRIMITIVES_WRITTEN         = 0x8C88;
    const GLenum RASTERIZER_DISCARD                            = 0x8C89;
    const GLenum MAX_TRANSFORM_FEEDBACK_INTERLEAVED_COMPONENTS = 0x8C8A;
    const GLenum MAX_TRANSFORM_FEEDBACK_SEPARATE_ATTRIBS       = 0x8C8B;
    const GLenum INTERLEAVED_ATTRIBS                           = 0x8C8C;
    const GLenum SEPARATE_ATTRIBS                              = 0x8C8D;
    const GLenum TRANSFORM_FEEDBACK_BUFFER                     = 0x8C8E;
    const GLenum TRANSFORM_FEEDBACK_BUFFER_BINDING             = 0x8C8F;
    const GLenum RGBA32UI                                      = 0x8D70;
    const GLenum RGB32UI                                       = 0x8D71;
    const GLenum RGBA16UI                                      = 0x8D76;
    const GLenum RGB16UI                                       = 0x8D77;
    const GLenum RGBA8UI                                       = 0x8D7C;
    const GLenum RGB8UI                                        = 0x8D7D;
    const GLenum RGBA32I                                       = 0x8D82;
    const GLenum RGB32I                                        = 0x8D83;
    const GLenum RGBA16I                                       = 0x8D88;
    const GLenum RGB16I                                        = 0x8D89;
    const GLenum RGBA8I                                        = 0x8D8E;
    const GLenum RGB8I                                         = 0x8D8F;
    const GLenum RED_INTEGER                                   = 0x8D94;
    const GLenum RGB_INTEGER                                   = 0x8D98;
    const GLenum RGBA_INTEGER                                  = 0x8D99;
    const GLenum SAMPLER_2D_ARRAY                              = 0x8DC1;
    const GLenum SAMPLER_2D_ARRAY_SHADOW                       = 0x8DC4;
    const GLenum SAMPLER_CUBE_SHADOW                           = 0x8DC5;
    const GLenum UNSIGNED_INT_VEC2                             = 0x8DC6;
    const GLenum UNSIGNED_INT_VEC3                             = 0x8DC7;
    const GLenum UNSIGNED_INT_VEC4                             = 0x8DC8;
    const GLenum INT_SAMPLER_2D                                = 0x8DCA;
    const GLenum INT_SAMPLER_3D                                = 0x8DCB;
    const GLenum INT_SAMPLER_CUBE                              = 0x8DCC;
    const GLenum INT_SAMPLER_2D_ARRAY                          = 0x8DCF;
    const GLenum UNSIGNED_INT_SAMPLER_2D                       = 0x8DD2;
    const GLenum UNSIGNED_INT_SAMPLER_3D                       = 0x8DD3;
    const GLenum UNSIGNED_INT_SAMPLER_CUBE                     = 0x8DD4;
    const GLenum UNSIGNED_INT_SAMPLER_2D_ARRAY                 = 0x8DD7;
    const GLenum DEPTH_COMPONENT32F                            = 0x8CAC;
    const GLenum DEPTH32F_STENCIL8                             = 0x8CAD;
    const GLenum FLOAT_32_UNSIGNED_INT_24_8_REV                = 0x8DAD;
    const GLenum FRAMEBUFFER_ATTACHMENT_COLOR_ENCODING         = 0x8210;
    const GLenum FRAMEBUFFER_ATTACHMENT_COMPONENT_TYPE         = 0x8211;
    const GLenum FRAMEBUFFER_ATTACHMENT_RED_SIZE               = 0x8212;
    const GLenum FRAMEBUFFER_ATTACHMENT_GREEN_SIZE             = 0x8213;
    const GLenum FRAMEBUFFER_ATTACHMENT_BLUE_SIZE              = 0x8214;
    const GLenum FRAMEBUFFER_ATTACHMENT_ALPHA_SIZE             = 0x8215;
    const GLenum FRAMEBUFFER_ATTACHMENT_DEPTH_SIZE             = 0x8216;
    const GLenum FRAMEBUFFER_ATTACHMENT_STENCIL_SIZE           = 0x8217;
    const GLenum FRAMEBUFFER_DEFAULT                           = 0x8218;
    const GLenum UNSIGNED_INT_24_8                             = 0x84FA;
    const GLenum DEPTH24_STENCIL8                              = 0x88F0;
    const GLenum UNSIGNED_NORMALIZED                           = 0x8C17;
    const GLenum DRAW_FRAMEBUFFER_BINDING                      = 0x8CA6; /* Same as FRAMEBUFFER_BINDING */
    const GLenum READ_FRAMEBUFFER                              = 0x8CA8;
    const GLenum DRAW_FRAMEBUFFER                              = 0x8CA9;
    const GLenum READ_FRAMEBUFFER_BINDING                      = 0x8CAA;
    const GLenum RENDERBUFFER_SAMPLES                          = 0x8CAB;
    const GLenum FRAMEBUFFER_ATTACHMENT_TEXTURE_LAYER          = 0x8CD4;
    const GLenum MAX_COLOR_ATTACHMENTS                         = 0x8CDF;
    const GLenum COLOR_ATTACHMENT1                             = 0x8CE1;
    const GLenum COLOR_ATTACHMENT2                             = 0x8CE2;
    const GLenum COLOR_ATTACHMENT3                             = 0x8CE3;
    const GLenum COLOR_ATTACHMENT4                             = 0x8CE4;
    const GLenum COLOR_ATTACHMENT5                             = 0x8CE5;
    const GLenum COLOR_ATTACHMENT6                             = 0x8CE6;
    const GLenum COLOR_ATTACHMENT7                             = 0x8CE7;
    const GLenum COLOR_ATTACHMENT8                             = 0x8CE8;
    const GLenum COLOR_ATTACHMENT9                             = 0x8CE9;
    const GLenum COLOR_ATTACHMENT10                            = 0x8CEA;
    const GLenum COLOR_ATTACHMENT11                            = 0x8CEB;
    const GLenum COLOR_ATTACHMENT12                            = 0x8CEC;
    const GLenum COLOR_ATTACHMENT13                            = 0x8CED;
    const GLenum COLOR_ATTACHMENT14                            = 0x8CEE;
    const GLenum COLOR_ATTACHMENT15                            = 0x8CEF;
    const GLenum FRAMEBUFFER_INCOMPLETE_MULTISAMPLE            = 0x8D56;
    const GLenum MAX_SAMPLES                                   = 0x8D57;
    const GLenum HALF_FLOAT                                    = 0x140B;
    const GLenum RG                                            = 0x8227;
    const GLenum RG_INTEGER                                    = 0x8228;
    const GLenum R8                                            = 0x8229;
    const GLenum RG8                                           = 0x822B;
    const GLenum R16F                                          = 0x822D;
    const GLenum R32F                                          = 0x822E;
    const GLenum RG16F                                         = 0x822F;
    const GLenum RG32F                                         = 0x8230;
    const GLenum R8I                                           = 0x8231;
    const GLenum R8UI                                          = 0x8232;
    const GLenum R16I                                          = 0x8233;
    const GLenum R16UI                                         = 0x8234;
    const GLenum R32I                                          = 0x8235;
    const GLenum R32UI                                         = 0x8236;
    const GLenum RG8I                                          = 0x8237;
    const GLenum RG8UI                                         = 0x8238;
    const GLenum RG16I                                         = 0x8239;
    const GLenum RG16UI                                        = 0x823A;
    const GLenum RG32I                                         = 0x823B;
    const GLenum RG32UI                                        = 0x823C;
    const GLenum VERTEX_ARRAY_BINDING                          = 0x85B5;
    const GLenum R8_SNORM                                      = 0x8F94;
    const GLenum RG8_SNORM                                     = 0x8F95;
    const GLenum RGB8_SNORM                                    = 0x8F96;
    const GLenum RGBA8_SNORM                                   = 0x8F97;
    const GLenum SIGNED_NORMALIZED                             = 0x8F9C;
    const GLenum COPY_READ_BUFFER                              = 0x8F36;
    const GLenum COPY_WRITE_BUFFER                             = 0x8F37;
    const GLenum COPY_READ_BUFFER_BINDING                      = 0x8F36; /* Same as COPY_READ_BUFFER */
    const GLenum COPY_WRITE_BUFFER_BINDING                     = 0x8F37; /* Same as COPY_WRITE_BUFFER */
    const GLenum UNIFORM_BUFFER                                = 0x8A11;
    const GLenum UNIFORM_BUFFER_BINDING                        = 0x8A28;
    const GLenum UNIFORM_BUFFER_START                          = 0x8A29;
    const GLenum UNIFORM_BUFFER_SIZE                           = 0x8A2A;
    const GLenum MAX_VERTEX_UNIFORM_BLOCKS                     = 0x8A2B;
    const GLenum MAX_FRAGMENT_UNIFORM_BLOCKS                   = 0x8A2D;
    const GLenum MAX_COMBINED_UNIFORM_BLOCKS                   = 0x8A2E;
    const GLenum MAX_UNIFORM_BUFFER_BINDINGS                   = 0x8A2F;
    const GLenum MAX_UNIFORM_BLOCK_SIZE                        = 0x8A30;
    const GLenum MAX_COMBINED_VERTEX_UNIFORM_COMPONENTS        = 0x8A31;
    const GLenum MAX_COMBINED_FRAGMENT_UNIFORM_COMPONENTS      = 0x8A33;
    const GLenum UNIFORM_BUFFER_OFFSET_ALIGNMENT               = 0x8A34;
    const GLenum ACTIVE_UNIFORM_BLOCKS                         = 0x8A36;
    const GLenum UNIFORM_TYPE                                  = 0x8A37;
    const GLenum UNIFORM_SIZE                                  = 0x8A38;
    const GLenum UNIFORM_BLOCK_INDEX                           = 0x8A3A;
    const GLenum UNIFORM_OFFSET                                = 0x8A3B;
    const GLenum UNIFORM_ARRAY_STRIDE                          = 0x8A3C;
    const GLenum UNIFORM_MATRIX_STRIDE                         = 0x8A3D;
    const GLenum UNIFORM_IS_ROW_MAJOR                          = 0x8A3E;
    const GLenum UNIFORM_BLOCK_BINDING                         = 0x8A3F;
    const GLenum UNIFORM_BLOCK_DATA_SIZE                       = 0x8A40;
    const GLenum UNIFORM_BLOCK_ACTIVE_UNIFORMS                 = 0x8A42;
    const GLenum UNIFORM_BLOCK_ACTIVE_UNIFORM_INDICES          = 0x8A43;
    const GLenum UNIFORM_BLOCK_REFERENCED_BY_VERTEX_SHADER     = 0x8A44;
    const GLenum UNIFORM_BLOCK_REFERENCED_BY_FRAGMENT_SHADER   = 0x8A46;
    const GLenum INVALID_INDEX                                 = 0xFFFFFFFF;
    const GLenum MAX_VERTEX_OUTPUT_COMPONENTS                  = 0x9122;
    const GLenum MAX_FRAGMENT_INPUT_COMPONENTS                 = 0x9125;
    const GLenum MAX_SERVER_WAIT_TIMEOUT                       = 0x9111;
    const GLenum OBJECT_TYPE                                   = 0x9112;
    const GLenum SYNC_CONDITION                                = 0x9113;
    const GLenum SYNC_STATUS                                   = 0x9114;
    const GLenum SYNC_FLAGS                                    = 0x9115;
    const GLenum SYNC_FENCE                                    = 0x9116;
    const GLenum SYNC_GPU_COMMANDS_COMPLETE                    = 0x9117;
    const GLenum UNSIGNALED                                    = 0x9118;
    const GLenum SIGNALED                                      = 0x9119;
    const GLenum ALREADY_SIGNALED                              = 0x911A;
    const GLenum TIMEOUT_EXPIRED                               = 0x911B;
    const GLenum CONDITION_SATISFIED                           = 0x911C;
    const GLenum WAIT_FAILED                                   = 0x911D;
    const GLenum SYNC_FLUSH_COMMANDS_BIT                       = 0x00000001;
    const GLenum VERTEX_ATTRIB_ARRAY_DIVISOR                   = 0x88FE;
    const GLenum ANY_SAMPLES_PASSED                            = 0x8C2F;
    const GLenum ANY_SAMPLES_PASSED_CONSERVATIVE               = 0x8D6A;
    const GLenum SAMPLER_BINDING                               = 0x8919;
    const GLenum RGB10_A2UI                                    = 0x906F;
    const GLenum INT_2_10_10_10_REV                            = 0x8D9F;
    const GLenum TRANSFORM_FEEDBACK                            = 0x8E22;
    const GLenum TRANSFORM_FEEDBACK_PAUSED                     = 0x8E23;
    const GLenum TRANSFORM_FEEDBACK_ACTIVE                     = 0x8E24;
    const GLenum TRANSFORM_FEEDBACK_BINDING                    = 0x8E25;
    const GLenum TEXTURE_IMMUTABLE_FORMAT                      = 0x912F;
    const GLenum MAX_ELEMENT_INDEX                             = 0x8D6B;
    const GLenum TEXTURE_IMMUTABLE_LEVELS                      = 0x82DF;

    const GLint64 TIMEOUT_IGNORED                              = -1;

    /* WebGL-specific enums */
    const GLenum MAX_CLIENT_WAIT_TIMEOUT_WEBGL                 = 0x9247;

    /* Buffer objects */
    // WebGL1:
    undefined bufferData(GLenum target, GLsizeiptr size, GLenum usage);
    undefined bufferData(GLenum target, [AllowShared] ArrayBuffer? srcData, GLenum usage);
    undefined bufferData(GLenum target, [AllowShared] ArrayBufferView srcData, GLenum usage);
    undefined bufferSubData(GLenum target, GLintptr offset, [AllowShared] ArrayBuffer srcData);
    undefined bufferSubData(GLenum target, GLintptr offset, [AllowShared] ArrayBufferView srcData);
    // WebGL2:
    undefined bufferData(GLenum target, [AllowShared] ArrayBufferView srcData, GLenum usage,
                         GLuint srcOffset, optional GLuint length = 0);
    undefined bufferSubData(GLenum target, GLintptr dstByteOffset, [AllowShared] ArrayBufferView srcData,
                            GLuint srcOffset, optional GLuint length = 0);

    undefined copyBufferSubData(GLenum readTarget, GLenum writeTarget, GLintptr readOffset,
                                GLintptr writeOffset, GLsizeiptr size);
    // MapBufferRange, in particular its read-only and write-only modes,
    // can not be exposed safely to JavaScript. GetBufferSubData
    // replaces it for the purpose of fetching data back from the GPU.
    undefined getBufferSubData(GLenum target, GLintptr srcByteOffset, [AllowShared] ArrayBufferView dstData,
                               optional GLuint dstOffset = 0, optional GLuint length = 0);

    /* Framebuffer objects */
    undefined blitFramebuffer(GLint srcX0, GLint srcY0, GLint srcX1, GLint srcY1, GLint dstX0, GLint dstY0,
                              GLint dstX1, GLint dstY1, GLbitfield mask, GLenum filter);
    undefined framebufferTextureLayer(GLenum target, GLenum attachment, WebGLTexture? texture, GLint level,
                                      GLint layer);

    [Throws]
    undefined invalidateFramebuffer(GLenum target, sequence<GLenum> attachments);

    [Throws]
    undefined invalidateSubFramebuffer(GLenum target, sequence<GLenum> attachments,
                                       GLint x, GLint y, GLsizei width, GLsizei height);

    undefined readBuffer(GLenum src);

    /* Renderbuffer objects */
    [Throws]
    any getInternalformatParameter(GLenum target, GLenum internalformat, GLenum pname);
    undefined renderbufferStorageMultisample(GLenum target, GLsizei samples, GLenum internalformat,
                                             GLsizei width, GLsizei height);

    /* Texture objects */
    undefined texStorage2D(GLenum target, GLsizei levels, GLenum internalformat, GLsizei width,
                           GLsizei height);
    undefined texStorage3D(GLenum target, GLsizei levels, GLenum internalformat, GLsizei width,
                           GLsizei height, GLsizei depth);

    // WebGL1 legacy entrypoints:
    [Throws] // Another overhead throws.
    undefined texImage2D(GLenum target, GLint level, GLint internalformat,
                         GLsizei width, GLsizei height, GLint border, GLenum format,
                         GLenum type, [AllowShared] ArrayBufferView? pixels);
    [Throws]
    undefined texImage2D(GLenum target, GLint level, GLint internalformat,
                         GLenum format, GLenum type, HTMLCanvasElement source); // May throw DOMException
    [Throws]
    undefined texImage2D(GLenum target, GLint level, GLint internalformat,
                         GLenum format, GLenum type, HTMLImageElement source); // May throw DOMException
    [Throws]
    undefined texImage2D(GLenum target, GLint level, GLint internalformat,
                         GLenum format, GLenum type, HTMLVideoElement source); // May throw DOMException
    [Throws] // Another overhead throws.
    undefined texImage2D(GLenum target, GLint level, GLint internalformat,
                         GLenum format, GLenum type, ImageBitmap source);
    [Throws] // Another overhead throws.
    undefined texImage2D(GLenum target, GLint level, GLint internalformat,
                         GLenum format, GLenum type, ImageData source);
    [Throws] // Another overhead throws.
    undefined texImage2D(GLenum target, GLint level, GLint internalformat,
                         GLenum format, GLenum type, OffscreenCanvas source);
    [Throws] // Another overhead throws.
    undefined texImage2D(GLenum target, GLint level, GLint internalformat,
                         GLenum format, GLenum type, VideoFrame source);

    [Throws] // Another overhead throws.
    undefined texSubImage2D(GLenum target, GLint level, GLint xoffset, GLint yoffset,
                            GLsizei width, GLsizei height,
                            GLenum format, GLenum type, [AllowShared] ArrayBufferView? pixels);
    [Throws]
    undefined texSubImage2D(GLenum target, GLint level, GLint xoffset, GLint yoffset,
                            GLenum format, GLenum type, HTMLCanvasElement source); // May throw DOMException
    [Throws]
    undefined texSubImage2D(GLenum target, GLint level, GLint xoffset, GLint yoffset,
                            GLenum format, GLenum type, HTMLImageElement source); // May throw DOMException
    [Throws]
    undefined texSubImage2D(GLenum target, GLint level, GLint xoffset, GLint yoffset,
                            GLenum format, GLenum type, HTMLVideoElement source); // May throw DOMException
    [Throws] // Another overhead throws.
    undefined texSubImage2D(GLenum target, GLint level, GLint xoffset, GLint yoffset,
                            GLenum format, GLenum type, ImageBitmap source);
    [Throws] // Another overhead throws.
    undefined texSubImage2D(GLenum target, GLint level, GLint xoffset, GLint yoffset,
                            GLenum format, GLenum type, ImageData source);
    [Throws] // Another overhead throws.
    undefined texSubImage2D(GLenum target, GLint level, GLint xoffset, GLint yoffset,
                            GLenum format, GLenum type, OffscreenCanvas source);
    [Throws] // Another overhead throws.
    undefined texSubImage2D(GLenum target, GLint level, GLint xoffset, GLint yoffset,
                            GLenum format, GLenum type, VideoFrame source);

    // WebGL2 entrypoints:
    [Throws] // Another overhead throws.
    undefined texImage2D(GLenum target, GLint level, GLint internalformat, GLsizei width, GLsizei height,
                         GLint border, GLenum format, GLenum type, GLintptr pboOffset);
    [Throws]
    undefined texImage2D(GLenum target, GLint level, GLint internalformat, GLsizei width, GLsizei height,
                         GLint border, GLenum format, GLenum type,
                         HTMLCanvasElement source); // May throw DOMException
    [Throws]
    undefined texImage2D(GLenum target, GLint level, GLint internalformat, GLsizei width, GLsizei height,
                         GLint border, GLenum format, GLenum type,
                         HTMLImageElement source); // May throw DOMException
    [Throws]
    undefined texImage2D(GLenum target, GLint level, GLint internalformat, GLsizei width, GLsizei height,
                         GLint border, GLenum format, GLenum type,
                         HTMLVideoElement source); // May throw DOMException
    [Throws] // Another overhead throws.
    undefined texImage2D(GLenum target, GLint level, GLint internalformat, GLsizei width, GLsizei height,
                         GLint border, GLenum format, GLenum type,
                         ImageBitmap source);
    [Throws] // Another overhead throws.
    undefined texImage2D(GLenum target, GLint level, GLint internalformat, GLsizei width, GLsizei height,
                         GLint border, GLenum format, GLenum type,
                         ImageData source);
    [Throws] // Another overhead throws.
    undefined texImage2D(GLenum target, GLint level, GLint internalformat, GLsizei width, GLsizei height,
                         GLint border, GLenum format, GLenum type,
                         OffscreenCanvas source);
    [Throws] // Another overhead throws.
    undefined texImage2D(GLenum target, GLint level, GLint internalformat, GLsizei width, GLsizei height,
                         GLint border, GLenum format, GLenum type,
                         VideoFrame source);
    [Throws] // Another overhead throws.
    undefined texImage2D(GLenum target, GLint level, GLint internalformat, GLsizei width, GLsizei height,
                         GLint border, GLenum format, GLenum type, [AllowShared] ArrayBufferView srcData,
                         GLuint srcOffset);

    [Throws] // Another overhead throws.
    undefined texImage3D(GLenum target, GLint level, GLint internalformat, GLsizei width, GLsizei height,
                         GLsizei depth, GLint border, GLenum format, GLenum type, GLintptr pboOffset);
    [Throws]
    undefined texImage3D(GLenum target, GLint level, GLint internalformat, GLsizei width, GLsizei height,
                         GLsizei depth, GLint border, GLenum format, GLenum type,
                         HTMLCanvasElement source); // May throw DOMException
    [Throws]
    undefined texImage3D(GLenum target, GLint level, GLint internalformat, GLsizei width, GLsizei height,
                         GLsizei depth, GLint border, GLenum format, GLenum type,
                         HTMLImageElement source); // May throw DOMException
    [Throws]
    undefined texImage3D(GLenum target, GLint level, GLint internalformat, GLsizei width, GLsizei height,
                         GLsizei depth, GLint border, GLenum format, GLenum type,
                         HTMLVideoElement source); // May throw DOMException
    [Throws] // Another overhead throws.
    undefined texImage3D(GLenum target, GLint level, GLint internalformat, GLsizei width, GLsizei height,
                         GLsizei depth, GLint border, GLenum format, GLenum type,
                         ImageBitmap source);
    [Throws] // Another overhead throws.
    undefined texImage3D(GLenum target, GLint level, GLint internalformat, GLsizei width, GLsizei height,
                         GLsizei depth, GLint border, GLenum format, GLenum type,
                         ImageData source);
    [Throws] // Another overhead throws.
    undefined texImage3D(GLenum target, GLint level, GLint internalformat, GLsizei width, GLsizei height,
                         GLsizei depth, GLint border, GLenum format, GLenum type,
                         OffscreenCanvas source);
    [Throws] // Another overhead throws.
    undefined texImage3D(GLenum target, GLint level, GLint internalformat, GLsizei width, GLsizei height,
                         GLsizei depth, GLint border, GLenum format, GLenum type,
                         VideoFrame source);
    [Throws] // Another overhead throws.
    undefined texImage3D(GLenum target, GLint level, GLint internalformat, GLsizei width, GLsizei height,
                         GLsizei depth, GLint border, GLenum format, GLenum type, [AllowShared] ArrayBufferView? srcData);
    [Throws] // Another overhead throws.
    undefined texImage3D(GLenum target, GLint level, GLint internalformat, GLsizei width, GLsizei height,
                         GLsizei depth, GLint border, GLenum format, GLenum type, [AllowShared] ArrayBufferView srcData,
                         GLuint srcOffset);

    [Throws] // Another overhead throws.
    undefined texSubImage2D(GLenum target, GLint level, GLint xoffset, GLint yoffset, GLsizei width,
                            GLsizei height, GLenum format, GLenum type, GLintptr pboOffset);
    [Throws]
    undefined texSubImage2D(GLenum target, GLint level, GLint xoffset, GLint yoffset, GLsizei width,
                            GLsizei height, GLenum format, GLenum type,
                            HTMLCanvasElement source); // May throw DOMException
    [Throws]
    undefined texSubImage2D(GLenum target, GLint level, GLint xoffset, GLint yoffset, GLsizei width,
                            GLsizei height, GLenum format, GLenum type,
                            HTMLImageElement source); // May throw DOMException
    [Throws]
    undefined texSubImage2D(GLenum target, GLint level, GLint xoffset, GLint yoffset, GLsizei width,
                            GLsizei height, GLenum format, GLenum type,
                            HTMLVideoElement source); // May throw DOMException
    [Throws] // Another overhead throws.
    undefined texSubImage2D(GLenum target, GLint level, GLint xoffset, GLint yoffset, GLsizei width,
                            GLsizei height, GLenum format, GLenum type,
                            ImageBitmap source);
    [Throws] // Another overhead throws.
    undefined texSubImage2D(GLenum target, GLint level, GLint xoffset, GLint yoffset, GLsizei width,
                            GLsizei height, GLenum format, GLenum type,
                            ImageData source);
    [Throws] // Another overhead throws.
    undefined texSubImage2D(GLenum target, GLint level, GLint xoffset, GLint yoffset, GLsizei width,
                            GLsizei height, GLenum format, GLenum type,
                            OffscreenCanvas source);
    [Throws] // Another overhead throws.
    undefined texSubImage2D(GLenum target, GLint level, GLint xoffset, GLint yoffset, GLsizei width,
                            GLsizei height, GLenum format, GLenum type,
                            VideoFrame source);
    [Throws] // Another overhead throws.
    undefined texSubImage2D(GLenum target, GLint level, GLint xoffset, GLint yoffset, GLsizei width,
                            GLsizei height, GLenum format, GLenum type, [AllowShared] ArrayBufferView srcData,
                            GLuint srcOffset);

    [Throws] // Another overhead throws.
    undefined texSubImage3D(GLenum target, GLint level, GLint xoffset, GLint yoffset, GLint zoffset,
                            GLsizei width, GLsizei height, GLsizei depth, GLenum format, GLenum type,
                            GLintptr pboOffset);
    [Throws]
    undefined texSubImage3D(GLenum target, GLint level, GLint xoffset, GLint yoffset, GLint zoffset,
                            GLsizei width, GLsizei height, GLsizei depth, GLenum format, GLenum type,
                            HTMLCanvasElement source); // May throw DOMException
    [Throws]
    undefined texSubImage3D(GLenum target, GLint level, GLint xoffset, GLint yoffset, GLint zoffset,
                            GLsizei width, GLsizei height, GLsizei depth, GLenum format, GLenum type,
                            HTMLImageElement source); // May throw DOMException
    [Throws]
    undefined texSubImage3D(GLenum target, GLint level, GLint xoffset, GLint yoffset, GLint zoffset,
                            GLsizei width, GLsizei height, GLsizei depth, GLenum format, GLenum type,
                            HTMLVideoElement source); // May throw DOMException
    [Throws] // Another overhead throws.
    undefined texSubImage3D(GLenum target, GLint level, GLint xoffset, GLint yoffset, GLint zoffset,
                            GLsizei width, GLsizei height, GLsizei depth, GLenum format, GLenum type,
                            ImageBitmap source);
    [Throws] // Another overhead throws.
    undefined texSubImage3D(GLenum target, GLint level, GLint xoffset, GLint yoffset, GLint zoffset,
                            GLsizei width, GLsizei height, GLsizei depth, GLenum format, GLenum type,
                            ImageData source);
    [Throws] // Another overhead throws.
    undefined texSubImage3D(GLenum target, GLint level, GLint xoffset, GLint yoffset, GLint zoffset,
                            GLsizei width, GLsizei height, GLsizei depth, GLenum format, GLenum type,
                            OffscreenCanvas source);
    [Throws] // Another overhead throws.
    undefined texSubImage3D(GLenum target, GLint level, GLint xoffset, GLint yoffset, GLint zoffset,
                            GLsizei width, GLsizei height, GLsizei depth, GLenum format, GLenum type,
                            VideoFrame source);
    [Throws] // Another overhead throws.
    undefined texSubImage3D(GLenum target, GLint level, GLint xoffset, GLint yoffset, GLint zoffset,
                            GLsizei width, GLsizei height, GLsizei depth, GLenum format, GLenum type,
                            [AllowShared] ArrayBufferView? srcData, optional GLuint srcOffset = 0);

    undefined copyTexSubImage3D(GLenum target, GLint level, GLint xoffset, GLint yoffset, GLint zoffset,
                                GLint x, GLint y, GLsizei width, GLsizei height);

    undefined compressedTexImage2D(GLenum target, GLint level, GLenum internalformat, GLsizei width,
                                   GLsizei height, GLint border, GLsizei imageSize,  GLintptr offset);
    undefined compressedTexImage2D(GLenum target, GLint level, GLenum internalformat, GLsizei width,
                                   GLsizei height, GLint border, [AllowShared] ArrayBufferView srcData,
                                   optional GLuint srcOffset = 0, optional GLuint srcLengthOverride = 0);

    undefined compressedTexImage3D(GLenum target, GLint level, GLenum internalformat, GLsizei width,
                                   GLsizei height, GLsizei depth, GLint border, GLsizei imageSize, GLintptr offset);
    undefined compressedTexImage3D(GLenum target, GLint level, GLenum internalformat, GLsizei width,
                                   GLsizei height, GLsizei depth, GLint border, [AllowShared] ArrayBufferView srcData,
                                   optional GLuint srcOffset = 0, optional GLuint srcLengthOverride = 0);

    undefined compressedTexSubImage2D(GLenum target, GLint level, GLint xoffset, GLint yoffset,
                                      GLsizei width, GLsizei height, GLenum format, GLsizei imageSize, GLintptr offset);
    undefined compressedTexSubImage2D(GLenum target, GLint level, GLint xoffset, GLint yoffset,
                                      GLsizei width, GLsizei height, GLenum format,
                                      [AllowShared] ArrayBufferView srcData,
                                      optional GLuint srcOffset = 0,
                                      optional GLuint srcLengthOverride = 0);

    undefined compressedTexSubImage3D(GLenum target, GLint level, GLint xoffset, GLint yoffset,
                                      GLint zoffset, GLsizei width, GLsizei height, GLsizei depth,
                                      GLenum format, GLsizei imageSize, GLintptr offset);
    undefined compressedTexSubImage3D(GLenum target, GLint level, GLint xoffset, GLint yoffset,
                                      GLint zoffset, GLsizei width, GLsizei height, GLsizei depth,
                                      GLenum format, [AllowShared] ArrayBufferView srcData,
                                      optional GLuint srcOffset = 0,
                                      optional GLuint srcLengthOverride = 0);

    /* Programs and shaders */
    [WebGLHandlesContextLoss] GLint getFragDataLocation(WebGLProgram program, DOMString name);

    /* Uniforms */
    undefined uniform1ui(WebGLUniformLocation? location, GLuint v0);
    undefined uniform2ui(WebGLUniformLocation? location, GLuint v0, GLuint v1);
    undefined uniform3ui(WebGLUniformLocation? location, GLuint v0, GLuint v1, GLuint v2);
    undefined uniform4ui(WebGLUniformLocation? location, GLuint v0, GLuint v1, GLuint v2, GLuint v3);

    undefined uniform1fv(WebGLUniformLocation? location, Float32List data, optional GLuint srcOffset = 0,
                         optional GLuint srcLength = 0);
    undefined uniform2fv(WebGLUniformLocation? location, Float32List data, optional GLuint srcOffset = 0,
                         optional GLuint srcLength = 0);
    undefined uniform3fv(WebGLUniformLocation? location, Float32List data, optional GLuint srcOffset = 0,
                         optional GLuint srcLength = 0);
    undefined uniform4fv(WebGLUniformLocation? location, Float32List data, optional GLuint srcOffset = 0,
                         optional GLuint srcLength = 0);

    undefined uniform1iv(WebGLUniformLocation? location, Int32List data, optional GLuint srcOffset = 0,
                         optional GLuint srcLength = 0);
    undefined uniform2iv(WebGLUniformLocation? location, Int32List data, optional GLuint srcOffset = 0,
                         optional GLuint srcLength = 0);
    undefined uniform3iv(WebGLUniformLocation? location, Int32List data, optional GLuint srcOffset = 0,
                         optional GLuint srcLength = 0);
    undefined uniform4iv(WebGLUniformLocation? location, Int32List data, optional GLuint srcOffset = 0,
                         optional GLuint srcLength = 0);

    undefined uniform1uiv(WebGLUniformLocation? location, Uint32List data, optional GLuint srcOffset = 0,
                          optional GLuint srcLength = 0);
    undefined uniform2uiv(WebGLUniformLocation? location, Uint32List data, optional GLuint srcOffset = 0,
                          optional GLuint srcLength = 0);
    undefined uniform3uiv(WebGLUniformLocation? location, Uint32List data, optional GLuint srcOffset = 0,
                          optional GLuint srcLength = 0);
    undefined uniform4uiv(WebGLUniformLocation? location, Uint32List data, optional GLuint srcOffset = 0,
                          optional GLuint srcLength = 0);

    undefined uniformMatrix2fv(WebGLUniformLocation? location, GLboolean transpose, Float32List data,
                               optional GLuint srcOffset = 0, optional GLuint srcLength = 0);
    undefined uniformMatrix3x2fv(WebGLUniformLocation? location, GLboolean transpose, Float32List data,
                                 optional GLuint srcOffset = 0, optional GLuint srcLength = 0);
    undefined uniformMatrix4x2fv(WebGLUniformLocation? location, GLboolean transpose, Float32List data,
                                 optional GLuint srcOffset = 0, optional GLuint srcLength = 0);

    undefined uniformMatrix2x3fv(WebGLUniformLocation? location, GLboolean transpose, Float32List data,
                                 optional GLuint srcOffset = 0, optional GLuint srcLength = 0);
    undefined uniformMatrix3fv(WebGLUniformLocation? location, GLboolean transpose, Float32List data,
                               optional GLuint srcOffset = 0, optional GLuint srcLength = 0);
    undefined uniformMatrix4x3fv(WebGLUniformLocation? location, GLboolean transpose, Float32List data,
                                 optional GLuint srcOffset = 0, optional GLuint srcLength = 0);

    undefined uniformMatrix2x4fv(WebGLUniformLocation? location, GLboolean transpose, Float32List data,
                                 optional GLuint srcOffset = 0, optional GLuint srcLength = 0);
    undefined uniformMatrix3x4fv(WebGLUniformLocation? location, GLboolean transpose, Float32List data,
                                 optional GLuint srcOffset = 0, optional GLuint srcLength = 0);
    undefined uniformMatrix4fv(WebGLUniformLocation? location, GLboolean transpose, Float32List data,
                               optional GLuint srcOffset = 0, optional GLuint srcLength = 0);

    /* Vertex attribs */
    undefined vertexAttribI4i(GLuint index, GLint x, GLint y, GLint z, GLint w);
    undefined vertexAttribI4iv(GLuint index, Int32List values);
    undefined vertexAttribI4ui(GLuint index, GLuint x, GLuint y, GLuint z, GLuint w);
    undefined vertexAttribI4uiv(GLuint index, Uint32List values);
    undefined vertexAttribIPointer(GLuint index, GLint size, GLenum type, GLsizei stride, GLintptr offset);

    /* Writing to the drawing buffer */
    undefined vertexAttribDivisor(GLuint index, GLuint divisor);
    undefined drawArraysInstanced(GLenum mode, GLint first, GLsizei count, GLsizei instanceCount);
    undefined drawElementsInstanced(GLenum mode, GLsizei count, GLenum type, GLintptr offset, GLsizei instanceCount);
    undefined drawRangeElements(GLenum mode, GLuint start, GLuint end, GLsizei count, GLenum type, GLintptr offset);

    /* Reading back pixels */
    // WebGL1:
    [Throws, NeedsCallerType] // Throws on readback in a write-only context.
    undefined readPixels(GLint x, GLint y, GLsizei width, GLsizei height, GLenum format, GLenum type,
                         [AllowShared] ArrayBufferView? dstData);
    // WebGL2:
    [Throws, NeedsCallerType] // Throws on readback in a write-only context.
    undefined readPixels(GLint x, GLint y, GLsizei width, GLsizei height, GLenum format, GLenum type,
                         GLintptr offset);
    [Throws, NeedsCallerType] // Throws on readback in a write-only context.
    undefined readPixels(GLint x, GLint y, GLsizei width, GLsizei height, GLenum format, GLenum type,
                         [AllowShared] ArrayBufferView dstData, GLuint dstOffset);

    /* Multiple Render Targets */
    undefined drawBuffers(sequence<GLenum> buffers);

    undefined clearBufferfv(GLenum buffer, GLint drawbuffer, Float32List values,
                            optional GLuint srcOffset = 0);
    undefined clearBufferiv(GLenum buffer, GLint drawbuffer, Int32List values,
                            optional GLuint srcOffset = 0);
    undefined clearBufferuiv(GLenum buffer, GLint drawbuffer, Uint32List values,
                             optional GLuint srcOffset = 0);

    undefined clearBufferfi(GLenum buffer, GLint drawbuffer, GLfloat depth, GLint stencil);

    /* Query Objects */
    WebGLQuery createQuery();
    undefined deleteQuery(WebGLQuery? query);
    [WebGLHandlesContextLoss] GLboolean isQuery(WebGLQuery? query);
    undefined beginQuery(GLenum target, WebGLQuery query);
    undefined endQuery(GLenum target);
    any getQuery(GLenum target, GLenum pname);
    any getQueryParameter(WebGLQuery query, GLenum pname);

    /* Sampler Objects */
    WebGLSampler createSampler();
    undefined deleteSampler(WebGLSampler? sampler);
    [WebGLHandlesContextLoss] GLboolean isSampler(WebGLSampler? sampler);
    undefined bindSampler(GLuint unit, WebGLSampler? sampler);
    undefined samplerParameteri(WebGLSampler sampler, GLenum pname, GLint param);
    undefined samplerParameterf(WebGLSampler sampler, GLenum pname, GLfloat param);
    any getSamplerParameter(WebGLSampler sampler, GLenum pname);

    /* Sync objects */
    WebGLSync? fenceSync(GLenum condition, GLbitfield flags);
    [WebGLHandlesContextLoss] GLboolean isSync(WebGLSync? sync);
    undefined deleteSync(WebGLSync? sync);
    GLenum clientWaitSync(WebGLSync sync, GLbitfield flags, GLuint64 timeout);
    undefined waitSync(WebGLSync sync, GLbitfield flags, GLint64 timeout);
    any getSyncParameter(WebGLSync sync, GLenum pname);

    /* Transform Feedback */
    WebGLTransformFeedback createTransformFeedback();
    undefined deleteTransformFeedback(WebGLTransformFeedback? tf);
    [WebGLHandlesContextLoss] GLboolean isTransformFeedback(WebGLTransformFeedback? tf);
    undefined bindTransformFeedback(GLenum target, WebGLTransformFeedback? tf);
    undefined beginTransformFeedback(GLenum primitiveMode);
    undefined endTransformFeedback();
    undefined transformFeedbackVaryings(WebGLProgram program, sequence<DOMString> varyings, GLenum bufferMode);
    [NewObject]
    WebGLActiveInfo? getTransformFeedbackVarying(WebGLProgram program, GLuint index);
    undefined pauseTransformFeedback();
    undefined resumeTransformFeedback();

    /* Uniform Buffer Objects and Transform Feedback Buffers */
    undefined bindBufferBase(GLenum target, GLuint index, WebGLBuffer? buffer);
    undefined bindBufferRange(GLenum target, GLuint index, WebGLBuffer? buffer, GLintptr offset, GLsizeiptr size);
    [Throws] // GetOrCreateDOMReflector can fail.
    any getIndexedParameter(GLenum target, GLuint index);
    sequence<GLuint>? getUniformIndices(WebGLProgram program, sequence<DOMString> uniformNames);
    any getActiveUniforms(WebGLProgram program, sequence<GLuint> uniformIndices, GLenum pname);
    GLuint getUniformBlockIndex(WebGLProgram program, DOMString uniformBlockName);
    [Throws] // Creating a Uint32Array can fail.
    any getActiveUniformBlockParameter(WebGLProgram program, GLuint uniformBlockIndex, GLenum pname);
    DOMString? getActiveUniformBlockName(WebGLProgram program, GLuint uniformBlockIndex);
    undefined uniformBlockBinding(WebGLProgram program, GLuint uniformBlockIndex, GLuint uniformBlockBinding);

    /* Vertex Array Objects */
    WebGLVertexArrayObject createVertexArray();
    undefined deleteVertexArray(WebGLVertexArrayObject? vertexArray);
    [WebGLHandlesContextLoss] GLboolean isVertexArray(WebGLVertexArrayObject? vertexArray);
    undefined bindVertexArray(WebGLVertexArrayObject? array);
};

WebGL2RenderingContext includes WebGLRenderingContextBase;
WebGL2RenderingContext includes WebGL2RenderingContextBase;

[LegacyNoInterfaceObject,
 Exposed=(Window,Worker)]
interface EXT_color_buffer_float {
};

[LegacyNoInterfaceObject,
 Exposed=(Window,Worker)]
interface OVR_multiview2 {
    const GLenum FRAMEBUFFER_ATTACHMENT_TEXTURE_NUM_VIEWS_OVR = 0x9630;
    const GLenum FRAMEBUFFER_ATTACHMENT_TEXTURE_BASE_VIEW_INDEX_OVR = 0x9632;
    const GLenum MAX_VIEWS_OVR = 0x9631;
    const GLenum FRAMEBUFFER_INCOMPLETE_VIEW_TARGETS_OVR = 0x9633;

    undefined framebufferTextureMultiviewOVR(GLenum target, GLenum attachment, WebGLTexture? texture, GLint level, GLint baseViewIndex, GLsizei numViews);
};
/* parts/WebGLContextEvent.webidl ----------------------------------------------------- */
/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */

/* Castle Game Engine notes:
   This WEBIDL was cut down to our needs.
   This is not the original (complete) WEBIDL file,
   if you want a complete file get it from
   https://hg.mozilla.org/mozilla-central/raw-file/tip/dom/webidl/

   -------------------------------------------------------------------------------
*/

/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.

 * The origin of this IDL file is
 * https://www.khronos.org/registry/webgl/specs/latest/1.0/#fire-a-webgl-context-event
 */

[Exposed=(Window,Worker)]
interface WebGLContextEvent : Event {
  constructor(DOMString type, optional WebGLContextEventInit eventInit = {});

  readonly attribute DOMString statusMessage;
};

// EventInit is defined in the DOM4 specification.
dictionary WebGLContextEventInit : EventInit {
  DOMString statusMessage = "";
};
/* parts/WebGLRenderingContext.webidl ----------------------------------------------------- */
/* -*- Mode: IDL; tab-width: 4; indent-tabs-mode: nil; c-basic-offset: 4 -*- */
/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is
 * https://www.khronos.org/registry/webgl/specs/latest/webgl.idl
 *
 * Copyright © 2012 Khronos Group
 */

// WebGL IDL definitions scraped from the Khronos specification:
// https://www.khronos.org/registry/webgl/specs/latest/
//
// This IDL depends on the typed array specification defined at:
// https://www.khronos.org/registry/typedarray/specs/latest/typedarrays.idl

typedef unsigned long  GLenum;
typedef boolean        GLboolean;
typedef unsigned long  GLbitfield;
typedef byte           GLbyte;         /* 'byte' should be a signed 8 bit type. */
typedef short          GLshort;
typedef long           GLint;
typedef long           GLsizei;
typedef long long      GLintptr;
typedef long long      GLsizeiptr;
// Ideally the typedef below would use 'unsigned byte', but that doesn't currently exist in Web IDL.
typedef octet          GLubyte;        /* 'octet' should be an unsigned 8 bit type. */
typedef unsigned short GLushort;
typedef unsigned long  GLuint;
typedef unrestricted float GLfloat;
typedef unrestricted float GLclampf;
typedef unsigned long long GLuint64EXT;

// The power preference settings are documented in the WebGLContextAttributes
// section of the specification.
enum WebGLPowerPreference { "default", "low-power", "high-performance" };
enum PredefinedColorSpace { "srgb", "display-p3" };

[GenerateInit]
dictionary WebGLContextAttributes {
    // We deviate from the spec for alpha and antialias:
    // * alpha: Historically, we might use rgb565 instead of rgb(x)8, for
    //          memory bandwidth optimization.
    // * antialias: On Android, DPI is high and mem-bandwidth is low, so we
    //              default to antialias:false if it's not set.
    GLboolean alpha; // = true; // Default is controlled by webgl.default-no-alpha.
    GLboolean depth = true;
    GLboolean stencil = false;
    GLboolean antialias; // = true; // Default is controlled by webgl.default-antialias.
    GLboolean premultipliedAlpha = true;
    GLboolean preserveDrawingBuffer = false;
    GLboolean failIfMajorPerformanceCaveat = false;
    WebGLPowerPreference powerPreference = "default";

    [Func="nsRFPService::IsSystemPrincipalOrAboutFingerprintingProtection"]
    GLboolean forceSoftwareRendering = false;
};

[Exposed=(Window,Worker)]
interface WebGLBuffer {
};

[Exposed=(Window,Worker)]
interface WebGLFramebuffer {
};

[Exposed=(Window,Worker)]
interface WebGLProgram {
};

[Exposed=(Window,Worker)]
interface WebGLRenderbuffer {
};

[Exposed=(Window,Worker)]
interface WebGLShader {
};

[Exposed=(Window,Worker)]
interface WebGLTexture {
};

[Exposed=(Window,Worker)]
interface WebGLUniformLocation {
};

[Exposed=(Window,Worker)]
interface WebGLVertexArrayObject {
};

[Exposed=(Window,Worker)]
interface WebGLActiveInfo {
    readonly attribute GLint size;
    readonly attribute GLenum type;
    readonly attribute DOMString name;
};

[Exposed=(Window,Worker)]
interface WebGLShaderPrecisionFormat {
    readonly attribute GLint rangeMin;
    readonly attribute GLint rangeMax;
    readonly attribute GLint precision;
};

typedef ([AllowShared] Float32Array or sequence<GLfloat>) Float32List;
typedef ([AllowShared] Int32Array or sequence<GLint>) Int32List;

// Shared mixin for the things that WebGLRenderingContext and
// WebGL2RenderingContext have in common.  This doesn't have all the things they
// have in common, because we don't support splitting multiple overloads of the
// same method across separate interfaces and pulling them in with "includes".
[Exposed=(Window, Worker)]
interface mixin WebGLRenderingContextBase {
    /* ClearBufferMask */
    const GLenum DEPTH_BUFFER_BIT               = 0x00000100;
    const GLenum STENCIL_BUFFER_BIT             = 0x00000400;
    const GLenum COLOR_BUFFER_BIT               = 0x00004000;

    /* BeginMode */
    const GLenum POINTS                         = 0x0000;
    const GLenum LINES                          = 0x0001;
    const GLenum LINE_LOOP                      = 0x0002;
    const GLenum LINE_STRIP                     = 0x0003;
    const GLenum TRIANGLES                      = 0x0004;
    const GLenum TRIANGLE_STRIP                 = 0x0005;
    const GLenum TRIANGLE_FAN                   = 0x0006;

    /* AlphaFunction (not supported in ES20) */
    /*      NEVER */
    /*      LESS */
    /*      EQUAL */
    /*      LEQUAL */
    /*      GREATER */
    /*      NOTEQUAL */
    /*      GEQUAL */
    /*      ALWAYS */

    /* BlendingFactorDest */
    const GLenum ZERO                           = 0;
    const GLenum ONE                            = 1;
    const GLenum SRC_COLOR                      = 0x0300;
    const GLenum ONE_MINUS_SRC_COLOR            = 0x0301;
    const GLenum SRC_ALPHA                      = 0x0302;
    const GLenum ONE_MINUS_SRC_ALPHA            = 0x0303;
    const GLenum DST_ALPHA                      = 0x0304;
    const GLenum ONE_MINUS_DST_ALPHA            = 0x0305;

    /* BlendingFactorSrc */
    /*      ZERO */
    /*      ONE */
    const GLenum DST_COLOR                      = 0x0306;
    const GLenum ONE_MINUS_DST_COLOR            = 0x0307;
    const GLenum SRC_ALPHA_SATURATE             = 0x0308;
    /*      SRC_ALPHA */
    /*      ONE_MINUS_SRC_ALPHA */
    /*      DST_ALPHA */
    /*      ONE_MINUS_DST_ALPHA */

    /* BlendEquationSeparate */
    const GLenum FUNC_ADD                       = 0x8006;
    const GLenum BLEND_EQUATION                 = 0x8009;
    const GLenum BLEND_EQUATION_RGB             = 0x8009;   /* same as BLEND_EQUATION */
    const GLenum BLEND_EQUATION_ALPHA           = 0x883D;

    /* BlendSubtract */
    const GLenum FUNC_SUBTRACT                  = 0x800A;
    const GLenum FUNC_REVERSE_SUBTRACT          = 0x800B;

    /* Separate Blend Functions */
    const GLenum BLEND_DST_RGB                  = 0x80C8;
    const GLenum BLEND_SRC_RGB                  = 0x80C9;
    const GLenum BLEND_DST_ALPHA                = 0x80CA;
    const GLenum BLEND_SRC_ALPHA                = 0x80CB;
    const GLenum CONSTANT_COLOR                 = 0x8001;
    const GLenum ONE_MINUS_CONSTANT_COLOR       = 0x8002;
    const GLenum CONSTANT_ALPHA                 = 0x8003;
    const GLenum ONE_MINUS_CONSTANT_ALPHA       = 0x8004;
    const GLenum BLEND_COLOR                    = 0x8005;

    /* Buffer Objects */
    const GLenum ARRAY_BUFFER                   = 0x8892;
    const GLenum ELEMENT_ARRAY_BUFFER           = 0x8893;
    const GLenum ARRAY_BUFFER_BINDING           = 0x8894;
    const GLenum ELEMENT_ARRAY_BUFFER_BINDING   = 0x8895;

    const GLenum STREAM_DRAW                    = 0x88E0;
    const GLenum STATIC_DRAW                    = 0x88E4;
    const GLenum DYNAMIC_DRAW                   = 0x88E8;

    const GLenum BUFFER_SIZE                    = 0x8764;
    const GLenum BUFFER_USAGE                   = 0x8765;

    const GLenum CURRENT_VERTEX_ATTRIB          = 0x8626;

    /* CullFaceMode */
    const GLenum FRONT                          = 0x0404;
    const GLenum BACK                           = 0x0405;
    const GLenum FRONT_AND_BACK                 = 0x0408;

    /* DepthFunction */
    /*      NEVER */
    /*      LESS */
    /*      EQUAL */
    /*      LEQUAL */
    /*      GREATER */
    /*      NOTEQUAL */
    /*      GEQUAL */
    /*      ALWAYS */

    /* EnableCap */
    /* TEXTURE_2D */
    const GLenum CULL_FACE                      = 0x0B44;
    const GLenum BLEND                          = 0x0BE2;
    const GLenum DITHER                         = 0x0BD0;
    const GLenum STENCIL_TEST                   = 0x0B90;
    const GLenum DEPTH_TEST                     = 0x0B71;
    const GLenum SCISSOR_TEST                   = 0x0C11;
    const GLenum POLYGON_OFFSET_FILL            = 0x8037;
    const GLenum SAMPLE_ALPHA_TO_COVERAGE       = 0x809E;
    const GLenum SAMPLE_COVERAGE                = 0x80A0;

    /* ErrorCode */
    const GLenum NO_ERROR                       = 0;
    const GLenum INVALID_ENUM                   = 0x0500;
    const GLenum INVALID_VALUE                  = 0x0501;
    const GLenum INVALID_OPERATION              = 0x0502;
    const GLenum OUT_OF_MEMORY                  = 0x0505;

    /* FrontFaceDirection */
    const GLenum CW                             = 0x0900;
    const GLenum CCW                            = 0x0901;

    /* GetPName */
    const GLenum LINE_WIDTH                     = 0x0B21;
    const GLenum ALIASED_POINT_SIZE_RANGE       = 0x846D;
    const GLenum ALIASED_LINE_WIDTH_RANGE       = 0x846E;
    const GLenum CULL_FACE_MODE                 = 0x0B45;
    const GLenum FRONT_FACE                     = 0x0B46;
    const GLenum DEPTH_RANGE                    = 0x0B70;
    const GLenum DEPTH_WRITEMASK                = 0x0B72;
    const GLenum DEPTH_CLEAR_VALUE              = 0x0B73;
    const GLenum DEPTH_FUNC                     = 0x0B74;
    const GLenum STENCIL_CLEAR_VALUE            = 0x0B91;
    const GLenum STENCIL_FUNC                   = 0x0B92;
    const GLenum STENCIL_FAIL                   = 0x0B94;
    const GLenum STENCIL_PASS_DEPTH_FAIL        = 0x0B95;
    const GLenum STENCIL_PASS_DEPTH_PASS        = 0x0B96;
    const GLenum STENCIL_REF                    = 0x0B97;
    const GLenum STENCIL_VALUE_MASK             = 0x0B93;
    const GLenum STENCIL_WRITEMASK              = 0x0B98;
    const GLenum STENCIL_BACK_FUNC              = 0x8800;
    const GLenum STENCIL_BACK_FAIL              = 0x8801;
    const GLenum STENCIL_BACK_PASS_DEPTH_FAIL   = 0x8802;
    const GLenum STENCIL_BACK_PASS_DEPTH_PASS   = 0x8803;
    const GLenum STENCIL_BACK_REF               = 0x8CA3;
    const GLenum STENCIL_BACK_VALUE_MASK        = 0x8CA4;
    const GLenum STENCIL_BACK_WRITEMASK         = 0x8CA5;
    const GLenum VIEWPORT                       = 0x0BA2;
    const GLenum SCISSOR_BOX                    = 0x0C10;
    /*      SCISSOR_TEST */
    const GLenum COLOR_CLEAR_VALUE              = 0x0C22;
    const GLenum COLOR_WRITEMASK                = 0x0C23;
    const GLenum UNPACK_ALIGNMENT               = 0x0CF5;
    const GLenum PACK_ALIGNMENT                 = 0x0D05;
    const GLenum MAX_TEXTURE_SIZE               = 0x0D33;
    const GLenum MAX_VIEWPORT_DIMS              = 0x0D3A;
    const GLenum SUBPIXEL_BITS                  = 0x0D50;
    const GLenum RED_BITS                       = 0x0D52;
    const GLenum GREEN_BITS                     = 0x0D53;
    const GLenum BLUE_BITS                      = 0x0D54;
    const GLenum ALPHA_BITS                     = 0x0D55;
    const GLenum DEPTH_BITS                     = 0x0D56;
    const GLenum STENCIL_BITS                   = 0x0D57;
    const GLenum POLYGON_OFFSET_UNITS           = 0x2A00;
    /*      POLYGON_OFFSET_FILL */
    const GLenum POLYGON_OFFSET_FACTOR          = 0x8038;
    const GLenum TEXTURE_BINDING_2D             = 0x8069;
    const GLenum SAMPLE_BUFFERS                 = 0x80A8;
    const GLenum SAMPLES                        = 0x80A9;
    const GLenum SAMPLE_COVERAGE_VALUE          = 0x80AA;
    const GLenum SAMPLE_COVERAGE_INVERT         = 0x80AB;

    /* GetTextureParameter */
    /*      TEXTURE_MAG_FILTER */
    /*      TEXTURE_MIN_FILTER */
    /*      TEXTURE_WRAP_S */
    /*      TEXTURE_WRAP_T */

    const GLenum COMPRESSED_TEXTURE_FORMATS     = 0x86A3;

    /* HintMode */
    const GLenum DONT_CARE                      = 0x1100;
    const GLenum FASTEST                        = 0x1101;
    const GLenum NICEST                         = 0x1102;

    /* HintTarget */
    const GLenum GENERATE_MIPMAP_HINT            = 0x8192;

    /* DataType */
    const GLenum BYTE                           = 0x1400;
    const GLenum UNSIGNED_BYTE                  = 0x1401;
    const GLenum SHORT                          = 0x1402;
    const GLenum UNSIGNED_SHORT                 = 0x1403;
    const GLenum INT                            = 0x1404;
    const GLenum UNSIGNED_INT                   = 0x1405;
    const GLenum FLOAT                          = 0x1406;

    /* PixelFormat */
    const GLenum DEPTH_COMPONENT                = 0x1902;
    const GLenum ALPHA                          = 0x1906;
    const GLenum RGB                            = 0x1907;
    const GLenum RGBA                           = 0x1908;
    const GLenum LUMINANCE                      = 0x1909;
    const GLenum LUMINANCE_ALPHA                = 0x190A;

    /* PixelType */
    /*      UNSIGNED_BYTE */
    const GLenum UNSIGNED_SHORT_4_4_4_4         = 0x8033;
    const GLenum UNSIGNED_SHORT_5_5_5_1         = 0x8034;
    const GLenum UNSIGNED_SHORT_5_6_5           = 0x8363;

    /* Shaders */
    const GLenum FRAGMENT_SHADER                  = 0x8B30;
    const GLenum VERTEX_SHADER                    = 0x8B31;
    const GLenum MAX_VERTEX_ATTRIBS               = 0x8869;
    const GLenum MAX_VERTEX_UNIFORM_VECTORS       = 0x8DFB;
    const GLenum MAX_VARYING_VECTORS              = 0x8DFC;
    const GLenum MAX_COMBINED_TEXTURE_IMAGE_UNITS = 0x8B4D;
    const GLenum MAX_VERTEX_TEXTURE_IMAGE_UNITS   = 0x8B4C;
    const GLenum MAX_TEXTURE_IMAGE_UNITS          = 0x8872;
    const GLenum MAX_FRAGMENT_UNIFORM_VECTORS     = 0x8DFD;
    const GLenum SHADER_TYPE                      = 0x8B4F;
    const GLenum DELETE_STATUS                    = 0x8B80;
    const GLenum LINK_STATUS                      = 0x8B82;
    const GLenum VALIDATE_STATUS                  = 0x8B83;
    const GLenum ATTACHED_SHADERS                 = 0x8B85;
    const GLenum ACTIVE_UNIFORMS                  = 0x8B86;
    const GLenum ACTIVE_ATTRIBUTES                = 0x8B89;
    const GLenum SHADING_LANGUAGE_VERSION         = 0x8B8C;
    const GLenum CURRENT_PROGRAM                  = 0x8B8D;

    /* StencilFunction */
    const GLenum NEVER                          = 0x0200;
    const GLenum LESS                           = 0x0201;
    const GLenum EQUAL                          = 0x0202;
    const GLenum LEQUAL                         = 0x0203;
    const GLenum GREATER                        = 0x0204;
    const GLenum NOTEQUAL                       = 0x0205;
    const GLenum GEQUAL                         = 0x0206;
    const GLenum ALWAYS                         = 0x0207;

    /* StencilOp */
    /*      ZERO */
    const GLenum KEEP                           = 0x1E00;
    const GLenum REPLACE                        = 0x1E01;
    const GLenum INCR                           = 0x1E02;
    const GLenum DECR                           = 0x1E03;
    const GLenum INVERT                         = 0x150A;
    const GLenum INCR_WRAP                      = 0x8507;
    const GLenum DECR_WRAP                      = 0x8508;

    /* StringName */
    const GLenum VENDOR                         = 0x1F00;
    const GLenum RENDERER                       = 0x1F01;
    const GLenum VERSION                        = 0x1F02;

    /* TextureMagFilter */
    const GLenum NEAREST                        = 0x2600;
    const GLenum LINEAR                         = 0x2601;

    /* TextureMinFilter */
    /*      NEAREST */
    /*      LINEAR */
    const GLenum NEAREST_MIPMAP_NEAREST         = 0x2700;
    const GLenum LINEAR_MIPMAP_NEAREST          = 0x2701;
    const GLenum NEAREST_MIPMAP_LINEAR          = 0x2702;
    const GLenum LINEAR_MIPMAP_LINEAR           = 0x2703;

    /* TextureParameterName */
    const GLenum TEXTURE_MAG_FILTER             = 0x2800;
    const GLenum TEXTURE_MIN_FILTER             = 0x2801;
    const GLenum TEXTURE_WRAP_S                 = 0x2802;
    const GLenum TEXTURE_WRAP_T                 = 0x2803;

    /* TextureTarget */
    const GLenum TEXTURE_2D                     = 0x0DE1;
    const GLenum TEXTURE                        = 0x1702;

    const GLenum TEXTURE_CUBE_MAP               = 0x8513;
    const GLenum TEXTURE_BINDING_CUBE_MAP       = 0x8514;
    const GLenum TEXTURE_CUBE_MAP_POSITIVE_X    = 0x8515;
    const GLenum TEXTURE_CUBE_MAP_NEGATIVE_X    = 0x8516;
    const GLenum TEXTURE_CUBE_MAP_POSITIVE_Y    = 0x8517;
    const GLenum TEXTURE_CUBE_MAP_NEGATIVE_Y    = 0x8518;
    const GLenum TEXTURE_CUBE_MAP_POSITIVE_Z    = 0x8519;
    const GLenum TEXTURE_CUBE_MAP_NEGATIVE_Z    = 0x851A;
    const GLenum MAX_CUBE_MAP_TEXTURE_SIZE      = 0x851C;

    /* TextureUnit */
    const GLenum TEXTURE0                       = 0x84C0;
    const GLenum TEXTURE1                       = 0x84C1;
    const GLenum TEXTURE2                       = 0x84C2;
    const GLenum TEXTURE3                       = 0x84C3;
    const GLenum TEXTURE4                       = 0x84C4;
    const GLenum TEXTURE5                       = 0x84C5;
    const GLenum TEXTURE6                       = 0x84C6;
    const GLenum TEXTURE7                       = 0x84C7;
    const GLenum TEXTURE8                       = 0x84C8;
    const GLenum TEXTURE9                       = 0x84C9;
    const GLenum TEXTURE10                      = 0x84CA;
    const GLenum TEXTURE11                      = 0x84CB;
    const GLenum TEXTURE12                      = 0x84CC;
    const GLenum TEXTURE13                      = 0x84CD;
    const GLenum TEXTURE14                      = 0x84CE;
    const GLenum TEXTURE15                      = 0x84CF;
    const GLenum TEXTURE16                      = 0x84D0;
    const GLenum TEXTURE17                      = 0x84D1;
    const GLenum TEXTURE18                      = 0x84D2;
    const GLenum TEXTURE19                      = 0x84D3;
    const GLenum TEXTURE20                      = 0x84D4;
    const GLenum TEXTURE21                      = 0x84D5;
    const GLenum TEXTURE22                      = 0x84D6;
    const GLenum TEXTURE23                      = 0x84D7;
    const GLenum TEXTURE24                      = 0x84D8;
    const GLenum TEXTURE25                      = 0x84D9;
    const GLenum TEXTURE26                      = 0x84DA;
    const GLenum TEXTURE27                      = 0x84DB;
    const GLenum TEXTURE28                      = 0x84DC;
    const GLenum TEXTURE29                      = 0x84DD;
    const GLenum TEXTURE30                      = 0x84DE;
    const GLenum TEXTURE31                      = 0x84DF;
    const GLenum ACTIVE_TEXTURE                 = 0x84E0;

    /* TextureWrapMode */
    const GLenum REPEAT                         = 0x2901;
    const GLenum CLAMP_TO_EDGE                  = 0x812F;
    const GLenum MIRRORED_REPEAT                = 0x8370;

    /* Uniform Types */
    const GLenum FLOAT_VEC2                     = 0x8B50;
    const GLenum FLOAT_VEC3                     = 0x8B51;
    const GLenum FLOAT_VEC4                     = 0x8B52;
    const GLenum INT_VEC2                       = 0x8B53;
    const GLenum INT_VEC3                       = 0x8B54;
    const GLenum INT_VEC4                       = 0x8B55;
    const GLenum BOOL                           = 0x8B56;
    const GLenum BOOL_VEC2                      = 0x8B57;
    const GLenum BOOL_VEC3                      = 0x8B58;
    const GLenum BOOL_VEC4                      = 0x8B59;
    const GLenum FLOAT_MAT2                     = 0x8B5A;
    const GLenum FLOAT_MAT3                     = 0x8B5B;
    const GLenum FLOAT_MAT4                     = 0x8B5C;
    const GLenum SAMPLER_2D                     = 0x8B5E;
    const GLenum SAMPLER_CUBE                   = 0x8B60;

    /* Vertex Arrays */
    const GLenum VERTEX_ATTRIB_ARRAY_ENABLED        = 0x8622;
    const GLenum VERTEX_ATTRIB_ARRAY_SIZE           = 0x8623;
    const GLenum VERTEX_ATTRIB_ARRAY_STRIDE         = 0x8624;
    const GLenum VERTEX_ATTRIB_ARRAY_TYPE           = 0x8625;
    const GLenum VERTEX_ATTRIB_ARRAY_NORMALIZED     = 0x886A;
    const GLenum VERTEX_ATTRIB_ARRAY_POINTER        = 0x8645;
    const GLenum VERTEX_ATTRIB_ARRAY_BUFFER_BINDING = 0x889F;

    /* Read Format */
    const GLenum IMPLEMENTATION_COLOR_READ_TYPE   = 0x8B9A;
    const GLenum IMPLEMENTATION_COLOR_READ_FORMAT = 0x8B9B;

    /* Shader Source */
    const GLenum COMPILE_STATUS                 = 0x8B81;

    /* Shader Precision-Specified Types */
    const GLenum LOW_FLOAT                      = 0x8DF0;
    const GLenum MEDIUM_FLOAT                   = 0x8DF1;
    const GLenum HIGH_FLOAT                     = 0x8DF2;
    const GLenum LOW_INT                        = 0x8DF3;
    const GLenum MEDIUM_INT                     = 0x8DF4;
    const GLenum HIGH_INT                       = 0x8DF5;

    /* Framebuffer Object. */
    const GLenum FRAMEBUFFER                    = 0x8D40;
    const GLenum RENDERBUFFER                   = 0x8D41;

    const GLenum RGBA4                          = 0x8056;
    const GLenum RGB5_A1                        = 0x8057;
    const GLenum RGB565                         = 0x8D62;
    const GLenum DEPTH_COMPONENT16              = 0x81A5;
    const GLenum STENCIL_INDEX8                 = 0x8D48;
    const GLenum DEPTH_STENCIL                  = 0x84F9;

    const GLenum RENDERBUFFER_WIDTH             = 0x8D42;
    const GLenum RENDERBUFFER_HEIGHT            = 0x8D43;
    const GLenum RENDERBUFFER_INTERNAL_FORMAT   = 0x8D44;
    const GLenum RENDERBUFFER_RED_SIZE          = 0x8D50;
    const GLenum RENDERBUFFER_GREEN_SIZE        = 0x8D51;
    const GLenum RENDERBUFFER_BLUE_SIZE         = 0x8D52;
    const GLenum RENDERBUFFER_ALPHA_SIZE        = 0x8D53;
    const GLenum RENDERBUFFER_DEPTH_SIZE        = 0x8D54;
    const GLenum RENDERBUFFER_STENCIL_SIZE      = 0x8D55;

    const GLenum FRAMEBUFFER_ATTACHMENT_OBJECT_TYPE           = 0x8CD0;
    const GLenum FRAMEBUFFER_ATTACHMENT_OBJECT_NAME           = 0x8CD1;
    const GLenum FRAMEBUFFER_ATTACHMENT_TEXTURE_LEVEL         = 0x8CD2;
    const GLenum FRAMEBUFFER_ATTACHMENT_TEXTURE_CUBE_MAP_FACE = 0x8CD3;

    const GLenum COLOR_ATTACHMENT0              = 0x8CE0;
    const GLenum DEPTH_ATTACHMENT               = 0x8D00;
    const GLenum STENCIL_ATTACHMENT             = 0x8D20;
    const GLenum DEPTH_STENCIL_ATTACHMENT       = 0x821A;

    const GLenum NONE                           = 0;

    const GLenum FRAMEBUFFER_COMPLETE                      = 0x8CD5;
    const GLenum FRAMEBUFFER_INCOMPLETE_ATTACHMENT         = 0x8CD6;
    const GLenum FRAMEBUFFER_INCOMPLETE_MISSING_ATTACHMENT = 0x8CD7;
    const GLenum FRAMEBUFFER_INCOMPLETE_DIMENSIONS         = 0x8CD9;
    const GLenum FRAMEBUFFER_UNSUPPORTED                   = 0x8CDD;

    const GLenum FRAMEBUFFER_BINDING            = 0x8CA6;
    const GLenum RENDERBUFFER_BINDING           = 0x8CA7;
    const GLenum MAX_RENDERBUFFER_SIZE          = 0x84E8;

    const GLenum INVALID_FRAMEBUFFER_OPERATION  = 0x0506;

    /* WebGL-specific enums */
    const GLenum UNPACK_FLIP_Y_WEBGL            = 0x9240;
    const GLenum UNPACK_PREMULTIPLY_ALPHA_WEBGL = 0x9241;
    const GLenum CONTEXT_LOST_WEBGL             = 0x9242;
    const GLenum UNPACK_COLORSPACE_CONVERSION_WEBGL = 0x9243;
    const GLenum BROWSER_DEFAULT_WEBGL          = 0x9244;

    // The canvas might actually be null in some cases, apparently.
    readonly attribute CanvasSource? canvas;
    readonly attribute GLsizei drawingBufferWidth;
    readonly attribute GLsizei drawingBufferHeight;

    /* Upon context creation, drawingBufferColorSpace and unpackColorSpace both
       default to the value "srgb". */
    [Pref="webgl.drawing_buffer_color_space"]
    attribute PredefinedColorSpace drawingBufferColorSpace;
    attribute PredefinedColorSpace unpackColorSpace;

    [WebGLHandlesContextLoss] WebGLContextAttributes? getContextAttributes();
    [WebGLHandlesContextLoss] boolean isContextLost();

    [NeedsCallerType]
    sequence<DOMString>? getSupportedExtensions();

    [Throws, NeedsCallerType]
    object? getExtension(DOMString name);

    undefined activeTexture(GLenum texture);
    undefined attachShader(WebGLProgram program, WebGLShader shader);
    undefined bindAttribLocation(WebGLProgram program, GLuint index, DOMString name);
    undefined bindBuffer(GLenum target, WebGLBuffer? buffer);
    undefined bindFramebuffer(GLenum target, WebGLFramebuffer? framebuffer);
    undefined bindRenderbuffer(GLenum target, WebGLRenderbuffer? renderbuffer);
    undefined bindTexture(GLenum target, WebGLTexture? texture);
    undefined blendColor(GLfloat red, GLfloat green, GLfloat blue, GLfloat alpha);
    undefined blendEquation(GLenum mode);
    undefined blendEquationSeparate(GLenum modeRGB, GLenum modeAlpha);
    undefined blendFunc(GLenum sfactor, GLenum dfactor);
    undefined blendFuncSeparate(GLenum srcRGB, GLenum dstRGB,
                                GLenum srcAlpha, GLenum dstAlpha);

    [WebGLHandlesContextLoss] GLenum checkFramebufferStatus(GLenum target);
    undefined clear(GLbitfield mask);
    undefined clearColor(GLfloat red, GLfloat green, GLfloat blue, GLfloat alpha);
    undefined clearDepth(GLclampf depth);
    undefined clearStencil(GLint s);
    undefined colorMask(GLboolean red, GLboolean green, GLboolean blue, GLboolean alpha);
    undefined compileShader(WebGLShader shader);

    undefined copyTexImage2D(GLenum target, GLint level, GLenum internalformat,
                             GLint x, GLint y, GLsizei width, GLsizei height,
                             GLint border);
    undefined copyTexSubImage2D(GLenum target, GLint level, GLint xoffset, GLint yoffset,
                                GLint x, GLint y, GLsizei width, GLsizei height);

    WebGLBuffer createBuffer();
    WebGLFramebuffer createFramebuffer();
    WebGLProgram createProgram();
    WebGLRenderbuffer createRenderbuffer();
    WebGLShader? createShader(GLenum type);
    WebGLTexture createTexture();

    undefined cullFace(GLenum mode);

    undefined deleteBuffer(WebGLBuffer? buffer);
    undefined deleteFramebuffer(WebGLFramebuffer? framebuffer);
    undefined deleteProgram(WebGLProgram? program);
    undefined deleteRenderbuffer(WebGLRenderbuffer? renderbuffer);
    undefined deleteShader(WebGLShader? shader);
    undefined deleteTexture(WebGLTexture? texture);

    undefined depthFunc(GLenum func);
    undefined depthMask(GLboolean flag);
    undefined depthRange(GLclampf zNear, GLclampf zFar);
    undefined detachShader(WebGLProgram program, WebGLShader shader);
    undefined disable(GLenum cap);
    undefined disableVertexAttribArray(GLuint index);
    undefined drawArrays(GLenum mode, GLint first, GLsizei count);
    undefined drawElements(GLenum mode, GLsizei count, GLenum type, GLintptr offset);

    undefined enable(GLenum cap);
    undefined enableVertexAttribArray(GLuint index);
    undefined finish();
    undefined flush();
    undefined framebufferRenderbuffer(GLenum target, GLenum attachment,
                                      GLenum renderbuffertarget,
                                      WebGLRenderbuffer? renderbuffer);
    undefined framebufferTexture2D(GLenum target, GLenum attachment, GLenum textarget,
                                   WebGLTexture? texture, GLint level);
    undefined frontFace(GLenum mode);

    undefined generateMipmap(GLenum target);

    [NewObject]
    WebGLActiveInfo? getActiveAttrib(WebGLProgram program, GLuint index);
    [NewObject]
    WebGLActiveInfo? getActiveUniform(WebGLProgram program, GLuint index);

    sequence<WebGLShader>? getAttachedShaders(WebGLProgram program);

    [WebGLHandlesContextLoss] GLint getAttribLocation(WebGLProgram program, DOMString name);

    any getBufferParameter(GLenum target, GLenum pname);
    [Throws]
    any getParameter(GLenum pname);

    [WebGLHandlesContextLoss] GLenum getError();

    [Throws]
    any getFramebufferAttachmentParameter(GLenum target, GLenum attachment,
                                          GLenum pname);
    any getProgramParameter(WebGLProgram program, GLenum pname);
    DOMString? getProgramInfoLog(WebGLProgram program);
    any getRenderbufferParameter(GLenum target, GLenum pname);
    any getShaderParameter(WebGLShader shader, GLenum pname);

    [NewObject]
    WebGLShaderPrecisionFormat? getShaderPrecisionFormat(GLenum shadertype, GLenum precisiontype);

    DOMString? getShaderInfoLog(WebGLShader shader);

    DOMString? getShaderSource(WebGLShader shader);

    any getTexParameter(GLenum target, GLenum pname);

    any getUniform(WebGLProgram program, WebGLUniformLocation location);

    [NewObject]
    WebGLUniformLocation? getUniformLocation(WebGLProgram program, DOMString name);

    [Throws]
    any getVertexAttrib(GLuint index, GLenum pname);

    [WebGLHandlesContextLoss] GLintptr getVertexAttribOffset(GLuint index, GLenum pname);

    undefined hint(GLenum target, GLenum mode);
    [WebGLHandlesContextLoss] GLboolean isBuffer(WebGLBuffer? buffer);
    [WebGLHandlesContextLoss] GLboolean isEnabled(GLenum cap);
    [WebGLHandlesContextLoss] GLboolean isFramebuffer(WebGLFramebuffer? framebuffer);
    [WebGLHandlesContextLoss] GLboolean isProgram(WebGLProgram? program);
    [WebGLHandlesContextLoss] GLboolean isRenderbuffer(WebGLRenderbuffer? renderbuffer);
    [WebGLHandlesContextLoss] GLboolean isShader(WebGLShader? shader);
    [WebGLHandlesContextLoss] GLboolean isTexture(WebGLTexture? texture);
    undefined lineWidth(GLfloat width);
    undefined linkProgram(WebGLProgram program);
    undefined pixelStorei(GLenum pname, GLint param);
    undefined polygonOffset(GLfloat factor, GLfloat units);

    undefined renderbufferStorage(GLenum target, GLenum internalformat,
                             GLsizei width, GLsizei height);
    undefined sampleCoverage(GLclampf value, GLboolean invert);
    undefined scissor(GLint x, GLint y, GLsizei width, GLsizei height);

    undefined shaderSource(WebGLShader shader, DOMString source);

    undefined stencilFunc(GLenum func, GLint ref, GLuint mask);
    undefined stencilFuncSeparate(GLenum face, GLenum func, GLint ref, GLuint mask);
    undefined stencilMask(GLuint mask);
    undefined stencilMaskSeparate(GLenum face, GLuint mask);
    undefined stencilOp(GLenum fail, GLenum zfail, GLenum zpass);
    undefined stencilOpSeparate(GLenum face, GLenum fail, GLenum zfail, GLenum zpass);

    undefined texParameterf(GLenum target, GLenum pname, GLfloat param);
    undefined texParameteri(GLenum target, GLenum pname, GLint param);

    undefined uniform1f(WebGLUniformLocation? location, GLfloat x);
    undefined uniform2f(WebGLUniformLocation? location, GLfloat x, GLfloat y);
    undefined uniform3f(WebGLUniformLocation? location, GLfloat x, GLfloat y, GLfloat z);
    undefined uniform4f(WebGLUniformLocation? location, GLfloat x, GLfloat y, GLfloat z, GLfloat w);

    undefined uniform1i(WebGLUniformLocation? location, GLint x);
    undefined uniform2i(WebGLUniformLocation? location, GLint x, GLint y);
    undefined uniform3i(WebGLUniformLocation? location, GLint x, GLint y, GLint z);
    undefined uniform4i(WebGLUniformLocation? location, GLint x, GLint y, GLint z, GLint w);

    undefined useProgram(WebGLProgram? program);
    undefined validateProgram(WebGLProgram program);

    undefined vertexAttrib1f(GLuint indx, GLfloat x);
    undefined vertexAttrib1fv(GLuint indx, Float32List values);
    undefined vertexAttrib2f(GLuint indx, GLfloat x, GLfloat y);
    undefined vertexAttrib2fv(GLuint indx, Float32List values);
    undefined vertexAttrib3f(GLuint indx, GLfloat x, GLfloat y, GLfloat z);
    undefined vertexAttrib3fv(GLuint indx, Float32List values);
    undefined vertexAttrib4f(GLuint indx, GLfloat x, GLfloat y, GLfloat z, GLfloat w);
    undefined vertexAttrib4fv(GLuint indx, Float32List values);
    undefined vertexAttribPointer(GLuint indx, GLint size, GLenum type,
                                  GLboolean normalized, GLsizei stride, GLintptr offset);

    undefined viewport(GLint x, GLint y, GLsizei width, GLsizei height);
};

[Exposed=(Window,Worker)]
interface WebGLRenderingContext {
    // bufferData has WebGL2 overloads.
    undefined bufferData(GLenum target, GLsizeiptr size, GLenum usage);
    undefined bufferData(GLenum target, [AllowShared] ArrayBuffer? data, GLenum usage);
    undefined bufferData(GLenum target, [AllowShared] ArrayBufferView data, GLenum usage);
    // bufferSubData has WebGL2 overloads.
    undefined bufferSubData(GLenum target, GLintptr offset, [AllowShared] ArrayBuffer data);
    undefined bufferSubData(GLenum target, GLintptr offset, [AllowShared] ArrayBufferView data);

    // compressedTexImage2D has WebGL2 overloads.
    undefined compressedTexImage2D(GLenum target, GLint level, GLenum internalformat,
                                   GLsizei width, GLsizei height, GLint border,
                                   [AllowShared] ArrayBufferView data);
    // compressedTexSubImage2D has WebGL2 overloads.
    undefined compressedTexSubImage2D(GLenum target, GLint level,
                                      GLint xoffset, GLint yoffset,
                                      GLsizei width, GLsizei height, GLenum format,
                                      [AllowShared] ArrayBufferView data);

    // readPixels has WebGL2 overloads.
    [Throws, NeedsCallerType]
    undefined readPixels(GLint x, GLint y, GLsizei width, GLsizei height,
                    GLenum format, GLenum type, [AllowShared] ArrayBufferView? pixels);

    // texImage2D has WebGL2 overloads.
    // Overloads must share [Throws].
    [Throws] // Can't actually throw.
    undefined texImage2D(GLenum target, GLint level, GLint internalformat,
                         GLsizei width, GLsizei height, GLint border, GLenum format,
                         GLenum type, [AllowShared] ArrayBufferView? pixels);
    [Throws] // Can't actually throw.
    undefined texImage2D(GLenum target, GLint level, GLint internalformat,
                         GLenum format, GLenum type, ImageBitmap pixels);
    [Throws] // Can't actually throw.
    undefined texImage2D(GLenum target, GLint level, GLint internalformat,
                         GLenum format, GLenum type, ImageData pixels);
    [Throws]
    undefined texImage2D(GLenum target, GLint level, GLint internalformat,
                         GLenum format, GLenum type, HTMLImageElement image); // May throw DOMException
    [Throws]
    undefined texImage2D(GLenum target, GLint level, GLint internalformat,
                         GLenum format, GLenum type, HTMLCanvasElement canvas); // May throw DOMException
    [Throws]
    undefined texImage2D(GLenum target, GLint level, GLint internalformat,
                         GLenum format, GLenum type, HTMLVideoElement video); // May throw DOMException
    [Throws]
    undefined texImage2D(GLenum target, GLint level, GLint internalformat,
                         GLenum format, GLenum type, OffscreenCanvas canvas); // May throw DOMException
    [Throws]
    undefined texImage2D(GLenum target, GLint level, GLint internalformat,
                         GLenum format, GLenum type, VideoFrame videoFrame); // May throw DOMException

    // texSubImage2D has WebGL2 overloads.
    [Throws] // Can't actually throw.
    undefined texSubImage2D(GLenum target, GLint level, GLint xoffset, GLint yoffset,
                            GLsizei width, GLsizei height,
                            GLenum format, GLenum type, [AllowShared] ArrayBufferView? pixels);
    [Throws] // Can't actually throw.
    undefined texSubImage2D(GLenum target, GLint level, GLint xoffset, GLint yoffset,
                            GLenum format, GLenum type, ImageBitmap pixels);
    [Throws] // Can't actually throw.
    undefined texSubImage2D(GLenum target, GLint level, GLint xoffset, GLint yoffset,
                            GLenum format, GLenum type, ImageData pixels);
    [Throws]
    undefined texSubImage2D(GLenum target, GLint level, GLint xoffset, GLint yoffset,
                            GLenum format, GLenum type, HTMLImageElement image); // May throw DOMException
    [Throws]
    undefined texSubImage2D(GLenum target, GLint level, GLint xoffset, GLint yoffset,
                            GLenum format, GLenum type, HTMLCanvasElement canvas); // May throw DOMException
    [Throws]
    undefined texSubImage2D(GLenum target, GLint level, GLint xoffset, GLint yoffset,
                            GLenum format, GLenum type, HTMLVideoElement video); // May throw DOMException
    [Throws]
    undefined texSubImage2D(GLenum target, GLint level, GLint xoffset, GLint yoffset,
                            GLenum format, GLenum type, OffscreenCanvas canvas); // May throw DOMException
    [Throws]
    undefined texSubImage2D(GLenum target, GLint level, GLint xoffset, GLint yoffset,
                            GLenum format, GLenum type, VideoFrame videoFrame); // May throw DOMException

    // uniform*fv have WebGL2 overloads, or rather extensions, that are not
    // distinguishable from the WebGL1 versions when called with two arguments.
    undefined uniform1fv(WebGLUniformLocation? location, Float32List data);
    undefined uniform2fv(WebGLUniformLocation? location, Float32List data);
    undefined uniform3fv(WebGLUniformLocation? location, Float32List data);
    undefined uniform4fv(WebGLUniformLocation? location, Float32List data);

    // uniform*iv have WebGL2 overloads, or rather extensions, that are not
    // distinguishable from the WebGL1 versions when called with two arguments.
    undefined uniform1iv(WebGLUniformLocation? location, Int32List data);
    undefined uniform2iv(WebGLUniformLocation? location, Int32List data);
    undefined uniform3iv(WebGLUniformLocation? location, Int32List data);
    undefined uniform4iv(WebGLUniformLocation? location, Int32List data);

    // uniformMatrix*fv have WebGL2 overloads, or rather extensions, that are
    // not distinguishable from the WebGL1 versions when called with two
    // arguments.
    undefined uniformMatrix2fv(WebGLUniformLocation? location, GLboolean transpose, Float32List data);
    undefined uniformMatrix3fv(WebGLUniformLocation? location, GLboolean transpose, Float32List data);
    undefined uniformMatrix4fv(WebGLUniformLocation? location, GLboolean transpose, Float32List data);
};

WebGLRenderingContext includes WebGLRenderingContextBase;

////////////////////////////////////////
// specific extension interfaces

[LegacyNoInterfaceObject,
 Exposed=(Window,Worker)]
interface EXT_texture_compression_bptc {
    const GLenum COMPRESSED_RGBA_BPTC_UNORM_EXT = 0x8E8C;
    const GLenum COMPRESSED_SRGB_ALPHA_BPTC_UNORM_EXT = 0x8E8D;
    const GLenum COMPRESSED_RGB_BPTC_SIGNED_FLOAT_EXT = 0x8E8E;
    const GLenum COMPRESSED_RGB_BPTC_UNSIGNED_FLOAT_EXT = 0x8E8F;
};

[LegacyNoInterfaceObject,
 Exposed=(Window,Worker)]
interface EXT_texture_compression_rgtc {
    const GLenum COMPRESSED_RED_RGTC1_EXT = 0x8DBB;
    const GLenum COMPRESSED_SIGNED_RED_RGTC1_EXT = 0x8DBC;
    const GLenum COMPRESSED_RED_GREEN_RGTC2_EXT = 0x8DBD;
    const GLenum COMPRESSED_SIGNED_RED_GREEN_RGTC2_EXT = 0x8DBE;
};

// https://www.khronos.org/registry/webgl/extensions/EXT_texture_norm16/
[LegacyNoInterfaceObject,
 Exposed=(Window,Worker)]
interface EXT_texture_norm16 {
  const GLenum R16_EXT = 0x822A;
  const GLenum RG16_EXT = 0x822C;
  const GLenum RGB16_EXT = 0x8054;
  const GLenum RGBA16_EXT = 0x805B;
  const GLenum R16_SNORM_EXT = 0x8F98;
  const GLenum RG16_SNORM_EXT = 0x8F99;
  const GLenum RGB16_SNORM_EXT = 0x8F9A;
  const GLenum RGBA16_SNORM_EXT = 0x8F9B;
};

[LegacyNoInterfaceObject,
 Exposed=(Window,Worker)]
interface WEBGL_compressed_texture_s3tc
{
    const GLenum COMPRESSED_RGB_S3TC_DXT1_EXT  = 0x83F0;
    const GLenum COMPRESSED_RGBA_S3TC_DXT1_EXT = 0x83F1;
    const GLenum COMPRESSED_RGBA_S3TC_DXT3_EXT = 0x83F2;
    const GLenum COMPRESSED_RGBA_S3TC_DXT5_EXT = 0x83F3;
};

[LegacyNoInterfaceObject,
 Exposed=(Window,Worker)]
interface WEBGL_compressed_texture_s3tc_srgb {
    /* Compressed Texture Formats */
    const GLenum COMPRESSED_SRGB_S3TC_DXT1_EXT        = 0x8C4C;
    const GLenum COMPRESSED_SRGB_ALPHA_S3TC_DXT1_EXT  = 0x8C4D;
    const GLenum COMPRESSED_SRGB_ALPHA_S3TC_DXT3_EXT  = 0x8C4E;
    const GLenum COMPRESSED_SRGB_ALPHA_S3TC_DXT5_EXT  = 0x8C4F;
};

[LegacyNoInterfaceObject,
 Exposed=(Window,Worker)]
interface WEBGL_compressed_texture_astc {
    /* Compressed Texture Format */
    const GLenum COMPRESSED_RGBA_ASTC_4x4_KHR = 0x93B0;
    const GLenum COMPRESSED_RGBA_ASTC_5x4_KHR = 0x93B1;
    const GLenum COMPRESSED_RGBA_ASTC_5x5_KHR = 0x93B2;
    const GLenum COMPRESSED_RGBA_ASTC_6x5_KHR = 0x93B3;
    const GLenum COMPRESSED_RGBA_ASTC_6x6_KHR = 0x93B4;
    const GLenum COMPRESSED_RGBA_ASTC_8x5_KHR = 0x93B5;
    const GLenum COMPRESSED_RGBA_ASTC_8x6_KHR = 0x93B6;
    const GLenum COMPRESSED_RGBA_ASTC_8x8_KHR = 0x93B7;
    const GLenum COMPRESSED_RGBA_ASTC_10x5_KHR = 0x93B8;
    const GLenum COMPRESSED_RGBA_ASTC_10x6_KHR = 0x93B9;
    const GLenum COMPRESSED_RGBA_ASTC_10x8_KHR = 0x93BA;
    const GLenum COMPRESSED_RGBA_ASTC_10x10_KHR = 0x93BB;
    const GLenum COMPRESSED_RGBA_ASTC_12x10_KHR = 0x93BC;
    const GLenum COMPRESSED_RGBA_ASTC_12x12_KHR = 0x93BD;

    const GLenum COMPRESSED_SRGB8_ALPHA8_ASTC_4x4_KHR = 0x93D0;
    const GLenum COMPRESSED_SRGB8_ALPHA8_ASTC_5x4_KHR = 0x93D1;
    const GLenum COMPRESSED_SRGB8_ALPHA8_ASTC_5x5_KHR = 0x93D2;
    const GLenum COMPRESSED_SRGB8_ALPHA8_ASTC_6x5_KHR = 0x93D3;
    const GLenum COMPRESSED_SRGB8_ALPHA8_ASTC_6x6_KHR = 0x93D4;
    const GLenum COMPRESSED_SRGB8_ALPHA8_ASTC_8x5_KHR = 0x93D5;
    const GLenum COMPRESSED_SRGB8_ALPHA8_ASTC_8x6_KHR = 0x93D6;
    const GLenum COMPRESSED_SRGB8_ALPHA8_ASTC_8x8_KHR = 0x93D7;
    const GLenum COMPRESSED_SRGB8_ALPHA8_ASTC_10x5_KHR = 0x93D8;
    const GLenum COMPRESSED_SRGB8_ALPHA8_ASTC_10x6_KHR = 0x93D9;
    const GLenum COMPRESSED_SRGB8_ALPHA8_ASTC_10x8_KHR = 0x93DA;
    const GLenum COMPRESSED_SRGB8_ALPHA8_ASTC_10x10_KHR = 0x93DB;
    const GLenum COMPRESSED_SRGB8_ALPHA8_ASTC_12x10_KHR = 0x93DC;
    const GLenum COMPRESSED_SRGB8_ALPHA8_ASTC_12x12_KHR = 0x93DD;

    // Profile query support.
    sequence<DOMString>? getSupportedProfiles();
};

[LegacyNoInterfaceObject,
 Exposed=(Window,Worker)]
interface WEBGL_compressed_texture_etc
{
    const GLenum COMPRESSED_R11_EAC                                 = 0x9270;
    const GLenum COMPRESSED_SIGNED_R11_EAC                          = 0x9271;
    const GLenum COMPRESSED_RG11_EAC                                = 0x9272;
    const GLenum COMPRESSED_SIGNED_RG11_EAC                         = 0x9273;
    const GLenum COMPRESSED_RGB8_ETC2                               = 0x9274;
    const GLenum COMPRESSED_SRGB8_ETC2                              = 0x9275;
    const GLenum COMPRESSED_RGB8_PUNCHTHROUGH_ALPHA1_ETC2           = 0x9276;
    const GLenum COMPRESSED_SRGB8_PUNCHTHROUGH_ALPHA1_ETC2          = 0x9277;
    const GLenum COMPRESSED_RGBA8_ETC2_EAC                          = 0x9278;
    const GLenum COMPRESSED_SRGB8_ALPHA8_ETC2_EAC                   = 0x9279;
};

[LegacyNoInterfaceObject,
 Exposed=(Window,Worker)]
interface WEBGL_compressed_texture_etc1
{
    const GLenum COMPRESSED_RGB_ETC1_WEBGL = 0x8D64;
};

[LegacyNoInterfaceObject,
 Exposed=(Window,Worker)]
interface WEBGL_compressed_texture_pvrtc
{
    const GLenum COMPRESSED_RGB_PVRTC_4BPPV1_IMG  = 0x8C00;
    const GLenum COMPRESSED_RGB_PVRTC_2BPPV1_IMG  = 0x8C01;
    const GLenum COMPRESSED_RGBA_PVRTC_4BPPV1_IMG = 0x8C02;
    const GLenum COMPRESSED_RGBA_PVRTC_2BPPV1_IMG = 0x8C03;
};

[LegacyNoInterfaceObject,
 Exposed=(Window,Worker)]
interface WEBGL_debug_renderer_info
{
    const GLenum UNMASKED_VENDOR_WEBGL        = 0x9245;
    const GLenum UNMASKED_RENDERER_WEBGL      = 0x9246;
};

[LegacyNoInterfaceObject,
 Exposed=(Window,Worker)]
interface WEBGL_debug_shaders
{
    DOMString getTranslatedShaderSource(WebGLShader shader);
};

[LegacyNoInterfaceObject,
 Exposed=(Window,Worker)]
interface WEBGL_depth_texture
{
    const GLenum UNSIGNED_INT_24_8_WEBGL = 0x84FA;
};

[LegacyNoInterfaceObject,
 Exposed=(Window,Worker)]
interface OES_element_index_uint
{
};

[LegacyNoInterfaceObject,
 Exposed=(Window,Worker)]
interface EXT_frag_depth
{
};

[LegacyNoInterfaceObject,
 Exposed=(Window,Worker)]
interface WEBGL_lose_context {
    undefined loseContext();
    undefined restoreContext();
};

[LegacyNoInterfaceObject,
 Exposed=(Window,Worker)]
interface EXT_texture_filter_anisotropic
{
    const GLenum TEXTURE_MAX_ANISOTROPY_EXT     = 0x84FE;
    const GLenum MAX_TEXTURE_MAX_ANISOTROPY_EXT = 0x84FF;
};

[LegacyNoInterfaceObject,
 Exposed=(Window,Worker)]
interface EXT_sRGB
{
    const GLenum SRGB_EXT                                  = 0x8C40;
    const GLenum SRGB_ALPHA_EXT                            = 0x8C42;
    const GLenum SRGB8_ALPHA8_EXT                          = 0x8C43;
    const GLenum FRAMEBUFFER_ATTACHMENT_COLOR_ENCODING_EXT = 0x8210;
};

[LegacyNoInterfaceObject,
 Exposed=(Window,Worker)]
interface OES_standard_derivatives {
    const GLenum FRAGMENT_SHADER_DERIVATIVE_HINT_OES = 0x8B8B;
};

[LegacyNoInterfaceObject,
 Exposed=(Window,Worker)]
interface OES_texture_float
{
};

[LegacyNoInterfaceObject,
 Exposed=(Window,Worker)]
interface WEBGL_draw_buffers {
    const GLenum COLOR_ATTACHMENT0_WEBGL     = 0x8CE0;
    const GLenum COLOR_ATTACHMENT1_WEBGL     = 0x8CE1;
    const GLenum COLOR_ATTACHMENT2_WEBGL     = 0x8CE2;
    const GLenum COLOR_ATTACHMENT3_WEBGL     = 0x8CE3;
    const GLenum COLOR_ATTACHMENT4_WEBGL     = 0x8CE4;
    const GLenum COLOR_ATTACHMENT5_WEBGL     = 0x8CE5;
    const GLenum COLOR_ATTACHMENT6_WEBGL     = 0x8CE6;
    const GLenum COLOR_ATTACHMENT7_WEBGL     = 0x8CE7;
    const GLenum COLOR_ATTACHMENT8_WEBGL     = 0x8CE8;
    const GLenum COLOR_ATTACHMENT9_WEBGL     = 0x8CE9;
    const GLenum COLOR_ATTACHMENT10_WEBGL    = 0x8CEA;
    const GLenum COLOR_ATTACHMENT11_WEBGL    = 0x8CEB;
    const GLenum COLOR_ATTACHMENT12_WEBGL    = 0x8CEC;
    const GLenum COLOR_ATTACHMENT13_WEBGL    = 0x8CED;
    const GLenum COLOR_ATTACHMENT14_WEBGL    = 0x8CEE;
    const GLenum COLOR_ATTACHMENT15_WEBGL    = 0x8CEF;

    const GLenum DRAW_BUFFER0_WEBGL          = 0x8825;
    const GLenum DRAW_BUFFER1_WEBGL          = 0x8826;
    const GLenum DRAW_BUFFER2_WEBGL          = 0x8827;
    const GLenum DRAW_BUFFER3_WEBGL          = 0x8828;
    const GLenum DRAW_BUFFER4_WEBGL          = 0x8829;
    const GLenum DRAW_BUFFER5_WEBGL          = 0x882A;
    const GLenum DRAW_BUFFER6_WEBGL          = 0x882B;
    const GLenum DRAW_BUFFER7_WEBGL          = 0x882C;
    const GLenum DRAW_BUFFER8_WEBGL          = 0x882D;
    const GLenum DRAW_BUFFER9_WEBGL          = 0x882E;
    const GLenum DRAW_BUFFER10_WEBGL         = 0x882F;
    const GLenum DRAW_BUFFER11_WEBGL         = 0x8830;
    const GLenum DRAW_BUFFER12_WEBGL         = 0x8831;
    const GLenum DRAW_BUFFER13_WEBGL         = 0x8832;
    const GLenum DRAW_BUFFER14_WEBGL         = 0x8833;
    const GLenum DRAW_BUFFER15_WEBGL         = 0x8834;

    const GLenum MAX_COLOR_ATTACHMENTS_WEBGL = 0x8CDF;
    const GLenum MAX_DRAW_BUFFERS_WEBGL      = 0x8824;

    undefined drawBuffersWEBGL(sequence<GLenum> buffers);
};

[LegacyNoInterfaceObject,
 Exposed=(Window,Worker)]
interface OES_texture_float_linear
{
};

[LegacyNoInterfaceObject,
 Exposed=(Window,Worker)]
interface EXT_shader_texture_lod
{
};

[LegacyNoInterfaceObject,
 Exposed=(Window,Worker)]
interface OES_texture_half_float
{
    const GLenum HALF_FLOAT_OES = 0x8D61;
};

[LegacyNoInterfaceObject,
 Exposed=(Window,Worker)]
interface OES_texture_half_float_linear
{
};

[LegacyNoInterfaceObject,
 Exposed=(Window,Worker)]
interface WEBGL_color_buffer_float
{
    const GLenum RGBA32F_EXT = 0x8814;
    const GLenum RGB32F_EXT = 0x8815;
    const GLenum FRAMEBUFFER_ATTACHMENT_COMPONENT_TYPE_EXT = 0x8211;
    const GLenum UNSIGNED_NORMALIZED_EXT = 0x8C17;
};

[LegacyNoInterfaceObject,
 Exposed=(Window,Worker)]
interface EXT_color_buffer_half_float
{
    const GLenum RGBA16F_EXT = 0x881A;
    const GLenum RGB16F_EXT = 0x881B;
    const GLenum FRAMEBUFFER_ATTACHMENT_COMPONENT_TYPE_EXT = 0x8211;
    const GLenum UNSIGNED_NORMALIZED_EXT = 0x8C17;
};

[LegacyNoInterfaceObject,
 Exposed=(Window,Worker)]
interface OES_vertex_array_object {
    const GLenum VERTEX_ARRAY_BINDING_OES = 0x85B5;

    WebGLVertexArrayObject createVertexArrayOES();
    undefined deleteVertexArrayOES(WebGLVertexArrayObject? arrayObject);
    [WebGLHandlesContextLoss] GLboolean isVertexArrayOES(WebGLVertexArrayObject? arrayObject);
    undefined bindVertexArrayOES(WebGLVertexArrayObject? arrayObject);
};

[LegacyNoInterfaceObject,
 Exposed=(Window,Worker)]
interface ANGLE_instanced_arrays {
    const GLenum VERTEX_ATTRIB_ARRAY_DIVISOR_ANGLE = 0x88FE;

    undefined drawArraysInstancedANGLE(GLenum mode, GLint first, GLsizei count, GLsizei primcount);
    undefined drawElementsInstancedANGLE(GLenum mode, GLsizei count, GLenum type, GLintptr offset, GLsizei primcount);
    undefined vertexAttribDivisorANGLE(GLuint index, GLuint divisor);
};

[LegacyNoInterfaceObject,
 Exposed=(Window,Worker)]
interface EXT_blend_minmax {
    const GLenum MIN_EXT = 0x8007;
    const GLenum MAX_EXT = 0x8008;
};

[Exposed=(Window,Worker)]
interface WebGLQuery {
};

[LegacyNoInterfaceObject,
 Exposed=(Window,Worker)]
interface EXT_disjoint_timer_query {
    const GLenum QUERY_COUNTER_BITS_EXT = 0x8864;
    const GLenum CURRENT_QUERY_EXT = 0x8865;
    const GLenum QUERY_RESULT_EXT = 0x8866;
    const GLenum QUERY_RESULT_AVAILABLE_EXT = 0x8867;
    const GLenum TIME_ELAPSED_EXT = 0x88BF;
    const GLenum TIMESTAMP_EXT = 0x8E28;
    const GLenum GPU_DISJOINT_EXT = 0x8FBB;

    WebGLQuery createQueryEXT();
    undefined deleteQueryEXT(WebGLQuery? query);
    [WebGLHandlesContextLoss] boolean isQueryEXT(WebGLQuery? query);
    undefined beginQueryEXT(GLenum target, WebGLQuery query);
    undefined endQueryEXT(GLenum target);
    undefined queryCounterEXT(WebGLQuery query, GLenum target);
    any getQueryEXT(GLenum target, GLenum pname);
    any getQueryObjectEXT(WebGLQuery query, GLenum pname);
};

[LegacyNoInterfaceObject,
 Exposed=(Window,Worker)]
interface MOZ_debug {
    const GLenum EXTENSIONS = 0x1F03;

    const GLenum WSI_INFO   = 0x10000;
    const GLenum UNPACK_REQUIRE_FASTPATH = 0x10001;
    const GLenum DOES_INDEX_VALIDATION   = 0x10002;
    const GLenum CONTEXT_TYPE   = 0x10003;

    [Throws]
    any getParameter(GLenum pname);
};

[LegacyNoInterfaceObject,
 Exposed=(Window,Worker)]
interface EXT_float_blend {
};

[LegacyNoInterfaceObject,
 Exposed=(Window,Worker)]
interface OES_fbo_render_mipmap {
};

[LegacyNoInterfaceObject,
 Exposed=(Window,Worker)]
interface WEBGL_explicit_present {
    undefined present();
};

// https://www.khronos.org/registry/webgl/extensions/OES_draw_buffers_indexed/
[Exposed=(Window,Worker), LegacyNoInterfaceObject]
interface OES_draw_buffers_indexed {
  undefined enableiOES(GLenum target, GLuint index);

  undefined disableiOES(GLenum target, GLuint index);

  undefined blendEquationiOES(GLuint buf, GLenum mode);

  undefined blendEquationSeparateiOES(GLuint buf,
                                      GLenum modeRGB, GLenum modeAlpha);

  undefined blendFunciOES(GLuint buf,
                          GLenum src, GLenum dst);

  undefined blendFuncSeparateiOES(GLuint buf,
                                  GLenum srcRGB, GLenum dstRGB,
                                  GLenum srcAlpha, GLenum dstAlpha);

  undefined colorMaskiOES(GLuint buf,
                          GLboolean r, GLboolean g, GLboolean b, GLboolean a);
};

[Exposed=(Window,Worker), LegacyNoInterfaceObject]
interface WEBGL_provoking_vertex {
    const GLenum FIRST_VERTEX_CONVENTION_WEBGL = 0x8E4D;
    const GLenum LAST_VERTEX_CONVENTION_WEBGL  = 0x8E4E; // default
    const GLenum PROVOKING_VERTEX_WEBGL        = 0x8E4F;

    undefined provokingVertexWEBGL(GLenum provokeMode);
};

[Exposed=(Window,Worker), LegacyNoInterfaceObject]
interface EXT_depth_clamp {
    const GLenum DEPTH_CLAMP_EXT = 0x864F;
};

// https://immersive-web.github.io/webxr/#dom-webglcontextattributes-xrcompatible
partial dictionary WebGLContextAttributes {
    [Pref="dom.vr.webxr.enabled"]
    boolean xrCompatible = false;
};

// https://immersive-web.github.io/webxr/#dom-webglrenderingcontextbase-makexrcompatible
partial interface mixin WebGLRenderingContextBase {
    [NewObject, Pref="dom.vr.webxr.enabled"]
    Promise<undefined> makeXRCompatible();
};
/* parts/Window.webidl ----------------------------------------------------- */
/* -*- Mode: IDL; tab-width: 2; indent-tabs-mode: nil; c-basic-offset: 2 -*- */

/* Castle Game Engine notes:
   This WEBIDL was cut down to our needs.
   This is not the original (complete) WEBIDL file,
   if you want a complete file get it from
   https://hg.mozilla.org/mozilla-central/raw-file/tip/dom/webidl/

   -------------------------------------------------------------------------------
*/

/* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this file,
 * You can obtain one at http://mozilla.org/MPL/2.0/.
 *
 * The origin of this IDL file is:
 * http://www.whatwg.org/specs/web-apps/current-work/
 * https://dvcs.w3.org/hg/editing/raw-file/tip/editing.html
 * https://dvcs.w3.org/hg/IndexedDB/raw-file/tip/Overview.html
 * http://dev.w3.org/csswg/cssom/
 * http://dev.w3.org/csswg/cssom-view/
 * https://dvcs.w3.org/hg/webperf/raw-file/tip/specs/RequestAnimationFrame/Overview.html
 * https://dvcs.w3.org/hg/webperf/raw-file/tip/specs/NavigationTiming/Overview.html
 * https://dvcs.w3.org/hg/webcrypto-api/raw-file/tip/spec/Overview.html
 * http://dvcs.w3.org/hg/speech-api/raw-file/tip/speechapi.html
 * https://w3c.github.io/webappsec-secure-contexts/#monkey-patching-global-object
 * https://w3c.github.io/requestidlecallback/
 * https://drafts.css-houdini.org/css-paint-api-1/#dom-window-paintworklet
 * https://wicg.github.io/visual-viewport/#the-visualviewport-interface
 * https://wicg.github.io/cookie-store/#Window
 */

interface Principal;
interface nsIBrowserDOMWindow;
interface XULControllers;
interface nsIDOMWindowUtils;
interface nsIPrintSettings;

interface Window : EventTarget {
};

Window includes AnimationFrameProvider;

partial interface Window {
  [Replaceable, Throws, NeedsCallerType]
  readonly attribute double devicePixelRatio;
};