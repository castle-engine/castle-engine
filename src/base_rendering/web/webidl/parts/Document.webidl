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
