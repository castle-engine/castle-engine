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
