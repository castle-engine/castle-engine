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
