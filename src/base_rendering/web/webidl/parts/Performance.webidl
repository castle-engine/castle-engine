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
