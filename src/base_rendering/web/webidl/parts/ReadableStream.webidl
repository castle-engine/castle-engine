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
 * https://streams.spec.whatwg.org/#rs-class-definition
 */

[Exposed=*] // [Transferable] - See Bug 1562065
interface ReadableStream {
  [Throws]
  constructor(optional object underlyingSource, optional QueuingStrategy strategy = {});

  [Throws]
  static ReadableStream from(any asyncIterable);

  readonly attribute boolean locked;

  [NewObject]
  Promise<undefined> cancel(optional any reason);

  [Throws]
  ReadableStreamReader getReader(optional ReadableStreamGetReaderOptions options = {});

  [Throws]
  ReadableStream pipeThrough(ReadableWritablePair transform, optional StreamPipeOptions options = {});

  // CGE: removed to make castleinternaljobweb.pas smaller
  // [NewObject]
  // Promise<undefined> pipeTo(WritableStream destination, optional StreamPipeOptions options = {});

  [Throws]
  sequence<ReadableStream> tee();

  // not handled by webidl2pas
  //[GenerateReturnMethod]
  //async_iterable<any>(optional ReadableStreamIteratorOptions options = {});
};

enum ReadableStreamReaderMode { "byob" };

dictionary ReadableStreamGetReaderOptions {
  ReadableStreamReaderMode mode;
};

dictionary ReadableStreamIteratorOptions {
  boolean preventCancel = false;
};

dictionary ReadableWritablePair {
  required ReadableStream readable;
  // CGE: removed to make castleinternaljobweb.pas smaller
  // required WritableStream writable;
};

dictionary StreamPipeOptions {
  boolean preventClose = false;
  boolean preventAbort = false;
  boolean preventCancel = false;
  AbortSignal signal;
};
