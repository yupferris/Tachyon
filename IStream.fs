module Tachyon.IStream
    type IStream<'a> =
        abstract addWatch : Handler<'a> -> unit
        abstract removeWatch : Handler<'a> -> unit
