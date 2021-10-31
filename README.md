murmur-hash
===========

<div style="background:#ffeedd; padding: 0.5em">
New maintainer needed. Let me know if you're interested.
</div>

A simple, Haskell-only implementation of [MurmurHash2][1].

Two variants are provided: 32 bit and 64 bits.  We do not simply use
the machine word size since the bit width of the hash determines the
collision probability which should stay under user control.

 [1]: http://murmurhash.googlepages.com/

Example Usage
-------------

Generating hashes:

    $ import Data.Digest.Murmur32
    $ hash32 "foo"             ==> Hash32 0xd2d0a99a
    $ hash32 "foa"             ==> Hash32 0x7d544e71
    $ hash32WithSeed 42 "foo"  ==> Hash32 0x7a69563b

Custom instances:

    data Foo a = Foo a | Bar String

    instance Hashable32 a => Hashable32 (Foo a) where
      hash32Add (Bar s) = hash32AddInt 1 `combine` hash32Add s
      hash32Add (Foo a) = hash32AddInt 2 `combine` hash32Add a
