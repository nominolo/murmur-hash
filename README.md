murmur-hash
===========

A simple, Haskell-only implementation of [MurmurHash2][1].

Two variants are provided: 32 bit and 64 bits.  We do not simply use
the machine word size since the bit width of the hash determines the
collision probability which should stay under user control.

 [1]: http://murmurhash.googlepages.com/