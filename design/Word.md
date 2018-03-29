# Words and memory

Memory is defined as an array of "words", where the meaning of "word" is implementation-defined modulo some universal requirements on words; namely, words must be isomorphic to the "unpacked" word, as defined here.

## The "stack"

Because we support continuations, cactus stacks are really the only reasonable option. As such, any given stack frame is also a corresponding continuation, and the "stack" is actually an immutable linked list where the head is the currently executing continuation (and the currently executing continuation links to the prior stack frame, and so on.) As a matter of course, the "stack" also lives on the heap; the heap becomes the *only* relevant area of addressable memory for the currently executing program.

## Anatomy of an allocation

When we look at the heap, we see it segmented into allocations. Every allocation is created at the same time as its corresponding pointer. Some allocations have headers; others are "headless".
