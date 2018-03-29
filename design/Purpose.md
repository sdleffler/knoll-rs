# Purpose

The general purpose of the Knoll VM is to be capable of running several thousand concurrent programs, in an actor-like paradigm, with resource restrictions on each actor. This is intended to be usable in a game-like situation where one wants to simulate several hundred to several thousand separately running machines, each with limited communication with other machines. One example of this is a space combat game with hundreds to thousands of individual spaceships, each heavily automated, where spaceship programs control all systems of the ship, and spaceship message-passing is restricted by artificial constraints mimicking e.g. "radio range". This example will be referred to throughout this reference.

Additional constraints come out of the sort of languages we would like to compile to and run on the Knoll. We have two specific source languages as goals to keep in mind when designing the Knoll; it should be useful as an efficient compilation target for:

1. A Scheme-like language, with support for continuations.
2. A Prolog-like language, built on top of the continuation support provided to the Scheme-like language.

These are carefully chosen to be simple and easy to use for two very specific types of players of our hypothetical game: the Scheme, for new to experienced programmers who want to program their machines and have tight control over the logic involved. And, the Prolog, for non-programmers (or programmers who like declarative programming) to write ship programs which are easily human-readable; for example, given built-in ship routines which tie into game AI, a non-programmer might be able to write easily readable declarative programs such as:

```prolog
% If some ship "S" is colored red, then it is an enemy.
Colored(S, Red) :- Enemy(S) .

% If some ship "S" is colored green, then it is the squad leader.
Colored(S, Green) :- Leader(S) .

% Stay within 10 km of the leader at all times.
Leader(S) :- Follow(S, 10km) .

% Shoot all enemies we see.
Enemy(S) :- Shoot(S) .
```

# Constraints

We have a large number of constraints from these initial requirements. First off:

1. The VM must be capable of scheduling and running hundreds to thousands of actors in parallel, where each actor is a machine.
2. The VM must provide primitives conducive to building a Scheme-like language, such as cons-cells, vectors, and crucially first-class continuations.
3. The VM must provide primitives conducive to building a Prolog-like language, such as unification variables.
4. The VM must be resilient to actor errors. If an actor crashes, no other actors may crash with it.

As a matter of practicality for building a dynamically typed language:

4. The VM must be capable of allocating and typechecking record types.

As a matter of game mechanics:

5. The VM must be capable of running in "bursts", where in a single burst, all actors are updated to some configurable new state; for example, some ships may have slower processors than others, and can only run a certain number of instructions per burst.
6. The VM must be capable of enforcing memory constraints on actors; for example, some ships may have less memory than others.
7. The VM must be capable of predicating actor communication on external conditions; for example, two ships outside of radio range can't exchange messages. In addition, the VM must be capable of dispatching actor communication based on external conditions;  e.g. a ship should be able to "broadcast" messages to all ships within radio range.

# VM design

When considering programming languages designed to run in constrained-resource environments, stack languages (and stack machines) are a favored solution. However, using a stack language/machine for our VM seems counterproductive. Register machines are generally faster, and while memory efficiency is a priority, speed is a higher priority, because memory pressure can be played off as a game mechanic. Due to this the Knoll VM is designed as a register machine.

## Garbage collection

Supporting continuations (and garbage collection, by necessity of supporting primitives for Scheme-family and Prolog-family languages) means that our garbage collection scheme will be of the "Cheney on the M.T.A." variety: a copying garbage collector. Indeed, this also means that we have no "stack": the program runs entirely on the heap, and what stack there is emerges from the linked-list of continuations existing on the heap.

Crucially, there are two types of heaps we will need to support:
1. Actor heaps, which are mutable and undergo garbage collection.
2. Message heaps, which are immutable and represent single messages from other ships. These do not undergo garbage collection because of their short lifecycle.

Both actor and message heaps are formatted in terms of the same VM "words". More on that later.

### Pooling heaps

For efficiency, the VM should be able to pool both actor and message heaps; actor heaps return to the pool when being collected as another actor heap is pulled out of the pool for use. Message heaps are pulled from the pool when a message is created by one ship to send to another; *all* data in the message is deeply copied into the message heap, and then the message (as a heap plus a pointer to the root of the message) is dropped into the queues of any actors which successfully receive it. When actors poll their inboxes for new messages, new messages are deeply copied into their heaps (and then the pointer to the message heap is dropped.) As message heaps are immutable, they may be reference-counted and shared among inboxes as well; this makes it efficient to send "broadcasts" to multiple actors. Messages are copied to actor heaps lazily rather than eagerly.

# Datatypes and words

Here, "word" refers to the piece of data representing a single "object". An "object" may be a number of things; an integer, a cons cell, or more. The Knoll VM does not make assumptions as to the word size of the machine in question, and is intended to be runnable with arbitrary-sized words which implement "unpacking": that is, all words should be isomorphic (with some exceptions) to the specified "unpacked" representation. This is the only interface that a word type must present to the Knoll VM. A consequence of this is that the "unpacked" word type `UnpackedWord` may also be used as a VM word, which is useful to scope out possible bugs in execution or garbage collection.

In practice, anyone attempting to run Knoll fast will want to be using 32- or 64-bit words.

More on words and datatypes can be found in `Word.md`.
