# Talks

Here is a collection of all of the technical talks that I\'ve done. I have not
yet mastered the art of making simple, effective talks so please beware that
they can be a little rambly. 

---

## Haskell - Pure and Simple

This talk was an exploration of a haskell implementation of the pubsub-hubbub
protocol. The goal of the talk was to show that haskell is just as useful
for highly concurrent, stateful applications as it is for code that is mostly
pure. 

* Slides: [pureandsimple.benkolera.com](http://pureandsimple.benkolera.com/)
* Video: [Part 1](https://vimeo.com/70972939) [Part 2](http://vimeo.com/85909474)
* Code: [https://github.com/benkolera/haskell-hubbub](https://github.com/benkolera/haskell-hubbub)

---

## Why FP?

This talk was a high level outline of all of the reasons why you\'d like to take
a statically typed, functional approach when writing your
applications. Unfortunately I couldn\'t contain my bias towards static types and
hopefully wasn\'t too unfair. :(

* Slides: [whyfp.benkolera.com](whyfp.benkolera.com)
* Video: [https://vimeo.com/70972939](https://vimeo.com/70972939)

---

## Introduction to Reader, Writer, State

The goal of this talk was to show how you may take a pretty standard piece of
imperative code with mutable variables and translate it to be purely
functional with reader, writer & state. It also touches on how side effects in the
presence of laziness can cause considerable headaches.

I was just recovering from food poisoning in this talk so it is a bit muddled,
but there are still some good ideas and thoughts in here.

* Slides & Code: [https://github.com/benkolera/talk-isolating-effects-with-monads](https://github.com/benkolera/talk-isolating-effects-with-monads)
* Video: [https://vimeo.com/64796627](https://vimeo.com/64796627)

---

## Type-safe Web Development with Yesod & Haskell

My very first public talk! This explores the Yesod web application framework and
how its use of static types can save you from a lot of common web dev mistakes.

* Slides & Code: [https://github.com/benkolera/talk-yesod](https://github.com/benkolera/talk-yesod)
