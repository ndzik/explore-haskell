# Dependent Types in Haskell

The problem I was thinking about was how I would be able to expose an API for registering event callbacks with a single function.
I was faced with this problem when working on the [Erdstall SDK](https://github.com/perun-network/erdstall-ts-sdk) in TypeScript.
I hacked together a dependent type-ish solution, which has _some_ similarities with ad-hoc polymorphism making heavy use of the typing machinery `tsc` gives.
After finishing this I obviously asked myself: "How would I do this in Haskell?".
It is my favourite language, I should know an answer to this question and so began my journey.


A concrete example:
* We have some events which have different names.
* For each event exists a callback signature which can be provided by a user of this hypothetical library.
* The registration of those callbacks has to happen over a single function, below is an idealised example:

```haskell
data Event = Start | Message | Stop

on Start (cb :: (String -> SomeStruct -> Int -> IO ()))
on Message (cb :: (String -> Int -> Maybe String))
on Stop (cb :: (SomeOtherStruct -> Bool))
```

It is required that specifying an event statically fixes the to be given callback to a specific signature.
Each callback can have its own number of parameters and unique return types.
If possible, it should allow for exhaustively checking the implementation of `on` for the library implementer.

## Learnings

When looking at the implementations from `Simple` to `Better` to `Betterer` one might realise how stubborn I tried to index a type family by the dataconstructors of `Event`.
In the end a simple `GADT` does the trick and no fancy type applications or proof terms in conjunction with constraints and injective type families was necessary.
I read through multiple papers regarding dependent types, compiler theory and more and learned a lot.
It was a good exercise and I am glad I got sidetracked since I experimented with more compiler extensions than ever and finally know what they are useful for and even more important:
Knowing how to use them!

With that being said, it was quite a ride with a solution so simple I was almost salty when I finally realised my fault lied in the way I viewed the problem.
(Thinking that an extra `Event` sum type was necessary for the problem to be useful).
