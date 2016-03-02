# CriticMarkup.hs

A Parsec-based CriticMarkup cleaner. Run

`criticmarkuphs in.md out.md`

to clean all the gunk out of `in.md` and produce a svelte `out.md`.

## Install

I'm 80% sure the following will work. Download this repo to some folder. Then run:

```sh
cabal configure
cabal build
cabal install
```

That should produce an executable that you should then drop somewhere in your $PATH.

## TODO

This was a hack that nevertheless appears to work for all of my academic papers.
I'm sure there are monstrous edge-cases throughout the code.
