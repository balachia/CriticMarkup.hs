# CriticMarkup.hs

A Parsec-based CriticMarkup cleaner. Run

```
criticmarkuphs -i in.md -o out.md
cat in.md | criticmarkuphs > out.md
```

to clean all the gunk out of `in.md` and produce a svelte `out.md`.

## Install

I'm 95% sure the following will work. Download this repo to some folder. Then run:

```sh
cabal configure
cabal build
cabal install
```

That should produce an executable that you should then drop somewhere in your
`$PATH`.

## Changes

- 0.2:
    - `criticmarkuphs` can now run as a command line filter
    - `-i` and `-o` command line options can specify input/output files
    - old `criticmarkuphs in.md out.md` format still works, but is deprecated

## TODO

This was a hack that nevertheless appears to work for all of my academic papers.
I'm sure there are monstrous edge-cases throughout the code.
