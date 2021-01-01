## Setup

1. Install `ghc` and `cabal`

2. Install `tidy`

3. Run `cabal update` and `cabal build`

4. Run `cabal run -- site --help` and `cabal run -- site build`

## Other notes

- `docs/CNAME` is required for GitHub hosting with my domain name, ensure it remains in place as some Hakyll commands will remove it.

- If you get an error about `tidy`, you need to install `tidy`. `tidy` is a non-Haskell program that can probably be installed from a software repository.
