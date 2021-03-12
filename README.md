# SqlQL

```bash
nix-shell --pure shell.nix
cd src/
ghci
```
```haskell
> :set -XOverloadedStrings
> :l SQL/Parser.hs
>
> parseTest sqlP "select * from users"
```
