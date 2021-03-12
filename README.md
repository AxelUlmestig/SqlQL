# SqlQL

```bash
nix-shell --pure shell.nix
cd src/
ghci
```
```haskell
> :set -XOverloadedStrings
> :l SQL.hs
>
> parse "select * from users"
```
