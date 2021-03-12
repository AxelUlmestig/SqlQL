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
> -- parse to AST
> parse "select * from users"
>
> -- turn AST back into text
> format <$> parse "select * from users"
```
