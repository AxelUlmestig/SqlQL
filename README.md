# SqlQL

```bash
nix-shell --pure shell.nix
cd src/
ghci
```
```haskell
> :set -XOverloadedStrings
> :l Parser
>
> parseTest sqlP "select * from users"
```
