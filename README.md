# chess
### Getting Started
1. Install the Haskell Tool Stack (click [here](https://docs.haskellstack.org/en/stable/README/#how-to-install) for instructions)
2. `stack setup` - Installs a local stack based on the version in the yaml so we can have reproducible builds 🙂
3. `stack build` - Builds the project (this needs to be run after every code change)
4. `stack exec haskellChess-exe` - Runs the uci chess engine
5. `stack exec haskellChess-exe -- -i white` - Runs the game in interactive mode where you play white

#### Other Commands
`stack ghci` - Runs the interpreter

#### Misc.
Click [here](http://download.shredderchess.com/div/uci.zip) for the UCI Spec
