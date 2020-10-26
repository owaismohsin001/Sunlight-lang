import httpclient
import osproc

let client = newHttpClient()
let content = client.getContent("https://raw.githubusercontent.com/ameerwasi001/Hashing-Lua/main/hashLib.lua")

writeFile("hashLib.lua", content)

discard execCmd "cabal install megaparsec"
discard execCmd "cabal install text"
discard execCmd "cabal install containers"
discard execCmd "cabal install hashable"

discard execCmd "cabal install megaparsec --lib"
discard execCmd "cabal install text --lib"
discard execCmd "cabal install containers --lib"
discard execCmd "cabal install hashable --lib"

discard execCmd "cabal repl"