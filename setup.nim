import httpclient
import osproc

let client = newHttpClient()
let content = client.getContent("https://raw.githubusercontent.com/ameerwasi001/Hashing-Lua/main/hashLib.lua")

writeFile("hashLib.lua", content)

discard execCmd "cabal install"
discard execCmd "cabal repl"