import httpclient
import osproc

let client = newHttpClient()
let contentLua = client.getContent("https://raw.githubusercontent.com/ameerwasi001/Hashing-Lua/main/hashLib.lua")
let contentJs = client.getContent("https://raw.githubusercontent.com/ameerwasi001/hashLib-Sha1-Js/main/hashLib.js")

writeFile("./compiler_libs/hashLib.lua", contentLua)
writeFile("./compiler_libs/hashLib.js", contentJs)

discard execCmd "cabal install"
discard execCmd "cabal repl"