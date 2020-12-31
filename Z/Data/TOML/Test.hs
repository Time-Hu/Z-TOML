module Z.Data.TOML.Test where

import Z.Data.TOML.Value 
import Z.Data.TOML.Parser 
import Z.IO.FileSystem
import Z.Data.Parser

example = quickReadFile "/Users/shijiahu/Desktop/Workplace/TOML/exmaple.toml"

x = example >>= (return .parse tomlP)