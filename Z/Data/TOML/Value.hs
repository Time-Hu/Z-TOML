module Z.Data.TOML.Value where 

import Z.Data.Vector
import Z.Data.Text

type KeyV = (Text, Value)
type KeyVs = [(Text, Value)]
data TableKey = AKey Text | StdKey Text deriving (Show)

data Toml = TKeyV KeyVs | Table TableKey KeyVs deriving (Show)
data Value = TString Text 
           | TInteger Integer 
           | TDouble Double
           | TArray [Value]
           | TInlineTable KeyVs
           | TBool Bool
        --    | TTime UTCTime
          deriving (Show)

