module Safelog where

import Data.List
import qualified Data.Map as Map

-- Define the typeclass
class Safelog a where
  logSafe :: a -> String

-- Use the typeclass
logInfo :: Safelog a => a -> IO ()
logInfo line = print (logSafe line)

-- Example domain
data UserId = UserId String
data Email = Email String
data User = User { id :: UserId, email:: Email }

-- Instances of the typeclass
instance Safelog UserId where
  logSafe (UserId id) = "userId " ++ id

instance Safelog User where
  logSafe (User id email) = "user " ++ (logSafe id)

instance Safelog a => Safelog [a] where
  logSafe as = (foldMap (\s -> (logSafe s) ++ "\n") as)

main :: IO ()
main = do
  logInfo tammo
  logInfo allUsers
--  logInfo (email tammo) -- will not compile:  No instance for (Safelog Email) arising from a use of `logInfo'
  where
    tammo = (User (UserId "123") (Email "tammo.sminia@jdriven.com"))
    allUsers = [tammo]
