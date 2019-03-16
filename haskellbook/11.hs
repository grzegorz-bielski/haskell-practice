{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module FromChapter11 where

-- 11.9
class TooMany a where 
    tooMany :: a -> Bool

instance TooMany Int where 
    tooMany n = n > 42

newtype Goats =
    Goats Int deriving (Eq, Show, TooMany)

instance TooMany (Int, String) where
    tooMany (n,_) = n > 42

instance TooMany (Int, Int) where
    tooMany (n, m) = (n + m) > 42

-- 11.13
data OperatingSystem = 
      GnuPlusLinux
    | OpenBSDPlusNevermindJustBSDStill 
    | Mac
    | Windows
    deriving (Eq, Show)

data ProgLang =
    Haskell
    | Agda
    | Idris
    | PureScript deriving (Eq, Show)

data Programmer =
    Programmer { os :: OperatingSystem
               , lang :: ProgLang 
               } 
    deriving (Eq, Show)

allOperatingSystems :: [OperatingSystem] 
allOperatingSystems =
    [ GnuPlusLinux
    , OpenBSDPlusNevermindJustBSDStill 
    , Mac
    , Windows
    ]
 
allLanguages :: [ProgLang] 
allLanguages =
     [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer]
allProgrammers = foldr (
    \os acc -> acc <> (fmap (\lang -> Programmer{ os = os, lang = lang}) allLanguages) 
    ) [] allOperatingSystems
