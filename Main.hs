{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Main where

import           GHC.Generics
import           Data.Aeson
import           Data.Monoid
import           Data.ByteString
import qualified Data.ByteString as B
import           Text.LDIF.Types
import           Text.LDIF.Printer
import           Text.LDIF.Parser
import           Data.Text
import           Data.Text.Encoding (encodeUtf8)
import           Options.Applicative

instance Monoid DN where
  mempty = DN []
  mappend (DN xs) (DN ys) = DN (xs `mappend` ys)

data BranchDescription = B { branchName :: Text
                           , branchDescription :: Text
                           , branchProfiles :: [Text]
                           , branchProfilesDescriptions :: [Text]
                           } deriving (Generic, Show)

instance FromJSON BranchDescription

data Abilitazioni = A { matr :: [Text]
                      , matrProfiles :: [Text]
                      , ldapBranch :: Text
                      } deriving (Generic, Show)

instance FromJSON Abilitazioni

-- (nome, descrizione)
type NewEntry = (ByteString, ByteString)
data Branch = Branch {junction :: NewEntry, profiles :: [NewEntry]} deriving Show

dnRoot :: DN
dnRoot = DN [(Attribute "O", Value "GBBPER"), (Attribute "C", Value "IT")]

userDN :: ByteString -> DN
userDN matr = DN [(Attribute "uid", Value matr), (Attribute "ou", Value "utenti")] <> dnRoot

dummyUser :: DN
dummyUser = userDN "ABCD"

junctionDN :: ByteString -> DN
junctionDN jName = DN [(Attribute "ou", Value jName)] <> dnRoot

profileDN :: ByteString -> ByteString -> DN
profileDN jName pName = DN [(Attribute "cn", Value pName), (Attribute "ou", Value "profili")] <> junctionDN jName

createProfile :: ByteString -> ByteString -> ByteString -> LDIFRecord
createProfile jName pName pDescription = ChangeRecord (profileDN jName pName) (ChangeAdd values)
  where
    values = [(Attribute "cn", Value pName),
              (Attribute "objectclass", Value "top"),
              (Attribute "objectclass", Value "groupOfUniqueNames"),
              (Attribute "description", Value pDescription),
              (Attribute "uniquemember", Value (dn2str dummyUser))
             ]

-- gruppi, profili, utenti
createOus :: ByteString -> [LDIFRecord]
createOus jName = [newOu "utenti", newOu "profili", newOu "gruppi"]
  where
    newOu ou = ChangeRecord (DN [(Attribute "ou", Value ou)] <> junctionDN jName) (ChangeAdd (values ou))
    values ou = [(Attribute "ou", Value ou),
                 (Attribute "objectclass", Value "top"),
                 (Attribute "objectclass", Value "organizationalUnit")
                ]

params2Input :: BranchDescription -> Branch
params2Input x = Branch j ps
  where
    j = (encodeUtf8 . branchName $ x, encodeUtf8 . branchDescription $ x)
    ps = Prelude.zip (fmap encodeUtf8 $ branchProfiles x) (fmap encodeUtf8 $ branchProfilesDescriptions x)

abil2Input :: Abilitazioni -> (ByteString, [(ByteString, ByteString)])
abil2Input a = (encodeUtf8 $ ldapBranch a, Prelude.zip (fmap encodeUtf8 $ matr a) (fmap encodeUtf8 $ matrProfiles a))

createLDAPBranch :: Branch -> LDIF
createLDAPBranch (Branch j ps) = LDIF Nothing ([branch] <> ous <> newprofiles)
  where
    branch = ChangeRecord (junctionDN (fst j)) (ChangeAdd [(Attribute "ou", Value (fst j)),
                                                           (Attribute "objectclass", Value "top"),
                                                           (Attribute "objectclass", Value "organizationalUnit"),
                                                           (Attribute "description", Value (snd j))
                                                          ])
    ous = createOus (fst j)
    newprofiles = fmap (uncurry (createProfile (fst j))) ps

enableUsers :: (ByteString, [(ByteString, ByteString)]) -> LDIF
enableUsers (jName, xs) = LDIF Nothing permissions
  where
    permissions = fmap createPermission xs
    createPermission (matr, prof) = ChangeRecord (profileDN jName prof) (ChangeAdd [(Attribute "uniquemember", Value (dn2str . userDN $ matr))])

----------------------------------------------------------------------------

data Options = NewBranch
             | NewAbil

newBranchParser = flag' NewBranch
                    (long "creazione-ramo"
                    <> short 'r'
                    <> help "genera ldif per la creazione di un nuovo ramo LDAP")

newAbilParser = flag' NewAbil
                    (long "abilitazioni-utenti"
                    <> short 'a'
                    <> help "genera ldif per assegnare una serie di profili ldap a delle matircole")

optsparser = newBranchParser <|> newAbilParser


cmdParser :: ParserInfo Options
cmdParser = info (optsparser <**> helper)
    ( fullDesc
    <> progDesc "programma per automatizzare il processo di creazione di rami ldap attraverso la generazione di file ldif"
    <> header "ldif creator" )

main :: IO ()
main = do
  opts <- execParser cmdParser
  case opts of
    NewBranch -> B.interact newBranch
    NewAbil   -> B.interact newAbil

    where
      mkprogram f i = case fmap f (decodeStrict i) of
                        Just x -> ldif2str x
                        Nothing -> ""
      newAbil = mkprogram (enableUsers . abil2Input)
      newBranch = mkprogram (createLDAPBranch . params2Input)
