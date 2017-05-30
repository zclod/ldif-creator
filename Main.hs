{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Monoid
import           Data.ByteString
import           Text.LDIF.Types
import           Text.LDIF.Printer
import           Text.LDIF.Parser

main :: IO ()
main = print "Hello, Haskell!"

instance Monoid DN where
  mempty = DN []
  mappend (DN xs) (DN ys) = DN (xs `mappend` ys)

dnRoot :: DN
dnRoot = DN [(Attribute "O", Value "GBBPER"), (Attribute "C", Value "IT")]

dummyUser :: DN
dummyUser = DN [(Attribute "uid", Value "ABCD"), (Attribute "ou", Value "utenti")] <> dnRoot

type Description = ByteString
type Name = ByteString
type NewEntry = (Name, Description)
data ParsedInput = Input {junction :: NewEntry, profiles :: [NewEntry]}

junctionDN :: Name -> DN
junctionDN jName = DN [(Attribute "ou", Value jName)] <> dnRoot

profileDN :: Name -> Name -> DN
profileDN jName pName = DN [(Attribute "cn", Value pName), (Attribute "ou", Value "profili")] <> junctionDN jName

createProfile :: Name -> Name -> Description -> LDIFRecord
createProfile jName pName description = ChangeRecord (profileDN jName pName) (ChangeAdd values)
  where
    values = [(Attribute "cn", Value pName),
              (Attribute "objectclass", Value "top"),
              (Attribute "objectclass", Value "groupOfUniqueNames"),
              (Attribute "description", Value description),
              (Attribute "uniquemember", Value (dn2str dummyUser))
             ]

-- gruppi, profili, utenti
createOus :: Name -> [LDIFRecord]
createOus jName = [newOu "utenti", newOu "profili", newOu "gruppi"]
  where
    newOu :: ByteString -> LDIFRecord
    newOu ou = ChangeRecord (DN [(Attribute "ou", Value ou)] <> junctionDN jName) (ChangeAdd (values ou))
    values ou = [(Attribute "ou", Value ou),
                 (Attribute "objectclass", Value "top"),
                 (Attribute "objectclass", Value "organizationalUnit")
                ]

createLDAPBranch :: ParsedInput -> LDIF
createLDAPBranch (Input j ps) = LDIF Nothing ([branch] <> ous <> newprofiles)
  where
    branch = ChangeRecord (junctionDN (fst j)) (ChangeAdd [(Attribute "ou", Value (fst j)),
                                                           (Attribute "objectclass", Value "top"),
                                                           (Attribute "objectclass", Value "organizationalUnit"),
                                                           (Attribute "description", Value (snd j))
                                                          ])
    ous = createOus (fst j)
    newprofiles = fmap (uncurry (createProfile (fst j))) ps

{-

:r
:set -XOverloadedStrings

a = createLDAPBranch $ Input ("prova", "descrizione") [("p1", "profilo1"), ("p2", "profilo2")]
ldif2str a

prefix = DN [(Attribute "cn", Value "BPER"), (Attribute "cn", Value "IT")]
a = DN [(Attribute "ou", Value "prova")]
dn = a <> prefix

show dn

change = ChangeRecord dn (ChangeAdd [(Attribute "cn", Value "cacca")])
change1 = ChangeRecord dn (ChangeAdd [(Attribute "cn", Value "pupu")])

c = LDIF Nothing [change, change1]

ldif2str c

parseDNStr defaulLDIFConf "ou=prova,cn=BPER,cn=IT"

-}
