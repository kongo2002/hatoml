# HaTOML - TOML parser/encoder written in haskell

[TOML][toml] (Tom's Obvious Minimal Language) by *Tom Preston-Werner* is a
simple configuration file format inspired by JSON and YAML.


## Usage

You can build HaTOML using *cabal*:

    $ cabal configure
    $ cabal build

Using it in your haskell programs is pretty straightforward as well:

~~~ {.haskell}

import qualified Data.ByteString.Char8 as BS
import           Data.HaTOML


parseFile :: FilePath -> IO (Either String TOML)
parseFile f = parse `fmap` BS.readFile f
~~~


## Compatibility

As of now **HaTOML** is compatible with TOML [v0.2][latest]
`4f23be43e42775493f142e7dd025b6227e037dd9` except from *arrays of tables* (which
I hope to implement real soon).


## Example

This is what an example TOML file looks like:

~~~ {.toml}
# This is a TOML document. Boom.

title = "TOML Example"

[owner]
name = "Tom Preston-Werner"
organization = "GitHub"
bio = "GitHub Cofounder & CEO\nLikes tater tots and beer."
dob = 1979-05-27T07:32:00Z # First class dates? Why not?

[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
connection_max = 5000
enabled = true

[servers]

  # You can indent as you please. Tabs or spaces. TOML don't care.
  [servers.alpha]
  ip = "10.0.0.1"
  dc = "eqdc10"

  [servers.beta]
  ip = "10.0.0.2"
  dc = "eqdc10"

[clients]
data = [ ["gamma", "delta"], [1, 2] ]

# Line breaks are OK when inside arrays
hosts = [
  "alpha",
  "omega"
]
~~~

HaTOML parses the above TOML file into the following data structure:

~~~ {.haskell}
Right
 (TOML (fromList [
  ("clients", TGroup (TOML (fromList [
   ("data", TArray [
     TArray [
      TString "gamma",
      TString "delta"],
     TArray [
      TInteger 1,
      TInteger 2]]),
   ("hosts", TArray [
    TString "alpha", TString "omega"])]))),
  ("database", TGroup (TOML (fromList [
   ("connection_max", TInteger 5000),
   ("enabled", TBool True),
   ("ports", TArray [
    TInteger 8001,
    TInteger 8001,
    TInteger 8002]),
   ("server", TString "192.168.1.1")]))),
  ("owner", TGroup (TOML (fromList [
   ("bio", TString "GitHub Cofounder & CEO\nLikes tater tots and beer."),
   ("dob", TDate 1979-05-27 07:32:00 UTC),
   ("name", TString "Tom Preston-Werner"),
   ("organization", TString "GitHub")]))),
  ("servers", TGroup (TOML (fromList [
   ("alpha", TGroup (TOML (fromList [
    ("dc", TString "eqdc10"),
    ("ip", TString "10.0.0.1")]))),
   ("beta", TGroup (TOML (fromList [
    ("dc", TString "eqdc10"),
    ("ip", TString "10.0.0.2")])))]))),
  ("title", TString "TOML Example")]))
~~~


## TODO

* Support for arrays of tables
* Better error messages
* Further look into encoding logic
* Implement some convenience functions (i.e. `lookup`, `set` ...)
* More unit tests


[toml]: https://github.com/mojombo/toml
[latest]: https://github.com/mojombo/toml/commit/4f23be43e42775493f142e7dd025b6227e037dd9
