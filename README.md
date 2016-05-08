**Disclaimers**:

* Does not take upper bounds on a packages's dependencies into account, check the [open stackage issues](https://github.com/fpco/stackage/issues) for that.
* Does not follow the github-users map, only shows packages directly under the searched name.

# Example

```shell
$ stackage-user-status Michael
Parsing build-constraints.yaml
Parsed build-constraints.yaml

# Packages for Michael Xavier
## Tests
angel: ExpectFailure
drifter-postgresql: ExpectFailure
## All packages
angel
drifter
drifter-postgresql
phash
tasty-fail-fast
tasty-tap
uri-bytestring

# Packages for Michael Thompson
## All packages
lens-family
lens-family-core
lens-simple
pipes-text
streaming
streaming-bytestring

# Packages for Michael Snoyman
## Bounds
cabal-install: <1.24.0.0
persistent: <2.5
persistent-mysql: <2.5
persistent-postgresql: <2.5
persistent-sqlite: <2.5
persistent-template: <2.5
## Tests
cabal-install: ExpectFailure
## Benchmarks
bzlib-conduit: ExpectFailure
case-insensitive: Don'tBuild
mutable-containers: ExpectFailure
## All packages
JuicyPixels-repa
authenticate-oauth
binary-conduit
bzlib-conduit
cabal-install
cabal-src
case-insensitive
cereal-conduit
classy-prelude-yesod
conduit-combinators
conduit-extra
cryptonite-conduit
hebrew-time
hpc-coveralls
keter
lzma-conduit
markdown
mime-mail
mime-mail-ses
monad-unlift
monadcryptorandom
mutable-containers
network-conduit-tls
persistent
persistent-mysql
persistent-postgresql
persistent-sqlite
persistent-template
random-shuffle
repa
repa-algorithms
repa-devil
repa-io
servius
sphinx
stm-conduit
wai-websockets
warp-tls
yackage
yaml
yesod
yesod-auth
yesod-auth-deskcom
yesod-bin
yesod-eventsource
yesod-fay
yesod-gitrepo
yesod-newsfeed
yesod-sitemap
yesod-static
yesod-test
yesod-websockets

# Packages for Michael Sloan
## All packages
th-orphans
th-reify-many

# Packages for Michael Schröder
## Benchmarks
ttrie: Don'tBuild
## All packages
ctrie
ttrie

# Packages for Simon Michael
## Tests
darcs: ExpectFailure
## All packages
darcs
hledger
hledger-ui
hledger-web
regex-compat-tdfa
shelltestrunner

# Packages for Michael Walker
## All packages
async-dejafu
both
dejafu
dpor
hunit-dejafu
irc-client
irc-conduit
irc-ctcp
tasty-dejafu
```
