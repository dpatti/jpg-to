# jpg-to

```
$ jpg-cli "haskell logo"
https://wiki.haskell.org/wikiupload/6/6e/Haskell_logo_falconnl_8_fancy.png
```

It doesn't actually only limit to jpg files. Everything else is hard-coded.
Errors are not handled. I don't know how to write Haskell.

## Setup

```
$ cp jpg-cli/conf.sample conf
$ cabal sandbox init
$ cabal install -j
$ cabal build
```

Put your public API access key and custom search engine id in `conf`.

### Getting an API key

1. https://console.developers.google.com/project
2. Create project
3. APIs & auth > APIs > Custom Search API > Enable
4. APIs & auth > Credentials > Create new Key > Server key

### Getting the search engine id

1. https://cse.google.com/cse/all
2. Add
3. Pick a random site to search; name the search engine; create
4. Edit your search engine
5. Remove the site if you want
6. Change "Search only included sites" to "Search the entire web but emphasize
   included sites"
7. Click "Search engine ID"
