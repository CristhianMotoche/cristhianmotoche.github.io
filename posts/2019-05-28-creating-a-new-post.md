---
title: Steps to create a new post. What a bummer!
date: 2019-05-28 00:00:00
tags: en, things, pending
description: As a programmer, I know that I need to automate this.
---

Currently, this is the process that I follow to create a new post:

<img src="/images/create-new-blog/create-new-blog.gif" alt="drawing" width="1000"/>

Yeah, I know, the dates are wrong. I just fixed that, but that's not the worst.
The worst is that I have to do this each time that I want to create a new post.
That's a bummer! I must automate this!

I'm using [hakyll][hakyll] to generate static pages for this blog post.
This tool provides a [set of functions][hakyll-main] to run the hakyll compiler.
I'm using `hakyll` since it just takes the rules and is ready to go:

```haskell
main :: IO ()
main = hakyll rules
```

However, `hakyll` provides a CLI with a predefined set of commands:

```
$ stack exec site

Missing: COMMAND

Usage: site [-v|--verbose] COMMAND
  site - Static site compiler created with Hakyll

Available options:
  -h,--help                Show this help text
  -v,--verbose             Run in verbose mode

Available commands:
  build                    Generate the site
  check                    Validate the site output
  clean                    Clean up and remove cache
  deploy                   Upload/deploy your site
  preview                  [DEPRECATED] Please use the watch command
  rebuild                  Clean and build again
  server                   Start a preview server
  watch                    Autocompile on changes and start a preview server.
                           You can watch and recompile without running a server
```

All of those commands are useful, but I'd love to extend the commands to add a new
one that generates a new post. Something like:

```
$ stack exec site new-post 'Post name'
POST GENERATED: posts/2019-05-28-post-name.md
```

And that's going to be the goal of this post.

# Approach #1

My first approach was to extend the commands from `hakyll`, but it seems it's
not currently possible. So, I decided to try [optparse-applicative][optparse-applicative]
and create a command line interface (CLI) on my own. However, I would have had
to rewrite a lot of code for the `hakyll` commands. So, I decided just to add a
new command and then use the rest of `hakyll` commands.

```haskell
data Opts = New String deriving Show
```

Now, let's define a `Parser`:

```haskell
parser :: Parser Opts
parser =
  New <$>
    subparser
      (command "new" (info (option str (long "name")) (progDesc "New post")))
```

Unfortunately, `execParser` or `customExecParser` from
`optparse-applicative` use [`handleParseResult`][handleParseResult]
that exits when the command is not in the parser. I don't want to exit
on failure, because on failure I want to attempt `hakyll` commands. Fortunately,
`optparse-applicative` provides [`execParsePure`][execParsePure] so that I
can handle the response as I prefer. In this case, I attempted this:

```haskell
main :: IO ()
main = do
  args <- getArgs
  let parseResult =
        execParserPure
          (prefs showHelpOnError)
          (info (helper <*> parser) fullDesc)
          args
  case parseResult of
    Success (New name) -> mkNewPost name >>= createNewPost
    Failure failure    -> do
      progname <- getProgName
      let (msg, _) = renderFailure failure progname
      putStrLn "# CUSTOM COMMANDS"
      hPutStrLn stderr msg
      putStrLn "\n# HAKYLL COMMANDS"
      hakyll rules
    completionInvoke   -> void $ handleParseResult completionInvoke
```

`execParserPure` takes a `ParseInfo` and returns a `ParserResult` I simply
did pattern matching over that result. If the parsing is successful and parses
`New name` it creates a new post with the current date and the given `name`.
If it fails, it renders the failure, prints some messages to separate the
results from my custom commands and the ones from `hakyll`,
redirects the parsing error to `stderr`, and then executes any `hakyll` command.
Finally, if the parser result is `Completition`, I delegate `handleParseResult`
to take care of that.

Now the results:

```
$ stack exec site

# CUSTOM COMMANDS
Missing: COMMAND

Usage: site COMMAND

Available options:
  -h,--help                Show this help text

Available commands:
  new                      New post

# HAKYLL COMMANDS
Missing: COMMAND

Usage: site [-v|--verbose] COMMAND
  site - Static site compiler created with Hakyll

Available options:
  -h,--help                Show this help text
  -v,--verbose             Run in verbose mode

Available commands:
  build                    Generate the site
  check                    Validate the site output
  ...
```

Nice! Now, I can do this:

```
$ stack exec site new --name "some post"
$ ls posts/
...
2019-06-06-some-post.md
```

And if you want to execute the `hakyll` commands:

```
$ stack exec site watch
CUSTOM COMMANDS
Invalid argument `watch'

Usage: site COMMAND

Available options:
  -h,--help                Show this help text

Available commands:
  new                      New post

HAKYLL COMMANDS
Listening on http://127.0.0.1:8000
Initialising...
  Creating store...
  Creating provider...
  Running rules...
Checking for out-of-date items
Compiling
Success
```

You: Pfff! That's gonna print the failure of your parser dummy! That's terrible -_-

I: Definatelly, I agree with you. I'm going to look for a better approach
but for now will work.

Thank you for reading.

ByE!

[hakyll]:  https://hackage.haskell.org/package/hakyll
[hakyll-main]: https://hackage.haskell.org/package/hakyll-4.12.5.2/docs/Hakyll-Main.html
[optparse-applicative]: https://hackage.haskell.org/package/optparse-applicative
[handleParseResult]: https://hackage.haskell.org/package/optparse-applicative-0.14.3.0/docs/src/Options.Applicative.Extra.html#handleParseResult
[execParsePure]: https://hackage.haskell.org/package/optparse-applicative-0.14.3.0/docs/Options-Applicative.html#v:execParserPure
