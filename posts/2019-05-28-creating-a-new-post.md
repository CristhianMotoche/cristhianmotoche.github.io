---
title: Things to create a new post. What a bummer!
date: 2019-05-28 00:00:00
tags: en, things
description: As a programmer, I know that I need to automate this.
---

Currently, this is the process that I do to create a new post:

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

[hakyll]:  https://hackage.haskell.org/package/hakyll
[hakyll-main]: https://hackage.haskell.org/package/hakyll-4.12.5.2/docs/Hakyll-Main.html
