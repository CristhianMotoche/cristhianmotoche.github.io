---
title: Trying aeson-schemas
date: 2020-08-18 02:25:46
tags: haskell
description: A quick test of the library aeson-schemas
---

A few weeks ago, Brandon Chinn presented a talk about [aeson-schemas][aeson-schemas].
You can see the talk [here][talk-brandon-chinn]. It was cool to see some
type level programming applied to a really common task: parse JSON.

So, I decided to give it a try and write some code. As suggested, by Vitaly
Bragilevsky in his [talk][talk-vitaly]:

> _Always, always, always write your own code._


And that's what I'll do here! (the code snippets are [here][example-code]).

The problem is dead simple! I'm going to parse and process the JSON of one of
my [favorite][favorite-card] cards in Yu Gi Oh! and
I'm going to use the [Yu Gi Oh! Pro API][ygo-api].


Here is the JSON representation:

```json
{
  "data": [
  {
    "id": 71625222,
      "name": "Time Wizard",
      "type": "Effect Monster",
      "desc": "Once per turn: You can toss a coin and call it. If you call it right, ...",
      "atk": 500,
      "def": 400,
      "level": 2,
      "race": "Spellcaster",
      "attribute": "LIGHT",
      "archetype": "Magician",
      "card_sets": [
        {
          "set_name": "Starter Deck: Joey",
          "set_code": "SDJ-015",
          "set_rarity": "Common",
          "set_rarity_code": "(C)",
          "set_price": "1.14"
        }
      ],
      "card_images": [
        {
          "id": 71625222,
          "image_url": "https:\/\/storage.googleapis.com\/ygoprodeck.com\/pics\/71625222.jpg",
          "image_url_small": "https:\/\/storage.googleapis.com\/ygoprodeck.com\/pics_small\/71625222.jpg"

        }
      ],
      "card_prices": [
        {
          "cardmarket_price": "0.12",
          "tcgplayer_price": "0.22",
          "ebay_price": "3.95",
          "amazon_price": "2.11",
          "coolstuffinc_price": null
        }
      ]
    }
  ]
}
```

As you can see, the JSON is kind of nested and _aeson-schemas_ promisses to be
helpful in this situations. Let's see if that's true.

First, you'll need to add the following extensions:

```haskell
{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TypeApplications  #-}
```

and import these libraries:

```haskell

import qualified Data.Aeson           as A
import           Data.Aeson.Schema    (Object, get, schema)
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text.IO         as T
```

Now, let's define the first schema:

```haskell
type Data = [schema|
  {
    data: List #Card
  }
|]
```

`Data` is just a wrapper for a list of card. In this simple example,
we're just expecting one type of card, so it's not necessary to define all
the types of possible schemas that we could get from the API.

```haskell
type Card = [schema|
  {
    id: Int,
    name: Text,
    type: Text,
    desc: Text,
    atk: Int,
    def: Int,
    level: Int,
    race: Text,
    attribute: Text,
    archetype: Text,
    card_sets: List #CardSet,
    card_images: List #CardImage,
    card_prices: List #CardPrice,
  }
|]
```

`Card` is a simple struture with some simple types `Int`, `Text`, and `List`s.

```haskell
type CardSet = [schema|
  {
    set_name: Text,
    set_code: Text,
    set_rarity: Text,
    set_rarity_code: Text,
    set_price: Text,
  }
|]

type CardImage = [schema|
  {
    id: Int,
    image_url: Text,
    image_url_small: Text,
  }
|]

type CardPrice = [schema|
  {
    cardmarket_price: Text,
    tcgplayer_price: Text,
    ebay_price: Text,
    amazon_price: Text,
    coolstuffinc_price: Maybe Text,
  }
|]
```

As you can see, `CardSet`, `CardImage`, and `CardPrice` are simple strucures, too.

Now, let's parse it:

```haskell
main :: IO ()
main =
  let decodeStr :: BS.ByteString -> Either String (Object Data)
      decodeStr = A.eitherDecode
   in do
     eData <- decodeStr <$> BS.readFile "time-wizard.json"
     case eData of
       Left str -> putStrLn $ "Error: " ++ str
       Right dat -> print dat
```

Running this code, will print the parsed JSON in the console. If we change
something in the expected content, we'll get an error message.

Let's access to the properties of the parsed content. From the code above,
I'm going to access the `data` property of the `dat` object and pass it to the
function `printCardInfo` that expects a `Card` object:

```haskell
       Right dat -> mapM_ printCardInfo [get| dat.data[] |]
```

Now, `printCardInfo` has more data accessors:

```haskell

printCardInfo :: Object Card -> IO ()
printCardInfo card = do
  putStrLn $ "id: " ++ show [get| card.id |]
  putStrLn $ "name: " ++ show [get| card.name |]
  putStrLn $ mconcat [
    "atk: ", show [get| card.atk |], " ",
    "def: ", show [get| card.def |]
    ]
  T.putStrLn $ mconcat $
    ["Small image url: "] ++ [get| card.card_images[].image_url_small |]
  putStrLn $
    "Price In Coolstuff.inc: " ++ show [get| card.card_prices[].coolstuffinc_price |]
```

There are two important accessors to mention here:

- `[get| card.def |]` that access the property `def` of `card`
- `[get| card.card_images[].image_url_small |]` that access the property `image_url_small` of all the `card_images`

There are more interesting data accessors that you'll find in the library (e.g. `?` to work with optional data).

Now, here's where it comes really interesting. What would happen if we change
`name` with `first_name` in the data accessor? Well, the answer is simple:
the code **won't** compile!

```py
    • Key 'first_name' does not exist in the following schema:
      '[ '( 'Data.Aeson.Schema.Internal.NormalKey "id",
            'Data.Aeson.Schema.Internal.SchemaInt),
         '( 'Data.Aeson.Schema.Internal.NormalKey "name",
            'Data.Aeson.Schema.Internal.SchemaText),
         '( 'Data.Aeson.Schema.Internal.NormalKey "type",
```

Cool, right? And the error message is quite explanatory.

## Conclusion

aeson-schemas is a type safe library that helps us to parse JSON and access
its properties. If we misspell or use the incorrect syntax to access a property
we'll get a compilation error, which is much better than an error at runtime!

So, give it a look, I'm sure you'll find it interesting.

That's all for now. Thanks for reading!

ByE!


[aeson-schemas]: https://hackage.haskell.org/package/aeson-schemas
[talk-brandon-chinn]: https://www.youtube.com/watch?v=vJQtEa_7O3Q
[talk-vitaly]: https://www.youtube.com/watch?v=n3H_YipBDrY
[ygo-api]: https://db.ygoprodeck.com/api-guide/
[example-code]: https://github.com/CristhianMotoche/as-test-ygo
[favorite-card]: https://db.ygoprodeck.com/card/?search=Time%20Wizard
