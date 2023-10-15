---
title: Dynamic type checking with Python
date: 2023-09-14 03:13:48
tags: python
description: Check types at runtime
---

# Dynamic Type Checking

This year I had the pleasure to give a [talk about type hints](https://www.youtube.com/watch?v=yt8nYEkCfNU) at PyCon Colomba 2023.
It was rewarding and I really enjoyed it! I'll attend the next one even if I'm not a speaker. I've been using type hints to get
feedback about types before actually executing the code using a static analysis tool (e.g. mypy, pyright, pyre, etc.). It worked
for me but it doesn't ensure that the executing code respects the types. For that reason, I decided to give a look at dynamic type
checking which consist on verifying the types at runtime!

## Proof of Concept

I decided to do a simple proof of concept (poc) creating a parser of env variables given a type. I was planning to create a interface like this:

```py
class Config(BaseConfig):
    PORT: int
    HOSTNAME: str
    FLAGS: list[str] | None
    ENVIRONMENT: Literal['stage', 'prod']

conf = Config().load()

if not conf:
    print("Error with type variables")
    sys.exit(1)
else:
    reveal_type(conf.PORT) # It would be 'int'
    reveal_type(conf.ENVIRONMENT) # It would be "Literal['stage', 'prod']"
```

So, the first challenge was to get the type hint information at runtime!
Fortunately, the typing package provides some helpers to do that.
The first helper function is  `typing.get_type_hints()` that returns information
about type hints:

```py
class Config:
    PORT: int
    HOSTNAME: str
    FLAGS: list[str] | None
    ENVIRONMENT: Literal['stage', 'prod']

typing.get_type_hints(Config)

> {'PORT': <class 'int'>, 'HOSTNAME': <class 'str'>, 'FLAGS': list[str] | None, 'ENVIRONMENT': typing.Literal['stage', 'prod']}
```

After that, I had to get parse each type and propertly parse it. I created the function
`_check_type` which receive the `key` (name of the attribute),
`value` (the string value read from `os.environ`), and `type_to_apply` which
was the type obtained from the values of `get_type_hints` dictionary.

```py
    def _check_type(cls, key: str, value: str, type_to_apply: Type) -> Any:
        if issubclass(type_to_apply, bool):
            return value == "True"
        elif issubclass(type_to_apply, int):
            return int(value)
        elif issubclass(type_to_apply, float):
            return float(value)
        elif issubclass(type_to_apply, str):
            return value
        else:
            raise ValueError(f"The given type ({type_to_apply}) for {key} cannot be used to parse an env variable")
```

`issubclass` checks if the `type_to_apply` was of a particular type so that I could
apply the parser propertly. This worked for the basic types but it didn't work for
union types. I had to use `isinstance` in that case:

```py
    def _check_type(cls, key: str, value: str, type_to_apply: Type) -> Any:
        if isinstance(type_to_apply, types.UnionType):
            return cls._try_union_types(key, value, cast(types.UnionType, type_to_apply))
        elif issubclass(type_to_apply, bool):
            ...
```

`_try_union_types` looked like this:

```py
    @classmethod
    def _try_union_types(cls, key: str, value: str, type_to_apply: types.UnionType) -> Any:
        for inner_type_to_apply in typing.get_args(type_to_apply):
            try:
                return cls._check_type(key, value, inner_type_to_apply)
            except ValueError:
                continue
        raise ValueError(
            f'The value {value} is not of any of these types'
            f' {typing.get_args(type_to_apply)}'
        )
```

There were more things that I had to do like checking if a variable is `Optional[T]` or `T | None` and so on.

I noticed this was getting too big for a simple poc because I think I was doing exactly what dynamic type checkers like Pydantic or beartype do.

So, I decided to review them and give them a try for my use case and I noticed it was quite simple:

```py

from pydantic import BaseModel

class Config(BaseModel):
    PORT: int
    HOSTNAME: str
    FLAGS: list[str] | None
    ENVIRONMENT: Literal['stage', 'prod']

# This line does the trick!
conf = Config(**dict(os.environ.items()))

reveal_type(conf.PORT) # It's a 'int'!
```

I got exactly what I was looking for using pydantic in one line of code. That was great because I didn't had to reinvent the wheel!
Anyway, my poc taught me a few things about the typing package and type hints:

1. I can read type hint information at runtime with functions like `get_type_hints` or `get_args`.

2. I can pass additional information using `typing.Annotated`.

3. I can access typing metadata using `get_type_hints(thing, include_extras=True)` and `__metadata__`.

```py
from typing import Annotated, Type, get_type_hints

class Foo:
    x: Annotated[int, 'hello', 'wolrd']

def print_meta(cl: Type):
    for k, v in get_type_hints(cl, include_extras=True).items():
        print(k, ' says: ', v.__metadata__)

print_meta(Foo)
```

It was a fun journey and I enjoyed it! I plan to review more about typing in Python and I'll keep sharing information about that here!

Thanks for reading!
