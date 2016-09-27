# earl_jason
Erlang JSON parser

This is incomplete but should work for all syntactically correct JSON. I got as far as starting to add descriptive messages for parsing errors to make it more useful for ad-hoc JSON content creators.

The parser is pretty straightforward: it constructs a token stream which is then "compiled" into a native Erlang construct.

It should be easy to create different output representations by changing the second stage.

```erlang
jp:json_to_object_array("{\"a\":\"blah\"}").
```

Yields the erlang structure:

```erlang
{success,[{"a","blah"}]}
```
