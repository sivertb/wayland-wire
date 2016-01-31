# wayland-wire

## Introduction

This package provides a way to generate both server and client Haskell-bindings
from a wayland protocol definition in XML.

## Generate bindings

Given an XML-protocol definition named `test.xml`

```Haskell
$(generateFromXml Client "test.xml")
```

can be used to generate the Haskell-bindings for the client side. The module
with the generated code needs the following extensions to be enabled:

- `FlexibleContexts`
- `MultiParamTypeClasses`
- `Rank2Types`
- `TemplateHaskell`
- `TypeFamilies`

## Use the bindings

The bindings will create an empty data-type for every interface. The
`wl_display` interface in the core wayland protocol would for instance get the
following definition:

```Haskell
data WlDisplay
```

and it would be made an instance of `Dispatchable` and `DispatchInterace`. This
makes it possible to send _signals_ or add _slots_. On the server side the
_slots_ are the _requests_, and the _signals_ are the _events_. On the client
side it is the other way around, since _signals_ are always defined as the
outgoing messages, while _slots_ handle the incoming messages. For example, to
send a display error from the server to the client one could do:

```Haskell
wlDisplayError (signals display) objIdOfBadObject errorCode "An error occured"
```

The `W` Monad-transformer can be used to track objects and their slot handlers,
or you could define your own by making it an instance of `MonadObject`. You
also need a Monad that implements `MonadSend` for the generated bindings to be
able to send messages.
