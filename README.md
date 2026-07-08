# hyperion-static-plugin

This is an experimental GHC source plugin for writing local `Static c`
instances for concrete constraints.

With the plugin enabled, this deriving clause:

```haskell
data Foo = MkFoo Int
  deriving (Generic, Binary, Static Binary)
```

causes the plugin to add:

```haskell
instance Static (Binary Foo) where
  closureDict = static Dict
```

## Use

The module using the plugin must enable the extensions required by the
generated instance, and must have the `Hyperion.Static` names in scope:

```haskell
{-# OPTIONS_GHC -fplugin=Hyperion.Static.Plugin #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StaticPointers #-}
{-# LANGUAGE UndecidableInstances #-}

import Data.Binary (Binary)
import GHC.Generics (Generic)
import Hyperion.Static

data Foo = MkFoo Int
  deriving (Generic, Binary, Static Binary)
```

Use the simple form when GHC can infer the deriving strategies normally:

```haskell
data NoStrategy = NoStrategy Int
  deriving (Eq, Generic, Binary, Static Eq, Static Binary)
```

When there are strategy ambiguities, for example with newtypes, use explicit
strategies for the ordinary derived classes and keep the static marker in an
`anyclass` clause:

```haskell
newtype Box a = Box a
  deriving stock Generic
  deriving anyclass (Binary, Static Binary)
```

The plugin asks GHC for the already-derived `Binary (Box a)` instance context
and rewrites that second declaration into:

```haskell
instance (Typeable a, Static (Binary a)) => Static (Binary (Box a)) where
  closureDict =
    static (\Dict -> Dict)
      `cAp` (closureDict :: Closure (Dict (Binary a)))
```

Standalone declarations such as `deriving instance Static (Binary Foo)` are
still supported. The deriving-clause marker form is preferred when you are
already deriving the underlying class on the datatype.
