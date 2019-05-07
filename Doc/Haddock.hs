{-# OPTIONS_HADDOCK show-extensions #-}
{-# OPTIONS_GHC -Wno-duplicate-exports #-}
--------------------------------------------------------------------
-- |
-- Module      :  Doc.Haddock
-- Description :  A short description
-- Copyright   :  (c) <Dai> 2019
-- License     :  MIT
-- Maintainer  :  <daig@sodality.cc>
-- Stability   :  cursed
-- Portability  : x86-64
--
--
-- A longer description of the module with arbitrary @markup@
--
-- @{-\# OPTIONS_HADDOCK hide #-}@
--
-- Omit this module from the generated documentation,
-- but nevertheless propagate definitions and documentation from within this module
-- to modules that re-export those definitions.
--
-- @{-\# OPTIONS_HADDOCK prune #-}@
--
-- Omit definitions that have no documentation annotations.
--
-- @{-\# OPTIONS_HADDOCK ignore-exports #-}@
--
-- Ignore the export list. ie. generate documentation for all top-level declarations
--
-- @{-\# OPTIONS_HADDOCK not-home #-}@
--
-- Do not consider a definition in this module for home links unless no other
-- module exports it. 
--
-- By default all internal hyperlinks refer to the earliest/deepest dependency exporting it.
--
-- @{-\# OPTIONS_HADDOCK show-extensions #-}@
--
-- List all extensions enabled in this module in the module description box
--
--------------------------------------------------------------------
module Doc.Haddock
  (

-- * Section Headings #heading_label#
-- | @-- * Headings@

-- ** Section Subheadings
-- | @-- * Subheadings@
--
-- must be seperated by blank (no @--@) line from other headings or will parse as list

-- * Inline headings
-- $inlineHeadings

-- * Formatting
-- | /emphasis/
--
-- @\/emphasis/@
--
-- __bold__
--
-- @\__bold__@
--
-- @monospaced on its own line@
--
-- @\@monospaced on its own line\@@
--
-- @inline@ monospace
--
-- @\@inline\@ monospace@
--
-- Unicode: &#x3BB;, &#x3bb; and &#955;
--
-- @Unicode: \&#x3BB;, \&#x3bb; and \&#955;@

-- * Hyperlinks
-- ** Local links
-- | Link to module "Prelude"
--
-- @-- Link to module \"Prelude"@
--
-- Link to module-scoped 'identifier''
--
-- @Link to module-scoped \'identifier''@
--
-- Link to fully qualified 'Data.List.head' 
--
-- @Link to fully qualified \'Data.List.head'@
--
-- Link to arbitrary anchor in module "Doc.Haddock#anchor_label"
--
-- @Link to arbitrary anchor in module \"Doc.Haddock#anchor_label"@
--
-- Descriptive link to [anchor](#anchor_label) in same file
--
-- @Descriptive link to \[anchor](#anchor_label) in same module@

-- ** External urls
-- | [descriptive link](http://google.com)
--
-- @ \[descriptive link](http://google.com) @
--
-- bare url: <http://google.com>
--
-- @\<http://google.com>@
--
-- <relative_path>
--
-- @\<relative_path>@
--
-- ![images](https://raw.githubusercontent.com/haskell/haskell-platform/master/hptool/os-extras/win/icons/hsicon.ico)
--
-- @\![images](https:\/\/raw.githubusercontent.com\/haskell\/haskell-platform\/master\/hptool\/os-extras\/win\/icons/hsicon.ico)@

-- * Named Chunks
-- $namedChunks

-- * Lists
-- |
-- * Star bullet @*@
-- - Dash bullet @-@
--
-- 2. numbered bullet @2.@
--
--      * Nested list by 4 spaces. Can be any type
--
--      [foo]: description
--
-- (1) braced number bullet @(5)@
--
-- [labeled]: description list @[labeled]:@
--
--
--
-- Notice numbers start from 1 and ignore actual index.


-- * \( \LaTeX \)
-- | Displayed via [mathjax](https://www.mathjax.org)
--
-- \( in-line math  \)
--
-- @-- \( in-line math \)@
--
-- \[ displayed math \]
--
-- @-- \[ displayed math \]@

-- * Grid tables
-- $gridTable

-- * Examples
--
-- | 
-- >>> let fib = undefined
-- >>> fib 10
-- <BLANKLINE>
-- 55
--
-- @
-- -- >>> let fib = undefined
-- -- >>> fib 10
-- -- \<BLANKLINE\>
-- -- 55
-- @
--
-- Can be utilizied by third-party programs like [doctest](https://github.com/sol/doctest#quickcheck-properties)

-- * Properties
-- | prop> a + b = b + a
-- @-- prop> a + b = b + a@
--
-- Can be used by third-party libraries like [doctest](https://github.com/sol/doctest#quickcheck-properties)

-- * Data and function annotations
   Constructors(..)
  ,Record(..)
  ,GADT(..)
  ,GADTRecord(..)
  ,identifier'
  -- * Reexported identifiers
  -- | partial reexports are listed individually
  ,module X
  ,module Y -- | total reexports are linked indirectly

  -- ** individual reexports
  ,mkPolar
  -- $mkPolarChunk
  ,imagPart
  ) where

import Data.Complex as X (pattern (:+))
import Data.Complex as Y

-- $namedChunks
-- can be used to declutter the export list,
-- and include documentation in arbitrary locations,
-- not associated with any identifier
--
-- @
-- -- \$namedChunk
-- -- lots of text
-- @

-- $inlineHeadings
-- = __Are collapsible when bold__
-- @= \__Are collapsible when bold__@
--
-- But don't show up in the outline
--
-- == Nested subheaders fall under collapse
-- @== Nested subheaders fall under collapse@
--
-- = But larger headers do not
-- @= But larger headers do not@

-- $gridTable
-- This is a grid table:
--
-- +------------------------+------------+----------+----------+
-- | Header row, column 1   | Header 2   | Header 3 | Header 4 |
-- | (header rows optional) |            |          |          |
-- +========================+============+==========+==========+
-- | body row 1, column 1   | column 2   | column 3 | column 4 |
-- +------------------------+------------+----------+----------+
-- | body row 2             | Cells may span columns.          |
-- +------------------------+------------+---------------------+
-- | body row 3             | Cells may  | \[                  |
-- +------------------------+ span rows. | f(n) = \sum_{i=1}   |
-- | body row 4             |            | \]                  |
-- +------------------------+------------+---------------------+


-- | This identifier contains an #anchor_label#
-- @This identifier contains an \#anchor_label#@
--
-- @since 0.1.0.0
identifier' :: ()
identifier' = ()

data Constructors a = C1 a -- ^ The first constructor
                    | C2 a -- ^ another constructor
                    | C3   -- ^ The last constructor

data GADT a where
  -- | The first constructor
  GADT1 :: a {- ^ marking arguments -} -> a {- ^ inline -} -> GADT a {- ^ and return val -}
  -- | The second constructor
  GADT2 :: () -- ^ Marking arguments
        -> GADT a -- ^ On each line
data Record a b = Record {field_a :: a  {- ^ It's @a@ -} ,field_b :: b  {- ^ It's @b@ -}}
data GADTRecord a b where
  GADTRecord :: {fa :: a {- ^ first -}, fb :: [a] {-^ second -} } -> GADTRecord a a -- ^ return

-- $mkPolarChunk
-- Original documentation can be augmented with named chunks,
-- but is not attached to the identifier so will not appear on reexports
