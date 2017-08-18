module Test.Spans where

import Spans

import Prelude
import Partial.Unsafe
import Jack
import Jack (oneOf)
import Data.String (length, joinWith)
import Control.Monad.State (State, get, evalState)
import Control.Monad.State as State
import Data.String (fromCharArray)
import Data.List
import Data.List as List
import Data.Array as Array
import Data.Tuple
import Data.Maybe
import Data.String.Regex (test)
import Data.Traversable
import Data.Generic

genShrink :: forall a . Gen a -> Gen a
genShrink g =
  do random_tree <- mkGen_ (runGen g)
     elements (Array.fromFoldable (map outcome (shrinks random_tree)))

printable :: Gen Char
printable = noShrink $ frequency
  [ Tuple 10 (chooseChar 'A' 'Z')
  , Tuple 30 (chooseChar 'a' 'z')
  , Tuple 10 (chooseChar '0' '9')
  , Tuple 1 (pure '.')
  , Tuple 1 (pure ',')
  , Tuple 1 (pure '!')
  , Tuple 1 (pure '?')
  ]

whitespace :: Gen Char
whitespace = noShrink $ frequency
  [ Tuple 25 (pure ' ')
  , Tuple 1 (pure '\n')
  ]

genLabel :: Gen String
genLabel = fromCharArray <$> arrayOf printable

genText :: Gen String
genText = fromCharArray <$> do
  a <- printable
  b <- arrayOf (noShrink $ frequency [Tuple 5 printable, Tuple 1 whitespace])
  c <- whitespace
  pure ([a] <> b <> [c])

genNonempty :: Gen String
genNonempty = fromCharArray <$> do
  a <- arrayOf (noShrink $ frequency [Tuple 5 printable, Tuple 1 whitespace])
  b <- whitespace
  pure (a <> [b])

prop_genText :: Property
prop_genText = forAll genText (property <<< test textRegex)

prop_genNonempty :: Property
prop_genNonempty = forAll genText (property <<< test nonemptyRegex)

-- assumes positive numbers only
increasingLinks :: List (List Int) -> List (List Int)
increasingLinks v = evalState (go v) 0
  where
  go :: List (List Int) -> State Int (List (List Int))
  go Nil              = pure Nil
  go (Nil      : xss) = (Nil:_) <$> go xss
  go ((x : xs) : xss) = unsafePartial
    do State.modify (_ + x)
       y <- get
       ys : yss <- go (xs : xss)
       pure ((y : ys) : yss)

genBoolean :: Gen Boolean
genBoolean = elements [true, false]

-- O(n^2)
genPermutation :: forall a . List a -> Gen (List a)
genPermutation Nil = pure Nil
genPermutation xs =
  do i <- chooseInt 0 (List.length xs - 1)
     let r = splitAt i xs
     case r.after of
       b : bs -> (b:_) <$> genPermutation (r.before <> bs)
       Nil    -> unsafeCrashWith "genPermutation: unreachable"

-- requires strictly increasing indicies
updateIndicies :: forall a . List (Tuple Int a) -> List a -> List a
updateIndicies = go 0
  where
  go i js0@(Tuple j a' : js) (a : as)
    | i == j    = a' : go (i + 1) js as
    | otherwise = a : go (i + 1) js0 as
  go _ Nil as = as
  go _ _  Nil = unsafeCrashWith "updateIndicies: updates remaining"

genMoves :: List (List Int) -> Gen (List (Tuple Boolean (List Int)))
genMoves xss =
  do ims <- filterM (\ (Tuple i m) -> (not (null m) && _) <$> genBoolean)
                    (mapWithIndex Tuple xss)
     let Tuple is ms = unzip ims
     ms2 <- genPermutation ms
     pure (updateIndicies (zip is (map (Tuple true) ms2)) (map (Tuple false) xss))

showSpans :: Spans -> String
showSpans = gShow <<< map WrapSpan

genSpans :: Gen Spans
genSpans =
  do ts <- Cons <$> genNonempty <*> listOf genText
     pure (map (\ t -> {text: t, links: Nil, moved: false, labels: Nil}) ts)
  {-
     ls0 <- increasingLinks <$> traverse (\ _ -> listOf (chooseInt 1 10)) ts
     ls <- genMoves ls0
     traverse (\ (Tuple t (Tuple m l)) ->
         { text: t,
           links: l,
           moved: m,
           labels: _
         } <$> listOf genLabel
       )
       (zip ts ls)
       -}

prop_genSpans :: Property
prop_genSpans = forAllRender showSpans genSpans (property <<< isNothing <<< checkInvariant)

prop_spec_splitAtOffset :: Property
prop_spec_splitAtOffset =
  forAllRender showSpans genSpans \ spans ->
  forAll (chooseInt 0 (textLength spans)) \ i -> property $
    let r = splitAtOffset i spans
    in  textLength r.before + length r.pre == i &&
        ((r.before <> r.pivot : r.after) `eqSpans` spans) &&
        r.pre <> r.post == r.pivot.text

prop_modify_nothing :: Property
prop_modify_nothing =
  forAllRender showSpans genSpans \ spans ->
  forAll (chooseInt 0 (textLength spans)) \ i -> property $
    modify i i "" spans `eqSpans` spans

-- other properties for modify:
-- * length property
-- * invariant should hold after any modify

