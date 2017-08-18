module Spans where

import Prelude
import Data.List
import Data.String (joinWith)
import Data.String as String
import Data.String.Regex (match, test, Regex, parseFlags)
import Data.String.Regex.Flags (noFlags)
import Data.String.Regex.Unsafe (unsafeRegex)
import Data.Foldable (and, or, sum)
import Data.Array as Array
import Data.Maybe
import Data.Tuple (Tuple(..))
import Data.Generic

type Span = { text :: String,
              links :: List Int,
              labels :: List String,
              moved :: Boolean
            }

type Spans = List Span

newtype WrapSpan  = WrapSpan Span

derive instance eqWrapSpan :: Eq WrapSpan
derive instance genericWrapSpan :: Generic WrapSpan

eqSpan :: Span -> Span -> Boolean
eqSpan s1 s2 = WrapSpan s1 == WrapSpan s2

eqSpans :: Spans -> Spans -> Boolean
eqSpans s1 s2 = map WrapSpan s1 == map WrapSpan s2

mergeSpans :: Spans -> Span
mergeSpans spans = {
  text: joinWith "" (Array.fromFoldable (map _.text spans)),
  links: concatMap _.links spans,
  labels: concatMap _.labels spans,
  moved: any _.moved spans
}

emptySpan :: Span
emptySpan = mergeSpans Nil

initSpan :: String -> Span
initSpan s = emptySpan { text = s }

-- Lists for now, but could be changed to use FingerTrees for sublinear
-- complexities. Use a product measure: the text lengths and the index.
-- This is then a reference implementation.

wordsRegex :: Regex
wordsRegex = unsafeRegex "\\S*\\s+" (parseFlags "g")

tokenize :: String -> Array String
tokenize original = Array.catMaybes (fromMaybe [] (match wordsRegex (original <> " ")))

init :: Array String -> Spans
init original = map initSpan (fromFoldable original)

newtype Problem = Problem String

problem :: String -> Maybe Problem
problem = Just <<< Problem

-- Text starts with non-whitespace and ends with whitespace with anything in between.
textRegex :: Regex
textRegex = unsafeRegex "^\\S(.|\\n)*\\s$" noFlags

-- First span does not need to start a word, but cannot be empty and must end with whitespace.
nonemptyRegex :: Regex
nonemptyRegex = unsafeRegex "\\s$" noFlags

tails :: forall a . List a -> List (Tuple a (List a))
tails Nil      = Nil
tails (x : xs) = Tuple x xs : tails xs

increasing :: forall a . Ord a => List a -> Boolean
increasing xs = and (zipWith (<=) xs (drop 1 xs))

-- note: functions null/all/or are strict, consider changing to iterators or lazy lists

intersects :: forall a . Ord a => List a -> List a -> Boolean
intersects a b = not (null (a `intersect` b))

checkInvariant :: Spans -> Maybe Problem
checkInvariant spans@(first : rest)
  | not (test nonemptyRegex first.text) = problem "Empty head"
  | not (all (\ s -> test textRegex s.text) rest) = problem "Not text"
  | or do
      Tuple s ss <- tails spans
      s2 <- ss
      pure (s.links `intersects` s2.links) = problem "Links are not injective"
  | not (increasing (concatMap _.links (filter (\ s -> not s.moved) spans)))
     = problem "Links are not increasing"
  | any (_ < 0) (concatMap _.links spans)
     = problem "Negative link"
  | any (\ s -> s.moved && null s.links) spans
     = problem "Moved but without links"
checkInvariant _ = Nothing

type SplitResult = {
    before :: Spans,
    pre :: String,
    pivot :: Span,
    post :: String,
    after :: Spans }

prepend :: Span -> SplitResult -> SplitResult
prepend s r = r { before = s : r.before }

splitAtOffset :: Int -> Spans -> SplitResult
splitAtOffset _ Nil = { before: Nil, pre: "", pivot: initSpan "", post: "", after: Nil }
splitAtOffset i (x : xs)
    | i > String.length x.text = prepend x (splitAtOffset (i - String.length x.text) xs)
    | otherwise = {
        before: Nil,
        pre: String.take i x.text,
        pivot: x,
        post: String.drop i x.text,
        after: xs
      }

textLength :: Spans -> Int
textLength = sum <<< map (\ s -> String.length s.text)

modify :: Int -> Int -> String -> Spans -> Spans
modify begin end text spans =
  let a = splitAtOffset begin spans
      z = splitAtOffset end spans
      mid = drop (length a.before) (z.before) <> z.pivot : Nil
      new_span = (mergeSpans mid) { text = a.pre <> text <> z.post }
  in  cleanup (a.before <> Cons new_span z.after)

-- This is preformed after modify to preserve the invariants.
-- This is linear right now but could be optimized to only look at the
-- spans involved at the modification.
cleanup :: Spans -> Spans
cleanup = mergeNoFinalWhitespace <<< removeEmpty <<< moveWhitespace

initialWhitespaceRegex :: Regex
initialWhitespaceRegex = unsafeRegex "^(\\s+)(.*)$" noFlags

-- | Shuffles initial whitespace from a token to the previous one
moveWhitespace :: Spans -> Spans
moveWhitespace (prev : me : rest) =
  case match initialWhitespaceRegex me.text of
    Just [Just s, Just w] -> prev { text = prev.text <> s } :
                             moveWhitespace (me { text = w } : rest)
    _ -> prev : moveWhitespace (me : rest)
moveWhitespace xs = xs

-- | Remove spans with empty text
removeEmpty :: Spans -> Spans
removeEmpty = filter (\ s -> String.length s.text > 0)

noFinalWhitespaceRegex :: Regex
noFinalWhitespaceRegex = unsafeRegex "\\S$" noFlags

-- | Merge tokens without final whitespace with next token
mergeNoFinalWhitespace :: Spans -> Spans
mergeNoFinalWhitespace (me : next : rest)
  | test noFinalWhitespaceRegex me.text =
    mergeNoFinalWhitespace (mergeSpans (me : next : Nil) : rest)
  | otherwise = me : mergeNoFinalWhitespace (next : rest)
mergeNoFinalWhitespace xs = xs

-- | Reverts a span if it matches the original exactly.
-- |
-- | Only performed when this breaks up a span into many.
-- |
-- | Linear, but can be optimized to only look at relevant spans.
autoRevert :: Array String -> Spans -> Spans
autoRevert _ Nil = Nil
autoRevert original (x : xs)
  | length x.links > 1 &&
    not (x.moved) &&
    x.text == joinWith "" (Array.fromFoldable (mapMaybe (Array.index original) x.links)) =
    mapMaybe (map initSpan <<< Array.index original) x.links <> autoRevert original xs
  | otherwise = x : autoRevert original xs

splitAt :: forall a . Int -> List a -> { before :: List a, after :: List a }
splitAt i Nil      = { before: Nil, after: Nil }
splitAt 0 xs       = { before: Nil, after: xs }
splitAt i (x : xs) = let r = splitAt (i - 1) xs in r { before = x : r.before }

sliceSplit :: forall a . Int -> Int -> List a -> { before :: List a, slice :: List a, after :: List a }
sliceSplit begin end xs =
  let a = splitAt begin xs
      b = splitAt (end - begin) a.after
  in  { before: a.before, slice: b.before, after: b.after }

rearrange :: Int -> Int -> Int -> Spans -> Spans
rearrange begin end dest spans =
  let {before, slice, after} = sliceSplit begin end spans
      mod_dest | dest >= end = dest - (end - begin)
               | otherwise   = dest
      r = splitAt mod_dest (before <> after)
  in  r.before <> map _{ moved = true } slice <> r.after


