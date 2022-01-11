Serializing
-----------

.. class:: nodoc

> module Codec.Pesto.Serialize (serialize, prettyPrint, style) where
> import Data.Char (isSpace, isLetter)
> import Data.Ratio (numerator, denominator)
>
> import {-# SOURCE #-} Codec.Pesto.Parse

> class Serializeable a where
> 	serialize :: a -> String

.. class:: todo

- Add instance for graph
- use :math:`\mathcal{O}(1)` string builder

Finally transform linear stream of instructions into a string again:

> instance Serializeable a => Serializeable [a] where
> 	serialize ops = unlines $ map serialize ops

> instance Serializeable Instruction where
> 	serialize (Annotation s) = quote '(' ')' s
> 	serialize (Ingredient q) = '+':serialize q
> 	serialize (Tool q) = '&':serialize q
> 	serialize (Action s) = quote '[' ']' s
> 	serialize (Reference q) = '*':serialize q
> 	serialize (Result q) = '>':serialize q
> 	serialize (Alternative q) = '|':serialize q
> 	serialize (Directive s) = '%':serializeQstr s
> 	serialize (Unknown s) = s

> instance Serializeable Quantity where
> 	serialize (Quantity a b "") = serialize a ++ " " ++ serializeQstr b
> 	serialize (Quantity (Exact (AmountStr "")) "" c) = serializeQstr c
> 	serialize (Quantity a "" c) = serialize a ++ " _ " ++ serializeQstr c
> 	serialize (Quantity a b c) = serialize a ++ " " ++ serializeQstr b ++ " " ++ serializeQstr c

> instance Serializeable Approximately where
> 	serialize (Range a b) = serialize a ++ "-" ++ serialize b
> 	serialize (Approx a) = '~':serialize a
> 	serialize (Exact a) = serialize a

There are two special cases here, both for aesthetic reasons:

1) If the denominator is one we can just skip printing it, because
   :math:`\frac{2}{1} = 2` and
2) if the numerator is larger than the denominator use mixed fraction notation,
   because :math:`\frac{7}{2} = 3+\frac{1}{2}`

> instance Serializeable Amount where
> 	serialize (AmountRatio a) | denominator a == 1 = show (numerator a)
> 	serialize (AmountRatio a) | numerator a > denominator a =
> 		show full ++ "/" ++ show num ++ "/" ++ show denom
> 		where
> 			full = numerator a `div` denom
> 			num = numerator a - full * denom
> 			denom = denominator a
> 	serialize (AmountRatio a) = show (numerator a) ++ "/" ++ show (denominator a)
> 	serialize (AmountStr s) = serializeQstr s

> serializeQstr "" = "_"
> serializeQstr s | (not . isLetter . head) s || hasSpaces s = quote '"' '"' s
> serializeQstr s = s
> hasSpaces = any isSpace
> quote start end s = [start] ++ concatMap (\c -> if c == end then ['\\', end] else [c]) s ++ [end]

dot
^^^


> class PrettyPrintable a where
> 	prettyPrint :: a -> String

> instance PrettyPrintable a => PrettyPrintable [a] where
> 	prettyPrint ops = unlines $ map prettyPrint ops

> instance PrettyPrintable Instruction where
> 	prettyPrint (Annotation s) = quote '(' ')' s
> 	prettyPrint (Ingredient q) = prettyPrint q
> 	prettyPrint (Tool q) = '&':prettyPrint q
> 	prettyPrint (Action s) = s
> 	prettyPrint (Reference q) = '*':prettyPrint q
> 	prettyPrint (Result q) = '>':prettyPrint q
> 	prettyPrint (Alternative q) = '|':prettyPrint q
> 	prettyPrint (Directive s) = '%':prettyPrintQstr s
> 	prettyPrint (Unknown s) = s

> instance PrettyPrintable Quantity where
> 	prettyPrint (Quantity a b "") = prettyPrint a ++ " " ++ prettyPrintQstr b
> 	prettyPrint (Quantity (Exact (AmountStr "")) "" c) = prettyPrintQstr c
> 	prettyPrint (Quantity a "" c) = prettyPrint a ++ " _ " ++ prettyPrintQstr c
> 	prettyPrint (Quantity a b c) = prettyPrint a ++ " " ++ prettyPrintQstr b ++ " " ++ prettyPrintQstr c

> instance PrettyPrintable Approximately where
> 	prettyPrint (Range a b) = prettyPrint a ++ "-" ++ prettyPrint b
> 	prettyPrint (Approx a) = '~':prettyPrint a
> 	prettyPrint (Exact a) = prettyPrint a

There are two special cases here, both for aesthetic reasons:

1) If the denominator is one we can just skip printing it, because
   :math:`\frac{2}{1} = 2` and
2) if the numerator is larger than the denominator use mixed fraction notation,
   because :math:`\frac{7}{2} = 3+\frac{1}{2}`

> instance PrettyPrintable Amount where
> 	prettyPrint (AmountRatio a) | denominator a == 1 = show (numerator a)
> 	prettyPrint (AmountRatio a) | numerator a > denominator a =
> 		show full ++ "/" ++ show num ++ "/" ++ show denom
> 		where
> 			full = numerator a `div` denom
> 			num = numerator a - full * denom
> 			denom = denominator a
> 	prettyPrint (AmountRatio a) = show (numerator a) ++ "/" ++ show (denominator a)
> 	prettyPrint (AmountStr s) = prettyPrintQstr s

> prettyPrintQstr "" = "_"
> prettyPrintQstr s | (not . isLetter . head) s || hasSpaces s = quote '"' '"' s
> prettyPrintQstr s = s



dot style
^^^^^^^^^

> class Styleable a where
> 	style :: a -> [(String, String)]


> instance Styleable Instruction where
> 	style (Annotation s) = [("shape", "none")]
> 	style (Ingredient q) = [("shape", "none")]
> 	style (Tool q) = [("shape", "box")]
> 	style (Action s) = [("shape", "triangle"), ("orientation", "270")]
> 	style (Reference q) = []
> 	style (Result q) = []
> 	style (Alternative q) = []
> 	style (Directive s) = []
> 	style (Unknown s) = []

> instance Styleable Quantity where
> 	style (Quantity a b "") = []
> 	style (Quantity (Exact (AmountStr "")) "" c) = []
> 	style (Quantity a "" c) = []
> 	style (Quantity a b c) = []

> instance Styleable Approximately where
> 	style (Range a b) = []
> 	style (Approx a) = []
> 	style (Exact a) = []

There are two special cases here, both for aesthetic reasons:

1) If the denominator is one we can just skip printing it, because
   :math:`\frac{2}{1} = 2` and
2) if the numerator is larger than the denominator use mixed fraction notation,
   because :math:`\frac{7}{2} = 3+\frac{1}{2}`

> instance Styleable Amount where
> 	style (AmountRatio a) | denominator a == 1 = []
> 	style (AmountRatio a) | numerator a > denominator a =
> 		[]
> 		where
> 			full = numerator a `div` denom
> 			num = numerator a - full * denom
> 			denom = denominator a
> 	style (AmountRatio a) = []
> 	style (AmountStr s) = []

> styleQstr "" = "_"
> styleQstr s | (not . isLetter . head) s || hasSpaces s = quote '"' '"' s
> styleQstr s = s
