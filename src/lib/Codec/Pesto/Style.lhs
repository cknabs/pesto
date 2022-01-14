Styling
-----------

.. class:: nodoc

> module Codec.Pesto.Style (prettyPrint, style) where
> import Data.Ratio (numerator, denominator)
> import Data.Text (pack, unpack)
> import Text.Wrap (wrapText, defaultWrapSettings)
>
> import Codec.Pesto.Parse

> maxLineLen = 30

> class PrettyPrintable a where
> 	prettyPrint :: a -> String

> instance PrettyPrintable a => PrettyPrintable [a] where
> 	prettyPrint ops = unlines $ map prettyPrint ops

> instance PrettyPrintable Instruction where
> 	prettyPrint (Annotation s) = quote '(' ')' s
> 	prettyPrint (Ingredient q) = prettyPrint q
> 	prettyPrint (Tool q) = prettyPrint q
> 	prettyPrint (Action s) = unpack (wrapText defaultWrapSettings maxLineLen (pack s))
> 	prettyPrint (Reference q) = prettyPrint q
> 	prettyPrint (Result q) = prettyPrint q
> 	prettyPrint (Alternative q) = '|':prettyPrint q
> 	prettyPrint (Directive s) = '%':prettyPrintQstr s
> 	prettyPrint (Unknown s) = s

> instance PrettyPrintable Quantity where
> 	prettyPrint (Quantity a b "") = prettyPrint a ++ " " ++ prettyPrintQstr b
> 	prettyPrint (Quantity (Exact (AmountStr "")) "" c) = prettyPrintQstr c
> 	prettyPrint (Quantity a "" c) = prettyPrint a ++ " " ++ prettyPrintQstr c
> 	prettyPrint (Quantity a b c) = prettyPrint a ++ " " ++ prettyPrintQstr b ++ " " ++ prettyPrintQstr c

> instance PrettyPrintable Approximately where
> 	prettyPrint (Range a b) = prettyPrint a ++ "-" ++ prettyPrint b
> 	prettyPrint (Approx a) = '~':prettyPrint a
> 	prettyPrint (Exact a) = prettyPrint a

> quote start end s = [start] ++ concatMap (\c -> if c == end then ['\\', end] else [c]) s ++ [end]

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

> prettyPrintQstr s = s


> class Styleable a where
> 	style :: a -> [(String, String)]

> instance Styleable Instruction where
> 	style (Ingredient _) = [("color", "#40A040"), ("fontcolor", "#FFFFFF")]
> 	style _ = []
> instance Styleable Quantity where
> 	style _ = []

> instance Styleable Approximately where
> 	style _ = []

> instance Styleable Amount where
> 	style _ = []
