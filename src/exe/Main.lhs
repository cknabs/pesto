User interface
++++++++++++++

.. class:: nodoc

> module Main (main) where
> import System.Environment (getArgs)
> import Data.Function (on)
> import Data.List (intercalate, sortBy)
>
> import Codec.Pesto.Parse (parse, Instruction (Ingredient, Tool, Annotation, Action, Reference, Result), Quantity (..))
> import Codec.Pesto.Graph (extract, toGraph, firstNodeId, resolveReferences)
> import Codec.Pesto.Lint (lint, extractMetadata, Metadata(..), LintResult (LintResult))
> import Codec.Pesto.Serialize (serialize, prettyPrint, style)

The user-interface has different modes of operation. All of them read a single
recipe from the standard input.

> main = do
> 	(op:_) <- getArgs
> 	s <- getContents
>	either malformedRecipe (run op) (parse s)

> run "dot" = runDot
> run "metadata" = runMeta
> run "ingredients" = runIngredients
> run _ = const (putStrLn "unknown operation")

> malformedRecipe = print

> streamToGraph stream = (nodes, edges)
> 	where
> 		doc = (head . extract . snd . unzip) stream
> 		nodes = zip [firstNodeId..] doc
> 		edges = toGraph nodes ++ resolveReferences nodes

dot
^^^

Since each recipe is just a directed graph (digraph), GraphViz’ dot language
can represent recipes as well. Example:

.. code:: bash

	cabal run --verbose=0 pesto dot < spaghetti.pesto | dot -Tpng > spaghetti.png

> runDot stream = putStrLn $ toDot dotNodes dotEdges nodes
> 	where
> 		(nodes, edges) = streamToGraph stream
> 		maxId = (maximum $ map fst nodes) + 1
> 		(lintNodes, lintEdges) = unzip $ map (uncurry lintToNodesEdges)
> 				$ zip [maxId..] (lint nodes edges)
> 		dotNodes = concat [
>				  [("node", [("fontname", "Roboto Semi-Light"), ("shape", "plaintext"), ("style", "filled, rounded"), ("margin", "0.2")])]
> 				, map (\(a, label) -> (show a, [("label", prettyPrint label)] ++ (style label))) nodes
> 				--, lintNodes
>				]
> 		dotEdges = concat [
> 				  map (both show) edges
> 				--, concat lintEdges
> 				]

> lintToNodesEdges nodeid (LintResult t nodes) = let
> 	n = (show nodeid, [("label", show t), ("color", "red")])
> 	e = map (\i -> both show (nodeid, i)) nodes
> 	in (n, e)

> both f (a, b) = (f a, f b)

> toDot nodes edges ns = "digraph a {\n"
>		<> "graph [rankdir=LR;splines=true;concentrate=true;nodesep=0.02;mindist=0.0;fontsize=40;ranksep=0.5]\n"
> 		<> "\t" <> intercalate "\n\t" (map nodeToDot nodes) <> "\n"
> 		<> "\t" <> intercalate "\n\t" (map edgeToDot edges) <> "\n"

Add invisible edges to order ingredients top-to-bottom (on left side) and tools left-to-right (on top).

>		<> "\t" <> "orig [style=invis];\n"
>		<> "\t" <> intercalate " -> " ("orig" : sortedIngredients) <> "[style=invis];\n"
>		<> "\t" <> intercalate " -> " ("orig" : sortedTools) <> "[style=invis;weight=999];\n"

Put ingredients on same vertical line. 

> 		<> "{rank=same;" <> intercalate ";" ("orig" : sortedIngredients) <> "}\n"
> 		<> "}"
> 	where
> 		sortedNodes = snd $ unzip $ sortBy (compare `on` fst) ns
>		sortedIngredients = reverse $ foldl getIngredient [] ns
>		sortedTools = reverse $ foldl getTool [] ns
>		getIngredient xs (i, Ingredient _) = (show i):xs
> 		getIngredient xs _ = xs
>		getTool xs (i, Tool _) = (show i):xs
> 		getTool xs _ = xs

Add a reversed (invisible) edge from an action to its tools to keep them close together on the x-axis. 

> 		edgeToDot (a, b) | (Just (Tool _), Just (Action _)) <- (sortedNodes `at` (read a :: Int), sortedNodes `at` (read b :: Int))  = a <> ":s -> " <> b <> ":n [weight=0;];" <> b <> ":n -> " <> a <> ":s [weight=0;style=invis];"
>		edgeToDot (a, b) | (Just (Annotation _), Just (Tool _)) <- (sortedNodes `at` (read a :: Int), sortedNodes `at` (read b :: Int)) = a <> ":s -> " <> b <> ":n [weight=1;style=dashed];" <> b <> ":n -> " <> a <> ":s [weight=1;style=invis];"
>		edgeToDot (a, b) | (Just (Annotation _), Just (Ingredient _)) <- (sortedNodes `at` (read a :: Int), sortedNodes `at` (read b :: Int)) = a <> ":e -> " <> b <> ":w [style=dashed];"
> 		edgeToDot (a, b) = a <> ":e -> " <> b <> ":w;"
>		nodeToDot (a, b) = a <> " [" <> intercalate "," (mapToDot b) <> "];"

> at xs i = if i < length xs then Just (xs !! i) else Nothing
> mapToDot = map kvToDot
> kvToDot (k, v) = k <> "=\"" <> quoteString v <> "\""
> quoteString s = mconcat $ map quoteChar s
> quoteChar '\n' = "\\n"
> quoteChar '"' = "\\\""
> quoteChar x = [x]

metadata
^^^^^^^^

Print metadata as key-value pairs, separated by ``=``.

> runMeta stream = maybe (return ()) (mapM_ printMeta) $ uncurry extractMetadata $ streamToGraph stream

ingredients
^^^^^^^^^^^

Extract ingredients and print them in CSV format. This does not take
alternatives into account yet.

> runIngredients stream = mapM_ (putStrLn . csvQty) $ reverse $ foldl getIngredient [] stream
> 	where
> 		getIngredient xs (_, Ingredient q) = q:xs
> 		getIngredient xs _ = xs

> printMeta (_, (key, MetaStr value)) = putStrLn $ key ++ "=" ++ value
> printMeta (_, (key, MetaQty q)) = putStrLn $ key ++ "=" ++ csvQty q

> csvQty (Quantity a b c) = intercalate "," [serialize a, b, c]

