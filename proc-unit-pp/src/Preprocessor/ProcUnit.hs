-- | A tiny source preprocessor for arrow @proc@ blocks.
--
-- Inside every @proc pat -> do@ block, a binding statement of the form
--
-- > val <- cmd
--
-- which does not mention @-<@ or @-<<@ is rewritten to
--
-- > val <- cmd -< Data.Proxy.Proxy @(... TypeError ...)
--
-- The input is a 'Data.Proxy.Proxy' whose type parameter is a custom
-- 'GHC.TypeLits.TypeError'. If @cmd@ is polymorphic in its input this is
-- harmless; if it actually needs an input, GHC reports the error message
-- below instead of an unhelpful unification failure.
--
-- The qualified imports needed by this expression are injected into every
-- module, followed by a @LINE@ pragma so source locations stay accurate.
--
-- We do not parse Haskell. Instead, we lex the source just well enough to
-- know where comments, strings, brackets and tokens are (using megaparsec),
-- and then use the layout rule to find statements in @proc@ blocks. Every
-- token is reproduced verbatim; we only insert text, never newlines (other
-- than the import block, which is compensated for by a @LINE@ pragma).
--
-- Use it as a GHC preprocessor:
--
-- > {-# OPTIONS_GHC -F -pgmF proc-unit-pp #-}
module Preprocessor.ProcUnit
  ( preprocess
  , preprocessorMain
  , Token (..)
  , TokKind (..)
  , lexHaskell
  ) where

import Control.Monad (void)
import Data.Char (isAlphaNum, isSpace)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.Void (Void)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)
import Text.Megaparsec hiding (Token)
import Text.Megaparsec.Char

------------------------------------------------------------------------------
-- Lexing

data TokKind
  = TWhite       -- ^ whitespace (incl. newlines)
  | TComment     -- ^ line or (nested) block comment, incl. pragmas
  | TString      -- ^ string literal
  | TChar        -- ^ character literal
  | TWord        -- ^ identifier, keyword, or number
  | TSym         -- ^ operator symbol
  | TOpen        -- ^ @(@, @[@, @{@
  | TClose       -- ^ @)@, @]@, @}@
  | TSpecial     -- ^ @,@, @;@, @`@, or anything else
  deriving stock (Eq, Show)

data Token = Token
  { tokKind :: TokKind
  , tokText :: String
  , tokLine :: Int
  , tokCol  :: Int
  }
  deriving stock (Eq, Show)

type Parser = Parsec Void String

lexHaskell :: FilePath -> String -> Either String [Token]
lexHaskell fp src =
  either (Left . errorBundlePretty) Right $
    parse (many token' <* eof) fp src

token' :: Parser Token
token' = do
  SourcePos _ l c <- getSourcePos
  (k, t) <- choice
    [ (TWhite,) <$> takeWhile1P (Just "whitespace") isSpace
    , (TComment,) <$> cppLine (unPos c)
    , (TComment,) <$> try blockComment
    , (TComment,) <$> try lineComment
    , (TString,) <$> stringLit
    , (TChar,) <$> try charLit
    , (TWord,) <$> word
    , (TSym,) <$> takeWhile1P (Just "symbol") isSymChar
    , (TOpen,) . pure <$> oneOf "([{"
    , (TClose,) . pure <$> oneOf ")]}"
    , (TSpecial,) . pure <$> anySingle
    ]
  pure $ Token k t (unPos l) (unPos c)

-- | A line beginning with @#@ in column 1. When run via @-F@, GHC has already
-- run CPP, so these are linemarkers like @# 12 "src/Foo.hs"@.
cppLine :: Int -> Parser String
cppLine col
  | col /= 1 = fail "not at column 1"
  | otherwise = do
      h <- char '#'
      notFollowedBy (satisfy isSymChar)
      rest <- takeWhileP Nothing (/= '\n')
      pure (h : rest)

isSymChar :: Char -> Bool
isSymChar = (`elem` ("!#$%&*+./<=>?@\\^|-~:" :: String))

word :: Parser String
word = do
  c <- satisfy (\x -> isAlphaNum x || x == '_')
  cs <- takeWhileP Nothing (\x -> isAlphaNum x || x == '_' || x == '\'')
  pure (c : cs)

-- | @--@ followed by a non-symbol char (otherwise it's an operator like @-->@).
lineComment :: Parser String
lineComment = do
  dashes <- takeWhile1P Nothing (== '-')
  if length dashes < 2 then fail "not a comment" else pure ()
  notFollowedBy (satisfy isSymChar)
  rest <- takeWhileP Nothing (/= '\n')
  pure (dashes <> rest)

blockComment :: Parser String
blockComment = do
  open <- string "{-"
  body <- go
  pure (open <> body)
  where
    go = choice
      [ string "-}"
      , (<>) <$> blockComment <*> go
      , (:) <$> anySingle <*> go
      ]

stringLit :: Parser String
stringLit = do
  void (char '"')
  body <- many (escape <|> (pure <$> noneOf "\"\\"))
  void (char '"')
  pure ("\"" <> concat body <> "\"")
  where
    escape = (\a b -> [a, b]) <$> char '\\' <*> anySingle

charLit :: Parser String
charLit = do
  void (char '\'')
  body <- ((\a b -> [a, b]) <$> char '\\' <*> anySingle)
          <|> ((\a -> [a]) <$> noneOf "'\\")
  -- multi-char escapes like '\n' are covered; '\DEL' etc. via takeWhile
  more <- takeWhileP Nothing (/= '\'')
  if length more > 4 then fail "not a char literal" else pure ()
  void (char '\'')
  pure ("'" <> body <> more <> "'")

------------------------------------------------------------------------------
-- Layout-aware rewriting

-- | Significant tokens, paired with their index in the full token stream.
type Sig = [(Int, Token)]

isSignificant :: Token -> Bool
isSignificant t = tokKind t `notElem` [TWhite, TComment]

preprocess :: FilePath -> String -> Either String String
preprocess fp src = do
  toks <- lexHaskell fp src
  let sig = filter (isSignificant . snd) $ zip [0 ..] toks
      Edits ins = findProcs sig
      body = concat
        [ tokText t <> IM.findWithDefault "" i ins
        | (i, t) <- zip [0 ..] toks
        ]
  pure $
    if IM.null ins
      then body -- nothing rewritten: leave the module untouched
      else insertImports fp toks (importSite (map snd sig)) body

-- | The expression fed to commands that were given no input.
implicitInput :: String
implicitInput =
  "Data.Proxy.Proxy @(GHC.TypeError.Assert 'False (GHC.TypeError.Unsatisfiable ("
    <> "'GHC.TypeError.Text \"Can't implicitly instantiate this arrow.\")))"

feed :: String
feed = " -< " <> implicitInput

importLines :: [String]
importLines =
  [ "import qualified GHC.TypeError"
  , "import qualified Data.Proxy"
  ]

-- | Where to put the injected imports: the @(line, column)@ of the first
-- significant token of the module body. That is the first token after the
-- module header's @where@, or the first token of the file if there is no
-- header. 'Nothing' means the module body is empty.
importSite :: [Token] -> Maybe (Int, Int)
importSite ts = case ts of
  (t : rest) | tokKind t == TWord, tokText t == "module" -> afterWhere (0 :: Int) rest
  (t : _) -> Just (pos t)
  [] -> Nothing
  where
    pos t = (tokLine t, tokCol t)
    afterWhere _ [] = Nothing
    afterWhere d (t : rest) = case tokKind t of
      TOpen -> afterWhere (d + 1) rest
      TClose -> afterWhere (d - 1) rest
      TWord | d == 0, tokText t == "where" -> pos <$> listToMaybe rest
      _ -> afterWhere d rest
    listToMaybe (x : _) = Just x
    listToMaybe [] = Nothing

-- | Insert the import block before the given line, indented to the module
-- body's layout column, followed by a @LINE@ pragma restoring numbering.
-- Any CPP linemarker or @LINE@ pragma earlier in the input is honoured.
insertImports :: FilePath -> [Token] -> Maybe (Int, Int) -> String -> String
insertImports fp toks site body =
  let ls = lines body
      (lineNo, col) = case site of
        Just s -> s
        Nothing -> (length ls + 1, 1)
      (before, after) = splitAt (lineNo - 1) ls
      indent = replicate (col - 1) ' '
      (origLine, origFile) = originalPos fp toks lineNo
      importBlock =
        map (indent <>) importLines
          <> ["{-# LINE " <> show origLine <> " " <> show origFile <> " #-}"]
   in unlines (before <> importBlock <> after)

-- | Map a line of the input back to the original source, using the last
-- linemarker (@# N "file"@) or @LINE@ pragma that precedes it.
originalPos :: FilePath -> [Token] -> Int -> (Int, FilePath)
originalPos fp toks lineNo =
  case [ (tokLine t, n, f)
       | t <- toks
       , tokKind t == TComment
       , tokLine t < lineNo
       , Just (n, f) <- [marker (tokText t)]
       ] of
    [] -> (lineNo, fp)
    ms -> let (ml, n, f) = last ms in (n + lineNo - ml - 1, f)
  where
    marker s = case words s of
      ("#" : n : f : _) | Just k <- readNat n -> Just (k, unquote f)
      ("{-#" : "LINE" : n : f : _) | Just k <- readNat n -> Just (k, unquote f)
      _ -> Nothing
    readNat n
      | not (null n), all (`elem` ['0' .. '9']) n = Just (read n)
      | otherwise = Nothing
    unquote = filter (/= '"')

-- | Text to insert immediately after the token with the given index.
newtype Edits = Edits (IntMap String)

instance Semigroup Edits where
  Edits a <> Edits b = Edits (IM.unionWith (<>) a b)

instance Monoid Edits where
  mempty = Edits mempty

insertAfter :: Int -> String -> Edits
insertAfter i s = Edits (IM.singleton i s)

-- | Find every @proc ... -> do@ and collect the edits for its statements.
findProcs :: Sig -> Edits
findProcs [] = mempty
findProcs ((_, t) : rest)
  | tokKind t == TWord, tokText t == "proc"
  , (_ : (_, d) : body@((_, b0) : _)) <- dropWhile (not . isArrow . snd) rest
  , tokKind d == TWord, tokText d == "do"
  , not (isOpenBrace b0)
  = block (tokCol b0) body <> findProcs rest
  | otherwise = findProcs rest
  where
    isArrow x = tokKind x == TSym && tokText x == "->"
    isOpenBrace x = tokText x == "{"

-- | Process a layout block whose statements begin at the given column.
block :: Int -> Sig -> Edits
block col = go
  where
    go [] = mempty
    go ts =
      let (stmt, rest) = statement col ts
       in if null stmt then mempty else stmtEdit stmt <> go rest

-- | Split off one statement. Returns an empty statement when the block ends.
statement :: Int -> Sig -> (Sig, Sig)
statement _ [] = ([], [])
statement col (first : more)
  | tokCol (snd first) < col || isWhere (snd first) = ([], first : more)
  | otherwise =
      let d0 = if tokKind (snd first) == TOpen then 1 else 0 :: Int
          (s, r) = loop d0 (opensLayout [first]) (snd first) more
       in (first : s, r)
  where
    -- @inner@ records whether an implicit layout block has been opened at
    -- depth zero; if so, any @;@ belongs to that inner block, not to us.
    loop _ _ _ [] = ([], [])
    loop depth inner prev (x@(_, t) : xs)
      | tokKind t == TClose && depth == 0 = ([], x : xs) -- closes the enclosing bracket
      | depth == 0 && isWhere t = ([], x : xs)
      | depth == 0 && tokLine t /= tokLine prev && tokCol t <= col = ([], x : xs)
      | depth == 0 && not inner && tokText t == ";" = ([x], xs)
      | otherwise =
          let depth' = case tokKind t of
                TOpen  -> depth + 1
                TClose -> depth - 1
                _      -> depth
              inner' = inner || (depth == 0 && opensLayout [(0, prev), x])
              (s, r) = loop depth' inner' t xs
           in (x : s, r)

isWhere :: Token -> Bool
isWhere t = tokKind t == TWord && tokText t == "where"

-- | Rewrite a single statement. Normally we append @ -< ()@ to the end of the
-- command. But if the command opens a layout block at bracket depth zero
-- (e.g. @foo $ do ...@), anything appended would land /inside/ that block,
-- so instead we wrap the command in parentheses: @val <- (cmd) -< ()@. The
-- closing paren terminates the implicit layout block via the parse-error(t)
-- rule, so this is valid regardless of how the block is laid out.
stmtEdit :: Sig -> Edits
stmtEdit [] = mempty
stmtEdit stmt@((_, t0) : rest)
  | tokText t0 == "rec", ((_, n) : _) <- rest = block (tokCol n) rest
  | tokText t0 == "let" = mempty
  | any ((== "proc") . tokText . snd) stmt = mempty -- handled by findProcs
  | Just (_, cmd) <- splitTopBind stmt = cmdEdit True cmd
  -- A bare command statement: never feed it, but still look inside it for
  -- control forms and nested blocks.
  | otherwise = cmdEdit False stmt

-- | Rewrite a single command.
--
-- * @do@ blocks are processed as nested statement blocks.
-- * Control-operator forms (see 'controlForms') have each of their command
--   arguments rewritten recursively; the form itself is never fed.
-- * Otherwise, if @feedPlain@ is set and the command has no @-<@ / @-<<@,
--   the implicit input is appended (wrapping in parens if necessary).
cmdEdit :: Bool -> Sig -> Edits
cmdEdit feedPlain cmd
  | null content = mempty
  | ((_, d) : body@((_, b0) : _)) <- content
  , tokKind d == TWord, tokText d `elem` ["do", "mdo"]
  , tokText b0 /= "{"
  = block (tokCol b0) body
  | forms@(_ : _) <- controlForms content = foldMap controlEdit forms
  | not feedPlain = mempty
  | any (isFeed . snd) content = mempty
  | opensLayout content, ((startIx, _) : _) <- content =
      insertAfter (startIx - 1) "(" <> insertAfter endIx (")" <> feed)
  | otherwise = insertAfter endIx feed
  where
    content = dropWhileEnd' ((== ";") . tokText . snd) cmd
    endIx = fst (last content)

isFeed :: Token -> Bool
isFeed t = tokKind t == TSym && tokText t `elem` ["-<", "-<<"]

dropWhileEnd' :: (a -> Bool) -> [a] -> [a]
dropWhileEnd' p = reverse . dropWhile p . reverse

------------------------------------------------------------------------------
-- Control-operator forms
--
-- Everything that knows about the /surface syntax/ of control-operator
-- application lives in this section. To support a different syntax, change
-- 'controlForms' (and, if the new syntax isn't valid GHC, have it emit
-- desugaring edits via 'ctlDesugar'); 'cmdEdit' only consumes 'Control'.

-- | A control operator applied to some commands.
data Control = Control
  { ctlArgs :: [Sig]
    -- ^ the command arguments, without any delimiters
  , ctlDesugar :: Edits
    -- ^ edits needed to turn the surface syntax into valid GHC
  }

controlEdit :: Control -> Edits
controlEdit c = ctlDesugar c <> foldMap (cmdEdit True) (ctlArgs c)

-- | Find the control forms at the top level of a command.
--
-- Current syntax: GHC banana brackets, @(| op (cmd1) ... (cmdN) |)@. Each
-- parenthesised argument after the operator is a command; any other
-- argument (e.g. a bare variable naming a command) is left alone.
controlForms :: Sig -> [Control]
controlForms = foldMap banana . atoms
  where
    banana g = case bananaInterior g of
      Just inner | (_ : args) <- atoms inner ->
        [Control (concatMap parenInterior args) mempty]
      _ -> []
    parenInterior g = case g of
      ((_, o) : xs@(_ : _))
        | tokText o == "(", Just inner <- dropLast xs, Nothing <- bananaInterior g
        -> [inner]
      _ -> []

-- | If the group is @(| ... |)@, return what's between the delimiters.
bananaInterior :: Sig -> Maybe Sig
bananaInterior g = case g of
  ((_, o) : (_, b) : rest)
    | tokText o == "(", tokText b == "|", adjacent o b
    , Just (inner, (_, b'), (_, c)) <- lastTwo rest
    , tokText b' == "|", tokText c == ")", adjacent b' c
    -> Just inner
  _ -> Nothing
  where
    lastTwo xs = case reverse xs of
      (c : b : r) -> Just (reverse r, b, c)
      _ -> Nothing

-- | Are two tokens immediately next to each other?
adjacent :: Token -> Token -> Bool
adjacent a b =
  tokLine a == tokLine b && tokCol b == tokCol a + length (tokText a)

dropLast :: [a] -> Maybe [a]
dropLast [] = Nothing
dropLast xs = Just (init xs)

-- | Split a token sequence into top-level atoms: single tokens, or whole
-- balanced bracket groups (including their brackets).
atoms :: Sig -> [Sig]
atoms [] = []
atoms (x@(_, t) : xs)
  | tokKind t == TOpen = let (g, r) = group (1 :: Int) xs in (x : g) : atoms r
  | otherwise = [x] : atoms xs
  where
    group _ [] = ([], [])
    group d (y@(_, u) : ys) =
      let d' = case tokKind u of
            TOpen -> d + 1
            TClose -> d - 1
            _ -> d
       in if d' == 0
            then ([y], ys)
            else let (g, r) = group d' ys in (y : g, r)

-- | Split at the first @<-@ at bracket depth zero, returning the index of the
-- @<-@ token and the tokens of the command after it.
splitTopBind :: Sig -> Maybe (Int, Sig)
splitTopBind = go (0 :: Int)
  where
    go _ [] = Nothing
    go d ((i, t) : ts) = case tokKind t of
      TOpen -> go (d + 1) ts
      TClose -> go (d - 1) ts
      TSym | d == 0, tokText t == "<-" -> Just (i, ts)
      _ -> go d ts

-- | Does this expression open an implicit layout block at bracket depth zero?
-- That is: @do@, @mdo@, @of@ (from @case@), or @\\case@ / @\\cases@.
opensLayout :: Sig -> Bool
opensLayout = go (0 :: Int) Nothing
  where
    go _ _ [] = False
    go d prev ((_, t) : ts) = case tokKind t of
      TOpen -> go (d + 1) (Just t) ts
      TClose -> go (d - 1) (Just t) ts
      TWord
        | d == 0
        , tokText t `elem` ["do", "mdo", "of"]
            || (tokText t `elem` ["case", "cases"] && fmap tokText prev == Just "\\")
        -> True
      _ -> go d (Just t) ts

------------------------------------------------------------------------------
-- GHC -pgmF entry point: @proc-unit-pp ORIGINAL INPUT OUTPUT@

preprocessorMain :: IO ()
preprocessorMain = getArgs >>= \case
  [orig, input, output] -> do
    src <- readFile input
    case preprocess orig src of
      Left err -> hPutStrLn stderr err >> exitFailure
      Right out ->
        writeFile output $ "{-# LINE 1 " <> show orig <> " #-}\n" <> out
  _ -> do
    hPutStrLn stderr "usage: proc-unit-pp ORIGINAL INPUT OUTPUT"
    exitFailure
