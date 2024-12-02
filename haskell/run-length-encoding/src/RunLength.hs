module RunLength (decode, encode) where

decode :: String -> String
decode "" = ""
decode encodedText
  | (n, ch, ss) <- takeGroup encodedText = replicate n ch ++ decode ss
  where
    takeGroup s = let
      cnt = takeWhile (`elem` ['0'..'9']) s
      ss = drop (length cnt) s
      (ch: ss') = ss
      in
        (if not (null cnt) then read cnt else 1, ch, ss')

encode :: String -> String
encode s = let
  (n, s') = _encode s 
  in if n > 1 then show n ++ s' else s'
  where
    _encode "" = (0, "")
    _encode [a] = (1, a:"")
    _encode (a:as) = let
      (n', s'@(a':_)) = _encode as
      in if a == a' then (n'+1, s') else (1, a:(if n' > 1 then show n' ++ s' else s'))
