--
-- 『素因数分解と素数判定』
--
-- 第1章 素因数一意分解とユークリッド互除法
--

-- 1.7 : ユークリッド互除法を使って gcd(a, b) を計算する。
gcd1 :: Integral a => a -> a -> a
gcd1 a 0 = abs a
gcd1 a b = gcd1 b (mod a b)


-- 1.8 : gcd だけでなく、m と n も計算するドナルド・クーヌスのアルゴリズム。
gcd2 :: Integral a => a -> a -> (a, a, a)
gcd2 a b = loop 1 0 a 0 1 b
  where
    loop u1 u2 u3 _  _  0  = (u1, u2, u3)
    loop u1 u2 u3 v1 v2 v3 = loop v1 v2 v3 (u1 - q * v1) (u2 - q * v2) (u3 - q * v3)
      where q = div u3 v3

gcd2' :: Integral a => a -> a -> (a, a, a)
gcd2' a b = loop [(a, b), (1, 0), (0, 1)] -- loop [(u3, v3), (u1, v1), (u2, v2)]
  where
    loop [(u3, 0), (u1, _), (u2, _)] = (u1, u2, u3)
    loop xs@((u3, v3):_) = loop $ map (\(u, v) -> (v, u - q * v)) xs
      where q = div u3 v3
