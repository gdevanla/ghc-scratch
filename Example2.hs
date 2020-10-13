module Example2
(FundEx1(..),
 FundEx2(..)
  )
where


data FundEx1 = FundConEx1 {z1 :: Integer, y1 :: String}

data FundEx2 = FundConEx2 {z3 :: Integer -> String, z4 :: String}


--add :: Int -> Int -> Int
--add x y = x + y

--func = Fund1 {}
