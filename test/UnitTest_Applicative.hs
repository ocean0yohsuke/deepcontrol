module Main where
import Test.HUnit

import DeepControl.Applicative

main :: IO ()
main = run >> return ()

-- | Run Unit-Test of this module. 
run = runTestTT $ TestList 
        [ TestList tLevel0
        , TestList tLevel1
        , TestList tLevel2
        , TestList tLevel3

        ]

----------------------------------------------------------------
-- unit test
----------------------------------------------------------------

tLevel0 = ("Level0" ~:) |$> [ 
      TestList $ ("|>" ~:) |$> [ 
          (1+) |> 2  ~?= 3
        , (1:) |> [] ~?= [1]
        , ("a"++) |> "b" ~?= "ab"
        , (True &&) |> False ~?= False
        , (True ||) |> False ~?= True
        ]

    , TestList $ ("<|" ~:) |$> [ 
          1 <| (+2)  ~?= 3
        , 1 <| (:[]) ~?= [1]
        , "a" <| (++"b")  ~?= "ab"
        , True <| (&& False) ~?= False
        , True <| (|| False) ~?= True
        ]

    , TestList $ ("<| and |>" ~:) |$> [ 
          1 <|(+)|> 2               ~?= 3
        , 1 <|(+)|> 2 <|(-)|> 3     ~?= 0
        , 1 <|(+)|> 2 <|(*)|> 3     ~?= 9

        , 1 <|(:)|> []             ~?= [1]
        , 1 <|(:)|> (2 <|(:)|> []) ~?= [1,2]

        , "a" <|(++)|> "b"              ~?= "ab"
        , "a" <|(++)|> "b" <|(++)|> "c" ~?= "abc"

        , True <|(&&)|> False ~?= False
        , True <|(||)|> False ~?= True

        , 1 <|(,)|> 2             ~?= (1,2)
        , 1 <|(,)|> 2 <|(,)|> 3   ~?= ((1,2),3)
        , 1 <|(,)|> (2 <|(,)|> 3) ~?= (1,(2,3))
        ]
    ]

tLevel1 = ("Level1" ~:) |$> [
      (+) |$> [1] |*> [2]      ~?= [3]

    , TestList $ ("|$>" ~:) |$> [ 
          (1+) |$> [2] ~?= [3]
        , (1:) |$> [[]] ~?= [[1]]
        , ("a"++) |$> ["b"] ~?= ["ab"]
        , (True &&) |$> [False] ~?= [False]
        , (True ||) |$> [False] ~?= [True]
    
        , (1+) |$> [0,1,2] ~?= [1,2,3]
        , (1+) |$> Just 0  ~?= Just 1
        , (1+) |$> Nothing ~?= Nothing
        , (1+) |$> Right 1  ~?= (Right 2 :: Either () Int)
        , (1+) |$> Left ()  ~?= (Left () :: Either () Int)
        ]
    , TestList $ ("|*>" ~:) |$> [ 
          [(1+)] |*> [0,1,2] ~?= [1,2,3]
        , Just (1+) |*> Just 0  ~?= Just 1
        , Just (1+) |*> Nothing ~?= Nothing
        , Right (1+) |*> Right 1 ~?= (Right 2 :: Either () Int)
        , Right (1+) |*> Left () ~?= (Left () :: Either () Int)

        ]
    , TestList $ ("<$|" ~:) |$> [ 
          [1] <$| (+2)  ~?= [3]

        , ("("++)|$> ["a","b"] <$|(++")") ~?= ["(a)", "(b)"]
        ]
    , TestList $ ("<$| and |*>" ~:) |$> [ 
          [1] <$|(+)|*> [0,1,2] ~?= [1,2,3]
        , Just 1 <$|(+)|*> Just 0  ~?= Just 1
        , Just 1 <$|(+)|*> Nothing ~?= Nothing
        , Right 1 <$|(+)|*> Right 1 ~?= (Right 2 :: Either () Int)
        , Right 1 <$|(+)|*> Left () ~?= (Left () :: Either () Int)

        , [1,11] <$|(+)|*> [0,1,2]              ~?= [1,2,3,11,12,13]
        , [0,1] <$|(+)|*> [2,3] <$|(^)|*> [4,5] ~?= [16,32,81,243,81,243,256,1024]

        , getZipList (ZipList ['a'..'e'] <$|(,)|*> ZipList [1..]) ~?= [('a',1),('b',2),('c',3),('d',4),('e',5)]

        ]
    , TestList $ ("<$|, |*> and higher-order-function" ~:) |$> [ 
          foldr (\n acc -> n <$|(+)|*> acc) ((*:) 0) [Just 1, Just 2,  Just 3] ~?= Just 6
        , foldr (\n acc -> n <$|(+)|*> acc) ((*:) 0) [Just 1, Nothing, Just 3] ~?= Nothing

        , foldr (\x acc -> x <$|(:)|*> acc) ((*:) []) [Just 1, Just 2,  Just 3] ~?= Just [1,2,3]
        , foldr (\x acc -> x <$|(:)|*> acc) ((*:) []) [Just 1, Nothing, Just 3] ~?= Nothing

        , filter ((10 >) <$|(&&)|*> even)                  [1..100] ~?= [2,4,6,8]
        , filter ((10 >) <$|(&&)|*> even <$|(&&)|*> (5 <)) [1..100] ~?= [6,8]
        ]
    , TestList $ ("|*, *|" ~:) |$> [ 
          [1] <$|(+)|*  2           ~?= [3]
        , 1    *|(+)|$> [2]         ~?= [3]

        , [(1+)] |* 2               ~?= [3]
        , 1 *| [(+2)]               ~?= [3]
        , 1 *| [(+)] |* 2           ~?= [3]

       , [1] <$|(+)|* 2 <$|(-)|* 3 ~?= [0]
       , 1 *|(+)|$> [2] <$|(-)|* 3 ~?= [0]

        , Just 1 <$|(,)|* 2  ~?= Just (1,2)
        , 1 *|Just (,)|* 2 ~?= Just (1,2)
        
        , 1 *|[(+),(-),(*),(^)]|* 2     ~?= [3,-1,2,1]
        ]

   , TestList $ ("other" ~:) |$> [ 
          [1] <$|(+)|> 2      ~?= [3]
        --, [2] <*| [1] <$| (+) ~?= [3]  -- invalid form
        , [2] <*|(+)|$> [1]   ~?= [3]
        ]
     ]

tLevel2 = ("Level2" ~:) |$> [
      (+) |$>> [Just 1] |*>> [Just 2]      ~?= [Just 3]

    , TestList $ ("<<$| and |*>>" ~:) |$> [ 
          [Just 1] <<$|(+)|*>> [Just 2]              ~?= [Just 3]
        , [[1]] <<$|(+)|*>> [[2]] <<$|(-)|*>> [[3]]  ~?= [[0]]

        , [Just 1] <<$|(,)|*>> [Just 2]              ~?= [Just (1,2)]
        ]
    , TestList $ ("<<$|, |*>> and higher-order-function" ~:) |$> [ 
          foldr (\n acc -> n <<$|(+)|*>> acc) ((**:) 0) ((Right . Just) |$> [1,2,3])               ~?= (Right (Just 6) :: Either () (Maybe Int))
        , foldr (\n acc -> n <<$|(+)|*>> acc) ((**:) 0) (Right |$> [Just 1,Nothing,Just 3])        ~?= (Right Nothing :: Either () (Maybe Int))
        , foldr (\n acc -> n <<$|(+)|*>> acc) ((**:) 0) ([Right (Just 1), Right Nothing, Left ()]) ~?= (Left () :: Either () (Maybe Int))
        ]
    , TestList $ ("|**, |*-, |-*, **|, *-|, -*|" ~:) |$> [ 
          [Just 1] <<$|(+)|** 2          ~?= [Just 3]
        , [Just 1] <<$|(+)|*- Just 2     ~?= [Just 3]
        , [Just 1] <<$|(+)|-* [2]        ~?= [Just 3]
        , 1      **|(+)|$>> [Just 2]     ~?= [Just 3]
        , Just 1 *-|(+)|$>> [Just 2]     ~?= [Just 3]
        , [1]    -*|(+)|$>> [Just 2]     ~?= [Just 3]
        , 1 **|[Just (+)]|** 2        ~?= [Just 3]
        , 1 **|[Just (+), Just (-), Just (*), Nothing]|** 2 ~?= [Just 3,Just (-1),Just 2,Nothing]
        , [0,1] -*|[Just (+), Just (-), Just (*), Nothing]|*- Just 2 ~?= [Just 2,Just 3,Just (-2),Just (-1),Just 0,Just 2,Nothing,Nothing]

        , [[1]] <<$|(+)|** 2 <<$|(-)|** 3    ~?= [[0]]
        , 1 **|(+)|$>> [[2]] <<$|(-)|** 3    ~?= [[0]]


        ]

    ]

tLevel3 = ("Level3" ~:) |$> [
      (+) |$>>> Right [Just 1] |*>>> Right [Just 2]      ~?= (Right [Just 3] :: Either () [Maybe Int])

    , TestList $ ("<<<$| and |*>>>" ~:) |$> [ 
          Right [Just 1] <<<$|(+)|*>>> Right [Just 2]         ~?= (Right [Just 3] :: Either () [Maybe Int])
        , [[[1]]] <<<$|(+)|*>>> [[[2]]] <<<$|(-)|*>>> [[[3]]] ~?= [[[0]]]

        , Right [Just 1] <<<$|(,)|*>>> Right [Just 2]       ~?= (Right [Just (1,2)] :: Either () [Maybe (Int,Int)])
        ]
    ]


