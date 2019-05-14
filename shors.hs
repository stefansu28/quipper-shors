module Shors where
import Quipper

plus_minus :: Bool -> Circ Qubit
plus_minus b = do
  q <- qinit b
  r <- hadamard q
  return r

-- | qft
-- transform a state to the fourier basis
qft :: [Qubit] -> Circ [Qubit]
qft [] = return []
qft [x] = do
  hadamard x
  return [x]

qft (x:xs) = do
  xs' <- qft xs
  xs'' <- rotations x xs' (length xs')
  x' <- hadamard x
  return (x':xs'')
  where
    rotations :: Qubit -> [Qubit] -> Int -> Circ [Qubit]
    rotations _ [] _ = return []
    rotations c (q:qs) n = do
      qs' <- rotations c qs n
      let m = ((n+1) - length qs)
      q' <- rGate m q `controlled` c
      return (q':qs')


qft_big_endian qs = do
  comment_with_label "Enter: qft" qs "qs"
  qs <- qft (reverse qs)
  comment_with_label "Exit: qft" qs "qs"
  return qs

inverse_qft = reverse_generic_endo qft_big_endian

-- construct the qubit representation for an integer mod 2^n
make_big_endian :: Int -> Int -> Circ [Qubit]
make_big_endian a n = do
  let xs = qinit (reverse (big_endian a n))
  xs
  where
    big_endian :: Int -> Int -> [Bool]
    big_endian a 0 = []
    big_endian a n = do
      let (q, r) = divMod a 2
      let x = (r == 1)
      let xs = big_endian q (n-1)
      (x:xs)

-- | const_add
-- "add" a state representing a number and a constant number mod 2^n
-- (x, 0) -> (x, x + c (mod 2^n))
-- xs - the state to be "added" onto (assumed to be in the fourier basis)
-- c - the built in number to add to the state
-- n - mod 2^n (just the length of xs)
const_add :: [Qubit] -> Int -> Circ [Qubit]
const_add [] c = return []
const_add xs c = do
  let (c', r) = divMod c 2
  (x':xs') <- rotations xs 1 r
  xs'' <- const_add xs' c'
  return (x':xs'')
  where
    rotations :: [Qubit] -> Int -> Int -> Circ [Qubit]
    rotations xs _ 0 = return xs
    rotations [] _ _ = return []
    rotations (x:xs) n 1 = do
      x' <- rGate n x
      xs' <- rotations xs (n+1) 1
      return (x':xs')

-- | qft_add
-- "add" two states together
-- (a, b) -> (a , a + b (mod 2^n))
-- as - the state that will be added to the other (computational basis big endian)
-- bs - the state to be "added" onto (fourier basis little endian)
-- n - mod 2^n (just the length of as and bs)
-- a + b output is in little endian
qft_add :: [Qubit] -> [Qubit] -> Circ [Qubit]
qft_add _ [] = return []
qft_add as (b:bs) = do
  b' <- rotations as b 1
  bs' <- qft_add (tail as) bs
  return (b': bs')
  where
    rotations :: [Qubit] -> Qubit -> Int -> Circ Qubit
    rotations [] b _ = return b
    rotations (a:as) b n = do
      b' <- rGate n b `controlled` a
      b'' <- rotations as b (n+1)
      return b''

add_big_endian as bs = do
  sum <- qft_add as (reverse bs)
  return (reverse sum)


-- | const_multiply
-- "multiply" a state representing a number and a constant number mod 2^n
-- (x, 0) -> (x, xc (mod 2^n))
-- xs - the state to be "multiplied" (in computational basis big endian)
-- c - the built in number to multiply the state by
-- n - number the qubits (should be the length of xs)
const_multiply :: [Qubit] -> Int -> Int -> Circ [Qubit]
const_multiply [] c n = return []
const_multiply xs c n = do
  acc <- make_big_endian 0 n
  label acc "acc"
  acc' <- qft_big_endian acc
  acc_multiply acc' xs c


acc_multiply :: [Qubit] -> [Qubit] -> Int -> Circ [Qubit]
acc_multiply _ [] _ = return []
acc_multiply acc xs c = do
  additions acc (reverse xs) c
  where
    additions :: [Qubit] -> [Qubit] -> Int -> Circ [Qubit]
    additions acc [] _ = return acc
    additions acc (x:xs) c = do
      -- let add = box ("A" ++ show c) const_add
      comment ("add " ++ show c)
      acc' <- const_add acc c `controlled` x
      additions acc' xs (c*2)


inv_acc_multiply :: [Qubit] -> [Qubit] -> Int -> Circ [Qubit]
inv_acc_multiply _ [] _ = return []
inv_acc_multiply acc xs c = do
  subs acc (reverse xs) c (length xs)
  where
    subs :: [Qubit] -> [Qubit] -> Int -> Int -> Circ [Qubit]
    subs acc [] _ _ = return acc
    subs acc (x:xs) c n = do
      -- let add = box ("A" ++ show c) const_add
      comment ("add -" ++ show c)
      acc' <- const_add acc (2^n - c) `controlled` x
      subs acc' xs (c*2) n

-- | powerA
-- (x, 1) -> (x, a^x (mod n))
-- xs - state encoded with the value to raise A to (in computational basis big endian)
-- a - the constant that will be raised to the power x
-- n - number of qubits of precision (should be the length of xs)
powerX :: [Qubit] -> Int -> Int -> Circ [Qubit]
powerX xs a n = do
  prod <- make_big_endian 1 n
  acc <- make_big_endian 0 n
  label prod "prod"
  acc_powerX acc prod xs a


acc_powerX acc prod xs a = do
  products acc prod (reverse xs) a
  where
    products :: [Qubit] -> [Qubit] -> [Qubit] -> Int -> Circ [Qubit]
    products acc prod [] _ = return prod
    products acc prod (x:xs) a = do
      acc' <- qft_big_endian acc `controlled` x
      acc' <- acc_multiply acc' (prod) a `controlled` x
      acc' <- inverse_qft (acc') `controlled` x

      swap acc' prod `controlled` x
      
      acc' <- qft_big_endian acc `controlled` x
      inv_acc_multiply acc' prod a `controlled` x
      acc' <- inverse_qft (acc') `controlled` x
      products acc' (prod) xs (a*a)
      return (prod)



phase_est :: ([Qubit] -> Circ [Qubit]) -> [Qubit] -> Int -> Circ [Qubit]
phase_est u phi n = do
  estimate <- make_big_endian 0 n
  mapUnary hadamard estimate
  loop estimate phi
  inverse_qft estimate
  where
    loop [] phi = return phi
    loop (x:xs) phi = do
      phi' <- loop xs phi
      u phi' `controlled` x

  

  
  
  
