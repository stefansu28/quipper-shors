import Shors
import Quipper
import QuipperLib.Simulation

qlist :: Integer -> [Qubit]
qlist 0 = []
qlist n = do
  let qs = qlist (n-1)
  (qubit: qs)



-- test add function
addc_test a b n = do
  as <- make_big_endian a n
  comment_with_label ("a = " ++ show a) as ("a" ++ show a)
  as' <- qft_big_endian as
  abs <- const_add as' b
  inverse_qft abs

add_test a b n = do
  as <- make_big_endian a n
  comment_with_label ("a = " ++ show a) as "a"
  bs <- make_big_endian b n
  comment_with_label ("b = " ++ show b) bs "b"

  bs' <- qft_big_endian bs
  sum <- add_big_endian as bs'
  inverse_qft sum

mult_test a b n = do
  as <- make_big_endian a n
  comment_with_label ("a = " ++ show a) as ("a" ++ show a)
  abs <- const_multiply as b n
  inverse_qft abs

power_test a b n = do
  as <- make_big_endian a n
  comment_with_label ("a = " ++ show a) as ("a" ++ show a)
  powerX as b n


phase_est_test a n = do
  as <- make_big_endian 1 n
  acc <- make_big_endian 0 n
  prod <- make_big_endian 1 n
  let u xs = do
        acc_powerX acc prod xs a
  phase_est u as n



-- main = print_generic Preview const_add (qlist 2) 2
-- main = print_generic Preview qft_add (qlist 3) (qlist 3)

-- main = print_generic Preview qft_big_endian (qlist 4)

-- main = do
--   let shape = (qlist 10)
--   print_generic Preview qft_big_endian shape

-- main = do
--   let (a, b, n) = (4, 3, 3)
--   print (show (sim_generic 1 (addc_test a b n)))
--   print_generic Preview (addc_test a b n)
  
-- main = do
--   let (a, b, n) = (4, 3, 3)
--   print_generic Preview (mult_test a b n)
--   print (show (sim_generic 1 (mult_test a b n)))

-- main = do
--   let (a, b, n)  = (4, 3, 3)
--   print_generic Preview (power_test a b n)
--   print (show (sim_generic 1 (power_test a b n)))

main = do
  let (a, n) = (3, 3)
  print (show (sim_generic 2 (phase_est_test a n)))
  print_generic Preview (phase_est_test a n)
