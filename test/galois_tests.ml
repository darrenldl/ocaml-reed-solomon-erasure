open OUnit2
open Reed_solomon_erasure__.Galois
open Reed_solomon_erasure__.Ops
open Reed_solomon_erasure__.Tables

let backblaze_log_table =
  (String.concat ""
     ["\000\000\001\025\002\050\026\198";
      "\003\223\051\238\027\104\199\075";
      "\004\100\224\014\052\141\239\129";
      "\028\193\105\248\200\008\076\113";
      "\005\138\101\047\225\036\015\033";
      "\053\147\142\218\240\018\130\069";
      "\029\181\194\125\106\039\249\185";
      "\201\154\009\120\077\228\114\166";
      "\006\191\139\098\102\221\048\253";
      "\226\152\037\179\016\145\034\136";
      "\054\208\148\206\143\150\219\189";
      "\241\210\019\092\131\056\070\064";
      "\030\066\182\163\195\072\126\110";
      "\107\058\040\084\250\133\186\061";
      "\202\094\155\159\010\021\121\043";
      "\078\212\229\172\115\243\167\087";
      "\007\112\192\247\140\128\099\013";
      "\103\074\222\237\049\197\254\024";
      "\227\165\153\119\038\184\180\124";
      "\017\068\146\217\035\032\137\046";
      "\055\063\209\091\149\188\207\205";
      "\144\135\151\178\220\252\190\097";
      "\242\086\211\171\020\042\093\158";
      "\132\060\057\083\071\109\065\162";
      "\031\045\067\216\183\123\164\118";
      "\196\023\073\236\127\012\111\246";
      "\108\161\059\082\041\157\085\170";
      "\251\096\134\177\187\204\062\090";
      "\203\089\095\176\156\169\160\081";
      "\011\245\022\235\122\117\044\215";
      "\079\174\213\233\230\231\173\232";
      "\116\214\244\234\168\080\088\175"])

let log_table_same_as_backblaze test_ctxt =
  for i = 0 to (256) - 1 do
    assert_equal log_table.%(i) backblaze_log_table.%(i)
  done

let test_associativity test_ctxt =
  for a = 0 to (256) - 1 do
    let a = char_of_int a in
    for b = 0 to (256) - 1 do
      let b = char_of_int b in
      for c = 0 to (256) - 1 do
        let c = char_of_int c in
        let x = add a (add b c) in
        let y = add (add b c) a in
        assert_equal x y;
        let x = mul a (mul b c) in
        let y = mul (mul a b) c in
        assert_equal x y;
      done
    done
  done

let qc_add_associativity =
  QCheck_runner.to_ounit2_test
    (QCheck.Test.make ~count:10000 ~name:"qc_add_associativity"
       QCheck.(triple char char char)
       (fun (a,b,c) -> add a (add b c) = add (add a b) c))

let qc_mul_associativity =
  QCheck_runner.to_ounit2_test
    (QCheck.Test.make ~count:10000 ~name:"qc_add_associativity"
       QCheck.(triple char char char)
       (fun (a,b,c) -> mul a (mul b c) = mul (mul a b) c))

let suite =
  "galois_tests">:::
  ["log_table_same_as_backblaze">:: log_table_same_as_backblaze;
   "test_associativity">::          test_associativity;
   qc_add_associativity;
   qc_mul_associativity;
  ]
