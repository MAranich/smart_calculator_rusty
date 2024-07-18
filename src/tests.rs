use crate::*;
use core::panic;
use std::f64::consts::PI;
use std::rc::Rc;
use std::vec;

use self::setup::*;
use crate::processing::*;

#[test]
fn bs_test() {
    let v: Vec<char> = vec!['0', '1'];
    let x: char = '1';
    let r: Result<usize, usize> = v.binary_search(&x);
    assert!(match r {
        Ok(i) => i == 1,
        _ => false,
    });
}

#[test]
fn bs_test_2() {
    let v: Vec<char> = vec!['0', '1'];
    let x: char = '1';

    let _ = advance_replica(v, x);
}

fn advance_replica(alph: Vec<char>, s: char) -> usize {
    let index: usize;
    println!("Alphabet: {:#?}", alph);
    println!("Symbol: {:#?}", s);
    match alph.binary_search(&s) {
        Ok(v) => index = v,
        Err(_) => {
            panic!("Symbol |{}| not in alphabet. ", s)
        }
    }
    return index;
}
#[test]
fn dfa_analyze_1() {
    assert!(basic_functionality_dfa(String::from("0001")));
    assert!(basic_functionality_dfa(String::from("111")));
}

#[test]
fn dfa_analyze_2() {
    assert!(!basic_functionality_dfa(String::from("1101110")));
    assert!(!basic_functionality_dfa(String::from("")));
}

fn basic_functionality_dfa(input: String) -> bool {
    let mut test_dfa: DFA = DFA::new(2, vec!['0', '1'], 2, vec![2]);
    test_dfa.sort_alphabet_and_map();
    let tt: Vec<Vec<u16>> = vec![vec![1, 2], vec![1, 2]];
    let res: Result<(), &str> = test_dfa.set_full_transition_table(tt, true);
    match res {
        Err(e) => {
            panic!("{}", e);
        }
        Ok(_) => {}
    }

    println!("DFA: {:?}", test_dfa);

    let dfa_rc: Rc<DFA> = Rc::new(test_dfa);

    let mut idfa: InstanceDFA = InstanceDFA::new(dfa_rc.clone(), None);

    let res2: Result<bool, String> = idfa.analyze(input);

    match res2 {
        Err(e) => panic!("{}", e),
        Ok(accepted) => {
            return accepted;
        }
    }
}

#[test]
fn test_identifier_1() {
    assert!(dfa_identifiers("myidentifier".to_owned()));
    assert!(dfa_identifiers("hello_64788".to_owned()));
    assert!(dfa_identifiers("H78AHBdf".to_owned()));
    assert!(dfa_identifiers("H78AHBdf1253".to_owned()));
    assert!(dfa_identifiers("_aedg".to_owned()));
    assert!(dfa_identifiers(
        "asdfghjklqwertyuiopzxcvbnmASDFGHJKLQWERTYUIOPZXCVBNM".to_owned()
    ));
}

#[test]
fn test_identifier_2() {
    assert!(!dfa_identifiers("".to_owned()));
    assert!(!dfa_identifiers("1hfydkhgf".to_owned()));
    assert!(!dfa_identifiers("0asf".to_owned()));
}

fn dfa_identifiers(input: String) -> bool {
    /*
    q1 -(majus, minus, _)-> q2 - (majus, minus, _, num)-> q2
    else go q0

    */

    let mut alphabet: Vec<char> = vec![
        'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r',
        's', 't', 'u', 'v', 'w', 'x', 'y', 'z',
    ];

    let alph_mayus: Vec<char> = alphabet
        .iter()
        .map(|&x| x.to_uppercase().next().unwrap())
        .collect();
    alphabet = [alphabet, alph_mayus].concat();
    alphabet = [
        vec!['0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '_'],
        alphabet,
    ]
    .concat();
    let len: usize = alphabet.len();
    let mut test_dfa: DFA = DFA::new(2, alphabet, 2 as u16, vec![2]);

    let map_vector: Vec<u16> = [vec![0; 10], vec![1; len - 10]].concat();

    test_dfa
        .full_alphabet_map(map_vector)
        .expect("Problems with mapping vector. ");

    test_dfa.sort_alphabet_and_map();
    println!("DFA: {:?}", test_dfa);
    let tt: Vec<Vec<u16>> = vec![vec![0, 2], vec![2, 2]];
    let res: Result<(), &str> = test_dfa.set_full_transition_table(tt, true);
    match res {
        Err(e) => {
            panic!("{}", e);
        }
        Ok(_) => {}
    }

    let dfa_rc: Rc<DFA> = Rc::new(test_dfa);

    let mut idfa: InstanceDFA = InstanceDFA::new(dfa_rc.clone(), None);

    println!("Alphabet: {:#?}", idfa.reference.alphabet);

    let res2: Result<bool, String> = idfa.analyze(input);

    match res2 {
        Err(e) => panic!("{}", e),
        Ok(accepted) => {
            return accepted;
        }
    }
}

fn dfa_numbers(input: String) -> bool {
    /*
    q1 -(-)-> q2 -(num)-> q3
    q1 -(num)-> q3 -(.)-> q4 -> q5
    else go q0
    */

    let alphabet: Vec<char> = vec!['-', '.', '0', '1', '2', '3', '4', '5', '6', '7', '8', '9'];
    let mapping_vector: Vec<u16> = vec![2, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0];

    //let len: usize = alphabet.len();
    let mut test_dfa: DFA = DFA::new(5, alphabet, 3 as u16, vec![3, 5]);

    test_dfa
        .full_alphabet_map(mapping_vector)
        .expect("Problems with mapping vector. ");

    test_dfa.sort_alphabet_and_map();
    let tt: Vec<Vec<u16>> = vec![
        vec![3, 0, 2],
        vec![3, 0, 0],
        vec![3, 4, 0],
        vec![5, 0, 5],
        vec![5, 0, 0],
    ];
    let res: Result<(), &str> = test_dfa.set_full_transition_table(tt, true);
    match res {
        Err(e) => {
            panic!("{}", e);
        }
        Ok(_) => {}
    }
    println!("{:?}", test_dfa);

    let dfa_rc: Rc<DFA> = Rc::new(test_dfa);

    let mut idfa: InstanceDFA = InstanceDFA::new(dfa_rc.clone(), None);

    //println!("Alphabet: {:#?}", idfa.reference.alphabet);

    let res2: Result<bool, String> = idfa.analyze(input);

    match res2 {
        Err(e) => panic!("{}", e),
        Ok(accepted) => {
            return accepted;
        }
    }
}

#[test]
fn test_numbers_1() {
    assert!(dfa_numbers("05218".to_owned()));
    assert!(dfa_numbers("3.1415".to_owned()));
    assert!(dfa_numbers("12.3".to_owned()));
    assert!(dfa_numbers("000.123".to_owned()));
    assert!(dfa_numbers("0000000000000000000".to_owned()));
    assert!(dfa_numbers("12315468977563159".to_owned()));
    assert!(dfa_numbers("-3".to_owned()));
    assert!(dfa_numbers("-3.2".to_owned()));
}

#[test]
fn test_numbers_2() {
    assert!(!dfa_numbers("".to_owned()));
    assert!(!dfa_numbers("15.056.123".to_owned()));
    assert!(!dfa_numbers(".123".to_owned()));
    assert!(!dfa_numbers("123.".to_owned()));
    assert!(!dfa_numbers("-.2".to_owned()));
}

#[test]
fn sorting_1() {
    let mut iter: std::iter::Rev<std::ops::Range<i32>> = (0..(20 / 2)).rev();
    println!("iter1: {:?}", iter);
    iter = (1..20).rev();
    println!("iter2: {:?}", iter);
    //for i in iter { print!("{i} "); }

    let mut alphabet = vec!['0', '1'];
    let mut test_dfa: DFA = DFA::new(alphabet.len() as u16, alphabet.clone(), 2, vec![2]);
    test_dfa.sort_alphabet_and_map();

    alphabet.sort();

    assert_eq!(alphabet, test_dfa.alphabet);
}

#[test]
fn sorting_2() {
    let mut alphabet: Vec<char> = vec!['3', '8', '7', '4', '0', '2', '6', '1', '5', '9'];
    let mut test_dfa: DFA = DFA::new(alphabet.len() as u16, alphabet.clone(), 2, vec![2]);
    test_dfa.sort_alphabet_and_map();

    alphabet.sort();

    assert_eq!(alphabet, test_dfa.alphabet);
}

fn dfa_operators(input: String) -> bool {
    /*
    Only accepts a single operator
    */

    let alphabet: Vec<char> = vec!['+', '-', '*', '/', '^', '!', '%'];
    //addition, substraction, multiplication, division, exponentiation, factoria, modulo

    let len: usize = alphabet.len();
    let mut test_dfa: DFA = DFA::new(2, alphabet, 1 as u16, vec![2]);

    let mapping_vector: Vec<u16> = vec![0; len];

    test_dfa
        .full_alphabet_map(mapping_vector)
        .expect("Problems with mapping vector. ");

    test_dfa.sort_alphabet_and_map();
    println!("DFA: {:?}", test_dfa);
    let tt: Vec<Vec<u16>> = vec![vec![2], vec![0]];
    let res: Result<(), &str> = test_dfa.set_full_transition_table(tt, true);
    match res {
        Err(e) => {
            panic!("{}", e);
        }
        Ok(_) => {}
    }

    let dfa_rc: Rc<DFA> = Rc::new(test_dfa);

    let mut idfa: InstanceDFA = InstanceDFA::new(dfa_rc.clone(), None);

    println!("Alphabet: {:#?}", idfa.reference.alphabet);

    let res2: Result<bool, String> = idfa.analyze(input);

    match res2 {
        Err(e) => panic!("{}", e),
        Ok(accepted) => {
            return accepted;
        }
    }
}

#[test]
fn test_operators_1() {
    assert!(dfa_operators("+".to_owned()));
    assert!(dfa_operators("-".to_owned()));
    assert!(dfa_operators("*".to_owned()));
    assert!(dfa_operators("/".to_owned()));
    assert!(dfa_operators("^".to_owned()));
    assert!(dfa_operators("!".to_owned()));
    assert!(dfa_operators("%".to_owned()));
}

#[test]
fn test_operators_2() {
    assert!(!dfa_operators("".to_owned()));
    assert!(!dfa_operators("++".to_owned()));
    assert!(!dfa_operators("*/".to_owned()));
}

fn dfa_special_chars(input: String) -> bool {
    /*
    Only accepts a single special char
    */

    let alphabet: Vec<char> = vec!['(', ')', '{', '}', '[', ']'];
    let len: usize = alphabet.len();
    let mut test_dfa: DFA = DFA::new(2, alphabet, 1 as u16, vec![2]);

    let mapping_vector: Vec<u16> = vec![0; len];

    test_dfa
        .full_alphabet_map(mapping_vector)
        .expect("Problems with mapping vector. ");

    test_dfa.sort_alphabet_and_map();
    println!("DFA: {:?}", test_dfa);
    let tt: Vec<Vec<u16>> = vec![vec![2], vec![0]];
    let res: Result<(), &str> = test_dfa.set_full_transition_table(tt, true);
    match res {
        Err(e) => {
            panic!("{}", e);
        }
        Ok(_) => {}
    }

    let dfa_rc: Rc<DFA> = Rc::new(test_dfa);

    let mut idfa: InstanceDFA = InstanceDFA::new(dfa_rc.clone(), None);

    println!("Alphabet: {:#?}", idfa.reference.alphabet);

    let res2: Result<bool, String> = idfa.analyze(input);

    match res2 {
        Err(e) => panic!("{}", e),
        Ok(accepted) => {
            return accepted;
        }
    }
}

#[test]
fn dfa_special_char_1() {
    assert!(dfa_special_chars("(".to_owned()));
    assert!(dfa_special_chars(")".to_owned()));
    assert!(dfa_special_chars("{".to_owned()));
    assert!(dfa_special_chars("}".to_owned()));
    assert!(dfa_special_chars("[".to_owned()));
    assert!(dfa_special_chars("]".to_owned()));
}

#[test]
fn dfa_special_char_2() {
    assert!(!dfa_special_chars("".to_owned()));
    assert!(!dfa_special_chars("()".to_owned()));
    assert!(!dfa_special_chars("{{".to_owned()));
    assert!(!dfa_special_chars("(}".to_owned()));
}

#[test]
fn parenthesis_correct_format_detecter() {
    let dfas: Vec<Rc<DFA>> = setup::setup_dfas();

    let associated_class: Vec<Option<TokenClass>> = vec![
        Some(datastructures::TokenClass::Number),
        Some(datastructures::TokenClass::Operator),
        Some(datastructures::TokenClass::SpecialChar),
        Some(datastructures::TokenClass::Identifier),
    ];

    let mut idfas: Vec<InstanceDFA> = setup::setup_idfas(&dfas, associated_class);

    let input = String::from("2+(4) - {sing[23 (23.5/15 * pi)]}");
    assert!(correct_parenthesis(&tokenize_input(&mut idfas, &input)));

    let input = String::from("(73/{func(sllms[2]) + sqrt(2) / 2} ^ [-1.2562])");
    assert!(correct_parenthesis(&tokenize_input(&mut idfas, &input)));

    let input = String::from("2^{marfg^[12^(-fllxx[223])]}");
    assert!(correct_parenthesis(&tokenize_input(&mut idfas, &input)));

    let input = String::from("({[()(())]})");
    assert!(correct_parenthesis(&tokenize_input(&mut idfas, &input)));

    let input = String::from("(]");
    assert!(!correct_parenthesis(&tokenize_input(&mut idfas, &input)));

    let input = String::from("[}");
    assert!(!correct_parenthesis(&tokenize_input(&mut idfas, &input)));

    let input = String::from("{)");
    assert!(!correct_parenthesis(&tokenize_input(&mut idfas, &input)));
}

#[test]
fn rationalizer_1() {
    rationalize_test(25, 7);
    rationalize_test(3, 7);
    rationalize_test(9, 44);
    rationalize_test(1570, 1551);
    rationalize_test(285, 6);
    rationalize_test(9999, 121);
    rationalize_test(540, 210861);
    //rationalize_test(56246455, 158697452);
    //rationalize_test(5401186054868, 1786541032);
}

fn rationalize_test(mut num: i64, mut den: u64) {
    let gcd = Number::euclidean_algorithm(num as u128, den as u128) as i64;
    num = num / gcd;
    den = den / gcd as u64;
    let x = (num as f64) / (den as f64);
    println!("Num: {} ", x);
    let res = Number::rationalize(x);
    assert_eq!(res.unwrap(), Number::Rational(num, den));
}

#[test]
fn rationalize_zero() {
    let x_1 = 0;
    let x_2 = 1;
    let x = (x_1 as f64) / (x_2 as f64);
    println!("Num: {} ", x);
    let res = Number::rationalize(x);
    assert_eq!(res.unwrap(), Number::Rational(x_1, x_2));
}

#[test]
fn rationalizer_parameter_test() {
    //let n = 56246455; //12033967895
    //let d = 158697452; //33953429463
    //let n = 6457802145; //(3495626569640639, 8589934592)
    //let d = 15869;
    //let n = 49822563; //(1373916799525553, 56641508110)
    //let d = 2054;
    //let n = 21534; //correct
    //let d = 9764614;
    //let n = 84632; //(844602545, 851232513307)
    //let d = 85296345;
    //let n = 48621526; //16t;
    //let d = 17 * 29 * 51 * 7 * 67 + 2;
    let n = 1; //16t;
    let d = 40;

    let tolerance = 0.00000000001; //0.00000001 // 0.00000000001 //0.00000000001
                                   //0.000000001
    let terms = 30; //[15, 40]
                    //26 terms

    let by_terms = true;

    if by_terms {
        for i in 10..35 {
            println!("terms: {}\t tolerance: {}", i, tolerance);
            rationalizer_parameter_test_wrapper(n, d, i, tolerance);
        }
    } else {
        for i in 5..16 {
            let t = (10 as f64).powf(-(i as f64));
            println!("terms: {}\t tolerance: {}", terms, t);
            rationalizer_parameter_test_wrapper(n, d, terms, t);
        }
    }

    //panic!();
}

fn rationalizer_parameter_test_wrapper(
    mut num: i64,
    mut den: u64,
    max_n_terms: u32,
    tolerance: f64,
) {
    let gcd = Number::euclidean_algorithm(num as u128, den as u128) as i64;
    num = num / gcd;
    den = den / gcd as u64;
    let x = (num as f64) / (den as f64);
    //println!("Num: {} ", x);
    let res = rationalize_testing(x, max_n_terms, tolerance);
    println!(
        "{}/{} = {} \t=> {:?} \nAbs Err = {} \tRel Err = {} \tFound_0 = {}\n\n",
        num, den, x, res.0, res.1, res.2, res.3
    );
}

pub fn rationalize_testing(
    mut x: f64,
    max_n_terms: u32,
    tolerance: f64,
) -> ((i64, i64), f64, f64, bool) {
    let print_debug = false;

    if x == 0.0 {
        panic!("No test for 0");
        //return Ok(NumberType::Rational(0, 1));
    }
    let is_neg = if x < 0.0 {
        x = -x;
        true
    } else {
        false
    };

    let mut n_vals: (f64, f64) = (x, 1.0);
    let mut sequence: Vec<i64> = Vec::new();
    //let max_n_terms: u32 = 24;
    let mut found_0 = false;

    for i in 2..max_n_terms {
        let new_term = (n_vals.0 / n_vals.1) as i64;
        sequence.push(new_term);

        let new_n = n_vals.0 % n_vals.1;
        if new_n <= tolerance {
            found_0 = true;
            break;
        }
        n_vals = (n_vals.1, new_n);
        if print_debug {
            println!("N_val[{}] = {}", i, n_vals.1);
        }
    }

    let mut rational: (i64, i64) = (sequence.pop().unwrap(), 1);
    sequence.reverse();
    for term in sequence {
        rational = (rational.1, rational.0); //inverse
        rational.0 = rational.0 + rational.1 * term;
        if print_debug {
            println!("Rational: {:?}", rational);
        }
    }

    let gcd = Number::euclidean_algorithm(rational.0 as u128, rational.1 as u128) as i64;
    if gcd != 1 {
        println!("GCD NOT EQUAL TO 1!");
    }

    rational = (rational.0 / gcd, rational.1 / gcd);

    if is_neg {
        rational.0 = -rational.0;
    }
    let aprox = rational.0 as f64 / rational.1 as f64;
    let abs_err = (aprox - x).abs();
    let rel_err = abs_err / x;

    return (rational, abs_err, rel_err, found_0);
}

#[test]
#[should_panic]
fn test_rules_1() {
    let rules: Vec<Rule> = setup::get_rules();
    let rule: Rule = rules[0].clone();

    let stack: Vec<Token> = vec![Token {
        lexeme: None,
        class: TokenClass::NTStart,
    }];

    assert!(rule.follows_rule(stack));
}

#[test]
fn temp_test() {
    //for fast testing
    let mut calc: Calculator = Calculator::new(setup::setup_dfas(), SRA::new(setup::get_rules()));
    let (inp, out_err): (String, String) = (
        String::from("arcsin(1.1)"),
        String::from("The domain of arcsin() is [-1, 1]."),
    );

    println!("\n\n\tTesting \"{}\": ", inp);
    let result: Result<Number, String> = evaluate_expression(inp.clone(), &mut calc, false);
    match result.clone() {
        Ok(v) => {
            panic!("\n\n\nInput: \"{}\" should give an error but it did not. Expected error: {}\nResult: {:?}", inp, out_err, v);
        }
        Err(msg) => {
            if !msg.contains(&out_err) {
                println!("\n\n\n\nError source: \n\n{:#?}", 0);
                panic!("\n\nInput: \"{}\" failed but for the wrong reason. \nExpected error: \n\t{}\nRecived error: \n\t{}\n", inp, out_err, msg);
            }
        }
    }
}

#[test]
fn global_fail_test() {
    let mut calc: Calculator = Calculator::new(setup::setup_dfas(), SRA::new(setup::get_rules()));
    let input: Vec<(String, String)> = vec![
        //basic syntax rules
        (String::from("1.2+"), String::from("INVALID parsing!")),
        (String::from("1.2-"), String::from("INVALID parsing!")),
        (
            String::from("145.34.2"),
            String::from("Token class is undefined (None)."),
        ),
        (
            String::from("145.36."),
            String::from("Token class is undefined (None)."),
        ),
        (
            String::from("NonExistentFunction(2)"),
            String::from("Function not found."),
        ),
        //outside domain evaluation
        (
            String::from("sqrt(0-1)"),
            String::from("The input of sqrt cannot be negative. Input provided: "),
        ),
        (
            String::from("sqrt(-1)"),
            String::from("The input of sqrt cannot be negative. Input provided: "),
        ),
        (
            String::from("sqrt(-3588)"),
            String::from("The input of sqrt cannot be negative. Input provided: "),
        ),
        (
            String::from("sqrt(ln(0.25))"),
            String::from("The input of sqrt cannot be negative. Input provided: "),
        ),
        (
            String::from("tan(PI/2)"),
            String::from("The domain of tan(x) does not include values in the form x"),
        ),
        (
            String::from("tan(PI/2 + PI*55)"),
            String::from("The domain of tan(x) does not include values in the form x"),
        ),
        (
            String::from("arcsin(1.1)"),
            String::from("The domain of arcsin() is [-1, 1]."),
        ),
        (
            String::from("arcsin(-1.568)"),
            String::from("The domain of arcsin() is [-1, 1]."),
        ),
        (
            String::from("arccos(-2)"),
            String::from("The domain of arccos() is [-1, 1]."),
        ),
        (
            String::from("arccos(2)"),
            String::from("The domain of arccos() is [-1, 1]."),
        ),
        (
            String::from("arccos(exp(1))"),
            String::from("The domain of arccos() is [-1, 1]."),
        ),
        (
            String::from("ln(0)"),
            String::from("The domain of ln() is the positive reals excluding 0."),
        ),
        (
            String::from("ln(-1)"),
            String::from("The domain of ln() is the positive reals excluding 0."),
        ),
        //division by 0
        (
            String::from("0/0"),
            String::from("Division by 0 is not possible."),
        ),
        (
            String::from("5/0"),
            String::from("Division by 0 is not possible."),
        ),
        (
            String::from("(-1+57^(22*0))/sin(0)"),
            String::from("Division by 0 is not possible."),
        ),
        (
            String::from("sin(32-sqrt(-1*ln(1/3)))/-(22/11*-(-0.5)+cos(PI))"),
            String::from("Division by 0 is not possible."),
        ),
        //expoentiation with negative base and fractionary exponent
        (
            String::from("(-0.333)**(-0.5)"),
            String::from("Cannot raise negative number to a fractioary exponent."),
        ),
        (
            String::from("(-0.333)**-0.5"),
            String::from("Cannot raise negative number to a fractioary exponent."),
        ),
        (
            String::from("-0.333**-0.5"),
            String::from("Cannot raise negative number to a fractioary exponent."),
        ),
        (
            String::from("-0.512**-1.52545"),
            String::from("Cannot raise negative number to a fractioary exponent."),
        ),
        (
            String::from("-2.3896548**2.1458"),
            String::from("Cannot raise negative number to a fractioary exponent."),
        ),
    ];

    for (inp, out_err) in input {
        println!("\n\n\tTesting \"{}\": ", inp);
        let result: Result<Number, String> = evaluate_expression(inp.clone(), &mut calc, false);
        match result {
            Ok(v) => {
                panic!("\nInput: \"{}\" should give an error but it did not. Expected error: {}\nResult: {:?}", inp, out_err, v);
            }
            Err(msg) => {
                if !msg.contains(&out_err) {
                    panic!("\nInput: \"{}\" failed but for the wrong reason. \nExpected error: \n\t{}\nRecived error: \n\t{}\n", inp, out_err, msg);
                }
            }
        }
    }
}

#[test]
fn global_test() {
    /*
       Test for the full aplication
    */

    let mut calc: Calculator = Calculator::new(setup::setup_dfas(), SRA::new(setup::get_rules()));
    let input: Vec<(String, Number)> = vec![
        (
            String::from("2+2"),
            Number::new_rational(4, 1).expect("Non zero div rational"),
        ),
        (
            String::from("1.2+3"),
            Number::new_rational(21, 5).expect("Non zero div rational"),
        ),
        (
            String::from("1+3.4"),
            Number::new_rational(22, 5).expect("Non zero div rational"),
        ),
        (
            String::from("2.2+3.9"),
            Number::new_rational(61, 10).expect("Non zero div rational"),
        ),
        (
            String::from("2-2"),
            Number::new_rational(0, 1).expect("Non zero div rational"),
        ),
        (
            String::from("1.2-3"),
            Number::new_rational(-9, 5).expect("Non zero div rational"),
        ),
        (
            String::from("1-3.4"),
            Number::new_rational(-12, 5).expect("Non zero div rational"),
        ),
        (
            String::from("2.2-3.9"),
            Number::new_rational(-17, 10).expect("Non zero div rational"),
        ),
        (
            String::from("1.223"),
            Number::new_rational(1223, 1000).expect("Non zero div rational"),
        ),
        (
            String::from("-2"),
            Number::new_rational(-2, 1).expect("Non zero div rational"),
        ),
        (
            String::from("4.2**3"),
            Number::new_rational(9261, 125).expect("Non zero div rational"),
        ),
        (
            String::from("1.2-3"),
            Number::new_rational(-9, 5).expect("Non zero div rational"),
        ),
        (
            String::from("-3"),
            Number::new_rational(-3, 1).expect("Non zero div rational"),
        ),
        (
            String::from("-2 * 3"),
            Number::new_rational(-6, 1).expect("Non zero div rational"),
        ),
        (
            String::from("-(2 * 3)"),
            Number::new_rational(-6, 1).expect("Non zero div rational"),
        ),
        (
            String::from("sin(-12)"),
            Number::new_real(0.5365729180004349),
        ),
        (
            String::from("1.3/sin(-12)"),
            Number::new_real(2.42278347711717),
        ),
        (
            String::from("355/-113"),
            Number::new_rational(-355, 113).expect("Non zero div rational"),
        ),
        (
            String::from("280/-cos(1+sqrt(2))"),
            Number::new_real(374.87298894779871047),
        ),
        (
            String::from("8/-11*2"),
            Number::new_rational(-16, 11).expect("Non zero div rational"),
        ),
        (
            String::from("(19%10)%7"),
            Number::new_rational(2, 1).expect("Non zero div rational"),
        ),
        (
            String::from("arctan(exp(arcsin(0.5) + 1) + 2) * sqrt(223)"),
            Number::new_real(21.2076769605266),
        ),
        (
            String::from("(-15)/13 * 2+5^2"),
            Number::new_rational(295, 13).expect("Non zero div rational"),
        ),
        (
            String::from("355/113 - 152 * 0.05**(1/3)"),
            Number::new_real(-52.855685858979896),
        ),
        (
            String::from("2**3**2"),
            Number::new_rational(512, 1).expect("Non zero div rational"),
        ),
        (
            String::from("3**2**3"),
            Number::new_rational(6561, 1).expect("Non zero div rational"),
        ),
        (
            String::from("3**3**3"),
            Number::new_rational(7625597484987, 1).expect("Non zero div rational"),
        ),
        (
            String::from("1.1**1.2**1.3**1.4"),
            Number::new_real(1.132029796302015),
        ),
        (
            String::from("32%5"),
            Number::new_rational(2, 1).expect("Non zero div rational"),
        ),
        //(String::from("253^(2^16 + 1) % 17*29"), Number::new_rational(338, 1)),
        (
            String::from("cos(tan(-2) *-arctan(ln(arccos(sqrt(10)/10)**2 + 1)))"),
            Number::new_real(-0.07775507073215164758612901210),
        ),
        (
            String::from("sqrt(-1*-1)"),
            Number::new_rational(1, 1).expect("Non zero div rational"),
        ),
        (
            String::from("cos(-1)"),
            Number::new_real((1.0 as f64).cos()),
        ),
        (
            String::from("arcsin(0.9)"),
            Number::new_real((0.9 as f64).asin()),
        ),
        (
            String::from("ln(1+exp(sqrt(-2 * sin(-1.111))))"),
            Number::new_real(1.571595112146293),
        ),
        (
            String::from("0.8992**9 + 0.8992 + arctan(5.26) + 6"),
            Number::new_real(8.666456411),
        ),
        (String::from("2.3^0.5"), Number::new_real(1.51657508881031)),
        (
            String::from("2.3^-0.5"),
            Number::new_real(1 as f64 / 1.51657508881031 as f64),
        ),
        //created by chatgpt
        (
            String::from("(3+5)*2-4/2"),
            Number::new_rational(14, 1).expect("Non zero div rational"),
        ),
        (
            String::from("10%3+7*2-5"),
            Number::new_rational(10, 1).expect("Non zero div rational"),
        ),
        (
            String::from("(3 + 5) * (2 - 8)"),
            Number::new_rational(-48, 1).expect("Non zero div rational"),
        ),
        (
            String::from("10 / 2 + 3 * 4 - 6"),
            Number::new_rational(11, 1).expect("Non zero div rational"),
        ),
        (
            String::from("2^3 * 4^2 / (8 - 4) + 5"),
            Number::new_rational(37, 1).expect("Non zero div rational"),
        ),
        (
            String::from("sqrt(25) + ln(exp(2))"),
            Number::new_rational(7, 1).expect("Non zero div rational"),
        ),
        (
            String::from("arcsin(0.5) + arccos(0.5) - arctan(1)"),
            Number::new_real(0.785398163),
        ),
        (
            String::from("5! + 3^3 - 2^4"),
            Number::new_rational(131, 1).expect("Non zero div rational"),
        ),
        (
            String::from("2^(3 + 2) - 10 / 5 + 4"),
            Number::new_rational(34, 1).expect("Non zero div rational"),
        ),
        (
            String::from("7 % 3 + (2 * 3)"),
            Number::new_rational(7, 1).expect("Non zero div rational"),
        ),
        (
            String::from("7 % 3 + (2 * 3) - 4 / 2"),
            Number::new_rational(0, 1).expect("Non zero div rational"),
        ),
        (
            String::from("exp(ln(5)) * sqrt(16) - 9"),
            Number::new_rational(11, 1).expect("Non zero div rational"),
        ),
        (
            String::from("ln(exp(1) + 1)"),
            Number::new_real(1.3132616875182228),
        ),
        (
            String::from("sin(arccos(0.3) + arctan(0.4))"),
            Number::new_real(0.997127515943563688),
        ),
        (
            String::from("exp(2) + ln(7)"),
            Number::new_real(9.334966247986),
        ),
        (
            String::from("sqrt(ln(10))"),
            Number::new_real(1.5174271293851465),
        ),
        (
            String::from("10 % 4 + 3^2"),
            Number::new_rational(10, 1).expect("Non zero div rational"),
        ),
        (
            String::from("10 % 4 + 3^2 - 2 * 5"),
            Number::new_rational(1, 1).expect("Non zero div rational"),
        ),
        (
            String::from("ln(5^2 + 1)"),
            Number::new_real(3.258096538021482),
        ),
        (
            String::from("sin(30) + cos(60) - tan(45)"),
            Number::new_real(-3.5602197950518796326),
        ),
        (
            String::from("arccos(0.5) + arcsin(0.5) - arctan(0.5)"),
            Number::new_real(1.1071487177940906),
        ),
        (
            String::from("exp(sqrt(4))"),
            Number::new_real(7.38905609893065),
        ),
        (
            String::from("sin(pi/4) + cos(pi/4)"),
            Number::new_real((2.0 as f64).sqrt()),
        ),
        (
            String::from("exp(ln(20) / ln(10))"),
            Number::new_real(3.673077974307202),
        ),
        (
            String::from("sqrt(2^3 + 1)"),
            Number::new_rational(3, 1).expect("Non zero div rational"),
        ),
        (
            String::from("3! * 2! / 4!"),
            Number::new_rational(1, 2).expect("Non zero div rational"),
        ),
        (
            String::from("tan(arctan(1) + 1)"),
            Number::new_real(-4.588037824983901),
        ),
        (
            String::from("ln(10) + exp(1) - 5"),
            Number::new_real(0.020866921453091436),
        ),
        (
            String::from("exp(2) + exp(3)"),
            Number::new_real(27.47459302211832),
        ),
        (
            String::from("(8 / 2) * (3 + 1)"),
            Number::new_rational(16, 1).expect("Non zero div rational"),
        ),
        (
            String::from("4 * (3 + 2) - 7"),
            Number::new_rational(13, 1).expect("Non zero div rational"),
        ),
        (
            String::from("exp(ln(3 + 4))"),
            Number::new_rational(7, 1).expect("Non zero div rational"),
        ),
        (
            String::from("sin(arccos(0.2) + arctan(0.3))"),
            Number::new_real(0.9959438415202),
        ),
        (
            String::from("exp(ln(9)) + 2 * 5"),
            Number::new_rational(19, 1).expect("Non zero div rational"),
        ),
        (
            String::from("ln(exp(1) + 1)"),
            Number::new_real(1.3132616875182228),
        ),
        //constants
        (String::from("pi"), Number::new_real(PI)),
        (String::from("tau"), Number::new_real(2 as f64 * PI)),
        //
        (
            String::from("1/(1+exp(-2))"),
            Number::new_real(0.8807970779778823),
        ),
        (
            String::from("1/(1+exp(-(ln(arctan(22/sqrt(2)))*2-1)))"),
            Number::new_real(0.45505014868634275),
        ),
        (String::from("abs(-1)"), Number::Rational(1, 1)),
        (String::from("abs(2)"), Number::Rational(2, 1)),
        (String::from("abs(0)"), Number::Rational(0, 1)),
        (String::from("abs(0.5*PI)"), Number::new_real(PI * 0.5)),
        (String::from("abs(-0.5*PI)"), Number::new_real(PI * 0.5)),
    ];

    let float_err_tol: f64 = 0.000000001;
    /*
       Floating opint operations are not implemented equally on all sistems. The
       same operation may give diferent outputs in diferent computers.
       To account for that this value attemps to cancel this problem.
    */

    for (inp, out) in input {
        println!("\n\n\tTesting \"{}\": ", inp);
        match evaluate_expression(inp.clone(), &mut calc, false) {
            Ok(v) => {
                if !v.in_tolerance_range(&out, float_err_tol) {
                    panic!("\nInput: \"{}\" was not inside the tolearnce range ({}) and gave the result {:?} instead of the expected result {:?}", inp, float_err_tol, v, out);
                }
            }
            Err(msg) => panic!("\nInput: \"{}\" gave the error {}", inp, msg),
        }
    }
}
