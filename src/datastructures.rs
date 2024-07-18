use core::fmt;
use std::{cell::RefCell, iter::zip, ops, rc::Rc, vec};

/// Represents the state that all [DFA] will start. 
const INITIAL_STATE: u16 = 1;

/// Represents the rejecting state in all [DFA]. 
const REJECTING_STATE: u16 = 0;

pub const ADD_STR: &'static str = "+";
pub const SUB_STR: &'static str = "-";
pub const MULT_STR: &'static str = "*";
pub const DIV_STR: &'static str = "/";
pub const EXP_STR: &'static str = "^";
pub const FACT_STR: &'static str = "!";
pub const MOD_STR: &'static str = "%";
pub const NEG_STR: &'static str = "--";

/// The tokens are the minimal expression with semantig meaning and the [TokenClass]
#[derive(Debug, PartialEq, Clone)]
pub enum TokenClass {
    ///The lexemme is just a number ("1.9232")
    Number,
    /// The lexemme is an operator ("\*")
    Operator,
    /// The lexemme is a special character ("(")
    SpecialChar,
    /// The lexemme is a function call or a constant ("sin", "PI")  
    Identifier,
    /// Used for [Rule] aplications. Non Terminal / Start
    NTStart,
    /// None of the previous
    None,
}
pub const NUM_DFA_CATEGORY_T: usize = 4;

/// A representation of a [Number]. 
/// 
/// It can be a rational number (can be expresed as a/b,
/// where b!=0 and a and b are whole numbers). This duality allows to perform some basic
/// operations between real numbers in a fast and exact way while retaining the versatility
/// of the eral numbers when the rationals are not enough.
#[derive(PartialEq, Clone)]
pub enum Number {
    /// A number that cannot be expressed as a/b
    Real(f64),
    /// A rational number.
    Rational(i64, u64),
}

/// The [AST] contains [Element] wich can be any of the following.
#[derive(Debug, PartialEq, Clone)]
pub enum Element {
    Function(String),
    Add,
    Sub,
    Mult,
    Div,
    Exp,
    Fact,
    Mod,
    Number(Number),
    Neg,
    None,
}

/// Deterministic Finite Automata [DFA]
/// 
/// A [DFA] is the basic structure to identify the given strings.
/// Go [here](https://en.wikipedia.org/wiki/Deterministic_finite_automaton) for more inforamtion.
#[derive(Debug, PartialEq)]
pub struct DFA {
    pub num_states: u16,
    pub alphabet: Vec<char>,
    /// In this case we also use an alphabet map that before provessing each character, it is maped to
    /// the corresponding value. This is uscefull because it avoids replication when multiple
    /// characters behave in the exact same way.
    pub alphabet_map: Vec<u16>,
    pub unique_symbols: u16,
    pub ending_states: Vec<u16>,
    pub transition_table: Vec<Vec<u16>>,
    /* Each entry in the tt represents all the transitions for a state. */
}

/// A concrete instance of a [DFA] 
/// 
/// that rontains information about the current state and if it's alive or not.
#[derive(Debug, PartialEq, Clone)]
pub struct InstanceDFA {
    pub reference: Rc<DFA>,
    pub state: u16,
    pub alive: bool,
    /// For processing pruposes we also aded an optional [TokenClass] wich the [InstanceDFA]
    /// can relate to. Therefore, if the string is accedpted it will be of the given [TokenClass].
    pub asociated_class: Option<TokenClass>,
}

/// A [Token] is the minimal lexical unit with sintctical meaning. 
/// 
/// It is the basic processing unit for most of the program.
#[derive(Debug, PartialEq, Clone)]
pub struct Token {
    /// An optional string of the literal text that the token is referring to.
    pub lexeme: Option<String>,
    /// The [TokenClass] that this token is asociated to.
    pub class: TokenClass,
}

/// A variant of [Token] explicitly designed to be compared or cloned.
/// 
/// It provides utility functions to simplify work while comparing and makes 
/// explicit the intention with that [Token].
#[derive(Debug, PartialEq, Clone)]
pub struct TokenModel {
    pub token: Token,
    pub compare_lexemme: bool,
}

/// A parsing [Rule]. 
#[derive(Debug, PartialEq, Clone)]
pub struct Rule {
    /// If these [Token] are found in that order,
    pub antecedent: Vec<TokenModel>,
    /// They can be converted to the following.
    pub consequent: TokenModel,
}

/// Shift Reduce Automata [SRA]. 
/// 
/// Reads [Token] and stores them in the stack. If a [Rule] can be used with the last n 
/// elements, it is aplied. While apliying a rule, it also creates an [AST] that can be 
/// used later.
#[derive(Debug, PartialEq)]
pub struct SRA {
    pub stack: Vec<Token>,
    pub rules: Vec<Rule>,
    pub ast: Vec<AST>,
}

/// Abstract Syntax Tree ([AST]) is a datastructure that encapsulates the
/// meaning of an expression using a tree structure. 
/// 
/// More details [here](https://en.wikipedia.org/wiki/Abstract_syntax_tree).
/// Each leaf contains a number and it's parent contains how to operate
/// between the children. This allows an unambiguous way to evaluate an expression.
#[derive(Debug, PartialEq, Clone)]
pub struct AST {
    pub value: Element,
    pub children: Vec<Rc<RefCell<AST>>>,
}

/// Wrapper that bundles together all the needed datastructures for a simpler
/// execution of the higher level functions.
pub struct Calculator {
    pub dfas: Vec<Rc<DFA>>,
    pub idfas: Vec<InstanceDFA>,
    pub parser: SRA,
}

/// A structure that can be evaluated. 
/// 
/// It returns a Number or a String containing an explanation of the error.
pub trait Evaluable {
    fn evaluate(&mut self) -> Result<Number, String>;
}

impl DFA {
    /// Create a new [DFA]. 
    /// 
    /// The transition table is set to everything going to the rejecting state (0).
    /// The alphabet matp is set to the identity.
    pub fn new(
        _num_states: u16,
        _alphabet: Vec<char>,
        _unique_symbols: u16,
        _ending_states: Vec<u16>,
    ) -> DFA {
        let len: usize = _alphabet.len();
        let mut base_map: Vec<u16> = Vec::with_capacity(len);
        for i in 0..len {
            base_map.push(i as u16 % _unique_symbols);
        }

        let _transition_table: Vec<Vec<u16>> =
            vec![vec![REJECTING_STATE; _unique_symbols as usize]; (_num_states + 1) as usize];

        let new_dfa: DFA = DFA {
            num_states: _num_states,
            alphabet: _alphabet,
            alphabet_map: base_map,
            unique_symbols: _unique_symbols,
            ending_states: _ending_states,
            transition_table: _transition_table,
        };

        return new_dfa;
    }

    /// Assigns a full transition table
    pub fn set_full_transition_table(
        &mut self,
        mut new_transition_table: Vec<Vec<u16>>,
        add_rejecting_row: bool,
    ) -> Result<(), &str> {
        let uniq: u16 = self.unique_symbols;

        if add_rejecting_row {
            new_transition_table.splice(0..0, vec![vec![0 as u16; uniq as usize]]);
        }

        let mut proper_matrix_dimensions: bool =
            new_transition_table.len() as u16 == self.num_states + 1;
        if !proper_matrix_dimensions {
            return Err("Not enough/too many rows for states. ");
        }

        proper_matrix_dimensions = new_transition_table.iter().all(|x| x.len() as u16 == uniq);
        if !proper_matrix_dimensions {
            return Err("Incorrect number of transitions for at least 1 state vector. ");
        }

        let n_states = self.num_states + 1;
        let all_values_valid: bool = new_transition_table.iter().flatten().all(|&x| x < n_states);
        if !all_values_valid {
            return Err("Exists invalid transition. ");
        }

        self.transition_table = new_transition_table;

        return Ok(());
    }

    /// Sets a single element of the transition table.
    pub fn set_transition_table(
        &mut self,
        state: u16,
        uniq_symbol: u16,
        new_state: u16,
    ) -> Result<(), &str> {
        //sets a single element
        match self.transition_table.get_mut(state as usize) {
            Some(row) => match row.get_mut(uniq_symbol as usize) {
                Some(v) => {
                    *v = new_state;
                }
                None => return Err("Invalid uniq_symbol. "),
            },
            None => return Err("State out of bounds. "),
        }

        return Ok(());
    }

    /// Set all of the alphabet map.
    pub fn full_alphabet_map(&mut self, new_map: Vec<u16>) -> Result<(), &str> {
        let len: u16 = self.alphabet.len() as u16;
        if len != new_map.len() as u16 {
            return Err("Invalid_length. ");
        }

        let inside_bounds: bool = new_map.iter().all(|&x| x < len);
        if !inside_bounds {
            return Err("Map points to invalid element. ");
        }

        self.alphabet_map = new_map;

        return Ok(());
    }

    ///Change the whole alphabet map
    pub fn change_map(&mut self, indexes: Vec<u16>, maps: Vec<u16>) -> Result<(), &str> {
        let len: u16 = self.alphabet.len() as u16;
        let all_indexes_valid: bool = indexes.iter().all(|&x| x < len);
        if !all_indexes_valid {
            return Err("Some index is outside of bounds. ");
        }

        let all_maps_are_valid = maps.iter().all(|&x| x < len);
        if !all_maps_are_valid {
            return Err("Some map is outside of bounds. ");
        }

        indexes
            .iter()
            .zip(maps.iter())
            .for_each(|(&i, &m)| self.alphabet_map[i as usize] = m);

        return Ok(());
    }

    /// Sort the alphabet and keep the map in relative order. 
    /// 
    /// Really just a wrapper of [fn@DFA::heap_sort]. 
    pub fn sort_alphabet_and_map(&mut self) {
        if self.alphabet.len() != self.alphabet_map.len() {
            panic!("Alphabet and map does not have the same length. ");
        }

        Self::heap_sort(&mut self.alphabet, &mut self.alphabet_map);
    }

    /// Sorts the char_vec while also retaining the correspondencies with map_vec.
    /// 
    /// For performance reasons, both the alphabet and the map must be sorted in
    /// ascending order. This is needed to be able to use binary search. 
    /// 
    /// If in a given index i map_vec\[i\] is translated to char_vec\[i\],
    /// after sorting, this relation is conserved even if i changes. 
    /// For more information about heapsort see [here](https://en.wikipedia.org/wiki/Heapsort). 
    fn heap_sort(char_vec: &mut Vec<char>, map_vec: &mut Vec<u16>) {
        // implementation source: https://www.geeksforgeeks.org/heap-sort/

        let len: u16 = char_vec.len() as u16;
        for i in (0..len / 2).rev() {
            Self::heapify(char_vec, map_vec, len, i);
        }

        for i in (1..len).rev() {
            char_vec.swap(0, i as usize);
            map_vec.swap(0, i as usize);

            Self::heapify(char_vec, map_vec, i, 0);
        }
    }

    /// Function used in [fn@DFA::heap_sort]. 
    fn heapify(char_vec: &mut Vec<char>, map_vec: &mut Vec<u16>, len: u16, idx: u16) {
        let length: usize = len as usize;
        let mut i: usize = idx as usize; 
        loop {
            let left: usize = 2 * i + 1;
            let right: usize = left + 1;

            let mut largest: usize = i;

            if left < length && char_vec[largest] < char_vec[left] {
                largest = left;
            }

            if right < length && char_vec[largest] < char_vec[right] {
                largest = right;
            }

            if largest != i {
                char_vec.swap(i, largest);
                map_vec.swap(i, largest);
                i = largest;
            } else {
                break;
            }
        }

        /*
                loop {
            let left: u16 = 2 * idx + 1;
            let right: u16 = left + 1;

            let mut largest: u16 = idx;

            if left < len && char_vec[largest as usize] < char_vec[left as usize] {
                largest = left;
            }

            if right < len && char_vec[largest as usize] < char_vec[right as usize] {
                largest = right;
            }

            if largest != idx {
                char_vec.swap(idx as usize, largest as usize);
                map_vec.swap(idx as usize, largest as usize);
                idx = largest;
            } else {
                break;
            }
        }
         */
    }

    /// Using binary search, get the index of the given symbol. 
    fn get_index_bs(&self, symbol: &char) -> Option<usize> {
        //source: https://spin.atomicobject.com/learning-rust-binary-search/
        let len: isize = self.alphabet.len() as isize;
        let mut high: isize = len - 1;
        let mut low: isize = 0;
        let mut mid: isize = len >> 1; //len / 2
        let mut current: char;

        while low <= high {
            current = self.alphabet[mid as usize];
            match current.cmp(symbol) {
                std::cmp::Ordering::Equal => return Some(mid as usize),
                std::cmp::Ordering::Less => low = mid + 1,
                std::cmp::Ordering::Greater => high = mid - 1,
            }
            mid = (high + low) >> 1;
        }

        return None;
    }
}

impl InstanceDFA {
    /// Create a new instance of a given [DFA]. 
    pub fn new(_reference: Rc<DFA>, _asociated_class: Option<TokenClass>) -> Self {
        InstanceDFA {
            reference: _reference,
            state: INITIAL_STATE,
            alive: true,
            asociated_class: _asociated_class,
        }
    }

    /// Advance the [InstanceDFA] by using the given symbol. 
    /// 
    /// Returns a boolean indicating if the [InstanceDFA] is alive or an error. 
    pub fn advance(&mut self, symbol: char) -> Result<bool, String> {
        //self.reference.alphabet.iter().position(|&x| x == symbol);
        //println!("Symbol: {:#?}", symbol);
        let index: usize;
        match self.reference.get_index_bs(&symbol) {
            Some(v) => index = v,
            None => {
                self.alive = false;
                return Err(format!("Symbol |{}| not in alphabet. ", symbol));
            }
        }

        let column: u16 = self.reference.alphabet_map[index]; //assume valid because assume DFA is properly set up

        self.state = self.reference.transition_table[self.state as usize][column as usize];

        self.alive = self.state != REJECTING_STATE;

        return Ok(self.alive);
    }

    /// Reset the [InstanceDFA] so it can be used again as new. 
    pub fn reset(&mut self) {
        self.state = INITIAL_STATE;
        self.alive = true;
    }

    /// Determinates if the [DFA] accepts or rejects the given String. 
    pub fn finalize(&self) -> bool {
        if !self.alive {
            return false;
        }

        return self
            .reference
            .ending_states
            .iter()
            .any(|&f_state| f_state == self.state);
    }

    /// Analyze a whole String and return if the [InstanceDFA] accepts or not, or an error. 
    pub fn analyze(&mut self, input: String) -> Result<bool, String> {
        for c in input.chars() {
            if !self.advance(c)? {
                return Ok(false);
            }
        }
        return Ok(self.finalize());
    }
}

impl Token {
    /// Create a new [Token]. 
    pub fn new(_lexeme: Option<String>, _class: TokenClass) -> Self {
        Token {
            lexeme: _lexeme,
            class: _class,
        }
    }

    /// Checks if the classes of 2 tokens are the same. 
    pub fn class_eq(&self, t: &Token) -> bool {
        return &self.class == &t.class;
    }

    /// Joins the 2 [Token] into one. 
    /// 
    /// The lexemmes are concatenated and the 
    /// class must be the same or one of them must be TokenClass::None, and 
    /// the other one will be set. 
    pub fn concatenate(&self, t: &Token, new_class: Option<TokenClass>) -> Token {
        let new_lexemme: Option<String>;

        new_lexemme = match &t.lexeme {
            Some(t_lex) => match &self.lexeme {
                Some(s_lex) => Some(s_lex.to_owned() + t_lex),
                None => Some(t_lex.to_owned()),
            },
            None => match &self.lexeme {
                Some(s_lex) => Some(s_lex.to_owned()),
                None => None,
            },
        };

        /*
            if new class is given, use it. Otherwise, if the classes of the
            tokens coincide, use them or use TokenClass::None otherwise.
        */
        let new_class: TokenClass = match new_class {
            Some(class) => class,
            None => {
                if t.class == self.class {
                    self.class.clone()
                } else {
                    TokenClass::None
                }
            }
        };

        let ret: Token = Token::new(new_lexemme, new_class);

        return ret;
    }

    /// Get the precedence and if it is left-asociative. 
    /// 
    /// If the operator is recognized, returns a tuple containing the precedence of the 
    /// operator and a bool determinating if its left-asociative or not. A lower value of 
    /// precedence means that it should be evaluated first. 
    pub fn get_precedence_operator(&self) -> Option<(i32, bool)> {
        /*
            If valid, returns a tuple with the precedence and if it is left-asociative (Left-to-right).
            If invalid (Lexeme = None or Class is None or distinct from class opertor),
            returns None.

            small values of priority means that the priority is high and
            must be evaluated first. Inspired on the C/C++ standard:
            https://en.wikipedia.org/wiki/Operators_in_C_and_C%2B%2B#Operator_precedence
        */

        if self.lexeme.is_none() {
            return None;
        }

        match self.class {
            TokenClass::Operator => {
                let lex: &str = &self.lexeme.as_ref().unwrap()[..];

                match lex {
                    ADD_STR => return Some((7, true)),
                    SUB_STR => return Some((7, true)),
                    MULT_STR => return Some((6, true)),
                    DIV_STR => return Some((5, true)),
                    EXP_STR => return Some((4, false)),
                    FACT_STR => return Some((2, false)),
                    MOD_STR => return Some((8, true)),
                    NEG_STR => return Some((3, false)),
                    _ => return None,
                }
            }
            _ => return None,
        }
    }
}

impl TokenModel {
    /// Creates a [TokenModel] of the given [Token]. 
    pub fn from_token(t: Token, _compare_lexemme: bool) -> Self {
        Self {
            token: t,
            compare_lexemme: _compare_lexemme,
        }
    }

    /// Compares a [Token] to itself. 
    pub fn cmp(&self, t: &Token) -> bool {
        if !self.token.class_eq(t) {
            return false;
        }

        if self.compare_lexemme {
            return self.token.lexeme == t.lexeme;
        }
        return true;
    }

    /// Gets a clone of the [Token] it contains. 
    pub fn get_token(&self) -> Token {
        return self.token.clone();
    }
}

impl Rule {
    /// Returns true if the given vector of [Token] follows the [Rule]. 
    pub fn follows_rule(&self, challenger: Vec<Token>) -> bool {
        if self.antecedent.len() != challenger.len() {
            return false;
        }

        let is_correct: bool =
            zip(self.antecedent.iter(), challenger.iter()).all(|(model, t)| model.cmp(t));

        return is_correct;
    }
}

impl SRA {
    /// Cretaes a new [SRA]. 
    pub fn new(_rules: Vec<Rule>) -> Self {
        Self {
            stack: vec![],
            rules: _rules,
            ast: vec![],
        }
    }
    /// Shifts the [SRA]. (Adds a new token)
    fn shift(&mut self, t: Token) -> Result<(), String> {
        self.add_ast(&t)?;
        self.stack.push(t);
        return Ok(());
    }

    /// Reduces the stack with the [Rule] until it can no longer be reduced further. 
    fn reduce(&mut self) -> bool {
        for rule in &self.rules {
            let slice_start: isize = self.stack.len() as isize - rule.antecedent.len() as isize;
            if slice_start < 0 {
                continue;
            }

            let scan: bool = rule.follows_rule(self.stack[(slice_start as usize)..].to_vec());
            if scan {
                let keep_items_num: usize = self.stack.len() - rule.antecedent.len();
                let dr = self.stack.drain(keep_items_num..);
                let mut cons_tok: Token = rule.consequent.get_token();
                if cons_tok.lexeme.is_none() {
                    let new_lex = dr.fold(String::from(""), |mut acc: String, t: Token| {
                        match t.lexeme {
                            Some(lex) => {
                                acc.push_str(&lex);
                                acc.push_str(" ");
                            }
                            None => {}
                        }
                        acc
                    });
                    cons_tok.lexeme = Some(new_lex);
                } else {
                    drop(dr);
                }
                self.stack.push(cons_tok);
                self.update_AST(keep_items_num);
                return true;
            }
        }

        return false;
    }

    /// Uses the given token to [fn@SRA::shift] and [fn@SRA::reduce]. 
    pub fn advance(&mut self, t: Token) -> Result<(), String> {
        self.shift(t)?;

        let mut has_reduced: bool = true;
        while has_reduced {
            has_reduced = self.reduce();
        }

        return Ok(());

    }

    /// Returns true if the stack only contains the staritng [Token]. 
    /// 
    /// Used to determinate if the parsing is successfull. 
    pub fn just_starting_token(&self) -> bool {
        if self.stack.len() != 1 {
            return false;
        }

        return self.stack[0].class == TokenClass::NTStart;
    }

    /// Resets the [SRA] so it can be used again. 
    pub fn reset(&mut self) {
        self.stack.clear();
        self.ast.clear();
    }

    /// Internal function to add a token to the [AST]. 
    fn add_ast(&mut self, new_tok: &Token) -> Result<(), String> {
        //let new_tok = self.stack.get(self.stack.len() - 1).unwrap();
        if let TokenClass::None = &new_tok.class {
            self.ast.push(AST::new_empty());
            return Ok(());
        }
        let class: TokenClass = new_tok.class.clone();

        let ret: Element;
        match class {
            TokenClass::Number => {
                let n: f64 = new_tok.lexeme.clone().unwrap().parse::<f64>().unwrap();
                let result: Result<Number, Number> = Number::rationalize(n);
                match result {
                    Ok(v) => ret = Element::Number(v),
                    Err(_) => ret = Element::Number(Number::Real(n)),
                }
            }
            TokenClass::Operator => {
                let lexeme_opt: &Option<String> = &new_tok.lexeme;
                let aux: &String = lexeme_opt.as_ref().unwrap();
                let lexeme_str: &str = aux.as_str();
                match lexeme_str {
                    ADD_STR => {
                        ret = Element::Add;
                    }
                    SUB_STR => {
                        ret = Element::Sub;
                    }
                    MULT_STR => {
                        ret = Element::Mult;
                    }
                    DIV_STR => {
                        ret = Element::Div;
                    }
                    EXP_STR => {
                        ret = Element::Exp;
                    }
                    FACT_STR => {
                        ret = Element::Fact;
                    }
                    MOD_STR => {
                        ret = Element::Mod;
                    }
                    NEG_STR => {
                        ret = Element::Neg;
                    }
                    _ => {
                        return Err(format!(
                            "Invalid operator / operator not supported: {:?}",
                            lexeme_str
                        ));
                    }
                }
            }
            TokenClass::Identifier => {
                ret = Element::Function(new_tok.lexeme.as_ref().unwrap().clone());
            }
            TokenClass::SpecialChar => ret = Element::None,
            TokenClass::NTStart => ret = Element::None,
            TokenClass::None => {
                return Err(format!("Token has Tokenclass::None: {:?}", new_tok));
            }
        }

        self.ast.push(AST::new(ret));
        return Ok(());
    }

    /// Updates the [AST] so it is now correct.
    #[allow(non_snake_case)] 
    fn update_AST(&mut self, start_idx: usize) {
        /*Basic implementation: always assumes last element is the relevant operator and uses
        everything else as childs. [start_idx, end_idx). */
        let end_idx: usize = self.ast.len();

        //let values_slice = &self.ast[start_idx..(end_idx - 1)];
        let mut oper_token: AST = self.ast.pop().unwrap();

        let new_childs = self.ast.drain(start_idx..(end_idx - 1));

        new_childs.for_each(|x| oper_token.add_children(x));
        //new_childs.for_each(|x| oper_token.children.push(Rc::new(RefCell::new(x))));

        self.ast.push(oper_token);
    }
}

impl AST {

    /// Creates an empty [AST] with 1 element that contains nothing. 
    pub fn new_empty() -> Self {
        Self {
            value: Element::None,
            children: vec![],
        }
    }

    /// Creates an [AST] using the given element. 
    pub fn new(expr: Element) -> Self {
        Self {
            value: expr,
            children: vec![],
        }
    }

    /// Adds the given [AST] as a children of Self. 
    pub fn add_children(&mut self, child: AST) {
        let ast_ptr: Rc<RefCell<AST>> = Rc::new(RefCell::new(child));
        self.children.push(ast_ptr);
    }

    /// Clears the [AST]. 
    pub fn set_empty(&mut self) {
        self.children.clear();
        self.value = Element::None;
    }
}

impl Evaluable for AST {
    /// Evaluates the [AST] recursively. 
    fn evaluate(&mut self) -> Result<Number, String> {
        match &self.value {
            Element::Function(name) => {
                return crate::functions::Functions::find_and_evaluate(
                    name.as_str(),
                    (*self.children[0].borrow_mut()).evaluate()?,
                );
            }
            Element::Add => {
                let mut acc: Number = Number::new_rational(0, 1)?;

                for x in &self.children {
                    acc = (*x.borrow_mut()).evaluate()? + acc
                }

                return Ok(acc);
            }
            Element::Sub => {
                match self.children.len() {
                    1 => {
                        return match (*self.children[0].borrow_mut()).evaluate()? {
                            Number::Real(x) => Ok(Number::new_real(-x)),
                            Number::Rational(n, d) => Ok(Number::new_rational(-n, d)?),
                        };

                        //return (*self.children[0].borrow_mut()).evaluate();
                    }
                    2 => {
                        return Ok((*self.children[0].borrow_mut()).evaluate()?
                            - (*self.children[1].borrow_mut()).evaluate()?);
                    }
                    _ => {
                        return Err(format!(
                            "Substraction needs exacly 1 or 2 arguments, {:?} were provided. \n",
                            self.children.len()
                        ));
                    }
                }
            }
            Element::Mult => {
                if self.children.len() < 2 {
                    return Err(format!(
                        "Multiplication needs at least 2 arguments, {:?} were provided. \n",
                        self.children.len()
                    ));
                }

                let mut acc: Number = Number::new_rational(1, 1)?;

                for x in &self.children {
                    acc = (*x.borrow_mut()).evaluate()? * acc;
                }

                return Ok(acc);
            }
            Element::Div => {
                if self.children.len() != 2 {
                    return Err(format!(
                        "Division needs exacly 2 arguments, {:?} were provided. \n",
                        self.children.len()
                    ));
                }

                let inverse: Number = crate::functions::Functions::find_and_evaluate(
                    "inv",
                    (*self.children[1].borrow_mut()).evaluate()?,
                )?;

                return Ok((*self.children[0].borrow_mut()).evaluate()? * inverse);
            }
            Element::Exp => {
                if self.children.len() != 2 {
                    return Err(format!(
                        "Exponentiation needs exacly 2 arguments, {:?} were provided. \n",
                        self.children.len()
                    ));
                }

                return Ok((*self.children[0].borrow_mut())
                    .evaluate()?
                    .raise_exponent((*self.children[1].borrow_mut()).evaluate()?)?);
            }
            Element::Fact => {
                if self.children.len() != 1 {
                    return Err(format!(
                        "The factorial takes exacly 1 arguments, {:?} were provided. \n",
                        self.children.len()
                    ));
                }

                let x: Number = (*self.children[0].borrow_mut()).evaluate()?;
                if !x.is_integer() {
                    return Err(format!(
                        "The factorial only takes integer inputs. Number found: {:?}\n",
                        x
                    ));
                }
                if !x.is_positive() {
                    return Err(format!(
                        "The factorial only takes positive inputs. Number found: {:?}\n",
                        x
                    ));
                }

                match x {
                    Number::Rational(num, _) => {
                        if num == 0 {
                            return Ok(Number::new_rational(1, 1)?);
                        }
                        let mut acc: i64 = 1;
                        for i in 1..=num {
                            acc = acc * i;
                        }

                        return Ok(Number::new_rational(acc, 1)?);
                    }
                    _ => return Err(format!("Impossible case. Real number factorial. \n")),
                };
            }
            Element::Mod => {
                if self.children.len() != 2 {
                    return Err(format!(
                        "Modulus needs exacly 2 arguments, {:?} were provided. \n",
                        self.children.len()
                    ));
                }

                let x: Number = (*self.children[0].borrow_mut()).evaluate()?;
                let y: Number = (*self.children[1].borrow_mut()).evaluate()?;

                if x.is_integer() && y.is_integer() {
                    let x_int: i64 = match x {
                        Number::Rational(num, _) => num,
                        _ => return Err(String::from("Unreachable statement. ")),
                    };

                    let y_int: i64 = match y {
                        Number::Rational(num, _) => num,
                        _ => return Err(String::from("Unreachable statement. ")),
                    };

                    return Ok(Number::new_rational(x_int % y_int, 1)?);
                }

                let x_numerical: f64 = x.get_numerical();
                let y_numerical: f64 = y.get_numerical();

                return Ok(Number::new_real(x_numerical % y_numerical));
            }
            Element::Number(x) => Ok(x.clone()),
            Element::Neg => {
                /*negation ( -x ) */
                if self.children.len() != 1 {
                    return Err(format!(
                        "Negation (-x) needs exacly 1 arguments, {:?} were provided. \nParsing went wrong. \n",
                        self.children.len()
                    ));
                }
                let mut ret: Number = (*self.children[0].borrow_mut()).evaluate()?;

                match &mut ret {
                    Number::Real(r) => *r = -*r,
                    Number::Rational(n, _) => *n = -*n,
                }

                return Ok(ret);
            }
            Element::None => return Err(String::from("None reached. ")),
        }
    }
}

impl Number {
    /// Creates a new real [Number]. 
    pub fn new_real(x: f64) -> Self {
        Number::Real(x)
    }

    /// Creates a new rational [Number]. Will fail if den = 0. 
    pub fn new_rational(num: i64, den: u64) -> Result<Self, String> {
        if den == 0 {
            return Err(format!("Division by 0 is not possible. \n"));
        }
        return Ok(Number::Rational(num, den));
    }

    /// Takes x as input and tries to see if it can be expressed as a/b without too much error. 
    /// 
    /// Toleracne: 0.000000001
    /// If it can be converted, it will return a [Number::Rational], otherwise will 
    /// return [Number::Rational] with the closest aproximation it can find. 
    /// 
    /// The [Number] contained in the result will be the same. The only difference is if 
    /// it is wrapped arround Ok() (The aproximation is good enough) or 
    /// Err() (Aproximation is NOT good enough). 
    pub fn rationalize(mut x: f64) -> Result<Number, Number> {
        let tolerance: f64 = 0.000000001;
        let terms: u32 = 22;

        if x == 0.0 {
            return Ok(Number::Rational(0, 1));
        }

        let is_neg: bool = if x < 0.0 {
            x = -x;
            true
        } else {
            false
        };

        let mut n_vals: (f64, f64) = (x, 1.0);
        let mut sequence: Vec<i64> = Vec::new();
        let mut found_0: bool = false;

        for _i in 2..terms {
            let new_term: i64 = (n_vals.0 / n_vals.1) as i64;
            sequence.push(new_term);

            let new_n: f64 = n_vals.0 % n_vals.1;
            if new_n <= tolerance {
                found_0 = true;
                break;
            }
            n_vals = (n_vals.1, new_n);
            //println!("N_val[{}] = {}", i, n_vals.1);
        }

        let mut rational: (i64, i64);
        rational = (sequence.pop().unwrap(), 1);

        sequence.reverse();

        for term in sequence {
            rational = (rational.1, rational.0); //inverse
            rational.0 = rational.0 + rational.1 * term; //add term units to the fraction
                                                         //println!("Rational: {:?}", rational);
        }

        if is_neg {
            rational.0 = -rational.0;
        }
        let aprox: f64 = rational.0 as f64 / rational.1 as f64;
        //println!("x = {} ~= {} / {} = {} \nAbs Err = {}\t\t Rel Err = {}\n\n", x, rational.0, rational.1, aprox, (aprox - x).abs(), (aprox - x).abs() / x);

        if found_0 && (aprox - x).abs() < tolerance {
            return Ok(Number::Rational(rational.0, rational.1 as u64));
        } else {
            return Err(Number::Rational(rational.0, rational.1 as u64));
        }
    }

    /// Use the euclidean algorithm to determinate the larger common divider (lcd) 
    /// between x and y.  
    pub fn euclidean_algorithm(mut x: u128, mut y: u128) -> u128 {
        loop {
            if x == 0 {
                return y;
            }

            (x, y) = (y % x, x);
        }
    }

    /// Determinates if the given integer is a perfect squer or not. 
    /// 
    /// If it is, returns 
    /// Some() with the square root as an integer. Otherwise returns None.  
    pub fn is_perfect_square(x: i64) -> Option<i64> {
        //Perfec number info: https://en.wikipedia.org/wiki/Square_number

        if x < 0 {
            // no negative numbers
            return None;
        }

        //No square ends with the digit 2, 3, 7, or 8.
        let remainder: i64 = x % 10;

        match remainder {
            0 => {}
            1 => {}
            4 => {}
            5 => {}
            6 => {}
            9 => {}
            _ => {
                return None;
            }
        }

        let sqrt: i64 = (x as f64).sqrt().floor() as i64;
        if sqrt * sqrt == x {
            return Some(sqrt);
        }

        return None;
    }

    /// Get the numerical value of Self. 
    pub fn get_numerical(&self) -> f64 {
        match self {
            Number::Real(r) => r.clone(),
            Number::Rational(n, d) => (n.clone() as f64) / (d.clone() as f64),
        }
    }

    /// If self is [Number::Rational], tries to reduce num/den to it's irreductible fraction. 
    /// 
    /// It is usefull to keep the values low, while keeping precision. 
    pub fn minimize(&mut self) {

        let mut rationalize: Option<(i64, u64)> = None; 

        match self {
            Number::Rational(num, den) => {
                let sign: i64 = num.signum();
                *num *= sign;
                let gcd = Self::euclidean_algorithm(*num as u128, *den as u128) as u64;
                if gcd != 1 {
                    *num = *num / gcd as i64;
                    *den = *den / gcd;
                }
                *num *= sign;
            }
            Number::Real(real) => {
                match Number::rationalize(*real) {
                    Ok(new_rational) => {

                        rationalize = match new_rational {
                            Number::Real(_) => unreachable!("rationalize always returns a Rational variant. "),
                            Number::Rational(n, d) => Some((n, d)),
                        }; 
                    },
                    Err(_failed_rational) => { },
                }
            },

        }

        if let Some((num, den)) = rationalize {
            *self = Number::new_rational(num, den).expect("Denominator should be non-zero. "); 
        }

    }

    /// Returns true if the number is an integer. 
    pub fn is_integer(&self) -> bool {
        //assume self is altrady minimized. if not / not sure, call self.minimize()
        match self {
            Number::Rational(_, den) => *den == 1,
            _ => false, //TODO: floor(x) == x
        }
    }

    /// Returns true is the number is positive or 0. 
    pub fn is_positive(&self) -> bool {
        // 0 is considered positive
        match self {
            Number::Real(x) => 0.0 <= *x,
            Number::Rational(x, _) => 0 <= *x,
        }
    }

    /// Computes self^exponent. 
    /// 
    /// It tries to keep the result as [Number::Rational], 
    /// otherwise, it automatically converts it to [Number::Real]. Attempting to 
    /// raise a negative value to a non-integer exponent will result in an Error. 
    pub fn raise_exponent(&mut self, mut exponent: Number) -> Result<Number, String> {
        self.minimize(); //just to make sure
        exponent.minimize();

        //println!("{:?} ^ {:?}", self, exponent);

        if !self.is_positive() {
            if !exponent.is_integer() {
                return Err(format!("Cannot raise negative number to a fractioary exponent. \nComplex numbers are not handled. \n"));
            }
        }

        if !exponent.is_positive() {
            //a ^ -b = (1/a) ^ b

            match &mut exponent {
                Number::Real(x) => *x = -1.0 * *x,
                Number::Rational(x, _) => *x = -*x,
            }
            *self = match self {
                Number::Real(r) => Number::Real(1.0 / *r),
                Number::Rational(num, den) => Number::Rational(*den as i64, *num as u64),
            }
        }

        if exponent.is_integer() {
            //square and multiply

            let mut exponent_: u64 = match exponent {
                Number::Rational(num, _) => num as u64,
                _ => panic!("Impossible case. \n"),
            };
            let mut accumulator: Number = self.clone();
            let mut ret: Number = Number::new_rational(1, 1).expect("Non zero div rational");

            while 0 < exponent_ {
                if (exponent_ & 1) == 1 {
                    // multiply
                    //is odd
                    ret = ret * accumulator.clone();
                }

                /*
                accumulator = match accumulator {
                    // square
                    Number::Real(x) => Number::Real(x * x),
                    Number::Rational(num, den) => Number::Rational(num * num, den * den),
                };*/
                accumulator = accumulator.clone() * accumulator;

                exponent_ = exponent_ >> 1; // divide by 2
            }

            return Ok(ret);
        }

        let numerical_base: f64 = self.get_numerical();
        let numerical_exponent: f64 = exponent.get_numerical();

        let result: f64 = numerical_base.powf(numerical_exponent);

        return Ok(Number::new_real(result));
    }

    /// Returns true, if self is within tolerance units of the given [Number]. 
    pub fn in_tolerance_range(&self, other: &Number, tolerance: f64) -> bool {
        /*Use neg. number or 0 in tolerance to ignore check */

        if tolerance <= 0.0 {
            match (&self, &other) {
                (Number::Real(x), Number::Real(y)) => return x == y,
                (Number::Real(r), Number::Rational(num, den)) => {
                    return *r == (*num as f64 / *den as f64);
                }
                (Number::Rational(num, den), Number::Real(r)) => {
                    return *r == (*num as f64 / *den as f64);
                }
                (Number::Rational(self_num, self_den), Number::Rational(oth_num, oth_den)) => {
                    if self_num == oth_num && self_den == oth_den {
                        return true;
                    }

                    return (*self_num as f64 / *self_den as f64)
                        == (*oth_num as f64 / *oth_den as f64);
                }
            }
        }

        match (&self, &other) {
            (Number::Real(x), Number::Real(y)) => return (x - y).abs() < tolerance,
            (Number::Real(r), Number::Rational(num, den)) => {
                return (*r - (*num as f64 / *den as f64)).abs() < tolerance;
            }
            (Number::Rational(num, den), Number::Real(r)) => {
                return (*r - (*num as f64 / *den as f64)).abs() < tolerance;
            }
            (Number::Rational(self_num, self_den), Number::Rational(oth_num, oth_den)) => {
                if self_num == oth_num && self_den == oth_den {
                    return true;
                }

                return ((*self_num as f64 / *self_den as f64)
                    - (*oth_num as f64 / *oth_den as f64))
                    .abs()
                    < tolerance;
            }
        }
    }

    /// Returns the number as a string. 
    pub fn as_str(&self) -> String {
        return self.get_numerical().to_string();
    }
}

impl ops::Add<Number> for Number {
    type Output = Number;

    fn add(self, rhs: Number) -> Self::Output {
        match self {
            Number::Real(left_real) => match rhs {
                Number::Real(right_real) => Number::new_real(left_real + right_real),
                Number::Rational(right_num, right_den) => {
                    Number::new_real(left_real + (right_num as f64 / right_den as f64))
                }
            },
            Number::Rational(left_num, left_den) => match rhs {
                Number::Real(right_real) => {
                    Number::new_real(right_real + (left_num as f64 / left_den as f64))
                }
                Number::Rational(right_num, right_den) => {
                    //let num: i64 = right_num * left_den as i64 + left_num * right_den as i64;
                    //let den: u64 = left_den * right_den;

                    //assuming both numbers are already minimized
                    let lcd: u128 =
                        Number::euclidean_algorithm(left_den as u128, right_den as u128);
                    let lcm_res: Result<u64, std::num::TryFromIntError> =
                        u64::try_from((left_den as u128 * right_den as u128) / lcd);

                    if lcm_res.is_err() {
                        // lcm is too large, transform to float and continue
                        let left_number: f64 = left_num as f64 / left_den as f64;
                        let right_number: f64 = right_num as f64 / right_den as f64;

                        return Number::new_real(left_number + right_number);
                    }

                    let lcm: u64 = lcm_res.unwrap();

                    let mult_factor_left: i64 = lcm as i64 / left_den as i64;
                    let mult_factor_right: i64 = lcm as i64 / right_den as i64;

                    let num: i64 = right_num * mult_factor_right + left_num * mult_factor_left;
                    let den: u64 = lcm;

                    let mut new_rational: Number =
                        Number::new_rational(num, den).expect("Non zero div rational");
                    new_rational.minimize();

                    return new_rational;
                }
            },
        }
    }
}

impl ops::Sub<Number> for Number {
    type Output = Number;

    fn sub(self, rhs: Number) -> Self::Output {
        //println!("{:?} - {:?}", self, rhs);

        match self {
            Number::Real(left_real) => match rhs {
                Number::Real(right_real) => Number::new_real(left_real - right_real),
                Number::Rational(right_num, right_den) => {
                    Number::new_real(left_real - (right_num as f64 / right_den as f64))
                }
            },
            Number::Rational(left_num, left_den) => match rhs {
                Number::Real(right_real) => {
                    Number::new_real((left_num as f64 / left_den as f64) - right_real)
                }
                Number::Rational(right_num, right_den) => {
                    //let num: i64 = right_num * left_den as i64 - left_num * right_den as i64;
                    //let den: u64 = left_den * right_den;

                    //assuming both numbers are already minimized
                    let lcd: u128 =
                        Number::euclidean_algorithm(left_den as u128, right_den as u128);
                    let lcm_res: Result<u64, std::num::TryFromIntError> =
                        u64::try_from((left_den as u128 * right_den as u128) / lcd);

                    if lcm_res.is_err() {
                        // lcm is too large, transform to float and continue
                        let left_number: f64 = left_num as f64 / left_den as f64;
                        let right_number: f64 = right_num as f64 / right_den as f64;

                        return Number::new_real(left_number + right_number);
                    }

                    let lcm: u64 = lcm_res.unwrap();

                    let mult_factor_left: i64 = lcm as i64 / left_den as i64;
                    let mult_factor_right: i64 = lcm as i64 / right_den as i64;

                    let num: i64 = left_num * mult_factor_left - right_num * mult_factor_right;
                    let den: u64 = lcm;

                    let mut new_rational: Number =
                        Number::new_rational(num, den).expect("Non zero div rational");
                    new_rational.minimize();

                    return new_rational;
                }
            },
        }
    }
}

impl ops::Mul<Number> for Number {
    type Output = Number;

    fn mul(self, rhs: Number) -> Self::Output {
        //println!("{:?} * {:?}", self, rhs);

        match self {
            Number::Real(left_real) => match rhs {
                Number::Real(right_real) => Number::new_real(left_real * right_real),
                Number::Rational(right_num, right_den) => {
                    Number::new_real(left_real * (right_num as f64 / right_den as f64))
                }
            },
            Number::Rational(left_num, left_den) => match rhs {
                Number::Real(right_real) => {
                    Number::new_real((left_num as f64 / left_den as f64) * right_real)
                }
                Number::Rational(right_num, right_den) => {
                    //handle possible overflow and coerse to rational if so
                    let num_opt: Option<i64> = left_num.checked_mul(right_num);
                    let den_opt: Option<u64> = left_den.checked_mul(right_den);

                    let mut new_rational: Number = match (num_opt, den_opt) {
                        (None, None) => {
                            let num: f64 = left_num as f64 * right_num as f64;
                            let den: f64 = left_den as f64 * right_den as f64;
                            Number::new_real(num / den)
                        }
                        (None, Some(den)) => {
                            let num: f64 = left_num as f64 * right_num as f64;
                            Number::new_real(num / den as f64)
                        }
                        (Some(num), None) => {
                            let den: f64 = left_den as f64 * right_den as f64;
                            Number::new_real(num as f64 / den)
                        }
                        (Some(num), Some(den)) => {
                            Number::new_rational(num, den).expect("Non zero div rational")
                        }
                    };

                    new_rational.minimize();

                    return new_rational;
                }
            },
        }
    }
}

impl fmt::Debug for Number {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Number::Real(r) => write!(f, "{} ", r),
            Number::Rational(num, den) => {
                write!(f, "{}/{} ~= {}", num, den, *num as f64 / *den as f64)
            }
        }
        //f.debug_struct("Number").field("value", &self.value).finish()
    }
}

impl Evaluable for Number {
    /// Returns the [Number] itself. 
    fn evaluate(&mut self) -> Result<Number, String> {
        return Ok(self.clone());
    }
}

impl Calculator {

    /// Bundles the necessary info thogether. 
    pub fn new(_dfas: Vec<Rc<DFA>>, _parser: SRA) -> Self {
        let associated_class: Vec<Option<TokenClass>> = vec![
            Some(TokenClass::Number),
            Some(TokenClass::Operator),
            Some(TokenClass::SpecialChar),
            Some(TokenClass::Identifier),
        ];

        let _idfas: Vec<InstanceDFA> = crate::setup::setup_idfas(&_dfas, associated_class);

        Self {
            dfas: _dfas,
            idfas: _idfas,
            parser: _parser,
        }
    }
}
