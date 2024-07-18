//! This project allows the numerical evaluation of string expressions. 
//! 
//! ## Basic usage and example
//! 
//! The following code gives a bery basic example of the usage: 
//! ```
//! let input: String = String::from("4 -2+sqrt(sin(PI/2))**ln(1) + 1/cos(tan(exp(-2.2)))"); 
//! let mut calc: Calculator = Calculator::new(setup::setup_dfas(), SRA::new(setup::get_rules()));
//! 
//! match evaluate_expression(input, &mut calc, false) {
//!     Ok(final_value) => {
//!         println!("\nEvaluated to: \t{:?}\n", final_result);
//!     }
//!     Err(msg) => panic!("\n{}", msg),
//! }
//! ```
//! After executing, it prints the following: 
//! > Evaluated to:   4.0062213501365935
//! 
//! However, the program will also try to work with fractions when possible. 
//! If we let input `"1/2 + 5/9"`, the printed message will be: 
//! > Evaluated to:   19/18 ~= 1.0555555555555556
//! 
//! The project can be run with `cargo run -- "<expression>"`, where `<expression>` is 
//! your desired input.  
//! 
//! ***
//! ## Operations and syntax: 
//! 
//! This project supports the following operations: 
//! 
//! 1) Addition                         (using `+`, as in `"2+5"`)
//! 2) Substraction and negation        (using `-`, as in `"3-5.5"` or in `"-sin(-2)"`)
//! 3) Multiplication                   (using `*`, as in `"4*8"`)
//! 4) Division                         (using `/`, as in `"2/9"`)
//! 5) Modulus                          (using `%`, as in `"19%10"`)
//! 6) Exponentiation                   (using `^` or `**` as in `"2^10"` or `"2**10"`)
//! 7) Factorial                        (using `!`, as in `"6!"`)
//! 8) Square root                      (using `sqrt(x)`, as in `"sqrt(3)"`)
//! 9) Sinus                            (using `sin(x)`, as in `"sin(PI)"`)
//! 10) Cosinus                         (using `cos(x)`, as in `"cos(PI)"`)
//! 11) Tangent                         (using `tan(x)`, as in `"tan(PI)"`)
//! 12) Arcsinus (inverse sinus)        (using `arcsin(x)`, as in `"arcsin(0)"`)
//! 13) Arccosinus (inverse cosinus)    (using `arccos(x)`, as in `"arccos(0)"`)
//! 14) Arctangent (inverse tangent)    (using `arctan(x)`, as in `"arctan(0)"`)
//! 15) Exponential                     (using `exp(x)`, as in `"exp(3)"`)
//! 16) Natural logarithm               (using `ln(x)`, as in `"ln(e)"`)
//! 17) Absolute value                  (using `abs(x)`, as in `"abs(-1)"`)
//! 
//! + All the functions can be combined and composed in any way as long as they are 
//! mathematically correct and fullfill the syntax requirments. 
//! 
//! + Some operations have priority over others, such as multiplication over 
//! addition. That means that `"2+5*3"` will be evaluated as `"2+(5*3)"`. To overwrite 
//! the order parenthesis can be used `()`. 
//! 
//! + All the trigonometric functions work with radians. In order to use degrees, multiply your 
//! value by `DEG2RAD`, for example: `sin(90*DEG2RAD)`
//! 
//! + Only real values are supported (no complex values), therefore `"sqrt(-1)"` lies outside 
//! the domains of the function and will return an error indicating the invalid 
//! evaluation. 
//! 
//! + Division by 0 is not allowed. 
//! 
//! + Every parenthesis (any of `()`, `{}` and `[]` can also be used. They are 
//! equivalent but have to match) must be closed. Therefore each parenthesys 
//! symbol must have its correspondinc counterpart. 
//! 
//! + Spaces are ignored, you can add all you want or even remove them completly. 
//! 
//! + Remember that a [logarithm](https://en.wikipedia.org/wiki/Logarithm#Change_of_base) 
//! in any base `b` can be expressed as `log_b(x) = (ln(x)/ln(b))` . 
//! 
//! ***
//! 
//! ## Constants: 
//! 
//! The program will automatically translate some constants to it's corresponding
//! numerical values. 
//! 
//! 1)  [x]  PI              (equal to 3.141592653589793)
//! 2)  [x]  RAD2DEG         (equal to 57.29577951308232 = 180 / PI)
//! 3)  [x]  DEG2RAD         (equal to 0.0174532925199433 = PI / 180)
//! 4)  [x]  phi             (equal to 1.618033988749895 = (1+sqrt(5))/2 )
//! 5)  [x]  e               (equal to 2.718281828459045)
//! 6)  [x]  tau             (equal to 6.283185307179586)
//! 7)  [ ]  gravitational   (equal to 0.000000000066743 = 6.6743 * 10^-11 m^3/(kg * s^2), the gravitational constant)
//! 8)  [ ]  plank           (equal to 0.000000000000000000000000000000000662607015 = 6.62607015 * 10^-34 J*s, Plank constant)
//! 9)  [ ]  light           (equal to 299 792 458 m/s, speed of light)
//! 10) [ ]  elecprem        (equal to 0.0000000000088541878188 = 8.8541878188 * 10^-12, vacuum electric permittivity)
//! 11) [ ]  magnprem        (equal to 0.00000125663706127 = 1.25663706127 * 10^-6, vacuum magnetic permeability)
//! 12) [ ]  elecmass        (equal to 0.00000000000000000000000000000091093837139 = 9.1093837139 * 10^-31 kg, mass of the electron)
//! 
//! 
//! Constants can be written on any combination of uppercase and lowercase letters. 
//! Physical constants have [IS units](https://en.wikipedia.org/wiki/International_System_of_Units). 
//! 



use std::env;

//#[allow(unused_parens)]

/// All information regarding the datastructures. 
pub mod datastructures;

/// Functions to process the string until evaluation. 
pub mod processing;

/// Hardcoded data that other parts of the code use. 
pub mod setup;

/// ## Cotains the functions and constants that can be used. 
pub mod functions;

/// All the testing. 
#[cfg(test)]
mod tests;


use crate::datastructures::*;
use functions::Constants;
use crate::processing::*; 

fn main() {
    let args: Vec<String> = env::args().collect();

    let mut input_vec: Vec<String> = args.clone();
    input_vec.remove(0);

    let mut input: String = if input_vec.len() == 1 && input_vec[0].contains('"') {
        input_vec[0][1..(input_vec[0].len() - 1)].to_string()
        //remove quotes and return string
    } else {
        input_vec.iter().fold("".to_string(), |acc, x| acc + x)
    };

    if input.len() == 0 {
        panic!("\nUse this command with an input. \ncargo run -- \"<your expression>\"");
    }

    //input.remove(0);
    input = input.trim().to_string();
    //input = space_adder(input);
    println!("\nProcessed input: {:#?}\n", input);

    let mut calc: Calculator = Calculator::new(setup::setup_dfas(), SRA::new(setup::get_rules()));

    match evaluate_expression(input, &mut calc, true) {
        Ok(_v) => {}
        Err(msg) => panic!("\n{}", msg),
    }
}
