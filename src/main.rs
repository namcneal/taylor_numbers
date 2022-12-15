fn main() {
    let x = TaylorNumber::new(1.0);
    println!("x is a Taylor number with no derivatives: {:?}", x);

    let mut y = TaylorNumber::new(1.0);
    y.perturb_by(1.0);
    println!("y is a Taylor number with one derivative: {:?}", y);

    let z = y.clone() + x.clone();
    println!("z demonstrates addition: {:?}", z);

    let mut a = TaylorNumber::new(3.0);
    a.perturb_by(1.0);
    let s3 =  cube(a);
    println!("Every derivative of the cubic (gotta figure how to take this) {:?}", s3)
}

fn cube<T>(x:TaylorNumber<T>) -> TaylorNumber<T> where T:TaylorScalar<T> {
        x.clone() * x.clone() * x
}

/* Type definition and implementations for TaylorNumber */

use num_traits::{One, Zero};
use std::ops::{Add, Mul, Neg, Sub, Div};

pub trait TaylorScalar<T> : Copy + Add<T, Output=T> + Sub<T, Output=T> 
                                 + Mul<T, Output=T> + Div<T, Output=T>
                                 + Neg<Output=T>    + Zero + One  {}
impl TaylorScalar<f64> for f64 {}

#
[derive(Debug, Clone)]
enum TaylorNumber<T : TaylorScalar<T>>{
    Unperturbed(T),
    Perturbed(T, Box<TaylorNumber<T>>)
}

impl<T> TaylorNumber<T> where T: TaylorScalar<T>{

    fn new(re:T) -> TaylorNumber<T> 
    where T: One
    {
        Self::Unperturbed(re)
    }

    fn perturb_by(&mut self, df:T) where 
        T: Add<Output=T>
    {
        match self{
            Self::Unperturbed(re) => *self = Self::Perturbed(*re, Box::new(Self::Unperturbed((df)))),
            Self::Perturbed(re, df_self) => {
                *self = Self::Perturbed(*re, Box::new((*df_self.clone()) + Self::Unperturbed(df)))
            }
        }
    }

    fn real(&self)-> T{
        match self {
            TaylorNumber::Unperturbed(re)  => *re,
            TaylorNumber::Perturbed(re, _) => *re
        }
    }

    fn diff(self) -> TaylorNumber<T>{
        match self {
            TaylorNumber::Unperturbed(_) => Self::Unperturbed(T::zero()),
            TaylorNumber::Perturbed(_, df) => *df
        }
    }

    fn derivative(self, nth : u32) -> TaylorNumber<T> where 
        T: Zero
    {
        
        match nth { 
            0 => Self::Unperturbed(self.real()),
            _ => self.derivative(nth - 1).diff()
        }
    }
}

impl<T> Add for TaylorNumber<T> where 
    T: TaylorScalar<T>{
    type Output = Self;
    
    fn add(self:Self, rhs:Self) -> TaylorNumber<T>{
        match (self, rhs){
            (Self::Unperturbed(a), Self::Unperturbed(b)) => Self::Unperturbed(a + b),
            (Self::Unperturbed(a), Self::Perturbed(b, df)) => Self::Perturbed(a+b, df),
            (Self::Perturbed(a, df), Self::Unperturbed(b)) => Self::Perturbed(a+b, df),
            (Self::Perturbed(a, df_a), Self::Perturbed(b, df_b)) => Self::Perturbed(a+b, Box::new((*df_a + *df_b))),

        }
    }
}

impl<T> Zero for TaylorNumber<T> where 
    T : TaylorScalar<T> {

    fn zero() -> TaylorNumber<T>{
        Self::Unperturbed(T::zero())
    }

    fn is_zero(&self) -> bool {
        match self{
            Self::Unperturbed(re) => T::is_zero(re),
            Self::Perturbed(re, df)   => T::is_zero(re) && df.is_zero()
        }
    }
}

impl<T> Neg for TaylorNumber<T> where 
    T: TaylorScalar<T>{

    type Output = Self;

    fn neg(self) -> TaylorNumber<T>{
        match self{
            Self::Unperturbed(re)   => Self::Unperturbed(-re),
            Self::Perturbed(re, df) => Self::Perturbed(-re, Box::new(-(*df)))
        }
    }
}

impl<T> Sub for TaylorNumber<T> where 
    T: TaylorScalar<T>{
    type Output = Self;

    fn sub(self, rhs:Self) -> TaylorNumber<T>{
            self + rhs.neg()
    }
}

impl<T> Mul<T> for TaylorNumber<T> where
    T: TaylorScalar<T>{

    type Output = TaylorNumber<T>;

    fn mul(self, rhs:T) -> TaylorNumber<T>{
        match self {
            Self::Unperturbed(re)   => Self::Unperturbed(rhs * re),
            Self::Perturbed(re, df) => Self::Perturbed(rhs*re, Box::new(*df * rhs))
        }
    }
}

// TODO: Make this into a macro and implement for not just f64
// impl Mul<TaylorNumber<f64>> for f64 where{

//     type Output = TaylorNumber<f64>;

//     fn mul(self, rhs:TaylorNumber<f64>) -> TaylorNumber<f64>{
//         match rhs {
//             TaylorNumber::Unperturbed(re)   => TaylorNumber::<f64>::Unperturbed(self * &re),
//             TaylorNumber::Perturbed(re, df) => TaylorNumber::Perturbed(self*&re, Box::new(*df * self))
//         }
//     }
// }

impl<T> Mul for TaylorNumber<T> where 
    T: TaylorScalar<T>{

    type Output = Self;

    fn mul(self:Self, rhs:Self) -> TaylorNumber<T> {

        match (&self, &rhs){
            (Self::Unperturbed(a), Self::Unperturbed(b)) => Self::new(*a * *b),
            (Self::Unperturbed(a), Self::Perturbed(b, df)) => Self::Perturbed(*a * *b, Box::new((**df).clone() * *a)),
            (Self::Perturbed(a, df), Self::Unperturbed(b)) => Self::Perturbed(*a * *b, Box::new((**df).clone() * *b)),
            _ =>{
                let new_real = self.real() * rhs.real();
                let product_rule = self.clone().diff() * rhs.clone() + rhs.diff() * self;
                Self::Perturbed(new_real, Box::new(product_rule))
            }
        }
    }
}

impl<T> One for TaylorNumber<T> where 
    T: TaylorScalar<T>{

    fn one() -> Self {
        TaylorNumber::Unperturbed(T::one())
    }
}

// TODO: implement Inv and then just derive Div with that and multiplication

impl<T> Div<T> for TaylorNumber<T> where 
    T: TaylorScalar<T>{

    type Output = Self;

    fn div(self, rhs:T) -> Self{
        match self{
            Self::Unperturbed(re) => Self::Unperturbed(re * rhs),
            Self::Perturbed(re, df) => Self::Perturbed(re*rhs, Box::new(*df * rhs))
        }
    }
}

