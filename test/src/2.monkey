let five = 5;  

let ten = 10;

fn add(x : i32, y : i32) -> i32 {  
  x + y;
}

fn subtract(x : i32, y : i32) -> i32 {  
  x - y;
}

fn multiply(x : i32, y : i32) -> i32 {  
  x * y;
}

fn divide(x : i32, y : i32) -> i32 {  
  x / y;
}

fn equals(x : i32 , y : i32) -> bool {  
  x == y;
}

fn not_equals(x : i32, y : i32) -> bool {  
  x != y;
}

fn less_than(x : i32, y : i32) -> bool {  
  x < y;
}

fn greater_than (x : i32, y : i32) -> bool {  
  x > y;
}

fn less_than_or_equal
(x : i32, y : i32) -> bool {  
  x <= y;
}

fn greater_than_or_equal
(x : i32, y : i32) -> bool {  
  x >= y;
}

let add_result = add(1, 2 * 3, 4 + 5);

fn main() -> i32 {
  add_result;
}