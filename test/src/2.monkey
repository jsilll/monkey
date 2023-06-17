let five = 5;  

let ten = 10;

fn add(x : i64, y : i64) -> i64 {  
  x + y;
}

fn subtract(x : i64, y : i64) -> i64 {  
  x - y;
}

fn multiply(x : i64, y : i64) -> i64 {  
  x * y;
}

fn divide(x : i64, y : i64) -> i64 {  
  x / y;
}

fn equals(x : i64 , y : i64) -> bool {  
  x == y;
}

fn not_equals(x : i64, y : i64) -> bool {  
  x != y;
}

fn less_than(x : i64, y : i64) -> bool {  
  x < y;
}

fn greater_than (x : i64, y : i64) -> bool {  
  x > y;
}

fn less_than_or_equal
(x : i64, y : i64) -> bool {  
  x <= y;
}

fn greater_than_or_equal
(x : i64, y : i64) -> bool {  
  x >= y;
}

let res = if equals(add(five, five), ten) { add(five, five) } else { 0 };