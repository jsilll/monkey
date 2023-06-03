let five = 5;  

let ten = 10;

let add = fn(x, y) {  
  x + y;
};

let equal = fn(x, y) {  
  x == y;
};

let notEqual = fn(x, y) {  
  x != y;
};

let lt = fn(x, y) {  
  x < y;
};

let gt = fn(x, y) {  
  x >y;
};

let lte = fn(x, y) {  
  x <= y;
};

let gte = fn(x, y) {  
  x >=y;
};

let result = add(five, ten);