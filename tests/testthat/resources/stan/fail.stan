parameters {
  row_vector[3] a;
  vector[3] b;
}

model {
  matrix[1, 1] c = a * b;
}
