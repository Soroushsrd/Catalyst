int main() {
  char a = 'A';
  int b = a + 1;  // i8 + i32, triggers coerce_to_same_int_type
  char c = a + 1; // result stored back into i8, triggers coerce_for_store

  int diff =
      'z' - 'a'; // two char literals in a binary expression, should be 25

  return diff; // expected: 25
}
