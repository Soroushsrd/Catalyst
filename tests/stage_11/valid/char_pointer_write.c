int main() {
  char a = 'A';
  char *p = &a;
  *p = 'C'; // write through pointer, triggers Dereference assignment path

  return a; // expected: 67 (ASCII for 'C'), a was modified via pointer
}
