int main() {
  char a = 'B';
  char *p = &a; // pointer to char
  char d = *p;  // dereference back to char

  return d; // expected: 66 (ASCII for 'B')
}
