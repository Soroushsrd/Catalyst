// expected 66
int main() {
  char a = 'B';
  char *p = &a; // pointer to char
  char d = *p;  // dereference back to char

  return d;
}
