// expected 67
int main() {
  char a = 'A';
  char *p = &a;
  *p = 'C'; // write through pointer, triggers Dereference assignment path

  return a;
}
