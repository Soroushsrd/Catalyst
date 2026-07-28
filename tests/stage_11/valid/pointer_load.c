// expected 10
int g = 7;
int main() {
  int a = 1;
  int *p = &a;
  *p = 5;
  int **pp = &p;
  **pp = 9;
  g = a + 1;
  return g;
}
