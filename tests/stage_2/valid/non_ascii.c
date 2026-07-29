// expected 0
int main() {
  // int a[3]; */
  int x = 1;
  x += 2;
  x++;
  --x;
  int y = x << 2 | 1 ^ 3;
  char nl = '\n';
  char plus = '+';
  return 0; // trailing comment, no newline
}
