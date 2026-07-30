// expected 0
int main() {
  int a[5];             // sized, uninitialized
  int b[3] = {1, 2, 3}; // full initializer list
  int c[] = {1, 2, 3};  // inferred size from initializer
  int d[4] = {1, 2};    // partial init (rest zero-filled)
  int e[3] = {0};       // zero-init idiom
  char s[6] = {'h', 'e', 'l', 'l', 'o', '\0'};
  int g[10] = {};
  return 0;
}
