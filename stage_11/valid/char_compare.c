int main() {
  char a = 'm';
  int result = 0;

  if (a == 'm') {
    result = 1;
  }

  if (a < 'z') {
    result = result + 1;
  }

  if (a > 'a') {
    result = result + 1;
  }

  return result; // expected: 3
}
