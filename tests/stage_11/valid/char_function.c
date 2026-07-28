// expected 71
char to_upper(char c) {
  if (c > 'a' && c < 'z') {
    return c - 32; // coerce_for_store: i32 result stored as i8 return
  }
  return c;
}

int main() {
  char a = 'g';
  char b = to_upper(a); // pass char, receive char

  return b;
}
