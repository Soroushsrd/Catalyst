// expected 4
int main() {
  int x = 5;
  x += 3;          // 8
  x -= 1;          // 7
  x *= 2;          // 14
  int y = x++;     // y=14, x=15
  int z = ++x;     // z=16, x=16
  int b = 12 & 10; // 8
  b = b | 1;       // 9
  b = b ^ 3;       // 10
  b = b << 2;      // 40
  b = b >> 1;      // 20
  char c = 'A';
  c++; // 'B' = 66
  return (y == 14) + (z == 16) + (b == 20) + (c == 66);
}
