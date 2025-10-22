int main() {
    int x = 5;
    int *p = &x;
    int **pp = &p;
    int y = **pp;
    return y;
}
