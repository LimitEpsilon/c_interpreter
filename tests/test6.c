struct test {
  int arr[2];
};

int main(void) {
  struct test a;
  int x;
  a.arr = 0;
  &(++x);
  return 0;
}
