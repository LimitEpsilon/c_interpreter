//#include <stdio.h>

int f(int x[6], char y);
int f(int y[7], char x)
{
  struct test {
    int x;
  };
  y = 0;
  return 0;
}

struct test2 k(int x);

struct test {int x;} g(int y)
{
  struct test ret;
  ret.x = y;
  return ret;
}

int main()
{
  int a[6];
  int h();
  struct test b;
  h(3);
  b.x = h();
//  printf("%d\n", b.x);
  f(a, 'a');
  b = g(1);
  return 0;
}

struct test2 {int x[10];};

struct test2 k(int x)
{
  struct test2 y;
  y.x = 0;
  return y;
}

int h(int z)
{
  return z;
}
